#include <iostream>
#include <stack>
#include <set>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>

#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Target/TargetMachine.h>

#include "util/assert.hpp"

#include "tx_lang_defs.hpp"
#include "tx_except.hpp"

#include "llvm_generator.hpp"
#include "driver.hpp"
#include "parsercontext.hpp"

#include "ast/ast_modbase.hpp"
#include "ast/expr/ast_ref.hpp"
#include "ast/expr/ast_exprs.hpp"
#include "symbol/package.hpp"
#include "symbol/symbol_lookup.hpp"

using namespace llvm;


void GenScope::use_alloca_insertion_point() {
    this->currentBlock = this->builder->GetInsertBlock();
    if ( this->lastAllocaInstr && this->lastAllocaInstr->getNextNode() )
        this->builder->SetInsertPoint( this->lastAllocaInstr->getNextNode() );
    else
        this->builder->SetInsertPoint( this->entryBlock, this->entryBlock->begin() );
}

void GenScope::use_current_insertion_point() {
    this->lastAllocaInstr = this->builder->GetInsertPoint()->getPrevNode();
    this->builder->SetInsertPoint( this->currentBlock );  // at end of current block
}


LlvmGenerationContext::LlvmGenerationContext( TxPackage& tuplexPackage, llvm::LLVMContext& llvmContext )
        : llvmModulePtr( new llvm::Module("top", llvmContext ) ),
          tuplexPackage( tuplexPackage ),
          llvmContext( llvmContext )
{
    this->voidPtrT = llvm::Type::getInt8PtrTy( this->llvmContext );
    this->closureRefT = TxReferenceTypeClassHandler::make_ref_llvm_type( *this, llvm::Type::getInt8Ty( this->llvmContext ), "ClosRef" );
    this->i32T = llvm::Type::getInt32Ty( this->llvmContext );
    this->superTypesPtrT = llvm::PointerType::getUnqual( llvm::StructType::get( i32T, i32T, llvm::ArrayType::get( i32T, 0 ) ) );

    // Add the current debug info version into the module.
    this->llvmModulePtr->addModuleFlag( Module::Warning, "Debug Info Version", DEBUG_METADATA_VERSION );

    // Darwin only supports dwarf2 (copied from Kaleidoscope)
    if ( Triple( sys::getProcessTriple() ).isOSDarwin() ) {
        this->llvmModulePtr->addModuleFlag( llvm::Module::Warning, "Dwarf Version", 2 );
    }

    this->_debugBuilder = new llvm::DIBuilder( *this->llvmModulePtr );
    this->_genDIFile = this->_debugBuilder->createFile( __FILE__, "" );
}


/***** Compile the AST into a module *****/

void LlvmGenerationContext::init_codegen() {
    // initialize debug info build:
    auto& firstSourceFilename = this->tuplexPackage.driver().get_first_source_filename();
    auto firstDebugFile = this->_debugBuilder->createFile( firstSourceFilename, "" );
    bool isOptimized = false;
    std::string commandLineOptions;
    unsigned runtimeVersion = 0;
    this->_debugUnit = this->_debugBuilder->createCompileUnit(
            llvm::dwarf::DW_LANG_C, firstDebugFile, get_version_string(), isOptimized, commandLineOptions, runtimeVersion);
}

int LlvmGenerationContext::generate_code( const TxParsingUnitNode* staticScopeNode ) {
    try {
        staticScopeNode->code_gen( *this );
        return 0;
    }
    catch ( const codecheck_error& err ) {
        LOG_DEBUG(this->LOGGER(), "Caught code check error in parsing unit " << staticScopeNode << ": " << err);
        return 1;
    }
}
int LlvmGenerationContext::generate_code( const TxTypeDeclNode* staticScopeNode ) {
    try {
        staticScopeNode->code_gen( *this );
        return 0;
    }
    catch ( const codecheck_error& err ) {
        LOG_DEBUG(this->LOGGER(), "Caught code check error in type decl node " << staticScopeNode << ": " << err);
        return 1;
    }
}

void LlvmGenerationContext::generate_main( const std::string& userMainIdent, const TxActualType* mainFuncType ) {
    this->entryFunction = this->gen_main_function( userMainIdent, mainFuncType );
}

void LlvmGenerationContext::initialize_target() {
    // (code derived from Kaleidoscope tutorial)
    auto targetTriple = sys::getDefaultTargetTriple();

    // For this example, we’ll initialize all the targets for emitting object code.
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    std::string error;
    auto target = TargetRegistry::lookupTarget( targetTriple, error );

    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialize the
    // TargetRegistry or we have a bogus target triple.
    if ( !target ) {
        this->LOGGER()->error( "LLVM target lookup error: " );
        errs() << error;
        return;
    }

    // For our example, we’ll use the generic CPU without any additional features, options or relocation model.
    auto cpu = "generic";
    auto features = "";
    TargetOptions opt;
    auto relocModel = Optional<Reloc::Model>();  // Reloc::Model::PIC_
    auto targetMachine = target->createTargetMachine( targetTriple, cpu, features, opt, relocModel );

    this->llvmModulePtr->setDataLayout( targetMachine->createDataLayout() );
    this->llvmModulePtr->setTargetTriple( targetTriple );
}

void LlvmGenerationContext::finalize_codegen() {
    this->initialize_target();

    this->_debugBuilder->finalize();
}

int LlvmGenerationContext::verify_code() {
    //this->LOG.info("Verifying LLVM code...");;
    std::string errInfo;
    raw_string_ostream ostr( errInfo );
    bool ret = verifyModule( this->llvmModule(), &ostr );
    if ( ret ) {
        this->LOGGER()->error( "LLVM code verification failed: \"%s\"", errInfo.c_str() );
        return 1;
    }
    else
        return 0;
}

void LlvmGenerationContext::print_IR() {
    // FUTURE: support writing to a .ll file
    this->LOGGER()->info( "Printing LLVM bytecode with pass is currently broken, dumping instead..." );
    this->llvmModule().print(outs(), nullptr);

    // FIXME: This crashes but need to set up IDE with LLVM source code to find cause
//    this->LOGGER()->info( "Printing LLVM bytecode..." );
//    PrintModulePass printPass( outs() );
//    PassManager<Module> pm;
//    pm.addPass( printPass );
//    AnalysisManager<Module> am;
//    auto& module = this->llvmModule();
//    pm.run( module, am );
//    std::cout << std::endl;
}

int LlvmGenerationContext::write_bitcode( const std::string& filepath ) {
    LOG_DEBUG( this->LOGGER(), "Writing LLVM bitcode file '" << filepath << "'" );
    std::error_code errInfo;
    raw_fd_ostream ostream( filepath.c_str(), errInfo, sys::fs::FA_Write /* was: F_RW */ );
    if ( errInfo ) {
        LOG( this->LOGGER(), ERROR, "Failed to open bitcode output file for writing: " << errInfo.message() );
        return 1;
    }
    else {
        WriteBitcodeToFile( this->llvmModule(), ostream );
        return 0;
    }
}


/***** generate runtime type data *****/

static void add_base_types( std::set<uint32_t>& supertypes, const TxActualType* type ) {
    auto ret = supertypes.emplace( type->get_runtime_type_id() );
    if ( !ret.second )
        return;
    if ( type->has_base_type() ) {
        if ( type->is_generic_specialization() )
            add_base_types( supertypes, type->get_semantic_base_type() );
        add_base_types( supertypes, type->get_base_type() );
        for ( auto intf : type->get_interfaces() ) {
            add_base_types( supertypes, intf );
        }
    }
}

/** construct array of sorted supertype ids (used for dynamic is-a) */
static Constant* gen_supertype_ids( LlvmGenerationContext* context, const TxActualType* acttype ) {
    std::set<uint32_t> supertypes;
    add_base_types( supertypes, acttype );  // (also adds the type itself)
    if ( acttype->get_type_class() == TXTC_INTERFACEADAPTER )
        add_base_types( supertypes, static_cast<const TxInterfaceAdapterType*>(acttype)->adapted_type() );

    std::vector<uint32_t> supertypesvec( supertypes.cbegin(), supertypes.cend() );

    auto int32T = Type::getInt32Ty( context->llvmContext );
    auto typesArrayLenC = ConstantInt::get( int32T, supertypesvec.size() );
    auto typesArrayT = StructType::get( int32T, int32T, ArrayType::get( int32T, supertypesvec.size() ) );
    Constant* typesArrayC = ConstantStruct::get( typesArrayT, typesArrayLenC, typesArrayLenC,
                                                 ConstantDataArray::get( context->llvmContext, supertypesvec ) );
    std::string supertypesName( acttype->get_declaration()->get_unique_full_name() + "$supers" );
    GlobalVariable* superTypesC = new GlobalVariable( context->llvmModule(), typesArrayT, true, GlobalValue::ExternalLinkage,
                                                      typesArrayC, supertypesName );
    return superTypesC;
}

/** Makes or gets previously made vtable meta type for the specified type. */
StructType* LlvmGenerationContext::get_vtable_meta_type( const TxActualType* acttype ) {
    auto pairI = this->llvmVTableTypes.find(acttype);
    if ( pairI != this->llvmVTableTypes.end() ) {
        return pairI->second;
    }
    else {
        auto vtableT = acttype->make_vtable_type( *this );
        this->llvmVTableTypes.emplace( acttype, vtableT );
        return vtableT;
    }
}

/** Makes or gets previously made vtable for the specified type. */
GlobalVariable* LlvmGenerationContext::get_vtable( const TxActualType* acttype ) {
    auto pairI = this->llvmVTables.find(acttype);
    if ( pairI != this->llvmVTables.end() ) {
        return pairI->second;
    }
    else {
        auto vtableT = this->get_vtable_meta_type( acttype );
        std::string vtableName( acttype->get_declaration()->get_unique_full_name() + "$vtable" );
        auto vtableC = new GlobalVariable( this->llvmModule(), vtableT, true, GlobalValue::ExternalLinkage, nullptr, vtableName );
        this->llvmVTables.emplace( acttype, vtableC );
        return vtableC;
    }
}

void LlvmGenerationContext::generate_runtime_type_info() {
    /* Runtime representation notes:
     * Data types are the built-in types, and the concrete user types.
     * Data types have a larger RTTI structure than other types.
     */

    /** array of i8, length equal to number of vtable types */
    std::vector<Constant*> typeClasses;

    auto emptyStructPtrT = PointerType::getUnqual( StructType::get( this->llvmContext ) );
    std::vector<Type*> metaTypeFields {
            emptyStructPtrT,  // vtable pointer
            i32T,           // instance size (for Array types, element instance size)
            i32T,           // type id
            i32T,           // element type id (for Array types)
    };
    StructType* typeInfoT = StructType::create( metaTypeFields, "tx.runtime.$TypeInfo" );

    std::vector<Constant*> supertypeArrays;

    // create runtime type info for the data types:
    std::vector<Constant*> typeInfos;
    for ( auto acttypeI = this->tuplexPackage.registry().runtime_types_cbegin();
            acttypeI != this->tuplexPackage.registry().data_types_cend();
            acttypeI++ )
    {
        const TxActualType* acttype = *acttypeI;

        Constant* instanceSizeC;
        if ( acttype->is_static() || ( acttype->get_type_class() == TXTC_ARRAY && !acttype->is_type_generic() ) ) {
            LOG_TRACE( this->LOGGER(), "Creating runtime type info for data type " << acttype );
            if ( acttype->get_type_class() == TXTC_ARRAY ) {
                // arrays are a special case, where the value-generic (non-concrete) supertype can have statically known element size
                instanceSizeC = ConstantExpr::getTrunc( acttype->gen_static_element_size( *this ),
                                                        this->i32T );
            }
            else {
                instanceSizeC = ConstantExpr::getTrunc( acttype->gen_static_element_size( *this ),
                                                        this->i32T );
            }
        }
        else {
            LOG_TRACE( this->LOGGER(), "Creating runtime type info for data type " << acttype << "  \t(not statically concrete)" );
            instanceSizeC = ConstantInt::get( this->i32T, 0 );
        }

        // ensure the vtable meta type is created:
        this->get_vtable_meta_type( acttype );

        // create the vtable instance:
        auto vtype = acttype;
        while ( vtype->is_same_vtable_type() ) {
            vtype = vtype->get_semantic_base_type();
        }
        // Note: Concrete types may use a (possibly shared) vtable instance of an abstract ancestor type which isn't itself a "data type".
        GlobalVariable* vtableC = this->get_vtable( vtype );

        auto typeIdC = ConstantInt::get( i32T, acttype->get_runtime_type_id() );

        Constant* elementTypeIdC = ConstantInt::get( i32T,
                ( acttype->get_type_class() == TXTC_ARRAY ? acttype->element_type()->get_runtime_type_id() : 0 ) );

        std::vector<Constant*> members {
                                         ConstantExpr::getBitCast( vtableC, emptyStructPtrT ),
                                         instanceSizeC,
                                         typeIdC,
                                         elementTypeIdC,
        };
        typeInfos.push_back( ConstantStruct::get( typeInfoT, members ) );

        // populate the type class array:
        auto typeClassC = ConstantInt::get( Type::getInt8Ty( this->llvmContext ), acttype->get_type_class() );
        typeClasses.push_back( typeClassC );

        // create the super type id array:
        auto superTypesC = gen_supertype_ids( this, acttype );
        supertypeArrays.push_back( ConstantExpr::getBitCast( superTypesC, superTypesPtrT ) );
    }

    // create the vtable meta types for the remaining vtable types:
    for ( auto acttypeI = this->tuplexPackage.registry().data_types_cend();
            acttypeI != this->tuplexPackage.registry().vtable_types_cend();
            acttypeI++ )
    {
        const TxActualType* acttype = *acttypeI;
        LOG_TRACE( this->LOGGER(), "Creating runtime type info for vtable type " << acttype );

        // ensure the vtable meta type is created:
        this->get_vtable_meta_type( acttype );

        // populate the type class array:
        auto typeClassC = ConstantInt::get( Type::getInt8Ty( this->llvmContext ), acttype->get_type_class() );
        typeClasses.push_back( typeClassC );

        // create the super type id array:
        auto superTypesC = gen_supertype_ids( this, acttype );
        supertypeArrays.push_back( ConstantExpr::getBitCast( superTypesC, superTypesPtrT ) );
    }

//    // create the type class info for the remaining runtime types:
//    for ( auto acttypeI = this->tuplexPackage.registry().vtable_types_cend();
//            acttypeI != this->tuplexPackage.registry().runtime_types_cend();
//            acttypeI++ )
//    {
//        const TxActualType* acttype = *acttypeI;
//
//        // populate the type class array:
//        auto typeClassC = ConstantInt::get( Type::getInt8Ty( this->llvmContext ), acttype->get_type_class() );
//        typeClasses.push_back( typeClassC );
//    }

    // generate the runtime type info:
    {
        auto typeCount = typeInfos.size();
        auto typeCountC = new GlobalVariable( this->llvmModule(), i32T, true, GlobalValue::ExternalLinkage,
                                              ConstantInt::get( i32T, typeCount ),
                                              "tx.runtime.TYPE_COUNT" );
        this->register_llvm_value( typeCountC->getName().str(), typeCountC );

        auto tiArrayT = ArrayType::get( typeInfoT, typeCount );
        auto tiArrayC = ConstantArray::get( tiArrayT, typeInfos );
        auto typeInfosC = new GlobalVariable( this->llvmModule(), tiArrayT, true, GlobalValue::ExternalLinkage,
                                              tiArrayC,
                                              "tx.runtime.TYPE_INFOS" );
        this->register_llvm_value( typeInfosC->getName().str(), typeInfosC );
    }
    {
        auto tcArrayT = ArrayType::get( Type::getInt8Ty( this->llvmContext ), typeClasses.size() );
        auto tcArrayC = ConstantArray::get( tcArrayT, typeClasses );
        auto typeClassesC = new GlobalVariable( this->llvmModule(), tcArrayT, true, GlobalValue::ExternalLinkage,
                                               tcArrayC,
                                               "tx.runtime.TYPE_CLASSES" );
        this->register_llvm_value( typeClassesC->getName().str(), typeClassesC );
    }
    {
        auto stArrayT = ArrayType::get( superTypesPtrT, supertypeArrays.size() );
        auto stArrayC = ConstantArray::get( stArrayT, supertypeArrays );
        auto supertypesC = new GlobalVariable( this->llvmModule(), stArrayT, true, GlobalValue::ExternalLinkage,
                                               stArrayC,
                                               "tx.runtime.SUPER_TYPES" );
        this->register_llvm_value( supertypesC->getName().str(), supertypesC );
    }
}


void LlvmGenerationContext::generate_runtime_vtables() {
    for ( auto & elem : this->llvmVTables )
    {
        auto acttype = elem.first;
        auto vtableC = elem.second;
        ASSERT( acttype->is_prepared(), "Non-prepared type: " << acttype );
        ASSERT( !acttype->is_same_vtable_type(), "vtable instance type does not have distinct vtable: " << acttype );

        bool isTypeGeneric = acttype->is_type_generic();

        {
            std::string typeIdStr = std::to_string( acttype->get_runtime_type_id() );
            if ( typeIdStr.size() < 5 )
                typeIdStr.resize( 5, ' ' );
            if ( isTypeGeneric )  // the only generic types that are data types and have distinct vtable is Array
                LOG_DEBUG( this->LOGGER(), "Type id " << typeIdStr
                           << ": Populating vtable initializer with null placeholders for generic base type " << acttype );
            else
                LOG_DEBUG( this->LOGGER(), "Type id " << typeIdStr << ": Populating vtable initializer for " << acttype );
        }

        auto virtualFields = acttype->get_virtual_fields();
        std::vector<Constant*> initMembers( virtualFields.get_field_count() );  // initializes full length to enable random access assignment
        ASSERT( virtualFields.get_field_count() == virtualFields.fieldMap.size(),
                "Unexpected shadowed fields in virtual fields of " << acttype );

        for ( auto & field : virtualFields.fieldMap ) {
            auto actualFieldEnt = virtualFields.get_field( field.second );
            Constant* llvmFieldC;

            if ( ( actualFieldEnt->get_decl_flags() & TXD_ABSTRACT ) || isTypeGeneric ) {
                //std::cerr << "inserting NULL for abstract virtual field: " << field.first << " at ix " << field.second << std::endl;
                Type* fieldType;
                if ( actualFieldEnt->get_storage() & TXS_INSTANCEMETHOD ) {
                    auto closureType = this->get_llvm_type( actualFieldEnt->qtype() );
                    fieldType = closureType->getStructElementType( 0 );
                }
                else if ( field.first == "$adTypeId" ) {
                    fieldType = this->get_llvm_type( actualFieldEnt->qtype() );
                }
                else
                    fieldType = PointerType::getUnqual( this->get_llvm_type( actualFieldEnt->qtype() ) );
                llvmFieldC = Constant::getNullValue( fieldType );
            }

            else if ( field.first == "$adTypeId" ) {
                ASSERT( acttype->get_type_class() == TXTC_INTERFACEADAPTER, "Expected InterfaceAdapter type: " << acttype );
                auto adapterType = static_cast<const TxInterfaceAdapterType*>( acttype );
                llvmFieldC = ConstantInt::get( this->i32T, adapterType->adapted_type()->get_runtime_type_id() );
            }

            else {
                llvmFieldC = cast<Constant>( actualFieldEnt->get_llvm_value() );
            }

            //std::cerr << "inserting field: " << field.first << " at ix " << field.second << ": " << actualFieldEnt << std::endl;
            auto ix = field.second;
            initMembers[ix] = llvmFieldC;
        }

        Constant* initializer = ConstantStruct::getAnon( this->llvmContext, initMembers );
        vtableC->setInitializer( initializer );
        //std::cerr << "initializing vtable (statically constant): " << vtableC << std::endl;
    }
}


/***** code generation helpers *****/

llvm::Value* LlvmGenerationContext::gen_malloc( GenScope* scope, llvm::Type* objT ) {
    auto int32T = Type::getInt32Ty( this->llvmContext );
    auto mallocParameterType = int32T;
    auto objSizeC = ConstantExpr::getTruncOrBitCast( ConstantExpr::getSizeOf( objT ), mallocParameterType );
    auto objCountC = ConstantInt::get( int32T, 1 );
    auto objAllocI = CallInst::CreateMalloc( scope->builder->GetInsertBlock(), mallocParameterType,
                                             objT, objSizeC, objCountC, nullptr, "" );
    scope->builder->GetInsertBlock()->getInstList().push_back( objAllocI );
    return objAllocI;
}

llvm::Value* LlvmGenerationContext::gen_malloc( GenScope* scope, Value* sizeV ) {
    auto int32T = Type::getInt32Ty( this->llvmContext );
    auto mallocParameterType = int32T;
    auto objSizeV = scope->builder->CreateTruncOrBitCast( sizeV, mallocParameterType );
    auto objCountC = ConstantInt::get( int32T, 1 );
    auto objT = Type::getInt8Ty( this->llvmContext );
    auto objAllocI = CallInst::CreateMalloc( scope->builder->GetInsertBlock(), mallocParameterType,
                                             objT, objSizeV, objCountC, nullptr, "" );
    scope->builder->GetInsertBlock()->getInstList().push_back( objAllocI );
    return objAllocI;
}


/*** runtime type info access ***/

Value* LlvmGenerationContext::gen_get_type_info( GenScope* scope, const TxActualType* statDeclType, Value* runtimeBaseTypeIdV, unsigned fieldIndex ) {
    //std::cerr << "gen_get_type_info() " << statDeclType << std::endl;
    auto typeInfoC = cast<GlobalVariable>( this->lookup_llvm_value( "tx.runtime.TYPE_INFOS" ) );
    if ( auto baseTypeIdC = dyn_cast<Constant>( runtimeBaseTypeIdV ) ) {
        //std::cerr << "Getting static type info of " << statDeclType << " and constant type id " << baseTypeIdC << std::endl;
        auto tid = cast<ConstantInt>( baseTypeIdC )->getZExtValue();
        if ( tid >= this->tuplexPackage.registry().data_types_count() )
            THROW_LOGIC( "(constant) type id out of bounds (not a data type) in gen_get_type_info(): " << tid );
        return typeInfoC->getInitializer()->getAggregateElement( baseTypeIdC )
                                          ->getAggregateElement( fieldIndex );
    }
    else if ( statDeclType && statDeclType->is_leaf_derivation() ) {
        //std::cerr << "Getting static type info of leaf derivation " << statDeclType << std::endl;
        if ( statDeclType->get_runtime_type_id() >= this->tuplexPackage.registry().data_types_count() )
            THROW_LOGIC( "Not a data type in gen_get_type_info(): " << statDeclType );
        return typeInfoC->getInitializer()->getAggregateElement( statDeclType->get_runtime_type_id() )
                                          ->getAggregateElement( fieldIndex );
    }
    ASSERT( scope, "NULL scope for non-constant runtimeBaseTypeIdV: " << runtimeBaseTypeIdV );

#ifdef DEVMODE
    if ( true ) { // add bounds check:
        auto parentFunc = scope->builder->GetInsertBlock()->getParent();
        BasicBlock* trueBlock = BasicBlock::Create( this->llvmContext, "if_rtti_outofbounds", parentFunc );
        BasicBlock* nextBlock = BasicBlock::Create( this->llvmContext, "if_rtti_inbounds", parentFunc );

        auto typeInfoLenC = cast<GlobalVariable>( this->lookup_llvm_value( "tx.runtime.TYPE_COUNT" ) );
        auto condV = scope->builder->CreateICmpUGE( runtimeBaseTypeIdV, typeInfoLenC->getInitializer() );
        scope->builder->CreateCondBr( condV, trueBlock, nextBlock );
        { // if type id out of bounds (not a data type)
            scope->builder->SetInsertPoint( trueBlock );
            auto indexV = scope->builder->CreateZExt( runtimeBaseTypeIdV, Type::getInt64Ty( this->llvmContext ) );
            this->gen_panic_call( scope, "RTTI type array index out of bounds: %d\n", indexV );
            scope->builder->CreateBr( nextBlock );  // terminate block, though won't be executed
        }
        scope->builder->SetInsertPoint( nextBlock );
    }
#endif

    Value* ixs[] = { ConstantInt::get( this->i32T, 0 ),
                     runtimeBaseTypeIdV,
                     ConstantInt::get( this->i32T, fieldIndex ) };
    auto valA = scope->builder->CreateInBoundsGEP( typeInfoC, ixs );
    return scope->builder->CreateLoad( valA );
}


Value* LlvmGenerationContext::gen_get_vtable( GenScope* scope, const TxActualType* statDeclType, Value* runtimeBaseTypeIdV ) {
    if ( statDeclType && statDeclType->get_type_class() == TXTC_FUNCTION ) {
        statDeclType = this->tuplexPackage.registry().get_builtin_type( TXBT_FUNCTION );
        runtimeBaseTypeIdV = ConstantInt::get( this->i32T, statDeclType->get_runtime_type_id() );
    }
    else if ( statDeclType && statDeclType->get_type_class() == TXTC_REFERENCE ) {
        statDeclType = this->tuplexPackage.registry().get_builtin_type( TXBT_REFERENCE );
        runtimeBaseTypeIdV = ConstantInt::get( this->i32T, statDeclType->get_runtime_type_id() );
    }
    // FIXME: Handle runtime function type vtable access

    auto vtablePtrV = this->gen_get_type_info( scope, statDeclType, runtimeBaseTypeIdV, 0 );
//    std::cerr << "getting vtable ptr for " << statDeclType << ", static id " << statDeclType->get_runtime_type_id()
//            << ", runtime id " << runtimeBaseTypeIdV << ": " << vtablePtrV << std::endl;
    // cast the "opaque" vtable pointer to the statically declared type (which may be an ancestor of the actual type):
    StructType* vtableT = this->llvmVTableTypes.at( statDeclType );
    Type* vtablePtrT = PointerType::getUnqual( vtableT );
    return scope->builder->CreatePointerCast( vtablePtrV, vtablePtrT, "vtableptr" );
}

Value* LlvmGenerationContext::gen_get_element_size( GenScope* scope, const TxActualType* statDeclType, Value* runtimeBaseTypeIdV ) {
    // FIXME: Handle runtime function type access
    return this->gen_get_type_info( scope, statDeclType, runtimeBaseTypeIdV, 1 );
}

Value* LlvmGenerationContext::gen_get_element_type_id( GenScope* scope, const TxActualType* statDeclType, Value* runtimeBaseTypeIdV ) {
    return this->gen_get_type_info( scope, statDeclType, runtimeBaseTypeIdV, 3 );
}

Value* LlvmGenerationContext::gen_get_type_class( GenScope* scope, const TxActualType* statDeclType, Value* runtimeBaseTypeIdV ) {
    if ( statDeclType && statDeclType->get_type_class() != TXTC_ANY && statDeclType->get_type_class() != TXTC_INTERFACE ) {
        // (interface adapters are subtypes of interfaces so a statically known interface type must be inspected in runtime)
        return ConstantInt::get( Type::getInt8Ty( this->llvmContext ), statDeclType->get_type_class() );
    }
    auto typeClassesC = cast<GlobalVariable>( this->lookup_llvm_value( "tx.runtime.TYPE_CLASSES" ) );
    if ( auto baseTypeIdC = dyn_cast<Constant>( runtimeBaseTypeIdV ) ) {
        return typeClassesC->getInitializer()->getAggregateElement( baseTypeIdC );
    }
    else if ( statDeclType && statDeclType->is_leaf_derivation() ) {
        return typeClassesC->getInitializer()->getAggregateElement( statDeclType->get_runtime_type_id() );
    }

#ifdef DEVMODE
    if ( true ) { // add bounds check:
        auto parentFunc = scope->builder->GetInsertBlock()->getParent();
        BasicBlock* trueBlock = BasicBlock::Create( this->llvmContext, "if_rtti_outofbounds", parentFunc );
        BasicBlock* nextBlock = BasicBlock::Create( this->llvmContext, "if_rtti_inbounds", parentFunc );

        auto typeClassesLenC = ConstantInt::get( this->i32T, this->tuplexPackage.registry().vtable_types_count() );
        auto condV = scope->builder->CreateICmpUGE( runtimeBaseTypeIdV, typeClassesLenC );
        scope->builder->CreateCondBr( condV, trueBlock, nextBlock );
        { // if type id out of bounds (not a vtable type)
            scope->builder->SetInsertPoint( trueBlock );
            auto indexV = scope->builder->CreateZExt( runtimeBaseTypeIdV, Type::getInt64Ty( this->llvmContext ) );
            this->gen_panic_call( scope, "RTTI type classes array index out of bounds: %d\n", indexV );
            scope->builder->CreateBr( nextBlock );  // terminate block, though won't be executed
        }
        scope->builder->SetInsertPoint( nextBlock );
    }
#endif
    // FIXME: Handle runtime function and ref type access

    Value* ixs[] = { ConstantInt::get( this->i32T, 0 ),
                     runtimeBaseTypeIdV };
    auto valA = scope->builder->CreateInBoundsGEP( typeClassesC, ixs );
    return scope->builder->CreateLoad( valA );
}


Value* LlvmGenerationContext::gen_get_supertypes_array( GenScope* scope, const TxActualType* statDeclType, Value* runtimeBaseTypeIdV ) {
    /* TODO: statically constant optimization (assume not that common though)
    auto supertypesC = cast<GlobalVariable>( this->lookup_llvm_value( "tx.runtime.SUPER_TYPES" ) );
    if ( auto baseTypeIdC = dyn_cast<Constant>( runtimeBaseTypeIdV ) ) {
        return supertypesC->getInitializer()->getAggregateElement( baseTypeIdC );
    }
    else if ( statDeclType && statDeclType->is_leaf_derivation() ) {
        return supertypesC->getInitializer()->getAggregateElement( statDeclType->get_runtime_type_id() );
    }
    */

    auto getSupersFuncA = this->llvmModule().getFunction( "$get_supertypes_array" );
    ASSERT( getSupersFuncA, "$get_supertypes_array() function not found in " << this );
    return scope->builder->CreateCall( getSupersFuncA, { runtimeBaseTypeIdV } );
//    Value* superTypesV = this->lookup_llvm_value( "tx.runtime.SUPER_TYPES" );
//    Value* ixs[] = { ConstantInt::get( i32T, 0 ),
//                     runtimeBaseTypeIdV };
//    auto valA = scope->builder->CreateInBoundsGEP( superTypesV, ixs );
//    return scope->builder->CreateLoad( valA );
}

Value* LlvmGenerationContext::gen_get_supertypes_array_ref( GenScope* scope, const TxActualType* statDeclType, Value* runtimeBaseTypeIdV,
                                                            Constant* arrayTypeIdC ) {
    auto stPtrV = this->gen_get_supertypes_array( scope, statDeclType, runtimeBaseTypeIdV );
    auto refT = TxReferenceTypeClassHandler::make_ref_llvm_type( *this, stPtrV->getType()->getPointerElementType() );
    return gen_ref( *this, scope, refT, stPtrV, arrayTypeIdC );
}


/*** global constants generation ***/

Constant* LlvmGenerationContext::gen_const_cstring_address( const std::string& value ) {
    std::vector<uint8_t> array( value.data(), value.data() + value.size() + 1 );
    return this->gen_const_byte_array_address( array );
}

Constant* LlvmGenerationContext::gen_const_byte_array_address( const std::vector<uint8_t>& arrayData ) {
    auto iter = this->byteArrayTable.find( arrayData );
    if ( iter != this->byteArrayTable.end() ) {
        //std::cerr << "reusing byte array constant \"" << arrayData.data() << "\"" << std::endl;
        return iter->second;
    }

    std::vector<Constant*> arrayMembers {
        ConstantInt::get( this->llvmContext, APInt( 32, arrayData.size() ) ),
        ConstantInt::get( this->llvmContext, APInt( 32, arrayData.size() ) ),
        ConstantDataArray::get( this->llvmContext, ArrayRef<uint8_t>( arrayData.data(), arrayData.size() ) )
    };
    auto arrayC = ConstantStruct::getAnon( arrayMembers );

    auto arrayGlobal = new GlobalVariable( this->llvmModule(), arrayC->getType(), true, GlobalValue::InternalLinkage, arrayC );
    this->byteArrayTable.emplace( arrayData, arrayGlobal );
    return arrayGlobal;
}

Constant* LlvmGenerationContext::gen_const_string_obj_address( StructType* stringObjT, Constant* arrayTIdC, const std::vector<uint8_t>& arrayData ) {
    auto iter = this->stringObjTable.find( arrayData );
    if ( iter != this->stringObjTable.end() ) {
        //std::cerr << "reusing string object constant \"" << arrayData.data() << "\"" << std::endl;
        return iter->second;
    }

    auto arrayRefObjT = cast<StructType>( stringObjT->getTypeAtIndex( 0U ) );

    auto arrayGlobalPtr = this->gen_const_byte_array_address( arrayData );

    // String datatype member is a reference to unknown array length (represented by length 0 in LLVM array type)
    auto arrayGenPtr = ConstantExpr::getBitCast( arrayGlobalPtr, arrayRefObjT->getTypeAtIndex( 0U ) );
    auto arrayRefObjC = gen_ref( *this, arrayRefObjT, arrayGenPtr, arrayTIdC );
    auto stringObjC = ConstantStruct::get( stringObjT, { arrayRefObjC } );

    auto stringGlobal = new GlobalVariable( this->llvmModule(), stringObjC->getType(), true, GlobalValue::InternalLinkage, stringObjC );
    this->stringObjTable.emplace( arrayData, stringGlobal );
    return stringGlobal;
}


/*** global core functions invocation ***/

Value* LlvmGenerationContext::gen_equals_invocation( GenScope* scope, Value* lvalA, Value* lvalTypeIdV, Value* rvalA, Value* rvalTypeIdV ) {
    const TxActualType* anyType = this->tuplexPackage.registry().get_builtin_type( TXBT_ANY );
    auto equalsField = anyType->get_virtual_fields().get_field( "equals" );
    auto methodLambdaV = instance_method_value_code_gen( *this, scope, anyType, lvalTypeIdV, lvalA,
                                                         equalsField->qtype().type(), equalsField->get_unique_name(), false );
    auto functionPtrV = gen_get_struct_member( *this, scope, methodLambdaV, 0 );
    auto closureRefV = gen_get_struct_member( *this, scope, methodLambdaV, 1 );
    auto otherRefT = TxReferenceTypeClassHandler::make_ref_llvm_type( *this, llvm::StructType::get( this->llvmContext ) );  // ref to Any
    auto otherRefV = gen_ref( *this, scope, otherRefT, rvalA, rvalTypeIdV );
    std::vector<Value*> args( { closureRefV, otherRefV } );

    auto resType = Type::getInt1Ty( this->llvmContext );
    auto fnTy = FunctionType::get( resType, ArrayRef<Type*>( { closureRefV->getType(), otherRefV->getType() } ), false );
    FunctionCallee callee( fnTy, functionPtrV );

    return scope->builder->CreateCall( callee, args, "equals" );
}

Value* LlvmGenerationContext::gen_isa( GenScope* scope, Value* refV, Value* typeIdV ) {
    auto refT = TxReferenceTypeClassHandler::make_ref_llvm_type( *this, llvm::StructType::get( this->llvmContext ) );  // ref to Any
    auto isaFuncC = this->llvmModule().getOrInsertFunction( "tx.isa$func", Type::getInt1Ty( this->llvmContext ),
                                                            this->closureRefT, refT, this->i32T );
    auto castRefV = gen_ref_conversion( *this, scope, refV, refT );
    std::vector<Value*> args( { ConstantStruct::getNullValue( this->closureRefT ), castRefV, typeIdV } );
    return scope->builder->CreateCall( isaFuncC, args, "isa" );
}

void LlvmGenerationContext::gen_panic_call( GenScope* scope, const std::string& message ) {
    auto panicSymbol = dynamic_cast<TxEntitySymbol*>( search_symbol( &this->tuplexPackage, "tx.panic" ) );
    ASSERT( panicSymbol, "Function not found: tx.panic" );
    for ( auto declI = panicSymbol->fields_cbegin(); declI != panicSymbol->fields_cend(); declI++ ) {
        auto panicFunc = (*declI)->get_definer()->field();
        auto panicFuncType = dynamic_cast<const TxFunctionType*>( panicFunc->qtype().type() );
        if ( panicFuncType->argument_types().size() == 1 ) {
            auto panicLambdaV = panicFunc->code_gen_field_decl( *this );

            auto msgRefType = panicFuncType->argument_types().at( 0 );
            auto msgRefTargTid = msgRefType->target_type()->get_runtime_type_id();
            auto msgRefT = this->get_llvm_type( msgRefType );
            auto msgPtrC = this->gen_const_cstring_address( message );
            auto msgRefC = gen_ref( *this, msgRefT, msgPtrC, ConstantInt::get( this->i32T, msgRefTargTid ) );

            std::vector<Value*> args( { (Value*)msgRefC } );
            StructType *lambdaT = cast<StructType>( this->get_llvm_type( panicFuncType ) );
            FunctionType* funcType = cast<FunctionType>( lambdaT->getElementType( 0 )->getPointerElementType() );
            gen_lambda_call( *this, scope, funcType, panicLambdaV, args, "", true );
            return;
        }
    }
    THROW_LOGIC( "tx.panic() function of proper signature not found" );
}

void LlvmGenerationContext::gen_panic_call( GenScope* scope, const std::string& message, Value* ulongValV ) {
    auto panicSymbol = dynamic_cast<TxEntitySymbol*>( search_symbol( &this->tuplexPackage, "tx.panic" ) );
    ASSERT( panicSymbol, "Function not found: tx.panic" );
    for ( auto declI = panicSymbol->fields_cbegin(); declI != panicSymbol->fields_cend(); declI++ ) {
        auto panicFunc = (*declI)->get_definer()->field();
        auto panicFuncType = dynamic_cast<const TxFunctionType*>( panicFunc->qtype().type() );
        if ( panicFuncType->argument_types().size() == 2 ) {
            auto panicLambdaV = panicFunc->code_gen_field_decl( *this );

            // TODO: make creating CString references easier
            auto msgRefType = panicFuncType->argument_types().at( 0 );
            auto msgRefTargTid = msgRefType->target_type()->get_runtime_type_id();
            auto msgRefT = this->get_llvm_type( msgRefType );
            auto msgPtrC = this->gen_const_cstring_address( message );
            auto msgRefC = gen_ref( *this, msgRefT, msgPtrC, ConstantInt::get( this->i32T, msgRefTargTid ) );

            std::vector<Value*> args( { (Value*)msgRefC, ulongValV } );
            StructType *lambdaT = cast<StructType>( this->get_llvm_type( panicFuncType ) );
            FunctionType* funcType = cast<FunctionType>( lambdaT->getElementType( 0 )->getPointerElementType() );
            gen_lambda_call( *this, scope, funcType, panicLambdaV, args, "", true );
            return;
        }
    }
    THROW_LOGIC( "tx.panic() function of proper signature not found" );
}


/***** LLVM types and values "symbol table" *****/

void LlvmGenerationContext::register_llvm_value( const std::string& identifier, Value* val ) {
    ASSERT( !identifier.empty(), "Empty identifier string when registering llvm value " << val );
    if ( identifier.compare( 0, strlen( BUILTIN_NS ), BUILTIN_NS ) != 0 )
        LOG_TRACE( this->LOGGER(), "Registering LLVM value '" << identifier << "' : " << val->getType() );
    this->llvmSymbolTable.emplace( identifier, val );
}

Value* LlvmGenerationContext::lookup_llvm_value( const std::string& identifier ) const {
    try {
        Value* val = this->llvmSymbolTable.at( identifier );
        LOG_TRACE( this->LOGGER(), "Looked up LLVM value " << identifier );
        return val;
    }
    catch ( const std::out_of_range& oor ) {
        LOG( this->LOGGER(), FATAL, "Unknown LLVM value identifier " << identifier );
        throw;
    }
}


Type* LlvmGenerationContext::get_llvm_type( const TxActualType* txType ) {
    ASSERT( txType, "NULL txType provided to get_llvm_type()" );
    if ( txType->get_type_class() != TXTC_REFERENCE && txType->is_same_instance_type() )
        // same data type as base type
        return this->get_llvm_type( txType->get_base_type() );

    // note: we do map abstract types (e.g. reference targets)

    auto iter = this->llvmTypeMapping.find( txType );
    if ( iter != this->llvmTypeMapping.end() ) {
        return iter->second;
    }
    Type* llvmType = txType->make_llvm_type( *this );
    this->llvmTypeMapping.emplace( txType, llvmType );
    LOG_TRACE( this->LOGGER(), "Made LLVM type mapping for type " << txType->str(true) << ": " << llvmType );

    Type* llvmTypeBody = txType->make_llvm_type_body( *this, llvmType );
    if ( llvmTypeBody != llvmType ) {
        // replace header with full type definition in mapping
        this->llvmTypeMapping[txType] = llvmTypeBody;
        LOG_NOTE( this->LOGGER(), "Replaced LLVM type header mapping for " << txType << " to body " << llvmTypeBody );
    }

    return llvmType;
}

DIType* LlvmGenerationContext::get_debug_type( const TxActualType* txType ) {
    ASSERT( txType, "NULL txType provided to get_debug_type()" );
    if ( txType->get_type_class() != TXTC_REFERENCE && txType->is_same_instance_type() )
        // same data type as base type
        return this->get_debug_type( txType->get_base_type() );

    auto iter = this->llvmDebugTypeMapping.find( txType );
    if ( iter != this->llvmDebugTypeMapping.end() ) {
        return iter->second;
    }
    DIType* debugType = txType->make_llvm_debug_type( *this );
    this->llvmDebugTypeMapping.emplace( txType, debugType );
    LOG_TRACE( this->LOGGER(), "Made LLVM DEBUG type mapping for type " << txType->str(true) << ": " << debugType );

    DIType* debugTypeBody = txType->make_llvm_debug_type_body( *this, debugType );
    if ( debugTypeBody != debugType ) {
        // replace header with full type definition in mapping
        this->llvmDebugTypeMapping[txType] = debugTypeBody;
        LOG_DEBUG( this->LOGGER(), "Replaced LLVM DEBUG type header mapping for " << txType << " to body " << debugTypeBody );
    }
    return debugTypeBody;
}


/***** main() and other initialization code *****/

/** Add main function so can be fully compiled
 * define i32 @main(i32 %argc, i8 **%argv)
 */
Function* LlvmGenerationContext::gen_main_function( const std::string userMain, const TxActualType* mainFuncType ) {
    //define i32 @main(i32 %argc, i8 **%argv)
    Function *mainFunc = cast<Function>(
            this->llvmModule().getOrInsertFunction(
                    "main",
                    this->i32T,
                    this->i32T,
                    PointerType::getUnqual( PointerType::getUnqual( IntegerType::getInt8Ty( this->llvmContext ) ) ) )
                    .getCallee() );
    Function::arg_iterator args = mainFunc->arg_begin();
    Value* argcV = &(*args);
    argcV->setName( "argc" );
    args++;
    Value* argvA = &(*args);
    argvA->setName( "argv" );

    const bool TX_LIB_MAIN_DEBUG_INFO = true;  // if true, generates debug info in this outer main function
    if ( TX_LIB_MAIN_DEBUG_INFO ) {
        // generate debug info
        SmallVector<Metadata*, 8> eltTypes;
        DIType* intTy = this->debug_builder()->createBasicType( "int", 32, dwarf::DW_ATE_signed );
        eltTypes.push_back( intTy );  // return type
        eltTypes.push_back( intTy );  // argc type
        eltTypes.push_back( /* argv type */ this->debug_builder()->createPointerType(
                this->debug_builder()->createPointerType(
                        this->debug_builder()->createBasicType( "char", 8, dwarf::DW_ATE_signed_char ), 64 ), 64 ) );
        DISubroutineType* subRoutineType = this->debug_builder()->createSubroutineType( this->debug_builder()->getOrCreateTypeArray( eltTypes ) );

        auto linkageName = StringRef();
        // Note - not local to unit, external linkage
        unsigned lineNo = __LINE__;
        unsigned scopeLine = lineNo;
        DISubprogram *subProg = this->debug_builder()->createFunction(
               this->_debugUnit,
               mainFunc->getName(), linkageName, this->_genDIFile,
               lineNo, subRoutineType, scopeLine,
               DINode::FlagPrototyped,
               DISubprogram::SPFlagDefinition
               );
        mainFunc->setSubprogram( subProg );
    }

    BasicBlock *entryBlock = BasicBlock::Create( this->llvmContext, "entry", mainFunc );
    IRBuilder<> builder( entryBlock );
    GenScope scope( &builder, mainFunc->getSubprogram() );

//    // initialize statics / runtime environment
//    Function *initFunc = this->gen_static_init_function();
//    CallInst *initCall = CallInst::Create(initFunc, "", entryBlock);
//    initCall->setTailCall(false);

    //call i32 user main()
    auto userMainFName = userMain + "$func";
    auto userMainFunc = this->llvmModule().getFunction( userMainFName );
    if ( userMainFunc ) {
        auto nullClosureRefV = Constant::getNullValue( this->get_closureRefT() );
        std::vector<Value*> args = { nullClosureRefV };

        if ( userMainFunc->getFunctionType()->getNumParams() > 1 ) {
            // provide program string arguments

            auto mainArgsRefType = mainFuncType->argument_types().at( 0 );
            auto argRefType = mainArgsRefType->target_type()->element_type().type();

            auto i8T = Type::getInt8Ty( this->llvmContext );
            auto i64T = Type::getInt64Ty( this->llvmContext );
            auto i8PtrT = i8T->getPointerTo();

            // declare strlen() function:  size_t strlen ( const char * str );
            Function* strlenFunc = Function::Create( FunctionType::get( i32T, { i8PtrT }, false ),
                                                     GlobalValue::ExternalLinkage, "strlen", &this->llvmModule() );
            strlenFunc->setCallingConv( CallingConv::C );

            // declare strcpy() function:  char * strcpy ( char * destination, const char * source );
            Function* strcpyFunc = Function::Create( FunctionType::get( i8PtrT, std::vector<Type*>( { i8PtrT, i8PtrT } ), false ),
                                                     GlobalValue::ExternalLinkage, "strcpy", &this->llvmModule() );
            strlenFunc->setCallingConv( CallingConv::C );

            //builder.SetCurrentDebugLocation( DebugLoc::get( __LINE__, 0, scope.debug_scope() ) );
            auto ixA = builder.CreateAlloca( i32T, nullptr, "ix" );
            builder.CreateStore( ConstantInt::get( i32T, 0 ), ixA );

            auto ubyteArrayT = StructType::get( i32T, i32T, ArrayType::get( i8T, 0 ) );
            auto ubyteArrayRefT = StructType::get( ubyteArrayT->getPointerTo(), i32T );
            auto argsArrayRefT = cast<StructType>( userMainFunc->getFunctionType()->getParamType( 1 ) );
            auto argsArrayPtrT = argsArrayRefT->getElementType( 0 );

            Value* argsArrayA;
            {
                //builder.SetCurrentDebugLocation( DebugLoc::get( __LINE__, 0, scope.debug_scope() ) );
                Value* arrayCap64V = builder.CreateZExtOrBitCast( argcV, i64T );
                Constant* elemSizeC = ConstantExpr::getSizeOf( ubyteArrayRefT );
                Value* objectSizeV = gen_compute_array_size( *this, &scope, elemSizeC, arrayCap64V );
                argsArrayA = builder.CreatePointerCast( builder.Insert( new AllocaInst( i8T, 0, objectSizeV, Align(8) ) ), argsArrayPtrT, "args" );
                initialize_array_obj( *this, &scope, argsArrayA, argcV, argcV );
            }

            auto argBlock = BasicBlock::Create( this->llvmContext, "arg", mainFunc );
            auto postBlock = BasicBlock::Create( this->llvmContext, "post", mainFunc );

            {
                //builder.SetCurrentDebugLocation( DebugLoc::get( __LINE__, 0, scope.debug_scope() ) );
                auto condV = builder.CreateICmpULT( ConstantInt::get( i32T, 0 ), argcV );
                builder.CreateCondBr( condV, argBlock, postBlock );
            }
            {
                builder.SetInsertPoint( argBlock );
                //builder.SetCurrentDebugLocation( DebugLoc::get( __LINE__, 0, scope.debug_scope() ) );
                auto ixV = builder.CreateLoad( ixA );

                {  // get length of cstring arg, allocate tx byte array, copy content, and write ref to tx args array element:
                    auto argCStrA = builder.CreateLoad( builder.CreateGEP( argvA, ixV ) );
                    auto argCStrLenV = builder.CreateCall( strlenFunc, { argCStrA } );
                    auto argCMemLenV = builder.CreateAdd( argCStrLenV, ConstantInt::get( i32T, 1 ) );
                    Value* arrayCap64V = builder.CreateZExtOrBitCast( argCMemLenV, i64T );
                    Constant* elemSizeC = ConstantExpr::getSizeOf( i8T );
                    Value* objectSizeV = gen_compute_array_size( *this, &scope, elemSizeC, arrayCap64V );
                    Value* argArrayObjA = builder.CreatePointerCast( builder.Insert( new AllocaInst( i8T, 0, objectSizeV, Align(8) ) ),
                                                                     ubyteArrayT->getPointerTo(), "arg" );
                    initialize_array_obj( *this, &scope, argArrayObjA, argCMemLenV, argCStrLenV );

                    auto argByteArrayA = builder.CreateGEP( argArrayObjA, std::vector<Value*>( { ConstantInt::get( i32T, 0 ),
                                                                                                 ConstantInt::get( i32T, 2 ) } ) );
                    auto argBytesA = builder.CreatePointerCast( argByteArrayA, i8PtrT );
                    builder.CreateCall( strcpyFunc, std::vector<Value*>( { argBytesA, argCStrA } ) );
                    auto argRefV = gen_ref( *this, &scope, argRefType, argArrayObjA );
                    auto dstIxs = std::vector<Value*>( { ConstantInt::get( i32T, 0 ), ConstantInt::get( i32T, 2 ), ixV } );
                    builder.CreateStore( argRefV, builder.CreateGEP( argsArrayA, dstIxs ) );
                }

                // increment index and iterate/exit:
                //builder.SetCurrentDebugLocation( DebugLoc::get( __LINE__, 0, scope.debug_scope() ) );
                auto ixV1 = builder.CreateAdd( ixV, ConstantInt::get( i32T, 1 ) );
                builder.CreateStore( ixV1, ixA );
                auto condV = builder.CreateICmpULT( ixV1, argcV );
                builder.CreateCondBr( condV, argBlock, postBlock );
            }

            builder.SetInsertPoint( postBlock );
            //builder.SetCurrentDebugLocation( DebugLoc::get( __LINE__, 0, scope.debug_scope() ) );
            // FIXME: If not in share-value-specializations-code mode, the ref type here must be for an array type with specific length:
            auto argsArrayRefV = gen_ref( *this, &scope, mainArgsRefType, argsArrayA );
            args.push_back( argsArrayRefV );
        }

        // debug location required upon user main invocation
        if ( TX_LIB_MAIN_DEBUG_INFO )
            builder.SetCurrentDebugLocation( DebugLoc::get( __LINE__, 0, scope.debug_scope() ) );

        if ( mainFuncType->has_return_value() ) {
            CallInst *userMainCall = builder.CreateCall( userMainFunc, args, "usermain" );
            userMainCall->setTailCall( false );
            userMainCall->setIsNoInline();

            if ( TX_LIB_MAIN_DEBUG_INFO )
                builder.SetCurrentDebugLocation( DebugLoc::get( __LINE__, 0, scope.debug_scope() ) );
            auto truncVal = builder.CreateIntCast( userMainCall, i32T, true, "ret" );  // truncate return value to i32
            builder.CreateRet( truncVal );
        }
        else {
            CallInst *userMainCall = builder.CreateCall( userMainFunc, args );
            userMainCall->setTailCall( false );
            userMainCall->setIsNoInline();

            if ( TX_LIB_MAIN_DEBUG_INFO )
                builder.SetCurrentDebugLocation( DebugLoc::get( __LINE__, 0, scope.debug_scope() ) );
            builder.CreateRet( ConstantInt::get( i32T, 0, true ) );
        }
    }
    else {
        this->LOGGER()->error( "LLVM function not found for name: %s", userMain.c_str() );
        if ( TX_LIB_MAIN_DEBUG_INFO )
            builder.SetCurrentDebugLocation( DebugLoc::get( __LINE__, 0, scope.debug_scope() ) );
        builder.CreateRet( ConstantInt::get( i32T, 0, true ) );
    }

    return mainFunc;
}


void LlvmGenerationContext::declare_builtin_code() {
    StructType* genArrayT = StructType::get( i32T, i32T, ArrayType::get( StructType::get( this->llvmContext ), 0 ) );
    PointerType* genArrayPtrT = PointerType::getUnqual( genArrayT );
    {
        Function* function = cast<Function>( this->llvmModule().getOrInsertFunction(
                "$array_elementary_equals",
                IntegerType::getInt1Ty( this->llvmContext ), // return value - Bool
                genArrayPtrT,
                genArrayPtrT,
                i32T )
                    .getCallee() );
        function->setLinkage( GlobalValue::InternalLinkage );
    }
    {
        Function* function = cast<Function>( this->llvmModule().getOrInsertFunction(
                "$array_any_equals",
                IntegerType::getInt1Ty( this->llvmContext ), // return value - Bool
                genArrayPtrT,
                genArrayPtrT,
                i32T,
                i32T )
                     .getCallee() );
        function->setLinkage( GlobalValue::InternalLinkage );
    }
    {
        Function* function = cast<Function>( this->llvmModule().getOrInsertFunction(
                "$get_supertypes_array",
                superTypesPtrT, // return value
                i32T )
                     .getCallee() );       // type id
        function->setLinkage( GlobalValue::InternalLinkage );
    }
}

void LlvmGenerationContext::generate_builtin_code() {
    this->gen_get_supertypes_array_function();
    this->gen_array_elementary_equals_function();
    this->gen_array_any_equals_function();
}

void LlvmGenerationContext::gen_get_supertypes_array_function() {
    Function* function = this->llvmModule().getFunction( "$get_supertypes_array" );
    Function::arg_iterator args = function->arg_begin();
    Value* runtimeBaseTypeIdV = &( *args );
    runtimeBaseTypeIdV->setName( "runtimeBaseTypeIdV" );

    const bool TX_DEBUG_INFO = true;  // if true, generates this debug info
    if ( TX_DEBUG_INFO )
    { // debug info
        auto globScope = this->_debugUnit;
        SmallVector<Metadata*, 2> eltTypes;
        auto uintDT = this->debug_builder()->createBasicType( "UInt", 32, dwarf::DW_ATE_unsigned );
        {  // return type:
            DINodeArray subscripts;  // ??
            SmallVector<Metadata*, 3> arrayElts( { uintDT, uintDT, this->debug_builder()->createArrayType( 0, 64, uintDT, subscripts ) } );
            eltTypes.push_back( this->debug_builder()->createPointerType(
                    this->debug_builder()->createStructType( globScope, "[]UInt", _genDIFile, __LINE__, 64, 64,
                                                             DINode::DIFlags::FlagZero, nullptr,
                                                             debug_builder()->getOrCreateArray( arrayElts ) ), 64 ) );
        }
        eltTypes.push_back( uintDT );  // arg type
        DISubroutineType* subRoutineType = this->debug_builder()->createSubroutineType( this->debug_builder()->getOrCreateTypeArray( eltTypes ) );

        auto linkageName = StringRef();
        unsigned lineNo = __LINE__;
        unsigned scopeLine = lineNo;
        DISubprogram *subProg = this->debug_builder()->createFunction(
                globScope,
                function->getName(), linkageName, _genDIFile,
                lineNo, subRoutineType, scopeLine,
                DINode::FlagPrototyped,
                DISubprogram::SPFlagLocalToUnit | DISubprogram::SPFlagDefinition
               );
        function->setSubprogram( subProg );
    }

    BasicBlock *entryBlock = BasicBlock::Create( this->llvmContext, "entry", function );
    IRBuilder<> builder( entryBlock );
    GenScope scope( &builder, function->getSubprogram() );

    auto parentFunc = builder.GetInsertBlock()->getParent();
    BasicBlock* nonVTableBlock = BasicBlock::Create( this->llvmContext, "if_nonvtabletype", parentFunc );
    BasicBlock* vTableBlock = BasicBlock::Create( this->llvmContext, "if_vtabletype", parentFunc );

    if ( TX_DEBUG_INFO )
        builder.SetCurrentDebugLocation( DebugLoc::get( __LINE__, 0, scope.debug_scope() ) );

    auto superTypesC = cast<GlobalVariable>( this->lookup_llvm_value( "tx.runtime.SUPER_TYPES" ) );
    {
        auto supertypesLenC = ConstantInt::get( i32T, this->tuplexPackage.registry().vtable_types_count() );
        auto condV = builder.CreateICmpUGE( runtimeBaseTypeIdV, supertypesLenC );
        builder.CreateCondBr( condV, nonVTableBlock, vTableBlock );
    }
    { // special handling for Refs and Functions:
        builder.SetInsertPoint( nonVTableBlock );

        BasicBlock* refTypeBlock = BasicBlock::Create( this->llvmContext, "if_reftype", parentFunc );
        BasicBlock* nonrefTypeBlock = BasicBlock::Create( this->llvmContext, "if_nonreftype", parentFunc );
        BasicBlock* funcTypeBlock = BasicBlock::Create( this->llvmContext, "if_functype", parentFunc );
        BasicBlock* otherTypeBlock = BasicBlock::Create( this->llvmContext, "if_othertype", parentFunc );

        auto refCondV = builder.CreateICmpULT( runtimeBaseTypeIdV, ConstantInt::get( i32T, this->tuplexPackage.registry().ref_types_limit() ) );
        builder.CreateCondBr( refCondV, refTypeBlock, nonrefTypeBlock );
        {
            builder.SetInsertPoint( refTypeBlock );
            auto refTypeIdC = ConstantInt::get( i32T, TXBT_REFERENCE );
            builder.CreateRet( superTypesC->getInitializer()->getAggregateElement( refTypeIdC ) );
        }
        {
            builder.SetInsertPoint( nonrefTypeBlock );
            auto refCondV = builder.CreateICmpULT( runtimeBaseTypeIdV, ConstantInt::get( i32T, this->tuplexPackage.registry().func_types_limit() ) );
            builder.CreateCondBr( refCondV, funcTypeBlock, otherTypeBlock );
        }
        {
            builder.SetInsertPoint( funcTypeBlock );
            auto funcTypeIdC = ConstantInt::get( i32T, TXBT_FUNCTION );
            builder.CreateRet( superTypesC->getInitializer()->getAggregateElement( funcTypeIdC ) );
        }
        {
            // type id out of bounds (not a vtable type nor reference nor function type)
            builder.SetInsertPoint( otherTypeBlock );
            auto indexV = builder.CreateZExt( runtimeBaseTypeIdV, Type::getInt64Ty( this->llvmContext ) );
            this->gen_panic_call( &scope, "RTTI supertypes array index out of bounds: %d\n", indexV );
            builder.CreateRet( Constant::getNullValue( this->superTypesPtrT ) );  // terminate block, though won't be executed
        }
    }
    {
        builder.SetInsertPoint( vTableBlock );
        Value* ixs[] = { ConstantInt::get( i32T, 0 ),
                         runtimeBaseTypeIdV };
        auto valA = builder.CreateInBoundsGEP( superTypesC, ixs );
        builder.CreateRet( builder.CreateLoad( valA ) );
    }
}

void LlvmGenerationContext::gen_array_any_equals_function() {
    // This function is invoked when it is NOT known what one or both of the arrays' element types are,
    // and there is a chance they are elementary types. If they are both of the same elementary type,
    // $array_elementary_equals() is called, otherwise the general Array.equals() is called.

    Function* function = this->llvmModule().getFunction( "$array_any_equals" );
    Function::arg_iterator args = function->arg_begin();
    Value* arrayA = &( *args );
    arrayA->setName( "arrayA" );
    args++;
    Value* arrayB = &( *args );
    arrayB->setName( "arrayB" );
    args++;
    Value* arrATypeIdV = &( *args );
    arrATypeIdV->setName( "arrayATypeId" );
    args++;
    Value* arrBTypeIdV = &( *args );
    arrBTypeIdV->setName( "arrayATypeId" );

    BasicBlock *entryBlock = BasicBlock::Create( this->llvmContext, "entry", function );
    IRBuilder<> builder( entryBlock );
    GenScope scope( &builder );

    auto parentFunc = builder.GetInsertBlock()->getParent();
    BasicBlock* elemDaTyIdsBlock  = BasicBlock::Create( this->llvmContext, "if_ArrElDaTyIds",   parentFunc );
    BasicBlock* elemTyClEqBlock   = BasicBlock::Create( this->llvmContext, "if_ArrElTyClEq",    parentFunc );
    BasicBlock* elemElemBlock     = BasicBlock::Create( this->llvmContext, "if_ArrElemElem",    parentFunc );
    BasicBlock* elemSameElemBlock = BasicBlock::Create( this->llvmContext, "if_ArrElemSameElem",parentFunc );
    BasicBlock* elemOtherBlock    = BasicBlock::Create( this->llvmContext, "if_ArrElemOther",   parentFunc );
    BasicBlock* arrUneqBlock      = BasicBlock::Create( this->llvmContext, "if_ArrUneq",        parentFunc );

    // When one or both the operands are dereferenced references to generic arrays (&Array),
    // the references' types must be examined in runtime for array element type class equality,
    // and if elementary also for element type equality.
    auto arrAElemTyIdV = this->gen_get_element_type_id( &scope, nullptr, arrATypeIdV );
    auto arrBElemTyIdV = this->gen_get_element_type_id( &scope, nullptr, arrBTypeIdV );

    {
        // check if both element types are data types, i.e. have RTTI; otherwise revert to arrayA.equals( arrayB)
        auto typeInfoLenC = cast<GlobalVariable>( this->lookup_llvm_value( "tx.runtime.TYPE_COUNT" ) )->getInitializer();
        auto elADaTyIdCondV = builder.CreateICmpULT( arrAElemTyIdV, typeInfoLenC );
        auto elBDaTyIdCondV = builder.CreateICmpULT( arrBElemTyIdV, typeInfoLenC );
        auto elDaTyIdsCondV = builder.CreateAnd( elADaTyIdCondV, elBDaTyIdCondV );
        builder.CreateCondBr( elDaTyIdsCondV, elemDaTyIdsBlock, elemOtherBlock );
    }

    {
        // both element types are data types, i.e. have RTTI
        builder.SetInsertPoint( elemDaTyIdsBlock );
        // FIXME: replace type class fetching with direct ELEMENTARY type class check in order to avoid RTTI out of bounds
        auto arrAElemTyClV = this->gen_get_type_class( &scope, nullptr, arrAElemTyIdV );
        auto arrBElemTyClV = this->gen_get_type_class( &scope, nullptr, arrBElemTyIdV );
        auto elTyClEqCondV = builder.CreateICmpEQ( arrAElemTyClV, arrBElemTyClV );
        builder.CreateCondBr( elTyClEqCondV, elemTyClEqBlock, arrUneqBlock );

        // both element types are of the same type class
        builder.SetInsertPoint( elemTyClEqBlock );
        auto elemTyClC = ConstantInt::get( Type::getInt8Ty( this->llvmContext ), TXTC_ELEMENTARY );
        auto elemElemCondV = builder.CreateICmpEQ( arrAElemTyClV, elemTyClC );
        builder.CreateCondBr( elemElemCondV, elemElemBlock, elemOtherBlock );
    }
    {
        // both element types are Elementary, check that the types are equal
        builder.SetInsertPoint( elemElemBlock );
        auto elTyEqCondV = builder.CreateICmpEQ( arrAElemTyIdV, arrBElemTyIdV );
        builder.CreateCondBr( elTyEqCondV, elemSameElemBlock, arrUneqBlock );
    }
    {
        // both element types are of the same Elementary type, call $array_elementary_equals()
        builder.SetInsertPoint( elemSameElemBlock );
        std::vector<Value*> args( { arrayA, arrayB, arrATypeIdV } );
        auto arrEqFuncA = this->llvmModule().getFunction( "$array_elementary_equals" );
        ASSERT( arrEqFuncA, "$array_elementary_equals() function not found" );
        builder.CreateRet( builder.CreateCall( arrEqFuncA, args ) );
    }
    {
        builder.SetInsertPoint( elemOtherBlock );
        // invoke virtual method arrayA.equals( arrayB )
        auto resultV = this->gen_equals_invocation( &scope, arrayA, arrATypeIdV, arrayB, arrBTypeIdV );
        builder.CreateRet( resultV );
    }
    {
        builder.SetInsertPoint( arrUneqBlock );
        builder.CreateRet( ConstantInt::get( Type::getInt1Ty( this->llvmContext ), 0 ) );  // return FALSE
    }
}

void LlvmGenerationContext::gen_array_elementary_equals_function() {
    // This function is invoked when it is known that both the arrays' element types are of the same Elementary type.

    Function* function = this->llvmModule().getFunction( "$array_elementary_equals" );
    Function::arg_iterator args = function->arg_begin();
    Value* arrayA = &( *args );
    arrayA->setName( "arrayA" );
    args++;
    Value* arrayB = &( *args );
    arrayB->setName( "arrayB" );
    args++;
    Value* arrATypeIdV = &( *args );
    arrATypeIdV->setName( "arrayATypeId" );

    BasicBlock *entryBlock = BasicBlock::Create( this->llvmContext, "entry", function );
    IRBuilder<> builder( entryBlock );
    GenScope scope( &builder );

    auto parentFunc = builder.GetInsertBlock()->getParent();
    BasicBlock* lenEqBlock   = BasicBlock::Create( this->llvmContext, "if_ArrLenEq", parentFunc );
    BasicBlock* arrUneqBlock = BasicBlock::Create( this->llvmContext, "if_ArrUneq",  parentFunc );

    Value* lenIxs[] = { ConstantInt::get( this->i32T, 0 ),
                        ConstantInt::get( this->i32T, 1 ) };
    auto arrALengthV = builder.CreateLoad( builder.CreateInBoundsGEP( arrayA, lenIxs ) );
    {
        auto arrBLengthV = builder.CreateLoad( builder.CreateInBoundsGEP( arrayB, lenIxs ) );
        auto lenCondV = builder.CreateICmpEQ( arrALengthV, arrBLengthV );
        builder.CreateCondBr( lenCondV, lenEqBlock, arrUneqBlock );
    }
    {
        builder.SetInsertPoint( lenEqBlock );
        auto elemActType = this->tuplexPackage.registry().get_builtin_type( TXBT_ELEMENTARY );
        auto elemSizeV = this->gen_get_element_size( &scope, elemActType, arrATypeIdV );
        auto elemSize64V = builder.CreateZExtOrBitCast( elemSizeV, Type::getInt64Ty( this->llvmContext ) );
        auto arrALength64V = builder.CreateZExtOrBitCast( arrALengthV, Type::getInt64Ty( this->llvmContext ) );
        auto dataSizeV = builder.CreateMul( elemSize64V, arrALength64V, "datasize" );

        Value* dataIxs[] = { ConstantInt::get( this->i32T, 0 ),
                             ConstantInt::get( this->i32T, 2 ) };
        auto arrADataA = builder.CreatePointerCast( builder.CreateInBoundsGEP( arrayA, dataIxs ), this->get_voidPtrT() );
        auto arrBDataA = builder.CreatePointerCast( builder.CreateInBoundsGEP( arrayB, dataIxs ), this->get_voidPtrT() );
        std::vector<Value*> args( { arrADataA, arrBDataA, dataSizeV } );
        auto memcmpFuncA = this->llvmModule().getFunction( "memcmp" );
        ASSERT( memcmpFuncA, "memcmp() function not found in " << this );
        auto callV = builder.CreateCall( memcmpFuncA, args );
        auto condV = builder.CreateICmpEQ( callV, ConstantInt::get( this->i32T, 0 ) );
        builder.CreateRet( condV );
    }
    {
        builder.SetInsertPoint( arrUneqBlock );
        builder.CreateRet( ConstantInt::get( Type::getInt1Ty( this->llvmContext ), 0 ) );
    }
}
