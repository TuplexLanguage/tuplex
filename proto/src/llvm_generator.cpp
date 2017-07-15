#include <iostream>
#include <stack>
#include <unordered_map>
#include <typeinfo>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>

#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Target/TargetMachine.h>

#include "util/util.hpp"
#include "util/assert.hpp"

#include "tx_lang_defs.hpp"
#include "tx_except.hpp"
#include "llvm_generator.hpp"

#include "ast/ast_modbase.hpp"
#include "ast/expr/ast_ref.hpp"
#include "symbol/package.hpp"

using namespace llvm;


/***** Compile the AST into a module *****/

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

bool LlvmGenerationContext::generate_main( const std::string& userMainIdent, const TxType* mainFuncType ) {
    this->entryFunction = this->gen_main_function( userMainIdent, ( mainFuncType->return_type()->get_type_class() != TXTC_VOID ) );
    return this->entryFunction;
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
    auto relocModel = Optional<Reloc::Model>();
    auto targetMachine = target->createTargetMachine( targetTriple, cpu, features, opt, relocModel );

    this->llvmModulePtr->setDataLayout( targetMachine->createDataLayout() );
    this->llvmModulePtr->setTargetTriple( targetTriple );
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
    // TODO: support writing to a .ll file
    this->LOGGER()->info( "Printing LLVM bytecode..." );
    PrintModulePass printPass( outs() );
    ModulePassManager pm;
    pm.addPass( printPass );
    AnalysisManager<Module> am;
    pm.run( this->llvmModule(), am );
    std::cout << std::endl;
}

int LlvmGenerationContext::write_bitcode( const std::string& filepath ) {
    LOG_DEBUG( this->LOGGER(), "Writing LLVM bitcode file '" << filepath << "'" );
    std::error_code errInfo;
    raw_fd_ostream ostream( filepath.c_str(), errInfo, sys::fs::F_RW );
    if ( errInfo ) {
        LOG( this->LOGGER(), ERROR, "Failed to open bitcode output file for writing: " << errInfo.message() );
        return 1;
    }
    else {
        WriteBitcodeToFile( &this->llvmModule(), ostream );
        return 0;
    }
}


/***** generate runtime type data *****/

void LlvmGenerationContext::initialize_runtime_data() {
    /* This is a possible future C declaration equivalent of the constructed runtime type data:

     typedef void (*TypeInitializerF)(void* memory);

     typedef struct {
     uint32_t typeId;
     void* vtable;
     uint32_t size;
     TypeInitializerF initializer;
     } MetaType;

     extern uint32_t TYPE_COUNT;
     extern MetaType META_TYPES[];
     */

    // define the MetaType LLVM data type:
    auto int32T = Type::getInt32Ty( this->llvmContext );
//    std::vector<Type*> typeInitFuncArgTypes {
//        this->get_voidPtrT()  // void* to type's data
//    };
//    FunctionType *typeInitFuncT = FunctionType::get(this->get_voidT(), typeInitFuncArgTypes, false);
//    auto dummyUserInitF = gen_dummy_type_user_init_func(*this);

    auto emptyStructPtrT = PointerType::getUnqual( StructType::get( this->llvmContext ) );
    std::vector<Type*> memberTypes {
                                     int32T,  // type id
            emptyStructPtrT,  // vtable pointer
    //int32T,  // data structure size
    //typeInitFuncT,  // initialization function
    };
    StructType* metaType = StructType::get( this->llvmContext, memberTypes );

    // create static meta type data:
    std::vector<Constant*> metaTypes;
    for ( auto txType = this->tuplexPackage.registry().vtable_types_cbegin(); txType != this->tuplexPackage.registry().vtable_types_cend();
            txType++ )
    {
        const TxActualType* acttype = *txType;
//        auto utinitF = acttype->get_type_user_init_func(*this);
//        if (! utinitF->getEntryBlock().getTerminator()) {
//            // inserting default void return instruction for entry block of function
//            ReturnInst::Create(this->llvmContext, &utinitF->getEntryBlock());
//        }

        // create the vtable type:
        auto vtableT = acttype->make_vtable_type( *this );
        if ( !vtableT )
            continue;
        //std::cerr << "vtable type for " << acttype << " (vtable id=" << acttype->get_vtable_id() << "): " << vtableT << std::endl;
        ASSERT( this->llvmVTableTypes.size() == acttype->get_vtable_id(), "Wrong vtable type id / order for " << acttype );
        this->llvmVTableTypes.push_back( vtableT );

        // if the type is a formal type, create an actual vtable instance:
        if ( acttype->has_formal_type_id() ) {
            auto typeId = acttype->get_formal_type_id();
            std::string vtableName( acttype->get_declaration()->get_unique_full_name() + "$vtable" );
            GlobalVariable* vtableV = new GlobalVariable( this->llvmModule(), vtableT, true, GlobalValue::ExternalLinkage, nullptr, vtableName );
            this->register_llvm_value( vtableV->getName(), vtableV );

            std::vector<Constant*> members {
                                             ConstantInt::get( int32T, typeId ),
                                             ConstantExpr::getBitCast( vtableV, emptyStructPtrT ),
            // dummyUserInitF  // utinitF
            };
            metaTypes.push_back( ConstantStruct::get( metaType, members ) );

//            // register the constant typeId values for later inclusion in the initialization code:
//            std::string typeIdName( acttype->get_declaration()->get_unique_full_name() + ".$typeid" );
//            this->register_llvm_value( typeIdName, ConstantInt::get( int32T, typeId ) );
        }
    }

    auto typeCount = this->tuplexPackage.registry().formal_type_count();
    auto mtArrayType = ArrayType::get( metaType, typeCount );
    auto mtArrayInit = ConstantArray::get( mtArrayType, metaTypes );

    Value* typeCountV = new GlobalVariable( this->llvmModule(), int32T, true, GlobalValue::ExternalLinkage,
                                            ConstantInt::get( int32T, typeCount ),
                                            "tx.runtime.TYPE_COUNT" );
    Value* metaTypesV = new GlobalVariable( this->llvmModule(), mtArrayType, true, GlobalValue::ExternalLinkage,
                                            mtArrayInit,
                                            "tx.runtime.META_TYPES" );
    this->register_llvm_value( typeCountV->getName(), typeCountV );
    this->register_llvm_value( metaTypesV->getName(), metaTypesV );
}


void LlvmGenerationContext::generate_runtime_data() {
    for ( auto txTypeI = this->tuplexPackage.registry().formal_types_cbegin(); txTypeI != this->tuplexPackage.registry().formal_types_cend();
            txTypeI++ ) {
        const TxActualType* txType = *txTypeI;
        ASSERT( txType->is_prepared(), "Non-prepared type: " << txType );
        std::string vtableName( txType->get_declaration()->get_unique_full_name() + "$vtable" );
        if ( auto vtableV = dyn_cast<GlobalVariable>( this->lookup_llvm_value( vtableName ) ) ) {
            bool isGeneric = txType->is_type_generic();
            std::string typeIdStr = std::to_string( txType->get_formal_type_id() );
            if ( typeIdStr.size() < 5 )
                typeIdStr.resize( 5, ' ' );
            if ( isGeneric )  // the only generic types that have a formal type id are Ref and Array
                LOG_DEBUG( this->LOGGER(), "Type id " << typeIdStr << ": Populating vtable initializer with null placeholders for generic base type " << txType );
            else
                LOG_DEBUG( this->LOGGER(), "Type id " << typeIdStr << ": Populating vtable initializer for " << txType );
            std::vector<Constant*> initMembers;
            auto virtualFields = txType->get_virtual_fields();
            initMembers.resize( virtualFields.get_field_count() );
            for ( auto & field : virtualFields.fieldMap ) {
                auto actualFieldEnt = virtualFields.get_field( field.second );
                Constant* llvmFieldC;
                if ( ( actualFieldEnt->get_decl_flags() & TXD_ABSTRACT ) || isGeneric ) {
                    //std::cerr << "inserting NULL for abstract virtual field: " << field.first << " at ix " << field.second << ": " << actualFieldEnt << std::endl;
                    Type* fieldType;
                    if ( actualFieldEnt->get_storage() & TXS_INSTANCEMETHOD ) {
                        auto closureType = this->get_llvm_type( actualFieldEnt->qualtype()->type() );
                        fieldType = closureType->getStructElementType( 0 );
                    }
                    else if ( field.first == "$adTypeId" ) {
                        fieldType = this->get_llvm_type( actualFieldEnt->qualtype()->type() );
                    }
                    else
                        fieldType = PointerType::getUnqual( this->get_llvm_type( actualFieldEnt->qualtype()->type() ) );
                    llvmFieldC = Constant::getNullValue( fieldType );
                }
                else if ( field.first == "$adTypeId" ) {
                    ASSERT( txType->get_type_class() == TXTC_INTERFACEADAPTER, "Expected InterfaceAdapter type: " << txType );
                    auto adapterType = static_cast<const TxInterfaceAdapterType*>( txType );
                    llvmFieldC = ConstantInt::get( Type::getInt32Ty( this->llvmContext ), adapterType->adapted_type()->get_formal_type_id() );
                    //valueName = adapterType->adapted_type()->get_declaration()->get_unique_full_name() + ".$typeid";
                    //auto llvmValue = this->lookup_llvm_value( valueName );
                    //std::cerr << "$typeid=" << llvmValue << " for adapted type " << adapterType->adapted_type()
                    //          << " with id " << adapterType->adapted_type()->get_formal_type_id() << std::endl;
                }
                else {
                    std::string valueName;
                    if ( actualFieldEnt->get_storage() & TXS_INSTANCEMETHOD ) {
                        //std::cerr << "inserting instance method: " << field.first << " at ix " << field.second << ": " << actualFieldEnt << std::endl;
                        valueName = actualFieldEnt->get_declaration()->get_unique_full_name() + "$func";
                    }
                    else {
                        //std::cerr << "inserting virtual field: " << field.first << " at ix " << field.second << ": " << actualFieldEnt << std::endl;
                        valueName = actualFieldEnt->get_declaration()->get_unique_full_name();
                    }
                    auto llvmValue = actualFieldEnt->get_llvm_value();
                    llvmFieldC = cast<Constant>( llvmValue );
                }
                //std::cerr << "inserting field: " << field.first << " at ix " << field.second << ": " << actualFieldEnt << std::endl;
                auto ix = field.second;
                initMembers[ix] = llvmFieldC;
            }
            Constant* initializer = ConstantStruct::getAnon( this->llvmContext, initMembers );
            vtableV->setInitializer( initializer );
            //std::cerr << "initializing " << vtableV << " with " << initializer << std::endl;
        }
        else
            this->LOGGER()->error( "No vtable found for %s", vtableName.c_str() );
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


Value* LlvmGenerationContext::gen_get_vtable( GenScope* scope, const TxActualType* statDeclType, Value* runtimeBaseTypeIdV ) const {
    // cast vtable type according to statically declared type (may be parent type of actual type):
    Type* vtablePtrT = PointerType::getUnqual( this->get_llvm_vtable_type( statDeclType ) );
    //std::cerr << "got vtable ptr type for " << statDeclType << " (id " << statDeclType->get_type_id() << "): " << vtablePtrT << std::endl;

    Value* metaTypesV = this->lookup_llvm_value( "tx.runtime.META_TYPES" );
    Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( this->llvmContext ), 0 ),
                     runtimeBaseTypeIdV,
                     ConstantInt::get( Type::getInt32Ty( this->llvmContext ), 1 ) };
    if ( !scope ) {
        auto vtablePtrA = GetElementPtrInst::CreateInBounds( metaTypesV, ixs );
        auto vtablePtr = new LoadInst( vtablePtrA );
        return CastInst::CreatePointerCast( vtablePtr, vtablePtrT, "vtableptr" );
    }
    else {
        auto vtablePtrA = scope->builder->CreateInBoundsGEP( metaTypesV, ixs );
        auto vtablePtr = scope->builder->CreateLoad( vtablePtrA );
        return scope->builder->CreatePointerCast( vtablePtr, vtablePtrT, "vtableptr" );
    }
}

Value* LlvmGenerationContext::gen_get_vtable( GenScope* scope, const TxActualType* statDeclType ) const {
    return gen_get_vtable( scope, statDeclType, ConstantInt::get( Type::getInt32Ty( this->llvmContext ), statDeclType->get_formal_type_id() ) );
}

StructType* LlvmGenerationContext::get_llvm_vtable_type( const TxActualType* txType ) const {
    return this->llvmVTableTypes.at( txType->get_vtable_id() );
}

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


/***** LLVM types and values "symbol table" *****/

void LlvmGenerationContext::register_llvm_value( const std::string& identifier, Value* val ) {
    ASSERT( !identifier.empty(), "Empty identifier string when registering llvm value " << val );
    if ( identifier.compare( 0, strlen( BUILTIN_NS ), BUILTIN_NS ) != 0 )
        LOG_TRACE( this->LOGGER(), "Registering LLVM value '" << identifier << "' : " << to_string(val->getType()) );
    this->llvmSymbolTable.emplace( identifier, val );
}

Value* LlvmGenerationContext::lookup_llvm_value( const std::string& identifier ) const {
    try {
        Value* val = this->llvmSymbolTable.at( identifier );
        LOG_TRACE( this->LOGGER(), "Looked up LLVM value " << identifier );
        return val;
    }
    catch ( const std::out_of_range& oor ) {
        LOG_DEBUG( this->LOGGER(), "Unknown LLVM value identifier " << identifier );
        return nullptr;
    }
}


Type* LlvmGenerationContext::get_llvm_type( const TxQualType* txType ) {
    return this->get_llvm_type( txType->type()->acttype() );
}

Type* LlvmGenerationContext::get_llvm_type( const TxType* txType ) {
    return this->get_llvm_type( txType->acttype() );
}

Type* LlvmGenerationContext::get_llvm_type( const TxActualType* txType ) {
    ASSERT( txType, "NULL txType provided to getLlvmType()" );
    if ( txType->get_type_class() != TXTC_REFERENCE && txType->is_same_instance_type() )
        // same data type as base type
        return this->get_llvm_type( txType->get_base_type() );

    // note: we do map abstract types (e.g. reference targets)

    auto iter = this->llvmTypeMapping.find( txType );
    if ( iter != this->llvmTypeMapping.end() ) {
        return iter->second;
    }
    Type* llvmType = txType->make_llvm_type( *this );
    ASSERT ( llvmType, "Failed to make LLVM type mapping for type " << txType );
    this->llvmTypeMapping.emplace( txType, llvmType );
    if (txType->get_type_class() != TXTC_FUNCTION)
        LOG_DEBUG( this->LOGGER(), "Made LLVM type mapping for type " << txType->str(true) << ": " << to_string(llvmType) );
    else
        LOG_TRACE( this->LOGGER(), "Made LLVM type mapping for type " << txType->str(true) << ": " << to_string(llvmType) );

    Type* llvmTypeBody = txType->make_llvm_type_body( *this, llvmType );
    if ( llvmTypeBody != llvmType ) {
        // replace header with full type definition in mapping
        this->llvmTypeMapping[txType] = llvmTypeBody;
        this->LOGGER()->note( "replaced LLVM type mapping for type %s: %s", txType->str( true ).c_str(), to_string( llvmTypeBody ).c_str() );
    }

    return llvmType;
}


/***** main() and other initialization code *****/

/** Add main function so can be fully compiled
 * define i32 @main(i32 %argc, i8 **%argv)
 */
Function* LlvmGenerationContext::gen_main_function( const std::string userMain, bool hasIntReturnValue ) {
    //define i32 @main(i32 %argc, i8 **%argv)
    Function *main_func = cast<Function>(
            this->llvmModule().getOrInsertFunction(
                    "main",
                    IntegerType::getInt32Ty( this->llvmModule().getContext() ),
                    IntegerType::getInt32Ty( this->llvmModule().getContext() ),
                    PointerType::getUnqual( PointerType::getUnqual( IntegerType::getInt8Ty( this->llvmModule().getContext() ) ) ),
                    NULL ) );
    {
        Function::arg_iterator args = main_func->arg_begin();
        Value *arg_0 = &(*args);
        arg_0->setName( "argc" );
        args++;
        Value *arg_1 = &(*args);
        arg_1->setName( "argv" );
        args++;
    }
    BasicBlock *bb = BasicBlock::Create( this->llvmModule().getContext(), "entry", main_func );

//    // initialize statics / runtime environment
//    Function *initFunc = this->gen_static_init_function();
//    CallInst *initCall = CallInst::Create(initFunc, "", bb);
//    initCall->setTailCall(false);

    //call i32 user main()
    auto userMainFName = userMain + "$func";
    auto func = this->llvmModule().getFunction( userMainFName );
    if ( func ) {
        auto nullClosureRefV = Constant::getNullValue( this->get_voidRefT() );
        Value* args[] = { nullClosureRefV };
        CallInst *user_main_call = CallInst::Create( func, args, "", bb );
        user_main_call->setTailCall( false );
        user_main_call->setIsNoInline();
        auto int32T = Type::getInt32Ty( this->llvmModule().getContext() );
        if ( hasIntReturnValue ) {
            // truncate return value to i32
            CastInst* truncVal = CastInst::CreateIntegerCast( user_main_call, int32T, true, "", bb );
            ReturnInst::Create( this->llvmModule().getContext(), truncVal, bb );
        }
        else {
            ReturnInst::Create( this->llvmModule().getContext(), ConstantInt::get( int32T, 0, true ), bb );
        }
    }
    else {
        this->LOGGER()->error( "LLVM function not found for name: %s", userMain.c_str() );
        ReturnInst::Create( this->llvmModule().getContext(), ConstantInt::get( this->llvmModule().getContext(), APInt( 32, 0, true ) ), bb );
    }

    return main_func;
}

// currently not used, but has working runtime initialization logic, including malloc:
//Function* LlvmGenerationContext::gen_static_init_function() {
//    auto typeCountA = this->lookup_llvm_value( "tx.runtime.TYPE_COUNT" );
//    ASSERT( typeCountA, "tx.runtime.TYPE_COUNT not found" );
//    auto metaTypesA = this->lookup_llvm_value( "tx.runtime.META_TYPES" );
//    ASSERT( metaTypesA, "tx.runtime.META_TYPES not found" );
//
//    auto int8T = Type::getInt8Ty( this->llvmContext );
//    auto int32T = Type::getInt32Ty( this->llvmContext );
//    auto int8PtrT = Type::getInt8PtrTy( this->llvmContext );
//    //auto int32PtrT = Type::getInt32PtrTy(this->llvmContext);
//    auto int8PtrArrPtrT = PointerType::getUnqual( ArrayType::get( int8PtrT, 0 ) );
//    auto constZeroV = ConstantInt::get( int32T, 0 );
//    auto constOneV = ConstantInt::get( int32T, 1 );
//    auto mallocParameterType = int32T;
//
//    Function *init_func = cast<Function>( this->llvmModule().getOrInsertFunction( "tx.runtime.thread_init",
//                                                                                int32T,
//                                                                                NULL ) );
//    BasicBlock *entryBlock = BasicBlock::Create( this->llvmModule().getContext(), "entry", init_func );
//    IRBuilder<> builder( entryBlock );
//    auto typeCountV = builder.CreateLoad( typeCountA, "TYPE_COUNT" );
//
//    Value* vtablePtrArr;
//    {
//        GlobalVariable* vtablePtrArrPtr = new GlobalVariable( this->llvmModule(), int8PtrArrPtrT, false, GlobalValue::ExternalLinkage,
//                                                              ConstantPointerNull::get( int8PtrArrPtrT ),
//                                                              "tx.runtime.VTABLES" );
//        this->register_llvm_value( vtablePtrArrPtr->getName(), vtablePtrArrPtr );
//        auto int8PtrSizeV = ConstantExpr::getTruncOrBitCast( ConstantExpr::getSizeOf( int8PtrT ), int32T );
//        auto vtablePtrAllocI = CallInst::CreateMalloc( builder.GetInsertBlock(), mallocParameterType,
//                                                       int8PtrT,
//                                                       int8PtrSizeV, typeCountV, nullptr, "" );
//        builder.GetInsertBlock()->getInstList().push_back( vtablePtrAllocI );
//        vtablePtrArr = builder.CreatePointerCast( vtablePtrAllocI, int8PtrArrPtrT, "vtPtrArr" );
//        builder.CreateStore( vtablePtrArr, vtablePtrArrPtr );
//    }
//
//    // loop through meta type array and initialize every type's thread-local static data:
//    BasicBlock* condBlock = BasicBlock::Create( this->llvmContext, "while_cond", init_func );
//    BasicBlock* loopBlock = BasicBlock::Create( this->llvmContext, "while_loop", init_func );
//    BasicBlock* postBlock = BasicBlock::Create( this->llvmContext, "while_post", init_func );
//
//    auto indexA = builder.CreateAlloca( int32T, nullptr, "index" );
//    builder.CreateStore( constZeroV, indexA );
//    builder.CreateBr( condBlock );  // branch from end of preceding block to condition-block
//
//    builder.SetInsertPoint( condBlock );
//    auto indexV = builder.CreateLoad( indexA );
//    {
//        auto condV = builder.CreateICmpNE( indexV, typeCountV );
//        builder.CreateCondBr( condV, loopBlock, postBlock );
//    }
//
//    builder.SetInsertPoint( loopBlock );
//    {
//        Value* mtSizeIxs[] = { constZeroV, indexV, ConstantInt::get( int32T, 1 ) };
//        auto allocSizeA = builder.CreateInBoundsGEP( metaTypesA, mtSizeIxs );
//        auto allocSizeV = builder.CreateLoad( allocSizeA, "size" );
//        auto vtableI = CallInst::CreateMalloc( builder.GetInsertBlock(), mallocParameterType,
//                                               int8T,
//                                               constOneV, allocSizeV, nullptr, "vtable" );
//        builder.GetInsertBlock()->getInstList().push_back( vtableI );
//        builder.CreateMemSet( vtableI, ConstantInt::get( int8T, 0 ), allocSizeV, 0 );  // FIX ME: init the vtable
//
//        {   // store the vtable pointer:
//            Value* vtIxs[] = { constZeroV, indexV };
//            auto vtPtrA = builder.CreateInBoundsGEP( vtablePtrArr, vtIxs );
//            builder.CreateStore( vtableI, vtPtrA );
//        }
//
//        // bump the index:
//        auto newIndexV = builder.CreateAdd( indexV, constOneV, "newindex" );
//        builder.CreateStore( newIndexV, indexA );
//        builder.CreateBr( condBlock );  // branch from end of loop body to condition-block
//    }
//
//    builder.SetInsertPoint( postBlock );
//    {
//        //Value* retVal = builder.CreatePtrToInt(vtablePtrArr, int32T);
//        builder.CreateRet( typeCountV );
//    }
//
//    return init_func;
//}

//void LlvmGenerationContext::initialize_external_functions() {
//    {   // declare external C abort():
//        std::vector<Type*> c_abort_args;
//        FunctionType* c_abort_func_type = FunctionType::get(
//                                                             /*Result=*/Type::getVoidTy( this->llvmContext ),
//                                                             /*Params=*/c_abort_args,
//                                                             /*isVarArg=*/false );
//        Function* c_abortF = Function::Create(
//                                               /*Type=*/c_abort_func_type,
//                                               /*Linkage=*/GlobalValue::ExternalLinkage, // (external, no body)
//                /*Name=*/"abort",
//                &this->llvmModule() );
//        c_abortF->setCallingConv( CallingConv::C );
//
//        // create adapter function:
//        Function *t_abortF = cast<Function>(
//                this->llvmModule().getOrInsertFunction( "tx.c.abort$func", Type::getVoidTy( this->llvmContext ), this->get_voidRefT(), NULL ) );
//        BasicBlock *bb = BasicBlock::Create( this->llvmModule().getContext(), "entry", t_abortF );
//        IRBuilder<> builder( bb );
//        GenScope scope( &builder );
//        CallInst *c_abortCall = builder.CreateCall( c_abortF );
//        c_abortCall->setTailCall( false );
//        ReturnInst::Create( this->llvmModule().getContext(), bb );
//
//        // store lambda object:
//        auto nullClosureRefV = Constant::getNullValue( this->get_voidRefT() );
//        std::vector<Type*> lambdaMemberTypes {
//                                               t_abortF->getType(),   // function pointer
//                this->get_voidRefT()  // null closure object pointer
//        };
//        auto lambdaT = StructType::get( this->llvmContext, lambdaMemberTypes );
//        auto lambdaV = ConstantStruct::get( lambdaT, t_abortF, nullClosureRefV, NULL );
//        auto lambdaA = new GlobalVariable( this->llvmModule(), lambdaT, true, GlobalValue::InternalLinkage, lambdaV, "tx.c.abort" );
//        this->register_llvm_value( "tx.c.abort", lambdaA );
//    }
//
//    // declare external C puts():
//    std::vector<Type*> c_puts_args( { Type::getInt8PtrTy( this->llvmContext ) } );
//    FunctionType* c_puts_func_type = FunctionType::get(
//                                                        /*Result=*/Type::getInt32Ty( this->llvmContext ),
//                                                        /*Params=*/c_puts_args,
//                                                        /*isVarArg=*/false );
//
//    Function* c_putsF = Function::Create(
//                                          /*Type=*/c_puts_func_type,
//                                          /*Linkage=*/GlobalValue::ExternalLinkage, // (external, no body)
//            /*Name=*/"puts",
//            &this->llvmModule() );
//    c_putsF->setCallingConv( CallingConv::C );
//
//    // create adapter function:
//    auto cstrRefT = TxReferenceType::make_ref_llvm_type( *this, Type::getInt8Ty( this->llvmContext ) );
//    Function *t_putsF = cast<Function>(
//            this->llvmModule().getOrInsertFunction( "tx.c.puts$func", Type::getVoidTy( this->llvmContext ), this->get_voidRefT(), cstrRefT, NULL ) );
//    BasicBlock *bb = BasicBlock::Create( this->llvmModule().getContext(), "entry", t_putsF );
//    IRBuilder<> builder( bb );
//    GenScope scope( &builder );
//    Function::arg_iterator args = t_putsF->arg_begin();
//    args++;  // the implicit closure pointer (null)
//    Value *arg_1 = &(*args);
//    arg_1->setName( "cstr" );
//    Value* ptrV = gen_get_ref_pointer( *this, &scope, arg_1 );
//    CallInst *cPutsCall = builder.CreateCall( c_putsF, ptrV );
//    cPutsCall->setTailCall( false );
//    ReturnInst::Create( this->llvmModule().getContext(), bb );
//
//    // store lambda object:
//    auto nullClosureRefV = Constant::getNullValue( this->get_voidRefT() );
//    std::vector<Type*> lambdaMemberTypes {
//                                           t_putsF->getType(),   // function pointer
//            this->get_voidRefT()  // null closure object pointer
//    };
//    auto lambdaT = StructType::get( this->llvmContext, lambdaMemberTypes );
//    auto lambdaV = ConstantStruct::get( lambdaT, t_putsF, nullClosureRefV, NULL );
//    auto lambdaA = new GlobalVariable( this->llvmModule(), lambdaT, true, GlobalValue::InternalLinkage, lambdaV, "tx.c.puts" );
//    this->register_llvm_value( "tx.c.puts", lambdaA );
//}
