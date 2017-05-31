#include "type.hpp"
#include "llvm_generator.hpp"
#include "tx_except.hpp"

using namespace llvm;

/** Returns the input value divided by 4, rounded up. Input must be an integer. */
static Value* code_gen_4_multiple( LlvmGenerationContext& context, GenScope* scope, Value* input ) {
    auto two = ConstantInt::get( Type::getInt64Ty( context.llvmContext ), 2 );
    auto three = ConstantInt::get( Type::getInt64Ty( context.llvmContext ), 3 );
    if ( auto ci = dyn_cast<Constant>( input ) )
        return ConstantExpr::getAShr( ConstantExpr::getAdd( ci, three ), two );
    else if ( scope )
        return scope->builder->CreateAShr( scope->builder->CreateAdd( input, three ), two );
    else
        return BinaryOperator::CreateAShr( BinaryOperator::CreateAdd( input, three ), two );
}

StructType* TxActualType::make_vtable_type( LlvmGenerationContext& context ) const {
    LOG_TRACE( context.LOGGER(), "Mapping vtable of type " << this->get_declaration()->get_unique_full_name() << ": " << this->str(true) );
    std::vector<Type*> members;
    for ( auto memberTxField : this->get_virtual_fields().fields ) {
        auto memberTxType = memberTxField->get_type();
        auto lMemberType = context.get_llvm_type( memberTxType->type() );
        if ( memberTxField->get_storage() == TXS_INSTANCEMETHOD )
            lMemberType = lMemberType->getStructElementType( 0 );
        else if ( memberTxField->get_unique_name() != "$adTypeId" )  // $adTypeId is direct value, not a pointer to separate global
            lMemberType = PointerType::getUnqual( lMemberType );
        members.push_back( lMemberType );
        LOG_TRACE( context.LOGGER(), "Mapping virtual member type " << memberTxType << " to: " << ::to_string(lMemberType) );
    }
    // (create() could be used to get named struct types)
    //StructType* vtableT = StructType::create(context.llvmContext, members, this->get_declaration()->get_unique_full_name() + "$VTable");
    StructType* vtableT = StructType::get( context.llvmContext, members );
    return vtableT;
}

//Constant* TxActualType::code_gen_vtable_size(LlvmGenerationContext& context) const {
//    Type* llvmType = this->make_vtable_llvm_type(context);
//    if (! llvmType)
//        return nullptr;
//    return ConstantExpr::getSizeOf(llvmType);
//}

Function* TxActualType::get_type_user_init_func( LlvmGenerationContext& context ) const {
    std::string funcName( this->get_declaration()->get_unique_full_name() + ".$tuinit" );

    std::vector<Type*> typeInitFuncArgTypes {
                                              context.get_voidPtrT()  // void* to type's data
    };
    FunctionType *typeInitFuncType = FunctionType::get( context.get_voidT(), typeInitFuncArgTypes, false );

    Function *initFunc = cast<Function>( context.llvmModule.getOrInsertFunction( funcName, typeInitFuncType ) );
    BasicBlock::Create( context.llvmModule.getContext(), "entry", initFunc );
    return initFunc;
}

Value* TxActualType::gen_size( LlvmGenerationContext& context, GenScope* scope ) const {
    ASSERT( this->is_static(), "Attempted to codegen size of non-static type " << this );
    Type* llvmType = context.get_llvm_type( this );  // (gets the cached LLVM type if previously accessed)
    if ( !llvmType )
        return nullptr;
    return ConstantExpr::getSizeOf( llvmType );
}

Value* TxActualType::gen_alloca( LlvmGenerationContext& context, GenScope* scope, const std::string &varName ) const {
    ASSERT( this->is_static(), "Attempted to codegen alloca of non-static type " << this );
    Type* llvmType = context.get_llvm_type( this );  // (gets the cached LLVM type if previously accessed)
    if ( !llvmType )
        return nullptr;
    return scope->builder->CreateAlloca( llvmType, 0, varName );
}

Constant* TxActualType::gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const {
    return ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->get_type_id() );
}

Type* TxBoolType::make_llvm_type( LlvmGenerationContext& context ) const {
    return Type::getInt1Ty( context.llvmContext );
}

llvm::Type* TxScalarType::make_llvm_type( LlvmGenerationContext& context ) const {
    return this->get_scalar_llvm_type( context );
}

llvm::Type* TxIntegerType::get_scalar_llvm_type( LlvmGenerationContext& context ) const {
    switch ( this->_size ) {
    case 1:
        return Type::getInt8Ty( context.llvmContext );
    case 2:
        return Type::getInt16Ty( context.llvmContext );
    case 4:
        return Type::getInt32Ty( context.llvmContext );
    case 8:
        return Type::getInt64Ty( context.llvmContext );
    default:
        THROW_LOGIC( "Unsupported integer size " << this->_size << " in type " << this );
    }
}

llvm::Type* TxFloatingType::get_scalar_llvm_type( LlvmGenerationContext& context ) const {
    switch ( this->_size ) {
    case 2:
        return Type::getHalfTy( context.llvmContext );
    case 4:
        return Type::getFloatTy( context.llvmContext );
    case 8:
        return Type::getDoubleTy( context.llvmContext );
    default:
        THROW_LOGIC( "Unsupported floating-point size " << this->_size << " in type " << this );
    }
}

Type* TxArrayType::make_llvm_type( LlvmGenerationContext& context ) const {
    //std::cout << "ArrayType make_llvm_type() " << ((void*)this) << std::endl;
    auto txElemType = this->element_type();
    if ( !txElemType ) {
        LOG( context.LOGGER(), ERROR, "Generic arrays with unspecified element type can't be directly mapped: " << this );
        return nullptr;
    }
    Type* elemType = context.get_llvm_type( txElemType );
    if ( !elemType ) {
        LOG( context.LOGGER(), ERROR, "No LLVM type mapping for array element type: " << txElemType );
        return nullptr;
    }

    uint32_t arrayCap;
    if ( auto capExpr = this->capacity() ) {
        // concrete array (specific capacity)
        if ( capExpr->is_statically_constant() )
            arrayCap = eval_unsigned_int_constant( capExpr );  // capacity is statically specified
        else
            arrayCap = 0;  // capacity is dynamically specified
    }
    else {
        // Generic arrays with unspecified capacity are mapped as zero capacity,
        // so they can be referenced from e.g. references.
        arrayCap = 0;
    }
    std::vector<Type*> llvmMemberTypes {
                                         Type::getInt32Ty( context.llvmContext ),
                                         Type::getInt32Ty( context.llvmContext ),
                                         ArrayType::get( elemType, arrayCap )
    };
    auto llvmType = StructType::get( context.llvmContext, llvmMemberTypes );
    LOG_DEBUG( context.LOGGER(), "Mapping array type " << this << " -> " << str(llvmType) );
    return llvmType;
}

Value* TxArrayType::gen_size( LlvmGenerationContext& context, GenScope* scope ) const {
    ASSERT( this->is_concrete(), "Attempted to codegen size of non-concrete type " << this );
    Value* elemSize = this->element_type()->gen_size( context, scope );
    Value* arrayLen = this->capacity()->code_gen_expr( context, scope );
    return this->inner_code_gen_size( context, scope, elemSize, arrayLen );
}

Value* TxArrayType::inner_code_gen_size( LlvmGenerationContext& context, GenScope* scope, Value* elemSize, Value* arrayLen ) const {
    Constant* headerSize = ConstantExpr::getSizeOf( Type::getInt64Ty( context.llvmContext ) );
    //std::cout << "Array header size: " << to_string(headerSize) << std::endl;

    if ( auto ce = dyn_cast<Constant>( elemSize ) )
        if ( auto cl = dyn_cast<Constant>( arrayLen ) )
            return ConstantExpr::getAdd( ConstantExpr::getMul( ce, cl ), headerSize );
    if ( scope ) {
        arrayLen = scope->builder->CreateZExtOrBitCast( arrayLen, Type::getInt64Ty( context.llvmContext ) );
        auto product = scope->builder->CreateMul( elemSize, arrayLen, "arraysize" );
        return scope->builder->CreateAdd( product, headerSize, "arrayobjsize" );
    }
    else {
        LOG( context.LOGGER(), WARN, "code_gen_size() with NULL scope and non-const expression for " << this );
        arrayLen = CastInst::CreateZExtOrBitCast( arrayLen, Type::getInt64Ty( context.llvmContext ) );
        auto product = BinaryOperator::CreateMul( elemSize, arrayLen, "arraysize" );
        return BinaryOperator::CreateAdd( product, headerSize, "arrayobjsize" );
    }
}

Value* TxArrayType::gen_alloca( LlvmGenerationContext& context, GenScope* scope, const std::string &varName ) const {
    //std::cerr << "ArrayType code_gen_alloca('" << varName << "')" << std::endl;
    Type* headerType = Type::getInt32Ty( context.llvmContext );
    ASSERT( this->is_concrete(), "Attempted to codegen alloca of non-concrete type " << this );

    // construct LLVM array type:
    Type* elemType = context.get_llvm_type( this->element_type() );
    bool staticCap = this->capacity()->is_statically_constant();
    uint32_t nofElems = ( staticCap ? eval_unsigned_int_constant( this->capacity() ) : 0 );
    std::vector<Type*> llvmMemberTypes {
                                         Type::getInt32Ty( context.llvmContext ),
                                         Type::getInt32Ty( context.llvmContext ),
                                         ArrayType::get( elemType, nofElems )
    };
    auto llvmType = StructType::get( context.llvmContext, llvmMemberTypes );

    // allocate array object:
    Value* arrayCap;  // uint32
    Value* arrayObj;  // pointer to llvm array obj
    if ( staticCap ) {
        // if statically known array size, use direct LLVM type
        arrayCap = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), nofElems );
        arrayObj = scope->builder->CreateAlloca( llvmType, nullptr, "arrayalloc" );
    }
    else {
        // otherwise calculate allocation size as a multiple of the array header size - ought to provide sufficient alignment?
        arrayCap = scope->builder->CreateZExtOrBitCast( this->capacity()->code_gen_expr( context, scope ),
                                                        Type::getInt32Ty( context.llvmContext ) );
        auto arrayCap64 = scope->builder->CreateZExtOrBitCast( arrayCap, Type::getInt64Ty( context.llvmContext ) );
        Value* elemSize = this->element_type()->gen_size( context, scope );
        Value* objectSize = this->inner_code_gen_size( context, scope, elemSize, arrayCap64 );

        Value* allocElems = code_gen_4_multiple( context, scope, objectSize );
        Value* allocation = scope->builder->CreateAlloca( headerType, allocElems, "arrayalloc" );
        Type* ptrType = PointerType::getUnqual( llvmType );
        arrayObj = scope->builder->CreatePointerCast( allocation, ptrType, varName );
    }

    { // initialize capacity field:
        Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                         ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ) };
        auto capField = scope->builder->CreateInBoundsGEP( arrayObj, ixs );
        scope->builder->CreateStore( arrayCap, capField );
    }

    { // initialize length field:
        Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                         ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ) };
        auto lenField = scope->builder->CreateInBoundsGEP( arrayObj, ixs );
        auto zeroVal = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 );
        scope->builder->CreateStore( zeroVal, lenField );
    }

    return arrayObj;
}

Type* TxReferenceType::make_llvm_type( LlvmGenerationContext& context ) const {
    // Note: a reference itself is always 'concrete'
    if ( auto txTargetType = this->target_type() ) {
        if ( Type* targetType = context.get_llvm_type( txTargetType ) ) {
            if ( targetType->isVoidTy() ) {
                // happens when the target type was resolved to Any
                targetType = Type::getInt8Ty( context.llvmContext );  // i8* represents void*
            }
            LOG_DEBUG( context.LOGGER(), "Mapping reference type " << this );
            return make_ref_llvm_type( context, targetType );
        }
        else
            LOG( context.LOGGER(), ERROR, "No LLVM type mapping for reference target type: " << txTargetType );
    }
    else
        LOG( context.LOGGER(), ERROR, "Unknown target type of reference type " << this );
    return nullptr;
}

Type* TxReferenceType::make_ref_llvm_type( LlvmGenerationContext& context, Type* targetType ) {
    std::vector<Type*> llvmMemberTypes {
                                         PointerType::getUnqual( targetType ),
                                         Type::getInt32Ty( context.llvmContext )  // type header for reference target type
    };
    auto llvmType = StructType::get( context.llvmContext, llvmMemberTypes );
    return llvmType;
}

Value* TxReferenceType::gen_ref_conversion( LlvmGenerationContext& context, GenScope* scope, Value* origValue,
                                            Type* targetRefT,
                                            uint32_t targetTypeId ) {
    auto newPtrT = cast<StructType>( targetRefT )->getElementType( 0 );
    Value* tidV = ( targetTypeId == UINT32_MAX ? gen_get_ref_typeid( context, scope, origValue )
                                                                     :
                                                 ConstantInt::get( Type::getInt32Ty( context.llvmContext ), targetTypeId ) );
    Value* origPtrV = gen_get_ref_pointer( context, scope, origValue );
    Value* newPtrV;
    // bitcast from one pointer type to another
    if ( !scope )
        newPtrV = ConstantExpr::getBitCast( cast<Constant>( origPtrV ), newPtrT );
    else
        newPtrV = scope->builder->CreateBitCast( origPtrV, newPtrT );

    return gen_ref( context, scope, targetRefT, newPtrV, tidV );
}

Type* TxFunctionType::make_llvm_type( LlvmGenerationContext& context ) const {
    auto closureRefT = context.get_voidRefT();
    std::vector<Type*> llvmArgTypes;
    llvmArgTypes.push_back( closureRefT );  // first argument is always the closure object pointer
    for ( auto argTxType : this->argumentTypes ) {
        llvmArgTypes.push_back( context.get_llvm_type( argTxType ) );
        LOG_TRACE( context.LOGGER(), "Mapping arg type " << argTxType << " to " << ::to_string(llvmArgTypes.back()) );
    }
    Type* llvmRetType = this->has_return_value()
                        ? context.get_llvm_type( this->returnType )
                                                 :
                          context.get_voidT();
    FunctionType *funcT = FunctionType::get( llvmRetType, llvmArgTypes, false );

    std::vector<Type*> llvmMemberTypes {
                                         PointerType::getUnqual( funcT ),  // function pointer
                                         closureRefT                     // closure object pointer
    };
    auto llvmType = StructType::get( context.llvmContext, llvmMemberTypes );
    return llvmType;
}

Type* TxTupleType::make_llvm_type( LlvmGenerationContext& context ) const {
    if (! this->is_concrete()) {
        // (we do need llvm types of abstract types, at least for super-references)
        LOG_DEBUG( context.LOGGER(), "making LLVM type of non-concrete type " << this );
    }
    StructType* opaqueType = StructType::create( context.llvmContext, this->get_declaration()->get_unique_full_name() );
    return opaqueType;
}

Type* TxTupleType::make_llvm_type_body( LlvmGenerationContext& context, Type* header ) const {
    LOG_TRACE( context.LOGGER(), "Mapping tuple type " << this->get_declaration()->get_unique_full_name() << ": " << this->str(true) );
    std::vector<Type*> fieldTypes;
    for ( auto memberTxField : this->get_instance_fields().fields ) {
        auto memberTxType = memberTxField->get_type()->type();
        auto memberLlvmType = context.get_llvm_type( memberTxType );
        fieldTypes.push_back( memberLlvmType );
        LOG_TRACE( context.LOGGER(), "Mapping member type " << memberTxType << " to " << ::to_string(memberLlvmType) );
    }
    StructType* sType = cast<StructType>( header );
    sType->setBody( fieldTypes );
    return sType;
}

Type* TxInterfaceType::make_llvm_type( LlvmGenerationContext& context ) const {
    return StructType::get( context.llvmContext );  // abstract type
}

Type* TxInterfaceAdapterType::make_llvm_type( LlvmGenerationContext& context ) const {
    return StructType::get( context.llvmContext );  // abstract type
}
