#include "type.hpp"
#include "ast/expr/ast_expr_node.hpp"
#include "ast/expr/ast_constexpr.hpp"
#include "ast/expr/ast_ref.hpp"
#include "llvm_generator.hpp"
#include "tx_except.hpp"

using namespace llvm;

///** Returns the input value divided by 4, rounded up. Input must be an integer. */
//static Value* code_gen_4_multiple( LlvmGenerationContext& context, GenScope* scope, Value* input ) {
//    auto two = ConstantInt::get( Type::getInt64Ty( context.llvmContext ), 2 );
//    auto three = ConstantInt::get( Type::getInt64Ty( context.llvmContext ), 3 );
//    if ( auto ci = dyn_cast<Constant>( input ) )
//        return ConstantExpr::getAShr( ConstantExpr::getAdd( ci, three ), two );
//    else if ( scope )
//        return scope->builder->CreateAShr( scope->builder->CreateAdd( input, three ), two );
//    else
//        return BinaryOperator::CreateAShr( BinaryOperator::CreateAdd( input, three ), two );
//}

StructType* TxActualType::make_vtable_type( LlvmGenerationContext& context ) const {
    LOG_TRACE( context.LOGGER(), "Mapping vtable of type " << this->get_declaration()->get_unique_full_name() << ": " << this->str(true) );
    std::vector<Type*> members;
    for ( auto memberTxField : this->get_virtual_fields().fields ) {
        auto memberTxType = memberTxField->qualtype();
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
    FunctionType *typeInitFuncType = FunctionType::get( llvm::Type::getVoidTy( context.llvmContext ), typeInitFuncArgTypes, false );

    Function *initFunc = cast<Function>( context.llvmModule().getOrInsertFunction( funcName, typeInitFuncType ) );
    BasicBlock::Create( context.llvmModule().getContext(), "entry", initFunc );
    return initFunc;
}

Type* TxActualType::make_llvm_externc_type( LlvmGenerationContext& context ) const {
    THROW_LOGIC( "This type does not support conversion to/from an extern C type in extern-c function calls: " << this );
}

Value* TxActualType::gen_size( LlvmGenerationContext& context, GenScope* scope ) const {
    if ( !this->is_static() ) {
        if ( this->is_concrete() )
            CERR_CODECHECK( this, "Currently not supported: Non-array types with VALUE bindings that are not statically constant: " << this );
        else
            THROW_LOGIC( "Attempted to codegen size of non-concrete type " << this );
    }
    Type* llvmType = context.get_llvm_type( this );  // (gets the cached LLVM type if previously accessed)
    return ConstantExpr::getSizeOf( llvmType );
}

Value* TxActualType::gen_alloca( LlvmGenerationContext& context, GenScope* scope, const std::string &varName ) const {
    if ( !this->is_static() ) {
        if ( this->is_concrete() )
            CERR_CODECHECK( this, "Currently not supported: Non-array types with VALUE bindings that are not statically constant: " << this );
        else
            THROW_LOGIC( "Attempted to codegen size of non-concrete type " << this );
    }
    Type* llvmType = context.get_llvm_type( this );  // (gets the cached LLVM type if previously accessed)
    Value* objPtrV = scope->builder->CreateAlloca( llvmType, nullptr, varName );
    this->initialize_specialized_obj( context, scope, objPtrV );
    return objPtrV;
}

Value* TxActualType::gen_malloc( LlvmGenerationContext& context, GenScope* scope, const std::string &varName ) const {
    if ( !this->is_static() ) {
        if ( this->is_concrete() )
            CERR_CODECHECK( this, "Currently not supported: Non-array types with VALUE bindings that are not statically constant: " << this );
        else
            THROW_LOGIC( "Attempted to codegen size of non-concrete type " << this );
    }
    Type* llvmType = context.get_llvm_type( this );  // (gets the cached LLVM type if previously accessed)
    Value* objPtrV = context.gen_malloc( scope, llvmType );
    this->initialize_specialized_obj( context, scope, objPtrV );
    return objPtrV;
}

Constant* TxActualType::gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const {
    return ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->get_formal_type_id() );
}


Type* TxBoolType::make_llvm_type( LlvmGenerationContext& context ) const {
    return Type::getInt1Ty( context.llvmContext );
}

Type* TxBoolType::make_llvm_externc_type( LlvmGenerationContext& context ) const {
    // Bool is mapped to a C int
    return Type::getInt32Ty( context.llvmContext );
}


llvm::Type* TxScalarType::make_llvm_type( LlvmGenerationContext& context ) const {
    return this->get_scalar_llvm_type( context );
}

llvm::Type* TxScalarType::make_llvm_externc_type( LlvmGenerationContext& context ) const {
    // scalar types are mapped 1:1 in Tuplex and C
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
    if ( !txElemType )
        THROW_LOGIC( "Generic arrays with unspecified element type can't be directly mapped to LLVM type: " << this );
    Type* elemType = context.get_llvm_type( txElemType );

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

Type* TxArrayType::make_llvm_externc_type( LlvmGenerationContext& context ) const {
    auto txElemType = this->element_type();
    if ( !txElemType )
        THROW_LOGIC( "Generic arrays with unspecified element type can't be directly mapped: " << this );
    Type* elemType = txElemType->type()->acttype()->make_llvm_externc_type( context );
    return elemType;
}

Value* TxArrayType::inner_code_gen_size( LlvmGenerationContext& context, GenScope* scope, Value* elemSize, Value* arrayCapi64V ) const {
    Constant* headerSize = ConstantExpr::getSizeOf( Type::getInt64Ty( context.llvmContext ) );
    //std::cout << "Array header size: " << to_string(headerSize) << std::endl;

    // FIXME: review how alignment needs should affect storage size
    if ( auto ce = dyn_cast<Constant>( elemSize ) ) {
        if ( auto cl = dyn_cast<Constant>( arrayCapi64V ) ) {
            ce = ConstantExpr::getZExtOrBitCast( ce, Type::getInt64Ty( context.llvmContext ) );
            return ConstantExpr::getAdd( ConstantExpr::getMul( ce, cl ), headerSize );
        }
    }

    if ( scope ) {
        elemSize = scope->builder->CreateZExtOrBitCast( elemSize, Type::getInt64Ty( context.llvmContext ) );
        auto product = scope->builder->CreateMul( elemSize, arrayCapi64V, "arraysize" );
        return scope->builder->CreateAdd( product, headerSize, "arrayobjsize" );
    }
    else {
        LOG( context.LOGGER(), WARN, "code_gen_size() with NULL scope and non-const expression for " << this );
        elemSize = CastInst::CreateZExtOrBitCast( elemSize, Type::getInt64Ty( context.llvmContext ) );
        auto product = BinaryOperator::CreateMul( elemSize, arrayCapi64V, "arraysize" );
        return BinaryOperator::CreateAdd( product, headerSize, "arrayobjsize" );
    }
}

static void initialize_array_obj( LlvmGenerationContext& context, GenScope* scope, Value* arrayObjPtrV, Value* arrayCap ) {
    { // initialize capacity field:
        Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                         ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ) };
        auto capField = scope->builder->CreateInBoundsGEP( arrayObjPtrV, ixs );
        scope->builder->CreateStore( arrayCap, capField );
    }

    { // initialize length field:
        Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                         ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ) };
        auto lenField = scope->builder->CreateInBoundsGEP( arrayObjPtrV, ixs );
        auto zeroVal = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 );
        scope->builder->CreateStore( zeroVal, lenField );
    }
}

void TxArrayType::initialize_specialized_obj( LlvmGenerationContext& context, GenScope* scope, Value* objPtrV ) const {
    auto capExpr = this->capacity();
    Value* arrayCapV = capExpr->code_gen_expr( context, scope );
    initialize_array_obj( context, scope, objPtrV, arrayCapV );

    /* TODO: Initialize array elements if they are arrays or tuples
    for ( uint32_t fieldIx = 0; fieldIx < this->instanceFields.fields.size(); ++fieldIx ) {
        auto field = this->instanceFields.fields.at( fieldIx );
        if ( field->get_decl_flags() & TXD_GENBINDING ) {
            Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                             ConstantInt::get( Type::getInt32Ty( context.llvmContext ), fieldIx ) };
            auto fieldPtrV = scope->builder->CreateInBoundsGEP( objPtrV, ixs );
            // type expressions aren't code generated prior to this: auto initV = field->get_llvm_value();
            auto initV = field->get_declaration()->get_definer()->get_init_expression()->code_gen_expr( context, scope );
            scope->builder->CreateStore( initV, fieldPtrV );
        }
        else {
            auto fieldType = field->qualtype()->type()->acttype();
            if ( fieldType->get_type_class() == TXTC_TUPLE || fieldType->get_type_class() == TXTC_ARRAY ) {
                Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                                 ConstantInt::get( Type::getInt32Ty( context.llvmContext ), fieldIx ) };
                auto fieldPtrV = scope->builder->CreateInBoundsGEP( objPtrV, ixs );
                fieldType->initialize_specialized_obj( context, scope, fieldPtrV );
            }
        }
    }
    */
}

Value* TxArrayType::gen_size( LlvmGenerationContext& context, GenScope* scope ) const {
    ASSERT( this->is_concrete(), "Attempted to codegen size of non-concrete type " << this );
    Value* elemSize = this->element_type()->type()->acttype()->gen_size( context, scope );
    Value* arrayCap = this->capacity()->code_gen_expr( context, scope );  // FIXME
    arrayCap = scope->builder->CreateZExtOrBitCast( arrayCap, Type::getInt64Ty( context.llvmContext ) );
    return this->inner_code_gen_size( context, scope, elemSize, arrayCap );
}

Value* TxArrayType::gen_alloca( LlvmGenerationContext& context, GenScope* scope, const std::string &varName ) const {
    Value* allocationPtr;

    auto capField = this->instanceFields.fields.at( 0 );
    ASSERT( capField->get_unique_name() == "C", "Expected Array's first instance field to be C but is: " << capField );
    //std::cerr << "capField decl: " << capField->get_declaration() << std::endl;
    auto capExpr = capField->get_declaration()->get_definer()->get_init_expression();
    //auto capExpr = this->capacity();
    Value* arrayCapV = capExpr->code_gen_expr( context, scope );
    if ( capExpr->is_statically_constant() ) {
        allocationPtr = TxActualType::gen_alloca( context, scope, varName );
    }
    else {
        // if size not statically constant, the llvm type will indicate zero array length and thus not describe full size of the allocation
        // compute the allocation size:
        Value* arrayCap64V = scope->builder->CreateZExtOrBitCast( arrayCapV, Type::getInt64Ty( context.llvmContext ) );
        Value* elemSizeV = this->element_type()->type()->acttype()->gen_size( context, scope );
        Value* objectSizeV = this->inner_code_gen_size( context, scope, elemSizeV, arrayCap64V );

        // allocate array object:
        allocationPtr = scope->builder->CreateAlloca( Type::getInt8Ty( context.llvmContext ), objectSizeV, "arrayalloca" );
    }

    // cast the pointer:
    Type* llvmType = context.get_llvm_type( this );  // (gets the cached LLVM type if previously accessed)
    Type* ptrType = PointerType::getUnqual( llvmType );
    Value* arrayObjPtrV = scope->builder->CreatePointerCast( allocationPtr, ptrType, varName );

    // initialize the memory:
    initialize_array_obj( context, scope, arrayObjPtrV, arrayCapV );

    return arrayObjPtrV;
}

Value* TxArrayType::gen_malloc( LlvmGenerationContext& context, GenScope* scope, const std::string &varName ) const {
    Value* allocationPtr;

    auto capField = this->instanceFields.fields.at( 0 );
    ASSERT( capField->get_unique_name() == "C", "Expected Array's first instance field to be C but is: " << capField );
    //std::cerr << "capField decl: " << capField->get_declaration() << std::endl;
    auto capExpr = capField->get_declaration()->get_definer()->get_init_expression();
    //auto capExpr = this->capacity();
    Value* arrayCapV = capExpr->code_gen_expr( context, scope );
    if ( capExpr->is_statically_constant() ) {
        allocationPtr = TxActualType::gen_malloc( context, scope, varName );
    }
    else {
        // if size not statically constant, the llvm type will indicate zero array length and thus not describe full size of the allocation
        // compute the allocation size:
        Value* arrayCap64V = scope->builder->CreateZExtOrBitCast( arrayCapV, Type::getInt64Ty( context.llvmContext ) );
        Value* elemSizeV = this->element_type()->type()->acttype()->gen_size( context, scope );
        Value* objectSizeV = this->inner_code_gen_size( context, scope, elemSizeV, arrayCap64V );

        // allocate array object:
        allocationPtr = context.gen_malloc( scope, objectSizeV );
    }

    // cast the pointer:
    Type* llvmType = context.get_llvm_type( this );  // (gets the cached LLVM type if previously accessed)
    Type* ptrType = PointerType::getUnqual( llvmType );
    Value* arrayObjPtrV = scope->builder->CreatePointerCast( allocationPtr, ptrType, varName );

    // initialize the memory:
    initialize_array_obj( context, scope, arrayObjPtrV, arrayCapV );

    return arrayObjPtrV;
}

Type* TxReferenceType::make_llvm_type( LlvmGenerationContext& context ) const {
    auto txTargetType = this->target_type();
    if ( !txTargetType )
        THROW_LOGIC( "Generic references with unspecified element type can't be directly mapped to LLVM type: " << this );
    Type* targetType = context.get_llvm_type( txTargetType );
    if ( targetType->isVoidTy() ) {
        // happens when the target type was resolved to Any
        targetType = Type::getInt8Ty( context.llvmContext );  // i8* represents void*
    }
    return make_ref_llvm_type( context, targetType );
}

Type* TxReferenceType::make_llvm_externc_type( LlvmGenerationContext& context ) const {
    auto txTargetType = this->target_type();
    if ( !txTargetType )
        THROW_LOGIC( "Generic references with unspecified element type can't be directly mapped to LLVM type: " << this );
    Type* targetType = txTargetType->type()->acttype()->make_llvm_externc_type( context );
    if ( targetType->isVoidTy() ) {
        // happens when the target type was resolved to Any
        targetType = Type::getInt8Ty( context.llvmContext );  // i8* represents void*
    }
    return PointerType::getUnqual( targetType );
}

Type* TxReferenceType::make_ref_llvm_type( LlvmGenerationContext& context, Type* targetType ) {
    std::vector<Type*> llvmMemberTypes {
                                         PointerType::getUnqual( targetType ),
                                         Type::getInt32Ty( context.llvmContext )  // type header for reference target type
    };
    auto llvmType = StructType::get( context.llvmContext, llvmMemberTypes );
    return llvmType;
}


Type* TxFunctionType::make_llvm_type( LlvmGenerationContext& context ) const {
    auto closureRefT = context.get_voidRefT();
    std::vector<Type*> llvmArgTypes;
    llvmArgTypes.push_back( closureRefT );  // first argument is always the closure object pointer
    for ( auto argTxType : this->argumentTypes ) {
        llvmArgTypes.push_back( context.get_llvm_type( argTxType ) );
        LOG_TRACE( context.LOGGER(), "Mapping arg type " << argTxType << " to " << ::to_string(llvmArgTypes.back()) );
    }
    Type* llvmRetType = this->has_return_value() ? context.get_llvm_type( this->returnType )
                                                 : llvm::Type::getVoidTy( context.llvmContext );
    FunctionType *funcT = FunctionType::get( llvmRetType, llvmArgTypes, false );

    std::vector<Type*> llvmMemberTypes {
                                         PointerType::getUnqual( funcT ),  // function pointer
                                         closureRefT                       // closure object pointer
    };
    auto llvmType = StructType::get( context.llvmContext, llvmMemberTypes );
    return llvmType;
}

Type* TxExternCFunctionType::make_llvm_type( LlvmGenerationContext& context ) const {
    // Note: In contrast to regular functions, externc functions don't have a closure object pointer as first argument.
    //       However the lambda object is always created with a closure reference field, even if unused in this case.
    auto closureRefT = context.get_voidRefT();
    auto *funcT = this->make_llvm_externc_type( context );
    std::vector<Type*> llvmMemberTypes {
                                         PointerType::getUnqual( funcT ),  // function pointer
                                         closureRefT                       // closure object pointer
    };
    auto llvmType = StructType::get( context.llvmContext, llvmMemberTypes );
    return llvmType;
}

Type* TxExternCFunctionType::make_llvm_externc_type( LlvmGenerationContext& context ) const {
    // Note: In contrast to regular functions, externc functions don't have a closure object pointer as first argument.
    std::vector<Type*> llvmArgTypes;
    for ( auto argTxType : this->argumentTypes ) {
        llvmArgTypes.push_back( argTxType->make_llvm_externc_type( context ) );
        //LOG_INFO( context.LOGGER(), "Mapping C arg type " << argTxType << " to " << ::to_string(llvmArgTypes.back()) );
    }
    Type* llvmRetType = this->has_return_value() ? this->returnType->make_llvm_externc_type( context )
                                                 : llvm::Type::getVoidTy( context.llvmContext );
    FunctionType *funcT = FunctionType::get( llvmRetType, llvmArgTypes, false );
    return funcT;
}


void TxTupleType::initialize_specialized_obj( LlvmGenerationContext& context, GenScope* scope, Value* objPtrV ) const {
    for ( uint32_t fieldIx = 0; fieldIx < this->instanceFields.fields.size(); ++fieldIx ) {
        auto field = this->instanceFields.fields.at( fieldIx );
        if ( field->get_decl_flags() & TXD_GENBINDING ) {
            Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                             ConstantInt::get( Type::getInt32Ty( context.llvmContext ), fieldIx ) };
            auto fieldPtrV = scope->builder->CreateInBoundsGEP( objPtrV, ixs );
            // type expressions aren't code generated prior to this: auto initV = field->get_llvm_value();
            auto initV = field->get_declaration()->get_definer()->get_init_expression()->code_gen_expr( context, scope );
            scope->builder->CreateStore( initV, fieldPtrV );
        }
        else {
            auto fieldType = field->qualtype()->type()->acttype();
            if ( fieldType->get_type_class() == TXTC_TUPLE || fieldType->get_type_class() == TXTC_ARRAY ) {
                Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                                 ConstantInt::get( Type::getInt32Ty( context.llvmContext ), fieldIx ) };
                auto fieldPtrV = scope->builder->CreateInBoundsGEP( objPtrV, ixs );
                fieldType->initialize_specialized_obj( context, scope, fieldPtrV );
            }
        }
    }
}

Type* TxTupleType::make_llvm_type( LlvmGenerationContext& context ) const {
    // (Note, we also need llvm types of abstract types, at least for super-references.)
    StructType* opaqueType = StructType::create( context.llvmContext, this->get_declaration()->get_unique_full_name() );
    return opaqueType;
}

Type* TxTupleType::make_llvm_type_body( LlvmGenerationContext& context, Type* header ) const {
    LOG_TRACE( context.LOGGER(), "Mapping tuple type " << this->get_declaration()->get_unique_full_name() << ": " << this->str(true) );
    std::vector<Type*> fieldTypes;
    for ( auto memberTxField : this->get_instance_fields().fields ) {
        auto memberTxType = memberTxField->qualtype()->type();
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
