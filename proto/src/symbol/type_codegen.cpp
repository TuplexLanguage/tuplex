#include "type.hpp"
#include "ast/expr/ast_expr_node.hpp"
#include "ast/expr/ast_constexpr.hpp"
#include "ast/expr/ast_ref.hpp"
#include "llvm_generator.hpp"
#include "tx_except.hpp"

using namespace llvm;

///* * Returns the input value divided by 4, rounded up. Input must be an integer. */
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

//Function* TxActualType::get_type_user_init_func( LlvmGenerationContext& context ) const {
//    std::string funcName( this->get_declaration()->get_unique_full_name() + ".$tuinit" );
//
//    std::vector<Type*> typeInitFuncArgTypes {
//                                              context.get_voidPtrT()  // void* to type's data
//    };
//    FunctionType *typeInitFuncType = FunctionType::get( llvm::Type::getVoidTy( context.llvmContext ), typeInitFuncArgTypes, false );
//
//    Function *initFunc = cast<Function>( context.llvmModule().getOrInsertFunction( funcName, typeInitFuncType ) );
//    BasicBlock::Create( context.llvmModule().getContext(), "entry", initFunc );
//    return initFunc;
//}

Type* TxActualType::make_llvm_externc_type( LlvmGenerationContext& context ) const {
    THROW_LOGIC( "This type does not support conversion to/from an extern C type in extern-c function calls: " << this );
}

Constant* TxActualType::gen_typeid( LlvmGenerationContext& context ) const {
    return ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->get_runtime_type_id() );
}

Constant* TxActualType::gen_static_element_size( LlvmGenerationContext& context ) const {
    if ( !this->is_static() ) {
        if ( this->is_concrete() )
            CERR_CODECHECK( this, "Currently not supported: Non-array types with VALUE bindings that are not statically constant: " << this );
        else
            THROW_LOGIC( "Attempted to codegen size of non-concrete type " << this );
    }
    Type* llvmType = context.get_llvm_type( this );
    return ConstantExpr::getSizeOf( llvmType );
}

Constant* TxActualType::gen_static_size( LlvmGenerationContext& context ) const {
    return this->gen_static_element_size( context );
}

Value* TxActualType::gen_alloca( LlvmGenerationContext& context, GenScope* scope, unsigned aligment, const std::string &varName ) const {
    if ( !this->is_static() ) {
        if ( this->is_concrete() )
            CERR_CODECHECK( this, "Currently not supported: Non-array types with VALUE bindings that are not statically constant: " << this );
        else
            THROW_LOGIC( "Attempted to codegen size of non-concrete type " << this );
    }
    Type* llvmType = context.get_llvm_type( this );

    scope->use_alloca_insertion_point();
    Value* objPtrV = scope->builder->Insert( new AllocaInst( llvmType, nullptr, aligment ), varName );
    scope->use_current_insertion_point();

    this->initialize_specialized_obj( context, scope, objPtrV );
    return objPtrV;
}

Value* TxActualType::gen_alloca( LlvmGenerationContext& context, GenScope* scope, const std::string &varName ) const {
    return this->gen_alloca( context, scope, 0, varName );
}

Value* TxActualType::gen_malloc( LlvmGenerationContext& context, GenScope* scope, const std::string &varName ) const {
    if ( !this->is_static() ) {
        if ( this->is_concrete() )
            CERR_CODECHECK( this, "Currently not supported: Non-array types with VALUE bindings that are not statically constant: " << this );
        else
            THROW_LOGIC( "Attempted to codegen size of non-concrete type " << this );
    }
    Type* llvmType = context.get_llvm_type( this );
    Value* objPtrV = context.gen_malloc( scope, llvmType );
    this->initialize_specialized_obj( context, scope, objPtrV );
    return objPtrV;
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


static Value* gen_compute_array_size( LlvmGenerationContext& context, GenScope* scope, Value* elemSizeV, Value* arrayCapi64V ) {
    // NOTE: This calculation is only "safe" on outer-most type - if used on an element of an aggregate type, padding effects are missed.
    Constant* headerSizeC = ConstantExpr::getSizeOf( Type::getInt64Ty( context.llvmContext ) );
    elemSizeV = scope->builder->CreateZExtOrBitCast( elemSizeV, Type::getInt64Ty( context.llvmContext ) );
    auto product = scope->builder->CreateMul( elemSizeV, arrayCapi64V, "datasize" );
    return scope->builder->CreateAdd( product, headerSizeC, "arraysize" );
}

Constant* TxArrayType::gen_static_element_size( LlvmGenerationContext& context ) const {
    ASSERT( !this->is_type_generic(), "Attempted to codegen size of type-generic array type " << this );
    auto elemType = this->element_type()->type()->acttype();
    return elemType->gen_static_size( context );
}

Constant* TxArrayType::gen_static_size( LlvmGenerationContext& context ) const {
    if (! this->is_static() )
        CERR_CODECHECK( this, "Can't generate static size for non-static type: " << this );
    Type* llvmType = context.get_llvm_type( this );
    return ConstantExpr::getSizeOf( llvmType );
}

Value* TxArrayType::gen_alloca( LlvmGenerationContext& context, GenScope* scope, const std::string &varName ) const {
    ASSERT( !this->is_generic(), "Attempted to alloca generic array type " << this );
    auto capField = this->instanceFields.fields.at( 0 );
    ASSERT( capField->get_unique_name() == "C", "Expected Array's first instance field to be C but is: " << capField );
    //std::cerr << "capField decl: " << capField->get_declaration() << std::endl;
    auto capExpr = capField->get_declaration()->get_definer()->get_init_expression();
    ASSERT( capExpr, "No capacity 'C' initialization expression for type " << this );

    auto elemType = this->element_type()->type()->acttype();
    if ( !elemType->is_static() )
        CERR_CODECHECK( this, "Arrays with non-statically sized element type not supported: " << elemType );

    if ( capExpr->is_statically_constant() ) {
        return TxActualType::gen_alloca( context, scope, 8, varName );  // allocate array object, alignment of 8
    }

    // If size not statically constant, the llvm type will indicate zero array length and thus not describe the full size of the allocation.
    // NOTE: In the current implementation only the outer-most array may be dynamically sized; its element types must be static.

    // NOTE: Proper use of alloca (in entry block) requires statically known array capacity
    // TODO: This can overflow stack when allocating dynamic-capacity arrays in loops.
    //       Use the i8* @llvm.stacksave() & @llvm.stackrestore(i8* %ptr) intrinsics at block start & end to fix this.

    // compute the allocation size:
    Value* arrayCapV = capExpr->code_gen_expr( context, scope );
    Value* arrayCap64V = scope->builder->CreateZExtOrBitCast( arrayCapV, Type::getInt64Ty( context.llvmContext ) );
    auto elemSizeC = this->gen_static_element_size( context );  //elemType->gen_size( context, scope );
    Value* objectSizeV = gen_compute_array_size( context, scope, elemSizeC, arrayCap64V );

    // allocate array object, alignment of 8:
    // NOTE: Can't perform alloca in entry block since dependent on dynamic capacity expression evaluation.
    //allocationPtr = scope->builder->CreateAlloca( Type::getInt8Ty( context.llvmContext ), objectSizeV, "arrayalloca" );
    Value* allocationPtr = scope->builder->Insert( new AllocaInst( Type::getInt8Ty( context.llvmContext ), objectSizeV, 8 ), "arrayalloca" );

    // cast the pointer:
    Type* llvmType = context.get_llvm_type( this );
    Type* ptrType = PointerType::getUnqual( llvmType );
    Value* arrayObjPtrV = scope->builder->CreatePointerCast( allocationPtr, ptrType, varName );

    // initialize the memory:
    initialize_array_obj( context, scope, arrayObjPtrV, arrayCapV );

    return arrayObjPtrV;
}

Value* TxArrayType::gen_malloc( LlvmGenerationContext& context, GenScope* scope, const std::string &varName ) const {
    ASSERT( !this->is_generic(), "Attempted to malloc generic array type " << this );
    auto capField = this->instanceFields.fields.at( 0 );
    ASSERT( capField->get_unique_name() == "C", "Expected Array's first instance field to be C but is: " << capField );
    //std::cerr << "capField decl: " << capField->get_declaration() << std::endl;
    auto capExpr = capField->get_declaration()->get_definer()->get_init_expression();
    ASSERT( capExpr, "No capacity 'C' initialization expression for type " << this );
    Value* arrayCapV = capExpr->code_gen_expr( context, scope );

    auto elemType = this->element_type()->type()->acttype();
    if ( !elemType->is_static() )
        CERR_CODECHECK( this, "Arrays with non-statically sized element type not supported: " << elemType );

    if ( capExpr->is_statically_constant() ) {
        return TxActualType::gen_malloc( context, scope, varName );  // we assume malloc will provide alignment of at least 8
    }

    // If size not statically constant, the llvm type will indicate zero array length and thus not describe the full size of the allocation.
    // NOTE: In the current implementation only the outer-most array may be dynamically sized; its element types must be static.

    Value* arrayCap64V = scope->builder->CreateZExtOrBitCast( arrayCapV, Type::getInt64Ty( context.llvmContext ) );
    auto elemSizeC = this->gen_static_element_size( context );  //elemType->gen_size( context, scope );
    Value* objectSizeV = gen_compute_array_size( context, scope, elemSizeC, arrayCap64V );

    // allocate array object:
    Value* allocationPtr = context.gen_malloc( scope, objectSizeV );  // we assume malloc will provide alignment of at least 8

    // cast the pointer:
    Type* llvmType = context.get_llvm_type( this );
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
    return StructType::get( context.llvmContext, llvmMemberTypes );
}

Type* TxReferenceType::make_ref_llvm_type( LlvmGenerationContext& context, Type* targetType, const std::string& name ) {
    std::vector<Type*> llvmMemberTypes {
                                         PointerType::getUnqual( targetType ),
                                         Type::getInt32Ty( context.llvmContext )  // type header for reference target type
    };
    return StructType::create( context.llvmContext, llvmMemberTypes, name );
}


Type* TxFunctionType::make_llvm_type( LlvmGenerationContext& context ) const {
    auto closureRefT = context.get_closureRefT();
    std::vector<Type*> llvmArgTypes;
    llvmArgTypes.push_back( closureRefT );  // first argument is always the closure object ref
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
    // Note: In contrast to regular functions, externc functions don't have a closure object ref as first argument.
    //       However the lambda object is always created with a closure reference field, even if unused in this case.
    auto closureRefT = context.get_closureRefT();
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
