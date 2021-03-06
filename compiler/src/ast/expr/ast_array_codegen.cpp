#include "ast_array.hpp"

#include "ast/stmt/ast_stmt_node.hpp"
#include "ast_constexpr.hpp"
#include "parsercontext.hpp"

#include "llvm_generator.hpp"

using namespace llvm;


Value* TxArrayLitNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    if ( this->is_statically_constant() ) {
        return this->code_gen_const_address( context );
    }

    // automatically allocates local storage for dynamic array literals
    // NOTE: Proper use of alloca (in entry block) requires statically known array capacity
    auto valueV = this->code_gen_dyn_value( context, scope );
    auto memPtrV = this->qtype()->gen_alloca( context, scope, "arraylit" );
    scope->builder->CreateStore( valueV, memPtrV );
    return memPtrV;
}


Value* TxFilledArrayLitNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );

    if ( this->_directArrayArg ) {
        return this->elemExprList->front()->code_gen_dyn_value( context, scope );
    }

    Type* arrayObjT = context.get_llvm_type( this->qtype() );
    Value* arrayObjV = UndefValue::get( arrayObjT );
    auto capacityC = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->elemExprList->size() );
    arrayObjV = scope->builder->CreateInsertValue( arrayObjV, capacityC, 0 );
    arrayObjV = scope->builder->CreateInsertValue( arrayObjV, capacityC, 1 );  // length equals capacity

    if ( this->elemExprList->empty() ) {
        Constant* emptyArrayC = ConstantArray::get( cast<ArrayType>( arrayObjT->getContainedType( 2 ) ), ArrayRef<Constant*>() );
        arrayObjV = scope->builder->CreateInsertValue( arrayObjV, emptyArrayC, 2 );
    }
    else {
        for ( unsigned i = 0; i < this->elemExprList->size(); i++ ) {
            auto elemV = this->elemExprList->at( i )->code_gen_dyn_value( context, scope );
            arrayObjV = scope->builder->CreateInsertValue( arrayObjV, elemV, std::vector<unsigned>( { 2, i } ) );
        }
    }
    return arrayObjV;
}

Constant* TxFilledArrayLitNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );

    if ( this->_directArrayArg ) {
        return this->elemExprList->front()->code_gen_const_value( context );
    }
    // FUTURE: optimize for arrays of scalars
    //    if (this->elementTypeNode->typeExprNode->get_type()->type()->is_scalar()) {
    //        Constant* dataArray = ConstantDataArray::get( context.llvmContext, nullptr );
    //        std::vector<Constant*> members {
    //            ConstantInt::get( context.llvmContext, APInt( 32, this->argsExprList->size() ) ),
    //            dataArray
    //        };
    //        return ConstantStruct::getAnon(members);
    //    }
    std::vector<Constant*> values;
    for ( auto elemExpr : *this->elemExprList )
        values.push_back( elemExpr->code_gen_const_value( context ) );
    ArrayRef<Constant*> data( values );

    Type* elemT = context.get_llvm_type( this->qtype()->element_type() );
    uint64_t arrayLen = this->elemExprList->size();
    ArrayType* arrayT = ArrayType::get( elemT, arrayLen );
    Constant* dataArrayC = ConstantArray::get( arrayT, data );
    std::vector<Constant*> objMembers {
                                        ConstantInt::get( context.llvmContext, APInt( 32, arrayLen ) ),
                                        ConstantInt::get( context.llvmContext, APInt( 32, arrayLen ) ),
                                        dataArrayC
    };
    return ConstantStruct::getAnon( objMembers );
}


static Value* unfilled_array_code_gen_value( LlvmGenerationContext& context, GenScope* scope, const TxActualType* txArrayType ) {
    Type* arrayObjT = txArrayType->make_llvm_type( context );
    Value* arrayObjV = UndefValue::get( arrayObjT );
    auto capacityExpr = txArrayType->capacity();
    ASSERT( capacityExpr, "Unfilled array literal's implementation requires known capacity: " << txArrayType );
    Value* arrayCapV = capacityExpr->code_gen_expr( context, scope );

    arrayObjV = scope->builder->CreateInsertValue( arrayObjV, arrayCapV, 0 );
    arrayObjV = scope->builder->CreateInsertValue( arrayObjV, ConstantInt::get( context.llvmContext, APInt( 32, 0 ) ), 1 );

    auto arrayT = cast<ArrayType>( arrayObjT->getContainedType( 2 ) );
    Constant* emptyArrayC = ConstantArray::get( arrayT, ArrayRef<Constant*>() );
    arrayObjV = scope->builder->CreateInsertValue( arrayObjV, emptyArrayC, 2 );
    return arrayObjV;
}

static Constant* unfilled_array_code_gen_constant( LlvmGenerationContext& context, const TxActualType* txArrayType  ) {
    Type* arrayObjT = txArrayType->make_llvm_type( context );
    auto arrayT = cast<ArrayType>( arrayObjT->getContainedType( 2 ) );
    auto cap = arrayT->getArrayNumElements();
    //std::cerr << txArrayType << " code_gen_constant() capacity=" << cap << std::endl;
    std::vector<Constant*> objMembers {
                                        ConstantInt::get( context.llvmContext, APInt( 32, cap ) ),
                                        ConstantInt::get( context.llvmContext, APInt( 32, 0 ) ),
                                        ConstantArray::get( arrayT, ArrayRef<Constant*>() )
    };
    return ConstantStruct::getAnon( objMembers );
}

Value* TxUnfilledArrayLitNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    auto txArrayType = this->qtype().type();
    return unfilled_array_code_gen_value( context, scope, txArrayType );
}

Constant* TxUnfilledArrayLitNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto txArrayType = this->qtype().type();
    return unfilled_array_code_gen_constant( context, txArrayType );
}

Value* TxUnfilledArrayCompLitNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    auto txArrayType = this->qtype().type();
    return unfilled_array_code_gen_value( context, scope, txArrayType );
}

Constant* TxUnfilledArrayCompLitNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto txArrayType = this->qtype().type();
    return unfilled_array_code_gen_constant( context, txArrayType );
}



static Value* gen_elem_address( const TxNode* origin, LlvmGenerationContext& context, GenScope* scope, Value* arrayPtrV, Value* subscriptV,
                                bool isAssignment, Value* subarrayCapacityV=nullptr, bool suppressBoundsCheck=false ) {
    ASSERT( subscriptV->getType()->isIntegerTy(), "expected subscript to be an integer: " << subscriptV );
    ASSERT( arrayPtrV->getType()->isPointerTy(), "expected array-operand to be a pointer: " << arrayPtrV );
    ASSERT( arrayPtrV->getType()->getPointerElementType()->isStructTy(), "expected array-operand to be a pointer to struct: " << arrayPtrV );

    if ( auto arrayPtrC = dyn_cast<Constant>( arrayPtrV ) ) {
        // address of global constant
        if ( auto intC = dyn_cast<ConstantInt>( subscriptV ) ) {
            //LOG_NOTE( context.LOGGER(), "constant expression not expected in gen_elem_address(); " << panicNode );
            // TODO: remove?
            Constant* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                                ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 2 ),
                                intC };
            return ConstantExpr::getInBoundsGetElementPtr( arrayPtrC->getType()->getPointerElementType(), arrayPtrC, ixs );
        }
    }

    ASSERT( scope, "NULL scope in non-const array elem access");

    std::string id = std::to_string( origin->ploc.begin.line );
    Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                     ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 2 ),
                     subscriptV };
    Value* elemA = scope->builder->CreateInBoundsGEP( arrayPtrV, ixs, "elemA"+id );

    if ( !suppressBoundsCheck ) {
        // add bounds check
        auto parentFunc = scope->builder->GetInsertBlock()->getParent();
        BasicBlock* oobBlock = BasicBlock::Create( context.llvmContext, "abc_if_out"+id, parentFunc );
        BasicBlock* postBlock = BasicBlock::Create( context.llvmContext, "abc_if_post"+id, parentFunc );

        Value* lenIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                            ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ) };
        auto lengthPtrV = scope->builder->CreateInBoundsGEP( arrayPtrV, lenIxs, "alenA"+id );
        auto lengthV = scope->builder->CreateLoad( lengthPtrV, "alen"+id );
        auto condV = scope->builder->CreateICmpUGE( subscriptV, lengthV, "is_oob"+id );
        scope->builder->CreateCondBr( condV, oobBlock, postBlock );

        scope->builder->SetInsertPoint( oobBlock );
        if ( isAssignment ) {
            // allow writing one slot past end if capacity suffices
            BasicBlock* okIncrBlock = BasicBlock::Create( context.llvmContext, "abc_if_ok_incr"+id,  parentFunc );
            BasicBlock* okCapBlock  = BasicBlock::Create( context.llvmContext, "abc_if_ok_cap"+id,   parentFunc );
            BasicBlock* panicBlock  = BasicBlock::Create( context.llvmContext, "abc_if_else_panic"+id, parentFunc );
            auto condAsmtV = scope->builder->CreateICmpEQ( subscriptV, lengthV, "is_eq_cap"+id );
            scope->builder->CreateCondBr( condAsmtV, okIncrBlock, panicBlock );

            { // check capacity:
                scope->builder->SetInsertPoint( okIncrBlock );
                Value* capIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                                    ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ) };
                auto capPtrV = scope->builder->CreateInBoundsGEP( arrayPtrV, capIxs, "acapA"+id );
                auto capV = scope->builder->CreateLoad( capPtrV, "acap"+id );
                //capV->setMetadata( LLVMContext::MD_invariant_load, MDNode::get( context.llvmContext, { MDString::get( context.llvmContext, "ACap") } ) );
                capV->setMetadata( LLVMContext::MD_invariant_group,
                                   MDNode::get( context.llvmContext, { MDString::get( context.llvmContext, "ACap") } ) );
                auto condCapV = scope->builder->CreateICmpULT( subscriptV, capV, "is_ok_cap"+id );
                scope->builder->CreateCondBr( condCapV, okCapBlock, panicBlock );
            }

            { // increment length:
                scope->builder->SetInsertPoint( okCapBlock );
                auto newLenV = scope->builder->CreateAdd( lengthV, ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ), "newlen"+id );
                scope->builder->CreateStore( newLenV, lengthPtrV );

                if ( subarrayCapacityV ) {
                    // initialize sub-array's header (capacity and length) fields
                    auto lenC = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 );
                    initialize_array_obj( context, scope, elemA, subarrayCapacityV, lenC );
                }

                scope->builder->CreateBr( postBlock );
            }

            { // panic:
                scope->builder->SetInsertPoint( panicBlock );
                auto indexV = scope->builder->CreateZExt( subscriptV, Type::getInt64Ty( context.llvmContext ), "ix64"+id );
                context.gen_panic_call( scope, origin->parse_loc_string() + ": Array write index out of bounds: %d\n", indexV );
                scope->builder->CreateBr( postBlock );  // terminate block, though won't be executed
            }
        }
        else {
            auto indexV = scope->builder->CreateZExt( subscriptV, Type::getInt64Ty( context.llvmContext ), "ix64"+id );
            context.gen_panic_call( scope, origin->parse_loc_string() + ": Array read index out of bounds: %d\n", indexV );
            scope->builder->CreateBr( postBlock );  // terminate block, though won't be executed
        }

        scope->builder->SetInsertPoint( postBlock );
    }
    return elemA;
}

Value* TxElemDerefNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    Value* subarrayCapacityV = nullptr;
    if ( this->_elemAssignment && this->qtype()->get_type_class() == TXTC_ARRAY ) {
        // this is part of an assignee expression and the element type is an array - initialize the sub-array's header:
        //std::cerr << this << " performing assignment, initializing sub-array's header" << std::endl;
        if ( auto capExpr = this->qtype()->capacity() ) {
            // concrete array (specific capacity)
            if ( capExpr->is_statically_constant() )
                // capacity is statically specified
                subarrayCapacityV = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), eval_unsigned_int_constant( capExpr ) );
            else
                CERR_CODECHECK( this, "Non-constant capacity of sub-array to initialize" );
        }
        else
            CERR_CODECHECK( this, "Unspecified capacity of sub-array to initialize" );
    }
    return gen_elem_address( this, context, scope, this->array->code_gen_dyn_address( context, scope ),
                             this->subscript->code_gen_dyn_value( context, scope ),
                             this->_elemAssignment, subarrayCapacityV );
}

Value* TxElemDerefNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    ASSERT( !this->_elemAssignment, "Unexpected _elemAssignment=true in code_gen_dyn_value() for " << this );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    Value* elemPtr = gen_elem_address( this, context, scope, this->array->code_gen_dyn_address( context, scope ),
                                       this->subscript->code_gen_dyn_value( context, scope ), false );
    return scope->builder->CreateLoad( elemPtr );
}

Constant* TxElemDerefNode::code_gen_const_address( LlvmGenerationContext& context ) const {
    Constant* arrayPtrC = this->array->code_gen_const_address( context );
    Constant* subscriptC = this->subscript->code_gen_const_value( context );

    if ( true /* do check */ ) {
        auto globalArrayPtrC = cast<GlobalVariable>( arrayPtrC);
        uint64_t index = cast<ConstantInt>( subscriptC )->getZExtValue();
        uint64_t length = cast<ConstantInt>( globalArrayPtrC->getInitializer()->getAggregateElement( 1 ) )->getZExtValue();
        if ( index >= length )
            CERR_CODECHECK( this, "Constant array index out of bounds: " << index << " >= " << length );
    }

    Constant* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                        ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 2 ),
                        subscriptC };
    return ConstantExpr::getInBoundsGetElementPtr( arrayPtrC->getType()->getPointerElementType(), arrayPtrC, ixs );
}

Constant* TxElemDerefNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto arrayC = this->array->code_gen_const_value( context );
    auto subscriptC = cast<ConstantInt>( this->subscript->code_gen_const_value( context ) );

    if ( true /* do check */ ) {
        uint64_t index = cast<ConstantInt>( subscriptC )->getZExtValue();
        uint64_t length = cast<ConstantInt>( arrayC->getAggregateElement( 1 ) )->getZExtValue();
        if ( index >= length )
            CERR_CODECHECK( this, "Constant array index out of bounds: " << index << " >= " << length );
    }

    uint32_t ixs[] = { 2, (uint32_t) subscriptC->getLimitedValue( UINT32_MAX ) };
    return ConstantExpr::getExtractValue( arrayC, ixs );
}

Value* TxElemAssigneeNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    Value* subarrayCapacityV = nullptr;
    if ( this->qtype()->get_type_class() == TXTC_ARRAY ) {
        // if the element type is an array - initialize the sub-array's header:
        //std::cerr << this << " performing assignment, initializing sub-array's header" << std::endl;
        if ( auto capExpr = this->qtype()->capacity() ) {
            // concrete array (specific capacity)
            if ( capExpr->is_statically_constant() )
                // capacity is statically specified
                subarrayCapacityV = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), eval_unsigned_int_constant( capExpr ) );
            else
                CERR_CODECHECK( this, "Non-constant capacity of sub-array to initialize" );
        }
        else
            CERR_CODECHECK( this, "Unspecified capacity of sub-array to initialize" );
    }
    return gen_elem_address( this, context, scope, this->array->code_gen_dyn_address( context, scope ),
                             this->subscript->code_gen_dyn_value( context, scope ),
                             true, subarrayCapacityV );
}

Value* TxArrayLenAssigneeNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->SetCurrentDebugLocation( DebugLoc::get( ploc.begin.line, ploc.begin.column, scope->debug_scope() ) );
    Value* arrayPtrV = this->array->code_gen_dyn_address( context, scope );
    Value* lenIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                        ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ) };
    auto lengthPtrV = scope->builder->CreateInBoundsGEP( arrayPtrV, lenIxs );
    return lengthPtrV;
}
