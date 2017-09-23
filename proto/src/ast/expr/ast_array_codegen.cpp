#include "ast_array.hpp"

#include "ast/stmt/ast_stmt_node.hpp"
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
    auto memPtrV = this->qualtype()->type()->acttype()->gen_alloca( context, scope, "arraylit" );
    scope->builder->CreateStore( valueV, memPtrV );
    return memPtrV;
}


Value* TxFilledArrayLitNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );

    if ( this->_directArrayArg ) {
        return this->elemExprList->front()->code_gen_dyn_value( context, scope );
    }

    {
        ASSERT( scope, "Expected non-constant array literal to be codegen'd within a scope: " << this );
        //Type* arrayObjT = this->get_type()->type()->type()->make_llvm_type( context );
        Type* arrayObjT = context.get_llvm_type( this->qualtype() );
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

    /*
    Value* arrayPtrV = this->get_type()->type()->gen_alloca( context, scope, "array_lit" );
    { // initialize length field:
        Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                         ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ) };
        auto lenField = scope->builder->CreateInBoundsGEP( arrayPtrV, ixs );
        auto lenVal = ConstantInt::get( Type::getInt32Ty( context.llvmContext ), this->elemExprList->size() );
        scope->builder->CreateStore( lenVal, lenField );
    }
    for ( unsigned i = 0; i < this->elemExprList->size(); i++ ) {
        Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                         ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 2 ),
                         ConstantInt::get( Type::getInt32Ty( context.llvmContext ), i ) };
        auto elemAddr = scope->builder->CreateInBoundsGEP( arrayPtrV, ixs );
        scope->builder->CreateStore( this->elemExprList->at( i )->code_gen_value( context, scope ), elemAddr );
    }
    return arrayPtrV;
    */
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

    Type* elemT = context.get_llvm_type( this->qualtype()->type()->element_type() );
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


static Value* unfilled_array_code_gen_value( LlvmGenerationContext& context, GenScope* scope, const TxArrayType* txArrayType ) {
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

static Constant* unfilled_array_code_gen_constant( LlvmGenerationContext& context, const TxArrayType* txArrayType  ) {
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
    auto txArrayType = static_cast<const TxArrayType*>( this->qualtype()->type()->acttype() );
    return unfilled_array_code_gen_value( context, scope, txArrayType );
}

Constant* TxUnfilledArrayLitNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto txArrayType = static_cast<const TxArrayType*>( this->qualtype()->type()->acttype() );
    return unfilled_array_code_gen_constant( context, txArrayType );
}

Value* TxUnfilledArrayCompLitNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto txArrayType = static_cast<const TxArrayType*>( this->qualtype()->type()->acttype() );
    return unfilled_array_code_gen_value( context, scope, txArrayType );
}

Constant* TxUnfilledArrayCompLitNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );
    auto txArrayType = static_cast<const TxArrayType*>( this->qualtype()->type()->acttype() );
    return unfilled_array_code_gen_constant( context, txArrayType );
}



static Value* gen_elem_address( LlvmGenerationContext& context, GenScope* scope, Value* arrayPtrV, Value* subscriptV,
                                TxStatementNode* panicNode, bool isAssignment ) {
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
    if ( panicNode ) {
        // add bounds check
        auto parentFunc = scope->builder->GetInsertBlock()->getParent();
        BasicBlock* trueBlock = BasicBlock::Create( context.llvmContext, "if_true", parentFunc );
        BasicBlock* postBlock = BasicBlock::Create( context.llvmContext, "if_post", parentFunc );

        Value* lenIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                            ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ) };
        auto lengthPtrV = scope->builder->CreateInBoundsGEP( arrayPtrV, lenIxs );
        auto lengthV = scope->builder->CreateLoad( lengthPtrV );
        auto condV = scope->builder->CreateICmpUGE( subscriptV, lengthV );
        scope->builder->CreateCondBr( condV, trueBlock, postBlock );

        scope->builder->SetInsertPoint( trueBlock );
        if ( isAssignment ) {
            // allow writing one slot past end if capacity suffices
            BasicBlock* okIncrBlock = BasicBlock::Create( context.llvmContext, "if_ok_incr",  parentFunc );
            BasicBlock* okCapBlock  = BasicBlock::Create( context.llvmContext, "if_ok_cap",   parentFunc );
            BasicBlock* panicBlock  = BasicBlock::Create( context.llvmContext, "if_else_panic", parentFunc );
            auto condAsmtV = scope->builder->CreateICmpEQ( subscriptV, lengthV );
            scope->builder->CreateCondBr( condAsmtV, okIncrBlock, panicBlock );

            { // check capacity:
                scope->builder->SetInsertPoint( okIncrBlock );
                Value* capIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                                    ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ) };
                auto capPtrV = scope->builder->CreateInBoundsGEP( arrayPtrV, capIxs );
                auto capV = scope->builder->CreateLoad( capPtrV );
                auto condCapV = scope->builder->CreateICmpULT( subscriptV, capV );
                scope->builder->CreateCondBr( condCapV, okCapBlock, panicBlock );
            }

            { // increment length:
                scope->builder->SetInsertPoint( okCapBlock );
                auto newLenV = scope->builder->CreateAdd( lengthV, ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ) );
                scope->builder->CreateStore( newLenV, lengthPtrV );
                scope->builder->CreateBr( postBlock );
            }

            { // panic:
                scope->builder->SetInsertPoint( panicBlock );
                //panicNode->code_gen( context, scope );  // TODO: make panicNode call context.gen_panic_call
                auto indexV = scope->builder->CreateZExt( subscriptV, Type::getInt64Ty( context.llvmContext ) );
                context.gen_panic_call( scope, "Array write index out of bounds: %d\n", indexV );
                scope->builder->CreateBr( postBlock );  // terminate block, though won't be executed
            }
        }
        else {
            //panicNode->code_gen( context, scope );
            auto indexV = scope->builder->CreateZExt( subscriptV, Type::getInt64Ty( context.llvmContext ) );
            context.gen_panic_call( scope, "Array read index out of bounds: %d\n", indexV );
            scope->builder->CreateBr( postBlock );  // terminate block, though won't be executed
        }

        scope->builder->SetInsertPoint( postBlock );
    }

    Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                     ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 2 ),
                     subscriptV };
    return scope->builder->CreateInBoundsGEP( arrayPtrV, ixs );
}

Value* TxElemDerefNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return gen_elem_address( context, scope, this->array->code_gen_dyn_address( context, scope ),
                             this->subscript->code_gen_dyn_value( context, scope ), this->panicNode, false );
}

Value* TxElemDerefNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    Value* elemPtr = gen_elem_address( context, scope, this->array->code_gen_dyn_address( context, scope ),
                                       this->subscript->code_gen_dyn_value( context, scope ), this->panicNode, false );
    if ( scope )
        return scope->builder->CreateLoad( elemPtr );
    else
        return new LoadInst( elemPtr );
}

Constant* TxElemDerefNode::code_gen_const_address( LlvmGenerationContext& context ) const {
    Constant* arrayPtrC = this->array->code_gen_const_address( context );
    Constant* subscriptC = this->subscript->code_gen_const_value( context );

    if (this->panicNode) {
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

    if (this->panicNode) {
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
    return gen_elem_address( context, scope, this->array->code_gen_dyn_address( context, scope ),
                             this->subscript->code_gen_dyn_value( context, scope ), this->panicNode, true );
}

Value* TxArrayLenAssigneeNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    Value* arrayPtrV = this->array->code_gen_dyn_address( context, scope );
    Value* lenIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                        ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ) };
    auto lengthPtrV = scope->builder->CreateInBoundsGEP( arrayPtrV, lenIxs );
    return lengthPtrV;
}
