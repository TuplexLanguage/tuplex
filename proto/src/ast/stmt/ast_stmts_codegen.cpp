#include "ast_stmts.hpp"
#include "ast_assertstmt_node.hpp"
#include "ast_panicstmt_node.hpp"
#include "symbol/package.hpp"
#include "driver.hpp"

#include "llvm_generator.hpp"
#include "llvm/IR/IntrinsicInst.h"

using namespace llvm;


void TxFieldStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    this->fieldDef->code_gen_field( context, scope );
}

void TxTypeStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    this->typeDecl->code_gen( context );
}

void TxAssignStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto lval = this->lvalue->code_gen_address( context, scope );
    auto rval = this->rvalue->code_gen_expr( context, scope );
    ASSERT ( lval->getType()->isPointerTy(), "At " << this->parse_loc_string() << ": L-value is not of pointer type:\n" << ::to_string(lval) );
    scope->builder->CreateStore( rval, lval );
}

void TxAssertStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( !context.tuplexPackage.driver().get_options().suppress_asserts )
        this->ifStmt->code_gen( context, scope );
}

void TxPanicStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    this->suite->code_gen( context, scope );
}

void TxSuiteNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    for ( auto stmt : *this->suite )
        stmt->code_gen( context, scope );
}

void TxExprStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    this->expr->code_gen_expr( context, scope );
}

void TxReturnStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( this->expr ) {
        auto exprV = this->expr->code_gen_expr( context, scope );
        scope->builder->CreateRet( exprV );
    }
    else
        scope->builder->CreateRetVoid();
}

void TxBreakStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->CreateBr( scope->compStmtStack.top()->breakBlock );
}

void TxContinueStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    scope->builder->CreateBr( scope->compStmtStack.top()->continueBlock );
}


void TxArrayCopyStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto lvalArrayPtrV = this->lvalue->code_gen_address( context, scope );
    auto rvalArrayPtrV = this->rvalue->code_gen_addr( context, scope );

    Value* lenIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                        ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 1 ) };
    auto srcLenPtrV = scope->builder->CreateInBoundsGEP( rvalArrayPtrV, lenIxs );
    auto srcLenV = scope->builder->CreateLoad( srcLenPtrV );

    { // add bounds check:
        auto parentFunc = scope->builder->GetInsertBlock()->getParent();
        BasicBlock* trueBlock = BasicBlock::Create( context.llvmContext, "if_true", parentFunc );
        BasicBlock* nextBlock = BasicBlock::Create( context.llvmContext, "if_next", parentFunc );

        Value* capIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                            ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ) };
        auto dstCapPtrV = scope->builder->CreateInBoundsGEP( lvalArrayPtrV, capIxs );
        auto dstCapV = scope->builder->CreateLoad( dstCapPtrV );

        auto condV = scope->builder->CreateICmpULT( dstCapV, srcLenV );
        scope->builder->CreateCondBr( condV, trueBlock, nextBlock );

        { // if assignee has insufficient capacity
            scope->builder->SetInsertPoint( trueBlock );
            panicNode->code_gen( context, scope );
            scope->builder->CreateBr( nextBlock );  // terminate block, though won't be executed
        }

        scope->builder->SetInsertPoint( nextBlock );
    }

    // set assignee's length equal to source's length:
    auto dstLenPtrV = scope->builder->CreateInBoundsGEP( lvalArrayPtrV, lenIxs );
    scope->builder->CreateStore( srcLenV, dstLenPtrV );

    {
        // Current implementation is to memcpy the data. (This will include uninitialized padding bytes.)
        // FUTURE: For non-elementary element types, copy each element according to type's copy constructor, if allowed.
        // compute array data size:
        auto lvalTypeIdV = this->lvalue->code_gen_typeid( context, scope );
        auto elemSizeV = context.gen_get_element_size( scope, this->lvalue->qtype().type(), lvalTypeIdV );
        auto elemSize64V = scope->builder->CreateZExtOrBitCast( elemSizeV, Type::getInt64Ty( context.llvmContext ) );
        auto srcLen64V = scope->builder->CreateZExtOrBitCast( srcLenV, Type::getInt64Ty( context.llvmContext ) );
        auto dataSizeV = scope->builder->CreateMul( elemSize64V, srcLen64V, "datasize" );

        // mem-copy array contents:
        Value* dataIxs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                             ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 2 ) };
        auto dstDataPtrV = scope->builder->CreateInBoundsGEP( lvalArrayPtrV, dataIxs );
        auto srcDataPtrV = scope->builder->CreateInBoundsGEP( rvalArrayPtrV, dataIxs );
        scope->builder->CreateMemCpy( dstDataPtrV, srcDataPtrV, dataSizeV, 8 );
    }
}
