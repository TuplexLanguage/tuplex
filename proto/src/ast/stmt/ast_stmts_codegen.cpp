#include "ast_stmts.hpp"
#include "ast_assertstmt_node.hpp"
#include "ast_panicstmt_node.hpp"
#include "symbol/package.hpp"
#include "driver.hpp"

#include "llvm_generator.hpp"

using namespace llvm;


void TxFieldStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( this->field->typeExpression )
        this->field->typeExpression->code_gen_type( context );
    auto declaration = this->field->get_declaration();
    auto uniqueName = declaration->get_unique_full_name();
    ASSERT( declaration->get_storage() == TXS_STACK, "TxFieldStmtNode can only apply to TX_STACK storage fields: " << uniqueName );
    auto txType = this->field->qualtype()->type()->acttype();

    // If init expression does a stack allocation of this field's type (instance-equivalent type),
    // this field shall bind to that allocation.

    Value* fieldValPtr;
    if ( this->field->initExpression ) {
        if ( this->field->initExpression->is_stack_allocation_expression() ) {
            fieldValPtr = this->field->initExpression->code_gen_addr( context, scope );
        }
        else {
            fieldValPtr = txType->gen_alloca( context, scope, declaration->get_symbol()->get_name() );
            // create implicit assignment statement
            if ( Value* initializer = this->field->initExpression->code_gen_expr( context, scope ) )
                scope->builder->CreateStore( initializer, fieldValPtr );
        }
    }
    else {
        fieldValPtr = txType->gen_alloca( context, scope, declaration->get_symbol()->get_name() );
        // We don't automatically invoke default constructor (in future, a code flow validator should check that initialized before first use)
    }
    context.register_llvm_value( uniqueName, fieldValPtr );
}

void TxTypeStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    this->typeDecl->code_gen( context );
}

void TxAssignStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    auto rval = this->rvalue->code_gen_expr( context, scope );
    auto lval = this->lvalue->code_gen_address( context, scope );
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
