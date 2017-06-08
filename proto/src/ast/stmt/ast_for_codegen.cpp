#include "ast_for.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

// defined in ast_stmts_codegen.cpp  TODO: review
extern void do_store( LlvmGenerationContext& context, GenScope* scope, Value* lval, Value* rval );

static void code_gen_field( LlvmGenerationContext& context, GenScope* scope, TxFieldDefNode* field ) {
    auto declaration = field->get_declaration();

    // If init expression does a stack allocation of this field's type (instance-equivalent type),
    // this field shall bind to that allocation.
    Value* fieldVal;
    if ( field->initExpression->is_stack_allocation_expression() ) {
        fieldVal = field->initExpression->code_gen_expr( context, scope );
    }
    else {
        auto txType = field->get_type()->type();
        fieldVal = txType->gen_alloca( context, scope, declaration->get_symbol()->get_name() );
        // create implicit assignment statement
        if ( Value* initializer = field->initExpression->code_gen_expr( context, scope ) )
            do_store( context, scope, fieldVal, initializer );
    }

    context.register_llvm_value( declaration->get_unique_full_name(), fieldVal );
}

void TxInClauseNode::code_gen_init( LlvmGenerationContext& context, GenScope* scope ) const {
    code_gen_field( context, scope, this->iterField );
}

Value* TxInClauseNode::code_gen_cond( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->nextCond->code_gen_expr( context, scope );
}

void TxInClauseNode::code_gen_step( LlvmGenerationContext& context, GenScope* scope ) const {
    code_gen_field( context, scope, this->valueField );
}

void TxForStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );

    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
    BasicBlock* condBlock = BasicBlock::Create( context.llvmContext, "for_cond", parentFunc );
    BasicBlock* loopBlock = BasicBlock::Create( context.llvmContext, "for_loop", parentFunc );
    BasicBlock* postBlock = nullptr;

    // generate initialization:
    for ( auto clause : *this->inClauses ) {
        clause->code_gen_init( context, scope );
    }
    scope->builder->CreateBr( condBlock );

    // generate condition block:
    scope->builder->SetInsertPoint( condBlock );
    // TODO: test all the conditions
    //for ( auto clause : *this->inClauses ) {
    //}
    auto condVal = this->inClauses->front()->code_gen_cond( context, scope );

    if ( this->elseClause ) {
        BasicBlock* elseBlock = BasicBlock::Create( context.llvmContext, "for_else", parentFunc );
        scope->builder->CreateCondBr( condVal, loopBlock, elseBlock );

        // generate else code:
        scope->builder->SetInsertPoint( elseBlock );
        this->elseClause->code_gen( context, scope );
        if ( !this->ends_with_terminal_stmt() ) {
            postBlock = BasicBlock::Create( context.llvmContext, "for_post", parentFunc );
            scope->builder->CreateBr( postBlock );  // branch from end of else suite to post-block
        }
    }
    else {
        postBlock = BasicBlock::Create( context.llvmContext, "for_post", parentFunc );
        scope->builder->CreateCondBr( condVal, loopBlock, postBlock );
    }

    // generate loop code:
    CompoundStatementScope css( condBlock, postBlock );
    scope->compStmtStack.push( &css );
    {
        scope->builder->SetInsertPoint( loopBlock );
        for ( auto clause : *this->inClauses ) {
            clause->code_gen_step( context, scope );
        }
        this->body->code_gen( context, scope );
        // note: loopBlock is may not be the "current" block anymore when reaching end of loop body
        if ( !scope->builder->GetInsertBlock()->getTerminator() )
            scope->builder->CreateBr( condBlock );  // branch from end of loop body to cond-block
    }
    scope->compStmtStack.pop();

    if ( postBlock )
        scope->builder->SetInsertPoint( postBlock );
}
