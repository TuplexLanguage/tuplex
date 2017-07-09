#include "ast_flow.hpp"

#include "llvm_generator.hpp"

using namespace llvm;


Value* TxWhileHeaderNode::code_gen_cond( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->nextCond->code_gen_expr( context, scope );
}

void TxForHeaderNode::code_gen_init( LlvmGenerationContext& context, GenScope* scope ) const {
    this->initStmt->code_gen( context, scope );
}

Value* TxForHeaderNode::code_gen_cond( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->nextCond->expr->code_gen_expr( context, scope );
}

void TxForHeaderNode::code_gen_poststep( LlvmGenerationContext& context, GenScope* scope ) const {
    this->stepStmt->code_gen( context, scope );
}

void TxInClauseNode::code_gen_init( LlvmGenerationContext& context, GenScope* scope ) const {
    this->iterField->code_gen_field( context, scope );
}

Value* TxInClauseNode::code_gen_cond( LlvmGenerationContext& context, GenScope* scope ) const {
    return this->nextCond->code_gen_expr( context, scope );
}

void TxInClauseNode::code_gen_prestep( LlvmGenerationContext& context, GenScope* scope ) const {
    this->valueField->code_gen_field( context, scope );
}


void TxElseClauseNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->body->code_gen( context, scope );
}

void TxIfStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );

    std::string id = std::to_string( this->ploc.begin.line );
    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
    BasicBlock* trueBlock = BasicBlock::Create( context.llvmContext, "if_true"+id, parentFunc );
    BasicBlock* postBlock = nullptr;

    // generate condition:
    auto condVal = this->cond->code_gen_expr( context, scope );

    // generate branch and else code:
    if ( this->elseClause ) {
        BasicBlock* elseBlock = BasicBlock::Create( context.llvmContext, "if_else"+id, parentFunc );
        scope->builder->CreateCondBr( condVal, trueBlock, elseBlock );
        scope->builder->SetInsertPoint( elseBlock );
        this->elseClause->code_gen( context, scope );

        if ( !this->elseClause->ends_with_terminal_stmt() ) {
            postBlock = BasicBlock::Create( context.llvmContext, "if_post"+id, parentFunc );
            scope->builder->CreateBr( postBlock );  // branch from end of else suite to next-block
        }
    }
    else {
        postBlock = BasicBlock::Create( context.llvmContext, "if_post"+id, parentFunc );
        scope->builder->CreateCondBr( condVal, trueBlock, postBlock );
    }

    // generate true code:
    scope->builder->SetInsertPoint( trueBlock );
    this->body->code_gen( context, scope );
    if ( postBlock ) {
        // note: trueBlock may not be the "current" block anymore when reaching end of body
        if ( !scope->builder->GetInsertBlock()->getTerminator() )
            scope->builder->CreateBr( postBlock );  // branch from end of true block to next-block
        scope->builder->SetInsertPoint( postBlock );
    }
}

void TxForStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );

    std::string id = std::to_string( this->ploc.begin.line );
    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
    BasicBlock* condBlock = BasicBlock::Create( context.llvmContext, "loop_cond"+id, parentFunc );
    BasicBlock* loopBlock = BasicBlock::Create( context.llvmContext, "loop_body"+id, parentFunc );
    BasicBlock* postBlock = nullptr;

    // generate initialization:
    for ( auto clause : *this->loopHeaders ) {
        clause->code_gen_init( context, scope );
    }
    scope->builder->CreateBr( condBlock );

    // generate condition block:
    scope->builder->SetInsertPoint( condBlock );
    Value* condVal = this->loopHeaders->back()->code_gen_cond( context, scope );
    if ( this->loopHeaders->size() > 1 ) {
        // TODO: test all the conditions
        for ( int i = this->loopHeaders->size()-1 ; i >= 0; i-- ) {
            condVal = scope->builder->CreateAnd( this->loopHeaders->at(i)->code_gen_cond( context, scope ), condVal );
        }
    }

    if ( this->elseClause ) {
        BasicBlock* elseBlock = BasicBlock::Create( context.llvmContext, "loop_else"+id, parentFunc );
        scope->builder->CreateCondBr( condVal, loopBlock, elseBlock );

        // generate else code:
        scope->builder->SetInsertPoint( elseBlock );
        this->elseClause->code_gen( context, scope );
        if ( !this->ends_with_terminal_stmt() ) {
            postBlock = BasicBlock::Create( context.llvmContext, "loop_post"+id, parentFunc );
            scope->builder->CreateBr( postBlock );  // branch from end of else suite to post-block
        }
    }
    else {
        postBlock = BasicBlock::Create( context.llvmContext, "loop_post"+id, parentFunc );
        scope->builder->CreateCondBr( condVal, loopBlock, postBlock );
    }

    // generate loop code:
    CompoundStatementScope css( condBlock, postBlock );
    scope->compStmtStack.push( &css );
    {
        scope->builder->SetInsertPoint( loopBlock );
        for ( auto clause : *this->loopHeaders ) {
            clause->code_gen_prestep( context, scope );
        }
        this->body->code_gen( context, scope );
        // note: loopBlock is may not be the "current" block anymore when reaching end of loop body
        if ( !scope->builder->GetInsertBlock()->getTerminator() ) {
            for ( auto clause : *this->loopHeaders ) {
                clause->code_gen_poststep( context, scope );
            }
            scope->builder->CreateBr( condBlock );  // branch from end of loop body to cond-block
        }
    }
    scope->compStmtStack.pop();

    if ( postBlock )
        scope->builder->SetInsertPoint( postBlock );
}
