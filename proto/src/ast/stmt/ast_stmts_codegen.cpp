#include "ast_stmts.hpp"
#include "ast_assertstmt_node.hpp"
#include "ast_panicstmt_node.hpp"
#include "symbol/package.hpp"
#include "driver.hpp"

#include "llvm_generator.hpp"

using namespace llvm;

///** Create an alloca instruction in the entry block of the current function.
// * This is used for variables encountered throughout the function that shall be viable for mem2reg.
// */
//static AllocaInst *create_entry_block_alloca(GenScope* scope, Type* varType, const std::string &varName) {
//    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
//    IRBuilder<> tmpB(&parentFunc->getEntryBlock(), parentFunc->getEntryBlock().begin());
//    return tmpB.CreateAlloca(varType, 0, varName);
//}

///** Create an alloca instruction in the appropriate block of the current function.
// * This is used for variables encountered throughout the function.
// */
//static AllocaInst *create_alloca(GenScope* scope, Type* varType, const std::string &varName) {
//    return scope->builder->CreateAlloca(varType, 0, varName);
//}

/** @param lval must be of pointer type */
// Note, also used by Lambda expr
void do_store( LlvmGenerationContext& context, GenScope* scope, Value* lval, Value* rval ) {
    if ( rval->getType()->isPointerTy() && lval->getType()->getPointerElementType() == rval->getType()->getPointerElementType() ) {
        rval = scope->builder->CreateLoad( rval );
    }
    scope->builder->CreateStore( rval, lval );
}


void TxFieldStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( this->field->typeExpression )
        this->field->typeExpression->code_gen_type( context );
    auto declaration = this->field->get_declaration();
    auto uniqueName = declaration->get_unique_full_name();
    ASSERT( declaration->get_storage() == TXS_STACK, "TxFieldStmtNode can only apply to TX_STACK storage fields: " << uniqueName );
    auto txType = this->field->get_type()->type();

    // If init expression does a stack allocation of this field's type (instance-equivalent type),
    // this field shall bind to that allocation.

    Value* fieldVal; // = txType->gen_alloca(context, scope, declaration->get_symbol()->get_name());
    if ( this->field->initExpression ) {
        if ( this->field->initExpression->is_stack_allocation_expression() ) {
            fieldVal = this->field->initExpression->code_gen_expr( context, scope );
        }
        else {
            fieldVal = txType->gen_alloca( context, scope, declaration->get_symbol()->get_name() );
            // create implicit assignment statement
            if ( Value* initializer = this->field->initExpression->code_gen_expr( context, scope ) )
                do_store( context, scope, fieldVal, initializer );
        }
    }
    else {
        fieldVal = txType->gen_alloca( context, scope, declaration->get_symbol()->get_name() );
        // We don't automatically invoke default constructor (in future, a code flow validator should check that initialized before first use)
    }
    context.register_llvm_value( uniqueName, fieldVal );
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
    do_store( context, scope, lval, rval );
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

void TxElseClauseNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->body->code_gen( context, scope );
}

void TxIfStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );

    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
    BasicBlock* trueBlock = BasicBlock::Create( context.llvmContext, "if_true", parentFunc );
    BasicBlock* nextBlock = nullptr;

    // generate condition:
    auto condVal = this->cond->code_gen_expr( context, scope );

    // generate branch and else code:
    if ( this->elseClause ) {
        BasicBlock* elseBlock = BasicBlock::Create( context.llvmContext, "if_else", parentFunc );
        scope->builder->CreateCondBr( condVal, trueBlock, elseBlock );
        scope->builder->SetInsertPoint( elseBlock );
        this->elseClause->code_gen( context, scope );

        if ( !this->elseClause->ends_with_terminal_stmt() ) {
            nextBlock = BasicBlock::Create( context.llvmContext, "if_next", parentFunc );
            scope->builder->CreateBr( nextBlock );  // branch from end of else suite to next-block
        }
    }
    else {
        nextBlock = BasicBlock::Create( context.llvmContext, "if_next", parentFunc );
        scope->builder->CreateCondBr( condVal, trueBlock, nextBlock );
    }

    // generate true code:
    scope->builder->SetInsertPoint( trueBlock );
    this->body->code_gen( context, scope );
    if ( nextBlock ) {
        // note: trueBlock may not be the "current" block anymore when reaching end of body
        if ( !scope->builder->GetInsertBlock()->getTerminator() )
            scope->builder->CreateBr( nextBlock );  // branch from end of true block to next-block
        scope->builder->SetInsertPoint( nextBlock );
    }
}

void TxWhileStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );

    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
    BasicBlock* condBlock = BasicBlock::Create( context.llvmContext, "while_cond", parentFunc );
    BasicBlock* loopBlock = BasicBlock::Create( context.llvmContext, "while_loop", parentFunc );
    BasicBlock* nextBlock = nullptr;

    // generate condition block:
    scope->builder->CreateBr( condBlock );  // branch from end of preceding block to condition-block
    scope->builder->SetInsertPoint( condBlock );
    auto condVal = this->cond->code_gen_expr( context, scope );

    // generate else code:
    if ( this->elseClause ) {
        BasicBlock* elseBlock = BasicBlock::Create( context.llvmContext, "while_else", parentFunc );
        scope->builder->CreateCondBr( condVal, loopBlock, elseBlock );
        scope->builder->SetInsertPoint( elseBlock );
        this->elseClause->code_gen( context, scope );

        if ( !this->ends_with_terminal_stmt() ) {
            nextBlock = BasicBlock::Create( context.llvmContext, "while_next", parentFunc );
            scope->builder->CreateBr( nextBlock );  // branch from end of else suite to next-block
        }
    }
    else {
        nextBlock = BasicBlock::Create( context.llvmContext, "while_next", parentFunc );
        scope->builder->CreateCondBr( condVal, loopBlock, nextBlock );
    }

    // generate true code:
    CompoundStatementScope css( condBlock, nextBlock );
    scope->compStmtStack.push( &css );
    scope->builder->SetInsertPoint( loopBlock );
    this->body->code_gen( context, scope );
    scope->compStmtStack.pop();
    // note: loopBlock is may not be the "current" block anymore when reaching end of loop body
    if ( !scope->builder->GetInsertBlock()->getTerminator() )
        scope->builder->CreateBr( condBlock );  // branch from end of loop body to condition-block

    if ( nextBlock )
        scope->builder->SetInsertPoint( nextBlock );
}

void TxCallStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    this->call->code_gen_expr( context, scope );
}

void TxReturnStmtNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    if ( this->expr ) {
        auto exprV = this->expr->code_gen_expr( context, scope );
        // TODO: this is hackish, can we find systematic solution?
        auto expectedT = context.get_llvm_type( this->expr->get_type() );
        if ( exprV->getType() == expectedT )
            scope->builder->CreateRet( exprV );
        else if ( exprV->getType()->isPointerTy() && exprV->getType()->getPointerElementType() == expectedT ) {
            LOG_DEBUG( context.LOGGER(),
                       "auto-loading return value type " << ::to_string(exprV->getType()) << "  to expected  " << ::to_string(expectedT) );
            scope->builder->CreateRet( scope->builder->CreateLoad( exprV ) );
        }
        else {
            LOG( context.LOGGER(), ERROR,
                 "Mismatching return value type: " << ::to_string(exprV->getType()) << " is not as expected " << ::to_string(expectedT) );
            scope->builder->CreateRet( exprV );
        }
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
