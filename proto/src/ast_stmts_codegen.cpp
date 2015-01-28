#include "ast_stmts.hpp"
#include "llvm_generator.hpp"


llvm::Value* TxFieldStmtNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto entity = this->field->get_entity();
    ASSERT (entity->get_storage() == TXS_STACK, "TxFieldStmtNode can only apply to TX_STACK storage fields: " << entity->get_full_name());
    auto txType = entity->get_type();
    llvm::Type* llvmType = context.getLlvmType(txType);
    if (! llvmType) {
        return nullptr;
    }
    //scope->builder->CreateAlloca();
    llvm::Value* fieldVal;
    if (this->field->initExpression) {
        if (entity->is_modifiable()) {
            // allocate variable on stack
            fieldVal = new llvm::AllocaInst(llvmType, entity->get_full_name().to_string().c_str(), scope->builder->GetInsertBlock());
            // create implicit assignment statement
            llvm::Value* initializer = this->field->initExpression->codeGen(context, scope);
            scope->builder->CreateStore(initializer, fieldVal);
        }
        else {
            // keep value in register
            fieldVal = this->field->initExpression->codeGen(context, scope);
        }
    }
    else {
        // allocate variable on stack
        fieldVal = new llvm::AllocaInst(llvmType, entity->get_full_name().to_string().c_str(), scope->builder->GetInsertBlock());
    }
    // Note: Emit the initializer before adding the variable to scope, this prevents
    // the initializer from referencing the variable itself, and permits stuff like this:
    //  var a = 1 in
    //    var a = a in ...   # refers to outer 'a'.
    context.register_llvm_value(entity->get_full_name().to_string(), fieldVal);
    return fieldVal;
}

llvm::Value* TxTypeStmtNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    return this->typeDecl->codeGen(context, scope);
}


llvm::Value* TxAssignStmtNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto rval = this->rvalue->codeGen(context, scope);
    auto lval = this->lvalue->codeGen(context, scope);
    if ((! lval) || (! rval))
        return NULL;
    if (! lval->getType()->isPointerTy()) {
        context.LOG.error("L-value is not of pointer type: %s; %s", ::to_string(lval).c_str(), ::to_string(lval->getType()).c_str());
        return rval;
    }
    return scope->builder->CreateStore(rval, lval);
}

llvm::Value* TxDerefAssigneeNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto refval = this->operand->codeGen(context, scope);
    if (! refval)
        return NULL;
    return refval;
}



llvm::Value* TxSuiteNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());

//    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
//    llvm::BasicBlock* suiteBlock = llvm::BasicBlock::Create(context.llvmContext,  "suite", parentFunc);
//    scope->builder->SetInsertPoint(suiteBlock);
    for (auto stmt : *this->suite)
        stmt->codeGen(context, scope);
//    scope->builder->SetInsertPoint(continuationBlock);

    return NULL;
}

llvm::Value* TxElseClauseNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    return this->suite->codeGen(context, scope);
}

llvm::Value* TxIfStmtNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());

    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
    llvm::BasicBlock*  trueBlock = llvm::BasicBlock::Create(context.llvmContext, "if_true", parentFunc);
    llvm::BasicBlock* falseBlock = llvm::BasicBlock::Create(context.llvmContext, "if_false", parentFunc);
    llvm::BasicBlock* postBlock;

    auto condVal = this->cond->codeGen(context, scope);
    auto condInstr = scope->builder->CreateCondBr(condVal, trueBlock, falseBlock);

    if (this->elseClause) {
        postBlock = llvm::BasicBlock::Create(context.llvmContext, "if_post", parentFunc);

        scope->builder->SetInsertPoint(falseBlock);
        this->elseClause->codeGen(context, scope);
        if (! scope->builder->GetInsertBlock()->getTerminator())
            scope->builder->CreateBr(postBlock);  // branch from end of else suite to post-block
    }
    else
        postBlock = falseBlock;

    scope->builder->SetInsertPoint(trueBlock);
    this->suite->codeGen(context, scope);
    // note: trueBlock is may not be the "current" block anymore when reaching end of true body
    if (! scope->builder->GetInsertBlock()->getTerminator())
        scope->builder->CreateBr(postBlock);  // branch from end of true suite to post-block

    scope->builder->SetInsertPoint(postBlock);

    return condInstr;
}

llvm::Value* TxWhileStmtNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());

    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
    llvm::BasicBlock* condBlock = llvm::BasicBlock::Create(context.llvmContext, "while_cond", parentFunc);
    llvm::BasicBlock* loopBlock = llvm::BasicBlock::Create(context.llvmContext, "while_loop", parentFunc);
    llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(context.llvmContext, "while_else", parentFunc);
    llvm::BasicBlock* postBlock;

    scope->builder->CreateBr(condBlock);  // branch from end of preceding block to condition-block
    scope->builder->SetInsertPoint(condBlock);
    auto condVal = this->cond->codeGen(context, scope);
    auto condInstr = scope->builder->CreateCondBr(condVal, loopBlock, elseBlock);

    if (this->elseClause) {
        postBlock = llvm::BasicBlock::Create(context.llvmContext, "while_post", parentFunc);

        scope->builder->SetInsertPoint(elseBlock);
        this->elseClause->codeGen(context, scope);
        if (! scope->builder->GetInsertBlock()->getTerminator())
            scope->builder->CreateBr(postBlock);  // branch from end of else body to post-block
    }
    else
        postBlock = elseBlock;

    CompoundStatementScope css(condBlock, postBlock);
    scope->compStmtStack.push(&css);
    scope->builder->SetInsertPoint(loopBlock);
    this->suite->codeGen(context, scope);
    scope->compStmtStack.pop();
    // note: loopBlock is may not be the "current" block anymore when reaching end of loop body
    if (! scope->builder->GetInsertBlock()->getTerminator())
        scope->builder->CreateBr(condBlock);  // branch from end of loop body to condition-block

    scope->builder->SetInsertPoint(postBlock);

    return condInstr;
}

llvm::Value* TxCallStmtNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    return this->call->codeGen(context, scope);
}

llvm::Value* TxReturnStmtNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    if (this->expr)
        return scope->builder->CreateRet(this->expr->codeGen(context, scope));
    else
        return scope->builder->CreateRetVoid();
}

llvm::Value* TxBreakStmtNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    return scope->builder->CreateBr(scope->compStmtStack.top()->breakBlock);
}

llvm::Value* TxContinueStmtNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    return scope->builder->CreateBr(scope->compStmtStack.top()->continueBlock);
}
