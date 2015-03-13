#include "ast_stmts.hpp"
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
static Value* do_store(LlvmGenerationContext& context, GenScope* scope, Value* lval, Value* rval) {
    if (rval->getType()->isPointerTy() && lval->getType()->getPointerElementType() == rval->getType()->getPointerElementType()) {
        rval = scope->builder->CreateLoad(rval);
    }
    return scope->builder->CreateStore(rval, lval);
}


Value* TxLambdaExprNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    FunctionType *ftype = cast<FunctionType>(context.get_llvm_type(this->funcTypeNode->get_type()));
    ASSERT(ftype, "Couldn't get LLVM type for function type " << this->funcTypeNode->get_type());
    std::string funcName = ""; // anonymous function
    if (this->fieldDefNode) {
        funcName = this->fieldDefNode->get_entity()->get_full_name().to_string();
    }
    context.LOG.debug("Creating function: %s", funcName.c_str());
    Function *function = cast<Function>(context.llvmModule.getOrInsertFunction(funcName, ftype));
    // function->setLinkage(GlobalValue::InternalLinkage);  FIXME (can cause LLVM to rename function)
    //Function *function = Function::Create(ftype, GlobalValue::InternalLinkage,
    //                                                  funcName.c_str(), &context.llvmModule);
    // note: function is of LLVM function pointer type (since it is an LLVM global value)

    BasicBlock *entryBlock = BasicBlock::Create(context.llvmContext, "entry", function);
    IRBuilder<> builder( entryBlock );
    GenScope fscope(&builder);

    // name the concrete args and allocate them on the stack:
    auto argDefI = this->funcTypeNode->arguments->begin();
    for (Function::arg_iterator fArgI = function->arg_begin();
         fArgI != function->arg_end();  fArgI++, argDefI++)
    {
        (*argDefI)->typeExpression->code_gen(context, scope);
        auto entity = (*argDefI)->get_entity();
        fArgI->setName(entity->get_name());
        auto txType = entity->get_type();
//        Type* llvmType = context.get_llvm_type(txType);
//        if (! llvmType)
//            return nullptr;
        auto argField = txType->gen_alloca(context, &fscope, entity->get_name() + "_");
        do_store(context, &fscope, argField, fArgI);
        context.register_llvm_value(entity->get_full_name().to_string(), argField);
    }

    this->suite->code_gen(context, &fscope);

    if (! fscope.builder->GetInsertBlock()->getTerminator()) {
        context.LOG.debug("inserting default void return instruction for last block of function %s", funcName.c_str());
        fscope.builder->CreateRetVoid();
        //ReturnInst::Create(context.llvmContext, entryBlock);
    }
    ASSERT (entryBlock->getTerminator(), "Function entry block has no terminator");

    return function;
}


Value* TxFieldStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    if (this->field->typeExpression)
        this->field->typeExpression->code_gen(context, scope);
    auto entity = this->field->get_entity();
    ASSERT (entity->get_storage() == TXS_STACK, "TxFieldStmtNode can only apply to TX_STACK storage fields: " << entity->get_full_name());
    auto txType = entity->get_type();
    Value* fieldVal;
    if (dynamic_cast<const TxFunctionType*>(txType)) {
        // FUTURE: make local function capture
        if (this->field->initExpression)
            fieldVal = this->field->initExpression->code_gen(context, scope);
        else {
            // TODO: Local function pointers without immediate initializer
            context.LOG.error("%s: Local function pointers without immediate initializer not yet supported", this->parse_loc_string().c_str());
            return nullptr;
        }
    }
    else {  // LLVM "FirstClassType"
        fieldVal = txType->gen_alloca(context, scope, entity->get_name());
        if (this->field->initExpression) {
            // create implicit assignment statement
            if (Value* initializer = this->field->initExpression->code_gen(context, scope))
                do_store(context, scope, fieldVal, initializer);
        }
    }
    context.register_llvm_value(entity->get_full_name().to_string(), fieldVal);
    return fieldVal;
}

Value* TxTypeStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    return this->typeDecl->code_gen(context, scope);
}


Value* TxAssignStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto rval = this->rvalue->code_gen(context, scope);
    auto lval = this->lvalue->code_gen(context, scope);
    if ((! lval) || (! rval))
        return NULL;
    if (! lval->getType()->isPointerTy()) {
        context.LOG.error("At %s: L-value is not of pointer type: %s; %s", this->parse_loc_string().c_str(), ::to_string(lval).c_str(), ::to_string(lval->getType()).c_str());
        return rval;
    }
    return do_store(context, scope, lval, rval);
}

Value* TxDerefAssigneeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto refval = this->operand->code_gen(context, scope);
    if (! refval)
        return NULL;
    return refval;
}



Value* TxSuiteNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());

//    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
//    BasicBlock* suiteBlock = BasicBlock::Create(context.llvmContext,  "suite", parentFunc);
//    scope->builder->SetInsertPoint(suiteBlock);
    for (auto stmt : *this->suite)
        stmt->code_gen(context, scope);
//    scope->builder->SetInsertPoint(continuationBlock);

    return NULL;
}

Value* TxElseClauseNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    return this->suite->code_gen(context, scope);
}

Value* TxIfStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());

    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
    BasicBlock*  trueBlock = BasicBlock::Create(context.llvmContext, "if_true", parentFunc);
    BasicBlock* falseBlock = BasicBlock::Create(context.llvmContext, "if_false", parentFunc);
    BasicBlock* postBlock;

    auto condVal = this->cond->code_gen(context, scope);
    auto condInstr = scope->builder->CreateCondBr(condVal, trueBlock, falseBlock);

    if (this->elseClause) {
        postBlock = BasicBlock::Create(context.llvmContext, "if_post", parentFunc);

        scope->builder->SetInsertPoint(falseBlock);
        this->elseClause->code_gen(context, scope);
        if (! scope->builder->GetInsertBlock()->getTerminator())
            scope->builder->CreateBr(postBlock);  // branch from end of else suite to post-block
    }
    else
        postBlock = falseBlock;

    scope->builder->SetInsertPoint(trueBlock);
    this->suite->code_gen(context, scope);
    // note: trueBlock is may not be the "current" block anymore when reaching end of true body
    if (! scope->builder->GetInsertBlock()->getTerminator())
        scope->builder->CreateBr(postBlock);  // branch from end of true suite to post-block

    scope->builder->SetInsertPoint(postBlock);

    return condInstr;
}

Value* TxWhileStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());

    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
    BasicBlock* condBlock = BasicBlock::Create(context.llvmContext, "while_cond", parentFunc);
    BasicBlock* loopBlock = BasicBlock::Create(context.llvmContext, "while_loop", parentFunc);
    BasicBlock* elseBlock = BasicBlock::Create(context.llvmContext, "while_else", parentFunc);
    BasicBlock* postBlock;

    scope->builder->CreateBr(condBlock);  // branch from end of preceding block to condition-block
    scope->builder->SetInsertPoint(condBlock);
    auto condVal = this->cond->code_gen(context, scope);
    auto condInstr = scope->builder->CreateCondBr(condVal, loopBlock, elseBlock);

    if (this->elseClause) {
        postBlock = BasicBlock::Create(context.llvmContext, "while_post", parentFunc);

        scope->builder->SetInsertPoint(elseBlock);
        this->elseClause->code_gen(context, scope);
        if (! scope->builder->GetInsertBlock()->getTerminator())
            scope->builder->CreateBr(postBlock);  // branch from end of else body to post-block
    }
    else
        postBlock = elseBlock;

    CompoundStatementScope css(condBlock, postBlock);
    scope->compStmtStack.push(&css);
    scope->builder->SetInsertPoint(loopBlock);
    this->suite->code_gen(context, scope);
    scope->compStmtStack.pop();
    // note: loopBlock is may not be the "current" block anymore when reaching end of loop body
    if (! scope->builder->GetInsertBlock()->getTerminator())
        scope->builder->CreateBr(condBlock);  // branch from end of loop body to condition-block

    scope->builder->SetInsertPoint(postBlock);

    return condInstr;
}

Value* TxCallStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    return this->call->code_gen(context, scope);
}

Value* TxReturnStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    if (this->expr)
        return scope->builder->CreateRet(this->expr->code_gen(context, scope));
    else
        return scope->builder->CreateRetVoid();
}

Value* TxBreakStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    return scope->builder->CreateBr(scope->compStmtStack.top()->breakBlock);
}

Value* TxContinueStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    return scope->builder->CreateBr(scope->compStmtStack.top()->continueBlock);
}
