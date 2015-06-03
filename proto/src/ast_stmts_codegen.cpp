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


static Value* gen_local_field(LlvmGenerationContext& context, GenScope* scope, const TxField* field, Value* fieldV) {
    fieldV->setName(field->get_unique_name());
    const TxType* txType = field->get_type();
    auto fieldA = txType->gen_alloca(context, scope, field->get_unique_name() + "_");
    do_store(context, scope, fieldA, fieldV);
    context.register_llvm_value(field->get_declaration()->get_unique_full_name(), fieldA);
    return fieldA;
}


Value* TxLambdaExprNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());

    // FUTURE: if this is a lambda within a code-block, define the implicit closure object here

    //FunctionType *ftype = cast<FunctionType>(context.get_llvm_type(this->funcTypeNode->get_type()));
    StructType *lambdaT = cast<StructType>(context.get_llvm_type(this->funcTypeNode->get_type()));
    FunctionType *funcT = cast<FunctionType>(cast<PointerType>(lambdaT->getElementType(0))->getPointerElementType());
    ASSERT(funcT, "Couldn't get LLVM type for function type " << this->funcTypeNode->get_type());

    std::string funcName;
    if (this->fieldDefNode) {
        auto declaration = this->fieldDefNode->get_declaration();
        if (declaration->get_decl_flags() & TXD_CONSTRUCTOR)
            funcName = declaration->get_unique_full_name();
        else
            funcName = declaration->get_unique_full_name() + "$func";
    }
    else
        funcName = "$func";  // anonymous function

    context.LOG.debug("Creating function: %s", funcName.c_str());
    Function *function = cast<Function>(context.llvmModule.getOrInsertFunction(funcName, funcT));
    // function->setLinkage(GlobalValue::InternalLinkage);  TODO (can cause LLVM to rename function)
    //Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, funcName.c_str(), &context.llvmModule);
    // note: function is of LLVM function pointer type (since it is an LLVM global value)

    // generate the function body:
    BasicBlock *entryBlock = BasicBlock::Create(context.llvmContext, "entry", function);
    IRBuilder<> builder( entryBlock );
    GenScope fscope(&builder);

    // name the concrete args (and self, if present) and allocate them on the stack:
    Function::arg_iterator fArgI = function->arg_begin();
    if (this->selfRefNode) {
        // (both self and super refer to the same object, but with different ref types)
        {
            this->selfRefNode->typeExpression->code_gen(context, &fscope);
            auto selfT = context.get_llvm_type(this->selfRefNode->get_type());
            auto convSelfV = TxReferenceType::gen_ref_conversion(context, &fscope, fArgI, selfT);
            gen_local_field(context, &fscope, this->selfRefNode->get_field(), convSelfV);
        }
        {
            this->superRefNode->typeExpression->code_gen(context, &fscope);
            auto superT = context.get_llvm_type(this->superRefNode->get_type());
            auto convSuperV = TxReferenceType::gen_ref_conversion(context, &fscope, fArgI, superT);
            gen_local_field(context, &fscope, this->superRefNode->get_field(), convSuperV);
        }
    }
    fArgI++;
    for (auto argDefI = this->funcTypeNode->arguments->cbegin();
         argDefI != this->funcTypeNode->arguments->cend();
         fArgI++, argDefI++)
    {
        (*argDefI)->typeExpression->code_gen(context, &fscope);
        gen_local_field(context, &fscope, (*argDefI)->get_field(), fArgI);
    }

    this->suite->code_gen(context, &fscope);

    if (! fscope.builder->GetInsertBlock()->getTerminator()) {
        context.LOG.debug("inserting default void return instruction for last block of function %s", funcName.c_str());
        fscope.builder->CreateRetVoid();
    }
    ASSERT (entryBlock->getTerminator(), "Function entry block has no terminator");

    // construct the lambda object:
    auto nullClosureRefV = Constant::getNullValue(lambdaT->getElementType(1));
    auto lambdaV = ConstantStruct::get(lambdaT, function, nullClosureRefV, NULL);
    //auto lambdaV = gen_lambda(context, scope, lambdaT, function, nullClosureRefV);
    return lambdaV;
}


Value* TxFieldStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    if (this->field->typeExpression)
        this->field->typeExpression->code_gen(context, scope);
    auto declaration = this->field->get_declaration();
    auto uniqueName = declaration->get_unique_full_name();
    ASSERT (declaration->get_storage() == TXS_STACK, "TxFieldStmtNode can only apply to TX_STACK storage fields: " << uniqueName);
    auto txType = this->field->get_type();
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
        fieldVal = txType->gen_alloca(context, scope, declaration->get_symbol()->get_name());
        if (this->field->initExpression) {
            // create implicit assignment statement
            if (Value* initializer = this->field->initExpression->code_gen(context, scope))
                do_store(context, scope, fieldVal, initializer);
        }
    }
    context.register_llvm_value(uniqueName, fieldVal);
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
