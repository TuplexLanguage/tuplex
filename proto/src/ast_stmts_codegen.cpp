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


Function* TxLambdaExprNode::code_gen_forward_decl( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN(this, context, " forward declaration");
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

    //FunctionType *ftype = cast<FunctionType>(context.get_llvm_type(this->funcTypeNode->get_type()));
    StructType *lambdaT = cast<StructType>(context.get_llvm_type(this->funcTypeNode->get_type()));
    FunctionType *funcT = cast<FunctionType>(cast<PointerType>(lambdaT->getElementType(0))->getPointerElementType());
    ASSERT(funcT, "Couldn't get LLVM type for function type " << this->funcTypeNode->get_type());

    Function* function = cast<Function>(context.llvmModule.getOrInsertFunction(funcName, funcT));
    // function->setLinkage(GlobalValue::InternalLinkage);  TODO (can cause LLVM to rename function)
    //Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, funcName.c_str(), &context.llvmModule);
    // note: function is of LLVM function pointer type (since it is an LLVM global value)
    return function;
}

Value* TxLambdaExprNode::code_gen( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN(this, context, " function body");
    Function* function = this->code_gen_forward_decl( context, scope );
    ASSERT(function, "NULL function pointer in " << this);

    // FUTURE: if this is a lambda within a code-block, define the implicit closure object here

    StructType *lambdaT = cast<StructType>(context.get_llvm_type(this->funcTypeNode->get_type()));

    // generate the function body:
    BasicBlock *entryBlock = BasicBlock::Create(context.llvmContext, "entry", function);
    IRBuilder<> builder( entryBlock );
    GenScope fscope(&builder);

    // name the concrete args (and self, if present) and allocate them on the stack:
    Function::arg_iterator fArgI = function->arg_begin();
    if (this->is_instance_method()) {
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

    if (! this->funcTypeNode->returnField && ! fscope.builder->GetInsertBlock()->getTerminator()) {
        LOG_DEBUG(context.LOGGER(), "inserting default void return instruction for last block of function " << function->getName().str());
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
    TRACE_CODEGEN(this, context);
    if (this->field->typeExpression)
        this->field->typeExpression->code_gen(context, scope);
    auto declaration = this->field->get_declaration();
    auto uniqueName = declaration->get_unique_full_name();
    ASSERT (declaration->get_storage() == TXS_STACK, "TxFieldStmtNode can only apply to TX_STACK storage fields: " << uniqueName);
    auto txType = this->field->get_type();

    // If init expression does a stack allocation of this field's type (instance-equivalent type),
    // this field shall bind to that allocation.
    // (If no explicit type is specified, the init expression will of course have the same type.)

    Value* fieldVal; // = txType->gen_alloca(context, scope, declaration->get_symbol()->get_name());
    if (this->field->initExpression) {
        if (this->field->initExpression->is_stack_allocation_expression()
            && (!this->field->typeExpression
                || this->field->initExpression->get_type()->is_assignable_to(*txType))) {
            fieldVal = this->field->initExpression->code_gen(context, scope);
        }
        else {
            fieldVal = txType->gen_alloca(context, scope, declaration->get_symbol()->get_name());
            // create implicit assignment statement
            if (Value* initializer = this->field->initExpression->code_gen(context, scope))
                do_store(context, scope, fieldVal, initializer);
        }
    }
    else {
        fieldVal = txType->gen_alloca(context, scope, declaration->get_symbol()->get_name());
        // We don't automatically invoke default constructor (in future, a code flow validator should check that initialized before first use)
    }
    context.register_llvm_value(uniqueName, fieldVal);
    return fieldVal;
}

Value* TxTypeStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    return this->typeDecl->code_gen(context, scope);
}


Value* TxAssignStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    auto rval = this->rvalue->code_gen(context, scope);
    auto lval = this->lvalue->code_gen(context, scope);
    if ((! lval) || (! rval))
        return NULL;
    if (! lval->getType()->isPointerTy()) {
        LOG(context.LOGGER(), ERROR, "At " << this->parse_loc_string() << ": L-value is not of pointer type:\n" << ::to_string(lval));
        return rval;
    }
    return do_store(context, scope, lval, rval);
}


Value* TxAssertStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    if (context.tuplexPackage.driver().get_options().suppress_asserts)
        return nullptr;
    else
        return this->ifStmt->code_gen(context, scope);
}



Value* TxSuiteNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);

//    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
//    BasicBlock* suiteBlock = BasicBlock::Create(context.llvmContext,  "suite", parentFunc);
//    scope->builder->SetInsertPoint(suiteBlock);
    for (auto stmt : *this->suite)
        stmt->code_gen(context, scope);
//    scope->builder->SetInsertPoint(continuationBlock);

    return NULL;
}

Value* TxElseClauseNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    return this->body->code_gen(context, scope);
}

Value* TxIfStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);

    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
    BasicBlock* trueBlock = BasicBlock::Create(context.llvmContext, "if_true", parentFunc);
    BasicBlock* elseBlock = BasicBlock::Create(context.llvmContext, "if_else", parentFunc);
    BasicBlock* nextBlock = nullptr;

    // generate condition:
    auto condVal = this->cond->code_gen(context, scope);
    auto condInstr = scope->builder->CreateCondBr(condVal, trueBlock, elseBlock);

    // generate else code:
    if (this->elseClause) {
        scope->builder->SetInsertPoint(elseBlock);
        this->elseClause->code_gen(context, scope);

        if (! this->elseClause->ends_with_terminal_stmt()) {
            nextBlock = BasicBlock::Create(context.llvmContext, "if_next", parentFunc);
            scope->builder->CreateBr(nextBlock);  // branch from end of else suite to next-block
        }
    }
    else
        nextBlock = elseBlock;

    // generate true code:
    scope->builder->SetInsertPoint(trueBlock);
    this->body->code_gen(context, scope);
    if (nextBlock) {
        // note: trueBlock may not be the "current" block anymore when reaching end of body
        if (! scope->builder->GetInsertBlock()->getTerminator())
            scope->builder->CreateBr(nextBlock);  // branch from end of true block to next-block
        scope->builder->SetInsertPoint(nextBlock);
    }

    return condInstr;
}

Value* TxWhileStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);

    auto parentFunc = scope->builder->GetInsertBlock()->getParent();
    BasicBlock* condBlock = BasicBlock::Create(context.llvmContext, "while_cond", parentFunc);
    BasicBlock* loopBlock = BasicBlock::Create(context.llvmContext, "while_loop", parentFunc);
    BasicBlock* elseBlock = BasicBlock::Create(context.llvmContext, "while_else", parentFunc);
    BasicBlock* nextBlock = nullptr;

    // generate condition block:
    scope->builder->CreateBr(condBlock);  // branch from end of preceding block to condition-block
    scope->builder->SetInsertPoint(condBlock);
    auto condVal = this->cond->code_gen(context, scope);
    auto condInstr = scope->builder->CreateCondBr(condVal, loopBlock, elseBlock);

    // generate else code:
    if (this->elseClause) {
        scope->builder->SetInsertPoint(elseBlock);
        this->elseClause->code_gen(context, scope);

        if (! this->ends_with_terminal_stmt()) {
            nextBlock = BasicBlock::Create(context.llvmContext, "while_next", parentFunc);
            scope->builder->CreateBr(nextBlock);  // branch from end of else suite to next-block
        }
    }
    else
        nextBlock = elseBlock;

    // generate true code:
    CompoundStatementScope css(condBlock, nextBlock);
    scope->compStmtStack.push(&css);
    scope->builder->SetInsertPoint(loopBlock);
    this->body->code_gen(context, scope);
    scope->compStmtStack.pop();
    // note: loopBlock is may not be the "current" block anymore when reaching end of loop body
    if (! scope->builder->GetInsertBlock()->getTerminator())
        scope->builder->CreateBr(condBlock);  // branch from end of loop body to condition-block

    if (nextBlock)
        scope->builder->SetInsertPoint(nextBlock);

    return condInstr;
}

Value* TxCallStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    return this->call->code_gen(context, scope);
}

Value* TxReturnStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    if (this->expr) {
        auto exprV = this->expr->code_gen(context, scope);
        if (! exprV)
            return nullptr;
        // TODO: this is hackish, can we find systematic solution?
        auto expectedT = context.get_llvm_type(this->expr->get_type());
        if (exprV->getType() == expectedT)
            return scope->builder->CreateRet(exprV);
        else if (exprV->getType()->isPointerTy() && exprV->getType()->getPointerElementType() == expectedT) {
            LOG_DEBUG(context.LOGGER(), "auto-loading return value type " << ::to_string(exprV->getType()) << "  to expected  " << ::to_string(expectedT));
            return scope->builder->CreateRet(scope->builder->CreateLoad(exprV));
        }
        else {
            LOG(context.LOGGER(), ERROR, "Mismatching return value type: " << ::to_string(exprV->getType()) << " is not as expected " << ::to_string(expectedT));
            return scope->builder->CreateRet(exprV);
        }
    }
    else
        return scope->builder->CreateRetVoid();
}

Value* TxBreakStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    return scope->builder->CreateBr(scope->compStmtStack.top()->breakBlock);
}

Value* TxContinueStmtNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    return scope->builder->CreateBr(scope->compStmtStack.top()->continueBlock);
}
