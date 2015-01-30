#include "ast_exprs.hpp"
#include "llvm_generator.hpp"


using namespace llvm;

typedef struct {
    TxOperation op;
    unsigned l_si_op;
    unsigned l_ui_op;
    unsigned l_f_op;
} OpMapping;

static const OpMapping OP_MAPPING[] = {
    { TXOP_NONE,  0, 0, 0 },
    { TXOP_PLUS,  Instruction::Add,  Instruction::Add,  Instruction::FAdd },
    { TXOP_MINUS, Instruction::Sub,  Instruction::Sub,  Instruction::FSub },
    { TXOP_MUL,   Instruction::Mul,  Instruction::Mul,  Instruction::FMul },
    { TXOP_DIV,   Instruction::SDiv, Instruction::UDiv, Instruction::FDiv },
    { TXOP_EQ,    CmpInst::Predicate::ICMP_EQ,  CmpInst::Predicate::ICMP_EQ,  CmpInst::Predicate::FCMP_OEQ },
    { TXOP_NE,    CmpInst::Predicate::ICMP_NE,  CmpInst::Predicate::ICMP_NE,  CmpInst::Predicate::FCMP_ONE },
    { TXOP_GT,    CmpInst::Predicate::ICMP_SGT, CmpInst::Predicate::ICMP_UGT, CmpInst::Predicate::FCMP_OGT },
    { TXOP_GE,    CmpInst::Predicate::ICMP_SGE, CmpInst::Predicate::ICMP_UGE, CmpInst::Predicate::FCMP_OGE },
    { TXOP_LT,    CmpInst::Predicate::ICMP_SLT, CmpInst::Predicate::ICMP_ULT, CmpInst::Predicate::FCMP_OLT },
    { TXOP_LE,    CmpInst::Predicate::ICMP_SLE, CmpInst::Predicate::ICMP_ULE, CmpInst::Predicate::FCMP_OLE },
};

llvm::Value* TxBinaryOperatorNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto lval = this->lhs->codeGen(context, scope);
    auto rval = this->rhs->codeGen(context, scope);
    //llvm::Value* result;
    if ((! lval) || (! rval))
        return NULL;

    // pick field's plain name, if available, for the expression value:
    const std::string fieldName = this->fieldDefNode ? this->fieldDefNode->ident : "";

    unsigned llvm_op;
    bool int_operands;
    auto operandsType = this->lhs->get_type();
    if (auto intType = dynamic_cast<const TxIntegerType*>(operandsType)) {
        llvm_op = intType->sign ? OP_MAPPING[this->op].l_si_op : OP_MAPPING[this->op].l_ui_op;
        int_operands = true;
    }
    else if (dynamic_cast<const TxFloatingType*>(operandsType)) {
        llvm_op = OP_MAPPING[this->op].l_f_op;
        int_operands = false;
    }

    auto op_class = get_op_class(this->op);
    if (op_class == TXOC_ARITHMETIC) {
        ASSERT(llvm::Instruction::isBinaryOp(llvm_op), "Not a valid LLVM binary op: " << llvm_op);
        llvm::Instruction::BinaryOps binop_instr = (llvm::Instruction::BinaryOps) llvm_op;
        if (this->is_statically_constant() && !scope)  // seems we can only do this in global scope?
            return llvm::ConstantExpr::get(binop_instr, (llvm::Constant*)lval, (llvm::Constant*)rval);
        else {
            ASSERT(scope, "scope is NULL, although expression is not constant and thus should be within runtime block");
            return scope->builder->CreateBinOp(binop_instr, lval, rval, fieldName);
            //return llvm::BinaryOperator::Create(instr, lval, rval, fieldName, scope->builder->GetInsertBlock());
        }
    }
    else { // if (op_class == TXOC_EQUALITY || op_class == TXOC_COMPARISON) {
        llvm::CmpInst::Predicate cmp_pred = (llvm::CmpInst::Predicate) llvm_op;
        if (this->is_statically_constant() && !scope)  // seems we can only do this in global scope?
            return llvm::ConstantExpr::getCompare(cmp_pred, (llvm::Constant*)lval, (llvm::Constant*)rval);
        else {
            ASSERT(scope, "scope is NULL, although expression is not constant and thus should be within runtime block");
            if (int_operands) {
                ASSERT(llvm::CmpInst::isIntPredicate(cmp_pred), "Not a valid LLVM Int comparison predicate: " << llvm_op);
                return scope->builder->CreateICmp(cmp_pred, lval, rval, fieldName);
            }
            else {
                ASSERT(llvm::CmpInst::isFPPredicate(cmp_pred), "Not a valid LLVM FP comparison predicate: " << llvm_op);
                return scope->builder->CreateFCmp(cmp_pred, lval, rval, fieldName);
            }
        }
    }
}



llvm::Value* TxUnaryMinusNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto operand = this->operand->codeGen(context, scope);
    if (! operand)
        return NULL;
    auto opType = this->get_type();
    if (dynamic_cast<const TxIntegerType*>(opType)) {
        if (this->is_statically_constant() && !scope)
            return llvm::ConstantExpr::getNeg((llvm::Constant*)operand);
        else
            return scope->builder->CreateNeg(operand);
    }
    else if (dynamic_cast<const TxFloatingType*>(opType)) {
        if (this->is_statically_constant() && !scope)
            return llvm::ConstantExpr::getFNeg((llvm::Constant*)operand);
        else
            return scope->builder->CreateFNeg(operand);
    }
    else {
        context.LOG.error("Invalid unary minus operand type: %s", opType->to_string().c_str());
        return NULL;
    }
}

llvm::Value* TxReferenceToNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    // note similarity with TxIdentifiedFieldNode, except the load
    auto entity = this->get_target_entity();
    llvm::Value* val = context.lookup_llvm_value(entity->get_full_name().to_string());
    if (! val)
        return NULL;
    //std::cout << "Line " << this->parseLocation.first_line << ": ReferenceTo: " << val << "  op type: "<< val->getType() << std::endl;
    return val;
}

llvm::Value* TxReferenceDerefNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto refval = this->reference->codeGen(context, scope);
    if (! refval)
        return NULL;
    ASSERT(refval->getType()->isPointerTy(), "expected reference-operand to be a pointer: " << refval);
    auto elemType = refval->getType()->getPointerElementType();
    //std::cout << "Line " << this->parseLocation.first_line << ": Dereferencing: " << refval << " of pointer element type: "<< elemType << std::endl;
    if (elemType->isSingleValueType()) {  // can be loaded in register
        if (scope)
            return scope->builder->CreateLoad(refval);
        else
            return new llvm::LoadInst(refval);
    }
    else {
        // handled as pointers in LLVM  // context.LOG.warning("De-referencing reference to non-single-value type not yet fully supported: %s", ::to_string(elemType).c_str());
        return refval;
    }
}


llvm::Value* TxElemDerefNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto arrayval = this->array->codeGen(context, scope);
    auto subval = this->subscript->codeGen(context, scope);
    if (! arrayval || ! subval)
        return NULL;
    ASSERT(arrayval->getType()->isPointerTy(), "expected array-operand to be a pointer: " << arrayval);
    ASSERT(subval->getType()->isIntegerTy(), "expected subscript to be an integer: " << subval);
    // TODO: When array objects get a size member, inject subscript range checking,
    // and support negative indexing from array end.

    llvm::Value* ixs[] = { llvm::ConstantInt::get(llvm::Type::getInt32Ty(context.llvmContext), 0),
                           subval };
    llvm::Value* elemPtr;
    if (this->is_statically_constant() && !scope) {  // seems we can only do this in global scope?
        elemPtr = llvm::GetElementPtrInst::CreateInBounds(arrayval, ixs);
    }
    else {
        ASSERT(scope, "scope is NULL, although expression is not constant and thus should be within runtime block");
        elemPtr = scope->builder->CreateInBoundsGEP(arrayval, ixs);
    }

    auto elemType = elemPtr->getType()->getPointerElementType();
    //std::cout << "Line " << this->parseLocation.first_line << ": Dereferencing array element: " << elemPtr << " of pointer element type: "<< elemType << std::endl;
    if (elemType->isSingleValueType()) {  // can be loaded in register
        if (scope)
            return scope->builder->CreateLoad(elemPtr);
        else
            return new llvm::LoadInst(elemPtr);
    }
    else {
        // handled as pointers in LLVM  context.LOG.warning("De-referencing array with non-single-value element type not yet fully supported: %s", ::to_string(elemType).c_str());
        return elemPtr;
    }
}

llvm::Value* TxElemAssigneeNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto arrayval = this->array->codeGen(context, scope);
    auto subval = this->subscript->codeGen(context, scope);
    if (! arrayval || ! subval)
        return NULL;
    ASSERT(arrayval->getType()->isPointerTy(), "expected operand to be a pointer: " << arrayval);
    ASSERT(subval->getType()->isIntegerTy(), "expected subscript to be an integer: " << subval);
    // TODO: When array objects get a size member, inject subscript range checking,
    // and support negative indexing from array end.

    llvm::Value* ixs[] = { llvm::ConstantInt::get(llvm::Type::getInt32Ty(context.llvmContext), 0),
                           subval };
    llvm::Value* elemPtr;
    elemPtr = scope->builder->CreateInBoundsGEP(arrayval, ixs);
    return elemPtr;
}



llvm::Value* TxCStringLitNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s\t\"%s\"", this->to_string().c_str(), this->value.c_str());
    if (scope)
        return scope->builder->CreateGlobalString(this->value);
    else
        return llvm::ConstantDataArray::getString(context.llvmContext, this->value);
}

llvm::Value* TxCharacterLitNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s\t'%c' == %d", this->to_string().c_str(), this->value, this->value);
    auto value = llvm::ConstantInt::get(context.llvmContext, llvm::APInt(8, this->value, false));
    return value;
}
llvm::Value* TxIntegerLitNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s\t%d", this->to_string().c_str(), this->value);
    llvm::IntegerType* type = static_cast<llvm::IntegerType*>(context.get_llvm_type(this->get_type()));
    ASSERT (type, "Could not get llvm::IntegerType for TxIntegerLitNode " << context.get_llvm_type(this->get_type()));
    auto value = llvm::ConstantInt::get(type, this->literal, 10);
    ASSERT (value->getSExtValue()==this->value, "LLVM's s-ext-int value " << value->getSExtValue() << "!=" << this->value);
    //auto value = llvm::ConstantInt::get(context.llvmContext, llvm::APInt(64, this->value, true));
    return value;
}
llvm::Value* TxFloatingLitNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s\t%f", this->to_string().c_str(), this->value);
    llvm::Type* type = context.get_llvm_type(this->get_type());
    ASSERT (type, "Could not get llvm::Type for TxFloatingLitNode " << context.get_llvm_type(this->get_type()));
    auto value = llvm::ConstantFP::get(type, this->literal);
    //auto value = llvm::ConstantFP::get(type, this->value);
    return value;
}

llvm::Value* TxScalarCastNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s -> %s", this->to_string().c_str(), this->targetType->to_string().c_str());
    auto origValue = this->expr->codeGen(context, scope);
    if (! origValue)
        return NULL;
    auto targetLlvmType = context.get_llvm_type(this->targetType);
    if (! targetLlvmType) {
        context.LOG.error("In scalar cast, no target LLVM type found for %s", this->targetType->to_string().c_str());
        return origValue;  // should we return null instead?
    }
    // FUTURE: manually determine cast instruction
    bool srcSigned = false, dstSigned = false;
    if (auto intType = dynamic_cast<const TxIntegerType*>(this->expr->get_type()))
        if (intType->sign)
            srcSigned = true;
    if (auto intType = dynamic_cast<const TxIntegerType*>(this->targetType))
        if (intType->sign)
            dstSigned = true;
    llvm::Instruction::CastOps cop = llvm::CastInst::getCastOpcode(origValue, srcSigned, targetLlvmType, dstSigned);
    ASSERT(cop, "No CastOps code found for cast from " << this->expr->get_type() << " to " << this->targetType);
    if (this->is_statically_constant() && !scope) {  // seems we can only do this in global scope?
        context.LOG.debug("constant cast -> %s", this->targetType->to_string().c_str());
        return llvm::ConstantExpr::getCast(cop, (llvm::Constant*)origValue, targetLlvmType);
    }
    else {
        ASSERT(scope, "scope is NULL, although expression is not constant and thus should be within runtime block");
        context.LOG.debug("non-constant cast -> %s", this->targetType->to_string().c_str());
        return scope->builder->CreateCast(cop, origValue, targetLlvmType, "");
    }
/* for reference, copied from Instruction.def:
HANDLE_CAST_INST(33, Trunc   , TruncInst   )  // Truncate integers
HANDLE_CAST_INST(34, ZExt    , ZExtInst    )  // Zero extend integers
HANDLE_CAST_INST(35, SExt    , SExtInst    )  // Sign extend integers
HANDLE_CAST_INST(36, FPToUI  , FPToUIInst  )  // floating point -> UInt
HANDLE_CAST_INST(37, FPToSI  , FPToSIInst  )  // floating point -> SInt
HANDLE_CAST_INST(38, UIToFP  , UIToFPInst  )  // UInt -> floating point
HANDLE_CAST_INST(39, SIToFP  , SIToFPInst  )  // SInt -> floating point
HANDLE_CAST_INST(40, FPTrunc , FPTruncInst )  // Truncate floating point
HANDLE_CAST_INST(41, FPExt   , FPExtInst   )  // Extend floating point
HANDLE_CAST_INST(42, PtrToInt, PtrToIntInst)  // Pointer -> Integer
HANDLE_CAST_INST(43, IntToPtr, IntToPtrInst)  // Integer -> Pointer
HANDLE_CAST_INST(44, BitCast , BitCastInst )  // Type cast
HANDLE_CAST_INST(45, AddrSpaceCast, AddrSpaceCastInst)  // addrspace cast
*/
}
llvm::Value* TxToPointerCastNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s -> %s", this->to_string().c_str(), this->targetType->to_string().c_str());
    auto origValue = this->expr->codeGen(context, scope);
    if (! origValue)
        return NULL;

    // from reference:
    if (dynamic_cast<const TxReferenceType*>(this->expr->get_type())) {
        // bitcast from one pointer type to another
        auto targetLlvmType = context.get_llvm_type(this->targetType);
        if (! targetLlvmType) {
            context.LOG.error("In reference-to-pointer cast, no target LLVM type found for %s", this->targetType->to_string().c_str());
            return origValue;  // should we return null instead?
        }
        if (this->is_statically_constant() && !scope)
            return llvm::ConstantExpr::getBitCast(llvm::cast<llvm::Constant>(origValue), targetLlvmType);
        else
            return scope->builder->CreateBitCast(origValue, targetLlvmType);
    }
    // from array:
    else if (dynamic_cast<const TxArrayType*>(this->expr->get_type())) {
        llvm::Value* ixs[] = { llvm::ConstantInt::get(llvm::Type::getInt32Ty(context.llvmContext), 0),
                               llvm::ConstantInt::get(llvm::Type::getInt32Ty(context.llvmContext), 0) };
        if (this->is_statically_constant() && !scope) {  // seems we can only do this in global scope?
            context.LOG.debug("constant cast -> %s", this->targetType->to_string().c_str());
            return llvm::GetElementPtrInst::CreateInBounds(origValue, ixs);
        }
        else {
            ASSERT(scope, "scope is NULL, although expression is not constant and thus should be within runtime block");
            context.LOG.debug("non-constant cast -> %s", this->targetType->to_string().c_str());
            return scope->builder->CreateInBoundsGEP(origValue, ixs);
        }
    }
    context.LOG.error("%s to pointer casts not yet implemented", this->expr->get_type()->to_string().c_str());
    return origValue;
}

llvm::Value* TxObjSpecCastNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s -> %s", this->to_string().c_str(), this->targetType->to_string().c_str());
    // this is a semantic conversion; it doesn't actually do anything
    return this->expr->codeGen(context, scope);
}



llvm::Value* TxLambdaExprNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    llvm::FunctionType *ftype = llvm::cast<llvm::FunctionType>(context.get_llvm_type(this->funcTypeNode->get_type()));
    ASSERT(ftype, "Couldn't get LLVM type for function type " << this->funcTypeNode->get_type());
    std::string funcName = ""; // anonymous function
    if (this->fieldDefNode) {
        //auto entity = context.tuplexContext.lookupField(*this->lexicalScope, *this->fieldIdent);
        //funcName = entity->getQualifiedName().toString();
        funcName = this->fieldDefNode->get_entity()->get_full_name().to_string();
    }
    context.LOG.debug("Creating function: %s", funcName.c_str());
    llvm::Function *function = llvm::cast<llvm::Function>(context.llvmModule.getOrInsertFunction(funcName, ftype));
    // function->setLinkage(llvm::GlobalValue::InternalLinkage);  FIXME (can cause LLVM to rename function)
    //llvm::Function *function = llvm::Function::Create(ftype, llvm::GlobalValue::InternalLinkage,
    //                                                  funcName.c_str(), &context.llvmModule);
    // note: function is of LLVM function pointer type (since it is an LLVM global value)

    // name the concrete args:
    auto argDefI = this->funcTypeNode->arguments->begin();
    for (llvm::Function::arg_iterator fArgI = function->arg_begin();
         fArgI != function->arg_end();  fArgI++, argDefI++) {
        auto entity = (*argDefI)->get_entity();
        fArgI->setName(entity->get_full_name().name());
        context.register_llvm_value(entity->get_full_name().to_string(), fArgI);
    }

    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(context.llvmContext, "entry", function);
    llvm::IRBuilder<> builder( entryBlock );
    GenScope fscope(&builder);

//    // load arg values code gen
//    for (auto argDef : this->funcTypeNode.arguments) {
//        argDef->codeGen(context, fblock);
//    }
    this->suite->codeGen(context, &fscope);

    if (! fscope.builder->GetInsertBlock()->getTerminator()) {
        context.LOG.debug("inserting default void return instruction for last block of function %s", funcName.c_str());
        fscope.builder->CreateRetVoid();
        //llvm::ReturnInst::Create(context.llvmContext, entryBlock);
    }
    ASSERT (entryBlock->getTerminator(), "Function entry block has no terminator");

    return function;
}

llvm::Value* TxFunctionCallNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    if (this->inlinedExpression) {
        return this->inlinedExpression->codeGen(context, scope);
    }

    auto calleeVal = this->callee->codeGen(context, scope);
    //std::cout << "callee: " << calleeVal << std::endl;
    llvm::Function *function = llvm::cast<llvm::Function>(calleeVal);
    //  llvm::Function *function = context.llvmModule->getFunction(id.name.c_str());
    if (! function) {
        context.LOG.error("no such function at %s", this->to_string().c_str());
        return nullptr;
    }

    std::vector<llvm::Value*> args;
    for (auto argDef : *this->argsExprList) {
        args.push_back(argDef->codeGen(context, scope));
    }

    context.LOG.debug("Creating function call %s", function->getName().str().c_str());
    // pick field's plain name, if available, for the expression value:
    const std::string fieldName = this->fieldDefNode ? this->fieldDefNode->ident : "";
    // FIXME: if not in function body, scope may be NULL
    return scope->builder->CreateCall(function, args, fieldName);
}
