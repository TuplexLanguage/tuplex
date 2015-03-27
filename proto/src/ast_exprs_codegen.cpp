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
    { TXOP_AND,   Instruction::And,  Instruction::And,  0 },
    { TXOP_OR,    Instruction::Or,   Instruction::Or,   0 },
};

Value* TxBinaryOperatorNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto lval = this->lhs->code_gen(context, scope);
    auto rval = this->rhs->code_gen(context, scope);
    //Value* result;
    if ((! lval) || (! rval))
        return NULL;

    // pick field's plain name, if available, for the expression value:
    const std::string fieldName = this->fieldDefNode ? this->fieldDefNode->fieldName : "";

    auto op_class = get_op_class(this->op);

    unsigned llvm_op;
    bool int_operands;
    auto operandsType = this->get_type();
    if (auto intType = dynamic_cast<const TxIntegerType*>(operandsType)) {
        ASSERT(op_class != TXOC_BOOLEAN, "Can't perform BOOLEAN operation on integer operands: " << this);
        llvm_op = intType->sign ? OP_MAPPING[this->op].l_si_op : OP_MAPPING[this->op].l_ui_op;
        int_operands = true;
    }
    else if (dynamic_cast<const TxFloatingType*>(operandsType)) {
        ASSERT(op_class != TXOC_BOOLEAN, "Can't perform BOOLEAN operation on floatingpoint operands: " << this);
        llvm_op = OP_MAPPING[this->op].l_f_op;
        int_operands = false;
    }
    else if (dynamic_cast<const TxBoolType*>(operandsType)) {
        ASSERT(op_class != TXOC_ARITHMETIC, "Can't perform ARITHMETIC operation on boolean operands: " << this);
        llvm_op = OP_MAPPING[this->op].l_ui_op;  // as unsigned integers
        int_operands = true;
    }
    else {
        context.LOG.error("%s: Unsupported binary operand type: %s", this->parse_loc_string().c_str(), (operandsType?operandsType->to_string().c_str():"NULL"));
        return NULL;
    }

    if (op_class == TXOC_ARITHMETIC || op_class == TXOC_BOOLEAN) {
        ASSERT(Instruction::isBinaryOp(llvm_op), "Not a valid LLVM binary op: " << llvm_op);
        Instruction::BinaryOps binop_instr = (Instruction::BinaryOps) llvm_op;
        if (this->is_statically_constant() && !scope)  // seems we can only do this in global scope?
            return ConstantExpr::get(binop_instr, cast<Constant>(lval), cast<Constant>(rval));
        else {
            ASSERT(scope, "scope is NULL, although expression is not constant and thus should be within runtime block");
            return scope->builder->CreateBinOp(binop_instr, lval, rval, fieldName);
            //return BinaryOperator::Create(instr, lval, rval, fieldName, scope->builder->GetInsertBlock());
        }
    }
    else { // if (op_class == TXOC_EQUALITY || op_class == TXOC_COMPARISON) {
        CmpInst::Predicate cmp_pred = (CmpInst::Predicate) llvm_op;
        if (this->is_statically_constant() && !scope)  // seems we can only do this in global scope?
            return ConstantExpr::getCompare(cmp_pred, cast<Constant>(lval), cast<Constant>(rval));
        else {
            ASSERT(scope, "scope is NULL, although expression is not constant and thus should be within runtime block");
            if (int_operands) {
                ASSERT(CmpInst::isIntPredicate(cmp_pred), "Not a valid LLVM Int comparison predicate: " << llvm_op);
                return scope->builder->CreateICmp(cmp_pred, lval, rval, fieldName);
            }
            else {
                ASSERT(CmpInst::isFPPredicate(cmp_pred), "Not a valid LLVM FP comparison predicate: " << llvm_op);
                return scope->builder->CreateFCmp(cmp_pred, lval, rval, fieldName);
            }
        }
    }
}


Value* TxUnaryMinusNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto operand = this->operand->code_gen(context, scope);
    if (! operand)
        return NULL;
    auto opType = this->get_type();
    if (dynamic_cast<const TxIntegerType*>(opType)) {
        if (this->is_statically_constant() && !scope)
            return ConstantExpr::getNeg(cast<Constant>(operand));
        else
            return scope->builder->CreateNeg(operand);
    }
    else if (dynamic_cast<const TxFloatingType*>(opType)) {
        if (this->is_statically_constant() && !scope)
            return ConstantExpr::getFNeg(cast<Constant>(operand));
        else
            return scope->builder->CreateFNeg(operand);
    }
    else {
        context.LOG.error("Invalid unary minus operand type: %s", opType->to_string().c_str());
        return NULL;
    }
}

Value* TxUnaryLogicalNotNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto operand = this->operand->code_gen(context, scope);
    if (! operand)
        return NULL;
    if (this->is_statically_constant() && !scope)
        return ConstantExpr::getNot(cast<Constant>(operand));
    else
        return scope->builder->CreateNot(operand);
}


Value* TxReferenceToNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    if (auto fieldNode = dynamic_cast<TxFieldValueNode*>(this->target)) {
        return fieldNode->code_gen_address(context, scope);
    }
    else if (auto elemNode = dynamic_cast<TxElemDerefNode*>(this->target)) {
        return elemNode->code_gen_address(context, scope);
    }
    else if (this->target->is_statically_constant()) {
        // experimental, automatically allocates space for literals, used for e.g. string literals
        auto targetVal = target->code_gen(context, scope);
        if (auto c = dyn_cast<Constant>(targetVal))
            return new GlobalVariable(context.llvmModule, c->getType(), true, GlobalValue::InternalLinkage, c, "");
        else
            context.LOG.error("Target expression supposed to be statically constant but isn't: %s", ::to_string(targetVal).c_str());
    }
    ASSERT(false, "Can't construct reference to expression of type: " << *this->target);
}

Value* TxReferenceDerefNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto refval = this->reference->code_gen(context, scope);
    if (! refval)
        return NULL;
    ASSERT(refval->getType()->isPointerTy(), this->to_string() << ": expected reference-operand to be a pointer: " << refval);
    auto elemType = refval->getType()->getPointerElementType();
    //std::cout << "Line " << this->parseLocation.first_line << ": Dereferencing: " << refval << " of pointer element type: "<< elemType << std::endl;
    if (elemType->isSingleValueType()) {  // can be loaded in register
        if (scope)
            return scope->builder->CreateLoad(refval);
        else
            return new LoadInst(refval);
    }
    else {
        // handled as pointers in LLVM  // context.LOG.warning("De-referencing reference to non-single-value type not yet fully supported: %s", ::to_string(elemType).c_str());
        return refval;
    }
}

llvm::Value* TxReferenceDerefNode::code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const {
    // FIXME: make dynamic by reading the reference's target type id
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context.llvmContext), this->get_type()->get_type_id());
}



Value* TxElemDerefNode::code_gen_address(LlvmGenerationContext& context, GenScope* scope) const {
    auto arrayval = this->array->code_gen(context, scope);
    auto subscriptval = this->subscript->code_gen(context, scope);
    if (! arrayval || ! subscriptval)
        return NULL;
    ASSERT(subscriptval->getType()->isIntegerTy(), "expected subscript to be an integer: " << subscriptval);
    ASSERT(arrayval->getType()->isPointerTy(), "expected array-operand to be a pointer: " << arrayval);
    ASSERT(arrayval->getType()->getPointerElementType()->isStructTy(), "expected array-operand to be a pointer to struct: " << arrayval);

    if (dyn_cast<Constant>(arrayval) && (dyn_cast<Constant>(subscriptval))) {
        // TODO: constant expression, static bounds check sufficient
    }
    else {
        // TODO: Inject code for bounds checking, and support negative indexing from array end
    }

    Value* ixs[] = { ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0),
                     ConstantInt::get(Type::getInt32Ty(context.llvmContext), 1),
                     subscriptval };
    Value* elemPtr;
    if (this->is_statically_constant() && !scope) {  // seems we can only do this in global scope?
        elemPtr = GetElementPtrInst::CreateInBounds(arrayval, ixs);
    }
    else {
        ASSERT(scope, "scope is NULL, although expression is not constant and thus should be within runtime block");
        elemPtr = scope->builder->CreateInBoundsGEP(arrayval, ixs);
    }
    return elemPtr;
}
Value* TxElemDerefNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    Value* elemPtr = this->code_gen_address(context, scope);
    auto elemType = elemPtr->getType()->getPointerElementType();
    //std::cout << "Line " << this->parseLocation.first_line << ": Dereferencing array element: " << elemPtr << " of pointer element type: "<< elemType << std::endl;
    if (elemType->isSingleValueType()) {  // can be loaded in register
        if (scope)
            return scope->builder->CreateLoad(elemPtr);
        else
            return new LoadInst(elemPtr);
    }
    else {
        // handled as pointers in LLVM  context.LOG.warning("De-referencing array with non-single-value element type not yet fully supported: %s", ::to_string(elemType).c_str());
        return elemPtr;
    }
}

Value* TxElemAssigneeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto arrayval = this->array->code_gen(context, scope);
    auto subscriptval = this->subscript->code_gen(context, scope);
    if (! arrayval || ! subscriptval)
        return NULL;
    ASSERT(subscriptval->getType()->isIntegerTy(), "expected subscript to be an integer: " << subscriptval);
    ASSERT(arrayval->getType()->isPointerTy(), "expected operand to be a pointer: " << arrayval);
    ASSERT(arrayval->getType()->getPointerElementType()->isStructTy(), "expected array-operand to be a pointer to struct: " << arrayval);
    // TODO: merge code with TxElemDerefNode::code_gen_address

    Value* ixs[] = { ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0),
                     ConstantInt::get(Type::getInt32Ty(context.llvmContext), 1),
                     subscriptval };
    return scope->builder->CreateInBoundsGEP(arrayval, ixs);
}



Value* TxBoolConvNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s -> %s", this->to_string().c_str(), this->targetType->to_string().c_str());
    auto origValue = this->expr->code_gen(context, scope);
    if (! origValue)
        return NULL;
    auto targetLlvmType = Type::getInt1Ty(context.llvmContext);
    // current implementation accepts most (all?) scalar types and converts to bool: 0 => FALSE, otherwise => TRUE
    Instruction::CastOps cop = CastInst::getCastOpcode(origValue, false, targetLlvmType, false);
    ASSERT(cop, "No CastOps code found for cast from " << this->expr->get_type() << " to " << this->targetType);
    if (!scope) {
        ASSERT(this->is_statically_constant(), "Non-statically-constant expression in global scope: " << this);
        context.LOG.debug("non-local scope cast -> %s", this->targetType->to_string().c_str());
        return ConstantExpr::getCast(cop, cast<Constant>(origValue), targetLlvmType);
    }
    else {
        context.LOG.debug("local scope cast -> %s", this->targetType->to_string().c_str());
        return scope->builder->CreateCast(cop, origValue, targetLlvmType, "");
    }
}

Value* TxScalarConvNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s -> %s", this->to_string().c_str(), this->targetType->to_string().c_str());
    auto origValue = this->expr->code_gen(context, scope);
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
    Instruction::CastOps cop = CastInst::getCastOpcode(origValue, srcSigned, targetLlvmType, dstSigned);
    ASSERT(cop, "No CastOps code found for cast from " << this->expr->get_type() << " to " << this->targetType);
    if (!scope) {
        ASSERT(this->is_statically_constant(), "Non-statically-constant expression in global scope: " << this);
        context.LOG.debug("non-local scope cast -> %s", this->targetType->to_string().c_str());
        return ConstantExpr::getCast(cop, cast<Constant>(origValue), targetLlvmType);
    }
    else {
        context.LOG.debug("local scope cast -> %s", this->targetType->to_string().c_str());
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


Constant* TxScalarConvNode::ScalarConvConstantProxy::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s -> %s", typeid(*this).name(), this->convNode->targetType->to_string().c_str());
    auto value = this->convNode->code_gen(context, scope);
    if (auto constant = dyn_cast<Constant>(value))
        return constant;
    context.LOG.error("%s: 'constant' scalar conversion did not generate a constant value: %s", this->convNode->parse_loc_string().c_str(), ::to_string(value).c_str());
    return nullptr;
}


Value* TxReferenceConvNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s -> %s", this->to_string().c_str(), this->targetType->to_string().c_str());
    auto origValue = this->expr->code_gen(context, scope);
    if (! origValue)
        return NULL;

    // from another reference:
    if (dynamic_cast<const TxReferenceType*>(this->expr->get_type())) {
        // bitcast from one pointer type to another
        auto targetLlvmType = context.get_llvm_type(this->targetType);
        if (! targetLlvmType) {
            context.LOG.error("In reference conversion, no target LLVM type found for %s", this->targetType->to_string().c_str());
            return origValue;  // should we return null instead?
        }
        if (this->is_statically_constant() && !scope)
            return ConstantExpr::getBitCast(cast<Constant>(origValue), targetLlvmType);
        else
            return scope->builder->CreateBitCast(origValue, targetLlvmType);
    }
//    // from array:
//    else if (dynamic_cast<const TxArrayType*>(this->expr->get_type())) {
//        Value* ixs[] = { ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0),
//                         ConstantInt::get(Type::getInt32Ty(context.llvmContext), 1),
//                         ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0) };
//        if (this->is_statically_constant() && !scope) {  // seems we can only do this in global scope?
//            context.LOG.debug("constant cast -> %s", this->targetType->to_string().c_str());
//            return GetElementPtrInst::CreateInBounds(origValue, ixs);
//        }
//        else {
//            ASSERT(scope, "scope is NULL, although expression is not constant and thus should be within runtime block");
//            context.LOG.debug("non-constant cast -> %s", this->targetType->to_string().c_str());
//            return scope->builder->CreateInBoundsGEP(origValue, ixs);
//        }
//    }
    ASSERT(this->expr->get_type(), "NULL type in " << this);
    context.LOG.error("%s to-reference conversion not supported", this->expr->get_type()->to_string().c_str());
    return origValue;
}


Value* TxObjSpecCastNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s -> %s", this->to_string().c_str(), this->targetType->to_string().c_str());
    // this is a semantic conversion; it doesn't actually do anything
    return this->expr->code_gen(context, scope);
}



Value* TxFunctionCallNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    if (this->inlinedExpression) {
        return this->inlinedExpression->code_gen(context, scope);
    }

    auto calleeVal = this->callee->code_gen(context, scope);
    //std::cout << "callee: " << calleeVal << std::endl;
    Function *function = cast<Function>(calleeVal);
    //  Function *function = context.llvmModule->getFunction(id.name.c_str());
    if (! function) {
        context.LOG.error("no such function at %s", this->to_string().c_str());
        return nullptr;
    }

    std::vector<Value*> args;
    for (auto argDef : *this->argsExprList) {
        args.push_back(argDef->code_gen(context, scope));
    }

    context.LOG.debug("Creating function call %s", function->getName().str().c_str());
    // pick field's plain name, if available, for the expression value:
    const std::string fieldName = this->fieldDefNode ? this->fieldDefNode->fieldName : "";
    // FIXME: if not in function body, scope may be NULL
    return scope->builder->CreateCall(function, args, fieldName);
}
