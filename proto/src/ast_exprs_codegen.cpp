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
    const std::string fieldName = this->fieldDefNode ? this->fieldDefNode->get_field_name() : "";

    auto op_class = get_op_class(this->op);

    unsigned llvm_op;
    bool int_operation;
    auto resultType = this->get_type();
    if (auto intType = dynamic_cast<const TxIntegerType*>(resultType)) {
        llvm_op = intType->sign ? OP_MAPPING[this->op].l_si_op : OP_MAPPING[this->op].l_ui_op;
        int_operation = true;
    }
    else if (dynamic_cast<const TxFloatingType*>(resultType)) {
        llvm_op = OP_MAPPING[this->op].l_f_op;
        int_operation = false;
    }
    else if (dynamic_cast<const TxBoolType*>(resultType)) {
        llvm_op = OP_MAPPING[this->op].l_ui_op;  // as unsigned integers
        int_operation = true;
    }
    else {
        context.LOG.error("%s: Unsupported binary operand type: %s", this->parse_loc_string().c_str(), (resultType?resultType->to_string().c_str():"NULL"));
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
            if (int_operation) {
                ASSERT(CmpInst::isIntPredicate(cmp_pred), "Not a valid LLVM Int comparison predicate: " << llvm_op);
                if (this->reference_operands) {
                    lval = gen_get_ref_pointer(context, scope, lval);
                    rval = gen_get_ref_pointer(context, scope, rval);
                }
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



Value* gen_get_struct_member(LlvmGenerationContext& context, GenScope* scope, Value* structV, unsigned ix) {
    Value* memberV;
    if (auto structPtrV = dyn_cast<PointerType>(structV->getType())) {  // address of struct
        ASSERT(structPtrV->getPointerElementType()->isStructTy(), "expected pointer element to be a struct: " << structV);
        if (scope) {
            auto memberA = scope->builder->CreateStructGEP(structV, ix);
            memberV = scope->builder->CreateLoad(memberA);
        }
        else {
            Value *idxs[] = {
              ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0),
              ConstantInt::get(Type::getInt32Ty(context.llvmContext), ix)
            };
            auto memberA = GetElementPtrInst::CreateInBounds(structV, idxs);
            memberV = new LoadInst(memberA);
        }
    }
    else {  // direct / "register" struct
        ASSERT(structV->getType()->isStructTy(), "expected value to be a struct: " << structV);
        memberV = ( scope ? scope->builder->CreateExtractValue(structV, ix)
                          : ExtractValueInst::Create(structV, ix) );
    }
    return memberV;
}



Value* gen_get_ref_pointer(LlvmGenerationContext& context, GenScope* scope, Value* refV) {
    Value* ptrV = gen_get_struct_member(context, scope, refV, 0);
    ASSERT(ptrV->getType()->isPointerTy(), "expected ref.ptr element to be a pointer: " << refV);
    return ptrV;
}

Value* gen_get_ref_typeid(LlvmGenerationContext& context, GenScope* scope, Value* refV) {
    Value* tidV = gen_get_struct_member(context, scope, refV, 1);
    return tidV;
}

Value* gen_ref(LlvmGenerationContext& context, GenScope* scope, Type* refT, Value* ptrV, Value* tidV) {
    if (scope) {
        Value* refV = UndefValue::get(refT);
        auto castPtrV = scope->builder->CreatePointerCast(ptrV, refT->getStructElementType(0));
        refV = scope->builder->CreateInsertValue(refV, castPtrV, 0);
        refV = scope->builder->CreateInsertValue(refV, tidV, 1);
        return refV;
    }
    else {
        ASSERT(false, "Not yet supported to construct reference to global: " << ptrV);  // TODO
    }
}



Value* gen_lambda(LlvmGenerationContext& context, GenScope* scope, Type* lambdaT, Value* funcV, Value* closureRefV) {
    if (scope) {
        Value* lambdaV = UndefValue::get(lambdaT);
        lambdaV = scope->builder->CreateInsertValue(lambdaV, funcV, 0);
        lambdaV = scope->builder->CreateInsertValue(lambdaV, closureRefV, 1);
        return lambdaV;
    }
    else {
        ASSERT(false, "Not yet supported to construct global lambda");  // TODO
    }
}



Value* TxReferenceToNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    Value* ptrV = nullptr;
    if (auto fieldNode = dynamic_cast<TxFieldValueNode*>(this->target)) {
        ptrV = fieldNode->code_gen_address(context, scope);
    }
    else if (auto elemNode = dynamic_cast<TxElemDerefNode*>(this->target)) {
        ptrV = elemNode->code_gen_address(context, scope);
    }
    else if (this->target->is_statically_constant()) {
        // experimental, automatically allocates space for literals, used for e.g. string literals
        auto targetVal = target->code_gen(context, scope);
        if (auto constInitializer = dyn_cast<Constant>(targetVal))
            ptrV = new GlobalVariable(context.llvmModule, constInitializer->getType(), true,
                                         GlobalValue::InternalLinkage, constInitializer);
        else {
            context.LOG.error("Target expression supposed to be statically constant but isn't: %s", ::to_string(targetVal).c_str());
            return nullptr;
        }
    }
    else {
        ASSERT(false, "Can't construct reference to expression of type: " << *this->target);
        return nullptr;
    }

    // the reference gets the statically known target type id
    auto tidV = ConstantInt::get(Type::getInt32Ty(context.llvmContext), this->target->get_type()->get_type_id());

    // box the pointer:
    auto refT = this->get_type()->make_llvm_type(context);
    return gen_ref(context, scope, refT, ptrV, tidV);
}



Value* TxReferenceDerefNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    if (! this->refExprValue) {
        this->refExprValue = this->reference->code_gen(context, scope);
        if (! this->refExprValue)
            return NULL;
    }

    Value* ptrV = gen_get_ref_pointer(context, scope, this->refExprValue);

    auto targT = ptrV->getType()->getPointerElementType();
    //std::cout << "Line " << this->parseLocation.first_line << ": Dereferencing: " << refval << " of pointer element type: "<< elemType << std::endl;
    if (targT->isSingleValueType()) {  // can be loaded in register
        if (scope)
            return scope->builder->CreateLoad(ptrV);
        else
            return new LoadInst(ptrV);
    }
    else {
        // handled as pointers in LLVM  // context.LOG.warning("De-referencing reference to non-single-value type not yet fully supported: %s", ::to_string(elemType).c_str());
        return ptrV;
    }
}

Value* TxReferenceDerefNode::code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const {
    // dynamic by reading the reference's target type id
    context.LOG.trace("%-48s TypeID", this->to_string().c_str());
    if (! this->refExprValue) {
        this->refExprValue = this->reference->code_gen(context, scope);
        if (! this->refExprValue)
            return NULL;
    }

    Value* tidV = gen_get_ref_typeid(context, scope, this->refExprValue);
    return tidV;
}



static Value* gen_elem_address(LlvmGenerationContext& context, GenScope* scope, Value* arrayPtrV, Value* subscriptV) {
    ASSERT(subscriptV->getType()->isIntegerTy(), "expected subscript to be an integer: " << subscriptV);
    ASSERT(arrayPtrV->getType()->isPointerTy(), "expected array-operand to be a pointer: " << arrayPtrV);
    ASSERT(arrayPtrV->getType()->getPointerElementType()->isStructTy(), "expected array-operand to be a pointer to struct: " << arrayPtrV);

    if (dyn_cast<Constant>(arrayPtrV) && (dyn_cast<Constant>(subscriptV))) {
        // TODO: constant expression, static bounds check sufficient
    }
    else {
        // TODO: Inject code for bounds checking, and support negative indexing from array end
    }

    if (auto arrayPtrC = dyn_cast<Constant>(arrayPtrV)) {
        if (auto intC = dyn_cast<ConstantInt>(subscriptV)) {
            Constant* ixs[] = { ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0),
                                ConstantInt::get(Type::getInt32Ty(context.llvmContext), 1),
                                intC };
            return ConstantExpr::getInBoundsGetElementPtr(arrayPtrC, ixs);
        }
    }

    Value* ixs[] = { ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0),
                     ConstantInt::get(Type::getInt32Ty(context.llvmContext), 1),
                     subscriptV };
    if (scope)
        return scope->builder->CreateInBoundsGEP(arrayPtrV, ixs);
    else
        return GetElementPtrInst::CreateInBounds(arrayPtrV, ixs);
}

Value* TxElemDerefNode::code_gen_address(LlvmGenerationContext& context, GenScope* scope) const {
    auto arrayV = this->array->code_gen(context, scope);
    auto subscriptV = this->subscript->code_gen(context, scope);
    if (! arrayV || ! subscriptV)
        return NULL;

    if (! arrayV->getType()->isPointerTy()) {
        // aggregate values must be stored in memory (have address) for getting address of element
        if (scope) {
            auto arrayA = scope->builder->CreateAlloca(arrayV->getType());
            scope->builder->CreateStore(arrayV, arrayA);
            arrayV = arrayA;
        }
        else if (auto arrayC = dyn_cast<Constant>(arrayV)) {
            auto arrayA = new GlobalVariable(context.llvmModule, arrayC->getType(), true, GlobalValue::InternalLinkage,
                                             arrayC, "$array");
            arrayV = arrayA;
        }
        else {
            context.LOG.error("Can't dereference non-constant array from global scope");
            return NULL;
        }
    }

    return gen_elem_address(context, scope, arrayV, subscriptV);
}

Value* TxElemDerefNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto arrayV = this->array->code_gen(context, scope);
    auto subscriptV = this->subscript->code_gen(context, scope);
    if (! arrayV || ! subscriptV)
        return NULL;

    if (! arrayV->getType()->isPointerTy()) {
        if (auto arrayC = dyn_cast<Constant>(arrayV)) {
            if (auto intC = dyn_cast<ConstantInt>(subscriptV)) {
                uint32_t ixs[] = { 1, (uint32_t)intC->getLimitedValue(UINT32_MAX) };
                return ConstantExpr::getExtractValue(arrayC, ixs);
            }
        }

        // aggregate values must be stored in memory (have address) for performing element access
        // when the element index value is not statically known
        if (scope) {
            auto arrayA = scope->builder->CreateAlloca(arrayV->getType());
            scope->builder->CreateStore(arrayV, arrayA);
            arrayV = arrayA;
        }
        else if (auto arrayC = dyn_cast<Constant>(arrayV)) {
            auto arrayA = new GlobalVariable(context.llvmModule, arrayC->getType(), true, GlobalValue::InternalLinkage,
                                             arrayC, "$array");
            arrayV = arrayA;
        }
        else {
            context.LOG.error("Can't dereference non-constant array from global scope");
            return NULL;
        }
    }
    else if (auto arrayPtrG = dyn_cast<GlobalVariable>(arrayV)) {
        // this enables dereferencing (constant) arrays from global scope
        // since we can't use load instructions in global (constant) initializers, access the original initializer directly
        if (arrayPtrG->hasInitializer()) {
            if (auto intC = dyn_cast<ConstantInt>(subscriptV)) {
                uint32_t ixs[] = { 1, (uint32_t)intC->getLimitedValue(UINT32_MAX) };
                return ConstantExpr::getExtractValue(arrayPtrG->getInitializer(), ixs);
            }
        }
    }

    Value* elemPtr = gen_elem_address(context, scope, arrayV, subscriptV);
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


Value* TxFieldAssigneeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    return this->field->code_gen_address(context, scope);
}

Value* TxDerefAssigneeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto refval = this->operand->code_gen(context, scope);
    if (! refval)
        return NULL;
    return gen_get_ref_pointer(context, scope, refval);
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
        auto targetRefT = context.get_llvm_type(this->targetType);
        if (! targetRefT) {
            context.LOG.error("In reference conversion, no target LLVM type found for %s", this->targetType->to_string().c_str());
            return origValue;  // should we return null instead?
        }
        return TxReferenceType::gen_ref_conversion(context, scope, origValue, targetRefT);
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
    return this->gen_call(context, scope);
}

Value* TxFunctionCallNode::gen_call(LlvmGenerationContext& context, GenScope* scope) const {
    auto lambdaV = this->callee->code_gen(context, scope);
    //std::cout << "callee: " << lambdaV << std::endl;
    auto functionPtrV = gen_get_struct_member(context, scope, lambdaV, 0);
    auto closureRefV = gen_get_struct_member(context, scope, lambdaV, 1);
    return this->gen_call(context, scope, functionPtrV, closureRefV);
}

Value* TxFunctionCallNode::gen_call(LlvmGenerationContext& context, GenScope* scope, Value* functionPtrV, Value* closureRefV) const {
    std::vector<Value*> args;
    args.push_back(closureRefV);
    for (auto argDef : *this->argsExprList) {
        args.push_back(argDef->code_gen(context, scope));
    }

    context.LOG.debug("Creating function call '%s'", functionPtrV->getName().str().c_str());
    // pick field's plain name, if available, for the expression value:
    const std::string fieldName = this->fieldDefNode ? this->fieldDefNode->get_field_name() : "";
    if (scope)
        return scope->builder->CreateCall(functionPtrV, args, fieldName);
    else {
        // FUTURE: support calling functions outside of code block (statically constant or instance initialization)
        context.LOG.error("calling functions outside of code block not currently supported");
        return nullptr;
    }
}


Value* TxConstructorCalleeExprNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    // constructors are similar to instance methods, but they are not virtual (and not in vtable)
    context.LOG.trace("%-48s", this->to_string().c_str());
    Value* funcPtrV = context.lookup_llvm_value(this->constructorEntity->get_full_name().to_string());
    return funcPtrV;
    /*
    //std::cout << "funcPtrV: " << funcPtrV << std::endl;
    auto allocType = this->get_object_type();
    if (! allocType)
        return nullptr;
    Constant* instanceTypeIdV = allocType->gen_typeid(context, scope);
    ASSERT(this->objectPtrV->getType()->isPointerTy(), "Expected baseValue to be of pointer type but was: " << this->objectPtrV->getType());
    {   // construct the lambda object:
        auto closureRefT = context.get_voidRefT();
        auto closureRefV = gen_ref(context, scope, closureRefT, this->objectPtrV, instanceTypeIdV);
        auto lambdaT = cast<StructType>(context.get_llvm_type(this->constructorEntity->get_type()));
        return gen_lambda(context, scope, lambdaT, funcPtrV, closureRefV);
    }
    */
}


Value* TxMakeObjectNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    Type* objT = context.get_llvm_type(this->get_object_type());
    Type* objRefT = context.get_llvm_type(this->get_type());
    if (!objT || !objRefT)
        return nullptr;

    auto objAllocI = this->gen_alloc(context, scope, objT);

    Constant* objTypeIdV = this->get_object_type()->gen_typeid(context, scope);
    auto objRefV = gen_ref(context, scope, objRefT, objAllocI, objTypeIdV);

    // initialize the object
    if (this->inlinedInitializer) {
        auto initValue = this->inlinedInitializer->code_gen(context, scope);
        ASSERT(scope, "new expression not supported in global/static scope: " << this->parse_loc_string());
        scope->builder->CreateStore(initValue, objAllocI);
    }
    else {
        Value* funcPtrV = this->constructor->code_gen(context, scope);
        ASSERT(funcPtrV->getType()->getPointerElementType()->isFunctionTy() , "Expected funcPtrV to be pointer-to-function type but was: " << funcPtrV->getType());
        auto closureRefT = context.get_voidRefT();  // avoids 'self' ref casting issue
        auto closureRefV = gen_ref(context, scope, closureRefT, objAllocI, objTypeIdV);
        this->constructorCall->gen_call(context, scope, funcPtrV, closureRefV);
    }

    return objRefV;
}

Value* TxNewExprNode::gen_alloc(LlvmGenerationContext& context, GenScope* scope, Type* objT) const {
    return context.gen_malloc(scope, objT);
}

Value* TxNewExprNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    // new constructor returns the constructed object by reference
    context.LOG.trace("%-48s", this->to_string().c_str());
    this->typeExpr->code_gen(context, scope);
    return TxMakeObjectNode::code_gen(context, scope);
}

Value* TxStackConstructorNode::gen_alloc(LlvmGenerationContext& context, GenScope* scope, Type* objT) const {
    return this->get_object_type()->gen_alloca(context, scope);
}

Value* TxStackConstructorNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    // stack constructor returns the constructed object by value, not by reference
    context.LOG.trace("%-48s", this->to_string().c_str());
    if (this->inlinedInitializer) {
        // if inlined, the stack constructor doesn't need to actually allocate storage on stack
        // (the receiver of this expression value might do this, if it needs to)
        return this->inlinedInitializer->code_gen(context, scope);
    }
    else {
        auto objRefV = TxMakeObjectNode::code_gen(context, scope);
        return gen_get_ref_pointer(context, scope, objRefV);
    }
}

