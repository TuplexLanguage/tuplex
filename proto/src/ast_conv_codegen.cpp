#include "ast_conv.hpp"
#include "llvm_generator.hpp"


using namespace llvm;


Value* TxMaybeConversionNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    return this->get_spec_expression()->code_gen(context, scope);
}


Value* TxBoolConvNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s -> %s", this->str().c_str(), this->resultType->str().c_str());
    auto origValue = this->expr->code_gen(context, scope);
    if (! origValue)
        return NULL;
    //std::cerr << "origValue: " << origValue << " type: " << origValue->getType() << std::endl;
    //std::cerr << "targType: " << Type::getInt1Ty(context.llvmContext) << std::endl;
    // accepts scalar types and converts to bool: 0 => FALSE, otherwise => TRUE
    // Note: can't cast, since that will simply truncate to the lowest source bit
    //Instruction::CastOps cop = CastInst::getCastOpcode(origValue, false, targetLlvmType, false);
    //ASSERT(cop, "No CastOps code found for cast from " << this->expr->get_type(0) << " to " << this->resultType);
    if (!scope) {
        ASSERT(this->is_statically_constant(), "Non-statically-constant expression in global scope: " << this);
        context.LOG.debug("non-local scope cast -> %s", this->resultType->str().c_str());
        if (origValue->getType()->isIntegerTy())
            return ConstantExpr::getICmp(ICmpInst::ICMP_NE, cast<Constant>(origValue), ConstantInt::get(origValue->getType(), 0));
        ASSERT(origValue->getType()->isFloatingPointTy(), "Expected floating point type but was: " << origValue->getType());
        return ConstantExpr::getFCmp(FCmpInst::FCMP_UNE, cast<Constant>(origValue), ConstantFP::get(origValue->getType(), 0));
    }
    else {
        context.LOG.debug("local scope cast -> %s", this->resultType->str().c_str());
        if (origValue->getType()->isIntegerTy())
            return scope->builder->CreateICmpNE(origValue, ConstantInt::get(origValue->getType(), 0));
        ASSERT(origValue->getType()->isFloatingPointTy(), "Expected floating point type but was: " << origValue->getType());
        return ConstantExpr::getFCmp(FCmpInst::FCMP_UNE, cast<Constant>(origValue), ConstantFP::get(origValue->getType(), 0));
    }
}

Value* TxScalarConvNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s -> %s", this->str().c_str(), this->resultType->str().c_str());
    auto origValue = this->expr->code_gen(context, scope);
    if (! origValue)
        return NULL;
    auto targetLlvmType = context.get_llvm_type(this->resultType);
    if (! targetLlvmType) {
        context.LOG.error("In scalar cast, no target LLVM type found for %s", this->resultType->str().c_str());
        return origValue;  // should we return null instead?
    }
    // FUTURE: manually determine cast instruction
    bool srcSigned = false, dstSigned = false;
    if (auto intType = dynamic_cast<const TxIntegerType*>(this->expr->get_type()))
        if (intType->sign)
            srcSigned = true;
    if (auto intType = dynamic_cast<const TxIntegerType*>(this->resultType))
        if (intType->sign)
            dstSigned = true;
    Instruction::CastOps cop = CastInst::getCastOpcode(origValue, srcSigned, targetLlvmType, dstSigned);
    ASSERT(cop, "No CastOps code found for cast from " << this->expr->get_type() << " to " << this->resultType);
    if (!scope) {
        ASSERT(this->is_statically_constant(), "Non-statically-constant expression in global scope: " << this);
        context.LOG.debug("non-local scope cast -> %s", this->resultType->str().c_str());
        return ConstantExpr::getCast(cop, cast<Constant>(origValue), targetLlvmType);
    }
    else {
        context.LOG.debug("local scope cast -> %s", this->resultType->str().c_str());
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
    context.LOG.trace("%-48s -> %s", typeid(*this).name(), this->convNode->resultType->str().c_str());
    auto value = this->convNode->code_gen(context, scope);
    if (auto constant = dyn_cast<Constant>(value))
        return constant;
    context.LOG.error("%s: 'constant' scalar conversion did not generate a constant value: %s", this->convNode->parse_loc_string().c_str(), ::to_string(value).c_str());
    return nullptr;
}


Value* TxReferenceConvNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s -> %s", this->str().c_str(), this->resultType->str().c_str());
    auto origValue = this->expr->code_gen(context, scope);
    if (! origValue)
        return NULL;

    // from another reference:
    if (dynamic_cast<const TxReferenceType*>(this->expr->get_type())) {
        auto refT = context.get_llvm_type(this->resultType);
        if (! refT) {
            context.LOG.error("In reference conversion, LLVM type not found for result type %s", this->resultType->str().c_str());
            return origValue;  // should we return null instead?
        }
        uint32_t adapterTypeId = (this->adapterType ? this->adapterType->get_type_id() : UINT32_MAX);
        //std::cerr << "Ref conversion\n from " << this->expr->get_type(0) << "\n   to " << this->resultType << " = " << refT
        //          << "\n adapterTypeId=" << adapterTypeId << std::endl;
        return TxReferenceType::gen_ref_conversion(context, scope, origValue, refT, adapterTypeId);
    }
//    // from array:
//    else if (dynamic_cast<const TxArrayType*>(this->expr->get_type(0))) {
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
    context.LOG.error("%s to-reference conversion not supported", this->expr->get_type()->str().c_str());
    return origValue;
}


Value* TxObjSpecCastNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s -> %s", this->str().c_str(), this->resultType->str().c_str());
    // this is a semantic conversion; it doesn't actually do anything
    return this->expr->code_gen(context, scope);
}
