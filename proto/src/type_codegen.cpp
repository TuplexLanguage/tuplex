#include "type.hpp"
#include "llvm_generator.hpp"


using namespace llvm;


Type* TxType::get_llvm_type(LlvmGenerationContext& context) const {
    return context.get_llvm_type(this);
}

Value* TxType::code_gen_size(LlvmGenerationContext& context, GenScope* scope) const {
    ASSERT(this->is_concrete(), "Attempted to codegen size of non-concrete type " << this);
    Type* llvmType = this->get_llvm_type(context);
    if (! llvmType)
        return nullptr;
    uint64_t size = context.dataLayout.getTypeAllocSize(llvmType);
    return ConstantInt::get(Type::getInt64Ty(context.llvmContext), size);
}

Value* TxType::code_gen_alloca(LlvmGenerationContext& context, GenScope* scope, const std::string &varName) const {
    ASSERT(this->is_concrete(), "Attempted to codegen size of non-concrete type " << this);
    Type* llvmType = this->get_llvm_type(context);
    if (! llvmType)
        return nullptr;
    return scope->builder->CreateAlloca(llvmType, 0, varName);
}


Value* TxScalarType::code_gen_size(LlvmGenerationContext& context, GenScope* scope) const {
    return ConstantInt::get(Type::getInt64Ty(context.llvmContext), this->size());
}



Value* TxArrayType::code_gen_size(LlvmGenerationContext& context, GenScope* scope) const {
    ASSERT(this->is_concrete(), "Attempted to codegen size of non-concrete type " << this);
    ResolutionContext resCtx;
    Value* elemSize = this->element_type(resCtx)->code_gen_size(context, scope);
    Value* arrayLen = this->length(resCtx)->code_gen(context, scope);
    if (auto ce = dyn_cast<Constant>(elemSize))
        if (auto cl = dyn_cast<Constant>(arrayLen))
            return ConstantExpr::getMul(ce, cl);
    if (!scope) {
        context.LOG.warning("code_gen_size() with NULL scope and non-const expression for %s", this->to_string().c_str());
        return llvm::BinaryOperator::CreateMul(elemSize, arrayLen, "arraysize");
    }
    else
        return scope->builder->CreateMul(elemSize, arrayLen, "arraysize");
}

Value* TxArrayType::code_gen_alloca(LlvmGenerationContext& context, GenScope* scope, const std::string &varName) const {
    // FIXME: emit code for dynamically sized array allocation
    ASSERT(this->is_concrete(), "Attempted to codegen size of non-concrete type " << this);
    Type* llvmType = this->get_llvm_type(context);
    if (! llvmType)
        return nullptr;
    return scope->builder->CreateAlloca(llvmType, 0, varName);
}


