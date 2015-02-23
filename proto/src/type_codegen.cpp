#include "type.hpp"
#include "llvm_generator.hpp"


using namespace llvm;


/** Returns the input value divided by 4, rounded up. Input must be an integer. */
static Value* code_gen_4_multiple(LlvmGenerationContext& context, GenScope* scope, Value* input) {
    auto two = ConstantInt::get(Type::getInt64Ty(context.llvmContext), 2);
    auto three = ConstantInt::get(Type::getInt64Ty(context.llvmContext), 3);
    if (auto ci = dyn_cast<Constant>(input))
        return ConstantExpr::getAShr(ConstantExpr::getAdd(ci, three), two);
    else if (scope)
        return scope->builder->CreateAShr(scope->builder->CreateAdd(input, three), two);
    else
        return BinaryOperator::CreateAShr(BinaryOperator::CreateAdd(input, three), two);
}



Type* TxType::get_llvm_type(LlvmGenerationContext& context) const {
    return context.get_llvm_type(this);
}

Value* TxType::code_gen_size(LlvmGenerationContext& context, GenScope* scope) const {
    Type* llvmType = this->get_llvm_type(context);
    if (! llvmType)
        return nullptr;
    return ConstantExpr::getSizeOf(llvmType);
}

Value* TxType::code_gen_alloca(LlvmGenerationContext& context, GenScope* scope, const std::string &varName) const {
    ASSERT(this->is_concrete(), "Attempted to codegen size of non-concrete type " << this);
    Type* llvmType = this->get_llvm_type(context);
    if (! llvmType)
        return nullptr;
    return scope->builder->CreateAlloca(llvmType, 0, varName);
}



Value* TxArrayType::code_gen_size(LlvmGenerationContext& context, GenScope* scope) const {
    ResolutionContext resCtx;
    Value* elemSize = this->element_type(resCtx)->code_gen_size(context, scope);
    Value* arrayLen = this->length(resCtx)->code_gen(context, scope);
    return this->inner_code_gen_size(context, scope, elemSize, arrayLen);
}

Value* TxArrayType::inner_code_gen_size(LlvmGenerationContext& context, GenScope* scope, Value* elemSize, Value* arrayLen) const {
    Type* headerType = Type::getInt32Ty(context.llvmContext);
    Constant* headerSize = ConstantExpr::getSizeOf(headerType);
    //std::cout << "Array header size: " << to_string(headerSize) << std::endl;

    if (auto ce = dyn_cast<Constant>(elemSize))
        if (auto cl = dyn_cast<Constant>(arrayLen))
            return ConstantExpr::getAdd(ConstantExpr::getMul(ce, cl), headerSize);
    if (scope) {
        arrayLen = scope->builder->CreateZExtOrBitCast(arrayLen, Type::getInt64Ty(context.llvmContext));
        auto product = scope->builder->CreateMul(elemSize, arrayLen, "arraysize");
        return scope->builder->CreateAdd(product, headerSize, "arrayobjsize");
    }
    else {
        context.LOG.warning("code_gen_size() with NULL scope and non-const expression for %s", this->to_string().c_str());
        arrayLen = CastInst::CreateZExtOrBitCast(arrayLen, Type::getInt64Ty(context.llvmContext));
        auto product = BinaryOperator::CreateMul(elemSize, arrayLen, "arraysize");
        return BinaryOperator::CreateAdd(product, headerSize, "arrayobjsize");
    }
}

Value* TxArrayType::code_gen_alloca(LlvmGenerationContext& context, GenScope* scope, const std::string &varName) const {
    //std::cout << "ArrayType code_gen_alloca('" << varName << "')" << std::endl;
    Type* headerType = Type::getInt32Ty(context.llvmContext);
    ASSERT(this->is_concrete(), "Attempted to codegen size of non-concrete type " << this);

    // construct LLVM array type:
    ResolutionContext resCtx;
    Type* elemType = context.get_llvm_type(this->element_type(resCtx));
    auto staticLength = this->length(resCtx)->get_static_constant_proxy();
    uint32_t nofElems = (staticLength ? staticLength->get_value_UInt() : 0);
    std::vector<llvm::Type*> llvmMemberTypes {
        headerType,
        ArrayType::get(elemType, nofElems)
    };
    auto llvmType = StructType::get(context.llvmContext, llvmMemberTypes);

    // allocate array object:
    Value* arrayLen;  // uint32
    Value* arrayObj;  // pointer to llvm array obj
    if (staticLength) {
        // if statically known array size, use direct LLVM type
        arrayLen = ConstantInt::get(Type::getInt32Ty(context.llvmContext), nofElems);
        arrayObj = scope->builder->CreateAlloca(llvmType, nullptr, "arrayalloc");
    }
    else {
        // otherwise calculate allocation size as a multiple of the array header size - ought to provide sufficient alignment?
        arrayLen = scope->builder->CreateZExtOrBitCast(this->length(resCtx)->code_gen(context, scope),
                                                       Type::getInt32Ty(context.llvmContext));
        auto arrayLen64 = scope->builder->CreateZExtOrBitCast(arrayLen, Type::getInt64Ty(context.llvmContext));
        Value* elemSize = this->element_type(resCtx)->code_gen_size(context, scope);
        Value* objectSize = this->inner_code_gen_size(context, scope, elemSize, arrayLen64);

        Value* allocElems = code_gen_4_multiple(context, scope, objectSize);
        Value* allocation = scope->builder->CreateAlloca(headerType, allocElems, "arrayalloc");
        Type* ptrType = PointerType::getUnqual(llvmType);
        arrayObj = scope->builder->CreatePointerCast(allocation, ptrType, varName);
    }

    // initialize length field:
    Value* ixs[] = { ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0),
                     ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0) };
    auto lenField = scope->builder->CreateInBoundsGEP(arrayObj, ixs);
    scope->builder->CreateStore(arrayLen, lenField);

    return arrayObj;
}
