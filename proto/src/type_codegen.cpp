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


//static Value* get_static_type(const TxType* type) {
//    if (auto baseType = type->get_base_type()) {
//        get_static_type(baseType);
//    }
//
//    if (auto entity = type.entity()) {
//    }
//
//}



llvm::StructType* TxType::make_vtable_type(LlvmGenerationContext& context) const {
    // (similar to tuple type creation)
    auto entity = this->entity();
    if (! entity) {
        context.LOG.error("No entity for type %s - can't perform vtable LLVM type mapping", this->to_string().c_str());
        return nullptr;
    }
    context.LOG.debug("Mapping vtable of type %s: %s", entity->get_full_name().to_string().c_str(), this->to_string(true).c_str());
    std::vector<llvm::Type*> members;
    for (auto memberTxType : entity->get_virtual_fields().fieldTypes) {
        auto lMemberType = context.get_llvm_type(memberTxType);
        auto membPtrType = PointerType::getUnqual(lMemberType);
        members.push_back(membPtrType);
        context.LOG.debug("Mapping static member pointer type %s to %s", memberTxType->to_string().c_str(), ::to_string(lMemberType).c_str());
    }
    // note: create() might be better for "named" struct types?
    llvm::StructType* vtableT = llvm::StructType::get(context.llvmContext, members);
    return vtableT;
}

//Constant* TxType::code_gen_vtable_size(LlvmGenerationContext& context) const {
//    Type* llvmType = this->make_vtable_llvm_type(context);
//    if (! llvmType)
//        return nullptr;
//    return ConstantExpr::getSizeOf(llvmType);
//}

Function* TxType::get_type_user_init_func(LlvmGenerationContext& context) const {
    auto entity = this->entity();
    if (! entity) {
        context.LOG.error("No entity for type %s - can't create type init func", this->to_string().c_str());
        return nullptr;
    }
    std::string funcName(entity->get_full_name().to_string() + ".$tuinit");

    auto voidType = Type::getVoidTy(context.llvmContext);
    std::vector<Type*> typeInitFuncArgTypes {
        Type::getInt8PtrTy(context.llvmContext)  // void* to type's data
    };
    FunctionType *typeInitFuncType = FunctionType::get(voidType, typeInitFuncArgTypes, false);

    Function *initFunc = cast<Function>(context.llvmModule.getOrInsertFunction(funcName, typeInitFuncType));
    BasicBlock::Create(context.llvmModule.getContext(), "entry", initFunc);
    return initFunc;
}



Value* TxType::gen_size(LlvmGenerationContext& context, GenScope* scope) const {
    ASSERT(this->is_concrete(), "Attempted to codegen size of non-concrete type " << this);
    Type* llvmType = context.get_llvm_type(this);  // (gets the cached LLVM type if previously accessed)
    if (! llvmType)
        return nullptr;
    return ConstantExpr::getSizeOf(llvmType);
}

Value* TxType::gen_alloca(LlvmGenerationContext& context, GenScope* scope, const std::string &varName) const {
    ASSERT(this->is_concrete(), "Attempted to codegen alloca of non-concrete type " << this);
    Type* llvmType = context.get_llvm_type(this);  // (gets the cached LLVM type if previously accessed)
    if (! llvmType)
        return nullptr;
    return scope->builder->CreateAlloca(llvmType, 0, varName);
}



Type* TxBoolType::make_llvm_type(LlvmGenerationContext& context) const {
    return llvm::Type::getInt1Ty(context.llvmContext);
}

Type* TxIntegerType::make_llvm_type(LlvmGenerationContext& context) const {
    switch (this->_size) {
    case 1:
        return llvm::Type::getInt8Ty(context.llvmContext);
    case 2:
        return llvm::Type::getInt16Ty(context.llvmContext);
    case 4:
        return llvm::Type::getInt32Ty(context.llvmContext);
    case 8:
        return llvm::Type::getInt64Ty(context.llvmContext);
    default:
        context.LOG.error("Unsupported integer size %ld in type %s", this->_size, this->to_string().c_str());
        return nullptr;
    }
}

Type* TxFloatingType::make_llvm_type(LlvmGenerationContext& context) const {
    switch (this->_size) {
    case 2:
        return llvm::Type::getHalfTy(context.llvmContext);
    case 4:
        return llvm::Type::getFloatTy(context.llvmContext);
    case 8:
        return llvm::Type::getDoubleTy(context.llvmContext);
    default:
        context.LOG.error("Unsupported floating-point size %ld in type %s", this->_size, this->to_string().c_str());
        return nullptr;
    }
}



Type* TxArrayType::make_llvm_type(LlvmGenerationContext& context) const {
    //std::cout << "ArrayType make_llvm_type() " << ((void*)this) << std::endl;
    ResolutionContext resCtx;
    auto txElemType = this->element_type(resCtx);
    if (! txElemType) {
        context.LOG.error("Generic arrays with unspecified element type can't be directly mapped: %s", this->to_string().c_str());
        return nullptr;
    }
    llvm::Type* elemType = context.get_llvm_type(txElemType);
    if (! elemType) {
        context.LOG.error("No LLVM type mapping for array element type: %s", txElemType->get_type()->to_string().c_str());
        return nullptr;
    }

    uint32_t arrayLen;
    if (auto lenExpr = this->length(resCtx)) {
        // concrete array (specific length)
        if (auto lenProxy = lenExpr->get_static_constant_proxy())
            arrayLen = lenProxy->get_value_UInt();  // length is statically specified
        else
            arrayLen = 0;  // length is dynamically specified
    }
    else {
        // Generic arrays with unspecified length are mapped as zero length,
        // so they can be referenced from e.g. references.
        arrayLen = 0;
    }
    std::vector<llvm::Type*> llvmMemberTypes {
        llvm::Type::getInt32Ty(context.llvmContext),
        llvm::ArrayType::get(elemType, arrayLen)
    };
    auto llvmType = llvm::StructType::get(context.llvmContext, llvmMemberTypes);
    context.LOG.debug("Mapping array type %s -> %s", this->to_string().c_str(), to_string(llvmType).c_str());
    return llvmType;
}

Value* TxArrayType::gen_size(LlvmGenerationContext& context, GenScope* scope) const {
    ASSERT(this->is_concrete(), "Attempted to codegen size of non-concrete type " << this);
    ResolutionContext resCtx;
    Value* elemSize = this->element_type(resCtx)->gen_size(context, scope);
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

Value* TxArrayType::gen_alloca(LlvmGenerationContext& context, GenScope* scope, const std::string &varName) const {
    //std::cout << "ArrayType code_gen_alloca('" << varName << "')" << std::endl;
    Type* headerType = Type::getInt32Ty(context.llvmContext);
    ASSERT(this->is_concrete(), "Attempted to codegen alloca of non-concrete type " << this);

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
        Value* elemSize = this->element_type(resCtx)->gen_size(context, scope);
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



Type* TxReferenceType::make_llvm_type(LlvmGenerationContext& context) const {
    // Note: a reference is always 'concrete'
    ResolutionContext resCtx;
    if (auto txTargetType = this->target_type(resCtx)) {
        if (Type* targetType = context.get_llvm_type(txTargetType)) {
            context.LOG.debug("Mapping reference type %s", this->to_string().c_str());
            return make_ref_llvm_type(context, targetType);
        }
        else
            context.LOG.error("No LLVM type mapping for reference target type: %s", txTargetType->to_string().c_str());
    }
    else
        context.LOG.error("Unknown target type of reference type %s", this->to_string().c_str());
    return nullptr;
}

Type* TxReferenceType::make_ref_llvm_type(LlvmGenerationContext& context, Type* targetType) {
    std::vector<Type*> llvmMemberTypes {
        PointerType::getUnqual(targetType),
        Type::getInt32Ty(context.llvmContext)  // type header for reference target type
    };
    auto llvmType = StructType::get(context.llvmContext, llvmMemberTypes);
    return llvmType;
}



Type* TxFunctionType::make_llvm_type(LlvmGenerationContext& context) const {
    std::vector<llvm::Type*> llvmArgTypes;
    for (auto argTxType : this->argumentTypes) {
        llvmArgTypes.push_back(context.get_llvm_type(argTxType));
        context.LOG.debug("Mapping arg type %s to %s", argTxType->to_string().c_str(), ::to_string(llvmArgTypes.back()).c_str());
    }
    llvm::Type* llvmRetType = this->returnType
                              ? context.get_llvm_type(this->returnType)
                              : llvm::Type::getVoidTy(context.llvmContext);
    llvm::FunctionType *ftype = llvm::FunctionType::get(llvmRetType, llvmArgTypes, false);
    return ftype;
}



Type* TxTupleType::make_llvm_type(LlvmGenerationContext& context) const {
    if (! this->is_concrete()) {
        context.LOG.warning("making LLVM type of non-concrete type %s", this->to_string().c_str());
        return llvm::StructType::create(context.llvmContext);  // creates opaque (empty placeholder) structure
    }
    auto entity = this->entity();
    if (! entity) {
        context.LOG.error("No entity for Tuple type %s - can't perform LLVM type mapping", this->to_string().c_str());
        return nullptr;
    }
    context.LOG.debug("Mapping tuple type %s: %s", entity->get_full_name().to_string().c_str(), this->to_string(true).c_str());
    std::vector<llvm::Type*> llvmTypes;
    for (auto memberTxType : entity->get_instance_fields().fieldTypes) {
        auto memberLlvmType = context.get_llvm_type(memberTxType);
        llvmTypes.push_back(memberLlvmType);
        context.LOG.debug("Mapping member type %s to %s", memberTxType->to_string().c_str(), ::to_string(memberLlvmType).c_str());
    }
    // note: create() might be better for "named" struct types?
    llvm::StructType* stype = llvm::StructType::get(context.llvmContext, llvmTypes);
    return stype;
}
