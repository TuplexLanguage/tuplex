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



StructType* TxActualType::make_vtable_type(LlvmGenerationContext& context) const {
    LOG_TRACE(context.LOGGER(), "Mapping vtable of type " << this->get_declaration()->get_unique_full_name() << ": " << this->str(true));
    std::vector<Type*> members;
    for (auto memberTxField : this->get_virtual_fields().fields) {
        auto memberTxType = memberTxField->get_type();
        auto lMemberType = context.get_llvm_type(memberTxType->type());
        if (memberTxField->get_storage() == TXS_INSTANCEMETHOD)
            lMemberType = lMemberType->getStructElementType(0);
        else if (memberTxField->get_unique_name() != "$adTypeId")  // $adTypeId is direct value, not a pointer to separate global
            lMemberType = PointerType::getUnqual(lMemberType);
        members.push_back(lMemberType);
        LOG_TRACE(context.LOGGER(), "Mapping virtual member type " << memberTxType << " to: " << ::to_string(lMemberType));
    }
    // (create() could be used to get named struct types)
    //StructType* vtableT = StructType::create(context.llvmContext, members, this->get_declaration()->get_unique_full_name() + "$VTable");
    StructType* vtableT = StructType::get(context.llvmContext, members);
    return vtableT;
}

//Constant* TxActualType::code_gen_vtable_size(LlvmGenerationContext& context) const {
//    Type* llvmType = this->make_vtable_llvm_type(context);
//    if (! llvmType)
//        return nullptr;
//    return ConstantExpr::getSizeOf(llvmType);
//}

Function* TxActualType::get_type_user_init_func(LlvmGenerationContext& context) const {
    std::string funcName(this->get_declaration()->get_unique_full_name() + ".$tuinit");

    std::vector<Type*> typeInitFuncArgTypes {
        context.get_voidPtrT()  // void* to type's data
    };
    FunctionType *typeInitFuncType = FunctionType::get(context.get_voidT(), typeInitFuncArgTypes, false);

    Function *initFunc = cast<Function>(context.llvmModule.getOrInsertFunction(funcName, typeInitFuncType));
    BasicBlock::Create(context.llvmModule.getContext(), "entry", initFunc);
    return initFunc;
}



Value* TxActualType::gen_size(LlvmGenerationContext& context, GenScope* scope) const {
    ASSERT(this->is_concrete(), "Attempted to codegen size of non-concrete type " << this);
    Type* llvmType = context.get_llvm_type(this);  // (gets the cached LLVM type if previously accessed)
    if (! llvmType)
        return nullptr;
    return ConstantExpr::getSizeOf(llvmType);
}

Value* TxActualType::gen_alloca(LlvmGenerationContext& context, GenScope* scope, const std::string &varName) const {
    ASSERT(this->is_concrete(), "Attempted to codegen alloca of non-concrete type " << this);
    Type* llvmType = context.get_llvm_type(this);  // (gets the cached LLVM type if previously accessed)
    if (! llvmType)
        return nullptr;
    return scope->builder->CreateAlloca(llvmType, 0, varName);
}

Constant* TxActualType::gen_typeid(LlvmGenerationContext& context, GenScope* scope) const {
    return ConstantInt::get(Type::getInt32Ty(context.llvmContext), this->get_type_id());
}


Type* TxBoolType::make_llvm_type(LlvmGenerationContext& context) const {
    return Type::getInt1Ty(context.llvmContext);
}

Type* TxIntegerType::make_llvm_type(LlvmGenerationContext& context) const {
    switch (this->_size) {
    case 1:
        return Type::getInt8Ty(context.llvmContext);
    case 2:
        return Type::getInt16Ty(context.llvmContext);
    case 4:
        return Type::getInt32Ty(context.llvmContext);
    case 8:
        return Type::getInt64Ty(context.llvmContext);
    default:
        LOG(context.LOGGER(), ERROR, "Unsupported integer size " << this->_size << " in type " << this);
        return nullptr;
    }
}

Type* TxFloatingType::make_llvm_type(LlvmGenerationContext& context) const {
    switch (this->_size) {
    case 2:
        return Type::getHalfTy(context.llvmContext);
    case 4:
        return Type::getFloatTy(context.llvmContext);
    case 8:
        return Type::getDoubleTy(context.llvmContext);
    default:
        LOG(context.LOGGER(), ERROR, "Unsupported floating-point size " << this->_size << " in type " << this);
        return nullptr;
    }
}



Type* TxArrayType::make_llvm_type(LlvmGenerationContext& context) const {
    //std::cout << "ArrayType make_llvm_type() " << ((void*)this) << std::endl;
    auto txElemType = this->element_type();
    if (! txElemType) {
        LOG(context.LOGGER(), ERROR, "Generic arrays with unspecified element type can't be directly mapped: " << this);
        return nullptr;
    }
    Type* elemType = context.get_llvm_type(txElemType);
    if (! elemType) {
        LOG(context.LOGGER(), ERROR, "No LLVM type mapping for array element type: " << txElemType);
        return nullptr;
    }

    uint32_t arrayLen;
    if (auto lenExpr = this->length()) {
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
    std::vector<Type*> llvmMemberTypes {
        Type::getInt32Ty(context.llvmContext),
        ArrayType::get(elemType, arrayLen)
    };
    auto llvmType = StructType::get(context.llvmContext, llvmMemberTypes);
    LOG_DEBUG(context.LOGGER(), "Mapping array type " << this << " -> " << str(llvmType));
    return llvmType;
}

Value* TxArrayType::gen_size(LlvmGenerationContext& context, GenScope* scope) const {
    ASSERT(this->is_concrete(), "Attempted to codegen size of non-concrete type " << this);
    Value* elemSize = this->element_type()->gen_size(context, scope);
    Value* arrayLen = this->length()->code_gen(context, scope);
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
        LOG(context.LOGGER(), WARN, "code_gen_size() with NULL scope and non-const expression for " << this);
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
    Type* elemType = context.get_llvm_type(this->element_type());
    auto staticLength = this->length()->get_static_constant_proxy();
    uint32_t nofElems = (staticLength ? staticLength->get_value_UInt() : 0);
    std::vector<Type*> llvmMemberTypes {
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
        arrayLen = scope->builder->CreateZExtOrBitCast(this->length()->code_gen(context, scope),
                                                       Type::getInt32Ty(context.llvmContext));
        auto arrayLen64 = scope->builder->CreateZExtOrBitCast(arrayLen, Type::getInt64Ty(context.llvmContext));
        Value* elemSize = this->element_type()->gen_size(context, scope);
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
    // Note: a reference itself is always 'concrete'
    if (auto txTargetType = this->target_type()) {
        if (Type* targetType = context.get_llvm_type(txTargetType)) {
            if (targetType->isVoidTy()) {
                // happens when the target type was resolved to Any
                targetType = Type::getInt8Ty(context.llvmContext);  // i8* represents void*
            }
            LOG_DEBUG(context.LOGGER(), "Mapping reference type " << this);
            return make_ref_llvm_type(context, targetType);
        }
        else
            LOG(context.LOGGER(), ERROR, "No LLVM type mapping for reference target type: " << txTargetType);
    }
    else
        LOG(context.LOGGER(), ERROR, "Unknown target type of reference type " << this);
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

Value* TxReferenceType::gen_ref_conversion(LlvmGenerationContext& context, GenScope* scope, Value* origValue,
                                           Type* targetRefT, uint32_t targetTypeId) {
    auto newPtrT = cast<StructType>(targetRefT)->getElementType(0);
    Value* tidV = (targetTypeId == UINT32_MAX ? gen_get_ref_typeid(context, scope, origValue)
                                              : ConstantInt::get(Type::getInt32Ty(context.llvmContext), targetTypeId));
    Value* origPtrV = gen_get_ref_pointer(context, scope, origValue);
    Value* newPtrV;
    // bitcast from one pointer type to another
    if (!scope)
        newPtrV = ConstantExpr::getBitCast(cast<Constant>(origPtrV), newPtrT);
    else
        newPtrV = scope->builder->CreateBitCast(origPtrV, newPtrT);

    return gen_ref(context, scope, targetRefT, newPtrV, tidV);
}



Type* TxFunctionType::make_llvm_type(LlvmGenerationContext& context) const {
    auto closureRefT = context.get_voidRefT();
    std::vector<Type*> llvmArgTypes;
    llvmArgTypes.push_back(closureRefT);  // first argument is always the closure object pointer
    for (auto argTxType : this->argumentTypes) {
        llvmArgTypes.push_back(context.get_llvm_type(argTxType));
        LOG_DEBUG(context.LOGGER(), "Mapping arg type " << argTxType << " to " << ::to_string(llvmArgTypes.back()));
    }
    Type* llvmRetType = this->has_return_value()
                        ? context.get_llvm_type(this->returnType)
                        : context.get_voidT();
    FunctionType *funcT = FunctionType::get(llvmRetType, llvmArgTypes, false);

    std::vector<Type*> llvmMemberTypes {
        PointerType::getUnqual(funcT),  // function pointer
        closureRefT                     // closure object pointer
    };
    auto llvmType = StructType::get(context.llvmContext, llvmMemberTypes);
    return llvmType;
}



Type* TxTupleType::make_llvm_type(LlvmGenerationContext& context) const {
// FUTURE: when is_concrete() is more robust (works for code within generic types) this should maybe be reintroduced:
//    if (! this->is_concrete()) {
//        context.LOG.warning("making LLVM type of non-concrete type %s", this->to_string().c_str());
//        return StructType::create(context.llvmContext);  // creates opaque (empty placeholder) structure
//    }
    StructType* opaqueType = StructType::create(context.llvmContext, this->get_declaration()->get_unique_full_name());
    return opaqueType;
}

Type* TxTupleType::make_llvm_type_body(LlvmGenerationContext& context, Type* header) const {
    LOG_TRACE(context.LOGGER(), "Mapping tuple type " << this->get_declaration()->get_unique_full_name() << ": " << this->str(true));
    std::vector<Type*> fieldTypes;
    for (auto memberTxField : this->get_instance_fields().fields) {
        auto memberTxType = memberTxField->get_type()->type();
        auto memberLlvmType = context.get_llvm_type(memberTxType);
        fieldTypes.push_back(memberLlvmType);
        LOG_TRACE(context.LOGGER(), "Mapping member type " << memberTxType << " to " << ::to_string(memberLlvmType));
    }
    StructType* sType = cast<StructType>(header);
    sType->setBody(fieldTypes);
    return sType;
}

Type* TxInterfaceType::make_llvm_type(LlvmGenerationContext& context) const {
    return StructType::get(context.llvmContext);  // abstract type
}

Type* TxInterfaceAdapterType::make_llvm_type(LlvmGenerationContext& context) const {
    return StructType::get(context.llvmContext);  // abstract type
}
