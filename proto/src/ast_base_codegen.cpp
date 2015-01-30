#include "ast_base.hpp"
#include "llvm_generator.hpp"


/*--- AST traversing LLVM IR code generation ---*/


/** Convenience function that returns true if type is a pointer to a non-single value type. */
inline bool is_complex_pointer(const llvm::Type* type) {
    bool ret = (type->isPointerTy() && !type->getPointerElementType()->isSingleValueType());
//    bool ret;
//    if (! type->isPointerTy())
//        ret = false;
//    else {
//        auto elemType = type->getPointerElementType();
//        //std::cout << value << " pointer type: "<< elemType << " is array: " << elemType->isArrayTy() << std::endl;
//        //ret = elemType->isArrayTy() || elemType->isFunctionTy() || elemType->isStructTy();
//        ret = !elemType->isSingleValueType();
//    }
    //std::cout << "is_complex_pointer(): " << ret << ": type: " << type << std::endl;
    return ret;
}


llvm::Value* TxNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.error("Invoked empty codeGen() on %s", this->to_string().c_str());
    return nullptr;
}

llvm::Value* TxParsingUnitNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    for (auto mod : this->modules) {
        mod->codeGen(context, scope);
    }
    return NULL;
}
llvm::Value* TxModuleNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    if (this->members)
        for (TxDeclarationNode* elem : *this->members) {
            elem->codeGen(context, scope);
        }
    return nullptr;
}

llvm::Value* TxTypeDeclNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    return this->typeExpression->codeGen(context, scope);
}
llvm::Value* TxTypeExpressionNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    return nullptr;  // default does nothing
}


static llvm::Value* make_constant_nonlocal_field(LlvmGenerationContext& context, GenScope* scope,
                                                 TxFieldDefNode* field, llvm::Type* llvmType) {
    auto entity = field->get_entity();
    llvm::Constant* constantInitializer = nullptr;
    if (field->initExpression) {
        if (field->initExpression->is_statically_constant()) {
            auto initValue = field->initExpression->codeGen(context, scope);
            constantInitializer = llvm::cast<llvm::Constant>(initValue);
            if (! constantInitializer)
                context.LOG.error("Global field %s constant initializer is NULL", entity->get_full_name().to_string().c_str());
            else if (is_complex_pointer(constantInitializer->getType())) {
                return constantInitializer;
            }
        }
        else
            context.LOG.error("Global field %s initializer is not a constant expression", entity->get_full_name().to_string().c_str());
    }
    else
        context.LOG.error("Global field %s does not have an initializer", entity->get_full_name().to_string().c_str());
    return new llvm::GlobalVariable(context.llvmModule, llvmType, true, llvm::GlobalValue::InternalLinkage,
                                    constantInitializer, entity->get_full_name().to_string());
}

llvm::Value* TxFieldDeclNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    auto entity = this->field->get_entity();
    auto txType = entity->get_type();
    llvm::Type* llvmType = context.get_llvm_type(txType);

    llvm::Value* fieldVal = nullptr;
    switch (entity->get_storage()) {
    case TXS_NOSTORAGE:
        context.LOG.error("TXS_NOSTORAGE specified for field: %s", entity->get_full_name().to_string().c_str());
        break;

    case TXS_GLOBAL:
        fieldVal = make_constant_nonlocal_field(context, scope, this->field, llvmType);
        break;

    case TXS_STATIC:
        // TODO: static fields have polymorphic lookup
        if (! txType->is_modifiable())
            fieldVal = make_constant_nonlocal_field(context, scope, this->field, llvmType);
        else {
            context.LOG.error("modifiable TXS_STATIC fields not yet implemented: %s", entity->get_full_name().to_string().c_str());
        }
        break;

    case TXS_INSTANCE:
        // just a type definition; field storage isn't created until parent object is allocated
        // TODO: if function and non-modifiable, skip "function pointer field assignment"
        // (else: convert function type to function pointer type)
        break;

    case TXS_STACK:
        context.LOG.error("TxFieldDeclNode can not apply to TXS_STACK storage fields: %s", entity->get_full_name().to_string().c_str());
        break;
    }
    if (fieldVal)
        context.register_llvm_value(entity->get_full_name().to_string(), fieldVal);
    return fieldVal;
}

llvm::Value* TxFieldDefNode::codeGen(LlvmGenerationContext& context, GenScope* scope) const {
    // (note that this is doesn't *declare* the field since that operation is context-sensitive;
    // the parent node does that)
    return nullptr;  // passive node
}
