#include "ast_base.hpp"
#include "llvm_generator.hpp"


using namespace llvm;


/** Convenience function that returns true if type is a pointer to a non-single value type. */
inline bool is_complex_pointer(const Type* type) {
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


//Value* TxNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
//    context.LOG.error("Invoked empty codeGen() on %s", this->to_string().c_str());
//    return nullptr;
//}

Value* TxParsingUnitNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    for (auto mod : this->modules) {
        mod->code_gen(context, scope);
    }
    return NULL;
}
Value* TxModuleNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    if (this->members)
        for (TxDeclarationNode* elem : *this->members) {
            elem->code_gen(context, scope);
        }
    return nullptr;
}

Value* TxTypeDeclNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    return this->typeExpression->code_gen(context, scope);
}
//Value* TxTypeExpressionNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
//    return nullptr;  // default does nothing
//}


static Value* make_constant_nonlocal_field(LlvmGenerationContext& context, GenScope* scope,
                                                 TxFieldDefNode* field, Type* llvmType) {
    auto entity = field->get_entity();
    Constant* constantInitializer = nullptr;
    if (field->initExpression) {
        if (field->initExpression->is_statically_constant()) {
            auto initValue = field->initExpression->code_gen(context, scope);
            constantInitializer = dyn_cast_or_null<Constant>(initValue);
            if (! constantInitializer)
                context.LOG.error("Global field %s initializer is not constant: %s",
                                  entity->get_full_name().to_string().c_str(), to_string(initValue).c_str());
            else if (is_complex_pointer(constantInitializer->getType())) {
                context.LOG.alert("Global field %s with complex ptr constant initializer", entity->get_full_name().to_string().c_str());
                //return constantInitializer;
                ASSERT(! context.llvmModule.getNamedGlobal(entity->get_full_name().to_string()),
                       "Can't declare llvm alias since global variable with same name already declared: " << entity->get_full_name().to_string());
                return GlobalAlias::create(llvmType, 0, GlobalValue::InternalLinkage,
                                           entity->get_full_name().to_string(), constantInitializer, &context.llvmModule);
            }
        }
        else
            context.LOG.error("Global/static constant field %s initializer is not a constant expression", entity->get_full_name().to_string().c_str());
    }
    else
        context.LOG.error("Global/static constant field %s does not have an initializer", entity->get_full_name().to_string().c_str());

    // handle case when there has been a "forward-declaration" of this field:
    auto globalV = cast<GlobalVariable>(context.llvmModule.getOrInsertGlobal(entity->get_full_name().to_string(), llvmType));
    globalV->setConstant(true);
    globalV->setLinkage(GlobalValue::InternalLinkage);
    ASSERT(!globalV->hasInitializer(), "global already has initializer: " << globalV);
    globalV->setInitializer(constantInitializer);
    return globalV;
    //context.LOG.alert("Global field %s", entity->get_full_name().to_string().c_str());
    //return new GlobalVariable(context.llvmModule, llvmType, true, GlobalValue::InternalLinkage,
    //                                constantInitializer, entity->get_full_name().to_string());
}

Value* TxFieldDeclNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    if (this->field->typeExpression)
        this->field->typeExpression->code_gen(context, scope);
    auto entity = this->field->get_entity();
    auto txType = entity->get_type();
    Type* llvmType = context.get_llvm_type(txType);

    Value* fieldVal = nullptr;
    switch (entity->get_storage()) {
    case TXS_NOSTORAGE:
        context.LOG.error("TXS_NOSTORAGE specified for field: %s", entity->get_full_name().to_string().c_str());
        break;

    case TXS_INSTANCEMETHOD:
        {
            auto entity = field->get_entity();
            ASSERT(field->initExpression, "instance method does not have an initializer/definition: " << entity->get_full_name().to_string().c_str());
            auto initLambdaV = cast<ConstantStruct>(field->initExpression->code_gen(context, scope));
            auto funcPtrV = initLambdaV->getAggregateElement((unsigned)0);
            //std::cout << "initLambdaV: " << initLambdaV << std::endl;
            //std::cout << "initFuncPtrV: " << funcPtrV << std::endl;
            fieldVal = funcPtrV;  // the naked $func is stored (as opposed to a full lambda object)
        }
        break;

    case TXS_GLOBAL:
        fieldVal = make_constant_nonlocal_field(context, scope, this->field, llvmType);
        break;

    case TXS_VIRTUAL:
    case TXS_STATIC:
        if (! txType->is_modifiable()) {
            if (this->field->initExpression && !this->field->initExpression->is_statically_constant()) {
                auto lvl = ( entity->is_generic_param_binding() ? Level::DEBUG : Level::WARN );
                context.LOG.log(lvl, "Skipping codegen for global/static constant field %s whose initializer is not a constant expression", entity->get_full_name().to_string().c_str());
            }
            else
                fieldVal = make_constant_nonlocal_field(context, scope, this->field, llvmType);
        }
        else {
            context.LOG.error("modifiable TXS_STATIC fields not yet implemented: %s", entity->get_full_name().to_string().c_str());
        }
        break;

    case TXS_INSTANCE:
        // just a type definition; field storage isn't created until parent object is allocated
        break;

    case TXS_STACK:
        context.LOG.error("TxFieldDeclNode can not apply to TXS_STACK storage fields: %s", entity->get_full_name().to_string().c_str());
        break;
    }
    if (fieldVal)
        context.register_llvm_value(fieldVal->getName(), fieldVal);
    return fieldVal;
}

Value* TxFieldDefNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    // (note that this is doesn't *declare* the field since that operation is context-sensitive;
    // the parent node does that)
    return nullptr;  // passive node
}


Value* TxExpressionNode::code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const {
    return this->get_type()->gen_typeid(context, scope);
}
