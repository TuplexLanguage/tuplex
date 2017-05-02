#include "ast_base.hpp"
#include "llvm_generator.hpp"

#include <llvm/Analysis/ConstantFolding.h>

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


Value* TxParsingUnitNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    this->module->code_gen(context, scope);
    return NULL;
}

Value* TxModuleNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    if (this->members) {
        for (auto mem : *this->members)
            mem->code_gen(context, scope);
    }
    if (this->subModules) {
        for (auto mod : *this->subModules)
            mod->code_gen(context, scope);
    }
    return nullptr;
}

Value* TxTypeDeclNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    if (this->_builtinCode)
        return NULL;
    return this->typeExpression->code_gen(context, scope);
}


static Value* make_constant_nonlocal_field(LlvmGenerationContext& context, GenScope* scope,
                                           TxFieldDefNode* field, Type* llvmType) {
    auto uniqueName = field->get_declaration()->get_unique_full_name();
    Constant* constantInitializer = nullptr;
    if (field->initExpression) {
        if (field->initExpression->is_statically_constant()) {
            auto initValue = field->initExpression->code_gen(context, scope);
            constantInitializer = dyn_cast_or_null<Constant>(initValue);
            if (! constantInitializer)
                LOG(context.LOGGER(), ERROR, "Global field " << uniqueName << " initializer is not constant: " << to_string(initValue));
            else if (is_complex_pointer(constantInitializer->getType())) {
                context.LOGGER()->note("Global field %s with complex ptr constant initializer", uniqueName.c_str());
                //return constantInitializer;
                ASSERT(! context.llvmModule.getNamedGlobal(uniqueName),
                       "Can't declare llvm alias since global variable with same name already declared: " << uniqueName);
                return GlobalAlias::create(llvmType, 0, GlobalValue::InternalLinkage,
                                           uniqueName, constantInitializer, &context.llvmModule);
            }
//            else if (auto constantInitExpr = dyn_cast_or_null<ConstantExpr>(initValue)) {
//                std::cerr << "It's a constant expression: " << constantInitExpr << std::endl;
//            }
        }
        else
            LOG(context.LOGGER(), ERROR, "Global/static constant field " << uniqueName << " initializer is not a constant expression");
    }
    else
        LOG(context.LOGGER(), ERROR, "Global/static constant field " << uniqueName << " does not have an initializer");

    // handle case when there has been a "forward-declaration" of this field:
    Constant* maybe = context.llvmModule.getOrInsertGlobal(uniqueName, llvmType);
    //std::cout << "maybe type: " << *maybe->getType() << "  value: " << *maybe << std::endl;
    auto globalV = cast<GlobalVariable>(maybe);  // bails if bitcast has been inserted, which means wrong type has been chosen
    globalV->setConstant(true);
    globalV->setLinkage(GlobalValue::InternalLinkage);
    ASSERT(!globalV->hasInitializer(), "global already has initializer: " << globalV);
    globalV->setInitializer(constantInitializer);
    return globalV;
    //context.LOG.alert("Global field %s", fullName.to_string().c_str());
    //return new GlobalVariable(context.llvmModule, llvmType, true, GlobalValue::InternalLinkage,
    //                                constantInitializer, fullName.to_string());
}

Value* TxFieldDeclNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
// experimental
//    if (this->codeGenValue) {
//        context.LOG.note("Code already generated for %-48s", this->to_string().c_str());
//        return this->codeGenValue;
//    }

    TRACE_CODEGEN(this, context);
    if (this->field->typeExpression)
        this->field->typeExpression->code_gen(context, scope);
    auto fieldDecl = this->field->get_declaration();
    auto uniqueName = fieldDecl->get_unique_full_name();
    auto txType = this->field->get_type()->type();
    Type* llvmType = context.get_llvm_type(txType);

    Value* fieldVal = nullptr;
    switch (fieldDecl->get_storage()) {
    case TXS_NOSTORAGE:
        LOG(context.LOGGER(), ERROR, "TXS_NOSTORAGE specified for field: " << uniqueName);
        break;

    case TXS_INSTANCEMETHOD:
        if (!( fieldDecl->get_decl_flags() & TXD_ABSTRACT )
            // constructors in generic types are suppressed (they are not abstract per se, but aren't code generated):
            && !( ( fieldDecl->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER ) )
                  && static_cast<TxEntitySymbol*>( fieldDecl->get_symbol()->get_outer() )->get_type_decl()->get_definer()->get_type()->is_generic() ) ) {
            ASSERT(this->field->initExpression, "instance method does not have an initializer/definition: " << uniqueName);
            auto initLambdaV = cast<ConstantStruct>(this->field->initExpression->code_gen(context, scope));
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
        if (! (fieldDecl->get_decl_flags() & (TXD_ABSTRACT | TXD_INITIALIZER))) {
            //if (txType->is_modifiable())
            //    context.LOG.error("modifiable TXS_STATIC fields not yet implemented: %s", uniqueName.c_str());
            if (this->field->initExpression && !this->field->initExpression->is_statically_constant()) {
                // TODO
                auto lvl = Level::WARN; //( entity->is_generic_param_binding() ? Level::DEBUG : Level::WARN );
                context.LOGGER()->log(lvl, "Skipping codegen for global/static constant field %s whose initializer is not a constant expression", uniqueName.c_str());
            }
            else
                fieldVal = make_constant_nonlocal_field(context, scope, this->field, llvmType);
        }
        break;

    case TXS_INSTANCE:
        // just a type definition; field storage isn't created until parent object is allocated
        break;

    case TXS_STACK:
        LOG(context.LOGGER(), ERROR, "TxFieldDeclNode can not apply to TXS_STACK storage fields: " << uniqueName);
        break;
    }
    if (fieldVal)
        context.register_llvm_value(fieldVal->getName(), fieldVal);

//    this->codeGenValue = fieldVal;
    return fieldVal;
}

Value* TxFieldTypeDefNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    TRACE_CODEGEN(this, context);
    return nullptr;  // passive node
}

Value* TxFieldDefNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    // (note that this is doesn't *declare* the field since that operation is context-sensitive;
    // the parent node does that)
    TRACE_CODEGEN(this, context);
    return nullptr;  // passive node
}


Value* TxExpressionNode::code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const {
    return this->get_type()->type()->gen_typeid(context, scope);
}
