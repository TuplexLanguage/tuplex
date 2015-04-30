#include "ast_exprs.hpp"
#include "llvm_generator.hpp"

using namespace llvm;


/** Convenience function that returns true if type is a pointer whose destination should be
 * directly loaded / stored when accessed. */
static bool access_via_load_store(const Type* type) {
    bool ret = (type->isPointerTy() && type->getPointerElementType()->isSingleValueType());
    //bool ret = (type->isPointerTy() && type->getPointerElementType()->isFirstClassType());
    //std::cout << "access_via_load_store(): " << ret << ": type: " << type << std::endl;
    return ret;
}


static Value* virtual_field_value_code_gen(LlvmGenerationContext& context, GenScope* scope,
                                           const TxType* staticBaseType, Value* runtimeBaseTypeIdV, const TxFieldEntity* fieldEntity) {
    // retrieve the vtable of the base's actual (runtime) type:
    Value* vtableBase = context.gen_get_vtable(scope, staticBaseType, runtimeBaseTypeIdV);
    //std::cout << "vtableBase: " << vtableBase << std::endl;
    if (! vtableBase) {
        context.LOG.error("No vtable obtained for %s", staticBaseType->to_string().c_str());
        return nullptr;
    }

    // get the virtual field:
    auto fieldIx = fieldEntity->get_virtual_field_index();
    //std::cout << "(static type id " << baseExpr->get_type()->get_type_id() << ") Getting TXS_VIRTUAL ix " << fieldIx << " value off LLVM base value: " << vtableBase << std::endl;
    Value* ixs[] = { ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0),
                     ConstantInt::get(Type::getInt32Ty(context.llvmContext), fieldIx) };

    if (!scope) {
        Value* fieldPtr = GetElementPtrInst::CreateInBounds(vtableBase, ixs);
        return new LoadInst(fieldPtr);
    }
    else {
        Value* fieldPtr = scope->builder->CreateInBoundsGEP(vtableBase, ixs);
        return scope->builder->CreateLoad(fieldPtr);
    }
}

/** Generate code to obtain field value, potentially based on a base value (pointer). */
static Value* field_value_code_gen(LlvmGenerationContext& context, GenScope* scope,
                                   Value* baseValue, const TxExpressionNode* baseExpr, const TxFieldEntity* fieldEntity, bool foldStatics=false) {
    Value* val;
    switch (fieldEntity->get_storage()) {
    case TXS_INSTANCEMETHOD:
        Value* funcPtrV;
        Value* instanceTypeIdV;
        if (baseExpr) {
            // effectively a polymorphic lookup if base expression is a reference dereference
            ASSERT(! baseValue, "Can't specify both baseValue and baseExpr");
            instanceTypeIdV = baseExpr->code_gen_typeid(context, scope);
            funcPtrV = virtual_field_value_code_gen(context, scope, baseExpr->get_type(), instanceTypeIdV, fieldEntity);
            baseValue = baseExpr->code_gen(context, scope);
        }
        else if (baseValue) {
            auto outerType = static_cast<TxTypeEntity*>(fieldEntity->get_outer())->get_type();
            instanceTypeIdV = outerType->gen_typeid(context, scope);
            funcPtrV = virtual_field_value_code_gen(context, scope, outerType, instanceTypeIdV, fieldEntity);
        }
        else {
            context.LOG.error("Can't access instance method without base value/expression: %s", fieldEntity->to_string().c_str());
            return nullptr;
        }
        //std::cout << "funcPtrV: " << funcPtrV << std::endl;
        ASSERT(funcPtrV->getType()->getPointerElementType()->isFunctionTy() , "Expected funcPtrV to be pointer-to-function type but was: " << funcPtrV->getType());
        ASSERT(baseValue->getType()->isPointerTy(), "Expected baseValue to be of pointer type but was: " << baseValue->getType());

        {   // construct the lambda object:
            auto closureRefT = context.get_voidRefT();
            auto closureRefV = gen_ref(context, scope, closureRefT, baseValue, instanceTypeIdV);
            auto lambdaT = cast<StructType>(context.get_llvm_type(fieldEntity->get_type()));
            val = gen_lambda(context, scope, lambdaT, funcPtrV, closureRefV);
        }
        break;

    case TXS_VIRTUAL:
        if (baseExpr) {
            // effectively a polymorphic lookup if base expression is a reference dereference
            ASSERT(! baseValue, "Can't specify both baseValue and baseExpr");
            val = virtual_field_value_code_gen(context, scope, baseExpr->get_type(), baseExpr->code_gen_typeid(context, scope), fieldEntity);
            break;
        }
        // no break

    case TXS_STATIC:
    case TXS_GLOBAL:
    case TXS_STACK:
        if (foldStatics) {
            if (auto constProxy = fieldEntity->get_static_constant_proxy()) {
                val = constProxy->code_gen(context, scope);
                context.LOG.debug("Generating field value code for statically constant fieldEntity %s: %s", fieldEntity->to_string().c_str(), ::to_string(val).c_str());
                break;
            }
        }
        val = context.lookup_llvm_value(fieldEntity->get_full_name().to_string());
        if (! val) {
            if (auto txType = fieldEntity->get_type()) {
                // forward declaration situation
                ASSERT(fieldEntity->get_storage() == TXS_GLOBAL || fieldEntity->get_storage() == TXS_STATIC,
                       "'forward-declaration' only expected for GLOBAL or STATIC fields: " << fieldEntity->to_string());
                if (auto txFuncType = dynamic_cast<const TxFunctionType*>(txType)) {
                    context.LOG.alert("Forward-declaring function object %s", fieldEntity->get_full_name().to_string().c_str());
                    StructType *lambdaT = cast<StructType>(context.get_llvm_type(txFuncType));
//                    FunctionType *funcT = cast<FunctionType>(cast<PointerType>(lambdaT->getElementType(0))->getPointerElementType());
//                    auto funcName = fieldEntity->get_full_name().to_string() + "$func";
//                    auto funcV = context.llvmModule.getOrInsertFunction(funcName, funcT);
//                    //cast<Function>(funcV)->setLinkage(GlobalValue::InternalLinkage);  FIXME (can cause LLVM to rename function)
//                    // construct the lambda object:
//                    auto nullClosureRefV = Constant::getNullValue(lambdaT->getElementType(1));
//                    val = ConstantStruct::get(lambdaT, funcV, nullClosureRefV, NULL);
                    //val = new llvm::GlobalVariable(context.llvmModule, lambdaT, true, llvm::GlobalValue::InternalLinkage,
                    //                               nullptr, fieldEntity->get_full_name().to_string());
                    val = context.llvmModule.getOrInsertGlobal(fieldEntity->get_full_name().to_string(), lambdaT);
                }
                else {
                    context.LOG.error("No LLVM value defined for %s", fieldEntity->to_string().c_str());
                    return nullptr;
                }
            }
        }
        break;

    case TXS_INSTANCE:
        {
            if (baseExpr) {
                ASSERT(! baseValue, "Can't specify both baseValue and baseExpr");
                baseValue = baseExpr->code_gen(context, scope);
            }
            else if (! baseValue) {
                context.LOG.error("Attempted to dereference TXS_INSTANCE field but no base pointer provided (identifier %s)", fieldEntity->get_full_name().to_string().c_str());
                return nullptr;
            }

            auto fieldIx = fieldEntity->get_instance_field_index();
            //std::cout << "Getting TXS_INSTANCE ix " << fieldIx << " value off LLVM base value: " << baseValue << std::endl;
            Value* ixs[] = { ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0),
                             ConstantInt::get(Type::getInt32Ty(context.llvmContext), fieldIx) };
            if (!scope)
                val = GetElementPtrInst::CreateInBounds(baseValue, ixs);
            else
                val = scope->builder->CreateInBoundsGEP(baseValue, ixs);
        }
        break;

    case TXS_NOSTORAGE:
        context.LOG.warning("TXS_NOSTORAGE specified for field: %s", fieldEntity->get_full_name().to_string().c_str());
        return nullptr;
    }
    return val;
}


Value* TxFieldValueNode::code_gen_address(LlvmGenerationContext& context, GenScope* scope, bool foldStatics) const {
    //return context.lookup_llvm_value(this->get_entity()->get_full_name().to_string());
    Value* value = NULL;
    const TxExpressionNode* baseExpr = this->baseExpr;
    for (auto symbol : this->memberPath) {
        if (auto fieldEntity = dynamic_cast<const TxFieldEntity*>(symbol)) {
            value = field_value_code_gen(context, scope, value, baseExpr, fieldEntity, foldStatics);
            if (symbol != this->memberPath.back()) {  // skips the load for the last segment
                if ( value && access_via_load_store(value->getType()) ) {
                    if (scope)
                        value = scope->builder->CreateLoad(value);
                    else
                        value = new LoadInst(value);
                }
            }
        }
        else
            value = NULL;
        baseExpr = NULL;
    }
    return value;
}

Value* TxFieldValueNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    Value* value = this->code_gen_address(context, scope, true);

    // Only function/complex pointers and non-modifiable temporaries don't require a load instruction:
    if ( value && access_via_load_store(value->getType()) ) {
        if (scope)
            value = scope->builder->CreateLoad(value);
        else {
           // in global scope we apparently don't want to load
        }
    }
    return value;
}


Value* TxFieldAssigneeNode::code_gen(LlvmGenerationContext& context, GenScope* scope) const {
    context.LOG.trace("%-48s", this->to_string().c_str());
    Value* value = NULL;
    const TxExpressionNode* baseExpr = this->baseExpr;
    for (auto symbol : this->memberPath) {
        if (auto fieldEntity = dynamic_cast<const TxFieldEntity*>(symbol))
            value = field_value_code_gen(context, scope, value, baseExpr, fieldEntity);
        else
            value = NULL;
        baseExpr = NULL;
    }
    return value;
}

