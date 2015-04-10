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


/** Generate code to obtain field value, potentially based on a base value (pointer). */
static Value* field_value_code_gen(LlvmGenerationContext& context, GenScope* scope,
                                   Value* baseValue, const TxExpressionNode* baseExpr, const TxFieldEntity* fieldEntity, bool foldStatics=false) {
    Value* val = NULL;
    switch (fieldEntity->get_storage()) {
    case TXS_VIRTUAL:
        if (baseExpr) {  // is polymorphic lookup if base expression is a reference (or reference dereference)
            ASSERT(! baseValue, "Can't specify both baseValue and baseExpr");
            // retrieve the vtable of the base's actual (runtime) type:
            auto statBaseType = baseExpr->get_type();
            Value* vtableBase = context.gen_get_vtable(scope, statBaseType, baseExpr->code_gen_typeid(context, scope));
            if (! vtableBase) {
                context.LOG.error("No vtable obtained for %s", baseExpr->get_type()->to_string().c_str());
                return nullptr;
            }

            // get the virtual field:
            auto fieldIx = fieldEntity->get_virtual_field_index();
            //std::cout << "(static type id " << baseExpr->get_type()->get_type_id() << ") Getting TXS_VIRTUAL ix " << fieldIx << " value off LLVM base value: " << vtableBase << std::endl;
            Value* ixs[] = { ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0),
                             ConstantInt::get(Type::getInt32Ty(context.llvmContext), fieldIx) };

            if (!scope) {
                Value* fieldPtr = GetElementPtrInst::CreateInBounds(vtableBase, ixs);
                val = new LoadInst(fieldPtr);
            }
            else {
                Value* fieldPtr = scope->builder->CreateInBoundsGEP(vtableBase, ixs);
                val = scope->builder->CreateLoad(fieldPtr);
            }
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
                if (auto txFuncType = dynamic_cast<const TxFunctionType*>(txType)) {
                    context.LOG.alert("Forward-declaring function %s", fieldEntity->get_full_name().to_string().c_str());
                    FunctionType *ftype = cast<FunctionType>(context.get_llvm_type(txFuncType));
                    val = context.llvmModule.getOrInsertFunction(fieldEntity->get_full_name().to_string(), ftype);
                    //cast<Function>(val)->setLinkage(GlobalValue::InternalLinkage);  FIXME (can cause LLVM to rename function)
                }
                else
                    context.LOG.error("No LLVM value defined for %s", fieldEntity->to_string().c_str());
            }
        }
        break;

    case TXS_INSTANCE:
        {
            if (baseExpr) {
                ASSERT(! baseValue, "Can't specify both baseValue and baseExpr");
                baseValue = baseExpr->code_gen(context, scope);
            }
            else if (! baseValue)
                context.LOG.error("Attempted to dereference TXS_INSTANCE field but no base pointer provided (identifier %s)", fieldEntity->get_full_name().to_string().c_str());

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
        //context.LOG.error("TXS_NOSTORAGE specified for field: %s", entity->get_full_name().to_string().c_str());
        break;
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

