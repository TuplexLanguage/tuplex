#include "ast_exprs.hpp"
#include "llvm_generator.hpp"

using namespace llvm;


/** Convenience function that returns true if type is a pointer whose destination should be
 * directly loaded / stored when accessed. */
static bool access_via_load_store(const Type* type) {
    // FIXME: This confuses 'void*' (i8*), which shouldn't be loaded, with pointer to i8. Can we name/unique voidT to avoid this?
    bool ret = (type->isPointerTy() && type->getPointerElementType()->isSingleValueType());
    //bool ret = (type->isPointerTy() && type->getPointerElementType()->isFirstClassType());
    //std::cout << "access_via_load_store(): " << ret << ": type: " << type << std::endl;
    return ret;
}


static Value* virtual_field_value_code_gen(LlvmGenerationContext& context, GenScope* scope,
                                           const TxType* staticBaseType, Value* runtimeBaseTypeIdV,
                                           Type* expectedValueType, const TxField* fieldEntity) {
    // retrieve the vtable of the base's actual (runtime) type:
    Value* vtableBase = context.gen_get_vtable(scope, staticBaseType, runtimeBaseTypeIdV);
    //std::cerr << "vtableBase: " << vtableBase << "  for field=" << fieldEntity << std::endl;
    if (! vtableBase) {
        context.LOG.error("No vtable obtained for %s", staticBaseType->to_string().c_str());
        return nullptr;
    }

    // get the virtual field:
    auto fieldIx = fieldEntity->get_virtual_field_index();
    //std::cerr << "(static type id " << staticBaseType->get_type_id() << ") Getting TXS_VIRTUAL ix " << fieldIx << " value off LLVM base value: " << vtableBase << std::endl;
    Value* ixs[] = { ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0),
                     ConstantInt::get(Type::getInt32Ty(context.llvmContext), fieldIx) };

    if (!scope) {
        Value* fieldPtr = GetElementPtrInst::CreateInBounds(vtableBase, ixs);
        return new LoadInst(fieldPtr);
    }
    else {
        Value* fieldPtr = scope->builder->CreateInBoundsGEP(vtableBase, ixs);
        Value* fieldV = scope->builder->CreateLoad(fieldPtr);
        // cast value type (necessary for certain (e.g. Ref-binding) specializations' methods):
        if (expectedValueType && expectedValueType->isPointerTy() && fieldV->getType()->isPointerTy()) {
            //std::cerr << "Casting from " << fieldV->getType() << "  to  " << expectedValueType << std::endl;
            fieldV = scope->builder->CreatePointerCast(fieldV, expectedValueType);
        }
        return fieldV;
    }
}

static Value* instance_method_value_code_gen(LlvmGenerationContext& context, GenScope* scope,
                                             const TxType* staticBaseType, Value* runtimeBaseTypeIdV, const TxField* fieldEntity,
                                             Value* baseValue, bool nonvirtualLookup) {
    auto lambdaT = cast<StructType>(context.get_llvm_type(fieldEntity->get_type()));
    Value* funcPtrV;
    if (nonvirtualLookup) {
        Value* staticBaseTypeIdV = staticBaseType->gen_typeid(context, scope);
        funcPtrV = virtual_field_value_code_gen(context, scope, staticBaseType, staticBaseTypeIdV, lambdaT->getElementType(0), fieldEntity);
    }
    else
        funcPtrV = virtual_field_value_code_gen(context, scope, staticBaseType, runtimeBaseTypeIdV, lambdaT->getElementType(0), fieldEntity);
    //std::cout << "funcPtrV: " << funcPtrV << std::endl;
    ASSERT(funcPtrV->getType()->getPointerElementType()->isFunctionTy() , "Expected funcPtrV to be pointer-to-function type but was: " << funcPtrV->getType());
    ASSERT(baseValue->getType()->isPointerTy(), "Expected baseValue to be of pointer type but was: " << baseValue->getType());

    {   // construct the lambda object:
        auto closureRefT = context.get_voidRefT();
        if (staticBaseType->get_type_class() == TXTC_INTERFACE) {
            // if base is an interface (in practice, interface adapter), populate the lambda with the adaptee type id instead
            auto adapteeTypeIdField = staticBaseType->get_virtual_fields().get_field("$adTypeId");
            auto adapteeTypeIdV = virtual_field_value_code_gen(context, scope, staticBaseType, runtimeBaseTypeIdV,
                                                               Type::getInt32Ty(context.llvmContext), adapteeTypeIdField);
            //std::cerr << "Invoking interface method " << fieldEntity << ", replacing type id " << runtimeBaseTypeIdV << " with " << adapteeTypeIdV << std::endl;
            runtimeBaseTypeIdV = adapteeTypeIdV;
        }
        auto closureRefV = gen_ref(context, scope, closureRefT, baseValue, runtimeBaseTypeIdV);
        return gen_lambda(context, scope, lambdaT, funcPtrV, closureRefV);
    }
}

static bool is_non_virtual_lookup(const TxExpressionNode* baseExpr) {
    if (auto fieldValueNode = dynamic_cast<const TxFieldValueNode*>(baseExpr))
        return (fieldValueNode->get_full_identifier() == "super");  // member invocation via super keyword is non-virtual
    else if (auto derefNode = dynamic_cast<const TxReferenceDerefNode*>(baseExpr))
        return is_non_virtual_lookup(derefNode->reference);
    else
        return false;
}

/** Generate code to obtain field value, potentially based on a base value (pointer). */
static Value* field_value_code_gen(LlvmGenerationContext& context, GenScope* scope,
                                   const TxExpressionNode* baseExpr, const TxField* fieldEntity, bool foldStatics=false) {
    ASSERT(!(fieldEntity->get_decl_flags() & TXD_CONSTRUCTOR), "Can't get 'field value' of constructor " << fieldEntity);

    bool nonvirtualLookup = is_non_virtual_lookup(baseExpr);  // true for super.foo lookups

    Value* val;
    switch (fieldEntity->get_storage()) {
    case TXS_INSTANCEMETHOD:
        if (baseExpr) {
            // virtual lookup will effectively be a polymorphic lookup if base expression is a reference dereference
            Value* runtimeBaseTypeIdV = baseExpr->code_gen_typeid(context, scope);  // (static unless reference)
            Value* baseValue = baseExpr->code_gen(context, scope);  // expected to be of pointer type
            val = instance_method_value_code_gen(context, scope, baseExpr->get_type(0), runtimeBaseTypeIdV, fieldEntity, baseValue, nonvirtualLookup);
        }
        else {
            context.LOG.error("Can't access instance method without base value/expression: %s", fieldEntity->to_string().c_str());
            return nullptr;
        }
        break;

    case TXS_VIRTUAL:
        if (baseExpr) {
            // virtual lookup will effectively be a polymorphic lookup if base expression is a reference dereference
            Value* baseTypeIdV = nonvirtualLookup ? baseExpr->get_type(0)->gen_typeid(context, scope)  // static
                                                  : baseExpr->code_gen_typeid(context, scope);  // runtime (static unless reference)
            Type* expectedT = context.get_llvm_type(fieldEntity->get_type());
            val = virtual_field_value_code_gen(context, scope, baseExpr->get_type(0), baseTypeIdV, expectedT, fieldEntity);
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
        val = context.lookup_llvm_value(fieldEntity->get_declaration()->get_unique_full_name());
        if (! val) {
            if (auto txType = fieldEntity->get_type()) {
                // forward declaration situation
                ASSERT(fieldEntity->get_storage() == TXS_GLOBAL || fieldEntity->get_storage() == TXS_STATIC,
                       "'forward-declaration' only expected for GLOBAL or STATIC fields: " << fieldEntity->to_string());
                if (auto txFuncType = dynamic_cast<const TxFunctionType*>(txType)) {
                    context.LOG.alert("Forward-declaring function object %s", fieldEntity->get_declaration()->get_unique_full_name().c_str());
                    StructType *lambdaT = cast<StructType>(context.get_llvm_type(txFuncType));
//                    FunctionType *funcT = cast<FunctionType>(cast<PointerType>(lambdaT->getElementType(0))->getPointerElementType());
//                    auto funcName = fieldEntity->get_full_name().to_string() + "$func";
//                    auto funcV = context.llvmModule.getOrInsertFunction(funcName, funcT);
//                    //cast<Function>(funcV)->setLinkage(GlobalValue::InternalLinkage);  TODO (can cause LLVM to rename function)
//                    // construct the lambda object:
//                    auto nullClosureRefV = Constant::getNullValue(lambdaT->getElementType(1));
//                    val = ConstantStruct::get(lambdaT, funcV, nullClosureRefV, NULL);
                    //val = new GlobalVariable(context.llvmModule, lambdaT, true, GlobalValue::InternalLinkage,
                    //                         nullptr, fieldEntity->get_full_name().to_string());
                    val = context.llvmModule.getOrInsertGlobal(fieldEntity->get_declaration()->get_unique_full_name(), lambdaT);
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
            if (! baseExpr) {
                context.LOG.error("Attempted to dereference TXS_INSTANCE field but no base expression provided (identifier %s)", fieldEntity->get_declaration()->get_unique_full_name().c_str());
                return nullptr;
            }
            auto baseValue = baseExpr->code_gen(context, scope);
            if (! baseValue)
                return nullptr;

            auto fieldIx = fieldEntity->get_instance_field_index();
            //std::cerr << "Getting TXS_INSTANCE ix " << fieldIx << " value off LLVM base value: " << baseValue << std::endl;
            Value* ixs[] = { ConstantInt::get(Type::getInt32Ty(context.llvmContext), 0),
                             ConstantInt::get(Type::getInt32Ty(context.llvmContext), fieldIx) };
            if (!scope)
                val = GetElementPtrInst::CreateInBounds(baseValue, ixs);
            else
                val = scope->builder->CreateInBoundsGEP(baseValue, ixs);
        }
        break;

    case TXS_NOSTORAGE:
        context.LOG.warning("TXS_NOSTORAGE specified for field: %s", fieldEntity->get_declaration()->get_unique_full_name().c_str());
        return nullptr;
    }
    return val;
}


Value* TxFieldValueNode::code_gen_address(LlvmGenerationContext& context, GenScope* scope, bool foldStatics) const {
    if (auto field = this->get_field(0))
        return field_value_code_gen(context, scope, this->baseExpr, field, foldStatics);
    else
        return NULL;
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
    //std::cerr << "skipping LOAD for " << value << std::endl;
    return value;
}
