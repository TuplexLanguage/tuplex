#include "ast_field.hpp"
#include "ast_ref.hpp"
#include "ast_lambda_node.hpp"
#include "llvm_generator.hpp"

using namespace llvm;

/** Gets a virtual field value via the vtable.
 * Note that this returns a value of pointer type for all virtual fields, except for $adTypeId which has type i32.
 */
static Value* virtual_field_addr_code_gen( LlvmGenerationContext& context, GenScope* scope,
                                           const TxActualType* staticBaseType, Value* runtimeBaseTypeIdV, const TxField* fieldEntity ) {
    // retrieve the vtable of the base's actual (runtime) type:
    Value* vtableBase = context.gen_get_vtable( scope, staticBaseType, runtimeBaseTypeIdV );
    if ( !vtableBase ) {
        THROW_LOGIC( "No vtable obtained for " << staticBaseType );
    }

    // get the virtual field:
    uint32_t fieldIx = staticBaseType->get_virtual_fields().get_field_index( fieldEntity->get_unique_name() );
    //std::cerr << "(static type id " << staticBaseType->get_type_id() << ") Getting TXS_VIRTUAL ix " << fieldIx << " value off LLVM base value: " << vtableBase << std::endl;
    Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                     ConstantInt::get( Type::getInt32Ty( context.llvmContext ), fieldIx ) };
    Value* fieldPtr = scope->builder->CreateInBoundsGEP( vtableBase, ixs );
    // vtable stores pointers to globals/statics (except for $adTypeId) so we dereference one step:
    Value* fieldV = scope->builder->CreateLoad( fieldPtr );
    return fieldV;
}

/** Returns an instance method lambda object value. */
static Value* instance_method_value_code_gen( LlvmGenerationContext& context, GenScope* scope,
                                              const TxActualType* staticBaseType,
                                              Value* runtimeBaseTypeIdV, const TxField* fieldEntity,
                                              Value* baseValue,
                                              bool nonvirtualLookup ) {
    auto lambdaT = cast<StructType>( context.get_llvm_type( fieldEntity->get_type() ) );
    Value* funcPtrV;
    if ( nonvirtualLookup ) {
        Value* staticBaseTypeIdV = staticBaseType->gen_typeid( context, scope );
        funcPtrV = virtual_field_addr_code_gen( context, scope, staticBaseType, staticBaseTypeIdV, fieldEntity );
    }
    else
        funcPtrV = virtual_field_addr_code_gen( context, scope, staticBaseType, runtimeBaseTypeIdV, fieldEntity );
    // cast pointer type (necessary for certain (e.g. Ref-binding) specializations' methods):
    funcPtrV = scope->builder->CreatePointerCast( funcPtrV, lambdaT->getElementType( 0 ) );
    ASSERT( funcPtrV->getType()->getPointerElementType()->isFunctionTy(),
            "Expected funcPtrV to be pointer-to-function type but was: " << funcPtrV->getType() );
    ASSERT( baseValue->getType()->isPointerTy(), "Expected baseValue to be of pointer type but was: " << baseValue->getType() );

    // construct the lambda object:
    auto closureRefT = context.get_voidRefT();
    if ( staticBaseType->get_type_class() == TXTC_INTERFACE ) {
        // if base is an interface (in practice, interface adapter), populate the lambda with the adaptee type id instead
        auto adapteeTypeIdField = staticBaseType->get_virtual_fields().get_field( "$adTypeId" );
        auto adapteeTypeIdV = virtual_field_addr_code_gen( context, scope, staticBaseType, runtimeBaseTypeIdV, adapteeTypeIdField );
        //std::cerr << "Invoking interface method " << fieldEntity << ", replacing type id " << runtimeBaseTypeIdV << " with " << adapteeTypeIdV << std::endl;
        runtimeBaseTypeIdV = adapteeTypeIdV;
    }
    auto closureRefV = gen_ref( context, scope, closureRefT, baseValue, runtimeBaseTypeIdV );
    return gen_lambda( context, scope, lambdaT, funcPtrV, closureRefV );
}

static bool is_non_virtual_lookup( const TxExpressionNode* baseExpr ) {
    if ( auto fieldValueNode = dynamic_cast<const TxFieldValueNode*>( baseExpr ) )
        return ( fieldValueNode->get_full_identifier() == "super" );  // member invocation via super keyword is non-virtual
    else if ( auto derefNode = dynamic_cast<const TxReferenceDerefNode*>( baseExpr ) )
        return is_non_virtual_lookup( derefNode->reference );
    else
        return false;
}

/** Generate code to obtain field value, potentially based on a base value (pointer). */
static Value* field_addr_code_gen( LlvmGenerationContext& context, GenScope* scope,
                                   const TxExpressionNode* baseExpr, const TxField* fieldEntity ) {
    ASSERT( !( fieldEntity->get_decl_flags() & TXD_CONSTRUCTOR ), "Can't get 'field value' of constructor " << fieldEntity );

    switch ( fieldEntity->get_storage() ) {
    case TXS_VIRTUAL:
        if ( baseExpr ) {
            // virtual lookup will effectively be a polymorphic lookup if base expression is a reference dereference
            bool nonvirtualLookup = is_non_virtual_lookup( baseExpr );  // true for super.foo lookups
            auto baseType = baseExpr->qualtype()->type()->acttype();
            Value* baseTypeIdV = nonvirtualLookup ? baseType->gen_typeid( context, scope )  // static
                                                  : baseExpr->code_gen_typeid( context, scope );  // runtime (static unless reference)
            return virtual_field_addr_code_gen( context, scope, baseType, baseTypeIdV, fieldEntity );
        }
        else {
            THROW_LOGIC( "Can't access virtual field without base value/expression: " << fieldEntity );
        }
        break;

    case TXS_STATIC:
    case TXS_GLOBAL:
        {
            Value* val = fieldEntity->get_llvm_value();
            if ( !val ) {
                // forward declaration situation
                Type *fieldT = context.get_llvm_type( fieldEntity->get_type() );
                if ( ( fieldEntity->get_decl_flags() & TXD_EXTERNC ) && fieldEntity->get_type()->get_type_class() != TXTC_FUNCTION ) {
                    LOG_NOTE( context.LOGGER(), "Forward-declaring extern-C field alias " << fieldEntity->get_unique_full_name() );
                    val = GlobalAlias::create( fieldT->getPointerTo(), 0, GlobalValue::InternalLinkage, fieldEntity->get_unique_full_name(),
                                               &context.llvmModule() );
                }
                else {
                    LOG_NOTE( context.LOGGER(), "Forward-declaring field " << fieldEntity->get_unique_full_name() );
                    val = context.llvmModule().getOrInsertGlobal( fieldEntity->get_unique_full_name(), fieldT );
                }
            }
            return val;
        }

    case TXS_INSTANCE:
        if ( baseExpr ) {
            auto baseValue = baseExpr->code_gen_dyn_address( context, scope );
            auto staticBaseType = baseExpr->qualtype()->type()->acttype();
            uint32_t fieldIx = staticBaseType->get_instance_fields().get_field_index( fieldEntity->get_unique_name() );
            ASSERT( fieldIx != UINT32_MAX, "Unknown field index for field " << fieldEntity->get_unique_name() << " in " << staticBaseType );
            //std::cerr << "Getting TXS_INSTANCE ix " << fieldIx << " value off LLVM base value: " << baseValue << std::endl;
            Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                             ConstantInt::get( Type::getInt32Ty( context.llvmContext ), fieldIx ) };
            return scope->builder->CreateInBoundsGEP( baseValue, ixs );
        }
        else {
            THROW_LOGIC( "Can't access instance field without base value/expression: " << fieldEntity );
        }
        break;

    case TXS_STACK:
        return fieldEntity->get_llvm_value();

    default:
        THROW_LOGIC( "Can't generate address for field with storage type " << fieldEntity->get_storage() << ": " << fieldEntity );
    }
}


Value* TxFieldValueNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    return field_addr_code_gen( context, scope, this->baseExpr, this->field );
}

Value* TxFieldValueNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );

    if ( this->field->get_storage() == TXS_INSTANCEMETHOD ) {
        if ( baseExpr ) {
            // virtual lookup will effectively be a polymorphic lookup if base expression is a reference dereference, and not 'super'
            bool nonvirtualLookup = is_non_virtual_lookup( baseExpr );  // true for super.foo lookups
            Value* runtimeBaseTypeIdV = baseExpr->code_gen_typeid( context, scope );  // (static unless reference)
            Value* baseValue = baseExpr->code_gen_dyn_address( context, scope );  // expected to be of pointer type
            return instance_method_value_code_gen( context, scope, baseExpr->qualtype()->type()->acttype(), runtimeBaseTypeIdV, this->field, baseValue,
                                                   nonvirtualLookup );
        }
        else {
            THROW_LOGIC( "Can't access instance method without base value/expression: " << this->field );
        }
    }

    Value* valuePtr = this->code_gen_dyn_address( context, scope );
    return scope->builder->CreateLoad( valuePtr );
}

Constant* TxFieldValueNode::code_gen_const_address( LlvmGenerationContext& context ) const {
    switch ( this->field->get_storage() ) {
    case TXS_STATIC:
    case TXS_GLOBAL:
    {
        Constant* fieldC = cast<Constant>( this->field->get_llvm_value() );
        if ( !fieldC ) {
            // forward declaration situation
            LOG_NOTE( context.LOGGER(), "Forward-declaring field " << this->field->get_declaration()->get_unique_full_name() );
            Type *fieldT = context.get_llvm_type( this->field->get_type() );
            fieldC = context.llvmModule().getOrInsertGlobal( this->field->get_declaration()->get_unique_full_name(), fieldT );
        }
        return fieldC;
    }
    default:
        THROW_LOGIC( "code_gen_addr_constant() not supported for " << this << " with storage=" << this->field->get_storage() );
    }
}

Constant* TxFieldValueNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );

    if ( this->field->get_declaration()->get_definer()->get_init_expression() ) {
        return static_cast<TxFieldDefNode*>( this->field->get_declaration()->get_definer() )->code_gen_const_init_value( context );
    }
    else if ( this->field->get_storage() == TXS_INSTANCE ) {
        auto baseObjC = this->baseExpr->code_gen_const_value( context );
        auto staticBaseType = baseExpr->qualtype()->type()->acttype();
        uint32_t fieldIx = staticBaseType->get_instance_fields().get_field_index( this->field->get_unique_name() );
        ASSERT( fieldIx != UINT32_MAX, "Unknown field index for field " << this->field->get_unique_name() << " in " << staticBaseType );
        //std::cerr << "Getting TXS_INSTANCE ix " << fieldIx << " value off LLVM base value: " << baseValue << std::endl;
        return baseObjC->getAggregateElement( fieldIx );
    }
    // FUTURE: support getting instance method lambda object of statically constant objects
    THROW_LOGIC( "TxFieldValueNode::code_gen_const_value() not supported for " << this );
}


Value* TxFieldAssigneeNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->field->code_gen_dyn_address( context, scope );
}
