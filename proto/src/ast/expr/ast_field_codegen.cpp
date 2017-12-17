#include "ast_field.hpp"
#include "ast_ref.hpp"
#include "ast_lambda_node.hpp"
#include "llvm_generator.hpp"

using namespace llvm;

/** Gets a virtual field value via the vtable.
 * Note that this returns a value of pointer type for all virtual fields, except for $adTypeId which has type i32.
 */
static Value* virtual_field_addr_code_gen( LlvmGenerationContext& context, GenScope* scope,
                                           const TxActualType* staticBaseType, Value* runtimeBaseTypeIdV, const std::string& fieldName ) {
    // retrieve the vtable of the base's actual (runtime) type:
    Value* vtableBase = context.gen_get_vtable( scope, staticBaseType, runtimeBaseTypeIdV );
    if ( !vtableBase ) {
        THROW_LOGIC( "No vtable obtained for " << staticBaseType );
    }

    // get the virtual field:
    uint32_t fieldIx = staticBaseType->get_virtual_fields().get_field_index( fieldName );
    //std::cerr << "(static type id " << staticBaseType->get_runtime_type_id() << ") Getting TXS_VIRTUAL ix " << fieldIx << " value off LLVM base value: " << vtableBase << std::endl;
    Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                     ConstantInt::get( Type::getInt32Ty( context.llvmContext ), fieldIx ) };
    Value* fieldPtr = scope->builder->CreateInBoundsGEP( vtableBase, ixs );
    // vtable stores pointers to globals/statics (except for $adTypeId) so we dereference one step:
    Value* fieldV = scope->builder->CreateLoad( fieldPtr );
//    std::cerr << "runtimeBaseTypeIdV isconst: " << bool(dyn_cast<Constant>(runtimeBaseTypeIdV))
//        << "   vtableBase isconst: " << bool(dyn_cast<Constant>(vtableBase))
//        << "   fieldPtr isconst: " << bool(dyn_cast<Constant>(fieldPtr))
//        << "   fieldV isconst: " << bool(dyn_cast<Constant>(fieldV)) << std::endl;
    return fieldV;
}

/** Returns an instance method lambda object value. */
Value* instance_method_value_code_gen( LlvmGenerationContext& context, GenScope* scope,
                                       const TxActualType* staticBaseType, Value* runtimeBaseTypeIdV, Value* basePtrV,
                                       const TxActualType* fieldType, const std::string& fieldName,
                                       bool nonvirtualLookup ) {
    auto lambdaT = cast<StructType>( context.get_llvm_type( fieldType ) );
    Value* funcPtrV;
    if ( nonvirtualLookup ) {
        Value* staticBaseTypeIdV = staticBaseType->gen_typeid( context );
        funcPtrV = virtual_field_addr_code_gen( context, scope, staticBaseType, staticBaseTypeIdV, fieldName );
    }
    else
        funcPtrV = virtual_field_addr_code_gen( context, scope, staticBaseType, runtimeBaseTypeIdV, fieldName );
    // cast pointer type (necessary for certain (e.g. Ref-binding) specializations' methods):
    funcPtrV = scope->builder->CreatePointerCast( funcPtrV, lambdaT->getElementType( 0 ) );
    ASSERT( funcPtrV->getType()->getPointerElementType()->isFunctionTy(),
            "Expected funcPtrV to be pointer-to-function type but was: " << funcPtrV->getType() );
    ASSERT( basePtrV->getType()->isPointerTy(), "Expected basePtrV to be of pointer type but was: " << basePtrV->getType() );

    // construct the lambda object:
    auto closureRefT = context.get_closureRefT();
    if ( staticBaseType->get_type_class() == TXTC_INTERFACE ) {
        // if base is an interface (in practice, interface adapter), populate the lambda with the adaptee type id instead
        //auto adapteeTypeIdField = staticBaseType->get_virtual_fields().get_field( "$adTypeId" );
        auto adapteeTypeIdV = virtual_field_addr_code_gen( context, scope, staticBaseType, runtimeBaseTypeIdV, "$adTypeId" );
        //std::cerr << "Invoking interface method " << fieldEntity << ", replacing type id " << runtimeBaseTypeIdV << " with " << adapteeTypeIdV << std::endl;
        runtimeBaseTypeIdV = adapteeTypeIdV;
    }
    auto closureRefV = gen_ref( context, scope, closureRefT, basePtrV, runtimeBaseTypeIdV );
    return gen_lambda( context, scope, lambdaT, funcPtrV, closureRefV );
}

static bool is_non_virtual_lookup( const TxExpressionNode* baseExpr ) {
    if ( auto fieldValueNode = dynamic_cast<const TxFieldValueNode*>( baseExpr ) ) {
        if ( fieldValueNode->get_full_identifier() == "super" )
            return true;  // member invocation via super keyword is non-virtual
    }
    else if ( auto derefNode = dynamic_cast<const TxReferenceDerefNode*>( baseExpr ) ) {
        return is_non_virtual_lookup( derefNode->reference );
    }
    return false;
    // FIXME return baseExpr->qtype()->is_leaf_derivation();
}


Value* TxFieldValueNode::code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const {
    // Generate code to obtain field value, potentially based on a base value (pointer).
    ASSERT( !( _field->get_decl_flags() & TXD_CONSTRUCTOR ), "Can't get 'field value' of constructor " << _field );

    switch ( _field->get_storage() ) {
    case TXS_VIRTUAL:
        if ( auto baseValExpr = dynamic_cast<TxExpressionNode*>( this->baseExpr ) ) {
            // virtual lookup will effectively be a polymorphic lookup if base expression is a reference dereference
            bool nonvirtualLookup = is_non_virtual_lookup( baseValExpr );  // true for super.foo lookups
            auto baseType = baseValExpr->qtype().type();
            Value* baseTypeIdV = nonvirtualLookup ? baseType->gen_typeid( context )  // static
                                                  : baseValExpr->code_gen_typeid( context, scope );  // runtime (static unless reference)
            return virtual_field_addr_code_gen( context, scope, baseType, baseTypeIdV, _field->get_unique_name() );
        }
        else {
            // TODO: allow un-based access to virtual (non-instance) methods within same type?
            CERR_CODECHECK( this, "Can't access virtual field without base value/expression: " << _field );
        }
        break;

    case TXS_INSTANCE:
        if ( auto baseValExpr = dynamic_cast<TxExpressionNode*>( this->baseExpr ) ) {
            auto baseValue = baseValExpr->code_gen_dyn_address( context, scope );
            auto staticBaseType = baseValExpr->qtype().type();
            uint32_t fieldIx = staticBaseType->get_instance_fields().get_field_index( _field->get_unique_name() );
            ASSERT( fieldIx != UINT32_MAX, "Unknown field index for field " << _field->get_unique_name() << " in " << staticBaseType );
            //std::cerr << "Getting TXS_INSTANCE ix " << fieldIx << " value off LLVM base value: " << baseValue << std::endl;
            Value* ixs[] = { ConstantInt::get( Type::getInt32Ty( context.llvmContext ), 0 ),
                             ConstantInt::get( Type::getInt32Ty( context.llvmContext ), fieldIx ) };
            return scope->builder->CreateInBoundsGEP( baseValue, ixs );
        }
        else if ( !( _field->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER | TXD_GENPARAM | TXD_GENBINDING ) ) ) {
            THROW_LOGIC( "Can't access instance field without base value/expression: " << _field );
        }
        // no break

    case TXS_STATIC:
    case TXS_GLOBAL:
        {
            Value* val = _field->code_gen_field_decl( context );
            return val;
        }

    case TXS_STACK:
        return _field->get_llvm_value();

    default:
        THROW_LOGIC( "Can't generate address for field with storage type " << _field->get_storage() << ": " << _field );
    }
}

Value* TxFieldValueNode::code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );

    if ( this->_field->get_storage() == TXS_INSTANCEMETHOD ) {
        if ( auto baseValExpr = dynamic_cast<TxExpressionNode*>( this->baseExpr ) ) {
            // virtual lookup will effectively be a polymorphic lookup if base expression is a reference dereference, and not 'super'
            // TODO: leaf type accesses are also non-virtual
            bool nonvirtualLookup = is_non_virtual_lookup( baseValExpr );  // true for super.foo lookups
            Value* runtimeBaseTypeIdV = baseValExpr->code_gen_typeid( context, scope );  // (static unless reference)
            Value* basePtrV = baseValExpr->code_gen_dyn_address( context, scope );  // expected to be of pointer type
            return instance_method_value_code_gen( context, scope, baseValExpr->qtype().type(), runtimeBaseTypeIdV, basePtrV,
                                                   this->_field->qtype().type(), this->_field->get_unique_name(),
                                                   nonvirtualLookup );
        }
        else {
            THROW_LOGIC( "Can't access instance method without base value/expression: " << this->_field );
        }
    }

    Value* valuePtr = this->code_gen_dyn_address( context, scope );
    return scope->builder->CreateLoad( valuePtr );
}

Constant* TxFieldValueNode::code_gen_const_address( LlvmGenerationContext& context ) const {
    switch ( this->_field->get_storage() ) {
    case TXS_STATIC:
    case TXS_GLOBAL:
    {
        Constant* fieldC = cast<Constant>( this->_field->code_gen_field_decl( context ) );
        return fieldC;
    }
    default:
        THROW_LOGIC( "code_gen_addr_constant() not supported for " << this << " with storage=" << this->_field->get_storage() );
    }
}

Constant* TxFieldValueNode::code_gen_const_value( LlvmGenerationContext& context ) const {
    TRACE_CODEGEN( this, context );

    if ( this->_field->get_declaration()->get_definer()->get_init_expression() ) {
        return static_cast<TxFieldDefiningNode*>( this->_field->get_declaration()->get_definer() )->code_gen_const_init_value( context );
    }
    else if ( this->_field->get_storage() == TXS_INSTANCE ) {
        if ( auto baseValExpr = dynamic_cast<TxExpressionNode*>( this->baseExpr ) ) {
            auto baseObjC = baseValExpr->code_gen_const_value( context );
            auto staticBaseType = baseValExpr->qtype().type();
            uint32_t fieldIx = staticBaseType->get_instance_fields().get_field_index( this->_field->get_unique_name() );
            ASSERT( fieldIx != UINT32_MAX, "Unknown field index for field " << this->_field->get_unique_name() << " in " << staticBaseType );
            //std::cerr << "Getting TXS_INSTANCE ix " << fieldIx << " value off LLVM base value: " << baseValue << std::endl;
            return baseObjC->getAggregateElement( fieldIx );
        }
    }
    // FUTURE: support getting instance method lambda object of statically constant objects
    THROW_LOGIC( "TxFieldValueNode::code_gen_const_value() not supported for " << this );
}


Value* TxFieldAssigneeNode::code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
    TRACE_CODEGEN( this, context );
    return this->fieldNode->code_gen_dyn_address( context, scope );
}
