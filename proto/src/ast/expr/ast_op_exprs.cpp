#include "ast_op_exprs.hpp"
#include "ast_constexpr.hpp"
#include "ast_conv.hpp"

#include "symbol/type_registry.hpp"
#include "tx_error.hpp"
#include "tx_lang_defs.hpp"


static inline void match_binary_operand_types( TxBinaryOperatorNode* binOpNode, const TxType* ltype, const TxType* rtype ) {
    if ( ltype != rtype ) {
        if ( auto_converts_to( binOpNode->rhs->originalExpr, ltype ) ) {
            binOpNode->rhs->insert_conversion( ltype );
        }
        else if ( auto_converts_to( binOpNode->lhs->originalExpr, rtype ) ) {
            binOpNode->lhs->insert_conversion( rtype );
        }
        else
            CERR_THROWRES( binOpNode, "Mismatching operand types for binary operator " << binOpNode->op << ": " << ltype << ", " << rtype );
    }
}

const TxQualType* TxBinaryOperatorNode::define_type() {
    auto ltype = this->lhs->originalExpr->resolve_type()->type();
    auto rtype = this->rhs->originalExpr->resolve_type()->type();

    switch ( this->op_class ) {
    case TXOC_ARITHMETIC:
    case TXOC_COMPARISON:
        if ( !ltype->is_scalar() )
            CERR_THROWRES( this, "Left operand of " << this->op << " is not of scalar type: " << ltype );
        if ( !rtype->is_scalar() )
            CERR_THROWRES( this, "Right operand of " << this->op << " is not of scalar type: " << rtype );

        match_binary_operand_types( this, ltype, rtype );
        break;

    case TXOC_LOGICAL:
        if ( !( is_concrete_sinteger_type( (BuiltinTypeId)ltype->get_type_id() ) ||
                is_concrete_uinteger_type( (BuiltinTypeId)ltype->get_type_id() ) ||
                ltype->get_type_id() == TXBT_BOOL ) )
            CERR_THROWRES( this, "Left operand of " << this->op << " is not of integer or boolean type: " << ltype );
        if ( !( is_concrete_sinteger_type( (BuiltinTypeId)rtype->get_type_id() ) ||
                is_concrete_uinteger_type( (BuiltinTypeId)rtype->get_type_id() ) ||
                rtype->get_type_id() == TXBT_BOOL ) )
            CERR_THROWRES( this, "Right operand of " << this->op << " is not of integer or boolean type: " << rtype );

        match_binary_operand_types( this, ltype, rtype );
        break;

    case TXOC_SHIFT:
        // Note: In LLVM and in common CPUs, for an integer type of N bits, the result of shifting by >= N is undefined.
        if ( !( is_concrete_sinteger_type( (BuiltinTypeId)ltype->get_type_id() ) ||
                is_concrete_uinteger_type( (BuiltinTypeId)ltype->get_type_id() ) ) )
            CERR_THROWRES( this, "Left operand of " << this->op << " is not of integer type: " << ltype );
        if ( !is_concrete_uinteger_type( (BuiltinTypeId)rtype->get_type_id() ) )
            CERR_THROWRES( this, "Right operand of " << this->op << " is not of unsigned integer type: " << rtype );
        this->rhs->insert_conversion( ltype );  // LLVM shift instructions require right operand to be same integer type as left one
        break;

    case TXOC_EQUALITY:
        if ( ltype->get_type_class() != rtype->get_type_class() )
            CERR_THROWRES( this, "Mismatching operand types for binary operator " << this->op << ": " << ltype << ", " << rtype );
        if ( ltype->get_type_class() == TXTC_ELEMENTARY ) {
            match_binary_operand_types( this, ltype, rtype );
        }
        break;

    default:
        THROW_LOGIC( "Invalid/unhandled op-class " << this->op_class << " in " << this );
    }

    this->lhs->resolve_type();
    this->rhs->resolve_type();

    if ( this->op_class == TXOC_EQUALITY || this->op_class == TXOC_COMPARISON )
        return new TxQualType( this->registry().get_builtin_type( TXBT_BOOL ) );
    else
        return this->lhs->qualtype();
}

const TxQualType* TxUnaryMinusNode::define_type() {
    auto opType = this->operand->originalExpr->resolve_type();
    if ( !opType->type()->is_scalar() )
        CERR_THROWRES( this, "Operand for unary '-' is not of scalar type: " << opType );
    if ( dynamic_cast<TxIntegerLitNode*>( this->operand->originalExpr ) )
        return opType;

    // promote unsigned integers upon negation:
    // (if operand is an unsigned integer, statically constant, and small enough it's converted to signed of same width)
    const TxType* opTypeEnt;
    switch ( opType->get_type_id() ) {
    case TXBT_UBYTE:
        if ( this->operand->is_statically_constant() && eval_unsigned_int_constant( this->operand ) <= 127 )
            opTypeEnt = this->registry().get_builtin_type( TXBT_BYTE );
        else
            opTypeEnt = this->registry().get_builtin_type( TXBT_SHORT );
        break;
    case TXBT_USHORT:
        if ( this->operand->is_statically_constant() && eval_unsigned_int_constant( this->operand ) <= 32767 )
            opTypeEnt = this->registry().get_builtin_type( TXBT_SHORT );
        else
            opTypeEnt = this->registry().get_builtin_type( TXBT_INT );
        break;
    case TXBT_UINT:
        if ( this->operand->is_statically_constant() && eval_unsigned_int_constant( this->operand ) <= 2147483647 )
            opTypeEnt = this->registry().get_builtin_type( TXBT_INT );
        else
            opTypeEnt = this->registry().get_builtin_type( TXBT_LONG );
        break;
    case TXBT_ULONG:
        if ( this->operand->is_statically_constant() && eval_unsigned_int_constant( this->operand ) <= 9223372036854775807 )
            opTypeEnt = this->registry().get_builtin_type( TXBT_LONG );
        else
            CERR_THROWRES( this, "ULong can't be safely negated with unary '-' (requires explicit cast to signed type)" );
        break;
    default:
        return opType;
    }
    this->operand->insert_conversion( opTypeEnt );
    return this->operand->resolve_type();
}

const TxQualType* TxUnaryLogicalNotNode::define_type() {
    return new TxQualType( this->registry().get_builtin_type( TXBT_BOOL ) );
}
