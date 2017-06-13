#include "ast_op_exprs.hpp"
#include "ast_constexpr.hpp"

#include "symbol/type_registry.hpp"


const TxType* TxBinaryOperatorNode::define_type() {
    auto ltype = this->lhs->originalExpr->resolve_type();
    auto rtype = this->rhs->originalExpr->resolve_type();

    const TxType* arithResultType = nullptr;
    if ( ltype->is_scalar() ) {
        if ( rtype->is_scalar() ) {
            if ( ltype != rtype ) {
                if ( auto_converts_to( rhs->originalExpr, ltype ) ) {
                    this->rhs->insert_conversion( ltype );
                    arithResultType = this->rhs->resolve_type();
                }
                else if ( auto_converts_to( lhs->originalExpr, rtype ) ) {
                    this->lhs->insert_conversion( rtype );
                    arithResultType = this->lhs->resolve_type();
                }
            }
            else
                // same type, no additional action necessary
                arithResultType = ltype;
        }
        if ( arithResultType ) {
            if ( op_class == TXOC_BOOLEAN )
                CERROR( this, "Can't perform boolean operation on operands of scalar type: " << ltype );
        }
        else
            CERR_THROWRES( this, "Mismatching scalar operand types for binary operator " << this->op << ": " << ltype << ", " << rtype );
    }
    else if ( ltype->is_builtin( TXBT_BOOL ) ) {
        if ( rtype->is_builtin( TXBT_BOOL ) ) {
            if ( op_class == TXOC_ARITHMETIC )
                CERROR( this, "Can't perform arithmetic operation on operands of boolean type: " << this->op );
        }
        else
            CERROR( this, "Mismatching operand types for binary operator " << this->op << ": " << ltype << ", " << rtype );
    }
    else if ( ltype->get_type_class() == TXTC_REFERENCE ) {
        if ( rtype->get_type_class() == TXTC_REFERENCE ) {
            if ( op_class != TXOC_EQUALITY )
                CERROR( this, "Invalid operator for reference operands: " << this->op );
        }
        else
            CERROR( this, "Mismatching operand types for binary operator " << this->op << ": " << ltype << ", " << rtype );
    }
    else
        CERR_THROWRES( this, "Unsupported operand types for binary operator " << this->op << ": " << ltype << ", " << rtype );

    if ( this->op_class == TXOC_ARITHMETIC ) {
        // Note: After analyzing conversions, the lhs will hold the proper resulting type.
        if ( !arithResultType )
            throw resolution_error( this, "Mismatching arithmetic binary operand types" );
        return arithResultType;
    }
    else {  // TXOC_EQUALITY, TXOC_COMPARISON, TXOC_BOOLEAN
        return this->registry().get_builtin_type( TXBT_BOOL );
    }
}

const TxType* TxUnaryMinusNode::define_type() {
    auto opType = this->operand->originalExpr->resolve_type();
    if ( !opType->is_scalar() )
        CERR_THROWRES( this, "Operand for unary '-' is not of scalar type: " << opType );
    if ( dynamic_cast<TxIntegerLitNode*>( this->operand->originalExpr ) )
        return opType;

    // promote unsigned integers upon negation:
    // (if operand is an unsigned integer, statically constant, and small enough it's converted to signed of same width)
    switch ( opType->get_type_id() ) {
    case TXBT_UBYTE:
        if ( this->operand->is_statically_constant() && eval_unsigned_int_constant( this->operand ) <= 127 )
            opType = this->registry().get_builtin_type( TXBT_BYTE );
        else
            opType = this->registry().get_builtin_type( TXBT_SHORT );
        break;
    case TXBT_USHORT:
        if ( this->operand->is_statically_constant() && eval_unsigned_int_constant( this->operand ) <= 32767 )
            opType = this->registry().get_builtin_type( TXBT_SHORT );
        else
            opType = this->registry().get_builtin_type( TXBT_INT );
        break;
    case TXBT_UINT:
        if ( this->operand->is_statically_constant() && eval_unsigned_int_constant( this->operand ) <= 2147483647 )
            opType = this->registry().get_builtin_type( TXBT_INT );
        else
            opType = this->registry().get_builtin_type( TXBT_LONG );
        break;
    case TXBT_ULONG:
        if ( this->operand->is_statically_constant() && eval_unsigned_int_constant( this->operand ) <= 9223372036854775807 )
            opType = this->registry().get_builtin_type( TXBT_LONG );
        else
            CERR_THROWRES( this, "ULong can't be safely negated with unary '-' (requires explicit cast to signed type)" );
        break;
    default:
        return opType;
    }
    this->operand->insert_conversion( opType );
    return this->operand->resolve_type();
}

const TxType* TxUnaryLogicalNotNode::define_type() {
    return this->registry().get_builtin_type( TXBT_BOOL );
}
