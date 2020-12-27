#include "ast_op_exprs.hpp"
#include "ast_constexpr.hpp"
#include "ast_conv.hpp"

#include "symbol/type_registry.hpp"
#include "tx_error.hpp"
#include "tx_lang_defs.hpp"


void TxBinaryElemOperatorNode::match_binary_operand_types( TxPassInfo passInfo, const TxActualType* ltype, const TxActualType* rtype ) {
    if ( ltype != rtype ) {
        if ( auto_converts_to( this->rhs->originalExpr, ltype ) ) {
            this->rhs->insert_conversion( passInfo, ltype );
            this->lhs->resolve_type( passInfo );
        }
        else if ( auto_converts_to( this->lhs->originalExpr, rtype ) ) {
            this->lhs->insert_conversion( passInfo, rtype );
            this->rhs->resolve_type( passInfo );
        }
        else
            CERR_THROWRES( this, "Mismatching operand types for binary operator " << this->op << ": " << ltype << ", " << rtype );
    }
    else {
        this->lhs->resolve_type( passInfo );
        this->rhs->resolve_type( passInfo );
    }
}

TxQualType TxBinaryElemOperatorNode::define_type( TxPassInfo passInfo ) {
    auto ltype = this->lhs->originalExpr->resolve_type( passInfo ).type();
    auto rtype = this->rhs->originalExpr->resolve_type( passInfo ).type();

    if ( ltype->get_type_class() != TXTC_ELEMENTARY )
        CERR_THROWRES( this, "Left operand of " << this->op << " is not an elementary type: " << ltype );
    if ( rtype->get_type_class() != TXTC_ELEMENTARY )
        CERR_THROWRES( this, "Right operand of " << this->op << " is not an elementary type: " << rtype );

    switch ( this->op_class ) {
    case TXOC_ARITHMETIC:
    case TXOC_COMPARISON:
        if ( !ltype->is_scalar() )
            CERR_THROWRES( this, "Left operand of " << this->op << " is not of scalar type: " << ltype );
        if ( !rtype->is_scalar() )
            CERR_THROWRES( this, "Right operand of " << this->op << " is not of scalar type: " << rtype );

        this->match_binary_operand_types( passInfo, ltype, rtype );
        break;

    case TXOC_LOGICAL:
        if ( !( is_concrete_sinteger_type( ltype ) ||
                is_concrete_uinteger_type( ltype ) ||
                ltype->get_runtime_type_id() == TXBT_BOOL ) )
            CERR_THROWRES( this, "Left operand of " << this->op << " is not of integer or boolean type: " << ltype );
        if ( !( is_concrete_sinteger_type( rtype ) ||
                is_concrete_uinteger_type( rtype ) ||
                rtype->get_runtime_type_id() == TXBT_BOOL ) )
            CERR_THROWRES( this, "Right operand of " << this->op << " is not of integer or boolean type: " << rtype );

        this->match_binary_operand_types( passInfo, ltype, rtype );
        break;

    case TXOC_SHIFT:
        // Note: In LLVM and in common CPUs, for an integer type of N bits, the result of shifting by >= N is undefined.
        if ( !( is_concrete_sinteger_type( ltype ) ||
                is_concrete_uinteger_type( ltype ) ) )
            CERR_THROWRES( this, "Left operand of " << this->op << " is not of integer type: " << ltype );
        if ( !is_concrete_uinteger_type( rtype ) )
            CERR_THROWRES( this, "Right operand of " << this->op << " is not of unsigned integer type: " << rtype );
        this->rhs->insert_conversion( passInfo, ltype );  // LLVM shift instructions require right operand to be same integer type as left one
        this->lhs->resolve_type( passInfo );
        break;

    default:
        THROW_LOGIC( "Invalid/unhandled op-class " << this->op_class << " in " << this );
    }

    if ( this->op_class == TXOC_COMPARISON )
        return TxQualType( this->registry().get_builtin_type( TXBT_BOOL ) );
    else
        return this->lhs->qtype();
}

TxQualType TxUnaryMinusNode::define_type( TxPassInfo passInfo ) {
    auto opType = this->operand->originalExpr->resolve_type( passInfo );
    if ( !opType->is_scalar() )
        CERR_THROWRES( this, "Operand for unary '-' is not of scalar type: " << opType );
    if ( dynamic_cast<TxIntegerLitNode*>( this->operand->originalExpr ) )
        return opType;

    // promote unsigned integers upon negation:
    // (if operand is an unsigned integer, statically constant, and small enough it's converted to signed of same width)
    const TxActualType* opTypeEnt;
    switch ( opType->get_runtime_type_id() ) {
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
    this->operand->insert_conversion( passInfo, opTypeEnt );
    return this->operand->qtype();
}

TxQualType TxUnaryLogicalNotNode::define_type( TxPassInfo passInfo ) {
    return TxQualType( this->registry().get_builtin_type( TXBT_BOOL ) );
}


TxQualType TxEqualityOperatorNode::define_type( TxPassInfo passInfo ) {
    auto ltype = this->lhs->originalExpr->resolve_type( passInfo );
    auto rtype = this->rhs->originalExpr->resolve_type( passInfo );

    if ( ltype->get_type_class() == TXTC_ELEMENTARY && rtype->get_type_class() == TXTC_ELEMENTARY ) {
        if ( ltype != rtype ) {
            if ( auto_converts_to( this->rhs->originalExpr, ltype ) ) {
                this->rhs->insert_conversion( passInfo, ltype.type() );
            }
            else if ( auto_converts_to( this->lhs->originalExpr, rtype ) ) {
                this->lhs->insert_conversion( passInfo, rtype.type() );
            }
            else
                CERR_THROWRES( this, "Equality is always false: Incompatible operand types for equality operator: " << ltype << ", " << rtype );
        }
    }
    this->lhs->resolve_type( passInfo );
    this->rhs->resolve_type( passInfo );

    return TxQualType( this->registry().get_builtin_type( TXBT_BOOL ) );
}

void TxEqualityOperatorNode::verification_pass() const {
    auto ltype = this->lhs->attempt_qtype();
    auto rtype = this->rhs->attempt_qtype();
    if ( !( ltype && rtype ) )
        return;
    if ( ltype->get_type_class() != rtype->get_type_class() )
        CERROR( this, "Equality is always false: Unequal operand type classes for equality operator: " << ltype << ", " << rtype );

// currently we allow the == operator for reference equality (for instance this enables Array.equals() to use == on the elements)
//    if ( ltype->get_type_class() == TXTC_REFERENCE || rtype->get_type_class() == TXTC_REFERENCE )
//        CERR_THROWRES( this, "Value equality operator == not applicable to reference operands: " << ltype << ", " << rtype );

// operands don't have to be concrete - if not then equals() is invoked
//    if ( !ltype->is_concrete() )
//        CERR_THROWRES( this->lhs, "Left operand of equality operator is not concrete (equals() may be used instead): " << ltype );
//    if ( !rtype->is_concrete() )
//        CERR_THROWRES( this->rhs, "Right operand of equality operator is not concrete (equals() may be used instead): " << rtype );

    if ( ltype->get_type_class() == TXTC_ARRAY && rtype->get_type_class() == TXTC_ARRAY) {
        // comparison of length and then of each element
        auto lelemtype = ltype->element_type();
        auto relemtype = rtype->element_type();
        if ( lelemtype->get_type_class() == TXTC_ANY || relemtype->get_type_class() == TXTC_ANY ) {
            // if either operand's element type is 'Any', type checking must be done in runtime
            // Example:
            //     r : &Array;  s : &Array<Int>;
            //     r^ == s^
        }
        else if ( lelemtype->get_type_class() != relemtype->get_type_class() ) {
            CERROR( this, "Equality is always false: Unequal array element types for equality operator: " << lelemtype << ", " << relemtype );
        }
        else if ( lelemtype->get_type_class() == TXTC_ELEMENTARY ) {
            if ( lelemtype->is_concrete() && relemtype->is_concrete() ) {
                if ( lelemtype.type() != relemtype.type() )
                    CERROR( this, "Equality is always false: Unequal array element types for equality operator: " << lelemtype << ", " << relemtype );
            }
        }
        else if ( lelemtype->get_type_class() == TXTC_REFERENCE ) {
            // Since the built-in comparison of arrays of references compares the pointers (identity equality), we don't
            // strictly need to check the ref target types here.
            // However if they are mutually exclusive, the comparison will always yield false.
            // The element types are mutually exclusive if neither type "is-a" the other type.
            if ( !( lelemtype->target_type()->is_a( *relemtype->target_type() ) ||
                    relemtype->target_type()->is_a( *lelemtype->target_type() ) ) )
                CERROR( this, "Equality is always false: Unequal array element ref target types for equality operator: "
                        << lelemtype << ", " << relemtype );
        }
    }
}

bool TxEqualityOperatorNode::is_statically_constant() const {
    auto tc = this->lhs->originalExpr->qtype()->get_type_class();
    return this->lhs->is_statically_constant() && this->rhs->is_statically_constant()
            && ( tc == TXTC_ELEMENTARY || tc == TXTC_REFERENCE || tc == TXTC_FUNCTION || tc == TXTC_ARRAY );
}

void TxRefEqualityOperatorNode::verification_pass() const {
    auto ltype = this->lhs->qtype();
    auto rtype = this->rhs->qtype();
    if ( ltype->get_type_class() != TXTC_REFERENCE )
        CERROR( this, "Left operand for identity equality operator is not a reference: " << ltype );
    if ( rtype->get_type_class() != TXTC_REFERENCE )
        CERROR( this, "Right operand for identity equality operator is not a reference: " << rtype );
}

TxQualType TxRefEqualityOperatorNode::define_type( TxPassInfo passInfo ) {
    return TxQualType( this->registry().get_builtin_type( TXBT_BOOL ) );
}
