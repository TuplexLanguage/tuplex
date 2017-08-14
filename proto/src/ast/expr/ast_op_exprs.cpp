#include "ast_op_exprs.hpp"
#include "ast_constexpr.hpp"
#include "ast_conv.hpp"

#include "symbol/type_registry.hpp"
#include "tx_error.hpp"
#include "tx_lang_defs.hpp"


static inline void match_binary_operand_types( TxBinaryElemOperatorNode* binOpNode, const TxType* ltype, const TxType* rtype ) {
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

const TxQualType* TxBinaryElemOperatorNode::define_type() {
    auto ltype = this->lhs->originalExpr->resolve_type()->type();
    auto rtype = this->rhs->originalExpr->resolve_type()->type();

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

        match_binary_operand_types( this, ltype, rtype );
        break;

    case TXOC_LOGICAL:
        if ( !( is_concrete_sinteger_type( (BuiltinTypeId)ltype->get_formal_type_id() ) ||
                is_concrete_uinteger_type( (BuiltinTypeId)ltype->get_formal_type_id() ) ||
                ltype->get_formal_type_id() == TXBT_BOOL ) )
            CERR_THROWRES( this, "Left operand of " << this->op << " is not of integer or boolean type: " << ltype );
        if ( !( is_concrete_sinteger_type( (BuiltinTypeId)rtype->get_formal_type_id() ) ||
                is_concrete_uinteger_type( (BuiltinTypeId)rtype->get_formal_type_id() ) ||
                rtype->get_formal_type_id() == TXBT_BOOL ) )
            CERR_THROWRES( this, "Right operand of " << this->op << " is not of integer or boolean type: " << rtype );

        match_binary_operand_types( this, ltype, rtype );
        break;

    case TXOC_SHIFT:
        // Note: In LLVM and in common CPUs, for an integer type of N bits, the result of shifting by >= N is undefined.
        if ( !( is_concrete_sinteger_type( (BuiltinTypeId)ltype->get_formal_type_id() ) ||
                is_concrete_uinteger_type( (BuiltinTypeId)ltype->get_formal_type_id() ) ) )
            CERR_THROWRES( this, "Left operand of " << this->op << " is not of integer type: " << ltype );
        if ( !is_concrete_uinteger_type( (BuiltinTypeId)rtype->get_formal_type_id() ) )
            CERR_THROWRES( this, "Right operand of " << this->op << " is not of unsigned integer type: " << rtype );
        this->rhs->insert_conversion( ltype );  // LLVM shift instructions require right operand to be same integer type as left one
        break;

    default:
        THROW_LOGIC( "Invalid/unhandled op-class " << this->op_class << " in " << this );
    }

    this->lhs->resolve_type();
    this->rhs->resolve_type();

    if ( this->op_class == TXOC_COMPARISON )
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


const TxQualType* TxEqualityOperatorNode::define_type() {
    auto ltype = this->lhs->originalExpr->resolve_type()->type();
    auto rtype = this->rhs->originalExpr->resolve_type()->type();

    // TODO: auto-dereference reference operands? (maybe required if comparison of arrays of references is to work)
    // (References' pointer values are compared using the === identity equality operator.)

    // Object type classes may be compared directly via this operation.
    // Note that they don't have to be concrete.
    // For non-object type classes, the equals() method is invoked instead, e.g:
    //     r : &SomeInterface;  s : &SomeInterface;
    //     r.equals( s )


    if ( ltype->get_type_class() == TXTC_REFERENCE || rtype->get_type_class() == TXTC_REFERENCE )
        CERR_THROWRES( this, "Value equality operator == not applicable to reference operands: " << ltype << ", " << rtype );
        //CERR_THROWRES( this, "Mismatching operand type classes for equality operator: " << ltype << ", " << rtype );
    // TODO: Review how to handle e.g.
    //     r : &Any;  s : &Array<Int>;
    //     r^ == s^
    //     x : &Number;  y : Float;
    //     x^ == y

    switch ( ltype->get_type_class() ) {
    case TXTC_ELEMENTARY:
        if ( ltype != rtype ) {
            if ( auto_converts_to( this->rhs->originalExpr, ltype ) ) {
                this->rhs->insert_conversion( ltype );
            }
            else if ( auto_converts_to( this->lhs->originalExpr, rtype ) ) {
                this->lhs->insert_conversion( rtype );
            }
            else
                CERR_THROWRES( this, "Mismatching operand types for equality operator: " << ltype << ", " << rtype );
        }
        break;

    case TXTC_ARRAY:
        {   // comparison of length and then of each element
            auto lelemtype = ltype->element_type();
            auto relemtype = rtype->element_type();
            if ( lelemtype->get_type_class() == TXTC_ANY || relemtype->get_type_class() == TXTC_ANY ) {
                // if either operand's element type is 'Any', type checking must be done in runtime
                // Example:
                //     r : &Array;  s : &Array<Int>;
                //     r^ == s^
            }
            else if ( lelemtype->get_type_class() != relemtype->get_type_class() ) {
                CERR_THROWRES( this, "Mismatching Array element types for equality operator: " << lelemtype << ", " << relemtype );
            }
            else if ( lelemtype->get_type_class() == TXTC_ELEMENTARY ) {
                if ( lelemtype->type()->is_concrete() && relemtype->type()->is_concrete() ) {
                    if ( lelemtype->get_type_id() != relemtype->get_type_id() )
                        CERR_THROWRES( this, "Mismatching Array element types for equality operator: " << lelemtype << ", " << relemtype );
                }
            }
            else if ( lelemtype->get_type_class() == TXTC_REFERENCE ) {
                // Since the built-in comparison of arrays of references compares the pointers (identity equality), we don't
                // strictly need to check the ref target types here.
                // However if they are mutually exclusive, the comparison will always yield false.
                // The element types are mutually exclusive if neither type "is-a" the other type.
                if ( !( lelemtype->type()->target_type()->type()->is_a( *relemtype->type()->target_type()->type() ) ||
                        relemtype->type()->target_type()->type()->is_a( *lelemtype->type()->target_type()->type() ) ) )
                    CERR_THROWRES( this, "Mismatching Array element ref target types for equality operator: " << lelemtype << ", " << relemtype );
            }
        }
        break;

    default:
        break;
    }

    this->lhs->resolve_type();
    this->rhs->resolve_type();

    return new TxQualType( this->registry().get_builtin_type( TXBT_BOOL ) );
}

const TxQualType* TxRefEqualityOperatorNode::define_type() {
    auto ltype = this->lhs->originalExpr->resolve_type()->type();
    auto rtype = this->rhs->originalExpr->resolve_type()->type();

    // NOTE: For now we don't auto-reference the operands in this operation.

    if ( ltype->get_type_class() != TXTC_REFERENCE )
        CERR_THROWRES( this, "Left operand for identity equality operator is not a reference: " << ltype );
    if ( rtype->get_type_class() != TXTC_REFERENCE )
        CERR_THROWRES( this, "Right operand for identity equality operator is not a reference: " << rtype );

    this->lhs->resolve_type();
    this->rhs->resolve_type();

    return new TxQualType( this->registry().get_builtin_type( TXBT_BOOL ) );
}
