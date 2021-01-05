#include "ast_conv.hpp"
#include "inner_conv.hpp"
#include "ast_exprs.hpp"
#include "ast_maybe_conv_node.hpp"
#include "ast_ref.hpp"
#include "ast_constexpr.hpp"
#include "tx_error.hpp"
#include "tx_logging.hpp"
#include <cmath>
#include <utility>
#include <float.h>

/** Evaluates if the operand is a statically constant scalar value that fits in the required type without loss of precision. */
static bool statically_converts_to( TxExpressionNode* originalExpr, TxQualType originalType, TxQualType requiredType ) {
    if ( !originalExpr->is_statically_constant() )
        return false;
    auto storage = originalExpr->get_storage();
    if ( storage == TXS_VIRTUAL || storage == TXS_INSTANCEMETHOD || storage == TXS_INSTANCE ) {
        // currently not supported to access member fields before types are prepared
        return false;
    }

    const TxActualType* origActType = originalType.type();
    if ( is_concrete_uinteger_type( origActType ) ) {
        uint64_t val = eval_unsigned_int_constant( originalExpr );
        switch ( requiredType->get_elementary_type_id() ) {
        case TXBT_BYTE:
            return ( val <= 127 );
        case TXBT_SHORT:
            return ( val <= 32767 );
        case TXBT_INT:
            return ( val <= 2147483647 );
        case TXBT_LONG:
            return ( val <= 9223372036854775807 );
        case TXBT_UBYTE:
            return ( val <= 255 );
        case TXBT_USHORT:
            return ( val <= 65535 );
        case TXBT_UINT:
            return ( val <= 4294967295 );
        case TXBT_ULONG:
            return true;
        case TXBT_HALF:
            return ( val <= 2048 );  // largest integer that can be stored without precision loss
        case TXBT_FLOAT:
            return ( val <= 16777216 );  // largest integer that can be stored without precision loss
        case TXBT_DOUBLE:
            return ( val <= 9007199254740992 );  // largest integer that can be stored without precision loss
        default:
            return false;
        }
    }
    else if ( is_concrete_sinteger_type( origActType ) ) {
        int64_t val = eval_signed_int_constant( originalExpr );
        switch ( requiredType->get_elementary_type_id() ) {
        case TXBT_BYTE:
            return ( val >= -128 && val <= 127 );
        case TXBT_SHORT:
            return ( val >= -32768 && val <= 32767 );
        case TXBT_INT:
            return ( val >= -2147483648 && val <= 2147483647 );
        case TXBT_LONG:
            return true;
        case TXBT_UBYTE:
            return ( val >= 0 && val <= 255 );
        case TXBT_USHORT:
            return ( val >= 0 && val <= 65535 );
        case TXBT_UINT:
            return ( val >= 0 && val <= 4294967295 );
        case TXBT_ULONG:
            return ( val >= 0 );
        case TXBT_HALF:
            return ( llabs( val ) <= 2048 );  // largest integer that can be stored without precision loss
        case TXBT_FLOAT:
            return ( llabs( val ) <= 16777216 );  // largest integer that can be stored without precision loss
        case TXBT_DOUBLE:
            return ( llabs( val ) <= 9007199254740992 );  // largest integer that can be stored without precision loss
        default:
            return false;
        }
    }
    else if ( is_concrete_floating_type( origActType ) ) {
        double val = eval_floatingpoint_constant( originalExpr );
        double intpart = 0;
        switch ( requiredType->get_elementary_type_id() ) {
        case TXBT_BYTE:
            return ( modf( val, &intpart) == 0 && intpart >= -128 && intpart <= 127 );
        case TXBT_SHORT:
            return ( modf( val, &intpart) == 0 && intpart >= -32768 && intpart <= 32767 );
        case TXBT_INT:
            return ( modf( val, &intpart) == 0 && intpart >= -2147483648 && intpart <= 2147483647 );
        case TXBT_LONG:
            return ( modf( val, &intpart) == 0 && intpart >= INT64_MIN && intpart <= INT64_MAX );
        case TXBT_UBYTE:
            return ( modf( val, &intpart) == 0 && intpart >= 0 && intpart <= 255 );
        case TXBT_USHORT:
            return ( modf( val, &intpart) == 0 && intpart >= 0 && intpart <= 65535 );
        case TXBT_UINT:
            return ( modf( val, &intpart) == 0 && intpart >= 0 && intpart <= 4294967295 );
        case TXBT_ULONG:
            return ( modf( val, &intpart) == 0 && intpart >= 0 && intpart <= UINT64_MAX  );
        case TXBT_HALF:
            return ( fabs( val ) <= 65503.0 );
        case TXBT_FLOAT:
            return ( fabs( val ) <= FLT_MAX );
        case TXBT_DOUBLE:
            return true;
        default:
            return false;
        }
    }
    return false;
}


bool auto_converts_to( TxExpressionNode* originalExpr, TxQualType requiredType ) {
    auto originalType = originalExpr->resolve_type( TXP_FULL_RESOLUTION );
    return ( originalType->auto_converts_to( *requiredType )
             || ( originalType->is_scalar() && requiredType->is_scalar()
                  && statically_converts_to( originalExpr, originalType.type(), requiredType ) ) );
}


/** Returns null if conversion failed. */
static TxExpressionNode* inner_wrap_conversion( TxExpressionNode* originalExpr, TxQualType originalType,
                                                TxQualType requiredType, bool _explicit ) {
    if ( requiredType.is_modifiable() && !originalType.is_modifiable() )
        return nullptr;  // should we do this check here?
    else if ( originalType.type() == requiredType.type() ) {
//        if ( originalType->get_type_class() != TXTC_ELEMENTARY )
//            std::cerr << "EQUAL WHEN WITHOUT MODIFIABILITY CHECK: " << originalType << "  ->  " << requiredType << std::endl;
        return originalExpr;  // assume correct to not check modifiability here...
    }

    if ( _explicit || originalType->auto_converts_to( *requiredType ) ) {
        // wrap originalExpr with conversion node
        TxExpressionNode* convExpr = nullptr;
        auto requiredTypeClass = requiredType->get_type_class();

        if ( requiredTypeClass == TXTC_ELEMENTARY ) {
            if ( requiredType->is_builtin( TXBT_BOOL ) )
                convExpr = new TxBoolConvNode( originalExpr, requiredType );

            else if ( requiredType->is_scalar() )
                convExpr = new TxScalarConvNode( originalExpr, requiredType );
        }

        else if ( requiredTypeClass == TXTC_REFERENCE )
            convExpr = new TxReferenceConvNode( originalExpr, requiredType );

        else if ( requiredTypeClass == TXTC_ARRAY )
            convExpr = new TxObjSpecCastNode( originalExpr, requiredType );

        else if ( requiredTypeClass == TXTC_FUNCTION )
            return originalExpr;

        else if ( requiredTypeClass == TXTC_TUPLE || requiredTypeClass == TXTC_INTERFACE )
            return originalExpr;

        else if ( requiredTypeClass == TXTC_ANY )
            return nullptr;  // in practice Any can't be converted to Any; error message about non-concrete type generated elsewhere

        else {
            LOG( originalExpr->LOGGER(), ERROR, "Type supposedly auto-converts but no conversion logic available:  "
                 << originalType << " => " << requiredType );
            return nullptr;
        }

        convExpr->node_declaration_pass( originalExpr->parent() );
        return convExpr;
    }
    else if ( originalType->is_scalar() && requiredType->is_scalar()
              && statically_converts_to( originalExpr, originalType, requiredType ) ) {
        TxExpressionNode* convExpr = new TxScalarConvNode( originalExpr, requiredType );
        convExpr->node_declaration_pass( originalExpr->parent() );
        return convExpr;
    }

    return nullptr;
}

/** Checks that an expression has a type that matches the required type, and wraps
 * a value & type conversion node around it if permitted and necessary.
 *
 * Assumes that originalExpr declaration pass has already run.
 * If a conversion node is created, symbol declaration pass is run on it.
 * Returns null if conversion failed.
 */
static TxExpressionNode* inner_validate_wrap_convert( TxExpressionNode* originalExpr,
                                                      TxQualType requiredType,
                                                      bool _explicit ) {
    // Note: Resolution pass is not run on the created wrapper nodes.
    auto originalType = originalExpr->resolve_type( TXP_FULL_RESOLUTION );

    if ( auto newExpr = inner_wrap_conversion( originalExpr, originalType, requiredType, _explicit ) )
        return newExpr;

#ifndef NO_IMPLICIT_REF_DEREF
    // implicit reference-to ('&') operation:
    if ( requiredType->get_type_class() == TXTC_REFERENCE ) {
        if ( auto reqRefTargetType = requiredType->target_type() ) {
            if ( originalType->is_a( *reqRefTargetType ) ) {
                if ( reqRefTargetType.is_modifiable() ) {
//                    if ( !originalType->is_modifiable() ) {
//                        // Note, is_a() will have already returned false for attempt to convert reference with non-mod-target to one with mod target
//                        return originalExpr;
//                    }
                    CERR_THROWRES( originalExpr, "Cannot implicitly convert to reference with modifiable target: "
                                   << originalType << " -> " << requiredType );
                }
                else {
                    // wrap originalExpr with a reference-to node
                    //std::cerr << "Adding implicit '&' to: " << originalExpr << std::endl;
                    auto refToNode = new TxReferenceToNode( originalExpr->ploc, originalExpr );
                    auto refConvNode = new TxReferenceConvNode( refToNode, requiredType );
                    refConvNode->node_declaration_pass( originalExpr->parent() );
                    refToNode->node_declaration_pass( refConvNode );
                    refToNode->resolve_type( TXP_FULL_RESOLUTION );  // ensure 'inner' conversion node gets resolved
                    return refConvNode;
                }
            }
        }
    }

    // implicit dereferencing ('^') operation:
    if ( originalType->get_type_class() == TXTC_REFERENCE ) {
        if ( auto origRefTargetType = originalType->target_type() ) {
            if ( origRefTargetType->auto_converts_to( *requiredType ) ) {
                // wrap originalExpr with a dereference node
                //std::cerr << "Adding implicit '^' to: " << originalExpr << std::endl;
                auto derefNode = new TxReferenceDerefNode( originalExpr->ploc, originalExpr );
                derefNode->node_declaration_pass( originalExpr->parent() );
                if ( auto newExpr = inner_wrap_conversion( derefNode, origRefTargetType, requiredType, _explicit ) ) {
                    derefNode->resolve_type( TXP_FULL_RESOLUTION );  // ensure 'inner' conversion node gets resolved
                    return newExpr;
                }
            }
        }
    }
#endif

    CERR_THROWRES( originalExpr, "Can't auto-convert value\n\tFrom: " << originalType << " \t@" << originalExpr->get_parse_location()
                   << "\n\tTo:   " << requiredType << " \t@" << requiredType->get_parse_location() );
    return nullptr;
}

TxExpressionNode* make_conversion( TxExpressionNode* originalExpr, TxQualType requiredType, bool _explicit ) {
    ASSERT( originalExpr->is_context_set(), "Conversion's original expression hasn't run declaration pass: " << originalExpr );
    auto exprNode = inner_validate_wrap_convert( originalExpr, requiredType, _explicit );
    if ( exprNode && exprNode != originalExpr ) {
        LOG_TRACE( originalExpr->LOGGER(), "Wrapping conversion to type " << requiredType->str() << " around " << originalExpr );
    }
    return exprNode;
}


TxQualType TxMaybeConversionNode::define_type( TxTypeResLevel typeResLevel ) {
//    if (get_node_id()==22303)
//        std::cerr << "HERE " << this << std::endl;
    if ( this->insertedResultType ) {
        this->resolvedExpr = make_conversion( this->originalExpr, this->insertedResultType, this->_explicit );
    }
    return this->resolvedExpr->resolve_type( typeResLevel );
}

void TxMaybeConversionNode::insert_conversion( TxTypeResLevel typeResLevel, const TxActualType* resultType, bool _explicit ) {
    this->insert_qual_conversion( typeResLevel, resultType, _explicit);
}

void TxMaybeConversionNode::insert_qual_conversion( TxTypeResLevel typeResLevel, TxQualType resultType, bool _explicit ) {
//    if (get_node_id()==9308)
//        std::cerr << "HERE " << this << std::endl;
    ASSERT( this->originalExpr->is_context_set(), "declaration pass not yet run on originalExpr" );
    ASSERT( !this->attempt_qtype(), "can't insert conversion after already type-resolved: " << this );

    if ( this->insertedResultType ) {
        LOG( this->LOGGER(), ALERT, this << ": Skipping overwrite previously inserted conversion node" );
        return;
    }
    this->insertedResultType = resultType;
    this->_explicit = _explicit;

    this->resolve_type( typeResLevel );
}


/** Returns true if the two types are mutually "equivalent".
 * Note, does not take explicit naming into account. */
// TODO: possibly revise together with is_equivalent_derivation()
static bool equivalent_target_types( TxQualType typeA, TxQualType typeB ) {
    while ( typeA->is_same_vtable_type() )
        typeA = typeA->get_semantic_base_type();
    while ( typeB->is_same_vtable_type() )
        typeB = typeB->get_semantic_base_type();
    return ( *typeA == *typeB );
}

TxQualType TxReferenceConvNode::define_type( TxTypeResLevel typeResLevel ) {
    auto resultTargetType = this->resultType->target_type();
    if ( resultTargetType->get_type_class() == TXTC_INTERFACE ) {
        auto origTargetType = this->expr->resolve_type( typeResLevel )->target_type();
        if ( origTargetType->get_type_class() == TXTC_INTERFACE ) {
            if ( !origTargetType->is_a_primary_path( *resultTargetType ) )
                CERR_THROWRES( this, "Can't convert from one interface-ref to another interface-ref that is not a primary derivation" );
            //std::cerr << "converting from one interface-ref to another interface-ref: " << this << std::endl;
        }
        else if ( !equivalent_target_types( resultTargetType, origTargetType ) ) {
            // create / retrieve interface adapter type
            //std::cerr << "Converting to interface adapter reference in " << this << ":\n\tfrom & " << origTargetType << "\n\tto   & " << resultTargetType << std::endl;
            this->adapterType = this->registry().get_interface_adapter( this, resultTargetType.type(), origTargetType.type() );
            // (earlier we created a reference type to the adapter type, but shouldn't be necessary, doesn't affect code generation)
        }
    }
    return TxConversionNode::define_type( typeResLevel );  // returns the required resultType
}
