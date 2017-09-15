#include "ast_conv.hpp"
#include "ast_exprs.hpp"
#include "ast_maybe_conv_node.hpp"
#include "ast_ref.hpp"
#include "ast_constexpr.hpp"

#include <float.h>

/** Evaluates if the operand is a statically constant scalar value that fits in the required type without loss of precision. */
static bool statically_converts_to( TxExpressionNode* originalExpr, const TxType* originalType, const TxType* requiredType ) {
    if ( !originalExpr->is_statically_constant() )
        return false;
    auto storage = originalExpr->get_storage();
    if ( storage == TXS_VIRTUAL || storage == TXS_INSTANCEMETHOD || storage == TXS_INSTANCE ) {
        // currently not supported to access member fields before types are prepared
        return false;
    }

    auto origActType = originalType->acttype();
    if ( is_concrete_uinteger_type( origActType ) ) {
        uint64_t val = eval_unsigned_int_constant( originalExpr );
        switch ( requiredType->get_runtime_type_id() ) {
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
        switch ( requiredType->get_runtime_type_id() ) {
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
            return ( abs( val ) <= 2048 );  // largest integer that can be stored without precision loss
        case TXBT_FLOAT:
            return ( abs( val ) <= 16777216 );  // largest integer that can be stored without precision loss
        case TXBT_DOUBLE:
            return ( abs( val ) <= 9007199254740992 );  // largest integer that can be stored without precision loss
        default:
            return false;
        }
    }
    else if ( is_concrete_floating_type( origActType ) ) {
        double val = eval_floatingpoint_constant( originalExpr );
        double intpart = 0;
        switch ( requiredType->get_runtime_type_id() ) {
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


bool auto_converts_to( TxExpressionNode* originalExpr, const TxType* requiredType ) {
    auto originalType = originalExpr->resolve_type()->type();
    return ( originalType->auto_converts_to( *requiredType )
             || ( originalType->is_scalar() && requiredType->is_scalar()
                  && statically_converts_to( originalExpr, originalType, requiredType ) ) );
}


/** Returns null if conversion failed. */
static TxExpressionNode* inner_wrap_conversion( TxExpressionNode* originalExpr, const TxType* originalType,
                                                const TxType* requiredType, bool _explicit ) {
    if ( originalType == requiredType ) // TODO: test with:  || *originalType == *requiredType)
        return originalExpr;

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
                                                      const TxType* requiredType,
                                                      bool _explicit ) {
    // Note: Symbol declaration and resolution passes are not run on the created wrapper nodes.
    auto originalType = originalExpr->resolve_type()->type();

    if ( auto newExpr = inner_wrap_conversion( originalExpr, originalType, requiredType, _explicit ) )
        return newExpr;

    // implicit reference-to ('&') operation:
    if ( requiredType->get_type_class() == TXTC_REFERENCE ) {
        if ( auto reqRefTargetType = requiredType->target_type() ) {
            if ( originalType->is_a( *reqRefTargetType->type() ) ) {
                if ( reqRefTargetType->is_modifiable() ) {
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
                    return refConvNode;
                }
            }
        }
    }

    // implicit dereferencing ('^') operation:
    if ( originalType->get_type_class() == TXTC_REFERENCE ) {
        if ( auto origRefTargetType = originalType->target_type()->type() ) {
            if ( origRefTargetType->auto_converts_to( *requiredType ) ) {
                // wrap originalExpr with a dereference node
                //std::cerr << "Adding implicit '^' to: " << originalExpr << std::endl;
                auto derefNode = new TxReferenceDerefNode( originalExpr->ploc, originalExpr );
                derefNode->node_declaration_pass( originalExpr->parent() );
                if ( auto newExpr = inner_wrap_conversion( derefNode, origRefTargetType, requiredType, _explicit ) ) {
                    return newExpr;
                }
            }
        }
    }

    CERR_THROWRES( originalExpr, "Can't auto-convert value\n\tFrom: " << originalType << " \t@" << originalExpr->get_parse_location()
                   << "\n\tTo:   " << requiredType << " \t@" << requiredType->get_parse_location() );
    return nullptr;
}

TxExpressionNode* make_conversion( TxExpressionNode* originalExpr, const TxType* resultType, bool _explicit ) {
    ASSERT( originalExpr->is_context_set(), "Conversion's original expression hasn't run declaration pass: " << originalExpr );
    auto exprNode = inner_validate_wrap_convert( originalExpr, resultType, _explicit );
    if ( exprNode && exprNode != originalExpr ) {
        LOG_TRACE( originalExpr->LOGGER(), "Wrapping conversion to type " << resultType->str() << " around " << originalExpr );
    }
    return exprNode;
}


const TxQualType* TxMaybeConversionNode::define_type() {
    if ( this->insertedResultType ) {
        this->resolvedExpr = make_conversion( this->originalExpr, this->insertedResultType, this->_explicit );
    }
    return this->resolvedExpr->resolve_type();
}

void TxMaybeConversionNode::insert_conversion( const TxType* resultType, bool _explicit ) {
    ASSERT( this->originalExpr->is_context_set(), "declaration pass not yet run on originalExpr" );
    ASSERT( !this->attempt_qualtype(), "type already resolved for " << this );

    if ( this->insertedResultType ) {
        LOG( this->LOGGER(), ALERT, this << ": Skipping overwrite previously inserted conversion node" );
        return;
    }
    this->insertedResultType = resultType;
    this->_explicit = _explicit;
    //this->conversionExpr = make_conversion( this->originalExpr, resultType, _explicit );
}


/** Returns true if the two types are mutually "equivalent".
 * Note, does not take explicit naming into account. */
// TODO: possibly revise together with is_equivalent_derivation()
static bool equivalent_interface_target_types( const TxType* typeA, const TxType* typeB ) {
    while ( typeA->is_same_vtable_type() )
        typeA = typeA->get_semantic_base_type();
    while ( typeB->is_same_vtable_type() )
        typeB = typeB->get_semantic_base_type();
    return ( *typeA == *typeB );
}

const TxQualType* TxReferenceConvNode::define_type() {
    auto resultTargetType = this->resultType->target_type()->type();
    if ( resultTargetType->get_type_class() == TXTC_INTERFACE ) {
        auto origTargetType = this->expr->resolve_type()->type()->target_type()->type();
        if ( !equivalent_interface_target_types( resultTargetType, origTargetType ) ) {
            // create / retrieve interface adapter type
            //std::cerr << "Converting interface reference to adapter:\n\tfrom & " << origTargetType << "\n\tto   & " << resultTargetType << std::endl;
            this->adapterType = this->registry().get_interface_adapter( resultTargetType, origTargetType );
            ASSERT( this->adapterType->get_type_class() == TXTC_INTERFACEADAPTER, "Not an interface adapter type: " << this->adapterType );

            // create reference type to the adapter type  TODO: review if node creation can be moved to constructor / declaration pass
            auto adapterDefiner = new TxTypeDeclWrapperNode( this->ploc, adapterType->get_declaration() );
            TxTypeTypeArgumentNode* targetTypeNode = new TxTypeTypeArgumentNode( adapterDefiner );
            run_declaration_pass( targetTypeNode, this, "type" );
            return new TxQualType( this->registry().get_reference_type( this, targetTypeNode, nullptr ) );
        }
    }
    return TxConversionNode::define_type();  // returns the required resultType
}
