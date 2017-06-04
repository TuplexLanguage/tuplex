#include "ast_exprs.hpp"
#include "ast_conv.hpp"
#include "ast_maybe_conv_node.hpp"

// FUTURE: rework together with overloaded function resolution
/** Returns null if conversion failed. */
static TxExpressionNode* inner_wrap_conversion( TxExpressionNode* originalExpr, const TxType* originalType,
                                                const TxType* requiredType, bool _explicit ) {
    if ( originalType == requiredType ) // TODO: test with:  || *originalType == *requiredType)
        return originalExpr;

    if ( _explicit || originalType->auto_converts_to( *requiredType ) ) {
        // wrap originalExpr with conversion node
        TxExpressionNode* convExpr = nullptr;

        if ( requiredType->is_builtin( TXBT_BOOL ) )
            convExpr = new TxBoolConvNode( originalExpr, requiredType );

        else if ( requiredType->is_scalar() )
            convExpr = new TxScalarConvNode( originalExpr, requiredType );

        else if ( requiredType->get_type_class() == TXTC_REFERENCE )
            convExpr = new TxReferenceConvNode( originalExpr, requiredType );

        else if ( requiredType->get_type_class() == TXTC_ARRAY )
            convExpr = new TxObjSpecCastNode( originalExpr, requiredType );

        else if ( requiredType->get_type_class() == TXTC_FUNCTION )
            return originalExpr;

        else if ( requiredType->get_type_class() == TXTC_TUPLE )
            return originalExpr;

        else {
            LOG( originalExpr->LOGGER(), ERROR, "Type supposedly auto-converts but no conversion logic available:  "
                 << originalType << " => " << requiredType );
            return nullptr;
        }

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
    auto originalType = originalExpr->resolve_type();

    if ( auto newExpr = inner_wrap_conversion( originalExpr, originalType, requiredType, _explicit ) )
        return newExpr;

    // implicit reference-to ('&') operation:
    if ( requiredType->get_type_class() == TXTC_REFERENCE ) {
        if ( auto reqRefTargetType = requiredType->target_type() ) {
            if ( originalType->is_a( *reqRefTargetType ) ) {
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
                    auto refToNode = new TxReferenceToNode( originalExpr->parseLocation, originalExpr );
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
        if ( auto origRefTargetType = originalType->target_type() ) {
            if ( origRefTargetType->auto_converts_to( *requiredType ) ) {
                // wrap originalExpr with a dereference node
                //std::cerr << "Adding implicit '^' to: " << originalExpr << std::endl;
                auto derefNode = new TxReferenceDerefNode( originalExpr->parseLocation, originalExpr );
                derefNode->node_declaration_pass( originalExpr->parent() );
                if ( auto newExpr = inner_wrap_conversion( derefNode, origRefTargetType, requiredType, _explicit ) ) {
                    return newExpr;
//                    if ( newExpr != derefNode )
//                        return newExpr;
//                    auto convNode = new TxNoConversionNode( derefNode, origRefTargetType );
//                    convNode->node_declaration_pass( originalExpr->parent() );
//                    return convNode;
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


const TxType* TxMaybeConversionNode::define_type() {
    if ( this->insertedResultType ) {
        this->conversionExpr = make_conversion( this->originalExpr, this->insertedResultType, this->_explicit );
        return this->conversionExpr->resolve_type();
    }
    else {
        return this->originalExpr->resolve_type();
    }
}

void TxMaybeConversionNode::insert_conversion( const TxType* resultType, bool _explicit ) {
    ASSERT( this->originalExpr->is_context_set(), "declaration pass not yet run on originalExpr" );
    ASSERT( !this->attempt_get_type(), "type already resolved for " << this );

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

const TxType* TxReferenceConvNode::define_type() {
    auto resultTargetType = this->resultType->target_type();
    if ( resultTargetType->get_type_class() == TXTC_INTERFACE ) {
        auto origType = this->expr->resolve_type();
        auto origTargetType = origType->target_type();
        if ( !equivalent_interface_target_types( resultTargetType, origTargetType ) ) {
            // create / retrieve interface adapter type
            //std::cerr << "Converting interface reference to adapter:\n\tfrom & " << origTargetType << "\n\tto   & " << resultTargetType << std::endl;
            this->adapterType = this->registry().get_interface_adapter( resultTargetType, origTargetType );
            ASSERT( this->adapterType->get_type_class() == TXTC_INTERFACEADAPTER, "Not an interface adapter type: " << this->adapterType );

            // create reference type to the adapter type  TODO: review if node creation can be moved to constructor / declaration pass
            auto adapterDefiner = new TxTypeDeclWrapperNode( this->parseLocation, adapterType->get_declaration() );
            TxTypeTypeArgumentNode* targetTypeNode = new TxTypeTypeArgumentNode( adapterDefiner );
            run_declaration_pass( targetTypeNode, this, "type" );
            return this->registry().get_reference_type( this, targetTypeNode, nullptr );
        }
    }
    return TxConversionNode::define_type();  // returns the required resultType
}
