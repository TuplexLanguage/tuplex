#include "ast_conv.hpp"
#include "ast_exprs.hpp"


// FUTURE: factor out the 'explicit' code path into separate function
// FUTURE: rework together with overloaded function resolution
static TxExpressionNode* inner_wrap_conversion(TxExpressionNode* originalExpr, const TxType* originalType,
                                               const TxType* requiredType, bool _explicit) {
    if (originalType == requiredType) // TODO: test with:  || *originalType == *requiredType)
        return originalExpr;
    if (_explicit || originalType->auto_converts_to(*requiredType)) {
        // wrap originalExpr with conversion node

        if (requiredType->is_builtin( BOOL ))
            return new TxBoolConvNode(originalExpr, requiredType);

        if (requiredType->is_scalar())
            return new TxScalarConvNode(originalExpr, requiredType);

        if (requiredType->get_type_class() == TXTC_REFERENCE)
            return new TxReferenceConvNode(originalExpr, requiredType);

        if (requiredType->get_type_class() == TXTC_ARRAY)
            return new TxObjSpecCastNode(originalExpr, requiredType);

        if (requiredType->get_type_class() == TXTC_FUNCTION)
            return originalExpr;  // or do we actually need to do something here?

        LOG(originalExpr->LOGGER(), ERROR, "Type supposedly auto-converts but no conversion logic available:  "
                                           << originalType << " => " << requiredType);
    }
    return nullptr;
}

/** Checks that an expression has a type that matches the required type, and wraps
 * a value & type conversion node around it if permitted and necessary.
 *
 * Assumes that originalExpr symbol registration pass has already run.
 * Will not run symbol declaration / resolution passes on any inserted nodes.
 */
static TxExpressionNode* inner_validate_wrap_convert( TxExpressionNode* originalExpr,
                                                      const TxType* requiredType, bool _explicit ) {
    // Note: Symbol declaration and resolution passes are not run on the created wrapper nodes.
    auto originalType = originalExpr->resolve_type();
    if (! originalType)
        return originalExpr;

    if (auto newExpr = inner_wrap_conversion(originalExpr, originalType, requiredType, _explicit))
        return newExpr;

    // implicit reference-to ('&') operation:
    if (requiredType->get_type_class() == TXTC_REFERENCE) {
        if (auto reqRefTargetType = requiredType->target_type()) {
            if (originalType->is_a(*reqRefTargetType)) {
                if (reqRefTargetType->is_modifiable()) {
                    if (originalType->is_modifiable())
                        CERROR(originalExpr, "Cannot implicitly convert to reference with modifiable target: "
                               << originalType << " -> " << requiredType);
                    // is_a() will return false for this case:
                    //else
                    //    CERROR(originalExpr, "Cannot convert reference with non-mod-target to one with mod target: "
                    //           << originalType << " -> " << requiredType);
                    return originalExpr;
                }
                else {
                    // wrap originalExpr with a reference-to node
                    //std::cerr << "Adding implicit '&' to: " << originalExpr << std::endl;
                    auto refToNode = new TxReferenceToNode(originalExpr->parseLocation, originalExpr);
                    //refToNode->symbol_declaration_pass( originalExpr->context() );
                    return new TxReferenceConvNode( refToNode, requiredType );
                }
            }
        }
    }

    // implicit dereferencing ('^') operation:
    if (originalType->get_type_class() == TXTC_REFERENCE) {
        if (auto origRefTargetType = originalType->target_type()) {
            if (origRefTargetType->auto_converts_to(*requiredType)) {
                // wrap originalExpr with a dereference node
                //std::cerr << "Adding implicit '^' to: " << originalExpr << std::endl;
                auto derefNode = new TxReferenceDerefNode(originalExpr->parseLocation, originalExpr);
                //derefNode->symbol_declaration_pass( originalExpr->context());
                if (auto newExpr = inner_wrap_conversion(derefNode, origRefTargetType, requiredType, _explicit)) {
                    if (newExpr != derefNode)
                        return newExpr;
                    return new TxNoConversionNode( derefNode, origRefTargetType );
                }
            }
        }
    }

    CERROR(originalExpr, "Can't auto-convert value\n\tFrom: " << originalType << " \t@" << originalExpr->get_parse_location()
                         << "\n\tTo:   " << requiredType << " \t@" << requiredType->get_parse_location());
    return originalExpr;
}


TxExpressionNode* make_conversion( TxExpressionNode* originalExpr, const TxType* resultType, bool _explicit ) {
    auto exprNode = inner_validate_wrap_convert( originalExpr, resultType, _explicit );
    if (exprNode != originalExpr) {
        LOG_TRACE(originalExpr->LOGGER(), "Wrapping conversion to type " << resultType->str() << " around " << originalExpr);
        exprNode->symbol_declaration_pass( originalExpr->context() );
    }
    return exprNode;
}


void TxMaybeConversionNode::insert_conversion( const TxType* resultType, bool _explicit ) {
    ASSERT(this->originalExpr->is_context_set(), "declaration pass not yet run on originalExpr");

    if (this->conversionExpr) {
        LOG(this->LOGGER(), ALERT, this << ": Skipping overwrite previously inserted conversion node");
        return;
    }
    //ASSERT(!this->specificConvs.at(six), this->parse_loc_string() << ": Can't overwrite previously inserted conversion node for six=" << six);
    this->conversionExpr = make_conversion(this->originalExpr, resultType, _explicit);
}



/** Returns true if the two types are mutually "equivalent".
 * Note, does not take explicit naming into account. */
// FIXME: possibly revise together with is_equivalent_derivation()
static bool equivalent_interface_target_types(const TxType* typeA, const TxType* typeB) {
    while (typeA->is_same_vtable_type())
        typeA = typeA->get_semantic_base_type();
    while (typeB->is_same_vtable_type())
        typeB = typeB->get_semantic_base_type();
    return (*typeA == *typeB);
}

const TxType* TxReferenceConvNode::define_type() {
    auto resultTargetType = this->resultType->target_type();
    if (resultTargetType->get_type_class() == TXTC_INTERFACE) {
        auto origType = this->expr->resolve_type();
        auto origTargetType = origType->target_type();
        if (! equivalent_interface_target_types(resultTargetType, origTargetType)) {
            // create / retrieve interface adapter type
            //std::cerr << "Converting interface reference to adapter:\n\tfrom & " << origTargetType << "\n\tto   & " << resultTargetType << std::endl;
            this->adapterType = this->types().get_interface_adapter(resultTargetType, origTargetType);
            ASSERT(this->adapterType->get_type_class() == TXTC_INTERFACEADAPTER, "Not an interface adapter type: " << this->adapterType);

            // create reference type to the adapter type  TODO: review if node creation can be moved to constructor / declaration pass
            auto adapterDefiner = new TxTypeDeclWrapperNode( this->parseLocation, adapterType->get_declaration() );
            TxTypeTypeArgumentNode* targetTypeNode = new TxTypeTypeArgumentNode( adapterDefiner );
            targetTypeNode->symbol_declaration_pass( this->context(), this->context());
            return this->types().get_reference_type( this, targetTypeNode, nullptr );
        }
    }
    return TxConversionNode::define_type();  // returns the required resultType
}
