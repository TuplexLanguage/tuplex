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

        if (auto boolType = dynamic_cast<const TxBoolType*>(requiredType))
            return new TxBoolConvNode(originalExpr, boolType);

        if (auto scalarType = dynamic_cast<const TxScalarType*>(requiredType))
            return new TxScalarConvNode(originalExpr, scalarType);

        if (auto refType = dynamic_cast<const TxReferenceType*>(requiredType))
            return new TxReferenceConvNode(originalExpr, refType);

        if (auto arrayType = dynamic_cast<const TxArrayType*>(requiredType))
            return new TxObjSpecCastNode(originalExpr, arrayType);

        if (dynamic_cast<const TxFunctionType*>(requiredType))
            return originalExpr;  // or do we actually need to do something here?

        originalExpr->LOGGER().error("Type supposedly auto-converts but no conversion logic available:  %s => %s",
                                     originalType->to_string().c_str(), requiredType->to_string().c_str());
    }
    return nullptr;
}

/** Checks that an expression has a type that matches the required type, and wraps
 * a value & type conversion node around it if permitted and necessary.
 *
 * Assumes that originalExpr symbol registration pass has already run.
 * Will run symbol registration and symbol resolution passes on any inserted nodes.
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
    if (auto reqRefType = dynamic_cast<const TxReferenceType*>(requiredType)) {
        if (auto reqRefTargetType = reqRefType->target_type()) {
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
                    refToNode->set_context( originalExpr->context());  // in lieu of symbol_declaration_pass()
                    return new TxReferenceConvNode( refToNode, reqRefType );
                }
            }
        }
    }

    // implicit dereferencing ('^') operation:
    if (auto origRefType = dynamic_cast<const TxReferenceType*>(originalType)) {
        if (auto origRefTargetType = origRefType->target_type()) {
            if (origRefTargetType->auto_converts_to(*requiredType)) {
                // wrap originalExpr with a dereference node
                //std::cerr << "Adding implicit '^' to: " << originalExpr << std::endl;
                auto derefNode = new TxReferenceDerefNode(originalExpr->parseLocation, originalExpr);
                derefNode->set_context( originalExpr->context());  // in lieu of symbol_declaration_pass()
                if (auto newExpr = inner_wrap_conversion(derefNode, origRefTargetType, requiredType, _explicit)) {
                    if (newExpr != derefNode)
                        return newExpr;
                    return new TxNoConversionNode( derefNode, origRefTargetType ); //new TxExprWrapperNode( derefNode );
                }
            }
        }
    }

    CERROR(originalExpr, "Can't auto-convert value\n\tFrom: " << originalType << " \t@" << originalExpr->get_parse_location()
                         << "\n\tTo:   " << requiredType << " \t@" << requiredType->get_parse_location());
    return originalExpr;
}


static TxExpressionNode* make_conversion( TxExpressionNode* originalExpr, const TxType* resultType, bool _explicit ) {
    auto exprNode = inner_validate_wrap_convert( originalExpr, resultType, _explicit );
    if (exprNode != originalExpr) {
        originalExpr->LOGGER().trace("Wrapping conversion to type %s around %s", resultType->to_string(true).c_str(), originalExpr->to_string().c_str());
        exprNode->symbol_declaration_pass( originalExpr->context() );
    }
    return exprNode;
}


void TxMaybeConversionNode::insert_conversion( const TxType* resultType, bool _explicit ) {
    ASSERT(this->originalExpr->is_context_set(), "declaration pass not yet run on originalExpr");

    if (this->conversionExpr) {
        LOGGER().alert("%s: Skipping overwrite previously inserted conversion node", this->parse_loc_string().c_str());
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
    auto resultTargetType = static_cast<const TxReferenceType*>(this->resultType)->target_type();
    if (resultTargetType && resultTargetType->get_type_class() == TXTC_INTERFACE) {
        auto origType = static_cast<const TxReferenceType*>(this->expr->resolve_type());
        auto origTargetType = origType->target_type();
        if (! equivalent_interface_target_types(resultTargetType, origTargetType)) {
            // create / retrieve interface adapter type
            //std::cerr << "Converting interface reference to adapter:\n\tfrom & " << origTargetType << "\n\tto   & " << resultTargetType << std::endl;
            this->adapterType = this->types().get_interface_adapter(resultTargetType, origTargetType);
            ASSERT(this->adapterType->get_type_class() == TXTC_INTERFACEADAPTER, "Not an interface adapter type: " << this->adapterType);

            // create reference type to the adapter type
            auto adapterDefiner = new TxTypeDeclWrapperNode( this->parseLocation, adapterType->get_declaration() );
            return this->types().get_reference_type( this, TxGenericBinding::make_type_binding("T", adapterDefiner), nullptr );
        }
    }
    return TxConversionNode::define_type();  // returns the required resultType
}
