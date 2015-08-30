#include "ast_conversion.hpp"
#include "ast_exprs.hpp"


// FUTURE: factor out the 'explicit' code path into separate function
// FUTURE: rework together with overloaded function resolution
static TxExpressionNode* inner_wrap_conversion(TxSpecializationIndex six, ResolutionContext& resCtx,
                                               TxExpressionNode* originalExpr, const TxType* originalType,
                                               const TxType* requiredType, bool _explicit) {
    if (originalType == requiredType) // TODO: test with:  || *originalType == *requiredType)
        return originalExpr;
    if (_explicit || originalType->auto_converts_to(*requiredType)) {
        // wrap originalExpr with conversion node
        if (auto boolType = dynamic_cast<const TxBoolType*>(requiredType))
            return new TxBoolConvNode(originalExpr->parseLocation, originalExpr, boolType);
        if (auto scalarType = dynamic_cast<const TxScalarType*>(requiredType))
            return new TxScalarConvNode(originalExpr->parseLocation, originalExpr, scalarType);
        if (auto refType = dynamic_cast<const TxReferenceType*>(requiredType))
            return new TxReferenceConvNode(originalExpr->parseLocation, originalExpr, refType);
        if (auto arrayType = dynamic_cast<const TxArrayType*>(requiredType))
            return new TxObjSpecCastNode(originalExpr->parseLocation, originalExpr, arrayType);
        if (dynamic_cast<const TxFunctionType*>(requiredType))
            return originalExpr;  // or do we actually need to do something here?
        originalExpr->LOGGER().error("Type supposedly auto-converts but no conversion logic available:  %s => %s",
                                     originalType->to_string().c_str(), requiredType->to_string().c_str());
    }
    return nullptr;
}

static TxExpressionNode* inner_validate_wrap_convert(TxSpecializationIndex six, ResolutionContext& resCtx,
                                                     TxExpressionNode* originalExpr,
                                                     const TxType* requiredType, bool _explicit) {
    // Note: Symbol table pass and semantic pass are not run on the created wrapper nodes.
    auto originalType = originalExpr->resolve_type(six, resCtx);
    if (! originalType)
        return originalExpr;

    if (auto newExpr = inner_wrap_conversion(six, resCtx, originalExpr, originalType, requiredType, _explicit))
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
                    refToNode->set_context(six, originalExpr->context(six));  // in lieu of symbol_declaration_pass()
                    return new TxReferenceConvNode(originalExpr->parseLocation, refToNode, reqRefType);
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
                derefNode->set_context(six, originalExpr->context(six));  // in lieu of symbol_declaration_pass()
                if (auto newExpr = inner_wrap_conversion(six, resCtx, derefNode, origRefTargetType, requiredType, _explicit)) {
                    if (newExpr != derefNode)
                        return newExpr;
                    return new TxExprWrapperNode(derefNode, six);
                }
            }
        }
    }

    CERROR(originalExpr, "Can't auto-convert value (s-ix " << six << ")\n\tFrom: " << originalType << " \t@" << originalType->get_parse_location()
                         << "\n\tTo:   " << requiredType << " \t@" << requiredType->get_parse_location());
    return originalExpr;
}

TxExpressionNode* validate_wrap_convert(TxSpecializationIndex six, ResolutionContext& resCtx, TxExpressionNode* originalExpr,
                                        const TxType* requiredType, bool _explicit) {
    if (six != 0) {
        // TODO: refactor
        TxExpressionNode* fromExpr = originalExpr;
        if (auto convNode = dynamic_cast<TxConversionNode*>(originalExpr))
            fromExpr = convNode->expr;  // pass-through conversion added in s-ix 0
        if (auto originalType = fromExpr->resolve_type(six, resCtx)) {
            if (! (originalType == requiredType || _explicit || originalType->auto_converts_to(*requiredType)))
                CERROR(fromExpr, "Can't auto-convert value (s-ix " << six << ")\n\tFrom: " << originalType << " \t@" << originalType->get_parse_location()
                                     << "\n\tTo:   " << requiredType << " \t@" << requiredType->get_parse_location());
        }
        return originalExpr;
    }
    auto exprNode = inner_validate_wrap_convert(six, resCtx, originalExpr, requiredType, _explicit);
    if (exprNode != originalExpr) {
        originalExpr->LOGGER().trace("Wrapping conversion to type %s around %s", requiredType->to_string(true).c_str(), originalExpr->to_string().c_str());
        for (TxSpecializationIndex s = 0; s < originalExpr->next_spec_index(); s++) {
            exprNode->symbol_declaration_pass(s, originalExpr->context(s));
            exprNode->symbol_resolution_pass(s, resCtx);
        }
    }
    return exprNode;
}

TxExpressionNode* validate_wrap_assignment(TxSpecializationIndex six, ResolutionContext& resCtx, TxExpressionNode* rValueExpr, const TxType* requiredType) {
    if (! requiredType->is_concrete())
        // TODO: dynamic concrete type resolution (recognize actual type in runtime when dereferencing a generic pointer)
        CERROR(rValueExpr, "Assignee is not a concrete type (size potentially unknown): " << requiredType);
    // if assignee is a reference:
    // TODO: check dataspace rules
    return validate_wrap_convert(six, resCtx, rValueExpr, requiredType);
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

const TxType* TxReferenceConvNode::define_type(TxSpecializationIndex six, ResolutionContext& resCtx) {
    auto resultTargetType = static_cast<const TxReferenceType*>(this->resultType)->target_type();
    if (resultTargetType && resultTargetType->get_type_class() == TXTC_INTERFACE) {
        auto origType = static_cast<const TxReferenceType*>(this->expr->resolve_type(six, resCtx));
        auto origTargetType = origType->target_type();
        if (! equivalent_interface_target_types(resultTargetType, origTargetType)) {
            // create / retrieve interface adapter type
            //std::cerr << "Converting interface reference to adapter:\n\tfrom & " << origTargetType << "\n\tto   & " << resultTargetType << std::endl;
            this->adapterType = this->types().get_interface_adapter(resultTargetType, origTargetType);

            // create reference type to the adapter type  TODO: delegate this to TypeRegistry
            auto implTypeName = this->context(six).scope()->make_unique_name("$type");
            auto typeDecl = this->context(six).scope()->declare_type(implTypeName, this->get_type_definer(six), TXD_PUBLIC | TXD_IMPLICIT);
            auto adapterDefiner = new TxTypeWrapperDef(adapterType);
            return this->types().get_reference_type(typeDecl, TxGenericBinding::make_type_binding("T", adapterDefiner));
        }
    }
    return TxConversionNode::define_type(six, resCtx);  // returns the required resultType
}
