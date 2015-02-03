#include "ast.hpp"


void TxFieldDeclNode::symbol_table_pass(LexicalContext& lexContext) {
    TxFieldStorage storage;
    if (dynamic_cast<TxModule*>(lexContext.scope())) {  // if in global scope
        if (this->declFlags & TXD_STATIC)
            parser_error(this->parseLocation, "'static' is invalid modifier for module scope field %s", this->field->ident.c_str());
        if (this->declFlags & TXD_FINAL)
            parser_error(this->parseLocation, "'final' is invalid modifier for module scope field %s", this->field->ident.c_str());
        if (this->declFlags & TXD_OVERRIDE)
            parser_error(this->parseLocation, "'override' is invalid modifier for module scope field %s", this->field->ident.c_str());
        storage = TXS_GLOBAL;
    }
    else if (this->isMethod) {
        // static has special meaning for methods, technically a method is always a static function pointer field
        storage = TXS_STATIC;
        if (! (this->declFlags & TXD_STATIC)) {
            // instance method (add implicit self argument)
            static_cast<TxLambdaExprNode*>(field->initExpression)->make_instance_method();
        }
    }
    else {
        storage = (this->declFlags & TXD_STATIC) ? TXS_STATIC : TXS_INSTANCE;
    }
    this->field->symbol_table_pass_decl_field(lexContext, this->declFlags, storage, TxIdentifier(""));
    this->set_context(this->field);
}


// FUTURE: factor out the 'explicit' code path into separate function
TxExpressionNode* validate_wrap_convert(TxExpressionNode* originalExpr, const TxType* requiredType, bool _explicit) {
    // Note: Symbol table pass and semantic pass are not run on the created wrapper nodes.
    auto originalType = originalExpr->get_type();
    if (! originalType)
        return originalExpr;
    if (originalType == requiredType)
        return originalExpr;
    if (_explicit || requiredType->auto_converts_from(*originalType)) {
        // wrap originalExpr with conversion node
        if (auto scalarType = dynamic_cast<const TxScalarType*>(requiredType))
            return new TxScalarConvNode(originalExpr->parseLocation, originalExpr, scalarType);
        if (auto refType = dynamic_cast<const TxReferenceType*>(requiredType))
            return new TxReferenceConvNode(originalExpr->parseLocation, originalExpr, refType);
        if (auto arrayType = dynamic_cast<const TxArrayType*>(requiredType))
            return new TxObjSpecCastNode(originalExpr->parseLocation, originalExpr, arrayType);
        if (dynamic_cast<const TxFunctionType*>(requiredType))
            return originalExpr;  // or do we actually need to do something here?
    }
    else if (auto refType = dynamic_cast<const TxReferenceType*>(requiredType)) {
        auto refTargetType = refType->target_type();
        if (refTargetType && originalType->is_a(*refTargetType->get_type())) {
            if (refTargetType->get_type()->is_modifiable()) {
                if (!originalType->is_modifiable())
                    parser_error(originalExpr->parseLocation, "Cannot convert reference with non-mod-target to one with mod target: %s -> %s",
                                 originalType->to_string().c_str(), requiredType->to_string().c_str());
                else
                    parser_error(originalExpr->parseLocation, "Cannot implicitly convert to reference with modifiable target: %s -> %s",
                                 originalType->to_string().c_str(), requiredType->to_string().c_str());
                return originalExpr;
            }
            else {
                // wrap originalExpr with a reference-to node
                auto refToNode = new TxReferenceToNode(originalExpr->parseLocation, originalExpr);
                refToNode->set_context(originalExpr);
                return new TxReferenceConvNode(originalExpr->parseLocation, refToNode, refType);
            }
        }
    }
    parser_error(originalExpr->parseLocation, "Can't auto-convert %s -> %s",
                 originalType->to_string().c_str(), requiredType->to_string().c_str());
    return originalExpr;
}


TxExpressionNode* validate_wrap_assignment(TxExpressionNode* rValueExpr, const TxType* requiredType) {
    if (! requiredType->is_concrete())
        // TODO: dynamic concrete type resolution (recognize actual type in runtime when dereferencing a generic pointer)
        parser_error(rValueExpr->parseLocation, "Assignee is not a concrete type (size potentially unknown): %s", requiredType->to_string().c_str());
    // if assignee is a reference:
    // TODO: check dataspace rules
    return validate_wrap_convert(rValueExpr, requiredType);
}



TxSuiteNode::TxSuiteNode(const yy::location& parseLocation)
        : TxStatementNode(parseLocation), suite(new std::vector<TxStatementNode*>())  {
}

TxSuiteNode::TxSuiteNode(const yy::location& parseLocation, std::vector<TxStatementNode*>* suite)
        : TxStatementNode(parseLocation), suite(suite)  {
}



const std::vector<TxTypeParam>* TxTypeExpressionNode::makeTypeParams(const std::vector<TxDeclarationNode*>* typeParamDecls) {
    auto paramsVec = new std::vector<TxTypeParam>();
    for (auto decl : *typeParamDecls) {
        if (auto typeDecl = dynamic_cast<TxTypeDeclNode*>(decl))
            paramsVec->push_back(TxTypeParam(TxTypeParam::MetaType::TXB_TYPE, typeDecl->typeName, typeDecl->typeExpression));
        else if (auto valueDecl = dynamic_cast<TxFieldDeclNode*>(decl))
            paramsVec->push_back(TxTypeParam(TxTypeParam::MetaType::TXB_VALUE, valueDecl->field->ident, valueDecl->field));
    }
    return paramsVec;
}


void TxTypeDeclNode::wrap_alias() {
    if (dynamic_cast<TxIdentifiedTypeNode*>(this->typeExpression))
        this->typeExpression = new TxAliasedTypeNode(this->typeExpression->parseLocation, this->typeExpression);
}
