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


TxExpressionNode* wrapConversion(TxSymbolScope* scope, TxExpressionNode* originalExpr, const TxType* requiredType,
                                 bool _explicit) {
    auto originalType = originalExpr->get_type();
    if (! originalType)
        return originalExpr;
    if (originalType == requiredType)
        return originalExpr;
    if (_explicit || requiredType->auto_converts_from(*originalType)) {
        // wrap originalExpr with cast instruction node
        if (auto scalar_type = dynamic_cast<const TxScalarType*>(requiredType))
            return new TxScalarCastNode(originalExpr->parseLocation, originalExpr, scalar_type);
        if (auto ref_type = dynamic_cast<const TxReferenceType*>(requiredType))
            return new TxToPointerCastNode(originalExpr->parseLocation, originalExpr, ref_type);
        if (auto array_type = dynamic_cast<const TxArrayType*>(requiredType))
            return new TxObjSpecCastNode(originalExpr->parseLocation, originalExpr, array_type);
        if (dynamic_cast<const TxFunctionType*>(requiredType))
            return originalExpr;  // or do we actually need to do something here?
    }
    parser_error(originalExpr->parseLocation, "Can't auto-convert %s -> %s",
                 originalType->to_string().c_str(), requiredType->to_string().c_str());
    return originalExpr;
}


TxSuiteNode::TxSuiteNode(const yy::location& parseLocation)
        : TxStatementNode(parseLocation), suite(new std::vector<TxStatementNode*>())  {
}

TxSuiteNode::TxSuiteNode(const yy::location& parseLocation, std::vector<TxStatementNode*>* suite)
        : TxStatementNode(parseLocation), suite(suite)  {
//    // attach else-clauses to preceding conditional statements:
//    std::vector<TxStatementNode*> tmpSuite;
//    for (TxStatementNode* stmt : *suite) {
//        //if (auto elseClause = dynamic_cast<TxElseClauseNode*>(stmt)) {
//            /*  now handled by grammar instead
//            if (!tmpSuite.empty())
//                if (auto compStmt = dynamic_cast<TxCondCompoundStmtNode*>(tmpSuite.back())) {
//                    if (compStmt->has_else_clause())
//                        parser_error(elseClause->parseLocation, "preceding if-statement already has an else-clause");
//                    else
//                        compStmt->set_else_clause(elseClause);
//                    continue;
//                }
//            */
//        //    parser_error(elseClause->parseLocation, "else-clause does not match a preceding conditional clause");
//        //}
//        //else
//            tmpSuite.push_back(stmt);
//    }
//    *this->suite = tmpSuite;
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
