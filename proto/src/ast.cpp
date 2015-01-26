#include "ast.hpp"


TxExpressionNode* wrapConversion(TxSymbolScope* scope, TxExpressionNode* originalExpr, const TxType* requiredType,
                                 bool _explicit) {
    auto originalType = originalExpr->get_type();
    if (! originalType)
        return originalExpr;
    if (originalType == requiredType)
        return originalExpr;
    if (_explicit || requiredType->autoConvertsFrom(*originalType)) {
        // wrap originalExpr with cast instruction node
        if (auto scalar_type = dynamic_cast<const TxScalarType*>(requiredType))
            return new TxScalarCastNode(originalExpr->parseLocation, originalExpr, scalar_type);
        if (auto ref_type = dynamic_cast<const TxReferenceType*>(requiredType))
            return new TxToPointerCastNode(originalExpr->parseLocation, originalExpr, ref_type);
        if (auto array_type = dynamic_cast<const TxArrayType*>(requiredType))
            return new TxObjSpecCastNode(originalExpr->parseLocation, originalExpr, array_type);
        if (dynamic_cast<const TxFunctionType*>(requiredType))
            return originalExpr;  // or do we actually need to do something here?
        // FUTURE: fill in non-scalar auto-conversions
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
    // attach else-clauses to preceding conditional statements:
    std::vector<TxStatementNode*> tmpSuite;
    for (TxStatementNode* stmt : *suite) {
        //if (auto elseClause = dynamic_cast<TxElseClauseNode*>(stmt)) {
            /*  now handled by grammar instead
            if (!tmpSuite.empty())
                if (auto compStmt = dynamic_cast<TxCondCompoundStmtNode*>(tmpSuite.back())) {
                    if (compStmt->has_else_clause())
                        parser_error(elseClause->parseLocation, "preceding if-statement already has an else-clause");
                    else
                        compStmt->set_else_clause(elseClause);
                    continue;
                }
            */
        //    parser_error(elseClause->parseLocation, "else-clause does not match a preceding conditional clause");
        //}
        //else
            tmpSuite.push_back(stmt);
    }
    *this->suite = tmpSuite;
}
