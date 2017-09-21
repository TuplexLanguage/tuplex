#include "ast_assertstmt_node.hpp"

#include "ast/expr/ast_ref.hpp"
#include "ast/expr/ast_lit.hpp"
#include "ast/expr/ast_string.hpp"
#include "ast/expr/ast_op_exprs.hpp"
#include "ast_flow.hpp"


TxAssertStmtNode::TxAssertStmtNode( const TxLocation& ploc, TxExpressionNode* expr )
        : TxStatementNode( ploc ), expr( expr ) {
    auto invertedCond = new TxUnaryLogicalNotNode( expr->ploc, expr );

    // print assert failed message:
    /* example C assert failed message:
     txc: /home/christer/proj/workspace/proto/src/ast.cpp:515: TxAssertStmtNode::TxAssertStmtNode(const TxLocation&, TxExpressionNode*): Assertion `!this->expr' failed.
     */
    auto & pLoc = ploc;
    std::stringstream msg;
    //msg << procName << ": ";         // TODO: will need to be determined via system call
    if ( pLoc.begin.filename )
        msg << *pLoc.begin.filename;
    msg << ":" << pLoc.begin.line;
    //msg << ": " << srcFuncHeader;    // TODO: source text needed for this
    msg << ": Assertion failed";
    //msg << ": `" << srcExpr << "`";  // TODO: source text needed for this
    //msg << ": " << customMessage;    // TODO: supported custom assert message
    std::string assertFailedMsg = "c\"" + msg.str() + "\n\"";
    auto msgExpr = new TxReferenceToNode( pLoc, new TxCStringLitNode( pLoc, assertFailedMsg ) );
    auto stderrArg = new TxFieldValueNode( this->ploc, nullptr, "tx.c.stderr" );
    auto putsCallee = new TxFieldValueNode( pLoc, nullptr, "tx.c.fputs" );
    auto putsCallExpr = new TxFunctionCallNode( pLoc, putsCallee, new std::vector<TxExpressionNode*>( { msgExpr, stderrArg } ) );
    TxStatementNode* putsStmt = new TxCallStmtNode( pLoc, putsCallExpr );

    // we call c library abort() upon assertion failure
    auto abortCallee = new TxFieldValueNode( pLoc, nullptr, "tx.c.abort" );
    auto abortCallExpr = new TxFunctionCallNode( pLoc, abortCallee, new std::vector<TxExpressionNode*>(), true );
    TxStatementNode* abortStmt = new TxCallStmtNode( pLoc, abortCallExpr );

    auto failureSuite = new TxSuiteNode( pLoc, new std::vector<TxStatementNode*>( { putsStmt, abortStmt } ) );
    this->ifStmt = new TxIfStmtNode( pLoc, new TxCondClauseNode( pLoc, invertedCond ), failureSuite );
}
