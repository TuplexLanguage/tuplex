#include "ast_panicstmt_node.hpp"

#include "ast/expr/ast_ref.hpp"
#include "ast/expr/ast_lit.hpp"
#include "ast/expr/ast_string.hpp"
#include "ast/expr/ast_op_exprs.hpp"
#include "ast/stmt/ast_stmts.hpp"


TxPanicStmtNode::TxPanicStmtNode( const TxLocation& parseLocation, std::string message )
        : TxStatementNode( parseLocation ) {
    // print assert failed message:
    /* example C assert failed message:
     txc: /home/christer/proj/workspace/proto/src/ast.cpp:515: TxAssertStmtNode::TxAssertStmtNode(const TxLocation&, TxExpressionNode*): Assertion `!this->expr' failed.
     */
    auto & pLoc = parseLocation;
    std::stringstream msg;
    //msg << procName << ": ";         // TODO: will need to be determined via system call
    msg << *pLoc.begin.filename << ":" << pLoc.begin.line;
    msg << ": Panic: " << message;
    std::string assertFailedMsg = "c\"" + msg.str() + "\"";
    auto msgExpr = new TxCStringLitNode( pLoc, assertFailedMsg );
    auto convStrExpr = new TxReferenceToNode( pLoc, new TxElemDerefNode( pLoc, msgExpr, new TxIntegerLitNode( pLoc, 0, false ), true ) );
    auto putsCallee = new TxFieldValueNode( pLoc, nullptr, "tx.c.puts" );
    auto putsCallExpr = new TxFunctionCallNode( pLoc, putsCallee, new std::vector<TxExpressionNode*>( { convStrExpr } ) );
    TxStatementNode* putsStmt = new TxCallStmtNode( pLoc, putsCallExpr );
    // TODO: emit it to stderr instead of stdout

    // we call c library abort() upon assertion failure
    auto abortCallee = new TxFieldValueNode( pLoc, nullptr, "tx.c.abort" );
    auto abortCallExpr = new TxFunctionCallNode( pLoc, abortCallee, new std::vector<TxExpressionNode*>(), true );
    TxStatementNode* abortStmt = new TxCallStmtNode( pLoc, abortCallExpr );

    this->suite = new TxSuiteNode( pLoc, new std::vector<TxStatementNode*>( { putsStmt, abortStmt } ) );
}
