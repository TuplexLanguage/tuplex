#include "ast_panicstmt_node.hpp"

#include "ast/expr/ast_ref.hpp"
#include "ast/expr/ast_lit.hpp"
#include "ast/expr/ast_string.hpp"
#include "ast/expr/ast_op_exprs.hpp"
#include "ast/stmt/ast_stmts.hpp"


TxPanicStmtNode::TxPanicStmtNode( const TxLocation& ploc, TxExpressionNode* messageExpr )
        : TxStatementNode( ploc ) {
    std::stringstream header;
    header << '"' << *this->ploc.begin.filename << ":" << this->ploc.begin.line << ": Panic: " << '"';
    auto headerExpr = new TxStringLitNode( this->ploc, header.str() );
    auto panicMsgExpr = TxConcatenateStringsNode::make_strcat_node( this->ploc, headerExpr, messageExpr );
    auto printCallee = new TxFieldValueNode( this->ploc, nullptr, "tx.print" );
    auto printCallExpr = new TxFunctionCallNode( this->ploc, printCallee, new std::vector<TxExpressionNode*>( { panicMsgExpr } ) );
    TxStatementNode* printStmt = new TxCallStmtNode( this->ploc, printCallExpr );
    // TODO: emit it to stderr instead of stdout

    // we call c library abort() upon assertion failure
    auto abortCallee = new TxFieldValueNode( this->ploc, nullptr, "tx.c.abort" );
    auto abortCallExpr = new TxFunctionCallNode( this->ploc, abortCallee, new std::vector<TxExpressionNode*>(), true );
    TxStatementNode* abortStmt = new TxCallStmtNode( this->ploc, abortCallExpr );

    //this->suite = new TxSuiteNode( this->ploc, new std::vector<TxStatementNode*>( { abortStmt } ) );
    this->suite = new TxSuiteNode( this->ploc, new std::vector<TxStatementNode*>( { printStmt, abortStmt } ) );
}

TxPanicStmtNode::TxPanicStmtNode( const TxLocation& ploc, std::string message )
        : TxStatementNode( ploc ) {
    // print assert failed message:
    /* example C assert failed message:
     txc: /home/christer/proj/workspace/proto/src/ast.cpp:515: TxAssertStmtNode::TxAssertStmtNode(const TxLocation&, TxExpressionNode*): Assertion `!this->expr' failed.
     */
    std::stringstream msg;
    //msg << procName << ": ";         // TODO: will need to be determined via system call
    msg << *this->ploc.begin.filename << ":" << this->ploc.begin.line;
    msg << ": Panic: " << message;
    std::string panicMsg = "c\"" + msg.str() + "\"";
    auto msgExpr = new TxCStringLitNode( this->ploc, panicMsg );
    //auto convStrExpr = new TxReferenceToNode( this->ploc, new TxElemDerefNode( this->ploc, msgExpr,
    //                                                                           new TxIntegerLitNode( this->ploc, 0, false ), true ) );
    auto putsCallee = new TxFieldValueNode( this->ploc, nullptr, "tx.c.puts" );
    auto putsCallExpr = new TxFunctionCallNode( this->ploc, putsCallee, new std::vector<TxExpressionNode*>( { msgExpr } ) );
    TxStatementNode* putsStmt = new TxCallStmtNode( this->ploc, putsCallExpr );
    // TODO: emit it to stderr instead of stdout

    // we call c library abort() upon assertion failure
    auto abortCallee = new TxFieldValueNode( this->ploc, nullptr, "tx.c.abort" );
    auto abortCallExpr = new TxFunctionCallNode( this->ploc, abortCallee, new std::vector<TxExpressionNode*>(), true );
    TxStatementNode* abortStmt = new TxCallStmtNode( this->ploc, abortCallExpr );

    this->suite = new TxSuiteNode( this->ploc, new std::vector<TxStatementNode*>( { putsStmt, abortStmt } ) );
}
