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
    auto printCallee = new TxFieldValueNode( this->ploc, "tx.print_err" );
    auto printCallExpr = new TxFunctionCallNode( this->ploc, printCallee, new std::vector<TxExpressionNode*>( { panicMsgExpr } ) );
    TxStatementNode* printStmt = new TxCallStmtNode( this->ploc, printCallExpr );

    // we call c library abort() upon assertion failure
    auto abortCallee = new TxFieldValueNode( this->ploc, "tx.c.abort" );
    auto abortCallExpr = new TxFunctionCallNode( this->ploc, abortCallee, new std::vector<TxExpressionNode*>(), true );
    TxStatementNode* abortStmt = new TxCallStmtNode( this->ploc, abortCallExpr );

    //this->suite = new TxSuiteNode( this->ploc, new std::vector<TxStatementNode*>( { abortStmt } ) );
    this->suite = new TxSuiteNode( this->ploc, new std::vector<TxStatementNode*>( { printStmt, abortStmt } ) );
}

TxPanicStmtNode::TxPanicStmtNode( const TxLocation& ploc, const std::string& message )
        : TxStatementNode( ploc ) {
    std::stringstream msg;
    if ( this->ploc.begin.filename )
        msg << *this->ploc.begin.filename;
    msg << ":" << this->ploc.begin.line;
    msg << ": Panic: " << message;
    std::string panicMsg = "c\"" + msg.str() + "\n\"";
    auto msgExpr = new TxReferenceToNode( this->ploc, new TxCStringLitNode( this->ploc, panicMsg ) );

    auto stderrArg = new TxFieldValueNode( this->ploc, "tx.c.stderr" );
    auto putsCallee = new TxFieldValueNode( this->ploc, "tx.c.fputs" );
    auto putsCallExpr = new TxFunctionCallNode( this->ploc, putsCallee, new std::vector<TxExpressionNode*>( { msgExpr, stderrArg } ) );
    TxStatementNode* putsStmt = new TxCallStmtNode( this->ploc, putsCallExpr );

    // we call c library abort() upon assertion failure
    auto abortCallee = new TxFieldValueNode( this->ploc, "tx.c.abort" );
    auto abortCallExpr = new TxFunctionCallNode( this->ploc, abortCallee, new std::vector<TxExpressionNode*>(), true );
    TxStatementNode* abortStmt = new TxCallStmtNode( this->ploc, abortCallExpr );

    this->suite = new TxSuiteNode( this->ploc, new std::vector<TxStatementNode*>( { putsStmt, abortStmt } ) );
}
