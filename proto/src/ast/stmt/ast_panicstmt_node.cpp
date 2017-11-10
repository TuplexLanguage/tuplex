#include "ast_panicstmt_node.hpp"

#include "ast/expr/ast_ref.hpp"
#include "ast/expr/ast_lit.hpp"
#include "ast/expr/ast_string.hpp"
#include "ast/expr/ast_op_exprs.hpp"
#include "ast/stmt/ast_stmts.hpp"


static TxStatementNode* make_output_stmt( const TxLocation& ploc, TxExpressionNode* messageExpr ) {
    if ( auto cstringMsg = dynamic_cast<TxCStringLitNode*>( messageExpr ) ) {
        auto msgExpr = new TxReferenceToNode( ploc, cstringMsg );
        auto stderrArg = new TxFieldValueNode( ploc, "tx.c.stderr" );
        auto putsCallee = new TxFieldValueNode( ploc, "tx.c.fputs" );
        auto putsCallExpr = new TxFunctionCallNode( ploc, putsCallee, new std::vector<TxExpressionNode*>( { msgExpr, stderrArg } ) );
        return new TxCallStmtNode( ploc, putsCallExpr );
    }
    else {
        auto printCallee = new TxFieldValueNode( ploc, "tx.print_err" );
        auto printCallExpr = new TxFunctionCallNode( ploc, printCallee, new std::vector<TxExpressionNode*>( { messageExpr } ) );
        return new TxCallStmtNode( ploc, printCallExpr );
    }
}

static TxSuiteNode* make_panic_suite( const TxLocation& ploc, TxExpressionNode* panicMsgExpr ) {
    TxStatementNode* printStmt = make_output_stmt( ploc, panicMsgExpr );

    // we call c library abort() upon assertion failure
    auto abortCallee = new TxFieldValueNode( ploc, "tx.c.abort" );
    auto abortCallExpr = new TxFunctionCallNode( ploc, abortCallee, new std::vector<TxExpressionNode*>(), true );
    TxStatementNode* abortStmt = new TxCallStmtNode( ploc, abortCallExpr );

    return new TxSuiteNode( ploc, new std::vector<TxStatementNode*>( { printStmt, abortStmt } ) );
}

TxPanicStmtNode::TxPanicStmtNode( const TxLocation& ploc, TxExpressionNode* messageExpr )
        : TxStatementNode( ploc ) {
//    std::stringstream header;
//    header << '"' << *this->ploc.begin.filename << ":" << this->ploc.begin.line << ": Panic: " << '"';
//    auto headerExpr = new TxStringLitNode( this->ploc, header.str() );
//    auto panicMsgExpr = TxConcatenateStringsNode::make_strcat_node( this->ploc, headerExpr, messageExpr );
    auto panicMsgExpr = messageExpr;  // FIXME: temporary, instead of above

//    TxStatementNode* printStmt = make_output_stmt( this->ploc, panicMsgExpr );
////    auto printCallee = new TxFieldValueNode( this->ploc, "tx.print_err" );
////    auto printCallExpr = new TxFunctionCallNode( this->ploc, printCallee, new std::vector<TxExpressionNode*>( { panicMsgExpr } ) );
////    TxStatementNode* printStmt = new TxCallStmtNode( this->ploc, printCallExpr );
//
//    // we call c library abort() upon assertion failure
//    auto abortCallee = new TxFieldValueNode( this->ploc, "tx.c.abort" );
//    auto abortCallExpr = new TxFunctionCallNode( this->ploc, abortCallee, new std::vector<TxExpressionNode*>(), true );
//    TxStatementNode* abortStmt = new TxCallStmtNode( this->ploc, abortCallExpr );
//
//    this->suite = new TxSuiteNode( this->ploc, new std::vector<TxStatementNode*>( { printStmt, abortStmt } ) );
    this->suite = make_panic_suite( this->ploc, panicMsgExpr );
}

TxPanicStmtNode::TxPanicStmtNode( const TxLocation& ploc, const std::string& message )
        : TxStatementNode( ploc ) {
    std::stringstream msg;
    if ( this->ploc.begin.filename )
        msg << *this->ploc.begin.filename;
    msg << ":" << this->ploc.begin.line;
    msg << ": Panic: " << message;
    std::string panicMsg = "c\"" + msg.str() + "\n\"";
    auto panicMsgExpr = new TxCStringLitNode( this->ploc, panicMsg );
//    TxStatementNode* printStmt = make_output_stmt( this->ploc, panicMsgExpr );
////    auto msgExpr = new TxReferenceToNode( this->ploc, new TxCStringLitNode( this->ploc, panicMsg ) );
////    auto stderrArg = new TxFieldValueNode( this->ploc, "tx.c.stderr" );
////    auto putsCallee = new TxFieldValueNode( this->ploc, "tx.c.fputs" );
////    auto putsCallExpr = new TxFunctionCallNode( this->ploc, putsCallee, new std::vector<TxExpressionNode*>( { msgExpr, stderrArg } ) );
////    TxStatementNode* putsStmt = new TxCallStmtNode( this->ploc, putsCallExpr );
//
//    // we call c library abort() upon assertion failure
//    auto abortCallee = new TxFieldValueNode( this->ploc, "tx.c.abort" );
//    auto abortCallExpr = new TxFunctionCallNode( this->ploc, abortCallee, new std::vector<TxExpressionNode*>(), true );
//    TxStatementNode* abortStmt = new TxCallStmtNode( this->ploc, abortCallExpr );
//
//    this->suite = new TxSuiteNode( this->ploc, new std::vector<TxStatementNode*>( { putsStmt, abortStmt } ) );
    this->suite = make_panic_suite( this->ploc, panicMsgExpr );
}
