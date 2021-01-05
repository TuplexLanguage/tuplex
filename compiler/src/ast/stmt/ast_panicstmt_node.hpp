#pragma once

#include "ast_stmt_node.hpp"
#include "ast/expr/ast_expr_node.hpp"

/** Causes an unconditional 'panic'.
 * Currently this results in a program abort.
 * (In future this might be replaced with an exception mechanism.)
 */
class TxPanicStmtNode : public TxStatementNode {
    TxStatementNode* suite;

    TxPanicStmtNode( const TxLocation& ploc, TxStatementNode* suite )
        : TxStatementNode( ploc ), suite( suite )  { }

public:
    TxPanicStmtNode( const TxLocation& ploc, TxExpressionNode* messageExpr );

    TxPanicStmtNode( const TxLocation& ploc, const std::string& message );

    virtual TxPanicStmtNode* make_ast_copy() const override {
        return new TxPanicStmtNode( this->ploc, this->suite->make_ast_copy() );
    }

    virtual bool ends_with_terminal_stmt() const override final {
        return true;
    }

    virtual bool ends_with_return_stmt() const override final {
        return true;
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->suite->visit_ast( visitor, cursor, "suite", aux );
    }
};
