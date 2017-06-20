#pragma once

#include "ast_stmt_node.hpp"
#include "ast/expr/ast_expr_node.hpp"

class TxAssertStmtNode : public TxStatementNode {
    TxExpressionNode* expr;
    TxStatementNode* ifStmt;

public:
    TxAssertStmtNode( const TxLocation& ploc, TxExpressionNode* expr );

    virtual TxAssertStmtNode* make_ast_copy() const override {
        return new TxAssertStmtNode( this->ploc, this->expr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        this->ifStmt->symbol_resolution_pass();
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->ifStmt->visit_ast( visitor, thisCursor, "ifstmt", context );
    }
};
