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

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->ifStmt->visit_ast( visitor, cursor, "ifstmt", aux );
    }
};
