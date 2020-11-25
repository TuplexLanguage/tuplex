#pragma once

#include "ast_stmt_node.hpp"
#include "ast/expr/ast_expr_node.hpp"

class TxDeleteStmtNode : public TxStatementNode {
    TxExpressionNode* expr;

protected:
    void verification_pass() const {
        // FUTURE: check proper data space of reference
        auto refType = this->expr->qtype();
        if ( refType->get_type_class() != TXTC_REFERENCE )
            CERROR( this, "Expression of 'delete' is not a reference: " << refType );
    }

public:
    TxDeleteStmtNode( const TxLocation& ploc, TxExpressionNode* expr )
            : TxStatementNode( ploc ), expr( expr ) {
    }

    virtual TxDeleteStmtNode* make_ast_copy() const override {
        return new TxDeleteStmtNode( this->ploc, this->expr->make_ast_copy() );
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->expr->visit_ast( visitor, thisCursor, "expr", context );
    }
};
