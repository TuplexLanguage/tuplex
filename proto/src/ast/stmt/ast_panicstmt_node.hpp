#pragma once

#include "ast_stmt_node.hpp"
#include "ast/expr/ast_expr_node.hpp"

/** Causes an unconditional 'panic'.
 * Currently this results in a program abort.
 * (In future this might be replaced with an exception mechanism.)
 */
class TxPanicStmtNode : public TxStatementNode {
    TxStatementNode* suite;

    TxPanicStmtNode( const TxLocation& parseLocation, TxStatementNode* suite )
        : TxStatementNode( parseLocation ), suite( suite )  { }

public:
    TxPanicStmtNode( const TxLocation& parseLocation, std::string message );

    virtual TxPanicStmtNode* make_ast_copy() const override {
        return new TxPanicStmtNode( this->parseLocation, this->suite->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        this->suite->symbol_resolution_pass();
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->suite->visit_ast( visitor, thisCursor, "suite", context );
    }
};
