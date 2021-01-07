#pragma once

#include "ast/ast_node.hpp"

class TxStatementNode : public TxNode {
    TxScopeSymbol* successorScope = nullptr;
    friend class TxFieldStmtNode;
    friend class TxInitStmtNode;

protected:
    /** The predecessor that defines the scope of this statement. */
    TxStatementNode* predecessor = nullptr;
    friend class TxSuiteNode;
    friend class TxForStmtNode;
    friend class TxForHeaderNode;

    /** called by TxExpErrStmtNode on it's child statement */
    virtual void set_exp_error_stmt() { }
    friend class TxExpErrStmtNode;

    void declaration_pass() final {
        if (this->predecessor) {
            // place statement in the effective sub-scope of its predecessor
            this->lexContext._scope = this->predecessor->get_stmt_successor_scope();
        }
        this->successorScope = this->context().scope();
        this->stmt_declaration_pass();
    }

    /** Performs declaration pass operations on this statement node. To be overridden by subclasses as necessary. */
    virtual void stmt_declaration_pass() { }

    /** Returns the effective sub-scope for statements succeeding this one. */
    inline TxScopeSymbol* get_stmt_successor_scope() const {
        return successorScope;
    }

public:
    explicit TxStatementNode( const TxLocation& ploc )
            : TxNode( ploc ) {
    }

    TxStatementNode* make_ast_copy() const override = 0;

    /** Returns true if this statement / compound statement *may* end with a break or continue statement. */
    virtual bool may_end_with_non_return_stmt() const {
        return false;
    }

    /** Returns true if this statement / compound statement always ends with an explicit terminal statement
     * (return, break, continue) OR with an abnormal exit (abort/exit/exception).
     * This means that any successor statement in the same suite will never be reached. */
    virtual bool ends_with_terminal_stmt() const {
        return false;
    }

    /** Returns true if this statement / compound statement always ends with an explicit return statement,
     * OR with an abnormal exit (abort/exit/exception). */
    virtual bool ends_with_return_stmt() const {
        return false;
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const = 0;
};
