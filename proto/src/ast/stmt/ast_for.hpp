#pragma once

#include <vector>

#include "ast_stmt_node.hpp"
#include "ast_stmts.hpp"
#include "ast/expr/ast_expr_node.hpp"

class TxElseClauseNode;

class TxInClauseNode : public TxNode {
    const std::string valueName;
    const std::string iterName;
    TxExpressionNode* origSeqExpr;
    TxFieldDefNode*   iterField = nullptr;
    TxExpressionNode* nextCond = nullptr;
    TxFieldDefNode*   valueField = nullptr;

protected:
    virtual void declaration_pass() override {
        //auto declScope = this->context().scope()->create_code_block_scope( *this );
        auto declScope = this->context().scope();
        this->iterField->declare_field( declScope, TXD_NONE, TXS_STACK );
        this->valueField->declare_field( declScope, TXD_NONE, TXS_STACK );
        // TODO: check: (to prevent init expr from referencing this field, it is processed in the 'outer' scope, not in the new block scope)
    }

public:
    TxInClauseNode( const TxLocation& parseLocation, const std::string& valueName, const std::string& initName, TxExpressionNode* seqExpr );

    TxInClauseNode( const TxLocation& parseLocation, const std::string& valueName, TxExpressionNode* seqExpr )
        : TxInClauseNode( parseLocation, valueName, "$iter", seqExpr )  { }

    virtual TxInClauseNode* make_ast_copy() const override {
        return new TxInClauseNode( this->parseLocation, this->valueName, this->origSeqExpr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        this->iterField->symbol_resolution_pass();
        this->nextCond->symbol_resolution_pass();
        this->valueField->symbol_resolution_pass();
    }

    void         code_gen_init( LlvmGenerationContext& context, GenScope* scope ) const;
    llvm::Value* code_gen_cond( LlvmGenerationContext& context, GenScope* scope ) const;
    void         code_gen_step( LlvmGenerationContext& context, GenScope* scope ) const;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->iterField->visit_ast( visitor, thisCursor, "iterator", context );
        this->nextCond->visit_ast( visitor, thisCursor, "cond", context );
        this->valueField->visit_ast( visitor, thisCursor, "value", context );
    }
};

class TxForStmtNode : public TxStatementNode {
    std::vector<TxInClauseNode*>* inClauses;
    TxStatementNode* body;
    TxElseClauseNode* elseClause;

protected:
    virtual void stmt_declaration_pass() override {
        this->lexContext._scope = this->context().scope()->create_code_block_scope( *this, "$for" );
    }

public:
    TxForStmtNode( const TxLocation& parseLocation, std::vector<TxInClauseNode*>* inClauses, TxStatementNode* body,
                   TxElseClauseNode* elseClause = nullptr )
            : TxStatementNode( parseLocation ), inClauses( inClauses ), body( body ), elseClause( elseClause ) {
    }

    virtual TxForStmtNode* make_ast_copy() const override {
        return new TxForStmtNode( this->parseLocation, make_node_vec_copy( this->inClauses ), body->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        for ( auto clause : *this->inClauses )
            clause->symbol_resolution_pass();
        this->body->symbol_resolution_pass();
        if ( this->elseClause )
            this->elseClause->symbol_resolution_pass();
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual bool ends_with_return_stmt() const override {
        return ( this->body->ends_with_return_stmt() && this->elseClause && this->elseClause->ends_with_return_stmt() );
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        for ( auto clause : *this->inClauses )
            clause->visit_ast( visitor, thisCursor, "in-clause", context );
        this->body->visit_ast( visitor, thisCursor, "body", context );
        if ( this->elseClause )
            this->elseClause->visit_ast( visitor, thisCursor, "else", context );
    }
};
