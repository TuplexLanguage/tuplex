#pragma once

#include <vector>

#include "ast_stmt_node.hpp"
#include "ast_stmts.hpp"
#include "ast/expr/ast_expr_node.hpp"


class TxElseClauseNode : public TxStatementNode {
public:
    TxStatementNode* body;

    TxElseClauseNode( const TxLocation& parseLocation, TxStatementNode* suite )
            : TxStatementNode( parseLocation ), body( suite ) {
    }

    virtual TxElseClauseNode* make_ast_copy() const override {
        return new TxElseClauseNode( this->parseLocation, this->body->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        this->body->symbol_resolution_pass();
    }

    virtual bool may_end_with_non_return_stmt() const override {
        return this->body->may_end_with_non_return_stmt();
    }
    virtual bool ends_with_terminal_stmt() const override {
        return this->body->ends_with_terminal_stmt();
    }
    virtual bool ends_with_return_stmt() const override {
        return this->body->ends_with_return_stmt();
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->body->visit_ast( visitor, thisCursor, "body", context );
    }
};


class TxIfStmtNode : public TxStatementNode {
    TxMaybeConversionNode* cond;
    TxStatementNode* body;
    TxElseClauseNode* elseClause;

public:
    TxIfStmtNode( const TxLocation& parseLocation, TxExpressionNode* cond, TxStatementNode* body,
                  TxElseClauseNode* elseClause = nullptr )
            : TxStatementNode( parseLocation ), cond( new TxMaybeConversionNode( cond ) ), body( body ), elseClause( elseClause ) {
    }

    virtual TxIfStmtNode* make_ast_copy() const override {
        return new TxIfStmtNode( this->parseLocation, this->cond->originalExpr->make_ast_copy(), this->body->make_ast_copy(),
                                 ( this->elseClause ? this->elseClause->make_ast_copy() : nullptr ) );
    }

    virtual bool may_end_with_non_return_stmt() const override {
        return ( this->body->may_end_with_non_return_stmt() || ( this->elseClause && this->elseClause->may_end_with_non_return_stmt() ) );
    }
    virtual bool ends_with_terminal_stmt() const override {
        return ( this->body->ends_with_terminal_stmt() && this->elseClause && this->elseClause->ends_with_terminal_stmt() );
    }
    virtual bool ends_with_return_stmt() const override {
        return ( this->body->ends_with_return_stmt() && this->elseClause && this->elseClause->ends_with_return_stmt() );
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void symbol_resolution_pass() override {
        this->cond->insert_conversion( this->registry().get_builtin_type( TXBT_BOOL ) );
        this->cond->symbol_resolution_pass();
        this->body->symbol_resolution_pass();
        if ( this->elseClause )
            this->elseClause->symbol_resolution_pass();
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->cond->visit_ast( visitor, thisCursor, "condition", context );
        this->body->visit_ast( visitor, thisCursor, "then", context );
        if (this->elseClause)
            this->elseClause->visit_ast( visitor, thisCursor, "else", context );
    }
};


class TxLoopHeaderNode : public TxNode {
public:
    TxLoopHeaderNode( const TxLocation& parseLocation ) : TxNode( parseLocation ) { }

    virtual TxLoopHeaderNode* make_ast_copy() const override = 0;

    //virtual void symbol_resolution_pass() override = 0;

    virtual void         code_gen_init    ( LlvmGenerationContext& context, GenScope* scope ) const = 0;
    virtual llvm::Value* code_gen_cond    ( LlvmGenerationContext& context, GenScope* scope ) const = 0;
    virtual void         code_gen_prestep ( LlvmGenerationContext& context, GenScope* scope ) const = 0;
    virtual void         code_gen_poststep( LlvmGenerationContext& context, GenScope* scope ) const = 0;
};

class TxWhileHeaderNode : public TxLoopHeaderNode {
    TxExpressionNode* nextCond;

public:
    TxWhileHeaderNode( const TxLocation& parseLocation, TxExpressionNode* nextCond )
        : TxLoopHeaderNode( parseLocation ), nextCond( nextCond )  { }

    virtual TxWhileHeaderNode* make_ast_copy() const override {
        return new TxWhileHeaderNode( this->parseLocation, this->nextCond->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        this->nextCond->symbol_resolution_pass();
    }

    void         code_gen_init( LlvmGenerationContext& context, GenScope* scope ) const { }
    llvm::Value* code_gen_cond( LlvmGenerationContext& context, GenScope* scope ) const;
    void         code_gen_prestep( LlvmGenerationContext& context, GenScope* scope ) const { }
    void         code_gen_poststep( LlvmGenerationContext& context, GenScope* scope ) const { }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->nextCond->visit_ast( visitor, thisCursor, "cond", context );
    }
};

class TxForHeaderNode : public TxLoopHeaderNode {
    TxStatementNode*  initStmt;
    TxExpressionNode* nextCond;
    TxStatementNode*  stepStmt;

public:
    TxForHeaderNode( const TxLocation& parseLocation, TxStatementNode* initStmt, TxExpressionNode* nextCond, TxStatementNode* stepStmt )
        : TxLoopHeaderNode( parseLocation ), initStmt( initStmt ), nextCond( nextCond ), stepStmt( stepStmt )  { }

    virtual TxForHeaderNode* make_ast_copy() const override {
        return new TxForHeaderNode( this->parseLocation, this->initStmt->make_ast_copy(), this->nextCond->make_ast_copy(),
                                    this->stepStmt->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        this->initStmt->symbol_resolution_pass();
        this->nextCond->symbol_resolution_pass();
        this->stepStmt->symbol_resolution_pass();
    }

    void         code_gen_init( LlvmGenerationContext& context, GenScope* scope ) const;
    llvm::Value* code_gen_cond( LlvmGenerationContext& context, GenScope* scope ) const;
    void         code_gen_prestep( LlvmGenerationContext& context, GenScope* scope ) const { }
    void         code_gen_poststep( LlvmGenerationContext& context, GenScope* scope ) const;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->initStmt->visit_ast( visitor, thisCursor, "init", context );
        this->nextCond->visit_ast( visitor, thisCursor, "cond", context );
        this->stepStmt->visit_ast( visitor, thisCursor, "step", context );
    }
};

class TxInClauseNode : public TxLoopHeaderNode {
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
    void         code_gen_prestep( LlvmGenerationContext& context, GenScope* scope ) const;
    void         code_gen_poststep( LlvmGenerationContext& context, GenScope* scope ) const { }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->iterField->visit_ast( visitor, thisCursor, "iterator", context );
        this->nextCond->visit_ast( visitor, thisCursor, "cond", context );
        this->valueField->visit_ast( visitor, thisCursor, "value", context );
    }
};


class TxForStmtNode : public TxStatementNode {
    std::vector<TxLoopHeaderNode*>* loopHeaders;
    TxStatementNode* body;
    TxElseClauseNode* elseClause;

protected:
    virtual void stmt_declaration_pass() override {
        this->lexContext._scope = this->context().scope()->create_code_block_scope( *this, "for" );
    }

public:
    TxForStmtNode( const TxLocation& parseLocation, std::vector<TxLoopHeaderNode*>* loopHeaders, TxStatementNode* body,
                   TxElseClauseNode* elseClause = nullptr )
            : TxStatementNode( parseLocation ), loopHeaders( loopHeaders ), body( body ), elseClause( elseClause )  { }

    TxForStmtNode( const TxLocation& parseLocation, TxLoopHeaderNode* loopHeader, TxStatementNode* body, TxElseClauseNode* elseClause = nullptr )
            : TxForStmtNode( parseLocation, new std::vector<TxLoopHeaderNode*>( { loopHeader } ), body, elseClause )  { }

    virtual TxForStmtNode* make_ast_copy() const override {
        return new TxForStmtNode( this->parseLocation, make_node_vec_copy( this->loopHeaders ), body->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        for ( auto header : *this->loopHeaders )
            header->symbol_resolution_pass();
        this->body->symbol_resolution_pass();
        if ( this->elseClause )
            this->elseClause->symbol_resolution_pass();
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual bool may_end_with_non_return_stmt() const override {
        // FUTURE: handle break & continue that terminate statement outside this loop
        return false;
    }
    virtual bool ends_with_terminal_stmt() const override {
        // FUTURE: handle break & continue that terminate statement outside this loop
        //return ( this->body->ends_with_terminal_stmt() && this->elseClause && this->elseClause->ends_with_terminal_stmt() );
        return this->ends_with_return_stmt();
    }

    virtual bool ends_with_return_stmt() const override {
        return ( this->body->ends_with_return_stmt() && this->elseClause && this->elseClause->ends_with_return_stmt() );
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        for ( auto header : *this->loopHeaders )
            header->visit_ast( visitor, thisCursor, "header", context );
        this->body->visit_ast( visitor, thisCursor, "body", context );
        if ( this->elseClause )
            this->elseClause->visit_ast( visitor, thisCursor, "else", context );
    }
};
