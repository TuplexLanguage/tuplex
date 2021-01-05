#pragma once

#include <vector>

#include "ast_stmt_node.hpp"
#include "ast_stmts.hpp"
#include "ast/expr/ast_expr_node.hpp"


class TxElseClauseNode : public TxStatementNode {
public:
    TxStatementNode* body;

    TxElseClauseNode( const TxLocation& ploc, TxStatementNode* suite )
            : TxStatementNode( ploc ), body( suite ) {
    }

    virtual TxElseClauseNode* make_ast_copy() const override {
        return new TxElseClauseNode( this->ploc, this->body->make_ast_copy() );
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

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->body->visit_ast( visitor, cursor, "body", aux );
    }
};


class TxFlowHeaderNode : public TxNode {
public:
    TxFlowHeaderNode( const TxLocation& ploc ) : TxNode( ploc ) { }

    virtual TxFlowHeaderNode* make_ast_copy() const override = 0;

    /** Returns the sub-scope-defining statement of this loop header, or null if none. */
    virtual TxStatementNode* get_stmt_predecessor() const {
        return nullptr;
    }

    virtual void         code_gen_init    ( LlvmGenerationContext& context, GenScope* scope ) const = 0;
    virtual llvm::Value* code_gen_cond    ( LlvmGenerationContext& context, GenScope* scope ) const = 0;
    virtual void         code_gen_prestep ( LlvmGenerationContext& context, GenScope* scope ) const = 0;
    virtual void         code_gen_poststep( LlvmGenerationContext& context, GenScope* scope ) const = 0;
};


class TxCondClauseNode : public TxFlowHeaderNode {
    TxMaybeConversionNode* condExpr;

protected:
    virtual void resolution_pass() override {
        this->condExpr->insert_conversion( TXR_FULL_RESOLUTION, this->registry().get_builtin_type( TXBT_BOOL ) );
    }

public:
    TxCondClauseNode( const TxLocation& ploc, TxExpressionNode* condExpr )
        : TxFlowHeaderNode( ploc ), condExpr( new TxMaybeConversionNode( condExpr ) )  { }

    virtual TxCondClauseNode* make_ast_copy() const override {
        return new TxCondClauseNode( this->ploc, this->condExpr->originalExpr->make_ast_copy() );
    }

    virtual void         code_gen_init( LlvmGenerationContext& context, GenScope* scope ) const override { }
    virtual llvm::Value* code_gen_cond( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual void         code_gen_prestep( LlvmGenerationContext& context, GenScope* scope ) const override { }
    virtual void         code_gen_poststep( LlvmGenerationContext& context, GenScope* scope ) const override { }

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->condExpr->visit_ast( visitor, cursor, "cond", aux );
    }
};


class TxIsClauseNode : public TxFlowHeaderNode {
    TxExpressionNode* origValueExpr;
    TxIdentifierNode* valueName;
    TxQualTypeExprNode* typeExpr;
    TxLocalFieldDefNode* valueField = nullptr;

protected:
    virtual void declaration_pass() override {
        auto declScope = this->context().scope();
        this->valueField->declare_field( this, declScope, TXD_NONE, TXS_STACK );
    }

    virtual void verification_pass() const override;

public:
    TxIsClauseNode( const TxLocation& ploc, TxExpressionNode* valueExpr, TxIdentifierNode* valueName, TxQualTypeExprNode* typeExpr );

    virtual TxIsClauseNode* make_ast_copy() const override {
        return new TxIsClauseNode( this->ploc, this->origValueExpr->make_ast_copy(),
                                   this->valueName->make_ast_copy(), this->typeExpr->make_ast_copy() );
    }

    virtual void         code_gen_init( LlvmGenerationContext& context, GenScope* scope ) const override { }
    virtual llvm::Value* code_gen_cond( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual void         code_gen_prestep( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual void         code_gen_poststep( LlvmGenerationContext& context, GenScope* scope ) const override { }

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->valueField->visit_ast( visitor, cursor, "value", aux );
    }
};


class TxForHeaderNode : public TxFlowHeaderNode {
    TxStatementNode* initStmt;
    TxExprStmtNode*  nextCond;
    TxStatementNode* stepStmt;

public:
    TxForHeaderNode( const TxLocation& ploc, TxStatementNode* initStmt, TxExpressionNode* nextCond, TxStatementNode* stepStmt )
        : TxFlowHeaderNode( ploc ), initStmt( initStmt ),
          nextCond( new TxExprStmtNode( nextCond ) ), stepStmt( stepStmt ) {
        this->nextCond->predecessor = this->initStmt;
        this->stepStmt->predecessor = this->nextCond;
    }

    virtual TxForHeaderNode* make_ast_copy() const override {
        return new TxForHeaderNode( this->ploc, this->initStmt->make_ast_copy(), this->nextCond->expr->make_ast_copy(),
                                    this->stepStmt->make_ast_copy() );
    }

    virtual TxStatementNode* get_stmt_predecessor() const override {
        return this->initStmt;
    }

    virtual void         code_gen_init( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_cond( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual void         code_gen_prestep( LlvmGenerationContext& context, GenScope* scope ) const override { }
    virtual void         code_gen_poststep( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->initStmt->visit_ast( visitor, cursor, "init", aux );
        this->nextCond->visit_ast( visitor, cursor, "cond", aux );
        this->stepStmt->visit_ast( visitor, cursor, "step", aux );
    }
};

class TxInClauseNode : public TxFlowHeaderNode {
    TxIdentifierNode* valueName;
    TxIdentifierNode* iterName;
    TxExpressionNode* origSeqExpr;
    TxLocalFieldDefNode*   iterField = nullptr;
    TxExpressionNode* nextCond = nullptr;
    TxLocalFieldDefNode*   valueField = nullptr;
    TxDeclarationFlags iterDeclFlags;

    TxInClauseNode( const TxLocation& ploc, TxIdentifierNode* valueName, TxIdentifierNode* iterName,
                    TxExpressionNode* seqExpr, TxDeclarationFlags iterDeclFlags );

protected:
    virtual void declaration_pass() override {
        //auto declScope = this->context().scope()->create_code_block_scope( *this );
        auto declScope = this->context().scope();
        this->iterField->declare_field( this, declScope, this->iterDeclFlags, TXS_STACK );
        this->valueField->declare_field( this, declScope, TXD_NONE, TXS_STACK );
        // TODO: check: (to prevent init expr from referencing this field, it is processed in the 'outer' scope, not in the new block scope)
    }

public:
    TxInClauseNode( const TxLocation& ploc, TxIdentifierNode* valueName, TxIdentifierNode* iterName, TxExpressionNode* seqExpr )
            : TxInClauseNode( ploc, valueName, iterName, seqExpr, TXD_NONE ) {
    }
    TxInClauseNode( const TxLocation& ploc, TxIdentifierNode* valueName, TxExpressionNode* seqExpr )
            : TxInClauseNode( ploc, valueName, new TxIdentifierNode(valueName->ploc, valueName->ident() + "$iter"), seqExpr, TXD_IMPLICIT ) {
    }

    virtual TxInClauseNode* make_ast_copy() const override {
        return new TxInClauseNode( this->ploc, this->valueName->make_ast_copy(), this->iterName->make_ast_copy(),
                                   this->origSeqExpr->make_ast_copy(), this->iterDeclFlags );
    }

    virtual void         code_gen_init( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_cond( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual void         code_gen_prestep( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual void         code_gen_poststep( LlvmGenerationContext& context, GenScope* scope ) const override { }

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->iterField->visit_ast( visitor, cursor, "iterator", aux );
        this->nextCond->visit_ast( visitor, cursor, "cond", aux );
        this->valueField->visit_ast( visitor, cursor, "value", aux );
    }
};


class TxIfStmtNode : public TxStatementNode {
    TxFlowHeaderNode* header;
    TxStatementNode* body;
    TxElseClauseNode* elseClause;

protected:
    virtual void stmt_declaration_pass() override {
        this->lexContext._scope = this->context().scope()->create_code_block_scope( *this, "if" );
    }

public:
    TxIfStmtNode( const TxLocation& ploc, TxFlowHeaderNode* header, TxStatementNode* body,
                  TxElseClauseNode* elseClause = nullptr )
            : TxStatementNode( ploc ), header( header ), body( body ), elseClause( elseClause ) {
    }

    virtual TxIfStmtNode* make_ast_copy() const override {
        return new TxIfStmtNode( this->ploc, this->header->make_ast_copy(), this->body->make_ast_copy(),
                                 ( this->elseClause ? this->elseClause->make_ast_copy() : nullptr ) );
    }

    const TxStatementNode* get_true_body() const { return this->body; }
    const TxStatementNode* get_else_body() const { return ( this->elseClause ? this->elseClause->body : nullptr ); }

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

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->header->visit_ast( visitor, cursor, "header", aux );
        this->body->visit_ast( visitor, cursor, "then", aux );
        if (this->elseClause)
            this->elseClause->visit_ast( visitor, cursor, "else", aux );
    }
};


class TxForStmtNode : public TxStatementNode {
    std::vector<TxFlowHeaderNode*>* loopHeaders;
    TxStatementNode* body;
    TxElseClauseNode* elseClause;

protected:
    virtual void stmt_declaration_pass() override {
        this->lexContext._scope = this->context().scope()->create_code_block_scope( *this, "lp" );
    }

public:
    TxForStmtNode( const TxLocation& ploc, std::vector<TxFlowHeaderNode*>* loopHeaders, TxStatementNode* body,
                   TxElseClauseNode* elseClause = nullptr )
            : TxStatementNode( ploc ), loopHeaders( loopHeaders ), body( body ), elseClause( elseClause )  {
        ASSERT( !loopHeaders->empty(), "loopHeaders empty" );
        this->body->predecessor = this->loopHeaders->back()->get_stmt_predecessor();
        if ( this->elseClause )
            this->elseClause->predecessor = this->body->predecessor;
    }

    TxForStmtNode( const TxLocation& ploc, TxFlowHeaderNode* loopHeader, TxStatementNode* body, TxElseClauseNode* elseClause = nullptr )
            : TxForStmtNode( ploc, new std::vector<TxFlowHeaderNode*>( { loopHeader } ), body, elseClause )  { }

    virtual TxForStmtNode* make_ast_copy() const override {
        return new TxForStmtNode( this->ploc, make_node_vec_copy( this->loopHeaders ), body->make_ast_copy() );
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

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        for ( auto header : *this->loopHeaders )
            header->visit_ast( visitor, cursor, "header", aux );
        this->body->visit_ast( visitor, cursor, "body", aux );
        if ( this->elseClause )
            this->elseClause->visit_ast( visitor, cursor, "else", aux );
    }
};
