#pragma once

#include "ast_stmt_node.hpp"

#include "ast/ast_fielddef_node.hpp"
#include "ast/expr/ast_assignee_node.hpp"
#include "ast/expr/ast_exprs.hpp"


/** Local field declaration */
class TxFieldStmtNode : public TxStatementNode {
    /** the effective sub-scope for statements succeeding this one */
    TxScopeSymbol* blockScope = nullptr;
    TxDeclarationFlags declFlags = TXD_NONE;

protected:
    virtual void set_exp_error_stmt() override {
        this->declFlags = TXD_EXPERRBLOCK;
    }

    virtual void stmt_declaration_pass() override {
        this->blockScope = lexContext.scope()->create_code_block_scope( *this );
        this->field->declare_field( this->blockScope, this->declFlags, TXS_STACK );
        // (to prevent init expr from referencing this field, it is processed in the 'outer' scope, not in the new block scope)
    }

    virtual TxScopeSymbol* get_stmt_successor_scope() const override {
        return this->blockScope;
    }

public:
    TxFieldDefNode* field;

    TxFieldStmtNode( const TxLocation& parseLocation, TxFieldDefNode* field )
            : TxStatementNode( parseLocation ), field( field ) {
    }

    virtual TxFieldStmtNode* make_ast_copy() const override {
        return new TxFieldStmtNode( this->parseLocation, this->field->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        this->field->symbol_resolution_pass();
        if ( !field->initExpression ) {
            // TODO: instead check that TXS_STACK fields are initialized before first use
            //CWARNING(this, "Local field without initializer: " << this->field->get_source_field_name());
        }
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->field->visit_ast( visitor, thisCursor, "fielddecl", context );
    }
};

/** Local type declaration */
class TxTypeStmtNode : public TxStatementNode {
protected:
    virtual void set_exp_error_stmt() override {
        this->typeDecl->declFlags |= TXD_EXPERRBLOCK;
    }

public:
    TxTypeDeclNode* const typeDecl;

    TxTypeStmtNode( const TxLocation& parseLocation, TxTypeDeclNode* typeDecl )
            : TxStatementNode( parseLocation ), typeDecl( typeDecl ) {
    }

    TxTypeStmtNode( const TxLocation& parseLocation, const std::string typeName,
                    const std::vector<TxDeclarationNode*>* typeParamDecls,
                    TxTypeExpressionNode* typeExpression, bool interfaceKW = false, bool mutableType = false )
            : TxTypeStmtNode( parseLocation, new TxTypeDeclNode( parseLocation, TXD_NONE, typeName, typeParamDecls, typeExpression,
                                                                 interfaceKW, mutableType ) )
    {
    }

    virtual TxTypeStmtNode* make_ast_copy() const override {
        return new TxTypeStmtNode( this->parseLocation, this->typeDecl->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        this->typeDecl->symbol_resolution_pass();
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->typeDecl->visit_ast( visitor, thisCursor, "typedecl", context );
    }
};

class TxCallStmtNode : public TxStatementNode {  // function call without assigning return value (if any)
public:
    TxFunctionCallNode* call;

    TxCallStmtNode( const TxLocation& parseLocation, TxFunctionCallNode* call )
            : TxStatementNode( parseLocation ), call( call ) {
    }

    virtual TxCallStmtNode* make_ast_copy() const override {
        return new TxCallStmtNode( this->parseLocation, this->call->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        //( (TxExpressionNode*) this->call )->symbol_resolution_pass();
        this->call->symbol_resolution_pass();
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->call->visit_ast( visitor, thisCursor, "call", context );
    }
};

class TxTerminalStmtNode : public TxStatementNode {
protected:
    TxTerminalStmtNode( const TxLocation& parseLocation )
            : TxStatementNode( parseLocation ) {
    }

    virtual void symbol_resolution_pass() override {
    }

    virtual bool ends_with_terminal_stmt() const override final {
        return true;
    }
};

class TxReturnStmtNode : public TxTerminalStmtNode {
public:
    TxMaybeConversionNode* expr;

    TxReturnStmtNode( const TxLocation& parseLocation )
            : TxTerminalStmtNode( parseLocation ), expr() {
    }
    TxReturnStmtNode( const TxLocation& parseLocation, TxExpressionNode* expr )
            : TxTerminalStmtNode( parseLocation ), expr( new TxMaybeConversionNode( expr ) ) {
    }

    virtual TxReturnStmtNode* make_ast_copy() const override {
        return new TxReturnStmtNode( this->parseLocation, this->expr->originalExpr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        // TODO: Fix so that this won't find false positive using outer function's $return typeDecl
        // TODO: Illegal to return reference to STACK dataspace
        if ( auto returnDecl = lookup_field( this->context().scope(), TxIdentifier( "$return" ) ) ) {
            if ( this->expr ) {
                if ( auto field = returnDecl->get_definer()->resolve_field() )
                    this->expr->insert_conversion( field->get_type() );
                this->expr->symbol_resolution_pass();
            }
            else
                CERROR( this, "Return statement has no value expression although function returns " << returnDecl );
        }
        else if ( this->expr )
            CERROR( this, "Return statement has value expression although function has no return type" );
    }

    virtual bool ends_with_return_stmt() const override {
        return true;
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if (this->expr)
            this->expr->visit_ast( visitor, thisCursor, "value", context );
    }
};

class TxBreakStmtNode : public TxTerminalStmtNode {
public:
    TxBreakStmtNode( const TxLocation& parseLocation )
            : TxTerminalStmtNode( parseLocation ) {
    }

    virtual TxBreakStmtNode* make_ast_copy() const override {
        return new TxBreakStmtNode( this->parseLocation );
    }

    virtual bool may_end_with_non_return_stmt() const override {
        return true;
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }
};

class TxContinueStmtNode : public TxTerminalStmtNode {
public:
    TxContinueStmtNode( const TxLocation& parseLocation )
            : TxTerminalStmtNode( parseLocation ) {
    }

    virtual TxContinueStmtNode* make_ast_copy() const override {
        return new TxContinueStmtNode( this->parseLocation );
    }

    virtual bool may_end_with_non_return_stmt() const override {
        return true;
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }
};

class TxSuiteNode : public TxStatementNode {
protected:
    virtual void stmt_declaration_pass() override;

public:
    std::vector<TxStatementNode*>* suite;

    TxSuiteNode( const TxLocation& parseLocation, std::vector<TxStatementNode*>* suite )
            : TxStatementNode( parseLocation ), suite( suite ) {
        if (suite->size() > 1) {
            // inject predecessor links
            TxStatementNode* pred = suite->front();
            for ( auto stmtI = std::next( suite->begin() ); stmtI != suite->end(); stmtI++ ) {
                (*stmtI)->predecessor = pred;
                pred = (*stmtI);
            }
        }
    }
    TxSuiteNode( const TxLocation& parseLocation )
            : TxSuiteNode( parseLocation, new std::vector<TxStatementNode*>() ) {
    }

    virtual TxSuiteNode* make_ast_copy() const override {
        return new TxSuiteNode( this->parseLocation, make_node_vec_copy( this->suite ) );
    }

    virtual void symbol_resolution_pass() override {
        TxStatementNode* prev_stmt = nullptr;
        for ( auto stmt : *this->suite ) {
            if ( prev_stmt && prev_stmt->ends_with_terminal_stmt() )
                CERROR( stmt, "This statement is unreachable." );

            try {
                stmt->symbol_resolution_pass();
            }
            catch ( const resolution_error& err ) {
                LOG( this->LOGGER(), DEBUG, "Caught resolution error in " << stmt << ": " << err );
            }
            prev_stmt = stmt;
        }
    }

    virtual bool may_end_with_non_return_stmt() const override {
        for ( auto stmt : *this->suite )
            if ( stmt->may_end_with_non_return_stmt() )
                return true;
        return false;
    }
    virtual bool ends_with_terminal_stmt() const override {
        return ( !this->suite->empty() && this->suite->back()->ends_with_terminal_stmt() );
    }
    virtual bool ends_with_return_stmt() const override {
        if ( this->suite->empty() )
            return false;
        for ( auto stmt : *this->suite ) {
            if ( stmt->may_end_with_non_return_stmt() )
                return false;
        }
        return this->suite->back()->ends_with_return_stmt();
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        for ( auto stmt : *this->suite )
            stmt->visit_ast( visitor, thisCursor, "stmt", context );
    }
};

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

class TxCondCompoundStmtNode : public TxStatementNode {
protected:
    TxMaybeConversionNode* cond;
    TxStatementNode* body;
    TxElseClauseNode* elseClause;

public:
    TxCondCompoundStmtNode( const TxLocation& parseLocation, TxExpressionNode* cond, TxStatementNode* body,
                            TxElseClauseNode* elseClause = nullptr )
            : TxStatementNode( parseLocation ), cond( new TxMaybeConversionNode( cond ) ), body( body ), elseClause( elseClause ) {
    }

    virtual TxCondCompoundStmtNode* make_ast_copy() const override = 0;

    virtual void symbol_resolution_pass() override {
        this->cond->insert_conversion( this->registry().get_builtin_type( TXBT_BOOL ) );
        this->cond->symbol_resolution_pass();
        this->body->symbol_resolution_pass();
        if ( this->elseClause )
            this->elseClause->symbol_resolution_pass();
    }

    virtual bool ends_with_return_stmt() const override {
        return ( this->body->ends_with_return_stmt() && this->elseClause && this->elseClause->ends_with_return_stmt() );
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->cond->visit_ast( visitor, thisCursor, "condition", context );
        this->body->visit_ast( visitor, thisCursor, "then", context );
        if (this->elseClause)
            this->elseClause->visit_ast( visitor, thisCursor, "else", context );
    }
};

class TxIfStmtNode : public TxCondCompoundStmtNode {
public:
    TxIfStmtNode( const TxLocation& parseLocation, TxExpressionNode* cond, TxStatementNode* body,
                  TxElseClauseNode* elseClause = nullptr )
            : TxCondCompoundStmtNode( parseLocation, cond, body, elseClause ) {
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

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;
};

class TxWhileStmtNode : public TxCondCompoundStmtNode {
public:
    TxWhileStmtNode( const TxLocation& parseLocation, TxExpressionNode* cond, TxStatementNode* body,
                     TxElseClauseNode* elseClause = nullptr )
            : TxCondCompoundStmtNode( parseLocation, cond, body, elseClause ) {
    }

    virtual TxWhileStmtNode* make_ast_copy() const override {
        return new TxWhileStmtNode( this->parseLocation, this->cond->originalExpr->make_ast_copy(), this->body->make_ast_copy(),
                                    ( this->elseClause ? this->elseClause->make_ast_copy() : nullptr ) );
    }

    virtual bool may_end_with_non_return_stmt() const override {
        // FUTURE: handle break & continue that terminate statement outside this loop
        return false;
    }
    virtual bool ends_with_terminal_stmt() const override {
        // FUTURE: handle break & continue that terminate statement outside this loop
        //return ( this->body->ends_with_terminal_stmt() && this->elseClause && this->elseClause->ends_with_terminal_stmt() );
        return this->ends_with_return_stmt();
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;
};

class TxAssignStmtNode : public TxStatementNode {
public:
    TxAssigneeNode* lvalue;
    TxMaybeConversionNode* rvalue;

    TxAssignStmtNode( const TxLocation& parseLocation, TxAssigneeNode* lvalue, TxExpressionNode* rvalue )
            : TxStatementNode( parseLocation ), lvalue( lvalue ), rvalue( new TxMaybeConversionNode( rvalue ) ) {
    }

    virtual TxAssignStmtNode* make_ast_copy() const override {
        return new TxAssignStmtNode( this->parseLocation, this->lvalue->make_ast_copy(), this->rvalue->originalExpr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override;

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->lvalue->visit_ast( visitor, thisCursor, "lvalue", context );
        this->rvalue->visit_ast( visitor, thisCursor, "rvalue", context );
    }
};

class TxExpErrStmtNode : public TxStatementNode {
    ExpectedErrorClause* expError;

protected:
    virtual void stmt_declaration_pass() override;

public:
    TxStatementNode* body;

    TxExpErrStmtNode( const TxLocation& parseLocation, ExpectedErrorClause* expError, TxStatementNode* body )
            : TxStatementNode( parseLocation ), expError( expError ), body( body ) {
        if ( dynamic_cast<const TxExpErrStmtNode*>( body ) )
            CERROR( this, "Can't nest Expected Error constructs in a statement" );
        body->set_exp_error_stmt();
    }

    virtual TxExpErrStmtNode* make_ast_copy() const override {
        return new TxExpErrStmtNode( this->parseLocation, nullptr, this->body->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        ScopedExpErrClause scopedEEClause( this );
        this->body->symbol_resolution_pass();
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override { }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        ScopedExpErrClause scopedEEClause( this );
        this->body->visit_ast( visitor, thisCursor, "stmt", context );
    }
};

class TxNoOpStmtNode : public TxStatementNode {
public:
    TxNoOpStmtNode( const TxLocation& parseLocation )
            : TxStatementNode( parseLocation ) {
    }

    virtual TxNoOpStmtNode* make_ast_copy() const override {
        return new TxNoOpStmtNode( this->parseLocation );
    }

    virtual void symbol_resolution_pass() override {
    }

    virtual void code_gen( LlvmGenerationContext& context, GenScope* scope ) const override { }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }
};
