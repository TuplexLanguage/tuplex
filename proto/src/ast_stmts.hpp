#pragma once

#include "ast_base.hpp"
#include "ast_exprs.hpp"



class TxStatementNode : public TxNode {
public:
    TxStatementNode(const TxLocation& parseLocation) : TxNode(parseLocation) { }

    virtual TxStatementNode* make_ast_copy() const override = 0;

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorStmt=false ) = 0;
    virtual void symbol_resolution_pass() = 0;

    /** Returns true if this statement / compound statement *may* end with a break or continue statement. */
    virtual bool may_end_with_non_return_stmt() const { return false; }

    /** Returns true if this statement / compound statement always ends with an explicit terminal statement
     * (return, break, continue).
     * This means that any successor statement in the same suite will never be reached. */
    virtual bool ends_with_terminal_stmt() const { return false; }

    /** Returns true if this statement / compound statement always ends with an explicit return statement. */
    virtual bool ends_with_return_stmt() const { return false; }
};



/** Local field declaration */
class TxFieldStmtNode : public TxStatementNode {
public:
    TxFieldDefNode* field;

    TxFieldStmtNode(const TxLocation& parseLocation, TxFieldDefNode* field)
        : TxStatementNode(parseLocation), field(field) { }

    virtual TxFieldStmtNode* make_ast_copy() const override {
        return new TxFieldStmtNode( this->parseLocation, this->field->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorStmt=false ) override {
        this->field->symbol_declaration_pass_local_field( lexContext, true, (isExpErrorStmt ? TXD_EXPERRBLOCK : TXD_NONE) );
        this->set_context( this->field->context());
    }

    virtual void symbol_resolution_pass() override {
        this->field->symbol_resolution_pass();
        if (! field->initExpression) {
            // TODO: instead check that TXS_STACK fields are initialized before first use
            //CWARNING(this, "Local field without initializer: " << this->field->get_source_field_name());
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->field->visit_ast( visitor, thisAsParent, "fielddecl", context );
    }
};

/** Local type declaration */
class TxTypeStmtNode : public TxStatementNode {
public:
    TxTypeDeclNode* const typeDecl;

    TxTypeStmtNode(const TxLocation& parseLocation, TxTypeDeclNode* typeDecl)
        : TxStatementNode(parseLocation), typeDecl(typeDecl) { }

    TxTypeStmtNode(const TxLocation& parseLocation, const std::string typeName,
                   const std::vector<TxDeclarationNode*>* typeParamDecls, TxTypeExpressionNode* typeExpression, bool interfaceKW=false)
        : TxTypeStmtNode( parseLocation, new TxTypeDeclNode( parseLocation, TXD_NONE, typeName, typeParamDecls, typeExpression, interfaceKW ) )
    { }

    virtual TxTypeStmtNode* make_ast_copy() const override {
        return new TxTypeStmtNode( this->parseLocation, this->typeDecl->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorStmt=false ) override {
        this->typeDecl->symbol_declaration_pass( lexContext, isExpErrorStmt );
        this->set_context( this->typeDecl->context());
    }

    virtual void symbol_resolution_pass() override {
        this->typeDecl->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->typeDecl->visit_ast( visitor, thisAsParent, "typedecl", context );
    }
};

class TxCallStmtNode : public TxStatementNode {  // function call without assigning return value (if any)
public:
    TxFunctionCallNode* call;

    TxCallStmtNode(const TxLocation& parseLocation, TxFunctionCallNode* call)
        : TxStatementNode(parseLocation), call(call) { }

    virtual TxCallStmtNode* make_ast_copy() const override {
        return new TxCallStmtNode( this->parseLocation, this->call->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorStmt=false ) override {
        this->set_context( lexContext);
        ((TxExpressionNode*)this->call)->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        ((TxExpressionNode*)this->call)->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->call->visit_ast( visitor, thisAsParent, "call", context );
    }
};

class TxTerminalStmtNode : public TxStatementNode {
protected:
    TxTerminalStmtNode(const TxLocation& parseLocation) : TxStatementNode(parseLocation)  { }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorStmt=false ) override {
        this->set_context( lexContext);
    }
    virtual void symbol_resolution_pass() override { }

    virtual bool ends_with_terminal_stmt() const override final { return true; }
};

class TxReturnStmtNode : public TxTerminalStmtNode {
public:
    TxMaybeConversionNode* expr;

    TxReturnStmtNode(const TxLocation& parseLocation)
        : TxTerminalStmtNode(parseLocation), expr() { }
    TxReturnStmtNode(const TxLocation& parseLocation, TxExpressionNode* expr)
        : TxTerminalStmtNode(parseLocation), expr(new TxMaybeConversionNode(expr)) { }

    virtual TxReturnStmtNode* make_ast_copy() const override {
        return new TxReturnStmtNode( this->parseLocation, this->expr->originalExpr->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorStmt=false ) override {
        this->set_context( lexContext);
        if (this->expr)
            this->expr->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        // TODO: Fix so that this won't find false positive using outer function's $return typeDecl
        // TODO: Illegal to return reference to STACK dataspace
        if (auto returnDecl = lookup_field(this->context().scope(), TxIdentifier("$return"))) {
            if (this->expr) {
                if (auto field = returnDecl->get_definer()->resolve_field())
                    this->expr->insert_conversion( field->get_type() );
                this->expr->symbol_resolution_pass();
            }
            else
                CERROR(this, "Return statement has no value expression although function returns " << returnDecl);
        }
        else if (this->expr)
            CERROR(this, "Return statement has value expression although function has no return type");
    }

    virtual bool ends_with_return_stmt() const override { return true; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->expr->visit_ast( visitor, thisAsParent, "value", context );
    }
};

class TxBreakStmtNode : public TxTerminalStmtNode {
public:
    TxBreakStmtNode(const TxLocation& parseLocation) : TxTerminalStmtNode(parseLocation)  { }

    virtual TxBreakStmtNode* make_ast_copy() const override {
        return new TxBreakStmtNode( this->parseLocation );
    }

    virtual bool may_end_with_non_return_stmt() const override { return true; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {}
};

class TxContinueStmtNode : public TxTerminalStmtNode {
public:
    TxContinueStmtNode(const TxLocation& parseLocation) : TxTerminalStmtNode(parseLocation)  { }

    virtual TxContinueStmtNode* make_ast_copy() const override {
        return new TxContinueStmtNode( this->parseLocation );
    }

    virtual bool may_end_with_non_return_stmt() const override { return true; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {}
};


class TxSuiteNode : public TxStatementNode {
public:
    std::vector<TxStatementNode*>* suite;

    TxSuiteNode( const TxLocation& parseLocation, std::vector<TxStatementNode*>* suite )
            : TxStatementNode(parseLocation), suite(suite)  { }
    TxSuiteNode( const TxLocation& parseLocation )
            : TxSuiteNode( parseLocation, new std::vector<TxStatementNode*>() )  { }

    virtual TxSuiteNode* make_ast_copy() const override {
        return new TxSuiteNode( this->parseLocation, make_node_vec_copy( this->suite ) );
    }

    virtual void symbol_declaration_pass_no_subscope( LexicalContext& lexContext ) {
        this->set_context( lexContext);
        for (auto stmt : *this->suite)
            stmt->symbol_declaration_pass( lexContext);
    }
    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorStmt=false ) override {
        LexicalContext suiteContext( lexContext, lexContext.scope()->create_code_block_scope( *this ) );
        this->symbol_declaration_pass_no_subscope( suiteContext );
    }

    virtual void symbol_resolution_pass() override {
        TxStatementNode* prev_stmt = nullptr;
        for (auto stmt : *this->suite) {
            if (prev_stmt && prev_stmt->ends_with_terminal_stmt())
                CERROR(stmt, "This statement is unreachable.");
            stmt->symbol_resolution_pass();
            prev_stmt = stmt;
        }
    }

    virtual bool may_end_with_non_return_stmt() const override {
        for (auto stmt : *this->suite)
            if (stmt->may_end_with_non_return_stmt())
                return true;
        return false;
    }
    virtual bool ends_with_terminal_stmt() const override {
        return ( !this->suite->empty() && this->suite->back()->ends_with_terminal_stmt() );
    }
    virtual bool ends_with_return_stmt() const override {
        if (this->suite->empty())
            return false;
        for (auto stmt : *this->suite) {
            if (stmt->may_end_with_non_return_stmt())
                return false;
        }
        return this->suite->back()->ends_with_return_stmt();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        for (auto stmt : *this->suite)
            stmt->visit_ast( visitor, thisAsParent, "stmt", context );
    }
};


class TxElseClauseNode : public TxStatementNode {
public:
    TxStatementNode* body;

    TxElseClauseNode(const TxLocation& parseLocation, TxStatementNode* suite)
        : TxStatementNode(parseLocation), body(suite)  { }

    virtual TxElseClauseNode* make_ast_copy() const override {
        return new TxElseClauseNode( this->parseLocation, this->body->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorStmt=false ) override {
        this->set_context( lexContext);
        this->body->symbol_declaration_pass( lexContext);
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

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->body->visit_ast( visitor, thisAsParent, "body", context );
    }
};

class TxCondCompoundStmtNode : public TxStatementNode {
protected:
    TxMaybeConversionNode* cond;
    TxStatementNode*  body;
    TxElseClauseNode* elseClause;

public:
    TxCondCompoundStmtNode(const TxLocation& parseLocation, TxExpressionNode* cond, TxStatementNode* body,
                           TxElseClauseNode* elseClause=nullptr)
        : TxStatementNode(parseLocation), cond(new TxMaybeConversionNode(cond)), body(body), elseClause(elseClause)  { }

    virtual TxCondCompoundStmtNode* make_ast_copy() const override = 0;

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorStmt=false ) override {
        this->set_context( lexContext);
        this->cond->symbol_declaration_pass( lexContext);
        this->body->symbol_declaration_pass( lexContext);
        if (this->elseClause)
            this->elseClause->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        this->cond->insert_conversion( this->types().get_builtin_type(BOOL) );
        this->cond->symbol_resolution_pass();
        this->body->symbol_resolution_pass();
        if (this->elseClause)
            this->elseClause->symbol_resolution_pass();
    }

    virtual bool ends_with_return_stmt() const override {
        return ( this->body->ends_with_return_stmt() && this->elseClause && this->elseClause->ends_with_return_stmt() );
    }

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->cond->visit_ast( visitor, thisAsParent, "condition", context );
        this->body->visit_ast( visitor, thisAsParent, "true", context );
        this->cond->visit_ast( visitor, thisAsParent, "else", context );
    }
};

class TxIfStmtNode : public TxCondCompoundStmtNode {
public:
    TxIfStmtNode(const TxLocation& parseLocation, TxExpressionNode* cond, TxStatementNode* body,
                 TxElseClauseNode* elseClause=nullptr)
        : TxCondCompoundStmtNode(parseLocation, cond, body, elseClause)  { }

    virtual TxIfStmtNode* make_ast_copy() const override {
        return new TxIfStmtNode( this->parseLocation, this->cond->originalExpr->make_ast_copy(), this->body->make_ast_copy(),
                                 ( this->elseClause ? this->elseClause->make_ast_copy() : nullptr ) );
    }

    virtual bool may_end_with_non_return_stmt() const override {
        return ( this->body->may_end_with_non_return_stmt() || (this->elseClause && this->elseClause->may_end_with_non_return_stmt()) );
    }
    virtual bool ends_with_terminal_stmt() const override {
        return ( this->body->ends_with_terminal_stmt() && this->elseClause && this->elseClause->ends_with_terminal_stmt() );
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxWhileStmtNode : public TxCondCompoundStmtNode {
public:
    TxWhileStmtNode(const TxLocation& parseLocation, TxExpressionNode* cond, TxStatementNode* body,
                    TxElseClauseNode* elseClause=nullptr)
        : TxCondCompoundStmtNode(parseLocation, cond, body, elseClause)  { }

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

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxAssignStmtNode : public TxStatementNode {
public:
    TxAssigneeNode* lvalue;
    TxMaybeConversionNode* rvalue;

    TxAssignStmtNode(const TxLocation& parseLocation, TxAssigneeNode* lvalue, TxExpressionNode* rvalue)
        : TxStatementNode(parseLocation), lvalue(lvalue), rvalue(new TxMaybeConversionNode(rvalue))  { }

    virtual TxAssignStmtNode* make_ast_copy() const override {
        return new TxAssignStmtNode( this->parseLocation, this->lvalue->make_ast_copy(), this->rvalue->originalExpr->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorStmt=false ) override {
        this->set_context( lexContext);
        this->lvalue->symbol_declaration_pass( lexContext);
        this->rvalue->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        this->lvalue->symbol_resolution_pass();
        auto ltype = this->lvalue->resolve_type();
        if (! ltype)
            return;  // (error message should have been emitted by lvalue node)
        if (! ltype->is_modifiable()) {
            if (! this->context().get_constructed())  // TODO: only members of constructed object should skip error
                CERROR(this, "Assignee is not modifiable: " << ltype);
            // Note: If the object as a whole is modifiable, it can be assigned to.
            // If it has any "non-modifiable" members, those will still get overwritten.
            // We could add custom check to prevent that scenario for Arrays, but then
            // it would in this regard behave differently than other aggregate objects.
        }

        // note: similar rules to passing function arg
        if (! ltype->is_concrete())
            // TODO: dynamic concrete type resolution (recognize actual type in runtime when dereferencing a generic pointer)
            CERROR(this->lvalue, "Assignee is not a concrete type (size potentially unknown): " << ltype);
        // if assignee is a reference:
        // TODO: check dataspace rules

        this->rvalue->insert_conversion( ltype );
        this->rvalue->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->lvalue->visit_ast( visitor, thisAsParent, "lvalue", context );
        this->rvalue->visit_ast( visitor, thisAsParent, "rvalue", context );
    }
};



class TxAssertStmtNode : public TxStatementNode {
    TxExpressionNode* expr;
    TxStatementNode* ifStmt;
public:
    TxAssertStmtNode(const TxLocation& parseLocation, TxExpressionNode* expr);

    virtual TxAssertStmtNode* make_ast_copy() const override {
        return new TxAssertStmtNode( this->parseLocation, this->expr->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorStmt=false ) override {
        this->set_context( lexContext);
        this->ifStmt->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        this->ifStmt->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->ifStmt->visit_ast( visitor, thisAsParent, "ifstmt", context );
    }
};



class TxExpErrStmtNode : public TxStatementNode {
    ExpectedErrorClause* expError;

public:
    TxStatementNode* body;

    TxExpErrStmtNode( const TxLocation& parseLocation, ExpectedErrorClause* expError, TxStatementNode* body )
        : TxStatementNode( parseLocation ), expError( expError ), body( body )  { }

    virtual TxExpErrStmtNode* make_ast_copy() const override {
        return new TxExpErrStmtNode( this->parseLocation, nullptr, this->body->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorStmt=false ) override {
        this->set_context( LexicalContext( lexContext, lexContext.scope()->create_code_block_scope( *this, "EE"), expError ) );
        if (isExpErrorStmt)
            CERROR(this, "Can't next Expected Error constructs in a statement");
        if (! this->context().is_reinterpretation()) {
            this->get_parse_location().parserCtx->begin_exp_err( this );
            this->body->symbol_declaration_pass( this->context(), true );
            this->get_parse_location().parserCtx->end_exp_err( this->parseLocation );
        }
        else
            this->body->symbol_declaration_pass( this->context(), true );
    }

    virtual void symbol_resolution_pass() override {
        auto ctx = this->context();
        if (! ctx.is_reinterpretation()) {
            this->get_parse_location().parserCtx->begin_exp_err( this );
            this->body->symbol_resolution_pass();
            this->get_parse_location().parserCtx->end_exp_err( this->parseLocation );
            finalize_expected_error_clause( this );
        }
        else
            this->body->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->body->visit_ast( visitor, thisAsParent, "stmt", context );
    }
};


class TxNoOpStmtNode : public TxStatementNode {
public:
    TxNoOpStmtNode(const TxLocation& parseLocation) : TxStatementNode(parseLocation) {}

    virtual TxNoOpStmtNode* make_ast_copy() const override {
        return new TxNoOpStmtNode( this->parseLocation );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorStmt=false ) override {
        this->set_context( lexContext);
    }

    virtual void symbol_resolution_pass() override {
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {}
};
