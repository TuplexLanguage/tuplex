#pragma once

#include "ast_base.hpp"

class TxFunctionCallNode;


/** Local field declaration */
class TxFieldStmtNode : public TxStatementNode {
public:
    TxFieldDefNode* field;

    TxFieldStmtNode(const TxLocation& parseLocation, TxFieldDefNode* field)
        : TxStatementNode(parseLocation), field(field) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->field->symbol_declaration_pass_local_field(six, lexContext, true);
        this->set_context(six, this->field->context(six));
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        this->field->symbol_resolution_pass(six);
        if (! field->initExpression) {
            // TODO: instead check that TXS_STACK fields are initialized before first use
            //CWARNING(this, "Local field without initializer: " << this->field->get_source_field_name());
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** Local type declaration */
class TxTypeStmtNode : public TxStatementNode {
public:
    TxTypeDeclNode* const typeDecl;

    TxTypeStmtNode(const TxLocation& parseLocation, TxTypeDeclNode* typeDecl)
        : TxStatementNode(parseLocation), typeDecl(typeDecl) { }

    TxTypeStmtNode(const TxLocation& parseLocation, const std::string typeName,
                   const std::vector<TxDeclarationNode*>* typeParamDecls, TxTypeExpressionNode* typeExpression, bool interfaceKW=false)
        : TxTypeStmtNode(parseLocation,
                         new TxTypeDeclNode(parseLocation, TXD_NONE, typeName, typeParamDecls, typeExpression, interfaceKW))
    { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->typeDecl->symbol_declaration_pass(six, lexContext);
        this->set_context(six, this->typeDecl->context(six));
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        this->typeDecl->symbol_resolution_pass(six);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxCallStmtNode : public TxStatementNode {  // function call without assigning return value (if any)
public:
    TxFunctionCallNode* call;

    TxCallStmtNode(const TxLocation& parseLocation, TxFunctionCallNode* call)
        : TxStatementNode(parseLocation), call(call) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        ((TxExpressionNode*)this->call)->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        ((TxExpressionNode*)this->call)->symbol_resolution_pass(six);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxTerminalStmtNode : public TxStatementNode {
protected:
    TxTerminalStmtNode(const TxLocation& parseLocation) : TxStatementNode(parseLocation)  { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
    }
    virtual void symbol_resolution_pass(TxSpecializationIndex six) override { }

    virtual bool ends_with_terminal_stmt() const override final { return true; }
};

class TxReturnStmtNode : public TxTerminalStmtNode {
public:
    TxExpressionNode* expr;

    TxReturnStmtNode(const TxLocation& parseLocation)
        : TxTerminalStmtNode(parseLocation), expr() { }
    TxReturnStmtNode(const TxLocation& parseLocation, TxExpressionNode* expr)
        : TxTerminalStmtNode(parseLocation), expr(make_generic_conversion_node(expr)) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        if (this->expr)
            this->expr->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        // TODO: Fix so that this won't find false positive using outer function's $return typeDecl
        // TODO: Illegal to return reference to STACK dataspace
        if (auto returnDecl = lookup_field(this->context(six).scope(), TxIdentifier("$return"))) {
            if (this->expr) {
                if (auto field = returnDecl->get_definer()->resolve_field())
                    insert_conversion(this->expr, six, field->get_type());
                this->expr->symbol_resolution_pass(six);
            }
            else
                CERROR(this, "Return statement has no value expression although function returns " << returnDecl);
        }
        else if (this->expr)
            CERROR(this, "Return statement has value expression although function has no return type");
    }

    virtual bool ends_with_return_stmt() const override { return true; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxBreakStmtNode : public TxTerminalStmtNode {
public:
    TxBreakStmtNode(const TxLocation& parseLocation) : TxTerminalStmtNode(parseLocation)  { }

    virtual bool may_end_with_non_return_stmt() const override { return true; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxContinueStmtNode : public TxTerminalStmtNode {
public:
    TxContinueStmtNode(const TxLocation& parseLocation) : TxTerminalStmtNode(parseLocation)  { }

    virtual bool may_end_with_non_return_stmt() const override { return true; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxSuiteNode : public TxStatementNode {
public:
    std::vector<TxStatementNode*>* suite;

    TxSuiteNode(const TxLocation& parseLocation);
    TxSuiteNode(const TxLocation& parseLocation, std::vector<TxStatementNode*>* suite);

    virtual void symbol_declaration_pass_no_subscope(TxSpecializationIndex six, LexicalContext& lexContext) {
        this->set_context(six, lexContext);
        for (auto stmt : *this->suite)
            stmt->symbol_declaration_pass(six, lexContext);
    }
    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        LexicalContext suiteContext(lexContext, lexContext.scope()->create_code_block_scope());
        this->symbol_declaration_pass_no_subscope(six, suiteContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxStatementNode* prev_stmt = nullptr;
        for (auto stmt : *this->suite) {
            if (prev_stmt && prev_stmt->ends_with_terminal_stmt())
                CERROR(stmt, "This statement is unreachable.");
            stmt->symbol_resolution_pass(six);
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
};


class TxElseClauseNode : public TxStatementNode {
public:
    TxStatementNode* body;

    TxElseClauseNode(const TxLocation& parseLocation, TxStatementNode* suite)
        : TxStatementNode(parseLocation), body(suite)  { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        this->body->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        this->body->symbol_resolution_pass(six);
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
};

class TxCondCompoundStmtNode : public TxStatementNode {
protected:
    TxExpressionNode* cond;
    TxStatementNode*  body;
    TxElseClauseNode* elseClause;

public:
    TxCondCompoundStmtNode(const TxLocation& parseLocation, TxExpressionNode* cond, TxStatementNode* body,
                           TxElseClauseNode* elseClause=nullptr)
        : TxStatementNode(parseLocation), cond(make_generic_conversion_node(cond)), body(body), elseClause(elseClause)  { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        this->cond->symbol_declaration_pass(six, lexContext);
        this->body->symbol_declaration_pass(six, lexContext);
        if (this->elseClause)
            this->elseClause->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        insert_conversion(this->cond, six, this->types(six).get_builtin_type(BOOL));
        this->cond->symbol_resolution_pass(six);
        this->body->symbol_resolution_pass(six);
        if (this->elseClause)
            this->elseClause->symbol_resolution_pass(six);
    }

    virtual bool ends_with_return_stmt() const override {
        return ( this->body->ends_with_return_stmt() && this->elseClause && this->elseClause->ends_with_return_stmt() );
    }
};

class TxIfStmtNode : public TxCondCompoundStmtNode {
public:
    TxIfStmtNode(const TxLocation& parseLocation, TxExpressionNode* cond, TxStatementNode* suite,
                 TxElseClauseNode* elseClause=nullptr)
        : TxCondCompoundStmtNode(parseLocation, cond, suite, elseClause)  { }

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
    TxWhileStmtNode(const TxLocation& parseLocation, TxExpressionNode* cond, TxStatementNode* suite,
                    TxElseClauseNode* elseClause=nullptr)
        : TxCondCompoundStmtNode(parseLocation, cond, suite, elseClause)  { }

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
    TxExpressionNode* rvalue;

    TxAssignStmtNode(const TxLocation& parseLocation, TxAssigneeNode* lvalue, TxExpressionNode* rvalue)
        : TxStatementNode(parseLocation), lvalue(lvalue), rvalue(make_generic_conversion_node(rvalue))  { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        this->lvalue->symbol_declaration_pass(six, lexContext);
        this->rvalue->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        this->lvalue->symbol_resolution_pass(six);
        auto ltype = this->lvalue->resolve_type(six);
        if (! ltype)
            return;  // (error message should have been emitted by lvalue node)
        if (! ltype->is_modifiable()) {
            if (! this->context(six).get_constructed())  // TODO: only members of constructed object should skip error
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

        insert_conversion(this->rvalue, six, ltype);
        this->rvalue->symbol_resolution_pass(six);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxAssertStmtNode : public TxStatementNode {
    TxExpressionNode* expr;
    TxStatementNode* ifStmt;
public:
    TxAssertStmtNode(const TxLocation& parseLocation, TxExpressionNode* expr);

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        this->ifStmt->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        this->ifStmt->symbol_resolution_pass(six);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxExpErrStmtNode : public TxStatementNode {
    const int expected_error_count;
    int encountered_error_count;
public:
    TxStatementNode* body;

    TxExpErrStmtNode(const TxLocation& parseLocation, int expected_error_count, int prev_encountered_errors, TxStatementNode* body)
        : TxStatementNode(parseLocation), expected_error_count(expected_error_count),
          encountered_error_count(prev_encountered_errors), body(body)  { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        LexicalContext experrBlockContext(lexContext, lexContext.scope()->create_code_block_scope("EE", true), true);
        this->set_context(six, experrBlockContext);
        if (six == 0) {
            experrBlockContext.package()->driver().begin_exp_err(this->parseLocation);
            this->body->symbol_declaration_pass(six, experrBlockContext);
            this->encountered_error_count += experrBlockContext.package()->driver().end_exp_err(this->parseLocation);
        }
        else
            this->body->symbol_declaration_pass(six, experrBlockContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        auto ctx = this->context(six);
        if (six == 0) {
            ctx.package()->driver().begin_exp_err(this->parseLocation);
            this->body->symbol_resolution_pass(six);
            this->encountered_error_count += ctx.package()->driver().end_exp_err(this->parseLocation);
            if ( this->expected_error_count <  0 ) {
                if ( this->encountered_error_count == 0 )
                    CERROR(this, "COMPILER TEST FAIL: Expected one or more compilation errors but encountered " << this->encountered_error_count);
            }
            else if ( this->expected_error_count != this->encountered_error_count )
                CERROR(this, "COMPILER TEST FAIL: Expected " << this->expected_error_count
                              << " compilation errors but encountered " << this->encountered_error_count);
        }
        else
            this->body->symbol_resolution_pass(six);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
};


class TxNoOpStmtNode : public TxStatementNode {
public:
    TxNoOpStmtNode(const TxLocation& parseLocation) : TxStatementNode(parseLocation) {}

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
};
