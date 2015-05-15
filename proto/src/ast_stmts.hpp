#pragma once

#include "ast_base.hpp"
//#include "ast_exprs.hpp"
class TxFunctionCallNode;


/** Local field declaration */
class TxFieldStmtNode : public TxStatementNode {
public:
    TxFieldDefNode* field;

    TxFieldStmtNode(const yy::location& parseLocation, TxFieldDefNode* field)
        : TxStatementNode(parseLocation), field(field) { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->field->symbol_declaration_pass_local_field(lexContext, true);
        this->set_context(this->field);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        this->field->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        this->field->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** Local type declaration */
class TxTypeStmtNode : public TxStatementNode {
public:
    TxTypeDeclNode* const typeDecl;

    TxTypeStmtNode(const yy::location& parseLocation, const std::string typeName,
                   const std::vector<TxDeclarationNode*>* typeParamDecls, TxTypeExpressionNode* typeExpression)
        : TxStatementNode(parseLocation),
          typeDecl(new TxTypeDeclNode(parseLocation, TXD_NONE, typeName, typeParamDecls, typeExpression))
    { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->typeDecl->symbol_declaration_pass(lexContext);
        this->set_context(this->typeDecl);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        this->typeDecl->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        this->typeDecl->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxCallStmtNode : public TxStatementNode {  // function call without assigning return value (if any)
public:
    TxFunctionCallNode* call;

    TxCallStmtNode(const yy::location& parseLocation, TxFunctionCallNode* call)
        : TxStatementNode(parseLocation), call(call) { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        ((TxExpressionNode*)this->call)->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        ((TxExpressionNode*)this->call)->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        ((TxExpressionNode*)this->call)->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxTerminalStmtNode : public TxStatementNode {
protected:
    TxTerminalStmtNode(const yy::location& parseLocation) : TxStatementNode(parseLocation)  { }
};

class TxReturnStmtNode : public TxTerminalStmtNode {
public:
    TxExpressionNode* expr;

    TxReturnStmtNode(const yy::location& parseLocation, TxExpressionNode* expr)
        : TxTerminalStmtNode(parseLocation), expr(expr) { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        if (this->expr)
            this->expr->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        // TODO: Fix so that this won't find false positive using outer function's $return typeDecl
        // TODO: Illegal to return reference to STACK dataspace
        auto returnValue = this->context().scope()->lookup_field(resCtx, TxIdentifier("$return"));
        if (this->expr) {
            this->expr->symbol_resolution_pass(resCtx);
            if (returnValue)
                this->expr = validate_wrap_convert(resCtx, this->expr, returnValue->resolve_symbol_type(resCtx));
            else
                cerror("Return statement has value expression although function has no return type");
        }
        else if (returnValue)
            cerror("Return statement has no value expression although function returns %s",
                   returnValue->to_string().c_str());
    }

    virtual void semantic_pass() override {
        if (this->expr)
            this->expr->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxBreakStmtNode : public TxTerminalStmtNode {
public:
    TxBreakStmtNode(const yy::location& parseLocation) : TxTerminalStmtNode(parseLocation)  { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override { this->set_context(lexContext); }
    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override { }
    virtual void semantic_pass() override { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxContinueStmtNode : public TxTerminalStmtNode {
public:
    TxContinueStmtNode(const yy::location& parseLocation) : TxTerminalStmtNode(parseLocation)  { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override { this->set_context(lexContext); }
    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override { }
    virtual void semantic_pass() override { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxSuiteNode : public TxStatementNode {
public:
    std::vector<TxStatementNode*>* suite;

    TxSuiteNode(const yy::location& parseLocation);
    TxSuiteNode(const yy::location& parseLocation, std::vector<TxStatementNode*>* suite);

    virtual void symbol_declaration_pass_no_subscope(LexicalContext& lexContext) {
        this->set_context(lexContext);
        for (auto stmt : *this->suite)
            stmt->symbol_declaration_pass(lexContext);
    }
    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        LexicalContext suiteContext(lexContext.scope()->create_code_block_scope(), lexContext.is_constructor());
        this->symbol_declaration_pass_no_subscope(suiteContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        for (auto stmt : *this->suite)
            stmt->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        TxStatementNode* prev_stmt = nullptr;
        for (auto stmt : *this->suite) {
            if (prev_stmt && dynamic_cast<TxTerminalStmtNode*>(prev_stmt))
                stmt->cerror("This statement is unreachable.");
            stmt->semantic_pass();
            prev_stmt = stmt;
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxElseClauseNode : public TxStatementNode {
public:
    TxStatementNode* suite;

    TxElseClauseNode(const yy::location& parseLocation, TxStatementNode* suite)
        : TxStatementNode(parseLocation), suite(suite)  { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        this->suite->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        this->suite->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        this->suite->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxCondCompoundStmtNode : public TxStatementNode {
protected:
    TxExpressionNode* cond;
    TxStatementNode* suite;
    TxElseClauseNode* elseClause;

public:
    TxCondCompoundStmtNode(const yy::location& parseLocation, TxExpressionNode* cond, TxStatementNode* suite,
                           TxElseClauseNode* elseClause=nullptr)
        : TxStatementNode(parseLocation), cond(cond), suite(suite), elseClause(elseClause)  { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        this->cond->symbol_declaration_pass(lexContext);
        this->suite->symbol_declaration_pass(lexContext);
        if (this->elseClause)
            this->elseClause->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        this->cond->symbol_resolution_pass(resCtx);
        this->cond = validate_wrap_convert(resCtx, this->cond, this->types().get_builtin_type(BOOL));
        this->suite->symbol_resolution_pass(resCtx);
        if (this->elseClause)
            this->elseClause->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        this->cond->semantic_pass();
        this->suite->semantic_pass();
        if (this->elseClause)
            this->elseClause->semantic_pass();
    }
};

class TxIfStmtNode : public TxCondCompoundStmtNode {
public:
    TxIfStmtNode(const yy::location& parseLocation, TxExpressionNode* cond, TxStatementNode* suite,
                 TxElseClauseNode* elseClause=nullptr)
        : TxCondCompoundStmtNode(parseLocation, cond, suite, elseClause)  { }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxWhileStmtNode : public TxCondCompoundStmtNode {
public:
    TxWhileStmtNode(const yy::location& parseLocation, TxExpressionNode* cond, TxStatementNode* suite,
                    TxElseClauseNode* elseClause=nullptr)
        : TxCondCompoundStmtNode(parseLocation, cond, suite, elseClause)  { }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxAssignStmtNode : public TxStatementNode {
public:
    TxAssigneeNode* lvalue;
    TxExpressionNode* rvalue;

    TxAssignStmtNode(const yy::location& parseLocation, TxAssigneeNode* lvalue, TxExpressionNode* rvalue)
        : TxStatementNode(parseLocation), lvalue(lvalue), rvalue(rvalue)  { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        this->lvalue->symbol_declaration_pass(lexContext);
        this->rvalue->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        this->lvalue->symbol_resolution_pass(resCtx);
        this->rvalue->symbol_resolution_pass(resCtx);
        auto ltype = this->lvalue->resolve_type(resCtx);
        if (! ltype)
            return;  // (error message should have been emitted by lvalue node)
        if (! ltype->is_modifiable()) {
            if (! this->context().is_constructor())
                cerror("Assignee is not modifiable: %s", ltype->to_string().c_str());
            // Note: If the object as a whole is modifiable, it can be assigned to.
            // If it has any "non-modifiable" members, those will still get overwritten.
            // We could add custom check to prevent that scenario for Arrays, but then
            // it would in this regard behave differently than other aggregate objects.
        }
        // note: similar rules to passing function arg
        this->rvalue = validate_wrap_assignment(resCtx, this->rvalue, ltype);
    }

    virtual void semantic_pass() override {
        this->lvalue->semantic_pass();
        this->rvalue->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};
