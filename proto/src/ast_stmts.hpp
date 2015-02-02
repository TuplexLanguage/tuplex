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

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->field->symbol_table_pass_local_field(lexContext, true);
        this->set_context(this->field);
    }

    virtual void semantic_pass() {
        this->field->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
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

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->typeDecl->symbol_table_pass(lexContext);
        this->set_context(this->typeDecl);
    }

    virtual void semantic_pass() {
        this->typeDecl->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};

class TxCallStmtNode : public TxStatementNode {  // function call without assigning return value (if any)
public:
    TxFunctionCallNode* call;

    TxCallStmtNode(const yy::location& parseLocation, TxFunctionCallNode* call)
        : TxStatementNode(parseLocation), call(call) { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        ((TxExpressionNode*)this->call)->symbol_table_pass(lexContext);
    }

    virtual void semantic_pass() {
        ((TxExpressionNode*)this->call)->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
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

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        if (this->expr)
            this->expr->symbol_table_pass(lexContext);
    }

    virtual void semantic_pass() {
        // TODO: Fix so that this won't find false positive using outer function's $return typeDecl
        auto returnValue = this->context().scope()->resolve_field(TxIdentifier("$return"));
        if (this->expr) {
            this->expr->semantic_pass();
            if (returnValue)
                this->expr = wrapConversion(this->context().scope(), this->expr, returnValue->get_type());
            else
                parser_error(this->parseLocation, "Return statement has value expression although function has no return type: %s",
                             this->expr->get_type()->to_string().c_str());
        }
        else if (returnValue)
            parser_error(this->parseLocation, "Return statement has no value expression although function returns %s",
                         returnValue->get_type()->to_string().c_str());
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};

class TxBreakStmtNode : public TxTerminalStmtNode {
public:
    TxBreakStmtNode(const yy::location& parseLocation) : TxTerminalStmtNode(parseLocation)  { }

    virtual void symbol_table_pass(LexicalContext& lexContext) { this->set_context(lexContext); }
    virtual void semantic_pass() { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};

class TxContinueStmtNode : public TxTerminalStmtNode {
public:
    TxContinueStmtNode(const yy::location& parseLocation) : TxTerminalStmtNode(parseLocation)  { }

    virtual void symbol_table_pass(LexicalContext& lexContext) { this->set_context(lexContext); }
    virtual void semantic_pass() { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxSuiteNode : public TxStatementNode {
public:
    std::vector<TxStatementNode*>* suite;

    TxSuiteNode(const yy::location& parseLocation);
    TxSuiteNode(const yy::location& parseLocation, std::vector<TxStatementNode*>* suite);

    virtual void symbol_table_pass_no_subscope(LexicalContext& lexContext) {
        this->set_context(lexContext);
        for (auto stmt : *this->suite)
            stmt->symbol_table_pass(lexContext);
    }
    virtual void symbol_table_pass(LexicalContext& lexContext) {
        LexicalContext suiteContext(lexContext.scope()->create_code_block_scope());
        this->symbol_table_pass_no_subscope(suiteContext);
    }

    virtual void semantic_pass() {
        TxStatementNode* prev_stmt = nullptr;
        for (auto stmt : *this->suite) {
            if (prev_stmt && dynamic_cast<TxTerminalStmtNode*>(prev_stmt))
                parser_error(stmt->parseLocation, "This statement is unreachable.");
            stmt->semantic_pass();
            prev_stmt = stmt;
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxElseClauseNode : public TxStatementNode {
public:
    TxStatementNode* suite;

    TxElseClauseNode(const yy::location& parseLocation, TxStatementNode* suite)
        : TxStatementNode(parseLocation), suite(suite)  { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        this->suite->symbol_table_pass(lexContext);
    }

    virtual void semantic_pass() {
        this->suite->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
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

//    bool has_else_clause() { return this->elseClause; }
//    void set_else_clause(TxElseClauseNode* elseClause) {
//        ASSERT(!this->elseClause, "else-clause already set");
//        this->elseClause = elseClause;
//    }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        this->cond->symbol_table_pass(lexContext);
        this->suite->symbol_table_pass(lexContext);
        if (this->elseClause)
            this->elseClause->symbol_table_pass(lexContext);
    }

    virtual void semantic_pass() {
        this->cond->semantic_pass();
        this->cond = wrapConversion(this->context().scope(), this->cond, this->types().get_builtin_type(BOOLEAN));
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

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};

class TxWhileStmtNode : public TxCondCompoundStmtNode {
public:
    TxWhileStmtNode(const yy::location& parseLocation, TxExpressionNode* cond, TxStatementNode* suite,
                    TxElseClauseNode* elseClause=nullptr)
        : TxCondCompoundStmtNode(parseLocation, cond, suite, elseClause)  { }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};



/*=== assignment ===*/

class TxDerefAssigneeNode : public TxAssigneeNode {
public:
    TxExpressionNode* operand;
    TxDerefAssigneeNode(const yy::location& parseLocation, TxExpressionNode* operand)
        : TxAssigneeNode(parseLocation), operand(operand) { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        operand->symbol_table_pass(lexContext);
    }

    virtual const TxType* get_type() const {
        auto opType = this->operand->get_type();
        if (auto refType = dynamic_cast<const TxReferenceType*>(opType)) {
            if (refType->is_generic())
                // FUTURE: return constraint type if present
                return this->types().get_builtin_type(ANY);
            return refType->target_type().get_type();
        }
        parser_error(this->parseLocation, "Operand is not a reference and can't be dereferenced: %s", opType->to_string().c_str());
        return nullptr;
    }

    virtual void semantic_pass() {
        operand->semantic_pass();
        if (! dynamic_cast<const TxReferenceType*>(this->operand->get_type()))
            parser_error(this->parseLocation, "Can't de-reference non-reference expression.");
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};

class TxElemAssigneeNode : public TxAssigneeNode {
public:
    TxExpressionNode* array;
    TxExpressionNode* subscript;
    TxElemAssigneeNode(const yy::location& parseLocation, TxExpressionNode* array, TxExpressionNode* subscript)
        : TxAssigneeNode(parseLocation), array(array), subscript(subscript)  { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        array->symbol_table_pass(lexContext);
        subscript->symbol_table_pass(lexContext);
    }

    virtual const TxType* get_type() const {
        auto opType = this->array->get_type();
        if (auto arrayType = dynamic_cast<const TxArrayType*>(opType)) {
            if (auto e = arrayType->resolve_param_type("E"))
                return e->get_type();
            else
                // FUTURE: return constraint type if present
                return this->types().get_builtin_type(ANY);
        }
        // operand type is unknown / not an array and can't be subscripted
        return nullptr;
    }

    virtual void semantic_pass() {
        array->semantic_pass();
        subscript->semantic_pass();
        if (! dynamic_cast<const TxArrayType*>(this->array->get_type()))
            parser_error(this->parseLocation, "Can't subscript non-array expression.");
        subscript = wrapConversion(this->context().scope(), subscript, this->types().get_builtin_type(LONG));
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};

class TxAssignStmtNode : public TxStatementNode {
public:
    TxAssigneeNode* lvalue;
    TxExpressionNode* rvalue;

    TxAssignStmtNode(const yy::location& parseLocation, TxAssigneeNode* lvalue, TxExpressionNode* rvalue)
        : TxStatementNode(parseLocation), lvalue(lvalue), rvalue(rvalue)  { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        this->lvalue->symbol_table_pass(lexContext);
        this->rvalue->symbol_table_pass(lexContext);
    }

    virtual void semantic_pass() {
        this->lvalue->semantic_pass();
        this->rvalue->semantic_pass();
        auto ltype = this->lvalue->get_type();
        if (! ltype)
            return;  // (error message should have been emitted by lvalue node)
        if (! ltype->is_modifiable())
            parser_error(this->parseLocation, "Assignee type %s is not modifiable.", ltype->to_string().c_str());
        // note: similar rules to passing function arg
        if (! ltype->is_concrete())
            parser_error(this->parseLocation, "Assignee type %s is not concrete (size potentially unknown).", ltype->to_string().c_str());
        // if assignee is a reference:
        // TODO: check that no modifiable attribute is lost
        // TODO: check dataspace rules
        this->rvalue = wrapConversion(this->context().scope(), this->rvalue, ltype);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};
