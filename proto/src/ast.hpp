#pragma once

#include "ast_base.hpp"
#include "ast_stmts.hpp"
#include "ast_fields.hpp"
#include "ast_types.hpp"
#include "ast_exprs.hpp"


// (in this file since uses both types, expressions, and statements)

class TxLambdaExprNode : public TxExpressionNode {
    bool instanceMethod = false;

protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        return this->funcTypeNode->resolve_type(resCtx);  // function header
    }

public:
    TxFunctionTypeNode* funcTypeNode;
    TxSuiteNode* suite;

    TxLambdaExprNode(const yy::location& parseLocation, TxFunctionTypeNode* funcTypeNode, TxSuiteNode* suite)
            : TxExpressionNode(parseLocation), funcTypeNode(funcTypeNode), suite(suite) {
    }

    /** Converts this lambda expression into an instance method - adding the implicit self argument. */
    void make_instance_method() {
        this->instanceMethod = true;
    }

    virtual bool has_predefined_type() const override { return false; }

    virtual void symbol_registration_pass(LexicalContext& lexContext) {
        if (this->instanceMethod) {
            if (auto typeEntity = dynamic_cast<TxTypeEntity*>(lexContext.scope())) {  // if in type scope
                // insert a first parameter named 'self', that is a reference to the current type
                auto identNode = new TxIdentifierNode(this->parseLocation, new TxIdentifier(typeEntity->get_full_name()));
                // FIXME: handle both generic and non-generic outer types
                auto identTypeNode = new TxIdentifiedTypeNode(this->parseLocation, identNode);
                TxTypeExpressionNode* selfTypeExpr = new TxReferenceTypeNode(this->parseLocation, nullptr, identTypeNode);
                auto selfArg = new TxFieldDefNode(this->parseLocation, "self", selfTypeExpr, nullptr);
                this->funcTypeNode->arguments->insert(this->funcTypeNode->arguments->begin(), selfArg);
            }
            else
                cerror("Method declared outside a type scope: %s", lexContext.scope()->to_string().c_str());
        }

        std::string funcName = this->fieldDefNode ? this->fieldDefNode->get_entity()->get_name() : "";
        LexicalContext funcLexContext(lexContext.scope()->create_code_block_scope(funcName));
        this->set_context(funcLexContext);

        // generate function instance:
        this->funcTypeNode->symbol_registration_pass_func_header(funcLexContext);  // function header
        this->suite->symbol_registration_pass_no_subscope(funcLexContext);  // function body
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        this->funcTypeNode->symbol_resolution_pass(resCtx);  // function header
        this->suite->symbol_resolution_pass(resCtx);  // function body
    }

    /** Returns false if this function may modify its closure when run, i.e. have side effects.
     * A modifying function is not regarded as statically constant since its closure may be modified when run.
     */
    virtual bool is_statically_constant() const override { return this->funcTypeNode->get_type()->is_immutable(); }

    virtual void semantic_pass() {
        // TODO: if in global scope, don't permit 'modifying'
        this->funcTypeNode->semantic_pass();
        this->suite->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};
