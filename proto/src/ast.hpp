#pragma once

#include "ast_base.hpp"
#include "ast_stmts.hpp"
#include "ast_fields.hpp"
#include "ast_types.hpp"
#include "ast_exprs.hpp"


// (in this file since uses both types, expressions, and statements)

class TxLambdaExprNode : public TxExpressionNode {
public:
    TxFunctionTypeNode* funcTypeNode;
    TxSuiteNode* suite;

    TxLambdaExprNode(const yy::location& parseLocation, TxFunctionTypeNode* funcTypeNode, TxSuiteNode* suite)
            : TxExpressionNode(parseLocation), funcTypeNode(funcTypeNode), suite(suite) {
    }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        std::string funcName = this->fieldDefNode ? this->fieldDefNode->get_entity()->get_name() : "";
        LexicalContext funcLexContext(lexContext.scope()->create_code_block_scope(funcName));
        this->set_context(funcLexContext);

        // generate function instance:
        this->funcTypeNode->symbol_table_pass_func_header(funcLexContext);  // function header
        this->suite->symbol_table_pass_no_subscope(funcLexContext);  // function body
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        return this->funcTypeNode->get_type();
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

    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};
