#pragma once

// (requires types, expressions, and statements to be included before this)


class TxLambdaExprNode : public TxExpressionNode {
    const bool isMethodSyntax;
    bool instanceMethod = false;
    TxFieldDefNode* selfRefNode = nullptr;

protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        return this->funcTypeNode->resolve_type(resCtx);  // function header
    }

public:
    TxFunctionTypeNode* funcTypeNode;
    TxSuiteNode* suite;

    TxLambdaExprNode(const yy::location& parseLocation, TxFunctionTypeNode* funcTypeNode, TxSuiteNode* suite,
                     bool isMethodSyntax=false)
            : TxExpressionNode(parseLocation), isMethodSyntax(isMethodSyntax), funcTypeNode(funcTypeNode), suite(suite) {
    }

    void set_instance_method(bool flag) {
        if (flag && !this->isMethodSyntax)
            this->cerror("Function definition was expected to have instance method syntax");
        this->instanceMethod = flag;
    }

    /** Returns true if this lambda expression is an instance method (with a runtime-provided 'self' argument). */
    inline bool is_instance_method() const { return this->instanceMethod; }

    virtual bool has_predefined_type() const override { return false; }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) {
        std::string funcName = this->fieldDefNode ? this->fieldDefNode->get_entity()->get_name() : "";
        LexicalContext funcLexContext(lexContext.scope()->create_code_block_scope(funcName));
        this->set_context(funcLexContext);

        if (this->is_instance_method()) {
            // insert implicit local field named 'self', that is a reference to the closure type
            if (auto typeEntity = dynamic_cast<TxTypeEntity*>(lexContext.scope())) {  // if in type scope
                auto selfTypeNameN = new TxIdentifierNode(this->parseLocation, new TxIdentifier(typeEntity->get_full_name()));
                auto selfTypeExprN = new TxPredefinedTypeNode(this->parseLocation, selfTypeNameN);
                TxTypeExpressionNode* selfRefTypeExpr = new TxReferenceTypeNode(this->parseLocation, nullptr, selfTypeExprN);
                this->selfRefNode = new TxFieldDefNode(this->parseLocation, "self", selfRefTypeExpr, nullptr);
                this->selfRefNode->symbol_declaration_pass_local_field(funcLexContext, false);
            }
            else
                this->cerror("The scope of an instance method must be a type scope");
        }
        // FUTURE: define implicit closure object when in code block

        this->funcTypeNode->symbol_declaration_pass_func_header(funcLexContext, funcLexContext);  // function header
        this->suite->symbol_declaration_pass_no_subscope(funcLexContext);  // function body
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        if (this->selfRefNode)
            this->selfRefNode->symbol_resolution_pass(resCtx);
        this->funcTypeNode->symbol_resolution_pass(resCtx);  // function header
        this->suite->symbol_resolution_pass(resCtx);  // function body

        if (this->funcTypeNode->returnField) {
            // verify that suite ends with return statement
            if (this->suite->suite->empty() || ! dynamic_cast<TxReturnStmtNode*>(this->suite->suite->back()))
                this->cerror("Function has return value, but does not end with a return statement.");
        }
        // TODO: if in global scope, don't permit 'modifying'
    }

//    /** Returns false if this function may modify its closure when run, i.e. have side effects.
//     * A modifying function is not regarded as statically constant since its closure may be modified when run.
//     */
//    virtual bool is_constant_closure() const { return this->funcTypeNode->get_type()->is_immutable(); }

    /** Returns true if this expression is a constant expression that can be evaluated at compile time. */
    virtual bool is_statically_constant() const override { return ! this->is_instance_method(); }

    virtual void semantic_pass() {
        if (this->selfRefNode)
            this->selfRefNode->semantic_pass();
        this->funcTypeNode->semantic_pass();
        this->suite->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};
