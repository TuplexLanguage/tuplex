#pragma once

// (requires types, expressions, and statements to be included before this)


class TxLambdaExprNode : public TxExpressionNode {
    //bool instanceMethod = false;
    const bool hasClosure;
    TxFieldDefNode* selfRefNode = nullptr;

protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        return this->funcTypeNode->resolve_type(resCtx);  // function header
    }

public:
    TxFunctionTypeNode* funcTypeNode;
    TxSuiteNode* suite;

    TxLambdaExprNode(const yy::location& parseLocation, TxFunctionTypeNode* funcTypeNode, TxSuiteNode* suite,
                     bool hasClosure = false)
            : TxExpressionNode(parseLocation), hasClosure(hasClosure), funcTypeNode(funcTypeNode), suite(suite) {
    }

//    /** Converts this lambda expression into an instance method - adding the implicit self argument. */
//    void make_instance_method() {
//        this->instanceMethod = true;
//    }

    virtual bool has_predefined_type() const override { return false; }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) {
        std::string funcName = this->fieldDefNode ? this->fieldDefNode->get_entity()->get_name() : "";
        LexicalContext funcLexContext(lexContext.scope()->create_code_block_scope(funcName));
        this->set_context(funcLexContext);

        // generate function instance:
        if (this->hasClosure) {
            // insert implicit local field named 'self', that is a reference to the closure type
            if (auto typeEntity = dynamic_cast<TxTypeEntity*>(lexContext.scope())) {  // if in type scope
                auto identNode = new TxIdentifierNode(this->parseLocation, new TxIdentifier(typeEntity->get_full_name()));
                auto identTypeNode = new TxPredefinedTypeNode(this->parseLocation, identNode);
                TxTypeExpressionNode* selfTypeExpr = new TxReferenceTypeNode(this->parseLocation, nullptr, identTypeNode);
                this->selfRefNode = new TxFieldDefNode(this->parseLocation, "self", selfTypeExpr, nullptr);
            }
            else {
                // FUTURE: define implicit closure object when in code block
                cerror("Method definition not in a type scope: %s", lexContext.scope()->to_string().c_str());
            }
        }
        this->funcTypeNode->symbol_declaration_pass_func_header(funcLexContext);  // function header
        this->suite->symbol_declaration_pass_no_subscope(funcLexContext);  // function body
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        if (this->selfRefNode)
            this->selfRefNode->symbol_resolution_pass(resCtx);
        this->funcTypeNode->symbol_resolution_pass(resCtx);  // function header
        this->suite->symbol_resolution_pass(resCtx);  // function body
    }

    /** Returns false if this function may modify its closure when run, i.e. have side effects.
     * A modifying function is not regarded as statically constant since its closure may be modified when run.
     */
    virtual bool is_statically_constant() const override { return this->funcTypeNode->get_type()->is_immutable(); }

    virtual void semantic_pass() {
        // TODO: if in global scope, don't permit 'modifying'
        if (this->selfRefNode)
            this->selfRefNode->semantic_pass();
        this->funcTypeNode->semantic_pass();
        this->suite->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};
