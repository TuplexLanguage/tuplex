#pragma once

// (requires types, expressions, and statements to be included before this)


class TxLambdaExprNode : public TxExpressionNode {
    bool instanceMethod = false;
    TxFieldDefNode* selfRefNode = nullptr;
    TxFieldDefNode* superRefNode = nullptr;

protected:
    virtual const TxType* define_type() override {
        return this->funcTypeNode->resolve_type();  // function header
    }

public:
    TxFunctionTypeNode* funcTypeNode;
    TxSuiteNode* suite;
    const bool isMethodSyntax;

    TxLambdaExprNode(const TxLocation& parseLocation, TxFunctionTypeNode* funcTypeNode, TxSuiteNode* suite,
                     bool isMethodSyntax=false)
            : TxExpressionNode(parseLocation), funcTypeNode(funcTypeNode), suite(suite), isMethodSyntax(isMethodSyntax) {
        if (isMethodSyntax) {
            // 'self' reference:
            auto selfRefTypeExprN = new TxIdentifiedTypeNode(this->parseLocation, "$Self");
            this->selfRefNode = new TxFieldDefNode(this->parseLocation, "self", selfRefTypeExprN, nullptr);
            // 'super' reference
            auto superRefTypeExprN = new TxIdentifiedTypeNode(this->parseLocation, "$Super");
            this->superRefNode = new TxFieldDefNode(this->parseLocation, "super", superRefTypeExprN, nullptr);
        }
    }

    virtual TxLambdaExprNode* make_ast_copy() const override {
        return new TxLambdaExprNode( this->parseLocation, this->funcTypeNode->make_ast_copy(), this->suite->make_ast_copy(), this->isMethodSyntax );
    }

    void set_instance_method(bool flag) {
        if (flag && !this->isMethodSyntax)
            CERROR(this, "Function definition was expected to have instance method syntax");
        this->instanceMethod = flag;
    }

    /** Returns true if this lambda expression is an instance method (with a runtime-provided 'self' argument). */
    inline bool is_instance_method() const { return this->instanceMethod; }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        std::string funcName = (this->fieldDefNode && this->fieldDefNode->get_declaration()) ?
                                    this->fieldDefNode->get_declaration()->get_unique_name() : "";
        LexicalContext funcLexContext( lexContext, lexContext.scope()->create_code_block_scope( *this, funcName ) );

        if (this->is_instance_method()) {
            auto entitySym = dynamic_cast<TxEntitySymbol*>(lexContext.scope());
            if (entitySym && entitySym->get_type_decl()) {  // if in type scope
                if (this->fieldDefNode->get_declaration()->get_decl_flags() & TXD_CONSTRUCTOR) {
                    // this is a constructor
                    auto constructedObjTypeDecl = entitySym->get_type_decl();
                    funcLexContext.set_constructed(constructedObjTypeDecl);
                }
            }
            else
                CERROR(this, "The scope of an instance method must be a type scope");

            this->selfRefNode->symbol_declaration_pass_local_field( funcLexContext, false );
            this->superRefNode->symbol_declaration_pass_local_field( funcLexContext, false );
        }
        // FUTURE: define implicit closure object when in code block

        this->set_context( funcLexContext);

        this->funcTypeNode->symbol_declaration_pass_func_header( funcLexContext );  // function header
        this->suite->symbol_declaration_pass_no_subscope( funcLexContext );  // function body
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        if (this->is_instance_method()) {
            this->selfRefNode->symbol_resolution_pass();
            this->superRefNode->symbol_resolution_pass();
        }
        this->funcTypeNode->symbol_resolution_pass();  // function header
        this->suite->symbol_resolution_pass();  // function body

        if (this->funcTypeNode->returnField) {
            // verify that body always ends with explicit return statement
            if (! this->suite->ends_with_return_stmt())
                CERROR(this, "Function has return value, but not all code paths end with a return statement.");
        }
        // TODO: if in global scope, don't permit 'modifying'
    }

//    /** Returns false if this function may modify its closure when run, i.e. have side effects.
//     * A modifying function is not regarded as statically constant since its closure may be modified when run.
//     */
//    virtual bool is_constant_closure() const { return this->funcTypeNode->get_type()->is_immutable(); }

    /** Returns true if this expression is a constant expression that can be evaluated at compile time. */
    virtual bool is_statically_constant() const override { return ! this->is_instance_method(); }

    llvm::Function* code_gen_forward_decl(LlvmGenerationContext& context, GenScope* scope) const;
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->funcTypeNode->visit_ast( visitor, thisAsParent, "functype", context );
        if (this->selfRefNode) {
            this->selfRefNode->visit_ast( visitor, thisAsParent, "selfref", context );
            this->superRefNode->visit_ast( visitor, thisAsParent, "superref", context );
        }
        this->suite->visit_ast( visitor, thisAsParent, "suite", context );
    }
};
