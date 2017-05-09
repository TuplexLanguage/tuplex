#pragma once

// (requires types, expressions, and statements to be included before this)

class TxFunctionHeaderNode : public TxTypeExpressionNode {
    TxFunctionTypeNode* funcTypeNode;

protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& lexContext ) override {
        this->funcTypeNode->symbol_declaration_pass( lexContext );  // (creates implicit declaration for the function type)

        // declare the function args, and the return type if any:
        for ( auto argField : *this->arguments )
            argField->symbol_declaration_pass_local_field( lexContext );
        if ( this->returnField )
            this->returnField->symbol_declaration_pass_local_field( lexContext, TXD_IMPLICIT );
    }

    virtual const TxType* define_type() override {
        return this->funcTypeNode->resolve_type();
    }

public:
    std::vector<TxFieldDefNode*>* arguments;
    TxFieldDefNode* returnField;

    TxFunctionHeaderNode( TxFunctionTypeNode* funcTypeNode )
            : TxTypeExpressionNode( funcTypeNode->parseLocation ), funcTypeNode( funcTypeNode ),
              arguments( new std::vector<TxFieldDefNode*>() ),
              returnField( funcTypeNode->returnField ? new TxFieldDefNode( funcTypeNode->returnField->make_ast_copy() ) : nullptr ) {
        for ( auto arg : *funcTypeNode->arguments )
            this->arguments->push_back( new TxFieldDefNode( arg->make_ast_copy() ) );
    }

    virtual TxFunctionHeaderNode* make_ast_copy() const override {
        return new TxFunctionHeaderNode( this->funcTypeNode->make_ast_copy() );
    }

    virtual std::string get_auto_type_name() const override {
        return this->get_declaration()->get_unique_full_name();
    }

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        this->funcTypeNode->symbol_resolution_pass();
        for ( auto argField : *this->arguments )
            argField->symbol_resolution_pass();
        if ( this->returnField )
            this->returnField->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) const override {
        // (for now, we don't treat funcTypeNode as a visited descendant)
        for ( auto argField : *this->arguments )
            argField->visit_ast( visitor, thisCursor, "arg", context );
        if ( this->returnField )
            this->returnField->visit_ast( visitor, thisCursor, "return", context );
    }
};

class TxLambdaExprNode : public TxExpressionNode {
    bool instanceMethod = false;
    TxFieldDefNode* selfRefNode = nullptr;
    TxFieldDefNode* superRefNode = nullptr;

protected:
    virtual const TxType* define_type() override {
        return this->funcHeaderNode->resolve_type();
    }

public:
    TxFunctionHeaderNode* funcHeaderNode;
    TxSuiteNode* suite;
    const bool isMethodSyntax;

    TxLambdaExprNode( const TxLocation& parseLocation, TxFunctionTypeNode* funcTypeNode, TxSuiteNode* suite, bool isMethodSyntax = false )
            : TxLambdaExprNode( parseLocation, new TxFunctionHeaderNode( funcTypeNode ), suite, isMethodSyntax ) {
    }

    TxLambdaExprNode( const TxLocation& parseLocation, TxFunctionHeaderNode* funcHeaderNode, TxSuiteNode* suite, bool isMethodSyntax = false )
            : TxExpressionNode( parseLocation ), funcHeaderNode( funcHeaderNode ), suite( suite ), isMethodSyntax( isMethodSyntax ) {
        if ( isMethodSyntax ) {
            // 'self' reference:
            // FUTURE: if type is immutable, the reference target type should perhaps not be modifiable?
            auto selfRefTypeExprN = new TxReferenceTypeNode(
                    this->parseLocation, nullptr,
                    new TxModifiableTypeNode( this->parseLocation, new TxNamedTypeNode( this->parseLocation, "$Self" ) ) );
            this->selfRefNode = new TxFieldDefNode( this->parseLocation, "self", selfRefTypeExprN, nullptr );
            // 'super' reference
            auto superRefTypeExprN = new TxNamedTypeNode( this->parseLocation, "$Super" );
            this->superRefNode = new TxFieldDefNode( this->parseLocation, "super", superRefTypeExprN, nullptr );
        }
    }

    virtual TxLambdaExprNode* make_ast_copy() const override {
        return new TxLambdaExprNode( this->parseLocation, this->funcHeaderNode->make_ast_copy(), this->suite->make_ast_copy(), this->isMethodSyntax );
    }

    void set_instance_method( bool flag ) {
        if ( flag && !this->isMethodSyntax ) {
            CERROR( this, "Function definition was expected to have instance method syntax" );
            this->instanceMethod = false;
        }
        else
            this->instanceMethod = flag;
    }

    /** Returns true if this lambda expression is an instance method (with a runtime-provided 'self' argument). */
    inline bool is_instance_method() const {
        return this->instanceMethod;
    }

    virtual void symbol_declaration_pass( const LexicalContext& lexContext ) override {
        std::string funcName = ( this->fieldDefNode && this->fieldDefNode->get_declaration() )
                                      ? this->fieldDefNode->get_declaration()->get_unique_name()
                                      : "";
        LexicalContext funcLexContext;
        if ( this->is_instance_method() ) {
            const TxTypeDeclaration* constructedObjTypeDecl = nullptr;
            auto entitySym = dynamic_cast<TxEntitySymbol*>( lexContext.scope() );
            if ( entitySym && entitySym->get_type_decl() ) {  // if in type scope
                if ( this->fieldDefNode->get_declaration()->get_decl_flags() & TXD_CONSTRUCTOR ) {
                    // this is a constructor
                    constructedObjTypeDecl = entitySym->get_type_decl();
                }
            }
            else
                CERROR( this, "The scope of an instance method must be a type scope" );

            funcLexContext = LexicalContext( lexContext, lexContext.scope()->create_code_block_scope( *this, funcName ), constructedObjTypeDecl );
            this->selfRefNode->symbol_declaration_pass_local_field( funcLexContext );
            this->superRefNode->symbol_declaration_pass_local_field( funcLexContext );
        }
        else {
            funcLexContext = LexicalContext( lexContext, lexContext.scope()->create_code_block_scope( *this, funcName ) );
        }
        // FUTURE: define implicit closure object when in code block

        this->set_context( funcLexContext );

        this->funcHeaderNode->symbol_declaration_pass( funcLexContext );  // function header
        this->suite->symbol_declaration_pass_no_subscope( funcLexContext );  // function body
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        if ( this->is_instance_method() ) {
            this->selfRefNode->symbol_resolution_pass();
            this->superRefNode->symbol_resolution_pass();
        }
        this->funcHeaderNode->symbol_resolution_pass();  // function header
        this->suite->symbol_resolution_pass();  // function body

        if ( this->funcHeaderNode->returnField ) {
            // verify that body always ends with explicit return statement
            if ( !this->suite->ends_with_return_stmt() )
                CERROR( this, "Function has return value, but not all code paths end with a return statement." );
        }
        // TODO: if in global scope, don't permit 'modifying'
    }

//    /** Returns false if this function may modify its closure when run, i.e. have side effects.
//     * A modifying function is not regarded as statically constant since its closure may be modified when run.
//     */
//    virtual bool is_constant_closure() const { return this->funcTypeNode->get_type()->is_immutable(); }

    /** Returns true if this expression is a constant expression that can be evaluated at compile time. */
    virtual bool is_statically_constant() const override {
        return !this->is_instance_method();
    }

    llvm::Function* code_gen_forward_decl( LlvmGenerationContext& context, GenScope* scope ) const;
    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) const override {
        this->funcHeaderNode->visit_ast( visitor, thisCursor, "functype", context );
        if ( this->selfRefNode ) {
            this->selfRefNode->visit_ast( visitor, thisCursor, "selfref", context );
            this->superRefNode->visit_ast( visitor, thisCursor, "superref", context );
        }
        this->suite->visit_ast( visitor, thisCursor, "suite", context );
    }
};
