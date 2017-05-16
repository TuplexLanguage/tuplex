#pragma once

// (requires types, expressions, and statements to be included before this)


/** Constructs a new TxFieldDefNode based on a TxFieldTypeDefNode (the new copy is independently allocated). */
inline TxFieldDefNode* make_field_def_node( TxArgTypeDefNode* fieldTypeDef ) {
    return new TxFieldDefNode( fieldTypeDef->parseLocation, fieldTypeDef->fieldName, fieldTypeDef->typeExpression->make_ast_copy(), nullptr );
}

class TxFunctionHeaderNode : public TxTypeExpressionNode {
    TxFunctionTypeNode* funcTypeNode;  // (creates implicit declaration for the function type)

protected:
    virtual void declaration_pass() override {
        for ( auto argField : *this->arguments ) {
            argField->declare_field( lexContext.scope(), TXD_NONE, TXS_STACK );
        }
        if ( this->returnField ) {
            this->returnField->declare_field( lexContext.scope(), TXD_IMPLICIT, TXS_STACK );
        }
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
              returnField( funcTypeNode->returnField ? make_field_def_node( funcTypeNode->returnField ) : nullptr ) {
        for ( auto arg : *funcTypeNode->arguments )
            this->arguments->push_back( make_field_def_node( arg ) );
    }

    virtual TxFunctionHeaderNode* make_ast_copy() const override {
        return new TxFunctionHeaderNode( this->funcTypeNode->make_ast_copy() );
    }

    /** Returns true if this function header is declared 'modifying', i.e. may modify its closure. */
    bool is_modifying() const {
        return this->funcTypeNode->modifying;
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

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->funcTypeNode->visit_ast( visitor, thisCursor, "functype", context );
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
    const TxTypeDeclaration* constructedObjTypeDecl = nullptr;

protected:
    virtual void declaration_pass() override {
        std::string funcName = ( this->fieldDefNode && this->fieldDefNode->get_declaration() )
                                      ? this->fieldDefNode->get_declaration()->get_unique_name()
                                      : "";
        TxScopeSymbol* funcScope = lexContext.scope()->create_code_block_scope( *this, funcName );
        if ( this->is_instance_method() ) {
            auto entitySym = dynamic_cast<TxEntitySymbol*>( lexContext.scope() );
            if ( entitySym && entitySym->get_type_decl() ) {  // if in type scope
                if ( this->fieldDefNode->get_declaration()->get_decl_flags() & TXD_CONSTRUCTOR ) {
                    // this is a constructor
                    this->constructedObjTypeDecl = entitySym->get_type_decl();
                }
            }
            else
                CERROR( this, "The scope of instance method must be a type scope: " << lexContext.scope() );

            this->selfRefNode->declare_field( funcScope, TXD_NONE, TXS_STACK );
            this->superRefNode->declare_field( funcScope, TXD_NONE, TXS_STACK );
        }
        this->lexContext._scope = funcScope;
        this->lexContext.enclosingLambda = this;
        // FUTURE: define implicit closure object when in code block
    }

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
            TxTypeExpressionNode* selfTypeNode;
            if ( this->funcHeaderNode->is_modifying() ) {
                selfTypeNode = new TxModifiableTypeNode( this->parseLocation, new TxNamedTypeNode( this->parseLocation, "$Self" ) );
            }
            else {
                selfTypeNode = new TxNamedTypeNode( this->parseLocation, "$Self" );
            }
            auto selfRefTypeExprN = new TxReferenceTypeNode( this->parseLocation, nullptr, selfTypeNode );
            this->selfRefNode = new TxFieldDefNode( this->parseLocation, "self", selfRefTypeExprN, nullptr );

            // 'super' reference
            auto superRefTypeExprN = new TxNamedTypeNode( this->parseLocation, "$Super" );
            this->superRefNode = new TxFieldDefNode( this->parseLocation, "super", superRefTypeExprN, nullptr );
            // FUTURE: if type is modifiable, the super target type should in some cases perhaps be modifiable as well?
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

    /** If non-null, this is a constructor and the declaration for the constructed object type is returned. */
    inline const TxTypeDeclaration* get_constructed() const {
        return this->constructedObjTypeDecl;
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        if ( this->is_instance_method() ) {
            try {
                this->selfRefNode->symbol_resolution_pass();
                this->superRefNode->symbol_resolution_pass();
            }
            catch ( const resolution_error& err ) {
                LOG( this->LOGGER(), DEBUG, "Caught resolution error in self/super of instance method " << this << ": " << err );
            }
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

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->funcHeaderNode->visit_ast( visitor, thisCursor, "functype", context );
        if ( this->is_instance_method() ) {
            this->selfRefNode->visit_ast( visitor, thisCursor, "selfref", context );
            this->superRefNode->visit_ast( visitor, thisCursor, "superref", context );
        }
        this->suite->visit_ast( visitor, thisCursor, "suite", context );
    }
};
