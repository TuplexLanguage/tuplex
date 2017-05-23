#pragma once

// (requires types, expressions, and statements to be included before this)


/** Constructs a new TxFieldDefNode based on a TxFieldTypeDefNode (the new copy is independently allocated). */
inline TxFieldDefNode* make_field_def_node( TxArgTypeDefNode* fieldTypeDef ) {
    return new TxFieldDefNode( fieldTypeDef->parseLocation, fieldTypeDef->fieldName, fieldTypeDef->typeExpression->make_ast_copy(), nullptr );
}

class TxFunctionHeaderNode : public TxTypeExpressionNode {
    TxFunctionTypeNode* funcTypeNode;  // (creates implicit declaration for the function type)

protected:
    virtual void typeexpr_declaration_pass() override {
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
    TxTypeExpressionNode* selfTypeNode = nullptr;
    TxFieldDefNode* selfRefNode = nullptr;
    TxFieldDefNode* superRefNode = nullptr;
    const TxTypeDeclaration* constructedObjTypeDecl = nullptr;

protected:
    virtual void declaration_pass() override;

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
            this->selfTypeNode = new TxNamedTypeNode( this->parseLocation, "Self" );
            TxTypeExpressionNode* selfRefTargetTypeNode;
            if ( this->funcHeaderNode->is_modifying() )
                selfRefTargetTypeNode = new TxModifiableTypeNode( this->parseLocation, this->selfTypeNode );
            else
                selfRefTargetTypeNode = this->selfTypeNode;
            auto selfRefTypeExprN = new TxReferenceTypeNode( this->parseLocation, nullptr, selfRefTargetTypeNode );
            this->selfRefNode = new TxFieldDefNode( this->parseLocation, "self", selfRefTypeExprN, nullptr );

            // 'super' reference
            auto superRefTypeExprN = new TxNamedTypeNode( this->parseLocation, "Super" );
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

    /** Returns true if this method is suppressed (as if it were abstract) due to being a modifying instance method
     * in an immutable specialization of a mutable generic type. */
    bool is_suppressed_modifying_method();

    virtual void symbol_resolution_pass() override;

    virtual bool is_statically_constant() const override {
        return true; //!this->instanceMethod;
    }

    llvm::Function* code_gen_forward_decl( LlvmGenerationContext& context ) const;
    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->funcHeaderNode->visit_ast( visitor, thisCursor, "functype", context );
        if ( this->is_instance_method() ) {
            this->selfRefNode->visit_ast( visitor, thisCursor, "selfref", context );
            this->superRefNode->visit_ast( visitor, thisCursor, "superref", context );
        }
        this->suite->visit_ast( visitor, thisCursor, "suite", context );
    }
};
