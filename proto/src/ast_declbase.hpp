#pragma once

#include "ast_base.hpp"

#include "symbol/entity_type.hpp"
#include "symbol/type_registry.hpp"


/**
 * The context of this node refers to its outer scope. This node's entity, if any, refers to its inner scope.
 */
class TxTypeExpressionNode : public TxTypeDefiningNode {
    const TxTypeDeclaration* declaration = nullptr;

protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) = 0;

public:
    TxTypeExpressionNode(const TxLocation& parseLocation) : TxTypeDefiningNode(parseLocation) { }

    virtual TxTypeExpressionNode* make_ast_copy() const override = 0;

    virtual std::string get_auto_type_name() const = 0;

    /** Gets the type declaration of this type expression, if any. */
    virtual const TxTypeDeclaration* get_declaration() const { return this->declaration; }

    /** Performs the symbol declaration pass for this type expression.
     * Type expressions evaluate within a "definition context", representing their "outer" scope,
     * and a "lexical context", within which they declare their constituent sub-expressions.
     * The definition context is used for named types lookups, to avoid conflation with names of the sub-expressions.
     */
    virtual void symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext,
                                          const TxTypeDeclaration* owningDeclaration );

    virtual void symbol_resolution_pass() {
        this->resolve_type();
    }
};


class TxFieldDefNode;
class TxMaybeConversionNode;

class TxExpressionNode : public TxTypeDefiningNode {
protected:
    /** injected by field definition if known and applicable */
    const TxFieldDefNode* fieldDefNode = nullptr;

    /** injected by outer expression if applicable */
    std::vector<TxMaybeConversionNode*>* appliedFuncArgs = nullptr;

public:
    TxExpressionNode(const TxLocation& parseLocation) : TxTypeDefiningNode(parseLocation) { }

    virtual TxExpressionNode* make_ast_copy() const override = 0;

    /** Injected by field definition if known and applicable. */
    virtual void set_field_def_node(const TxFieldDefNode* fieldDefNode) {
        this->fieldDefNode = fieldDefNode;
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) = 0;

    virtual void symbol_resolution_pass() {
        this->resolve_type();
    }

    /** Returns true if this expression is a stack allocation expression,
     * i.e. its result is in newly allocated stack space, and the allocation's type is the type of this expression.
     * Note that sub-expressions may perform allocations without this expression being an allocation. */
    // TODO: review combinatorial expressions that maybe should return true if any of their sub-expressions return true
    virtual bool is_stack_allocation_expression() const { return false; }

    /** Returns true if this expression is a constant expression that can be evaluated at compile time. */
    virtual bool is_statically_constant() const { return false; }

    /** If this expression can currently be statically evaluated,
     * a TxConstantProxy representing its value is returned, otherwise nullptr.
     * In future, this should return non-null for all expressions for which is_statically_constant() returns true.
     */
    virtual const TxConstantProxy* get_static_constant_proxy() const {
        return nullptr;
    }

    virtual const std::vector<TxMaybeConversionNode*>* get_applied_func_args() {
        return this->appliedFuncArgs;
    }
    virtual void set_applied_func_args( std::vector<TxMaybeConversionNode*>* appliedFuncArgs ) {
        this->appliedFuncArgs = appliedFuncArgs;
    }

    /** Generates code that produces the type id (as opposed to the value) of this expression. */
    virtual llvm::Value* code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const;
};


/** A conversion placeholder node which can wrap a specific conversion around an expression if necessary. */
class TxMaybeConversionNode : public TxExpressionNode {
    TxExpressionNode* conversionExpr = nullptr;

protected:
    virtual const TxType* define_type() override {
        auto expr = this->get_spec_expression();
        return expr->resolve_type();
    }

public:
    TxExpressionNode* const originalExpr;

    TxMaybeConversionNode(TxExpressionNode* originalExpr)
            : TxExpressionNode(originalExpr->parseLocation), originalExpr(originalExpr) {
        ASSERT( originalExpr, "NULL originalExpr" );
        ASSERT( !dynamic_cast<TxMaybeConversionNode*>( originalExpr ),
                "Can't wrap a TxMaybeConversionNode with another TxMaybeConversionNode: " << originalExpr );
    }

    virtual TxMaybeConversionNode* make_ast_copy() const override {
        ASSERT(false, "Can't make AST copy of a TxMaybeConversionNode: " << this);
        return nullptr;
    }

    void insert_conversion( const TxType* resultType, bool _explicit=false );

    inline TxExpressionNode* get_spec_expression() const {
        return (this->conversionExpr ? this->conversionExpr : this->originalExpr);
    }

//    virtual const TxType* attempt_get_type() const override { return this->get_spec_expression()->attempt_get_type(); }
//    virtual const TxType* get_type        () const override { return this->get_spec_expression()->get_type();         }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        auto expr = this->get_spec_expression();
        if (! expr->is_context_set())
            expr->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        auto expr = this->get_spec_expression();
        expr->symbol_resolution_pass();
    }

    virtual bool is_stack_allocation_expression() const override {
        return this->get_spec_expression()->is_stack_allocation_expression();
    }

    virtual bool is_statically_constant() const override {
        return this->get_spec_expression()->is_statically_constant();
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        return this->get_spec_expression()->get_static_constant_proxy();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        if (this->conversionExpr)
            this->conversionExpr->visit_ast( visitor, thisAsParent, "convertee", context );
        else
            this->originalExpr->visit_ast( visitor, thisAsParent, "unconverted", context );
    }
};


class TxFieldDeclNode;

class TxFieldDefNode : public TxFieldDefiningNode {
    /** injected by non-local field declaration if applicable */
    TxFieldDeclNode* fieldDeclNode = nullptr;

    const TxFieldDeclaration* declaration = nullptr;

    void symbol_declaration_pass( LexicalContext& outerContext, LexicalContext& innerContext, TxDeclarationFlags declFlags) {
        this->set_context( outerContext);
        if (this->typeExpression)
            this->typeExpression->symbol_declaration_pass( innerContext, innerContext, nullptr );
        if (this->initExpression)
            this->initExpression->symbol_declaration_pass( outerContext );
    };

protected:
    virtual const TxType* define_type() override {
        LOG_TRACE(this->LOGGER(), "defining  type  of " << this);
        const TxType* type;
        if (this->typeExpression) {
            type = this->typeExpression->resolve_type();
            // also resolve initExpression from here, which guards against recursive field value initialization:
            if (this->initExpression)
                this->initExpression->resolve_type();
        }
        else {
            type = this->initExpression->resolve_type();
            if (type) {
                if (this->modifiable) {
                    if (! type->is_modifiable())
                        type = this->types().get_modifiable_type(nullptr, type);
                }
                else if (type->is_modifiable())
                    // if initialization expression is modifiable type, and modifiable not explicitly specified,
                    // lose modifiable attribute (modifiability must be explicit)
                    type = type->get_semantic_base_type();
            }
        }
        return type;
    }

    virtual const TxField* define_field() override {
        LOG_TRACE(this->LOGGER(), "defining  field of " << this);
        ASSERT(this->attempt_get_type(), "Expected non-NULL type in " << this);
        if (this->declaration) {
            if (auto field = TxField::make_field( this->declaration, this->attempt_get_type() )) {
                return field;
            }
        }
        // else is not an error - function type's arguments & return type lack field declarations
        return nullptr;
    }

public:
    const TxIdentifier* fieldName;
    const bool modifiable;  // true if field name explicitly declared modifiable
    TxTypeExpressionNode* typeExpression;
    TxMaybeConversionNode* initExpression;

    TxFieldDefNode(const TxLocation& parseLocation, const std::string& fieldName,
                   TxTypeExpressionNode* typeExpression, TxExpressionNode* initExpression, bool modifiable=false)
            : TxFieldDefiningNode(parseLocation), fieldName(new TxIdentifier(fieldName)), modifiable(modifiable) {
        this->typeExpression = typeExpression;
        if (initExpression) {
            initExpression->set_field_def_node(this);
            this->initExpression = new TxMaybeConversionNode(initExpression);
        }
        else {
            ASSERT(typeExpression, "At least one of typeExpression and initExpression must be specified");
            this->initExpression = nullptr;
        }
    }

    virtual TxFieldDefNode* make_ast_copy() const override {
        TxTypeExpressionNode* typeExpr = ( this->typeExpression ? this->typeExpression->make_ast_copy() : nullptr );
        TxExpressionNode*     initExpr = ( this->initExpression ? this->initExpression->originalExpr->make_ast_copy() : nullptr );
        return new TxFieldDefNode( this->parseLocation, this->fieldName->str(), typeExpr, initExpr, this->modifiable );
    }

    void symbol_declaration_pass_local_field( LexicalContext& lexContext, bool create_local_scope, TxDeclarationFlags declFlags=TXD_NONE ) {
        LexicalContext outerCtx(lexContext);  // prevents init expr from referring to this field
        if (create_local_scope)
            lexContext.scope( lexContext.scope()->create_code_block_scope( *this ) );
        this->declaration = lexContext.scope()->declare_field(this->fieldName->str(), this, declFlags, TXS_STACK, TxIdentifier(""));
        this->symbol_declaration_pass( outerCtx, lexContext, declFlags);
    }

    void symbol_declaration_pass_nonlocal_field( LexicalContext& lexContext, TxFieldDeclNode* fieldDeclNode, TxDeclarationFlags declFlags,
                                                 TxFieldStorage storage, const TxIdentifier& dataspace ) {
        this->fieldDeclNode = fieldDeclNode;  // enables support for usage-order code generation of non-local fields
        TxDeclarationFlags fieldFlags = declFlags;
        std::string declName = this->fieldName->str();
        if (*this->fieldName == "self") {
            // handle constructor declaration
            declName = CONSTR_IDENT;
            fieldFlags = fieldFlags | TXD_CONSTRUCTOR;
            if (storage != TXS_INSTANCEMETHOD)
                CERROR(this, "Illegal declaration name for non-constructor member: " << this->fieldName);
        }

        this->declaration = lexContext.scope()->declare_field(declName, this, fieldFlags, storage, dataspace);
        this->symbol_declaration_pass( lexContext, lexContext, declFlags);
    }

    void symbol_declaration_pass_functype_arg( LexicalContext& lexContext ) {
        this->symbol_declaration_pass( lexContext, lexContext, TXD_NONE);
    }

    virtual void symbol_resolution_pass() {
        if (auto field = this->resolve_field()) {
            if (this->initExpression) {
                if (this->typeExpression) {
                    this->typeExpression->symbol_resolution_pass();
                    auto ltype = field->get_type();
                    auto nonModLType = ( ltype->is_modifiable() ? ltype->get_base_type() : ltype );  // rvalue doesn't need to be modifiable
                    this->initExpression->insert_conversion( nonModLType );
                }
                this->initExpression->symbol_resolution_pass();

                auto storage = field->get_storage();
                if ( storage == TXS_GLOBAL
                     || ( ( storage == TXS_STATIC || storage == TXS_VIRTUAL )
                          && ! field->get_type()->is_modifiable() ) ) {
                    // field is expected to have a statically constant initializer
                    // (Note: When static initializers in types are supported, static/virtual fields' initialization may be deferred.)
                    if (! this->initExpression->is_statically_constant())
                        CERROR(this, "Non-constant initializer for constant global/static/virtual field" << this->fieldName);
                }
            }
            else {  // if initExpression is null then typeExpression is set
                this->typeExpression->symbol_resolution_pass();
            }

            if (! field->get_type()->is_concrete())
                if ( ! ( field->get_type()->get_declaration() && ( field->get_type()->get_declaration()->get_decl_flags() & TXD_GENPARAM ) ) )
                    CERROR(this, "Field type is not a concrete type (size potentially unknown): " << this->get_identifier() << " : " << field->get_type());
            if (this->get_declaration()->get_decl_flags() & TXD_CONSTRUCTOR) {
                // TODO: check that constructor function type has void return value
            }
        }
        else {
            if (! this->get_type())
                CERROR(this, "Failed to resolve field " << this->get_identifier());
            if (this->initExpression) {
                if (this->typeExpression)
                    this->typeExpression->symbol_resolution_pass();
                this->initExpression->symbol_resolution_pass();
            }
            else  // if initExpression is null then typeExpression is set
                this->typeExpression->symbol_resolution_pass();
        }
    }

    virtual const TxExpressionNode* get_init_expression() const {
        return this->initExpression;
    }

    const TxFieldDeclaration* get_declaration() const {
        ASSERT(this->declaration, "field declaration not initialized for " << this->fieldName);
        return this->declaration;
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        if (this->typeExpression)
            this->typeExpression->visit_ast( visitor, thisAsParent, "type", context );
        if (this->initExpression)
            this->initExpression->visit_ast( visitor, thisAsParent, "initializer", context );
    }

    virtual const TxIdentifier* get_identifier() const override { return this->fieldName; }
};

/** Non-local field declaration */
class TxFieldDeclNode : public TxDeclarationNode {
    const bool isMethodSyntax = false;

// experimental
//    /** code value generated for this node (supports generation in usage order instead of lexical order) */
//    mutable llvm::Value* codeGenValue = nullptr;

public:
    TxFieldDefNode* field;

    TxFieldDeclNode(const TxLocation& parseLocation, const TxDeclarationFlags declFlags, TxFieldDefNode* field,
                    bool isMethodSyntax=false)
            : TxDeclarationNode(parseLocation, declFlags), isMethodSyntax(isMethodSyntax), field(field) { }

    virtual TxFieldDeclNode* make_ast_copy() const override {
        return new TxFieldDeclNode( this->parseLocation, this->declFlags, this->field->make_ast_copy(), this->isMethodSyntax );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorDecl ) override;

    virtual void symbol_resolution_pass() override;

    virtual const TxFieldDeclaration* get_declaration() const override {
        return this->field->get_declaration();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->field->visit_ast( visitor, thisAsParent, "field", context );
    }
};


/** Non-local type declaration */
class TxTypeDeclNode : public TxDeclarationNode {
public:
    const TxIdentifier* typeName;
    const bool interfaceKW;
    const std::vector<TxDeclarationNode*>* typeParamDecls;
    TxTypeExpressionNode* typeExpression;

    TxTypeDeclNode(const TxLocation& parseLocation, const TxDeclarationFlags declFlags,
                   const std::string& typeName, const std::vector<TxDeclarationNode*>* typeParamDecls,
                   TxTypeExpressionNode* typeExpression, bool interfaceKW=false)
        : TxDeclarationNode(parseLocation, declFlags),
          typeName(new TxIdentifier(typeName)), interfaceKW(interfaceKW), typeParamDecls(typeParamDecls), typeExpression(typeExpression) {
        validateTypeName(this, declFlags, typeName);
    }

    virtual TxTypeDeclNode* make_ast_copy() const override {
        return new TxTypeDeclNode( this->parseLocation, this->declFlags, this->typeName->str(),
                                   make_node_vec_copy( this->typeParamDecls ),
                                   this->typeExpression->make_ast_copy(), this->interfaceKW);
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorDecl=false ) override {
        this->symbol_declaration_pass( lexContext, lexContext, isExpErrorDecl );
    }
    virtual void symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext, bool isExpErrorDecl=false );

    virtual void symbol_resolution_pass() override {
        if (this->typeParamDecls)
            for (auto paramDecl : *this->typeParamDecls)
                paramDecl->symbol_resolution_pass();
        this->typeExpression->symbol_resolution_pass();
    }

    virtual const TxTypeDeclaration* get_declaration() const override {
        return this->typeExpression->get_declaration();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        if (this->typeParamDecls)
            for (auto decl : *this->typeParamDecls)
                decl->visit_ast( visitor, thisAsParent, "type-param", context );
        this->typeExpression->visit_ast( visitor, thisAsParent, "type", context );
    }

    virtual const TxIdentifier* get_identifier() const override { return this->typeName; }
};


class TxAssigneeNode : public TxTypeDefiningNode {
public:
    TxAssigneeNode(const TxLocation& parseLocation) : TxTypeDefiningNode(parseLocation) { }

    virtual TxAssigneeNode* make_ast_copy() const override = 0;

    virtual void symbol_declaration_pass( LexicalContext& lexContext) = 0;

    virtual void symbol_resolution_pass() {
        this->resolve_type();
    }
};



class TxExpErrDeclNode : public TxDeclarationNode {
    ExpectedErrorClause* expError;

public:
    TxDeclarationNode* body;

    TxExpErrDeclNode( const TxLocation& parseLocation, ExpectedErrorClause* expError, TxDeclarationNode* body )
        : TxDeclarationNode( parseLocation, (body ? body->get_decl_flags() : TXD_NONE) | TXD_EXPERRBLOCK),
          expError( expError ), body( body )  {
    }

    virtual TxExpErrDeclNode* make_ast_copy() const override {
        return new TxExpErrDeclNode( this->parseLocation, nullptr, ( this->body ? this->body->make_ast_copy() : nullptr ) );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorDecl ) override {
        this->set_context( LexicalContext( lexContext, lexContext.scope(), expError ) );
        if (isExpErrorDecl)
            CERROR(this, "Can't next Expected Error constructs in a declaration");
        if (this->body) {
            if (! this->context().is_reinterpretation()) {
                this->get_parse_location().parserCtx->register_exp_err_node( this );
                this->get_parse_location().parserCtx->begin_exp_err( this );
                this->body->symbol_declaration_pass( this->context(), true );
                this->get_parse_location().parserCtx->end_exp_err( this->parseLocation );
            }
            else
                this->body->symbol_declaration_pass( this->context(), true );
        }
    }

    virtual void symbol_resolution_pass() override {
        auto ctx = this->context();
        if (this->body) {
            if (! ctx.is_reinterpretation()) {
                this->get_parse_location().parserCtx->begin_exp_err( this );
                this->body->symbol_resolution_pass();
                this->get_parse_location().parserCtx->end_exp_err( this->parseLocation );
            }
            else
                this->body->symbol_resolution_pass();
        }
    }

    virtual const TxEntityDeclaration* get_declaration() const override {
        return ( this->body ? this->body->get_declaration() : nullptr );
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        if (this->body)
            this->body->visit_ast( visitor, thisAsParent, "decl", context );
    }
};
