#pragma once

#include "ast_base.hpp"

#include "symbol/type_registry.hpp"


/**
 * The context of this node refers to its outer scope. This node's entity, if any, refers to its inner scope.
 */
class TxTypeExpressionNode : public TxTypeDefiningNode {
    const TxTypeDeclaration* declaration = nullptr;
    /** if parent node is a type declaration that declares type parameters, these will be set by it */
    const std::vector<TxDeclarationNode*>* typeParamDeclNodes = nullptr;

protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext,
                                                      LexicalContext& lexContext, TxDeclarationFlags declFlags ) = 0;

public:
    TxTypeExpressionNode(const TxLocation& parseLocation) : TxTypeDefiningNode(parseLocation) { }

    virtual TxTypeExpressionNode* make_ast_copy() const override = 0;

    /** Returns true if this type expression is a directly identified type
     * (i.e. a previously declared type, does not construct a new type). */
    virtual bool has_predefined_type() const { return false; }

    /** Gets the type declaration of this type expression, if any. */
    inline const TxTypeDeclaration* get_declaration() const { return this->declaration; }

    /** Performs the symbol declaration pass for this type expression.
     * Type expressions evaluate within a "definition context", representing their "outer" scope,
     * and a "lexical context", within which they declare their constituent sub-expressions.
     * The definition context is used for named types lookups, to avoid conflation with names of the sub-expressions.
     */
    virtual void symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext,
                                          TxDeclarationFlags declFlags, const std::string designatedTypeName,
                                          const std::vector<TxDeclarationNode*>* typeParamDeclNodes );

    virtual void symbol_resolution_pass() {
        if (typeParamDeclNodes) {
            for (auto paramDeclNode : *typeParamDeclNodes) {
                paramDeclNode->symbol_resolution_pass();
            }
        }
        this->resolve_type();
    }
};


class TxFieldDefNode;

class TxExpressionNode : public TxTypeDefiningNode {
protected:
    /** injected by field definition if known and applicable */
    const TxFieldDefNode* fieldDefNode = nullptr;

    /** injected by outer expression if applicable */
    std::vector<const TxType*>* appliedFuncArgTypes = nullptr;

public:
    TxExpressionNode(const TxLocation& parseLocation) : TxTypeDefiningNode(parseLocation) { }

    virtual TxExpressionNode* make_ast_copy() const override = 0;

    /** Injected by field definition if known and applicable. */
    virtual void set_field_def_node(const TxFieldDefNode* fieldDefNode) {
        this->fieldDefNode = fieldDefNode;
    }

    /** Returns true if this value expression is of a directly identified type
     * (i.e. does not construct a new type), e.g. value literals and directly identified fields. */
    virtual bool has_predefined_type() const = 0;

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

    virtual std::vector<const TxType*>* get_applied_func_arg_types() {
        return this->appliedFuncArgTypes;
    }
    virtual void set_applied_func_arg_types( std::vector<const TxType*>* appliedFuncArgTypes ) {
        this->appliedFuncArgTypes = appliedFuncArgTypes;
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

    virtual bool has_predefined_type() const override { return false; }

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

    virtual bool is_statically_constant() const override {
        return this->get_spec_expression()->is_statically_constant();
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        return this->get_spec_expression()->get_static_constant_proxy();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxFieldDefNode : public TxFieldDefiningNode {
    const std::string fieldName;  // the original source field name
    std::string declName = "";  // the declared field name

    const TxFieldDeclaration* declaration = nullptr;

    void symbol_declaration_pass( LexicalContext& outerContext, LexicalContext& innerContext, TxDeclarationFlags declFlags) {
        this->set_context( outerContext);
        auto typeDeclFlags = (declFlags & (TXD_PUBLIC | TXD_PROTECTED | TXD_EXPERRBLOCK)) | TXD_IMPLICIT;
        if (this->typeExpression) {
            // unless the type expression is a directly named type, declare implicit type entity for this field's type:
            std::string implTypeName = ( this->typeExpression->has_predefined_type() ? "" : this->get_decl_field_name() + "$type" );
            this->typeExpression->symbol_declaration_pass( innerContext, innerContext, typeDeclFlags, implTypeName, nullptr);
        }
        if (this->initExpression) {
// TODO: delegate this to the expression nodes
//            if (!this->typeExpression && !this->initExpression->has_predefined_type()) {
//                // declare implicit type entity for this field's type:
//                TxTypeEntity* typeEntity = lexContext.scope()->declare_type(implTypeName, this->typeExpression, typeDeclFlags);
//                if (!typeEntity)
//                    CERROR(this, "Failed to declare implicit type %s for field %s", implTypeName.c_str(), this->fieldName.c_str());
//            }
            this->initExpression->symbol_declaration_pass( outerContext);
        }
    };

protected:
    virtual const TxType* define_type() override {
        LOGGER().trace("resolving type of %s", this->to_string().c_str());
        const TxType* type;
        if (this->typeExpression) {
            type = this->typeExpression->resolve_type();
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
        if (! type)
            CERROR(this, "No type defined for field");
        return type;
    }

    virtual const TxField* define_field() override {
        LOGGER().trace("resolving field of %s", this->to_string().c_str());
        ASSERT(this->attempt_get_type(), "Expected non-NULL type in " << this);
        if (this->declaration)
            return new TxField(this->declaration, this->attempt_get_type());
        // else is not an error - function type's arguments & return type lack field declarations
        return nullptr;
    }

public:
    const bool modifiable;  // true if field name explicitly declared modifiable
    TxTypeExpressionNode* typeExpression;
    TxMaybeConversionNode* initExpression;

    TxFieldDefNode(const TxLocation& parseLocation, const std::string& fieldName,
                   TxTypeExpressionNode* typeExpression, TxExpressionNode* initExpression, bool modifiable=false)
            : TxFieldDefiningNode(parseLocation), fieldName(fieldName), modifiable(modifiable) {
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
        return new TxFieldDefNode( this->parseLocation, this->fieldName, typeExpr, initExpr, this->modifiable );
    }

    void symbol_declaration_pass_local_field( LexicalContext& lexContext, bool create_local_scope, TxDeclarationFlags declFlags=TXD_NONE ) {
        LexicalContext outerCtx(lexContext);  // prevents init expr from referring to this field
        if (create_local_scope)
            lexContext.scope(lexContext.scope()->create_code_block_scope());
        this->declName = this->fieldName;
        this->declaration = lexContext.scope()->declare_field(this->declName, this, declFlags, TXS_STACK, TxIdentifier(""));
        this->symbol_declaration_pass( outerCtx, lexContext, declFlags);
    }

    void symbol_declaration_pass_nonlocal_field( LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                                 TxFieldStorage storage, const TxIdentifier& dataspace ) {
        TxDeclarationFlags fieldFlags = declFlags;
        if (this->fieldName != "self")
            this->declName = this->fieldName;
        else {
            // handle constructor declaration
            this->declName = "$init";
            fieldFlags = fieldFlags | TXD_CONSTRUCTOR;
        }

        this->declaration = lexContext.scope()->declare_field(this->declName, this, fieldFlags, storage, dataspace);
        this->symbol_declaration_pass( lexContext, lexContext, declFlags);
    }

    void symbol_declaration_pass_functype_arg( LexicalContext& lexContext ) {
        this->symbol_declaration_pass( lexContext, lexContext, TXD_NONE);
    }

    virtual void symbol_resolution_pass() {
        this->resolve_field();
        if (this->typeExpression) {
            this->typeExpression->symbol_resolution_pass();
        }
        auto field = this->get_field();
        if (this->initExpression) {
            if (this->typeExpression && field)
                this->initExpression->insert_conversion( field->get_type() );
            this->initExpression->symbol_resolution_pass();
            if (field && field->is_statically_constant())
                    if (! this->initExpression->is_statically_constant())
                        CERROR(this, "Non-constant initializer for constant global/static field.");
        }

        if (field) {
            if (! field->get_type()->is_concrete())
                CERROR(this, "Field type is not concrete (size potentially unknown): " << field->get_type());
            if (this->get_decl_field_name() == "$init") {
                if (this->get_declaration()->get_storage() != TXS_INSTANCEMETHOD)
                    CERROR(this, "Illegal declaration name for non-constructor member: " << this->fieldName);
                // TODO: check that constructor function type has void return value
            }
        }
    }

    virtual const TxExpressionNode* get_init_expression() const {
        return this->initExpression;
    }

    /** Gets the plain name of this field as defined in the source text. */
    inline const std::string& get_source_field_name() const {
        return this->fieldName;
    }

    /** Gets the plain name of this field as actually declared in the symbol table. */
    inline const std::string& get_decl_field_name() const {
        return this->declName;
    }

    const TxFieldDeclaration* get_declaration() const {
        ASSERT(this->declaration, "field declaration not initialized for " << this->fieldName);
        return this->declaration;
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual std::string to_string() const override {
        return TxFieldDefiningNode::to_string() + " '" + this->get_source_field_name() + "'";
    }
};

/** Non-local field declaration */
class TxFieldDeclNode : public TxDeclarationNode {
    const bool isMethodSyntax = false;
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

    virtual std::string to_string() const {
        return TxDeclarationNode::to_string() + " '" + this->field->get_source_field_name() + "'";
    }
};


/** Non-local type declaration */
class TxTypeDeclNode : public TxDeclarationNode {
public:
    const std::string typeName;
    const bool interfaceKW;
    const std::vector<TxDeclarationNode*>* typeParamDecls;
    TxTypeExpressionNode* typeExpression;

    TxTypeDeclNode(const TxLocation& parseLocation, const TxDeclarationFlags declFlags,
                   const std::string typeName, const std::vector<TxDeclarationNode*>* typeParamDecls,
                   TxTypeExpressionNode* typeExpression, bool interfaceKW=false)
        : TxDeclarationNode(parseLocation, declFlags),
          typeName(typeName), interfaceKW(interfaceKW), typeParamDecls(typeParamDecls), typeExpression(typeExpression) {
        validateTypeName(this, declFlags, typeName);
    }

    virtual TxTypeDeclNode* make_ast_copy() const override {
        return new TxTypeDeclNode( this->parseLocation, this->declFlags, this->typeName,
                                   make_node_vec_copy( this->typeParamDecls ),
                                   this->typeExpression->make_ast_copy(), this->interfaceKW);
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorDecl=false ) override {
        this->symbol_declaration_pass( lexContext, lexContext, isExpErrorDecl );
    }
    virtual void symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext, bool isExpErrorDecl=false );

    virtual void symbol_resolution_pass() override {
        this->typeExpression->symbol_resolution_pass();
        if (this->typeParamDecls)
            for (auto paramDecl : *this->typeParamDecls)
                paramDecl->symbol_resolution_pass();
    }

    virtual const TxTypeDeclaration* get_declaration() const override {
        return this->typeExpression->get_declaration();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual std::string to_string() const {
        return TxDeclarationNode::to_string() + " '" + this->typeName + "'";
    }
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
    ExpectedErrorContext expErrCtx;

public:
    TxDeclarationNode* body;

    TxExpErrDeclNode(const TxLocation& parseLocation, int expected_error_count, int prev_encountered_errors, TxDeclarationNode* body)
        : TxDeclarationNode(parseLocation, (body ? body->get_decl_flags() : TXD_NONE) | TXD_EXPERRBLOCK),
          expErrCtx( expected_error_count, prev_encountered_errors, prev_encountered_errors ), body(body)  {
    }

    virtual TxExpErrDeclNode* make_ast_copy() const override {
        return new TxExpErrDeclNode( this->parseLocation, this->expErrCtx.expected_error_count, this->expErrCtx.prev_encountered_errors,
                                     this->body->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext, bool isExpErrorDecl ) override {
        this->set_context( LexicalContext( lexContext, lexContext.scope(), &expErrCtx ) );
        if (isExpErrorDecl)
            CERROR(this, "Can't next Expected Error constructs in a declaration");
        if (this->body) {
            if (! this->context().is_reinterpretation()) {
                this->get_parse_location().parserCtx->begin_exp_err(this->parseLocation);
                this->body->symbol_declaration_pass( this->context(), true );
                this->expErrCtx.encountered_error_count += this->get_parse_location().parserCtx->end_exp_err(this->parseLocation);
            }
            else
                this->body->symbol_declaration_pass( this->context(), true );
        }
    }

    virtual void symbol_resolution_pass() override {
        auto ctx = this->context();
        if (! ctx.is_reinterpretation()) {
            this->get_parse_location().parserCtx->begin_exp_err(this->parseLocation);
            if (this->body)
                this->body->symbol_resolution_pass();
            this->expErrCtx.encountered_error_count += this->get_parse_location().parserCtx->end_exp_err(this->parseLocation);
            if ( this->expErrCtx.expected_error_count <  0 ) {
                if ( this->expErrCtx.encountered_error_count == 0 )
                    CERROR(this, "COMPILER TEST FAIL: Expected one or more compilation errors but encountered "
                                 << this->expErrCtx.encountered_error_count);
            }
            else if ( this->expErrCtx.expected_error_count != this->expErrCtx.encountered_error_count )
                CERROR(this, "COMPILER TEST FAIL: Expected " << this->expErrCtx.expected_error_count
                              << " compilation errors but encountered " << this->expErrCtx.encountered_error_count);
        }
        else if (this->body)
            this->body->symbol_resolution_pass();
    }

    virtual const TxEntityDeclaration* get_declaration() const override {
        return body->get_declaration();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
};
