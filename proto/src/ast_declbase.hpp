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
    virtual void declaration_pass() override;

public:
    TxTypeExpressionNode( const TxLocation& parseLocation )
            : TxTypeDefiningNode( parseLocation ) {
    }

    virtual TxTypeExpressionNode* make_ast_copy() const override = 0;

    virtual std::string get_auto_type_name() const = 0;

    /** Gets the type declaration of this type expression, if any. */
    inline const TxTypeDeclaration* get_declaration() const {
        return this->declaration;
    }

    inline void set_declaration( const TxTypeDeclaration* declaration ) {
        this->declaration = declaration;
    }

    virtual void symbol_resolution_pass() {
        this->resolve_type();
    }
};

class TxFieldDefNode;
class TxMaybeConversionNode;

class TxExpressionNode : public TxTypeDefiningNode {
protected:
    /** injected by field definition if known and applicable */
    const TxFieldDefNode* fieldDefNode = nullptr;  // FIXME: remove

    /** injected by outer expression if applicable */
    const std::vector<TxExpressionNode*>* appliedFuncArgs = nullptr;

public:
    TxExpressionNode( const TxLocation& parseLocation )
            : TxTypeDefiningNode( parseLocation ) {
    }

    virtual TxExpressionNode* make_ast_copy() const override = 0;

    /** Injected by field definition if known and applicable. */
    virtual void set_field_def_node( const TxFieldDefNode* fieldDefNode ) {
        this->fieldDefNode = fieldDefNode;
    }

    virtual void symbol_resolution_pass() {
        this->resolve_type();
    }

    /** Checks if this expression produces a modifiable type usage; this requires the whole access chain to be mutable.
     * Generates an error message if it is not and returns false.
     * Note: Transitive across the object graph via references, regardless of mutability of references' *pointer values*.
     */
    virtual bool check_chain_mutable() const {
        // The transitive mutability rule for references is that whether the reference itself
        // is modifiable does not matter (i.e. whether it can be changed to point to another object),
        // however the container of the reference must be mutable in order for the reference target
        // to be considered mutable.
        for ( const TxExpressionNode* origin = this; origin; origin = origin->get_data_graph_origin_expr() ) {
            auto type = origin->get_type();
            if ( !( type->get_type_class() == TXTC_REFERENCE || type->is_modifiable() ) ) {
                CERROR( this, "Expression is not mutable: " << type );
                return false;
            }
        }
        return true;
    }

    /** Gets the sub-expression of this expression that determines which data graph (if any) this value is stored in. */
    virtual const TxExpressionNode* get_data_graph_origin_expr() const {
        return nullptr;
    }

    /** Returns true if this expression is a stack allocation expression,
     * i.e. its result is in newly allocated stack space, and the allocation's type is the type of this expression.
     * Note that sub-expressions may perform allocations without this expression being an allocation. */
    // TODO: review combinatorial expressions that maybe should return true if any of their sub-expressions return true
    virtual bool is_stack_allocation_expression() const {
        return false;
    }

    /** Returns true if this expression is a constant expression that can be evaluated at compile time.
     * The result is not defined before the resolution pass is run on this node. */
    virtual bool is_statically_constant() const {
        ASSERT( this->attempt_get_type(), "Can't determine whether statically constant before resolution pass: " << this );
        return false;
    }

    /** If this expression can currently be statically evaluated,
     * a TxConstantProxy representing its value is returned, otherwise nullptr.
     * In future, this should return non-null for all expressions for which is_statically_constant() returns true.
     */
    virtual const TxConstantProxy* get_static_constant_proxy() const {
        return nullptr;
    }

    virtual const std::vector<TxExpressionNode*>* get_applied_func_args() const {
        return this->appliedFuncArgs;
    }
    virtual void set_applied_func_args( const std::vector<TxExpressionNode*>* appliedFuncArgs ) {
        this->appliedFuncArgs = appliedFuncArgs;
    }

    /** Generates code that produces a pointer to the value of this expression. */
    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
        THROW_LOGIC( "Unsupported: code_gen_address() for node type " << this );
    }

    /** Generates code that produces the type id (as opposed to the value) of this expression. */
    virtual llvm::Value* code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const;
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

    TxMaybeConversionNode( TxExpressionNode* originalExpr )
            : TxExpressionNode( originalExpr->parseLocation ), originalExpr( originalExpr ) {
        ASSERT( originalExpr, "NULL originalExpr" );
        ASSERT( !dynamic_cast<TxMaybeConversionNode*>( originalExpr ),
                "Can't wrap a TxMaybeConversionNode with another TxMaybeConversionNode: " << originalExpr );
    }

    virtual TxMaybeConversionNode* make_ast_copy() const override {
        ASSERT( false, "Can't make AST copy of a TxMaybeConversionNode: " << this );
        return nullptr;
    }

    /** If necessary and permitted, inserts a new conversion expression that wraps the original expression.
     * If a conversion node is created, symbol declaration pass is run on it.
     * Generates a compilation error if the types don't match and conversion is not possible.
     * @param _explicit if true, forces conversion between types that don't permit implicit conversion
     */
    void insert_conversion( const TxType* resultType, bool _explicit = false );

    inline TxExpressionNode* get_spec_expression() const {
        return ( this->conversionExpr ? this->conversionExpr : this->originalExpr );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        auto expr = this->get_spec_expression();
        expr->symbol_resolution_pass();
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->get_spec_expression()->get_data_graph_origin_expr();
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

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->conversionExpr )
            this->conversionExpr->visit_ast( visitor, thisCursor, "convertee", context );
        else
            this->originalExpr->visit_ast( visitor, thisCursor, "unconverted", context );
    }

    virtual std::string get_identifier() const override {
        return this->originalExpr->get_identifier();
    }
};

/** Describes an arg name and type - however does not declare or define a field entity.
 * This is used for function arguments and return values, they are not distinct declarations / entities,
 * rather they are part of the function type definition.
 */
class TxArgTypeDefNode : public TxTypeDefiningNode {
protected:
    virtual const TxType* define_type() override {
        LOG_TRACE( this->LOGGER(), "defining  type  of " << this );
        return this->typeExpression->resolve_type();
    }

public:
    const std::string fieldName;
    TxTypeExpressionNode* typeExpression;

    TxArgTypeDefNode( const TxLocation& parseLocation, const std::string& fieldName, TxTypeExpressionNode* typeExpression )
            : TxTypeDefiningNode( parseLocation ), fieldName( fieldName ), typeExpression( typeExpression ) {
        ASSERT( typeExpression, "typeExpression must be specified" );
    }

    virtual TxArgTypeDefNode* make_ast_copy() const override {
        return new TxArgTypeDefNode( this->parseLocation, this->fieldName, this->typeExpression->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() {
        this->resolve_type();
        this->typeExpression->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->typeExpression->visit_ast( visitor, thisCursor, "type", context );
    }

    virtual std::string get_identifier() const override {
        return std::string( this->fieldName );
    }
};

class TxFieldDefNode : public TxFieldDefiningNode {
    const TxFieldDeclaration* declaration = nullptr;

protected:
    virtual const TxType* define_type() override {
        LOG_TRACE( this->LOGGER(), "defining  type  of " << this );
        const TxType* type;
        if ( this->typeExpression ) {
            type = this->typeExpression->resolve_type();
            // also resolve initExpression from here, which guards against recursive field value initialization:
            if ( this->initExpression ) {
                auto nonModType = ( type->is_modifiable() ? type->get_base_type() : type );  // rvalue doesn't need to be modifiable
                this->initExpression->insert_conversion( nonModType );
                this->initExpression->resolve_type();
            }
        }
        else {
            type = this->initExpression->resolve_type();
            if ( this->modifiable ) {
                if ( !type->is_modifiable() )
                    type = this->registry().get_modifiable_type( nullptr, type );
            }
            else if ( type->is_modifiable() )
                // if initialization expression is modifiable type, and modifiable not explicitly specified,
                // lose modifiable attribute (modifiability must be explicit)
                type = type->get_base_type();
        }
        return type;
    }

    virtual const TxField* define_field() override {
        LOG_TRACE( this->LOGGER(), "defining  field of " << this );
        // FUTURE: consider if EXPERR decls shouldn't get their field created
        return TxField::make_field( this->declaration, this->attempt_get_type() );
    }

public:
    const TxIdentifier* fieldName;
    const bool modifiable;  // true if field name explicitly declared modifiable
    TxTypeExpressionNode* typeExpression;
    TxMaybeConversionNode* initExpression;

    TxFieldDefNode( const TxLocation& parseLocation, const std::string& fieldName,
                    TxTypeExpressionNode* typeExpression,
                    TxExpressionNode* initExpression, bool modifiable = false )
            : TxFieldDefiningNode( parseLocation ), fieldName( new TxIdentifier( fieldName ) ), modifiable( modifiable ) {
        validateFieldName( this, fieldName );
        this->typeExpression = typeExpression;
        if ( initExpression ) {
            initExpression->set_field_def_node( this );
            this->initExpression = new TxMaybeConversionNode( initExpression );
        }
        else {
            ASSERT( typeExpression, "At least one of typeExpression and initExpression must be specified" );
            this->initExpression = nullptr;
        }
    }

    virtual TxFieldDefNode* make_ast_copy() const override {
        TxTypeExpressionNode* typeExpr = ( this->typeExpression ? this->typeExpression->make_ast_copy() : nullptr );
        TxExpressionNode* initExpr = ( this->initExpression ? this->initExpression->originalExpr->make_ast_copy() : nullptr );
        return new TxFieldDefNode( this->parseLocation, this->fieldName->str(), typeExpr, initExpr, this->modifiable );
    }

    /** Performs the declaration of the field defined by this node. To be run before declaration pass is run on this node. */
    inline void declare_field( TxScopeSymbol* scope, TxDeclarationFlags declFlags, TxFieldStorage storage ) {
        this->declare_field( this->fieldName->str(), scope, declFlags, storage );
    }

    /** Performs the declaration of the field defined by this node. To be run before declaration pass is run on this node. */
    inline void declare_field( const std::string& name, TxScopeSymbol* scope, TxDeclarationFlags declFlags, TxFieldStorage storage ) {
        this->declaration = scope->declare_field( name, this, declFlags, storage, TxIdentifier() );
    }

    virtual void symbol_resolution_pass() {
        auto field = this->resolve_field();
        if ( this->initExpression ) {
            if ( this->typeExpression ) {
                this->typeExpression->symbol_resolution_pass();
            }
            this->initExpression->symbol_resolution_pass();

            auto storage = field->get_storage();
            if ( storage == TXS_GLOBAL
                 || ( ( storage == TXS_STATIC || storage == TXS_VIRTUAL )
                      && !field->get_type()->is_modifiable() ) ) {
                // field is expected to have a statically constant initializer
                // (Note: When static initializers in types are supported, static/virtual fields' initialization may be deferred.)
                if ( !this->initExpression->is_statically_constant() )
                    CERROR( this, "Non-constant initializer for constant global/static/virtual field" << this->fieldName );
            }
        }
        else {  // if initExpression is null then typeExpression is set
            this->typeExpression->symbol_resolution_pass();
        }

        if ( !field->get_type()->is_concrete() ) {
            if ( !this->context().is_generic() )
                CERROR( this, "Field type is not a concrete type (size potentially unknown): "
                        << this->get_identifier() << " : " << field->get_type() );
            else
                LOG_DEBUG( this->LOGGER(), "(Not error since generic context) Field type is not a concrete type (size potentially unknown): "
                           << this->get_identifier() << " : " << field->get_type() );
        }
        if ( this->get_declaration()->get_decl_flags() & TXD_CONSTRUCTOR ) {
            // TODO: check that constructor function type has void return value
        }
    }

    virtual const TxExpressionNode* get_init_expression() const {
        return this->initExpression;
    }

    const TxFieldDeclaration* get_declaration() const {
        ASSERT( this->declaration, "field declaration not initialized for " << this->fieldName );
        return this->declaration;
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->typeExpression )
            this->typeExpression->visit_ast( visitor, thisCursor, "type", context );
        if ( this->initExpression )
            this->initExpression->visit_ast( visitor, thisCursor, "initializer", context );
    }

    virtual std::string get_identifier() const override {
        return this->fieldName->str();
    }
};

/** Non-local field declaration */
class TxFieldDeclNode : public TxDeclarationNode {
    const bool isMethodSyntax = false;

protected:
    virtual void declaration_pass() override;

public:
    TxFieldDefNode* field;

    TxFieldDeclNode( const TxLocation& parseLocation, const TxDeclarationFlags declFlags, TxFieldDefNode* field,
                     bool isMethodSyntax = false )
            : TxDeclarationNode( parseLocation, declFlags ), isMethodSyntax( isMethodSyntax ), field( field ) {
    }

    virtual TxFieldDeclNode* make_ast_copy() const override {
        return new TxFieldDeclNode( this->parseLocation, this->get_decl_flags(), this->field->make_ast_copy(), this->isMethodSyntax );
    }

    virtual void symbol_resolution_pass() override;

    virtual const TxFieldDeclaration* get_declaration() const override {
        return this->field->get_declaration();
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->field->visit_ast( visitor, thisCursor, "field", context );
    }
};

/** Both non-local and local type declarations */
class TxTypeDeclNode : public TxDeclarationNode {
    /** if true, this node's subtree is merged with a built-in type definition */
    bool _builtinCode = false;

protected:
    virtual void declaration_pass() override;

public:
    const TxIdentifier* typeName;
    const bool interfaceKW;
    const bool mutableType;
    const std::vector<TxDeclarationNode*>* typeParamDecls;
    TxTypeExpressionNode* typeExpression;

    TxTypeDeclNode( const TxLocation& parseLocation, const TxDeclarationFlags declFlags, const std::string& typeName,
                    const std::vector<TxDeclarationNode*>* typeParamDecls, TxTypeExpressionNode* typeExpression,
                    bool interfaceKW = false, bool mutableType = false )
            : TxDeclarationNode( parseLocation, declFlags ), typeName( new TxIdentifier( typeName ) ),
              interfaceKW( interfaceKW ), mutableType( mutableType ), typeParamDecls( typeParamDecls ), typeExpression( typeExpression ) {
        validateTypeName( this, declFlags, typeName );
    }

    virtual TxTypeDeclNode* make_ast_copy() const override {
        return new TxTypeDeclNode( this->parseLocation, this->get_decl_flags(), this->typeName->str(),
                                   make_node_vec_copy( this->typeParamDecls ), this->typeExpression->make_ast_copy(),
                                   this->interfaceKW, this->mutableType );
    }

    virtual void symbol_resolution_pass() override {
        if ( this->_builtinCode ) {
            // the definer has been merged with the built-in type
            return;
        }
        if ( this->typeParamDecls )
            for ( auto paramDecl : *this->typeParamDecls )
                paramDecl->symbol_resolution_pass();
        try {
            this->typeExpression->symbol_resolution_pass();
        }
        catch ( const resolution_error& err ) {
            LOG( this->LOGGER(), DEBUG, "Caught resolution error in " << this->typeExpression << ": " << err );
            return;
        }
        if (this->interfaceKW) {
            if (this->typeExpression->get_type()->get_type_class() != TXTC_INTERFACE)
                CERROR(this, "Interface type cannot derive from non-interface type " << this->typeExpression->get_type());
        }
        else {
            if (this->typeExpression->get_type()->get_type_class() == TXTC_INTERFACE)
                if ( !( this->get_decl_flags() & ( TXD_GENPARAM | TXD_GENBINDING | TXD_IMPLICIT ) )
                     && !this->typeExpression->get_type()->is_modifiable() )
                    CWARNING(this, "Interface type not declared with 'interface' keyword: " << this->typeExpression->get_type());
        }
    }

    virtual const TxTypeDeclaration* get_declaration() const override {
        return this->typeExpression->get_declaration();
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( !this->_builtinCode && this->typeParamDecls )
            for ( auto decl : *this->typeParamDecls )
                decl->visit_ast( visitor, thisCursor, "type-param", context );
        this->typeExpression->visit_ast( visitor, thisCursor, "type", context );
    }

    virtual std::string get_identifier() const override {
        return this->typeName->str();
    }
};

class TxAssigneeNode : public TxTypeDefiningNode {
public:
    TxAssigneeNode( const TxLocation& parseLocation )
            : TxTypeDefiningNode( parseLocation ) {
    }

    virtual TxAssigneeNode* make_ast_copy() const override = 0;

    bool is_mutable() const {
        if ( !this->get_type()->is_modifiable() ) {
            CERROR( this, "Assignee is not mutable: " << this->get_type() );
            return false;
        }
        if ( auto origin = this->get_data_graph_origin_expr() )
            return origin->check_chain_mutable();
        return true;
    }

    /** Gets the sub-expression of this expression that determines which data graph (if any) this value is stored in. */
    virtual const TxExpressionNode* get_data_graph_origin_expr() const  = 0;

    virtual void symbol_resolution_pass() {
        this->resolve_type();
    }
};

class TxExpErrDeclNode : public TxDeclarationNode {
    ExpectedErrorClause* expError;

protected:
    virtual void declaration_pass() override {
        this->lexContext.expErrCtx = this->expError;
        if ( this->body ) {
            if ( !this->context().is_reinterpretation() ) {
                this->get_parse_location().parserCtx->register_exp_err_node( this );
            }
        }
    }

public:
    TxDeclarationNode* body;

    TxExpErrDeclNode( const TxLocation& parseLocation, ExpectedErrorClause* expError, TxDeclarationNode* body )
            : TxDeclarationNode( parseLocation, ( body ? body->get_decl_flags() : TXD_NONE ) | TXD_EXPERRBLOCK ),
              expError( expError ), body( body ) {
        if ( body ) {
            body->declFlags |= TXD_EXPERRBLOCK;
            if ( dynamic_cast<const TxExpErrDeclNode*>( body ) )
                CERROR( this, "Can't nest Expected Error constructs in a declaration" );
        }
    }

    virtual TxExpErrDeclNode* make_ast_copy() const override {
        return new TxExpErrDeclNode( this->parseLocation, nullptr, ( this->body ? this->body->make_ast_copy() : nullptr ) );
    }

    virtual void symbol_resolution_pass() override {
        if ( this->body ) {
            ScopedExpErrClause scopedEEClause( this );
            this->body->symbol_resolution_pass();
        }
    }

    virtual const TxEntityDeclaration* get_declaration() const override {
        return ( this->body ? this->body->get_declaration() : nullptr );
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override {
        return nullptr;
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->body ) {
            ScopedExpErrClause scopedEEClause( this );
            this->body->visit_ast( visitor, thisCursor, "decl", context );
        }
    }
};
