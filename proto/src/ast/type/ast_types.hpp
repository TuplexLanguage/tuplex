#pragma once

#include "../ast_entitydecls.hpp"
#include "ast/ast_entitydefs.hpp"
#include "ast/type/ast_typeexpr_node.hpp"
#include "ast/type/ast_typearg_node.hpp"
#include "ast/type/ast_argtypedef_node.hpp"
#include "ast/expr/ast_maybe_conv_node.hpp"

#include "symbol/type_registry.hpp"
#include "symbol/entity_type.hpp"

class TxIdentifiedSymbolNode : public TxTypeDefiningNode {
    const TxIdentifier* symbolName;
    TxScopeSymbol* symbol = nullptr;

    friend class TxNamedTypeNode;

protected:
    TxScopeSymbol* resolve_symbol();

    virtual const TxType* define_type() override;

public:
    TxIdentifiedSymbolNode* baseSymbol;

    TxIdentifiedSymbolNode( const TxLocation& parseLocation, TxIdentifiedSymbolNode* baseSymbol, const std::string& name )
            : TxTypeDefiningNode( parseLocation ), symbolName( new TxIdentifier( name ) ), baseSymbol( baseSymbol ) {
    }

    virtual TxIdentifiedSymbolNode* make_ast_copy() const override {
        return new TxIdentifiedSymbolNode( this->parseLocation, ( this->baseSymbol ? this->baseSymbol->make_ast_copy() : nullptr ),
                                           this->symbolName->str() );
    }

    virtual void symbol_resolution_pass() {
        if ( this->baseSymbol )
            this->baseSymbol->symbol_resolution_pass();
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->baseSymbol )
            this->baseSymbol->visit_ast( visitor, thisCursor, "basetype", context );
    }

    /** Returns the full identifier (dot-separated full name) as specified in the program text, up to and including this name. */
    inline TxIdentifier get_full_identifier() const {
        return ( this->baseSymbol ? TxIdentifier( this->baseSymbol->get_full_identifier(), this->symbolName->str() ) : *this->symbolName );
    }

    virtual std::string get_identifier() const override {
        return this->symbolName->str();
    }
};

/** Identifies a type directly via its name. */
class TxNamedTypeNode : public TxTypeExpressionNode {
protected:
    virtual const TxType* define_type() override;

public:
    TxIdentifiedSymbolNode* symbolNode;

    TxNamedTypeNode( const TxLocation& parseLocation, TxIdentifiedSymbolNode* symbolNode )
            : TxTypeExpressionNode( parseLocation ), symbolNode( symbolNode ) {
    }

    TxNamedTypeNode( const TxLocation& parseLocation, const std::string& name )
            : TxTypeExpressionNode( parseLocation ), symbolNode( new TxIdentifiedSymbolNode( parseLocation, nullptr, name ) ) {
    }

    virtual TxNamedTypeNode* make_ast_copy() const override {
        return new TxNamedTypeNode( this->parseLocation, symbolNode->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        this->symbolNode->symbol_resolution_pass();
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->symbolNode->visit_ast( visitor, thisCursor, "symbol", context );
    }

    virtual std::string get_identifier() const override {
        return this->symbolNode->get_identifier();
    }
};

/** Identifies a type that is a member of another type, which is determined by an arbitrary type expression. */
class TxMemberTypeNode : public TxTypeExpressionNode {
protected:
    virtual const TxType* define_type() override;

public:
    TxTypeExpressionNode* baseTypeExpr;
    const std::string memberName;

    TxMemberTypeNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseTypeExpr, const std::string& memberName )
            : TxTypeExpressionNode( parseLocation ), baseTypeExpr( baseTypeExpr ), memberName( memberName ) {
    }

    virtual TxMemberTypeNode* make_ast_copy() const override {
        return new TxMemberTypeNode( this->parseLocation, baseTypeExpr->make_ast_copy(), memberName );
    }

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        this->baseTypeExpr->symbol_resolution_pass();
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->baseTypeExpr->visit_ast( visitor, thisCursor, "type-expr", context );
        //this->memberName->visit_ast( visitor, thisCursor, "member", context );
    }

    virtual std::string get_identifier() const override {
        return this->memberName;
    }
};


/** Represents a specialization of a generic type (binding one or more type parameters).
 */
class TxGenSpecTypeNode : public TxTypeExpressionNode {
protected:
    virtual const TxType* define_type() override;

public:
    TxTypeExpressionNode* genTypeExpr;
    const std::vector<TxTypeArgumentNode*>* const typeArgs;

    TxGenSpecTypeNode( const TxLocation& parseLocation, TxTypeExpressionNode* genTypeExpr, const std::vector<TxTypeArgumentNode*>* typeArgs )
            : TxTypeExpressionNode( parseLocation ), genTypeExpr( genTypeExpr ), typeArgs( typeArgs ) {
        ASSERT( typeArgs && !typeArgs->empty(), "NULL or empty typeargs" );
    }

    virtual TxGenSpecTypeNode* make_ast_copy() const override {
        return new TxGenSpecTypeNode( this->parseLocation, this->genTypeExpr->make_ast_copy(), make_node_vec_copy( this->typeArgs ) );
    }

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        this->genTypeExpr->symbol_resolution_pass();
        for ( TxTypeArgumentNode* ta : *this->typeArgs ) {
            ta->symbol_resolution_pass();

            if (this->genTypeExpr->get_type()->get_type_class() != TXTC_REFERENCE) {
                if ( auto typeTypeArg = dynamic_cast<TxTypeTypeArgumentNode*>( ta ) ) {
                    auto elemType = typeTypeArg->typeExprNode->get_type();
                    if ( !elemType->is_concrete() ) {
                        if ( !this->context().is_generic() )
                            CERROR( this, "Type specialization parameter is not concrete: " << elemType );
                        else
                            LOG_DEBUG( this->LOGGER(), "(Not error since generic context) Type specialization parameter is not concrete: " << elemType );
                    }
                }
            }
        }
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->genTypeExpr->visit_ast( visitor, thisCursor, "gentype", context );
        for ( auto typeArg : *this->typeArgs )
            typeArg->visit_ast( visitor, thisCursor, "typearg", context );
    }

    virtual std::string get_identifier() const override {
        return this->genTypeExpr->get_identifier();
    }
};

/** Common superclass for specializations of the built-in types Ref and Array. */
class TxBuiltinTypeSpecNode : public TxTypeExpressionNode {
public:
    TxBuiltinTypeSpecNode( const TxLocation& parseLocation )
            : TxTypeExpressionNode( parseLocation ) {
    }
};

/**
 * Custom AST node needed to handle dataspaces. */
class TxReferenceTypeNode : public TxBuiltinTypeSpecNode {
    TxReferenceTypeNode( const TxLocation& parseLocation, const TxIdentifier* dataspace, TxTypeTypeArgumentNode* targetTypeArg )
            : TxBuiltinTypeSpecNode( parseLocation ), dataspace( dataspace ), targetTypeNode( targetTypeArg ) {
    }

protected:
    virtual const TxType* define_type() override {
        return this->registry().get_reference_type( this, targetTypeNode, this->dataspace );
    }

public:
    const TxIdentifier* dataspace;
    TxTypeTypeArgumentNode* targetTypeNode;

    TxReferenceTypeNode( const TxLocation& parseLocation, const TxIdentifier* dataspace, TxTypeExpressionNode* targetType )
            : TxReferenceTypeNode( parseLocation, dataspace, new TxTypeTypeArgumentNode( targetType ) ) {
    }

    virtual TxReferenceTypeNode* make_ast_copy() const override {
        return new TxReferenceTypeNode( this->parseLocation, this->dataspace, this->targetTypeNode->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxBuiltinTypeSpecNode::symbol_resolution_pass();
        this->targetTypeNode->symbol_resolution_pass();
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->targetTypeNode->visit_ast( visitor, thisCursor, "target", context );
    }
};

/**
 * Custom AST node needed to provide syntactic sugar for modifiable declaration. */
class TxArrayTypeNode : public TxBuiltinTypeSpecNode {
    TxArrayTypeNode( const TxLocation& parseLocation, TxTypeTypeArgumentNode* elementTypeArg, TxValueTypeArgumentNode* capacityExprArg )
            : TxBuiltinTypeSpecNode( parseLocation ), elementTypeNode( elementTypeArg ), capacityNode( capacityExprArg ) {
    }

protected:
    virtual const TxType* define_type() override;

public:
    TxTypeTypeArgumentNode* elementTypeNode;
    TxValueTypeArgumentNode* capacityNode;

    TxArrayTypeNode( const TxLocation& parseLocation, TxTypeExpressionNode* elementType, TxExpressionNode* capacityExpr = nullptr )
            : TxArrayTypeNode( parseLocation, new TxTypeTypeArgumentNode( elementType ),
                               ( capacityExpr ? new TxValueTypeArgumentNode( new TxMaybeConversionNode( capacityExpr ) ) : nullptr ) ) {
    }

    virtual TxArrayTypeNode* make_ast_copy() const override {
        return new TxArrayTypeNode( this->parseLocation, this->elementTypeNode->make_ast_copy(),
                                    ( this->capacityNode ? this->capacityNode->make_ast_copy() : nullptr ) );
    }

    virtual void symbol_resolution_pass() override {
        TxBuiltinTypeSpecNode::symbol_resolution_pass();
        this->elementTypeNode->symbol_resolution_pass();
        if ( this->capacityNode ) {
            this->capacityNode->symbol_resolution_pass();
        }
        auto elemType = this->elementTypeNode->typeExprNode->get_type();
        if ( !elemType->is_concrete() ) {
            if ( !this->context().is_generic() )
                CERROR( this, "Array element type is not concrete: " << elemType );
            else
                LOG_DEBUG( this->LOGGER(), "(Not error since generic context) Array element type is not concrete: " << elemType );
        }
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->elementTypeNode->visit_ast( visitor, thisCursor, "elementtype", context );
        if ( this->capacityNode )
            this->capacityNode->visit_ast( visitor, thisCursor, "capacity", context );
    }
};

/** Represents a type derivation with a body and / or implemented interfaces (a.k.a. a type extension). */
class TxDerivedTypeNode : public TxTypeExpressionNode {
    TxTypeDeclNode* superRefTypeNode = nullptr;

    TxTypeDefiningNode* builtinTypeDefiner = nullptr;

    /** Initializes certain implicit type members such as 'Super' for types with a body. */
    void init_implicit_types();

    friend class TxBuiltinTypeDefiningNode;
//    friend TxTypeDeclNode;
//    void set_decl_attributes( bool isInterface, bool mutableType ) {
//        this->interfaceKW = isInterface;
//        this->mutableType = mutableType;
//    }
    void set_builtin_type_definer( TxTypeDefiningNode* builtinTypeDefiner ) {
        this->builtinTypeDefiner = builtinTypeDefiner;
    }

    /** used by make_ast_copy() */
    TxDerivedTypeNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseType,
                       std::vector<TxTypeExpressionNode*>* interfaces, std::vector<TxDeclarationNode*>* members,
                       bool interfaceKW, bool mutableType )
            : TxTypeExpressionNode( parseLocation ), baseType( baseType ), interfaces( interfaces ), members( members ) {
//        this->interfaceKW = interfaceKW;
//        this->mutableType = mutableType;
    }

protected:
    virtual void typeexpr_declaration_pass() override {
        this->init_implicit_types();  // (can't run this before interfaceKW is known)
    }

    virtual const TxType* define_type() override;

public:
    TxTypeExpressionNode* baseType;
    std::vector<TxTypeExpressionNode*>* interfaces;
    std::vector<TxDeclarationNode*>* members;

    TxDerivedTypeNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseType,
                       std::vector<TxTypeExpressionNode*>* interfaces, std::vector<TxDeclarationNode*>* members )
            : TxTypeExpressionNode( parseLocation ), baseType( baseType ), interfaces( interfaces ), members( members ) {
    }

    TxDerivedTypeNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseType, std::vector<TxDeclarationNode*>* members )
        : TxDerivedTypeNode(parseLocation, baseType, new std::vector<TxTypeExpressionNode*>(), members) { }

    TxDerivedTypeNode( const TxLocation& parseLocation, std::vector<TxDeclarationNode*>* members )
        : TxDerivedTypeNode(parseLocation, nullptr, new std::vector<TxTypeExpressionNode*>(), members) { }

    virtual TxDerivedTypeNode* make_ast_copy() const override {
        return new TxDerivedTypeNode( this->parseLocation, this->baseType->make_ast_copy(),
                                      make_node_vec_copy( this->interfaces ), make_node_vec_copy( this->members ) );
                                      //this->interfaceKW, this->mutableType );
    }

//    inline bool is_interface() const { return this->interfaceKW; }
//
//    inline bool is_mutable() const { return this->mutableType; }

    virtual void symbol_resolution_pass() override;

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override;
};

///**
// * Custom AST node needed to resolve to a type's super type. */
//class TxSuperTypeNode : public TxTypeExpressionNode {
//protected:
//    virtual void symbol_declaration_pass_descendants( LexicalContext& lexContext ) override {
//        this->derivedTypeNode->symbol_declaration_pass( lexContext );
//    }
//
//    virtual const TxType* define_type() override {
//        if (auto dType = this->derivedTypeNode->resolve_type()) {
//            if (auto base = dType->get_semantic_base_type())
//                return base;
//            CERROR(this, "Can't refer to 'super type' of a type that has no base type: " << dType);
//        }
//        return nullptr;
//    }
//
//public:
//    TxTypeExpressionNode* derivedTypeNode;
//
//    TxSuperTypeNode(const TxLocation& parseLocation, TxTypeExpressionNode* derivedTypeNode)
//        : TxTypeExpressionNode(parseLocation), derivedTypeNode(derivedTypeNode)  { }
//
//    virtual TxSuperTypeNode* make_ast_copy() const override {
//        return new TxSuperTypeNode( this->parseLocation, this->derivedTypeNode->make_ast_copy() );
//    }
//
//    virtual void symbol_resolution_pass() override {
//        TxTypeExpressionNode::symbol_resolution_pass();
//        this->derivedTypeNode->symbol_resolution_pass();
//    }
//
//    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
//
//    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisCursor, const std::string& role, void* context ) const override {
//        this->derivedTypeNode->visit_ast( visitor, thisCursor, "derivedtype", context );
//    }
//};

/** Defines a function type. */
class TxFunctionTypeNode : public TxTypeExpressionNode {
    // Note: the field names aren't part of a function's formal type definition
    // (a function type doesn't declare (create entities for) the function args)

    static TxArgTypeDefNode* make_return_field( TxTypeExpressionNode* returnType ) {
        return ( returnType ? new TxArgTypeDefNode( returnType->parseLocation, "$return", returnType ) : nullptr );
    }

protected:
    virtual void typeexpr_declaration_pass() override;

    virtual const TxType* define_type() override;

public:
    /** Indicates whether functions of this type may modify its closure when run. */
    const bool modifying;
    std::vector<TxArgTypeDefNode*>* arguments;
    TxArgTypeDefNode* returnField;

    TxFunctionTypeNode( const TxLocation& parseLocation, const bool modifying,
                        std::vector<TxArgTypeDefNode*>* arguments, TxTypeExpressionNode* returnType )
            : TxTypeExpressionNode( parseLocation ), modifying( modifying ),
              arguments( arguments ),
              returnField( make_return_field( returnType ) ) {
    }

    virtual TxFunctionTypeNode* make_ast_copy() const override {
        return new TxFunctionTypeNode( this->parseLocation, this->modifying, make_node_vec_copy( this->arguments ),
                                       ( this->returnField ? this->returnField->typeExpression->make_ast_copy() : nullptr ) );
    }

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        for ( auto argField : *this->arguments ) {
            argField->symbol_resolution_pass();
            auto argType = argField->get_type();
            if ( !argType->is_concrete() ) {
                if ( !this->context().is_generic() )
                    CERROR( argField, "Function argument type is not concrete: "
                            << argField->get_identifier() << " : " << argType );
                else
                    LOG_DEBUG( this->LOGGER(), "(Not error since generic context) Function argument type is not concrete: "
                               << argField->get_identifier() << " : " << argType );
            }
        }
        if ( this->returnField ) {
            this->returnField->symbol_resolution_pass();
            auto retType = this->returnField->get_type();
            if ( !retType->is_concrete() ) {
                if ( !this->context().is_generic() )
                    CERROR( returnField, "Function return type is not concrete: " << retType );
                else
                    LOG_DEBUG( this->LOGGER(), "(Not error since generic context) Function return type is not concrete: " << retType );
            }
        }
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        for ( auto argField : *this->arguments )
            argField->visit_ast( visitor, thisCursor, "arg", context );
        if ( this->returnField )
            this->returnField->visit_ast( visitor, thisCursor, "return", context );
    }
};

class TxModifiableTypeNode : public TxTypeExpressionNode {
protected:
    virtual void typeexpr_declaration_pass() override;

    virtual const TxType* define_type() override {
        if ( auto bType = this->baseType->resolve_type() ) {
            if ( bType->is_modifiable() ) {
                CERROR( this, "'modifiable' specified more than once for type: " << bType );
                return bType;
            }
            else if ( !bType->is_mutable() )
                CERR_THROWRES( this, "Can't declare immutable type as modifiable: " << bType );
            return this->registry().get_modifiable_type( this->get_declaration(), bType );
        }
        return nullptr;
    }

public:
    TxTypeExpressionNode* baseType;

    TxModifiableTypeNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseType )
            : TxTypeExpressionNode( parseLocation ), baseType( baseType ) {
    }

    virtual TxModifiableTypeNode* make_ast_copy() const override {
        return new TxModifiableTypeNode( this->parseLocation, this->baseType->make_ast_copy() );
    }

    virtual bool is_modifiable() const { return true; }

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        this->baseType->symbol_resolution_pass();
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->baseType->visit_ast( visitor, thisCursor, "basetype", context );
    }
};

/** A potentially modifiable type expression, depending on syntactic sugar rules.
 * This node should not have TxModifiableTypeNode as parent, and vice versa. */
class TxMaybeModTypeNode : public TxModifiableTypeNode {
    bool isModifiable = false;

protected:
    virtual void typeexpr_declaration_pass() override;

    virtual const TxType* define_type() override {
        // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
        if ( this->isModifiable )
            return TxModifiableTypeNode::define_type();
        else
            return this->baseType->resolve_type();
    }

public:
    TxMaybeModTypeNode( const TxLocation& parseLocation, TxTypeExpressionNode* baseType )
            : TxModifiableTypeNode( parseLocation, baseType ) {
    }

    virtual TxMaybeModTypeNode* make_ast_copy() const override {
        return new TxMaybeModTypeNode( this->parseLocation, this->baseType->make_ast_copy() );
    }

    virtual bool is_modifiable() const override {
        return this->isModifiable;
    }

    void set_modifiable( bool mod ) {
        ASSERT( !this->attempt_get_type(), "Can't set modifiable after type already has been resolved in " << this );
        this->isModifiable = mod;
    }

    virtual std::string get_identifier() const override {
        return this->baseType->get_identifier();
    }
};
