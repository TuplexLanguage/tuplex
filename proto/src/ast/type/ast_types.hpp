#pragma once

#include "ast/ast_entitydecls.hpp"
#include "ast/ast_entitydefs.hpp"
#include "ast/type/ast_typeexpr_node.hpp"
#include "ast/type/ast_typearg_node.hpp"
#include "ast/type/ast_argtypedef_node.hpp"
#include "ast/expr/ast_maybe_conv_node.hpp"

#include "symbol/type_registry.hpp"
#include "symbol/qual_type.hpp"

class TxIdentifiedSymbolNode : public TxNode {
    const TxIdentifier* symbolName;
    TxScopeSymbol* symbol = nullptr;

    friend class TxNamedTypeNode;

protected:
    TxScopeSymbol* resolve_symbol();

public:
    TxIdentifiedSymbolNode* baseSymbolNode;

    TxIdentifiedSymbolNode( const TxLocation& ploc, TxIdentifiedSymbolNode* baseSymbol, const std::string& name )
            : TxNode( ploc ), symbolName( new TxIdentifier( name ) ), baseSymbolNode( baseSymbol ) {
    }

    virtual TxIdentifiedSymbolNode* make_ast_copy() const override {
        return new TxIdentifiedSymbolNode( this->ploc, ( this->baseSymbolNode ? this->baseSymbolNode->make_ast_copy() : nullptr ),
                                           this->symbolName->str() );
    }

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->baseSymbolNode )
            this->baseSymbolNode->visit_ast( visitor, thisCursor, "basesym", context );
    }

    /** Returns the full identifier (dot-separated full name) as specified in the program text, up to and including this name. */
    inline TxIdentifier get_full_identifier() const {
        return ( this->baseSymbolNode ? TxIdentifier( this->baseSymbolNode->get_full_identifier(), this->symbolName->str() ) : *this->symbolName );
    }

    virtual const std::string& get_descriptor() const override {
        return this->symbolName->str();
    }
};

/** Identifies a type directly via its name. */
class TxNamedTypeNode : public TxTypeExpressionNode {
protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override;

public:
    TxIdentifiedSymbolNode* symbolNode;

    TxNamedTypeNode( const TxLocation& ploc, TxIdentifiedSymbolNode* symbolNode )
            : TxTypeExpressionNode( ploc ), symbolNode( symbolNode ) {
    }

    TxNamedTypeNode( const TxLocation& ploc, const std::string& name )
            : TxTypeExpressionNode( ploc ), symbolNode( new TxIdentifiedSymbolNode( ploc, nullptr, name ) ) {
    }

    virtual TxNamedTypeNode* make_ast_copy() const override {
        return new TxNamedTypeNode( this->ploc, symbolNode->make_ast_copy() );
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->symbolNode->visit_ast( visitor, thisCursor, "symbol", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->symbolNode->get_descriptor();
    }
};

/** Identifies a type that is a member of another type, which is determined by an arbitrary type expression. */
class TxMemberTypeNode : public TxTypeExpressionNode {
protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override;

public:
    TxTypeExpressionNode* baseTypeExpr;
    const std::string memberName;

    TxMemberTypeNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeExpr, const std::string& memberName )
            : TxTypeExpressionNode( ploc ), baseTypeExpr( baseTypeExpr ), memberName( memberName ) {
    }

    virtual TxMemberTypeNode* make_ast_copy() const override {
        return new TxMemberTypeNode( this->ploc, baseTypeExpr->make_ast_copy(), memberName );
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->baseTypeExpr->visit_ast( visitor, thisCursor, "type-expr", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->memberName;
    }
};


/** Represents a specialization of a generic type (binding one or more type parameters).
 */
class TxGenSpecTypeNode : public TxTypeCreatingNode {
protected:
    virtual TxActualType* create_type( TxPassInfo passInfo ) override;

    virtual void verification_pass() const override {
        for ( TxTypeArgumentNode* ta : *this->typeArgs ) {
            if (this->genTypeExpr->qtype()->get_type_class() != TXTC_REFERENCE) {
                if ( auto typeTypeArg = dynamic_cast<TxTypeTypeArgumentNode*>( ta ) ) {
                    auto elemType = typeTypeArg->typeExprNode->qtype();
                    if ( is_not_properly_concrete( this, elemType ) )
                        CERROR( this, "Type specialization parameter is not concrete: " << elemType );
                }
            }
        }
    }

public:
    TxTypeExpressionNode* genTypeExpr;
    const std::vector<TxTypeArgumentNode*>* const typeArgs;

    TxGenSpecTypeNode( const TxLocation& ploc, TxTypeExpressionNode* genTypeExpr, const std::vector<TxTypeArgumentNode*>* typeArgs )
            : TxTypeCreatingNode( ploc ), genTypeExpr( genTypeExpr ), typeArgs( typeArgs ) {
        ASSERT( typeArgs && !typeArgs->empty(), "NULL or empty typeargs" );
    }

    virtual TxGenSpecTypeNode* make_ast_copy() const override {
        return new TxGenSpecTypeNode( this->ploc, this->genTypeExpr->make_ast_copy(), make_node_vec_copy( this->typeArgs ) );
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->genTypeExpr->visit_ast( visitor, thisCursor, "gentype", context );
        for ( auto typeArg : *this->typeArgs )
            typeArg->visit_ast( visitor, thisCursor, "typearg", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->genTypeExpr->get_descriptor();
    }
};

/** Common superclass for specializations of the built-in types Ref and Array. */
class TxBuiltinTypeSpecNode : public TxTypeCreatingNode {
public:
    TxBuiltinTypeSpecNode( const TxLocation& ploc )
            : TxTypeCreatingNode( ploc ) {
    }
};

/**
 * Custom AST node needed to handle dataspaces. */
class TxReferenceTypeNode : public TxBuiltinTypeSpecNode {
    TxTypeExpressionNode* refBaseNode;

    TxReferenceTypeNode( const TxLocation& ploc, const TxIdentifier* dataspace, TxTypeTypeArgumentNode* targetTypeArg )
            : TxBuiltinTypeSpecNode( ploc ), refBaseNode( new TxNamedTypeNode( ploc, "tx.Ref" ) ),
              dataspace( dataspace ), targetTypeNode( targetTypeArg ) {
    }

protected:
    virtual TxActualType* create_type( TxPassInfo passInfo ) override;

public:
    const TxIdentifier* dataspace;
    TxTypeTypeArgumentNode* targetTypeNode;

    TxReferenceTypeNode( const TxLocation& ploc, const TxIdentifier* dataspace, TxTypeExpressionNode* targetType )
            : TxReferenceTypeNode( ploc, dataspace, new TxQualTypeExprNode( targetType ) ) {
    }
    TxReferenceTypeNode( const TxLocation& ploc, const TxIdentifier* dataspace, TxQualTypeExprNode* targetType )
            : TxReferenceTypeNode( ploc, dataspace, new TxTypeTypeArgumentNode( targetType ) ) {
    }

    virtual TxReferenceTypeNode* make_ast_copy() const override {
        return new TxReferenceTypeNode( this->ploc, this->dataspace, this->targetTypeNode->make_ast_copy() );
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->refBaseNode->visit_ast( visitor, thisCursor, "refbase", context );
        this->targetTypeNode->visit_ast( visitor, thisCursor, "target", context );
    }
};

/**
 * Custom AST node needed to provide syntactic sugar for modifiable declaration. */
class TxArrayTypeNode : public TxBuiltinTypeSpecNode {
    TxTypeExpressionNode* arrayBaseNode;

    TxArrayTypeNode( const TxLocation& ploc, TxTypeTypeArgumentNode* elementTypeArg, TxValueTypeArgumentNode* capacityExprArg )
            : TxBuiltinTypeSpecNode( ploc ), arrayBaseNode( new TxNamedTypeNode( ploc, "tx.Array" ) ),
              elementTypeNode( elementTypeArg ), capacityNode( capacityExprArg ) {
    }

protected:
    virtual TxActualType* create_type( TxPassInfo passInfo ) override;

    virtual void verification_pass() const override {
        auto elemType = this->elementTypeNode->typeExprNode->qtype();
        if ( is_not_properly_concrete( this, elemType ) ) {
            CERROR( this, "Array element type is not concrete: " << elemType );
        }
    }

public:
    TxTypeTypeArgumentNode* elementTypeNode;
    TxValueTypeArgumentNode* capacityNode;

    TxArrayTypeNode( const TxLocation& ploc, TxTypeExpressionNode* elementType, TxExpressionNode* capacityExpr = nullptr )
            : TxArrayTypeNode( ploc, new TxQualTypeExprNode( elementType ), capacityExpr ) {
    }
    TxArrayTypeNode( const TxLocation& ploc, TxQualTypeExprNode* elementType, TxExpressionNode* capacityExpr = nullptr )
            : TxArrayTypeNode( ploc, new TxTypeTypeArgumentNode( elementType ),
                               ( capacityExpr ? new TxValueTypeArgumentNode( new TxMaybeConversionNode( capacityExpr ) ) : nullptr ) ) {
    }

    virtual TxArrayTypeNode* make_ast_copy() const override {
        return new TxArrayTypeNode( this->ploc, this->elementTypeNode->make_ast_copy(),
                                    ( this->capacityNode ? this->capacityNode->make_ast_copy() : nullptr ) );
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->arrayBaseNode->visit_ast( visitor, thisCursor, "arraybase", context );
        this->elementTypeNode->visit_ast( visitor, thisCursor, "elementtype", context );
        if ( this->capacityNode )
            this->capacityNode->visit_ast( visitor, thisCursor, "capacity", context );
    }
};


#include "ast_typecreating_node.hpp"

/** Represents a type derivation with a body and / or implemented interfaces (a.k.a. a type extension). */
class TxDerivedTypeNode : public TxTypeCreatingNode {
    TxTypeDeclNode* superRefTypeNode = nullptr;

    TxTypeResolvingNode* builtinTypeDefiner = nullptr;

    /** Initializes certain implicit type members such as 'Super' for types with a body. */
    void init_implicit_types();

    friend class TxBuiltinTypeDefiningNode;

    void set_builtin_type_definer( TxTypeResolvingNode* builtinTypeDefiner ) {
        this->builtinTypeDefiner = builtinTypeDefiner;
    }

    void code_gen_builtin_type( LlvmGenerationContext& context ) const;

    void inner_code_gen_type( LlvmGenerationContext& context ) const;

    /** used by make_ast_copy() */
    TxDerivedTypeNode( const TxLocation& ploc, TxTypeExpressionNode* baseType,
                       std::vector<TxTypeExpressionNode*>* interfaces, std::vector<TxDeclarationNode*>* members,
                       bool interfaceKW, bool mutableType )
            : TxTypeCreatingNode( ploc ), baseTypeNode( baseType ), interfaces( interfaces ), members( members ) {
    }

protected:
    virtual void typeexpr_declaration_pass() override {
        this->init_implicit_types();  // (can't run this before interfaceKW is known)
    }

    virtual TxActualType* create_type( TxPassInfo passInfo ) override;

public:
    TxTypeExpressionNode* baseTypeNode;
    std::vector<TxTypeExpressionNode*>* interfaces;
    std::vector<TxDeclarationNode*>* members;

    TxDerivedTypeNode( const TxLocation& ploc, TxTypeExpressionNode* baseType,
                       std::vector<TxTypeExpressionNode*>* interfaces, std::vector<TxDeclarationNode*>* members )
            : TxTypeCreatingNode( ploc ), baseTypeNode( baseType ), interfaces( interfaces ), members( members ) {
    }

    TxDerivedTypeNode( const TxLocation& ploc, TxTypeExpressionNode* baseType, std::vector<TxDeclarationNode*>* members )
        : TxDerivedTypeNode(ploc, baseType, new std::vector<TxTypeExpressionNode*>(), members) { }

    TxDerivedTypeNode( const TxLocation& ploc, std::vector<TxDeclarationNode*>* members )
        : TxDerivedTypeNode(ploc, nullptr, new std::vector<TxTypeExpressionNode*>(), members) { }

    virtual void set_requires_mutable( bool mut ) override;

    virtual TxDerivedTypeNode* make_ast_copy() const override {
        return new TxDerivedTypeNode( this->ploc, this->baseTypeNode->make_ast_copy(),
                                      make_node_vec_copy( this->interfaces ), make_node_vec_copy( this->members ) );
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override;
};

/** Defines a function type. */
class TxFunctionTypeNode : public TxTypeCreatingNode {
    // Note: the field names aren't part of a function's formal type definition
    // (a function type doesn't declare (create entities for) the function args)

    TxTypeExpressionNode* baseTypeNode;

    static TxArgTypeDefNode* make_return_field( TxTypeExpressionNode* returnType ) {
        return ( returnType ? new TxArgTypeDefNode( returnType->ploc, "$return", returnType ) : nullptr );
    }

protected:
    virtual void typeexpr_declaration_pass() override;

    virtual TxActualType* create_type( TxPassInfo passInfo ) override;

    virtual void verification_pass() const override {
        for ( auto argField : *this->arguments ) {
            auto argType = argField->qtype();
            if ( is_not_properly_concrete( this, argType ) )
                CERROR( argField, "Function argument type is not concrete: " << argField->get_descriptor() << " : " << argType );
        }
        if ( this->returnField ) {
            auto retType = this->returnField->qtype();
            if ( is_not_properly_concrete( this, retType ) )
                CERROR( returnField, "Function return type is not concrete: " << retType );
        }
    }

public:
    /** Indicates whether functions of this type may modify its closure when run. */
    const bool modifying;
    std::vector<TxArgTypeDefNode*>* arguments;
    TxArgTypeDefNode* returnField;

    TxFunctionTypeNode( const TxLocation& ploc, const bool modifying,
                        std::vector<TxArgTypeDefNode*>* arguments, TxTypeExpressionNode* returnType )
            : TxTypeCreatingNode( ploc ), baseTypeNode( new TxNamedTypeNode( ploc, "tx.Function" ) ),
              modifying( modifying ), arguments( arguments ),
              returnField( make_return_field( returnType ) ) {
    }

    virtual TxFunctionTypeNode* make_ast_copy() const override {
        return new TxFunctionTypeNode( this->ploc, this->modifying, make_node_vec_copy( this->arguments ),
                                       ( this->returnField ? this->returnField->typeExpression->make_ast_copy() : nullptr ) );
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->baseTypeNode->visit_ast( visitor, thisCursor, "basetype", context );
        for ( auto argField : *this->arguments )
            argField->visit_ast( visitor, thisCursor, "arg", context );
        if ( this->returnField )
            this->returnField->visit_ast( visitor, thisCursor, "return", context );
    }
};
