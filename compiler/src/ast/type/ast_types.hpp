#pragma once

#include "ast/ast_entitydecls.hpp"
#include "ast/ast_entitydefs.hpp"
#include "ast/ast_identifier.hpp"
#include "ast/type/ast_typeexpr_node.hpp"
#include "ast/type/ast_typearg_node.hpp"
#include "ast/type/ast_argtypedef_node.hpp"
#include "ast/expr/ast_maybe_conv_node.hpp"

#include "symbol/type_registry.hpp"
#include "symbol/qual_type.hpp"


/** Identifies a type via name or value expression. */
class TxNamedTypeNode : public TxTypeExpressionNode {
    TxExpressionNode* exprNode;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

public:

    TxNamedTypeNode( const TxLocation& ploc, TxExpressionNode* exprNode )
            : TxTypeExpressionNode( ploc ), exprNode( exprNode ) {
    }

    TxNamedTypeNode( const TxLocation& ploc, const std::string& compoundName );


    TxNamedTypeNode* make_ast_copy() const override {
        return new TxNamedTypeNode( this->ploc, exprNode->make_ast_copy() );
    }

    void code_gen_type( LlvmGenerationContext& context ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->exprNode->visit_ast( visitor, cursor, "expr", aux );
    }

    const std::string& get_descriptor() const override {
        return this->exprNode->get_descriptor();
    }
};

/** Identifies a type that is a member of another type, which is determined by an arbitrary type expression. */
class TxMemberTypeNode : public TxTypeExpressionNode {
protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

public:
    TxTypeExpressionNode* baseTypeExpr;
    TxIdentifierNode* memberName;

    TxMemberTypeNode( const TxLocation& ploc, TxTypeExpressionNode* baseTypeExpr, TxIdentifierNode* memberName )
            : TxTypeExpressionNode( ploc ), baseTypeExpr( baseTypeExpr ), memberName( memberName ) {
    }

    TxMemberTypeNode* make_ast_copy() const override {
        return new TxMemberTypeNode( this->ploc, baseTypeExpr->make_ast_copy(), memberName->make_ast_copy() );
    }

    void code_gen_type( LlvmGenerationContext& context ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->baseTypeExpr->visit_ast( visitor, cursor, "type-expr", aux );
    }

    const std::string& get_descriptor() const override {
        return this->memberName->get_descriptor();
    }
};


/** Represents a specialization of a generic type (binding one or more type parameters).
 */
class TxGenSpecTypeNode : public TxTypeCreatingNode {
protected:
    const TxActualType* create_type( TxTypeResLevel typeResLevel ) override;

    void verification_pass() const override {
        for ( TxTypeArgumentNode* ta : *this->typeArgs ) {
            if (this->genTypeExpr->qtype()->get_type_class() != TXTC_REFERENCE) {
                if ( !ta->is_value() ) {
                    if ( auto elemType = ta->type_expr_node()->attempt_qtype() )
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

    TxTypeClass resolve_type_class() override {
        return this->genTypeExpr->resolve_type_class();
    }

    TxGenSpecTypeNode* make_ast_copy() const override {
        return new TxGenSpecTypeNode( this->ploc, this->genTypeExpr->make_ast_copy(), make_node_vec_copy( this->typeArgs ) );
    }

    void code_gen_type( LlvmGenerationContext& context ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->genTypeExpr->visit_ast( visitor, cursor, "gentype", aux );
        for ( auto typeArg : *this->typeArgs )
            typeArg->visit_ast( visitor, cursor, "typearg", aux );
    }

    const std::string& get_descriptor() const override {
        return this->genTypeExpr->get_descriptor();
    }
};

/** Common superclass for specializations of the built-in types Ref and Array. */
class TxBuiltinTypeSpecNode : public TxTypeCreatingNode {
public:
    explicit TxBuiltinTypeSpecNode( const TxLocation& ploc )
            : TxTypeCreatingNode( ploc ) {
    }
};

/**
 * Custom AST node needed to handle dataspaces. */
class TxReferenceTypeNode : public TxBuiltinTypeSpecNode {
    TxTypeExpressionNode* refBaseNode;
    TxIdentifierNode* dataspace;
    TxTypeArgumentNode* targetTypeNode;

    TxReferenceTypeNode( const TxLocation& ploc, TxIdentifierNode* dataspace, TxTypeArgumentNode* targetTypeArg )
            : TxBuiltinTypeSpecNode( ploc ), refBaseNode( new TxNamedTypeNode( ploc, "tx.Ref" ) ),
              dataspace( dataspace ), targetTypeNode( targetTypeArg ) {
    }

protected:
    const TxActualType* create_type( TxTypeResLevel typeResLevel ) override;

public:

    TxReferenceTypeNode( const TxLocation& ploc, TxIdentifierNode* dataspace, TxTypeExpressionNode* targetType )
            : TxReferenceTypeNode( ploc, dataspace, new TxQualTypeExprNode( targetType ) ) {
    }
    TxReferenceTypeNode( const TxLocation& ploc, TxIdentifierNode* dataspace, TxQualTypeExprNode* targetType )
            : TxReferenceTypeNode( ploc, dataspace, new TxTypeArgumentNode( targetType ) ) {
    }

    TxTypeClass resolve_type_class() override { return TXTC_REFERENCE; }

    TxReferenceTypeNode* make_ast_copy() const override {
        return new TxReferenceTypeNode( this->ploc, this->dataspace, this->targetTypeNode->make_ast_copy() );
    }

    void code_gen_type( LlvmGenerationContext& context ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->refBaseNode->visit_ast( visitor, cursor, "refbase", aux );
        this->targetTypeNode->visit_ast( visitor, cursor, "target", aux );
    }
};

/**
 * Custom AST node needed to provide syntactic sugar for modifiable declaration. */
class TxArrayTypeNode : public TxBuiltinTypeSpecNode {
    TxTypeExpressionNode* arrayBaseNode;

    TxArrayTypeNode( const TxLocation& ploc, TxTypeArgumentNode* elementTypeArg, TxTypeArgumentNode* capacityExprArg )
            : TxBuiltinTypeSpecNode( ploc ), arrayBaseNode( new TxNamedTypeNode( ploc, "tx.Array" ) ),
              elementTypeNode( elementTypeArg ), capacityNode( capacityExprArg ) {
    }

protected:
    const TxActualType* create_type( TxTypeResLevel typeResLevel ) override;

    void verification_pass() const override {
        auto elemType = this->elementTypeNode->type_expr_node()->qtype();
        if ( is_not_properly_concrete( this, elemType ) ) {
            CERROR( this, "Array element type is not concrete: " << elemType );
        }
    }

public:
    TxTypeArgumentNode* elementTypeNode;
    TxTypeArgumentNode* capacityNode;

    TxArrayTypeNode( const TxLocation& ploc, TxTypeExpressionNode* elementType, TxExpressionNode* capacityExpr = nullptr )
            : TxArrayTypeNode( ploc, new TxQualTypeExprNode( elementType ), capacityExpr ) {
    }
    TxArrayTypeNode( const TxLocation& ploc, TxQualTypeExprNode* elementType, TxExpressionNode* capacityExpr = nullptr )
            : TxArrayTypeNode( ploc, new TxTypeArgumentNode( elementType ),
                               ( capacityExpr ? new TxTypeArgumentNode( new TxMaybeConversionNode( capacityExpr ) ) : nullptr ) ) {
    }

    TxTypeClass resolve_type_class() override { return TXTC_ARRAY; }

    TxArrayTypeNode* make_ast_copy() const override {
        return new TxArrayTypeNode( this->ploc, this->elementTypeNode->make_ast_copy(),
                                    ( this->capacityNode ? this->capacityNode->make_ast_copy() : nullptr ) );
    }

    void code_gen_type( LlvmGenerationContext& context ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->arrayBaseNode->visit_ast( visitor, cursor, "arraybase", aux );
        this->elementTypeNode->visit_ast( visitor, cursor, "elementtype", aux );
        if ( this->capacityNode )
            this->capacityNode->visit_ast( visitor, cursor, "capacity", aux );
    }
};


#include "ast_typecreating_node.hpp"

/** Represents a type derivation with a body and / or implemented interfaces (a.k.a. a type extension). */
class TxDerivedTypeNode : public TxTypeCreatingNode {
    TxTypeDeclNode* superRefTypeNode = nullptr;

    TxTypeCreatingNode* builtinTypeDefiner = nullptr;

    /** Initializes certain implicit type members such as 'Super' for types with a body. */
    void init_implicit_types();

    friend class TxBuiltinTypeDefiningNode;

    void set_builtin_type_definer( TxTypeCreatingNode* builtinTypeDef ) {
        this->builtinTypeDefiner = builtinTypeDef;
    }

    void code_gen_builtin_type( LlvmGenerationContext& context ) const;

    void inner_code_gen_type( LlvmGenerationContext& context ) const;

protected:
    void typeexpr_declaration_pass() override {
        this->init_implicit_types();  // (can't run this before interfaceKW is known)
    }

    const TxActualType* create_type( TxTypeResLevel typeResLevel ) override;

public:
    TxTypeExpressionNode* baseTypeNode;  // (set in declaration pass, if not explicitly provided)
    std::vector<TxTypeExpressionNode*>* interfaces;
    std::vector<TxDeclarationNode*>* members;

    TxDerivedTypeNode( const TxLocation& ploc, TxTypeExpressionNode* baseType,
                       std::vector<TxTypeExpressionNode*>* interfaces, std::vector<TxDeclarationNode*>* members )
            : TxTypeCreatingNode( ploc ), baseTypeNode( baseType ), interfaces( interfaces ), members( members ) {
    }

    TxDerivedTypeNode( const TxLocation& ploc, TxTypeExpressionNode* baseType, std::vector<TxDeclarationNode*>* members )
        : TxDerivedTypeNode(ploc, baseType, new std::vector<TxTypeExpressionNode*>(), members) { }

    /** Creates a 'basic' type derivation (from Tuple or Interface). */
    TxDerivedTypeNode( const TxLocation& ploc, std::vector<TxDeclarationNode*>* members )
        : TxDerivedTypeNode(ploc, nullptr, new std::vector<TxTypeExpressionNode*>(), members) { }

    /** Creates an 'empty' type derivation but with interfaces. */
    TxDerivedTypeNode( const TxLocation& ploc, TxTypeExpressionNode* baseType, std::vector<TxTypeExpressionNode*>* interfaces )
            : TxDerivedTypeNode(ploc, baseType, interfaces, new std::vector<TxDeclarationNode*>()) { }

    /** Creates an 'empty' type derivation. */
    TxDerivedTypeNode( const TxLocation& ploc, TxTypeExpressionNode* baseType )
        : TxDerivedTypeNode(ploc, baseType, new std::vector<TxTypeExpressionNode*>(), new std::vector<TxDeclarationNode*>()) { }

    void set_requires_mutable( bool mut ) override;

    TxTypeClass resolve_type_class() override;

    TxDerivedTypeNode* make_ast_copy() const override {
        return new TxDerivedTypeNode( this->ploc, this->baseTypeNode->make_ast_copy(),
                                      make_node_vec_copy( this->interfaces ), make_node_vec_copy( this->members ) );
    }

    void code_gen_type( LlvmGenerationContext& context ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override;
};

/** Defines a function type. */
class TxFunctionTypeNode : public TxTypeCreatingNode {
    // Note: the field names aren't part of a function's formal type definition
    // (a function type doesn't declare (create entities for) the function args)

    TxTypeExpressionNode* baseTypeNode;

    static TxArgTypeDefNode* make_return_field( TxTypeExpressionNode* returnType ) {
        if ( !returnType )
            return nullptr;
        return new TxArgTypeDefNode( returnType->ploc, new TxIdentifierNode( returnType->ploc, "$return"), returnType );
    }

protected:
    void typeexpr_declaration_pass() override;

    const TxActualType* create_type( TxTypeResLevel typeResLevel ) override;

    void verification_pass() const override {
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

    TxTypeClass resolve_type_class() override { return TXTC_FUNCTION; }

    TxFunctionTypeNode* make_ast_copy() const override {
        return new TxFunctionTypeNode( this->ploc, this->modifying, make_node_vec_copy( this->arguments ),
                                       ( this->returnField ? this->returnField->typeExpression->make_ast_copy() : nullptr ) );
    }

    void code_gen_type( LlvmGenerationContext& context ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->baseTypeNode->visit_ast( visitor, cursor, "basetype", aux );
        for ( auto argField : *this->arguments )
            argField->visit_ast( visitor, cursor, "arg", aux );
        if ( this->returnField )
            this->returnField->visit_ast( visitor, cursor, "return", aux );
    }
};
