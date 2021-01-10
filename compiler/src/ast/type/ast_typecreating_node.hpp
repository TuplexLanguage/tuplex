#pragma once

#include "ast_typeexpr_node.hpp"
#include "ast_qualtypes.hpp"
#include "tx_logging.hpp"

class TxActualType;

class TxTypeCreatingNode : public TxTypeExpressionNode {
    const TxTypeDeclaration* declaration = nullptr;

protected:
    friend class TxTypeDeclNode;

    void set_declaration( const TxTypeDeclaration* decl ) {
        this->declaration = decl;
    }

    void typeexpr_declaration_pass() override;

    void type_pass() final;

    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

    virtual const TxActualType* create_type( TxTypeResLevel typeResLevel ) = 0;

public:
    explicit TxTypeCreatingNode( const TxLocation& ploc ) : TxTypeExpressionNode( ploc )  { }

    TxTypeCreatingNode* make_ast_copy() const override = 0;

    /** Gets the type declaration of this type-creating expression. */
    inline const TxTypeDeclaration* get_declaration() const {
        return this->declaration;
    }
};


/** Produces a type alias, which is a type declaration that refers to a type already declared and defined elsewhere.
 * This node does not actually create a TxActualType instance.
 */
class TxAliasTypeNode : public TxTypeCreatingNode {
protected:
    const TxActualType* create_type( TxTypeResLevel typeResLevel ) override;

public:
    TxTypeExpressionNode* baseTypeNode;

    TxAliasTypeNode( const TxLocation& ploc, TxTypeExpressionNode* baseType )
        : TxTypeCreatingNode( ploc ), baseTypeNode( baseType )  { }

    TxAliasTypeNode* make_ast_copy() const override {
        return new TxAliasTypeNode( this->ploc, this->baseTypeNode->make_ast_copy() );
    }


    void code_gen_type( LlvmGenerationContext& context ) const override  { }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->baseTypeNode->visit_ast( visitor, cursor, "basetype", aux );
    }
};


/** Produces a generic type parameter type (which is a qualified type).
 * (This node owns the parameter constraint type expression node and thus forwards pass processing to it.)
 */
class TxGenParamTypeNode : public TxTypeCreatingNode {
    TxTypeExpressionNode* constraintTypeNode;

protected:
    const TxActualType* create_type( TxTypeResLevel typeResLevel ) override;

public:
    TxGenParamTypeNode( const TxLocation& ploc, TxTypeExpressionNode* boundTypeNode )
        : TxTypeCreatingNode( ploc ), constraintTypeNode( boundTypeNode )  { }

    void set_requires_mutable( bool mut ) override;

    TxTypeClass resolve_type_class() override;

    TxGenParamTypeNode* make_ast_copy() const override {
        return new TxGenParamTypeNode( this->ploc, this->constraintTypeNode->make_ast_copy() );
    }

    void code_gen_type( LlvmGenerationContext& context ) const override {
        this->constraintTypeNode->code_gen_type( context );
    }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->constraintTypeNode->visit_ast( visitor, cursor, "constraint", aux );
    }
};


/** Produces a type binding alias, which is a type declaration that refers to a qualified type
 * that binds a TYPE type parameter.
 * This node does not actually create a TxActualType instance.
 * (This node acts as a wrapper and doesn't own the binding type expression node, and thus doesn't forward pass processing to it.)
 */
class TxGenBindingAliasTypeNode : public TxTypeCreatingNode {
    TxTypeResolvingNode* boundTypeNode;

protected:
    TxActualType* create_type( TxTypeResLevel typeResLevel ) override {
        THROW_LOGIC( "Should not be called" );
    }

    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return this->boundTypeNode->resolve_type( typeResLevel );
    }

public:
    TxGenBindingAliasTypeNode( const TxLocation& ploc, TxTypeResolvingNode* boundTypeNode )
        : TxTypeCreatingNode( ploc ), boundTypeNode( boundTypeNode )  { }

    TxGenBindingAliasTypeNode* make_ast_copy() const override {
        return new TxGenBindingAliasTypeNode( this->ploc, this->boundTypeNode );
    }

    void code_gen_type( LlvmGenerationContext& context ) const override {
    }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
    }
};
