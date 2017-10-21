#pragma once

#include "ast_typeexpr_node.hpp"
#include "ast_qualtypes.hpp"
#include "tx_logging.hpp"

class TxActualType;

class TxTypeCreatingNode : public TxTypeExpressionNode {
    const TxTypeDeclaration* declaration = nullptr;

protected:
    friend class TxTypeDeclNode;

    void set_declaration( const TxTypeDeclaration* declaration ) {
        this->declaration = declaration;
    }

    virtual void typeexpr_declaration_pass() override;

    virtual void type_pass() override final;

    virtual TxQualType define_type( TxPassInfo passInfo ) override;

    virtual TxActualType* create_type( TxPassInfo passInfo ) = 0;

public:
    TxTypeCreatingNode( const TxLocation& ploc ) : TxTypeExpressionNode( ploc )  { }

    virtual TxTypeCreatingNode* make_ast_copy() const override = 0;

    /** Gets the type declaration of this type-creating expression. */
    inline const TxTypeDeclaration* get_declaration() const {
        return this->declaration;
    }
};


class TxEmptyDerivedTypeNode : public TxTypeCreatingNode {
protected:
    virtual TxActualType* create_type( TxPassInfo passInfo ) override;

public:
    TxTypeExpressionNode* baseTypeNode;

    TxEmptyDerivedTypeNode( const TxLocation& ploc, TxTypeExpressionNode* baseType )
        : TxTypeCreatingNode( ploc ), baseTypeNode( baseType )  { }

    virtual void set_requires_mutable( bool mut ) override;

    virtual TxEmptyDerivedTypeNode* make_ast_copy() const override {
        return new TxEmptyDerivedTypeNode( this->ploc, this->baseTypeNode->make_ast_copy() );
    }


    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->baseTypeNode->visit_ast( visitor, thisCursor, "basetype", context );
    }
};


/** Produces a type alias, which is a type declaration that refers to a type already declared and defined elsewhere.
 * This node does not actually create a TxActualType instance.
 */
class TxAliasTypeNode : public TxTypeCreatingNode {
protected:
    virtual TxActualType* create_type( TxPassInfo passInfo ) override;

public:
    TxTypeExpressionNode* baseTypeNode;

    TxAliasTypeNode( const TxLocation& ploc, TxTypeExpressionNode* baseType )
        : TxTypeCreatingNode( ploc ), baseTypeNode( baseType )  { }

    virtual TxAliasTypeNode* make_ast_copy() const override {
        return new TxAliasTypeNode( this->ploc, this->baseTypeNode->make_ast_copy() );
    }


    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->baseTypeNode->visit_ast( visitor, thisCursor, "basetype", context );
    }
};


/** Produces a generic type parameter type (which is a qualified type).
 * (This node owns the parameter constraint type expression node and thus forwards pass processing to it.)
 */
class TxGenParamTypeNode : public TxTypeCreatingNode {
    TxTypeExpressionNode* constraintTypeNode;

protected:
    virtual TxActualType* create_type( TxPassInfo passInfo ) override;

public:
    TxGenParamTypeNode( const TxLocation& ploc, TxTypeExpressionNode* boundTypeNode )
        : TxTypeCreatingNode( ploc ), constraintTypeNode( boundTypeNode )  { }

    virtual void set_requires_mutable( bool mut ) override;

    virtual TxGenParamTypeNode* make_ast_copy() const override {
        return new TxGenParamTypeNode( this->ploc, this->constraintTypeNode->make_ast_copy() );
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override {
        this->constraintTypeNode->code_gen_type( context );
    }

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->constraintTypeNode->visit_ast( visitor, thisCursor, "constraint", context );
    }
};


/** Produces a type binding alias, which is a type declaration that refers to a qualified type
 * that binds a TYPE type parameter.
 * This node does not actually create a TxActualType instance.
 * (This node acts as a wrapper and doesn't own the binding type expression node, and thus doesn't forward pass processing to it.)
 */
class TxGenBindingAliasTypeNode : public TxTypeCreatingNode {
    TxTypeExpressionNode* boundTypeNode;

protected:
    virtual TxActualType* create_type( TxPassInfo passInfo ) override {
        THROW_LOGIC( "Should not be called" );
    }

    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        return this->boundTypeNode->resolve_type( passInfo );
    }

public:
    TxGenBindingAliasTypeNode( const TxLocation& ploc, TxTypeExpressionNode* boundTypeNode )
        : TxTypeCreatingNode( ploc ), boundTypeNode( boundTypeNode )  { }

    virtual TxGenBindingAliasTypeNode* make_ast_copy() const override {
        return new TxGenBindingAliasTypeNode( this->ploc, this->boundTypeNode );
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override {
    }

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }
};
