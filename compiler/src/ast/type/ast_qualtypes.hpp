#pragma once

#include "ast_typeexpr_node.hpp"
#include "symbol/qual_type.hpp"
#include "tx_logging.hpp"
#include "tx_error.hpp"


/** Represents a qualified type, i.e. a type with potentially added qualifiers.
 * This base class defines a qualified type with the same qualifiers as the wrapped type expression. */
class TxQualTypeExprNode : public TxTypeExpressionNode {
protected:
    TxTypeExpressionNode* _typeNode;

    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return _typeNode->resolve_type( typeResLevel );
    }

    void verification_pass() const override;

public:
    explicit TxQualTypeExprNode( TxTypeExpressionNode* typeNode )
            : TxQualTypeExprNode( typeNode->ploc, typeNode ) {
    }
    TxQualTypeExprNode( const TxLocation& ploc, TxTypeExpressionNode* typeNode )
            : TxTypeExpressionNode( ploc ), _typeNode( typeNode ) {
    }

    TxTypeClass resolve_type_class() override { return this->_typeNode->resolve_type_class(); }

    TxQualTypeExprNode* make_ast_copy() const override {
        return new TxQualTypeExprNode( ploc, _typeNode->make_ast_copy() );
    }

    TxTypeExpressionNode* get_type_expr() const {
        return _typeNode;
    }

    void code_gen_type( LlvmGenerationContext& context ) const override {
        this->_typeNode->code_gen_type( context );
    }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->_typeNode->visit_ast( visitor, cursor, "type", aux );
    }

    const std::string& get_descriptor() const override {
        return this->_typeNode->get_descriptor();
    }
};

/** A programmatically specified qualified type expression. */
class TxSetQualTypeExprNode : public TxQualTypeExprNode {
    bool _modifiable;

    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

public:
    TxSetQualTypeExprNode( const TxLocation& ploc, TxTypeExpressionNode* baseType, bool mod )
            : TxQualTypeExprNode( ploc, baseType ), _modifiable( mod ) {
        this->_typeNode->set_requires_mutable( mod );
    }

    TxSetQualTypeExprNode* make_ast_copy() const override {
        return new TxSetQualTypeExprNode( this->ploc, this->_typeNode->make_ast_copy(), _modifiable );
    }

    bool is_modifiable() const {
        return this->_modifiable;
    }

    [[maybe_unused]] void set_modifiable( bool mod ) {
        ASSERT( !this->attempt_qtype(), "Can't set modifiable after type already has been resolved in " << this );
        this->_modifiable = mod;
        this->_typeNode->set_requires_mutable( mod );
    }
};

/** Automatically makes the qualified type modifiable if the actual type is mutable.
 * Used to automatically make constructors modifying (i.e. syntactic sugar). */
class TxFlexModTypeExprNode : public TxQualTypeExprNode {
    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

public:
    TxFlexModTypeExprNode( const TxLocation& ploc, TxTypeExpressionNode* baseType )
            : TxQualTypeExprNode( ploc, baseType ) {
    }

    TxFlexModTypeExprNode* make_ast_copy() const override {
        return new TxFlexModTypeExprNode( this->ploc, this->_typeNode->make_ast_copy() );
    }
};

class TxModifiableTypeNode : public TxQualTypeExprNode {
protected:
    TxModifiableTypeNode( const TxLocation& ploc, TxTypeExpressionNode* typeNode, bool reqMut )
            : TxQualTypeExprNode( ploc, typeNode ) {
        this->_typeNode->set_requires_mutable( reqMut );
    }

    void typeexpr_declaration_pass() override;

    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

public:
    TxModifiableTypeNode( const TxLocation& ploc, TxTypeExpressionNode* typeNode )
            : TxModifiableTypeNode( ploc, typeNode, true ) {
    }

    TxModifiableTypeNode* make_ast_copy() const override {
        return new TxModifiableTypeNode( this->ploc, this->_typeNode->make_ast_copy() );
    }

    virtual bool is_modifiable() const { return true; }
};

/** A potentially modifiable type expression, depending on syntactic sugar rules.
 * One aim is to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
 * This node should not have TxModifiableTypeNode as parent node, and vice versa. */
class TxMaybeModTypeNode : public TxModifiableTypeNode {
    bool _modifiable;

protected:
    void typeexpr_declaration_pass() override;

public:
    TxMaybeModTypeNode( const TxLocation& ploc, TxTypeExpressionNode* baseType )
            : TxModifiableTypeNode( ploc, baseType, false ), _modifiable() {
    }

    TxMaybeModTypeNode* make_ast_copy() const override {
        return new TxMaybeModTypeNode( this->ploc, this->_typeNode->make_ast_copy() );
    }

    bool is_modifiable() const override {
        return this->_modifiable;
    }

    void set_modifiable( bool mod ) {
        ASSERT( !this->attempt_qtype(), "Can't set modifiable after type already has been resolved in " << this );
        this->_modifiable = mod;
        this->_typeNode->set_requires_mutable( mod );
    }
};

/** Removes the 'modifiable' modifier on a type. Should only be relevant in combination with TYPE type parameters. */
class TxConstTypeNode : public TxQualTypeExprNode {
protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return TxQualType( _typeNode->resolve_type( typeResLevel ).type(), false );
    }

public:
    TxConstTypeNode( const TxLocation& ploc, TxTypeExpressionNode* typeNode )
            : TxQualTypeExprNode( ploc, typeNode ) {
    }

//    void set_interface( bool ifkw ) override {
//        TxTypeExpressionNode::set_interface( ifkw );
//        this->typeNode->set_interface( ifkw );
//    }
//
//    void set_requires_mutable( bool mut ) override {
//        // Note, will not compile for this type expression
//        TxTypeExpressionNode::set_requires_mutable( mut );
//        this->typeNode->set_requires_mutable( mut );
//    }

    TxConstTypeNode* make_ast_copy() const override {
        return new TxConstTypeNode( this->ploc, this->_typeNode->make_ast_copy() );
    }
};
