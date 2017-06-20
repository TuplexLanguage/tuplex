#pragma once

#include "ast/type/ast_types.hpp"
#include "ast/ast_fielddef_node.hpp"

/** Constructs a new TxFieldDefNode based on a TxFieldTypeDefNode (the new copy is independently allocated). */
inline TxFieldDefNode* make_field_def_node( TxArgTypeDefNode* fieldTypeDef ) {
    return new TxFieldDefNode( fieldTypeDef->ploc, fieldTypeDef->fieldName, fieldTypeDef->typeExpression->make_ast_copy(), nullptr );
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
            : TxTypeExpressionNode( funcTypeNode->ploc ), funcTypeNode( funcTypeNode ),
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

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        this->funcTypeNode->symbol_resolution_pass();
        for ( auto argField : *this->arguments )
            argField->symbol_resolution_pass();
        if ( this->returnField )
            this->returnField->symbol_resolution_pass();
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->funcTypeNode->visit_ast( visitor, thisCursor, "functype", context );
        for ( auto argField : *this->arguments )
            argField->visit_ast( visitor, thisCursor, "arg", context );
        if ( this->returnField )
            this->returnField->visit_ast( visitor, thisCursor, "return", context );
    }
};
