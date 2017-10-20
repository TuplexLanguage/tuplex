#pragma once

#include "ast/ast_node.hpp"
#include "ast/type/ast_qualtypes.hpp"
#include "ast/expr/ast_expr_node.hpp"

/** Represents a binding for a type parameter. Can be either a Type or a Value parameter binding. */
class TxTypeArgumentNode : public TxNode {
protected:
    TxTypeArgumentNode( const TxLocation& ploc )
            : TxNode( ploc ) {
    }

public:
    virtual TxTypeArgumentNode* make_ast_copy() const override = 0;

    virtual void code_gen_type( LlvmGenerationContext& context ) const = 0;
};

class TxTypeTypeArgumentNode : public TxTypeArgumentNode {
public:
    TxQualTypeExprNode* typeExprNode;

    TxTypeTypeArgumentNode( TxQualTypeExprNode* typeExprNode )
            : TxTypeArgumentNode( typeExprNode->ploc ), typeExprNode( typeExprNode ) {
    }

    virtual TxTypeTypeArgumentNode* make_ast_copy() const override {
        return new TxTypeTypeArgumentNode( this->typeExprNode->make_ast_copy() );
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->typeExprNode->visit_ast( visitor, thisCursor, "type", context );
    }
};

class TxValueTypeArgumentNode : public TxTypeArgumentNode {
public:
    TxExpressionNode* valueExprNode;

    TxValueTypeArgumentNode( TxExpressionNode* valueExprNode )
            : TxTypeArgumentNode( valueExprNode->ploc ), valueExprNode( valueExprNode ) {
    }

    virtual TxValueTypeArgumentNode* make_ast_copy() const override {
        return new TxValueTypeArgumentNode( this->valueExprNode->make_ast_copy() );
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->valueExprNode->visit_ast( visitor, thisCursor, "value", context );
    }
};
