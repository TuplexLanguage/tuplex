#pragma once

#include "ast/ast_node.hpp"
#include "ast/type/ast_qualtypes.hpp"
#include "ast/expr/ast_expr_node.hpp"

/** Represents a binding for a type parameter. Can be either a Type or a Value parameter binding. */
class TxTypeArgumentNode : public TxNode {
    TxTypeResolvingNode* exprNode;

public:
    TxTypeArgumentNode( TxTypeResolvingNode* exprNode )
            : TxNode( exprNode->ploc) , exprNode( exprNode ) {
    }

    inline bool is_value() const {
        return this->exprNode->is_value();
    }

    TxTypeResolvingNode* type_expr_node() const {
        ASSERT( !this->is_value(), "Type argument is not TYPE: " << this );
        return this->exprNode;
    }

    TxExpressionNode* value_expr_node() const {
        ASSERT( this->is_value(), "Type argument is not VALUE: " << this );
        return static_cast<TxExpressionNode*>( this->exprNode );
    }

    virtual TxTypeArgumentNode* make_ast_copy() const override {
        return new TxTypeArgumentNode( this->exprNode->make_ast_copy() );
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->exprNode->visit_ast( visitor, thisCursor, "expr", context );
    }
};
