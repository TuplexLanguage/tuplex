#pragma once

#include "ast_expr_node.hpp"

class TxType;

/** A conversion placeholder node which can wrap a specific conversion around an expression if necessary. */
class TxMaybeConversionNode : public TxExpressionNode {
    const TxType* insertedResultType = nullptr;
    bool _explicit = false;
    TxExpressionNode* resolvedExpr;

    inline TxExpressionNode* get_expr() const {
        return this->resolvedExpr;
    }

protected:
    virtual const TxQualType* define_type() override;

public:
    TxExpressionNode* const originalExpr;

    TxMaybeConversionNode( TxExpressionNode* originalExpr )
            : TxExpressionNode( originalExpr->ploc ), resolvedExpr( originalExpr ), originalExpr( originalExpr ) {
        ASSERT( originalExpr, "NULL originalExpr" );
        ASSERT( !dynamic_cast<TxMaybeConversionNode*>( originalExpr ),
                "Can't wrap a TxMaybeConversionNode with another TxMaybeConversionNode: " << originalExpr );
    }

    virtual TxMaybeConversionNode* make_ast_copy() const override {
        return new TxMaybeConversionNode( this->originalExpr->make_ast_copy() );
    }

    /** If necessary and permitted, inserts a new conversion expression that wraps the original expression.
     * If a conversion node is created, symbol declaration pass is run on it.
     * Will cause a compilation error to be generated in the resolution pass if the types don't match
     * so that conversion isn't possible.
     * @param _explicit if true, forces conversion between types that don't permit implicit conversion
     */
    void insert_conversion( const TxType* resultType, bool _explicit = false );

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        auto expr = this->get_expr();
        expr->symbol_resolution_pass();
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->get_expr()->get_data_graph_origin_expr();
    }

    virtual bool is_stack_allocation_expression() const override {
        return this->get_expr()->is_stack_allocation_expression();
    }

    virtual TxFieldStorage get_storage() const override {
        return this->get_expr()->get_storage();
    }

    virtual bool is_statically_constant() const override {
        return this->get_expr()->is_statically_constant();
    }

    virtual llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->resolvedExpr->visit_ast( visitor, thisCursor, "convertee", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->originalExpr->get_descriptor();
    }
};
