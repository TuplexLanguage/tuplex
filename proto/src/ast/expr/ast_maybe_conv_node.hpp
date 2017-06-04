#pragma once

#include "ast_expr_node.hpp"

/** A conversion placeholder node which can wrap a specific conversion around an expression if necessary. */
class TxMaybeConversionNode : public TxExpressionNode {
    const TxType* insertedResultType = nullptr;
    bool _explicit = false;
    TxExpressionNode* conversionExpr = nullptr;

    inline TxExpressionNode* get_spec_expression() const {
        return ( this->conversionExpr ? this->conversionExpr : this->originalExpr );
    }

protected:
    virtual const TxType* define_type() override;

public:
    TxExpressionNode* const originalExpr;

    TxMaybeConversionNode( TxExpressionNode* originalExpr )
            : TxExpressionNode( originalExpr->parseLocation ), originalExpr( originalExpr ) {
        ASSERT( originalExpr, "NULL originalExpr" );
        ASSERT( !dynamic_cast<TxMaybeConversionNode*>( originalExpr ),
                "Can't wrap a TxMaybeConversionNode with another TxMaybeConversionNode: " << originalExpr );
    }

    virtual TxMaybeConversionNode* make_ast_copy() const override {
        return new TxMaybeConversionNode( this->originalExpr->make_ast_copy() );
    }

    /** If necessary and permitted, inserts a new conversion expression that wraps the original expression.
     * If a conversion node is created, symbol declaration pass is run on it.
     * Generates a compilation error if the types don't match and conversion is not possible.
     * @param _explicit if true, forces conversion between types that don't permit implicit conversion
     */
    void insert_conversion( const TxType* resultType, bool _explicit = false );

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        auto expr = this->get_spec_expression();
        expr->symbol_resolution_pass();
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->get_spec_expression()->get_data_graph_origin_expr();
    }

    virtual bool is_stack_allocation_expression() const override {
        return this->get_spec_expression()->is_stack_allocation_expression();
    }

    virtual bool is_statically_constant() const override {
        return this->get_spec_expression()->is_statically_constant();
    }

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->conversionExpr )
            this->conversionExpr->visit_ast( visitor, thisCursor, "convertee", context );
        else
            this->originalExpr->visit_ast( visitor, thisCursor, "unconverted", context );
    }

    virtual std::string get_identifier() const override {
        return this->originalExpr->get_identifier();
    }
};
