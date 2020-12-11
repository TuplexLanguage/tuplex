#pragma once

#include "ast_expr_node.hpp"


/** A conversion placeholder node which can wrap a specific conversion around an expression if necessary. */
class TxMaybeConversionNode : public TxExpressionNode {
    TxQualType insertedResultType;
    bool _explicit = false;
    TxExpressionNode* resolvedExpr;

    inline TxExpressionNode* get_expr() const {
        return this->resolvedExpr;
    }

protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override;

    virtual void resolution_pass() override {
        // do nothing since shall not resolve before parent can insert conversion
    }

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
    void insert_conversion( TxPassInfo passInfo, const TxActualType* resultType, bool _explicit = false );

    /** Like insert_conversion, but can specify required mutability. */
    void insert_qual_conversion( TxPassInfo passInfo, TxQualType resultType, bool _explicit = false );

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->get_expr()->get_data_graph_origin_expr();
    }

    virtual TxFieldStorage get_storage() const override {
        return this->get_expr()->get_storage();
    }

    virtual bool is_statically_constant() const override {
        return this->get_expr()->is_statically_constant();
    }

    virtual llvm::Constant* code_gen_typeid( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->resolvedExpr->visit_ast( visitor, thisCursor, "convertee", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->originalExpr->get_descriptor();
    }
};
