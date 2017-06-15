#pragma once

#include "ast_expr_node.hpp"
#include "ast_maybe_conv_node.hpp"
#include "ast_lit.hpp"

#include "tx_operations.hpp"


class TxOperatorValueNode : public TxExpressionNode {
public:
    TxOperatorValueNode( const TxLocation& parseLocation )
            : TxExpressionNode( parseLocation ) {
    }
};

class TxBinaryOperatorNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type() override;

public:
    const TxOperation op;
    TxMaybeConversionNode* lhs;
    TxMaybeConversionNode* rhs;
    const int op_class;

    TxBinaryOperatorNode( const TxLocation& parseLocation, TxExpressionNode* lhs, const TxOperation op, TxExpressionNode* rhs )
            : TxOperatorValueNode( parseLocation ), op( op ),
              lhs( new TxMaybeConversionNode( lhs ) ), rhs( new TxMaybeConversionNode( rhs ) ), op_class( get_op_class( op ) ) {
        ASSERT( is_valid( op ), "Invalid operator value: " << (int)op );
    }

    virtual TxBinaryOperatorNode* make_ast_copy() const override {
        return new TxBinaryOperatorNode( this->parseLocation, this->lhs->originalExpr->make_ast_copy(), this->op,
                                         this->rhs->originalExpr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        lhs->symbol_resolution_pass();
        rhs->symbol_resolution_pass();
    }

    virtual bool is_statically_constant() const override {
        return this->lhs->is_statically_constant() && this->rhs->is_statically_constant();
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->lhs->visit_ast( visitor, thisCursor, "lhs", context );
        this->rhs->visit_ast( visitor, thisCursor, "rhs", context );
    }
};

class TxUnaryMinusNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type() override;

public:
    TxMaybeConversionNode* operand;
    TxUnaryMinusNode( const TxLocation& parseLocation, TxExpressionNode* operand )
            : TxOperatorValueNode( parseLocation ), operand( new TxMaybeConversionNode( operand ) ) {
        if ( auto intLit = dynamic_cast<TxIntegerLitNode*>( this->operand->originalExpr ) ) {
            intLit->set_negative();
        }
    }

    virtual TxUnaryMinusNode* make_ast_copy() const override {
        return new TxUnaryMinusNode( this->parseLocation, this->operand->originalExpr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        operand->symbol_resolution_pass();
    }

    virtual bool is_statically_constant() const override {
        return this->operand->is_statically_constant();
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->operand->visit_ast( visitor, thisCursor, "operand", context );
    }
};

class TxUnaryLogicalNotNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type() override;

public:
    TxExpressionNode* operand;
    TxUnaryLogicalNotNode( const TxLocation& parseLocation, TxExpressionNode* operand )
            : TxOperatorValueNode( parseLocation ), operand( operand ) {
    }

    virtual TxUnaryLogicalNotNode* make_ast_copy() const override {
        return new TxUnaryLogicalNotNode( this->parseLocation, this->operand->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        operand->symbol_resolution_pass();
        auto type = operand->get_type();
        // assume arithmetic, scalar negation:
        if ( !type->is_builtin( TXBT_BOOL ) )
            // should we support any auto-conversion to Bool?
            CERROR( this, "Operand of unary '!' is not of Bool type: " << (type ? type->str().c_str() : "NULL") );
    }

    virtual bool is_statically_constant() const override {
        return this->operand->is_statically_constant();
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->operand->visit_ast( visitor, thisCursor, "operand", context );
    }
};
