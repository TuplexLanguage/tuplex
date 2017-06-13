#pragma once

#include "ast/ast_entitydefs.hpp"

class TxFieldDefNode;

class TxExpressionNode : public TxTypeDefiningNode {
protected:
    /** injected by field definition if known and applicable */
    const TxFieldDefNode* fieldDefNode = nullptr;  // TODO: remove?

    /** injected by outer expression if applicable */
    const std::vector<TxExpressionNode*>* appliedFuncArgs = nullptr;

public:
    TxExpressionNode( const TxLocation& parseLocation )
            : TxTypeDefiningNode( parseLocation ) {
    }

    virtual TxExpressionNode* make_ast_copy() const override = 0;

    /** Injected by field definition if known and applicable. */
    virtual void set_field_def_node( const TxFieldDefNode* fieldDefNode ) {
        this->fieldDefNode = fieldDefNode;
    }

    virtual void symbol_resolution_pass() {
        this->resolve_type();
    }

    /** Checks if this expression produces a modifiable type usage; this requires the whole access chain to be mutable.
     * Generates an error message if it is not and returns false.
     * Note: Transitive across the object graph via references, regardless of mutability of references' *pointer values*.
     */
    bool check_chain_mutable() const;

    /** Gets the sub-expression of this expression that determines which data graph (if any) this value is stored in. */
    virtual const TxExpressionNode* get_data_graph_origin_expr() const {
        return nullptr;
    }

    /** Returns true if this expression is a stack allocation expression,
     * i.e. its result is in newly allocated stack space, and the allocation's type is the type of this expression.
     * Note that sub-expressions may perform allocations without this expression being an allocation. */
    // TODO: review combinatorial expressions that maybe should return true if any of their sub-expressions return true
    virtual bool is_stack_allocation_expression() const {
        return false;
    }

    /** Returns true if this expression is a constant expression that can be evaluated at compile time. */
    virtual bool is_statically_constant() const {
        return false;
    }

    virtual const std::vector<TxExpressionNode*>* get_applied_func_args() const {
        return this->appliedFuncArgs;
    }
    virtual void set_applied_func_args( const std::vector<TxExpressionNode*>* appliedFuncArgs ) {
        this->appliedFuncArgs = appliedFuncArgs;
    }

    /** Generates code that produces the value of this expression.
     * If this expression is constant, the value will be of llvm::Constant type. */
    virtual llvm::Value* code_gen_expr( LlvmGenerationContext& context, GenScope* scope ) const final;

    /** Generates code that produces the value of this expression. */
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const = 0;

    /** Generates code that produces a constant value.
     * Only valid to call on nodes for which is_statically_constant() returns true. */
    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const {
        ASSERT(! this->is_statically_constant(), "code_gen_constant() not implemented though is_statically_constant() returns true for " << this );
        THROW_LOGIC( "Unsupported: code_gen_constant() for node type " << this );
    }

    /** Generates code that produces a pointer to the value of this expression. */
    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const;

    /** Generates code that produces the type id (as opposed to the value) of this expression. */
    virtual llvm::Value* code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const;
};
