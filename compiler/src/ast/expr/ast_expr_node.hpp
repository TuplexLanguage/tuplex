#pragma once

#include "ast/ast_entitydefs.hpp"

class TxFieldDefiningNode;

/** Represents a value expression. A value also has a type. */
class TxExpressionNode : public TxTypeResolvingNode {
protected:
    /** injected by field definition if known and applicable */
    const TxFieldDefiningNode* fieldDefNode = nullptr;  // TODO: remove?

    /** injected by outer expression if applicable */
    const std::vector<TxExpressionNode*>* appliedFuncArgs = nullptr;

//    virtual void resolution_pass() override {
//        this->resolve_type( typeResLevel );
//    }

public:
    TxExpressionNode( const TxLocation& ploc )
            : TxTypeResolvingNode( ploc ) {
    }

    virtual TxExpressionNode* make_ast_copy() const override = 0;

    /** Injected by field definition if known and applicable. */
    virtual void set_field_def_node( const TxFieldDefiningNode* fieldDefNode ) {
        this->fieldDefNode = fieldDefNode;
    }

    const TxFieldDefiningNode* get_field_def_node() const {
        return this->fieldDefNode;
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

    virtual bool is_value() const override {
        return true;
    }

    /** Gets the storage form of the value of this expression.
     * Returns TXS_NOSTORAGE if its value has no known memory storage (i.e. an "rvalue").
     */
    virtual TxFieldStorage get_storage() const {
        return TXS_NOSTORAGE;
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

    /** Returns the constructed type, if this expression resolves to a constructor/initializer invocation, otherwise null. */
    virtual const TxActualType* get_constructed_type( TxTypeResLevel typeResLevel ) const { return nullptr; }


    /** Generates code that produces the type id (as opposed to the value) of this expression.
     * This typically returns a Constant value unless the expression contains reference dereferencing. */
    virtual llvm::Value* code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const;

    /** Generates code that produces the type id (as opposed to the value) of this expression. */
    virtual llvm::Constant* code_gen_typeid( LlvmGenerationContext& context ) const;


    /** Generates code that produces the value of this expression.
     * If this expression is constant, the value will be of llvm::Constant type. */
    virtual llvm::Value* code_gen_expr( LlvmGenerationContext& context, GenScope* scope ) const final;

    /** Generates code that produces a pointer to the value of this expression. */
    virtual llvm::Value* code_gen_addr( LlvmGenerationContext& context, GenScope* scope ) const final;


    /** Generates code that produces the value of this expression. */
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const = 0;

    /** Generates code that produces a pointer to the value of this expression. */
    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const;


    /** Generates code that produces a constant value.
     * Only valid to call on nodes for which is_statically_constant() returns true. */
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const;

    /** Generates code that produces a constant pointer to the constant value of this expression.
     * Only valid to call on nodes for which is_statically_constant() returns true. */
    virtual llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const;
};


/** Value expression that may produce a new type specialization, e.g. array literals and reference creation. */
class TxTypeDefiningValExprNode : public TxExpressionNode {
protected:
    // disabled, since types defined by value expressions will only be certain specializations
    // (e.g. refs, arrays, ranges, strings) which are fine to define during resolution pass,
    // and resolving these before resolution pass can cause inherited symbol resolution to fail.
    //virtual void type_pass() override final;

public:
    TxTypeDefiningValExprNode( const TxLocation& ploc ) : TxExpressionNode( ploc )  { }

    virtual TxTypeDefiningValExprNode* make_ast_copy() const override = 0;
};
