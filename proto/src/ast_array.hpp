#pragma once

#include "ast_declbase.hpp"
#include "ast_types.hpp"

class TxArrayLitNode : public TxExpressionNode {
//protected:
//    /** Returns true if this array literal's context requires it to be mutable. Used by subclasses upon type creation. */
//    bool requires_mutable_type() const;
//
public:
    TxArrayLitNode( const TxLocation& parseLocation ) : TxExpressionNode( parseLocation ) { }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;
};

/** Represents filled array literals, explicitly specified in source code as well as array initializers created implicitly
 * (e.g. for var-arg functions). Filled means their length will equal capacity, all elements initialized.
 * Note that an array literal doesn't necessarily only have literal elements; it is statically constant only if all its elements are.
 */
class TxFilledArrayLitNode : public TxArrayLitNode {
    std::vector<TxExpressionNode*> const * const origElemExprList;
    TxTypeTypeArgumentNode* elementTypeNode;
    TxMaybeConversionNode* capacityExpr;
    bool _directArrayArg = false;
    bool _constant = false;

protected:
    virtual const TxType* define_type() override;

public:
    std::vector<TxMaybeConversionNode*> const * const elemExprList;

    /** Represents an empty array with the specified element type. */
    TxFilledArrayLitNode( const TxLocation& parseLocation, TxTypeExpressionNode* elementTypeExpr, TxExpressionNode* capacityExpr = nullptr )
            : TxFilledArrayLitNode( parseLocation, elementTypeExpr, new std::vector<TxExpressionNode*>(), capacityExpr ) {
    }

    /** Represents a non-empty array with the specified element type. */
    TxFilledArrayLitNode( const TxLocation& parseLocation, TxTypeExpressionNode* elementTypeExpr, const std::vector<TxExpressionNode*>* elemExprList,
                    TxExpressionNode* capacityExpr = nullptr );

    /** Represents a non-empty array with the element type defined by the first element.
     * The provided element expression list must not be empty. */
    TxFilledArrayLitNode( const TxLocation& parseLocation, const std::vector<TxExpressionNode*>* elemExprList );

    /** Creates an array literal node with the specified element type, with elements that are owned by another AST node.
     * The resulting array literal node may not be AST-copied. */
    TxFilledArrayLitNode( const TxLocation& parseLocation, TxTypeExpressionNode* elementTypeExpr, const std::vector<TxMaybeConversionNode*>* elemExprList );

    /** Creates an array literal node with elements that are owned by another AST node.
     * The element type is defined by the first element.
     * The provided element expression list must not be empty.
     * The resulting array literal node may not be AST-copied. */
    TxFilledArrayLitNode( const TxLocation& parseLocation, const std::vector<TxMaybeConversionNode*>* elemExprList );

    virtual TxFilledArrayLitNode* make_ast_copy() const override {
        if ( !this->origElemExprList ) {
            ASSERT( false, "Can't make AST copy of a TxArrayLitNode whose elements are owned by another AST node: " << this );
            return nullptr;
        }
        //return new TxArrayLitNode( this->parseLocation, make_node_vec_copy( this->origElemExprList ) );
        return new TxFilledArrayLitNode( this->parseLocation,
                                         ( this->elementTypeNode ? this->elementTypeNode->typeExprNode->make_ast_copy() : nullptr ),
                                         make_node_vec_copy( this->origElemExprList ),
                                         ( this->capacityExpr ? this->capacityExpr->originalExpr->make_ast_copy() : nullptr ) );
    }

    virtual void symbol_resolution_pass() override;

    virtual bool is_stack_allocation_expression() const override {
        if ( this->_directArrayArg )
            return this->elemExprList->front()->is_stack_allocation_expression();
        return false;
    }

    virtual bool is_statically_constant() const override {
        return this->_constant;
    }

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->elementTypeNode )
            this->elementTypeNode->visit_ast( visitor, thisCursor, "elem-type", context );
        if ( this->capacityExpr )
            this->capacityExpr->visit_ast( visitor, thisCursor, "capacity", context );
        if ( this->origElemExprList ) {
            // if this node owns the element nodes, perform pass on them:
            for ( auto elem : *this->elemExprList )
                elem->visit_ast( visitor, thisCursor, "element", context );
        }
    }
};

class TxUnfilledArrayLitNode : public TxArrayLitNode {
    TxTypeExpressionNode* arrayTypeNode;

protected:
    virtual const TxType* define_type() override;

public:
    /** Represents an unfilled array with the specified type. */
    TxUnfilledArrayLitNode( const TxLocation& parseLocation, TxTypeExpressionNode* arrayTypeExpr );

    virtual TxUnfilledArrayLitNode* make_ast_copy() const override {
        return new TxUnfilledArrayLitNode( this->parseLocation, this->arrayTypeNode->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override;

    virtual bool is_stack_allocation_expression() const override {
        return false;
    }

    virtual bool is_statically_constant() const override {
        return this->arrayTypeNode->get_type()->is_static();
    }

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->arrayTypeNode->visit_ast( visitor, thisCursor, "type", context );
    }
};

class TxUnfilledArrayCompLitNode : public TxArrayLitNode {
    TxTypeTypeArgumentNode* elementTypeNode;
    TxMaybeConversionNode* capacityExpr;

protected:
    virtual const TxType* define_type() override;

public:
    /** Represents an unfilled array with the specified type. */
    TxUnfilledArrayCompLitNode( const TxLocation& parseLocation, TxTypeExpressionNode* elemTypeExpr, TxExpressionNode* capacityExpr = nullptr );

    virtual TxUnfilledArrayCompLitNode* make_ast_copy() const override {
        return new TxUnfilledArrayCompLitNode( this->parseLocation, this->elementTypeNode->typeExprNode->make_ast_copy(),
                                               this->capacityExpr->originalExpr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override;

    virtual bool is_stack_allocation_expression() const override {
        return false;
    }

    virtual bool is_statically_constant() const override {
        return this->capacityExpr->is_statically_constant();
    }

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->elementTypeNode->visit_ast( visitor, thisCursor, "elem-type", context );
        this->capacityExpr->visit_ast( visitor, thisCursor, "capacity", context );
    }
};
