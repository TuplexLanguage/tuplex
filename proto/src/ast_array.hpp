#pragma once

#include "ast_declbase.hpp"
#include "ast_types.hpp"

/** Represents explicit array literals in source code as well as array initializers created implicitly (e.g. for var-arg functions).
 * Note that an array literal doesn't necessarily only have literal elements; it is statically constant only if all its elements are.
 */
class TxArrayLitNode : public TxExpressionNode {
    std::vector<TxExpressionNode*> const * const origElemExprList;
    TxTypeTypeArgumentNode* elementTypeNode;
    TxExpressionNode* lengthExpr;
    bool _directArrayArg = false;
    bool _constant = false;

protected:
    virtual const TxType* define_type() override;

public:
    std::vector<TxMaybeConversionNode*> const * const elemExprList;

    /** Represents an empty array with the specified element type. */
    TxArrayLitNode( const TxLocation& parseLocation, TxTypeExpressionNode* elementTypeExpr, TxExpressionNode* lengthExpr = nullptr )
            : TxArrayLitNode( parseLocation, elementTypeExpr, new std::vector<TxExpressionNode*>(), lengthExpr ) {
    }

    /** Represents a non-empty array with the specified element type. */
    TxArrayLitNode( const TxLocation& parseLocation, TxTypeExpressionNode* elementTypeExpr, const std::vector<TxExpressionNode*>* elemExprList,
                    TxExpressionNode* lengthExpr = nullptr );

    /** Represents a non-empty array with the element type defined by the first element.
     * The provided element expression list must not be empty. */
    TxArrayLitNode( const TxLocation& parseLocation, const std::vector<TxExpressionNode*>* elemExprList );

    /** Creates an array literal node with the specified element type, with elements that are owned by another AST node.
     * The resulting array literal node may not be AST-copied. */
    TxArrayLitNode( const TxLocation& parseLocation, TxTypeExpressionNode* elementTypeExpr, const std::vector<TxMaybeConversionNode*>* elemExprList );

    /** Creates an array literal node with elements that are owned by another AST node.
     * The element type is defined by the first element.
     * The provided element expression list must not be empty.
     * The resulting array literal node may not be AST-copied. */
    TxArrayLitNode( const TxLocation& parseLocation, const std::vector<TxMaybeConversionNode*>* elemExprList );

    virtual TxArrayLitNode* make_ast_copy() const override {
        if ( !this->origElemExprList ) {
            ASSERT( false, "Can't make AST copy of a TxArrayLitNode whose elements are owned by another AST node: " << this );
            return nullptr;
        }
        return new TxArrayLitNode( this->parseLocation, make_node_vec_copy( this->origElemExprList ) );
    }

    virtual void symbol_resolution_pass() override;

    virtual bool is_stack_allocation_expression() const override {
        if ( this->_directArrayArg )
            return this->elemExprList->front()->is_stack_allocation_expression();
        // the array will be allocated on the stack if it is not statically constant
        return !this->_constant && !this->_directArrayArg;
    }

    virtual bool is_statically_constant() const override {
        return this->_constant;
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->elementTypeNode )
            this->elementTypeNode->visit_ast( visitor, thisCursor, "elem-type", context );
        if ( this->lengthExpr )
            this->lengthExpr->visit_ast( visitor, thisCursor, "length", context );
        if ( this->origElemExprList ) {
            // if this node owns the element nodes, perform pass on them:
            for ( auto elem : *this->elemExprList )
                elem->visit_ast( visitor, thisCursor, "element", context );
        }
    }
};
