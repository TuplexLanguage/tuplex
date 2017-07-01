#pragma once

#include "../ast_entitydecls.hpp"
#include "ast/type/ast_types.hpp"
#include "ast_assignee_node.hpp"

class TxArrayLitNode : public TxExpressionNode {
public:
    TxArrayLitNode( const TxLocation& ploc ) : TxExpressionNode( ploc ) { }

    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
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
    TxFilledArrayLitNode( const TxLocation& ploc, TxTypeExpressionNode* elementTypeExpr, TxExpressionNode* capacityExpr = nullptr )
            : TxFilledArrayLitNode( ploc, elementTypeExpr, new std::vector<TxExpressionNode*>(), capacityExpr ) {
    }

    /** Represents a non-empty array with the specified element type. */
    TxFilledArrayLitNode( const TxLocation& ploc, TxTypeExpressionNode* elementTypeExpr, const std::vector<TxExpressionNode*>* elemExprList,
                    TxExpressionNode* capacityExpr = nullptr );

    /** Represents a non-empty array with the element type defined by the first element.
     * The provided element expression list must not be empty. */
    TxFilledArrayLitNode( const TxLocation& ploc, const std::vector<TxExpressionNode*>* elemExprList );

    /** Creates an array literal node with the specified element type, with elements that are owned by another AST node.
     * The resulting array literal node may not be AST-copied. */
    TxFilledArrayLitNode( const TxLocation& ploc, TxTypeExpressionNode* elementTypeExpr, const std::vector<TxMaybeConversionNode*>* elemExprList );

    /** Creates an array literal node with elements that are owned by another AST node.
     * The element type is defined by the first element.
     * The provided element expression list must not be empty.
     * The resulting array literal node may not be AST-copied. */
    TxFilledArrayLitNode( const TxLocation& ploc, const std::vector<TxMaybeConversionNode*>* elemExprList );

    virtual TxFilledArrayLitNode* make_ast_copy() const override {
        if ( !this->origElemExprList ) {
            ASSERT( false, "Can't make AST copy of a TxArrayLitNode whose elements are owned by another AST node: " << this );
            return nullptr;
        }
        //return new TxArrayLitNode( this->ploc, make_node_vec_copy( this->origElemExprList ) );
        return new TxFilledArrayLitNode( this->ploc,
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

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

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
    TxUnfilledArrayLitNode( const TxLocation& ploc, TxTypeExpressionNode* arrayTypeExpr );

    virtual TxUnfilledArrayLitNode* make_ast_copy() const override {
        return new TxUnfilledArrayLitNode( this->ploc, this->arrayTypeNode->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override;

    virtual bool is_stack_allocation_expression() const override {
        return false;
    }

    virtual bool is_statically_constant() const override {
        return this->arrayTypeNode->get_type()->is_static();
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

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
    TxUnfilledArrayCompLitNode( const TxLocation& ploc, TxTypeExpressionNode* elemTypeExpr, TxExpressionNode* capacityExpr = nullptr );

    virtual TxUnfilledArrayCompLitNode* make_ast_copy() const override {
        return new TxUnfilledArrayCompLitNode( this->ploc, this->elementTypeNode->typeExprNode->make_ast_copy(),
                                               this->capacityExpr->originalExpr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override;

    virtual bool is_stack_allocation_expression() const override {
        return false;
    }

    virtual bool is_statically_constant() const override {
        return this->capacityExpr->is_statically_constant();
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->elementTypeNode->visit_ast( visitor, thisCursor, "elem-type", context );
        this->capacityExpr->visit_ast( visitor, thisCursor, "capacity", context );
    }
};



class TxElemDerefNode : public TxExpressionNode {
    bool unchecked;
    class TxStatementNode* panicNode = nullptr;

protected:
    virtual const TxType* define_type() override {
        this->subscript->insert_conversion( this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );

        auto opType = this->array->originalExpr->resolve_type();
        if ( opType->get_type_class() == TXTC_REFERENCE ) {
            auto targType = opType->target_type();
            if ( targType->get_type_class() == TXTC_ARRAY ) {
                this->array->insert_conversion( targType );
            }
        }
        opType = this->array->resolve_type();
        if ( opType->get_type_class() != TXTC_ARRAY )
            CERR_THROWRES( this, "Can't subscript non-array expression: " << opType );
        return opType->element_type();
    }

public:
    TxMaybeConversionNode* array;
    TxMaybeConversionNode* subscript;

    TxElemDerefNode( const TxLocation& ploc, TxExpressionNode* operand, TxExpressionNode* subscript, bool unchecked = false )
            : TxExpressionNode( ploc ), unchecked( unchecked ),
              array( new TxMaybeConversionNode( operand ) ), subscript( new TxMaybeConversionNode( subscript ) ) {
    }

    virtual TxElemDerefNode* make_ast_copy() const override {
        return new TxElemDerefNode( this->ploc, this->array->originalExpr->make_ast_copy(),
                                    this->subscript->originalExpr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override;

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->array;
    }

    virtual TxFieldStorage get_storage() const override {
        return this->array->get_storage();
    }

    virtual bool is_statically_constant() const override {
        return ( this->array->is_statically_constant() && this->subscript->is_statically_constant() );
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->array->visit_ast( visitor, thisCursor, "array", context );
        this->subscript->visit_ast( visitor, thisCursor, "subscript", context );
    }
};


class TxElemAssigneeNode : public TxAssigneeNode {
    bool unchecked;
    class TxStatementNode* panicNode = nullptr;

protected:
    virtual const TxType* define_type() override {
        this->subscript->insert_conversion( this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );

        auto opType = this->array->originalExpr->resolve_type();
        if ( opType->get_type_class() == TXTC_REFERENCE ) {
            auto targType = opType->target_type();
            if ( targType->get_type_class() == TXTC_ARRAY ) {
                this->array->insert_conversion( targType );
            }
        }
        opType = this->array->resolve_type();
        if ( opType->get_type_class() != TXTC_ARRAY )
            CERR_THROWRES( this, "Can't subscript non-array assignee expression: " << opType );
        return opType->element_type();
    }

public:
    TxMaybeConversionNode* array;
    TxMaybeConversionNode* subscript;

    TxElemAssigneeNode( const TxLocation& ploc, TxExpressionNode* array, TxExpressionNode* subscript, bool unchecked = false )
            : TxAssigneeNode( ploc ), unchecked( unchecked ),
              array( new TxMaybeConversionNode( array ) ), subscript( new TxMaybeConversionNode( subscript ) ) {
    }

    virtual TxElemAssigneeNode* make_ast_copy() const override {
        return new TxElemAssigneeNode( this->ploc, this->array->originalExpr->make_ast_copy(),
                                       this->subscript->originalExpr->make_ast_copy() );
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->array;
    }

    virtual void symbol_resolution_pass() override;

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->array->visit_ast( visitor, thisCursor, "array", context );
        this->subscript->visit_ast( visitor, thisCursor, "subscript", context );
    }
};
