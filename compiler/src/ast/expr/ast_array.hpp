#pragma once

#include "ast/ast_entitydecls.hpp"
#include "ast/type/ast_types.hpp"
#include "ast_assignee_node.hpp"

class TxArrayLitNode : public TxTypeDefiningValExprNode {
public:
    TxArrayLitNode( const TxLocation& ploc ) : TxTypeDefiningValExprNode( ploc ) { }

    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
};

/** Represents filled array literals, explicitly specified in source code as well as array initializers created implicitly
 * (e.g. for var-arg functions). Filled means their length will equal capacity, all elements initialized.
 * Note that an array literal doesn't necessarily only have literal elements; it is statically constant only if all its elements are.
 */
class TxFilledArrayLitNode : public TxArrayLitNode {
    std::vector<TxExpressionNode*> const * const origElemExprList;
    TxTypeArgumentNode* elementTypeNode;
    TxMaybeConversionNode* capacityExpr;
    bool _directArrayArg = false;
    bool _constant = false;

protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override;

    virtual void resolution_pass() override;

    virtual void verification_pass() const override;

public:
    std::vector<TxMaybeConversionNode*> const * const elemExprList;

    /** Represents an empty array with the specified element type. */
    TxFilledArrayLitNode( const TxLocation& ploc, TxQualTypeExprNode* elementTypeExpr, TxExpressionNode* capacityExpr = nullptr )
            : TxFilledArrayLitNode( ploc, elementTypeExpr, new std::vector<TxExpressionNode*>(), capacityExpr ) {
    }

    /** Represents a non-empty array with the specified element type. */
    TxFilledArrayLitNode( const TxLocation& ploc, TxQualTypeExprNode* elementTypeExpr, const std::vector<TxExpressionNode*>* elemExprList,
                    TxExpressionNode* capacityExpr = nullptr );

    /** Represents a non-empty array with the element type defined by the first element.
     * The provided element expression list must not be empty. */
    TxFilledArrayLitNode( const TxLocation& ploc, const std::vector<TxExpressionNode*>* elemExprList );

    /** Creates an array literal node with the specified element type, with elements that are owned by another AST node.
     * The resulting array literal node may not be AST-copied. */
    TxFilledArrayLitNode( const TxLocation& ploc, TxQualTypeExprNode* elementTypeExpr, const std::vector<TxMaybeConversionNode*>* elemExprList );


    virtual TxFilledArrayLitNode* make_ast_copy() const override {
        if ( !this->origElemExprList ) {
            ASSERT( false, "Can't make AST copy of a TxArrayLitNode whose elements are owned by another AST node: " << this );
            return nullptr;
        }
        return new TxFilledArrayLitNode( this->ploc,
                                         ( this->elementTypeNode ? static_cast<TxQualTypeExprNode*>(this->elementTypeNode->type_expr_node()->make_ast_copy()) : nullptr ),
                                         make_node_vec_copy( this->origElemExprList ),
                                         ( this->capacityExpr ? this->capacityExpr->originalExpr->make_ast_copy() : nullptr ) );
    }

    virtual TxFieldStorage get_storage() const override {
        if ( this->_directArrayArg )
            return this->elemExprList->front()->get_storage();
        return TXS_NOSTORAGE;
    }

    virtual bool is_statically_constant() const override {
        return this->_constant;
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
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
    virtual TxQualType define_type( TxPassInfo passInfo ) override;

public:
    /** Represents an unfilled array with the specified type. */
    TxUnfilledArrayLitNode( const TxLocation& ploc, TxTypeExpressionNode* arrayTypeExpr );

    virtual TxUnfilledArrayLitNode* make_ast_copy() const override {
        return new TxUnfilledArrayLitNode( this->ploc, this->arrayTypeNode->make_ast_copy() );
    }

    virtual TxFieldStorage get_storage() const override {
        return TXS_NOSTORAGE;
    }

    virtual bool is_statically_constant() const override {
        return this->arrayTypeNode->qtype()->is_static();
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->arrayTypeNode->visit_ast( visitor, thisCursor, "type", context );
    }
};

class TxUnfilledArrayCompLitNode : public TxArrayLitNode {
    TxTypeArgumentNode* elementTypeNode;
    TxMaybeConversionNode* capacityExpr;

protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override;

public:
    /** Represents an unfilled array with the specified type. */
    TxUnfilledArrayCompLitNode( const TxLocation& ploc, TxQualTypeExprNode* elemTypeExpr, TxExpressionNode* capacityExpr = nullptr );

    virtual TxUnfilledArrayCompLitNode* make_ast_copy() const override {
        return new TxUnfilledArrayCompLitNode( this->ploc,
                                               static_cast<TxQualTypeExprNode*>(this->elementTypeNode->type_expr_node()->make_ast_copy()),
                                               this->capacityExpr->originalExpr->make_ast_copy() );
    }

    virtual TxFieldStorage get_storage() const override {
        return TXS_NOSTORAGE;
    }

    virtual bool is_statically_constant() const override {
        return this->capacityExpr->is_statically_constant();
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->elementTypeNode->visit_ast( visitor, thisCursor, "elem-type", context );
        this->capacityExpr->visit_ast( visitor, thisCursor, "capacity", context );
    }
};



// TODO: Support negative array indexing.
class TxElemDerefNode : public TxExpressionNode {
    /** true if this is part of a nested array element assignee expression */
    bool _elemAssignment = false;

protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        this->subscript->insert_conversion( passInfo, this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );

        auto opType = this->array->originalExpr->resolve_type( passInfo );
        if ( opType->get_type_class() == TXTC_REFERENCE ) {
            auto targType = opType->target_type();
            if ( targType->get_type_class() == TXTC_ARRAY ) {
                this->array->insert_qual_conversion( passInfo, targType );
            }
        }
        opType = this->array->resolve_type( passInfo );
        if ( opType->get_type_class() != TXTC_ARRAY )
            CERR_THROWRES( this, "Can't subscript non-array expression: " << opType );
        return opType->element_type();
    }

public:
    TxMaybeConversionNode* array;
    TxMaybeConversionNode* subscript;

    TxElemDerefNode( const TxLocation& ploc, TxExpressionNode* operand, TxExpressionNode* subscript );

    virtual TxElemDerefNode* make_ast_copy() const override {
        return new TxElemDerefNode( this->ploc, this->array->originalExpr->make_ast_copy(),
                                    this->subscript->originalExpr->make_ast_copy() );
    }

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

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->array->visit_ast( visitor, thisCursor, "array", context );
        this->subscript->visit_ast( visitor, thisCursor, "subscript", context );
    }

    void set_elem_assignee_expr();
};


// TODO: Support negative array indexing.
class TxElemAssigneeNode : public TxAssigneeNode {
protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override;

public:
    TxMaybeConversionNode* array;
    TxMaybeConversionNode* subscript;

    TxElemAssigneeNode( const TxLocation& ploc, TxExpressionNode* array, TxExpressionNode* subscript );

    virtual TxElemAssigneeNode* make_ast_copy() const override {
        return new TxElemAssigneeNode( this->ploc, this->array->originalExpr->make_ast_copy(),
                                       this->subscript->originalExpr->make_ast_copy() );
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->array;
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->array->visit_ast( visitor, thisCursor, "array", context );
        this->subscript->visit_ast( visitor, thisCursor, "subscript", context );
    }
};


/** Internal assignee node for modifying the L field of an array (without mutability/modifiability or capacity checks) */
class TxArrayLenAssigneeNode : public TxAssigneeNode {
protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        auto opType = this->array->originalExpr->resolve_type( passInfo );
        if ( opType->get_type_class() == TXTC_REFERENCE ) {
            auto targType = opType->target_type();
            if ( targType->get_type_class() == TXTC_ARRAY ) {
                this->array->insert_qual_conversion( passInfo, targType );
            }
        }
        opType = this->array->resolve_type( passInfo );
        if ( opType->get_type_class() != TXTC_ARRAY )
            CERR_THROWRES( this, "Can't modify L of non-array assignee expression: " << opType );

        return TxQualType( this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ), true );
    }

public:
    TxMaybeConversionNode* array;

    TxArrayLenAssigneeNode( const TxLocation& ploc, TxExpressionNode* array )
            : TxAssigneeNode( ploc ), array( new TxMaybeConversionNode( array ) ) {
    }

    virtual TxArrayLenAssigneeNode* make_ast_copy() const override {
        return new TxArrayLenAssigneeNode( this->ploc, this->array->originalExpr->make_ast_copy() );
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->array;
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->array->visit_ast( visitor, thisCursor, "array", context );
    }
};
