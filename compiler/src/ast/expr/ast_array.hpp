#pragma once

#include "ast/ast_entitydecls.hpp"
#include "ast/type/ast_types.hpp"
#include "ast_assignee_node.hpp"

class TxArrayLitNode : public TxTypeDefiningValExprNode {
public:
    TxArrayLitNode( const TxLocation& ploc ) : TxTypeDefiningValExprNode( ploc ) { }

    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
};

/** Represents filled array literals, explicitly specified in source code as well as array initializers created implicitly
 * (e.g. for var-arg functions). Filled means their length will equal capacity, all elements initialized.
 * Note that an array literal doesn't necessarily only have literal elements; it is statically constant only if all its elements are.
 */
class TxFilledArrayLitNode : public TxArrayLitNode {
    std::vector<TxExpressionNode*> const * const origElemExprList;
    TxQualTypeExprNode* elementTypeExpr;
    TxMaybeConversionNode* capacityExpr;

    TxTypeArgumentNode* elementTypeNode;  // always non-null if elementTypeExpr is non-null
    TxTypeArgumentNode* capacityArgNode;  // can be non-null even if capacityExpr is null
    bool _directArrayArg = false;
    bool _constant = false;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

    void resolution_pass() override;

    void verification_pass() const override;

public:
    std::vector<TxMaybeConversionNode*> const * const elemExprList;

    /** Represents an empty array with the specified element type. */
    TxFilledArrayLitNode( const TxLocation& ploc, TxQualTypeExprNode* elementTypeExpr,
                          TxExpressionNode* capacityExpr )
            : TxFilledArrayLitNode( ploc, elementTypeExpr, new std::vector<TxExpressionNode*>(), capacityExpr ) {
    }

    /** Represents a non-empty array with the specified element type. */
    TxFilledArrayLitNode( const TxLocation& ploc, TxQualTypeExprNode* elementTypeExpr,
                          const std::vector<TxExpressionNode*>* elemExprList,
                          TxExpressionNode* capacityExpr );

    /** Represents a non-empty array with the element type defined by the first element.
     * The provided element expression list must not be empty. */
    TxFilledArrayLitNode( const TxLocation& ploc, const std::vector<TxExpressionNode*>* elemExprList );

    /** Creates an array literal node with the specified element type, with elements that are owned by another AST node.
     * The resulting array literal node may not be AST-copied. */
    TxFilledArrayLitNode( const TxLocation& ploc, TxQualTypeExprNode* elementTypeExpr,
                          const std::vector<TxMaybeConversionNode*>* elemExprList );


    TxFilledArrayLitNode* make_ast_copy() const override {
        if ( !this->origElemExprList ) {
            ASSERT( false, "Can't make AST copy of a TxArrayLitNode whose elements are owned by another AST node: " << this );
            return nullptr;
        }
        return new TxFilledArrayLitNode( ploc,
                                         ( elementTypeExpr ? elementTypeExpr->make_ast_copy() : nullptr ),
                                         make_node_vec_copy( origElemExprList ),
                                         ( capacityExpr ? capacityExpr->originalExpr->make_ast_copy() : nullptr ) );
    }

    TxFieldStorage get_storage() const override {
        if ( this->_directArrayArg )
            return this->elemExprList->front()->get_storage();
        return TXS_NOSTORAGE;
    }

    bool is_statically_constant() const override {
        return this->_constant;
    }

    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->elementTypeNode->visit_ast( visitor, cursor, "elem-type", aux );
        if ( this->capacityArgNode )
            this->capacityArgNode->visit_ast( visitor, cursor, "capacity", aux );
        if ( this->origElemExprList ) {
            // if this node owns the element nodes, perform pass on them:
            for ( auto elem : *this->elemExprList )
                elem->visit_ast( visitor, cursor, "element", aux );
        }
    }
};

class [[maybe_unused]] TxUnfilledArrayLitNode : public TxArrayLitNode {
    TxTypeExpressionNode* arrayTypeNode;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

public:
    /** Represents an unfilled array with the specified type. */
    TxUnfilledArrayLitNode( const TxLocation& ploc, TxTypeExpressionNode* arrayTypeExpr );

    TxUnfilledArrayLitNode* make_ast_copy() const override {
        return new TxUnfilledArrayLitNode( this->ploc, this->arrayTypeNode->make_ast_copy() );
    }

    TxFieldStorage get_storage() const override {
        return TXS_NOSTORAGE;
    }

    bool is_statically_constant() const override {
        return this->arrayTypeNode->qtype()->is_static();
    }

    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->arrayTypeNode->visit_ast( visitor, cursor, "type", aux );
    }
};

class [[maybe_unused]] TxUnfilledArrayCompLitNode : public TxArrayLitNode {
    TxTypeArgumentNode* elementTypeNode;
    TxMaybeConversionNode* capacityExpr;
    TxTypeArgumentNode* capacityArgNode;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

public:
    /** Represents an unfilled array with the specified type. */
    TxUnfilledArrayCompLitNode( const TxLocation& ploc, TxTypeArgumentNode* elemTypeExpr, TxExpressionNode* capacityExpr = nullptr );

    TxUnfilledArrayCompLitNode* make_ast_copy() const override {
        return new TxUnfilledArrayCompLitNode( this->ploc,
                                               this->elementTypeNode->make_ast_copy(),
                                               this->capacityExpr->originalExpr->make_ast_copy() );
    }

    TxFieldStorage get_storage() const override {
        return TXS_NOSTORAGE;
    }

    bool is_statically_constant() const override {
        return this->capacityExpr->is_statically_constant();
    }

    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->elementTypeNode->visit_ast( visitor, cursor, "elem-type", aux );
        this->capacityArgNode->visit_ast( visitor, cursor, "capacity", aux );
    }
};



// TODO: Support negative array indexing.
class TxElemDerefNode : public TxExpressionNode {
    /** true if this is part of a nested array element assignee expression */
    bool _elemAssignment = false;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        this->subscript->insert_conversion( typeResLevel, this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ) );

        auto opType = this->array->originalExpr->resolve_type( typeResLevel );
        if ( opType->get_type_class() == TXTC_REFERENCE ) {
            auto targType = opType->target_type();
            if ( targType->get_type_class() == TXTC_ARRAY ) {
                this->array->insert_qual_conversion( typeResLevel, targType );
            }
        }
        opType = this->array->resolve_type( typeResLevel );
        if ( opType->get_type_class() != TXTC_ARRAY )
            CERR_THROWRES( this, "Can't subscript non-array expression: " << opType );
        return opType->element_type();
    }

public:
    TxMaybeConversionNode* array;
    TxMaybeConversionNode* subscript;

    TxElemDerefNode( const TxLocation& ploc, TxExpressionNode* operand, TxExpressionNode* subscript );

    TxElemDerefNode* make_ast_copy() const override {
        return new TxElemDerefNode( this->ploc, this->array->originalExpr->make_ast_copy(),
                                    this->subscript->originalExpr->make_ast_copy() );
    }

    const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->array;
    }

    TxFieldStorage get_storage() const override {
        return this->array->get_storage();
    }

    bool is_statically_constant() const override {
        return ( this->array->is_statically_constant() && this->subscript->is_statically_constant() );
    }

    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override;
    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->array->visit_ast( visitor, cursor, "array", aux );
        this->subscript->visit_ast( visitor, cursor, "subscript", aux );
    }

    void set_elem_assignee_expr();
};


// TODO: Support negative array indexing.
class TxElemAssigneeNode : public TxAssigneeNode {
protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

public:
    TxMaybeConversionNode* array;
    TxMaybeConversionNode* subscript;

    TxElemAssigneeNode( const TxLocation& ploc, TxExpressionNode* array, TxExpressionNode* subscript );

    TxElemAssigneeNode* make_ast_copy() const override {
        return new TxElemAssigneeNode( this->ploc, this->array->originalExpr->make_ast_copy(),
                                       this->subscript->originalExpr->make_ast_copy() );
    }

    const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->array;
    }

    llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->array->visit_ast( visitor, cursor, "array", aux );
        this->subscript->visit_ast( visitor, cursor, "subscript", aux );
    }
};


/** Internal assignee node for modifying the L field of an array (without mutability/modifiability or capacity checks) */
class TxArrayLenAssigneeNode : public TxAssigneeNode {
protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        auto opType = this->array->originalExpr->resolve_type( typeResLevel );
        if ( opType->get_type_class() == TXTC_REFERENCE ) {
            auto targType = opType->target_type();
            if ( targType->get_type_class() == TXTC_ARRAY ) {
                this->array->insert_qual_conversion( typeResLevel, targType );
            }
        }
        opType = this->array->resolve_type( typeResLevel );
        if ( opType->get_type_class() != TXTC_ARRAY )
            CERR_THROWRES( this, "Can't modify L of non-array assignee expression: " << opType );

        return TxQualType( this->registry().get_builtin_type( ARRAY_SUBSCRIPT_TYPE_ID ), true );
    }

public:
    TxMaybeConversionNode* array;

    TxArrayLenAssigneeNode( const TxLocation& ploc, TxExpressionNode* array )
            : TxAssigneeNode( ploc ), array( new TxMaybeConversionNode( array ) ) {
    }

    TxArrayLenAssigneeNode* make_ast_copy() const override {
        return new TxArrayLenAssigneeNode( this->ploc, this->array->originalExpr->make_ast_copy() );
    }

    const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->array;
    }

    llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->array->visit_ast( visitor, cursor, "array", aux );
    }
};
