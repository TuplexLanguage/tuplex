#pragma once

#include "ast_assignee_node.hpp"

class TxFieldAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type() override {
        return this->field->resolve_type();
    }

public:
    TxFieldValueNode* field;
    TxFieldAssigneeNode( const TxLocation& parseLocation, TxFieldValueNode* field )
            : TxAssigneeNode( parseLocation ), field( field ) {
    }

    virtual TxFieldAssigneeNode* make_ast_copy() const override {
        return new TxFieldAssigneeNode( this->parseLocation, this->field->make_ast_copy() );
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->field->get_data_graph_origin_expr();
    }

    virtual void symbol_resolution_pass() override {
        TxAssigneeNode::symbol_resolution_pass();
        field->symbol_resolution_pass();

        auto fieldDecl = field->get_field_declaration();
        if ( fieldDecl && fieldDecl->get_storage() == TXS_NOSTORAGE )
            CERROR( this, "Assignee '" << field->symbolName << "' is not an L-value / has no storage." );
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->field->visit_ast( visitor, thisCursor, "field", context );
    }
};

class TxDerefAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type() override {
        auto refType = this->operand->resolve_type();
        if ( refType->get_type_class() != TXTC_REFERENCE )
            CERR_THROWRES( this, "Can't de-reference non-reference expression: " << refType );
        return refType->target_type();
    }

public:
    TxExpressionNode* operand;

    TxDerefAssigneeNode( const TxLocation& parseLocation, TxExpressionNode* operand )
            : TxAssigneeNode( parseLocation ), operand( operand ) {
    }

    virtual TxDerefAssigneeNode* make_ast_copy() const override {
        return new TxDerefAssigneeNode( this->parseLocation, this->operand->make_ast_copy() );
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->operand;
    }

    virtual void symbol_resolution_pass() override {
        TxAssigneeNode::symbol_resolution_pass();
        operand->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->operand->visit_ast( visitor, thisCursor, "operand", context );
    }
};

class TxElemAssigneeNode : public TxAssigneeNode {
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

    TxElemAssigneeNode( const TxLocation& parseLocation, TxExpressionNode* array, TxExpressionNode* subscript )
            : TxAssigneeNode( parseLocation ), array( new TxMaybeConversionNode( array ) ), subscript( new TxMaybeConversionNode( subscript ) ) {
    }

    virtual TxElemAssigneeNode* make_ast_copy() const override {
        return new TxElemAssigneeNode( this->parseLocation, this->array->originalExpr->make_ast_copy(),
                                       this->subscript->originalExpr->make_ast_copy() );
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->array;
    }

    virtual void symbol_resolution_pass() override {
        TxAssigneeNode::symbol_resolution_pass();
        array->symbol_resolution_pass();
        subscript->symbol_resolution_pass();
        // TODO: Add bounds checking. If statically constant operands, do static check, otherwise add AST nodes generating runtime check.
        // TODO: Support negative array indexing.
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->array->visit_ast( visitor, thisCursor, "array", context );
        this->subscript->visit_ast( visitor, thisCursor, "subscript", context );
    }
};
