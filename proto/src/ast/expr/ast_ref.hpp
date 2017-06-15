#pragma once

#include "ast_expr_node.hpp"

#include "ast_array.hpp"
#include "ast/type/ast_typearg_node.hpp"
#include "ast/ast_wrappers.hpp"
#include "ast/ast_declpass.hpp"
#include "ast_field.hpp"


llvm::Value* gen_get_struct_member( LlvmGenerationContext& context, GenScope* scope, llvm::Value* structV, unsigned ix );

llvm::Value* gen_get_ref_pointer( LlvmGenerationContext& context, GenScope* scope, llvm::Value* refV );
llvm::Value* gen_get_ref_typeid( LlvmGenerationContext& context, GenScope* scope, llvm::Value* refV );
llvm::Value* gen_ref( LlvmGenerationContext& context, GenScope* scope, llvm::Type* refT, llvm::Value* ptrV, llvm::Value* tidV );
llvm::Constant* gen_ref( LlvmGenerationContext& context, llvm::Type* refT, llvm::Constant* ptrC, llvm::Constant* tidC );

/** Converts a reference value from one type to another. If targetTypeId is specified, it will replace the original type id. */
llvm::Value* gen_ref_conversion( LlvmGenerationContext& context, GenScope* scope, llvm::Value* origRefV,
                                 llvm::Type* targetRefT, uint32_t targetTypeId = UINT32_MAX );

/** Converts a reference constant from one type to another. If targetTypeId is specified, it will replace the original type id. */
llvm::Constant* gen_ref_conversion( LlvmGenerationContext& context, llvm::Constant* origRefC,
                                    llvm::Type* targetRefT, uint32_t targetTypeId = UINT32_MAX );


class TxReferenceDerefNode : public TxExpressionNode {
    /** internal "cache" to prevent multiple code generations */
    mutable llvm::Value* refExprValue = nullptr;

protected:
    virtual const TxType* define_type() override {
        auto refType = this->reference->resolve_type();
        if ( refType->get_type_class() != TXTC_REFERENCE )
            CERR_THROWRES( this, "Can't de-reference non-reference expression: " << refType );
//        if (refType->target_type()->get_type_class() == TXTC_ARRAY) {
//            std::cerr << "   refType:     " << refType << std::endl;
//            std::cerr << "   target type: " << refType->target_type() << std::endl;
//        }
        return refType->target_type();
    }

public:
    TxExpressionNode* reference;
    TxReferenceDerefNode( const TxLocation& parseLocation, TxExpressionNode* operand )
            : TxExpressionNode( parseLocation ), reference( operand ) {
    }

    virtual TxReferenceDerefNode* make_ast_copy() const override {
        return new TxReferenceDerefNode( this->parseLocation, this->reference->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->reference->symbol_resolution_pass();
    }

    virtual const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->reference;
    }

    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->reference->visit_ast( visitor, thisCursor, "ref", context );
    }
};

class TxReferenceToNode : public TxExpressionNode {
    TxTypeTypeArgumentNode* targetTypeNode;
    TxExpressionNode* target;

protected:
    virtual void declaration_pass() override {
        // Special case handling: When this is is applied as an implicit conversion node,
        // target may have already run declaration pass, so we run it for targetTypeNode:
        if ( this->target->is_context_set() ) {
            run_declaration_pass( this->targetTypeNode, this, "type" );
        }
    }

    virtual const TxType* define_type() override {
        return this->registry().get_reference_type( this, this->targetTypeNode, nullptr );
    }

public:
    TxReferenceToNode( const TxLocation& parseLocation, TxExpressionNode* target )
            : TxExpressionNode( parseLocation ), target( target ) {
        TxTypeExprWrapperNode* targetTypeExpr = new TxTypeExprWrapperNode( this->target );
        this->targetTypeNode = new TxTypeTypeArgumentNode( targetTypeExpr );
    }

    virtual TxReferenceToNode* make_ast_copy() const override {
        return new TxReferenceToNode( this->parseLocation, this->target->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->target->symbol_resolution_pass();

        auto targetNode = this->target;
        if ( auto wrapperNode = dynamic_cast<TxExprWrapperNode*>( targetNode ) )
            targetNode = wrapperNode->exprNode;
        if ( !( dynamic_cast<TxFieldValueNode*>( targetNode )
                || dynamic_cast<TxElemDerefNode*>( targetNode )
                || targetNode->is_statically_constant() ) ) {
            CERROR( this, "Can't construct reference to non-addressable expression / rvalue: " << targetNode );
        }
    }

    virtual const std::vector<TxExpressionNode*>* get_applied_func_args() const override {
        return this->target->get_applied_func_args();
    }
    virtual void set_applied_func_args( const std::vector<TxExpressionNode*>* appliedTypeParameters ) override {
        this->target->set_applied_func_args( appliedTypeParameters );
    }

    virtual bool is_statically_constant() const override {
        if ( this->target->get_storage() == TXS_GLOBAL || this->target->get_storage() == TXS_STATIC )
            return true;
        return false;
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->targetTypeNode->visit_ast( visitor, thisCursor, "type", context );
        this->target->visit_ast( visitor, thisCursor, "target", context );
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
