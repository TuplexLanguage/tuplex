#pragma once

#include "ast_expr_node.hpp"

#include "ast_array.hpp"
#include "ast/type/ast_typearg_node.hpp"
#include "ast/ast_wrappers.hpp"
#include "ast/ast_declpass.hpp"
#include "ast_field.hpp"


llvm::Value*    gen_get_struct_member( LlvmGenerationContext& context, GenScope* scope, llvm::Value* structV, unsigned ix );
llvm::Constant* gen_get_struct_member( LlvmGenerationContext& context, llvm::Constant* structC, unsigned ix );

llvm::Value* gen_get_ref_pointer( LlvmGenerationContext& context, GenScope* scope, llvm::Value* refV );
llvm::Value* gen_get_ref_typeid ( LlvmGenerationContext& context, GenScope* scope, llvm::Value* refV );
llvm::Constant* gen_get_ref_pointer( LlvmGenerationContext& context, llvm::Constant* refC );
llvm::Constant* gen_get_ref_typeid ( LlvmGenerationContext& context, llvm::Constant* refC );

llvm::Value*    gen_ref( LlvmGenerationContext& context, GenScope* scope, llvm::Type* refT, llvm::Value* ptrV, llvm::Value* tidV );
llvm::Constant* gen_ref( LlvmGenerationContext& context, llvm::Type* refT, llvm::Constant* ptrC, llvm::Constant* tidC );

/** Generates a Ref value where the type id equals the specified type's *statically known* type id.
 * Caller must ensure this cannot be different from the proper runtime id.
 */
llvm::Value* gen_ref( LlvmGenerationContext& context, GenScope* scope, const TxActualType* refType, llvm::Value* ptrV );

/** Converts a reference value from one type to another. If targetTypeId is specified, it will replace the original type id. */
llvm::Value* gen_ref_conversion( LlvmGenerationContext& context, GenScope* scope, llvm::Value* origRefV,
                                 llvm::Type* targetRefT, llvm::Value* tidV = nullptr );

/** Converts a reference constant from one type to another. If targetTypeId is specified, it will replace the original type id. */
llvm::Constant* gen_ref_conversion( LlvmGenerationContext& context, llvm::Constant* origRefC,
                                    llvm::Type* targetRefT, llvm::Constant* tidC = nullptr );


class TxReferenceDerefNode : public TxExpressionNode {
    /** internal "cache" to prevent multiple code generations */
    mutable llvm::Value* refExprValue = nullptr;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        auto refType = this->reference->resolve_type( typeResLevel );
        if ( refType->get_type_class() != TXTC_REFERENCE )
            CERR_THROWRES( this, "Can't de-reference non-reference expression: " << refType );
        return refType->target_type();
    }

public:
    TxExpressionNode* reference;

    TxReferenceDerefNode( const TxLocation& ploc, TxExpressionNode* operand )
            : TxExpressionNode( ploc ), reference( operand ) {
    }

    TxReferenceDerefNode* make_ast_copy() const override {
        return new TxReferenceDerefNode( this->ploc, this->reference->make_ast_copy() );
    }

    const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->reference;
    }

    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Value* code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Constant* code_gen_typeid( LlvmGenerationContext& context ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->reference->visit_ast( visitor, cursor, "ref", aux );
    }
};

class TxReferenceToNode : public TxTypeDefiningValExprNode {
    TxTypeArgumentNode* targetTypeNode;
    TxExpressionNode* target;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return this->registry().get_reference_type( this, this->targetTypeNode, nullptr );
    }

    void verification_pass() const override;

public:
    TxReferenceToNode( const TxLocation& ploc, TxExpressionNode* target )
            : TxTypeDefiningValExprNode( ploc ),
              targetTypeNode( new TxTypeArgumentNode( new TxQualTypeExprNode( ploc, new TxTypeExprWrapperNode( target ) ) ) ),
              target( target ) {
    }

    TxReferenceToNode* make_ast_copy() const override {
        return new TxReferenceToNode( this->ploc, this->target->make_ast_copy() );
    }

    const std::vector<TxExpressionNode*>* get_applied_func_args() const override {
        return this->target->get_applied_func_args();
    }
    void set_applied_func_args( const std::vector<TxExpressionNode*>* appliedTypeParameters ) override {
        this->target->set_applied_func_args( appliedTypeParameters );
    }

    bool is_statically_constant() const override {
        if ( this->target->get_storage() == TXS_GLOBAL || this->target->get_storage() == TXS_STATIC )
            return true;
        return false;
    }

    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->targetTypeNode->visit_ast( visitor, cursor, "target-type", aux );
        this->target->visit_ast( visitor, cursor, "target", aux );
    }
};


class TxDerefAssigneeNode : public TxAssigneeNode {
    /** internal "cache" to prevent multiple code generations */
    mutable llvm::Value* refExprValue = nullptr;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        auto refType = this->reference->resolve_type( typeResLevel );
        if ( refType->get_type_class() != TXTC_REFERENCE )
            CERR_THROWRES( this, "Can't de-reference non-reference expression: " << refType );
        return refType->target_type();
    }

public:
    TxExpressionNode* reference;

    TxDerefAssigneeNode( const TxLocation& ploc, TxExpressionNode* operand )
            : TxAssigneeNode( ploc ), reference( operand ) {
    }

    TxDerefAssigneeNode* make_ast_copy() const override {
        return new TxDerefAssigneeNode( this->ploc, this->reference->make_ast_copy() );
    }

    const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->reference;
    }

    llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Value* code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->reference->visit_ast( visitor, cursor, "ref", aux );
    }
};
