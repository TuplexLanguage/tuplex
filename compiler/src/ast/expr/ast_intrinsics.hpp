#pragma once

#include "ast_expr_node.hpp"
#include "ast/type/ast_typeexpr_node.hpp"

#include "symbol/qual_type.hpp"
#include "symbol/type_registry.hpp"

/** intrinsic for _address( ref ) -> ULong */
class TxRefAddressNode : public TxExpressionNode {
protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        return this->registry().get_builtin_type( TXBT_ULONG );
    }

    virtual void verification_pass() const override {
        if ( this->refExpr->qtype()->get_type_class() != TXTC_REFERENCE )
            CERROR( this->refExpr, "Argument of _address( ) is not a reference: " << this->refExpr->qtype() );
    }

public:
    TxExpressionNode* refExpr;

    TxRefAddressNode( const TxLocation& ploc, TxExpressionNode* refExpr )
            : TxExpressionNode( ploc ), refExpr( refExpr )  { }

    virtual TxRefAddressNode* make_ast_copy() const override {
        return new TxRefAddressNode( this->ploc, this->refExpr->make_ast_copy() );
    }

    virtual bool is_statically_constant() const override final {
        return this->refExpr->is_statically_constant();
    }

    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->refExpr->visit_ast( visitor, thisCursor, "ref", context );
    }
};

/** intrinsic for _typeid( ref ) -> UInt */
class TxRefTypeIdNode : public TxExpressionNode {
protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        return this->registry().get_builtin_type( TXBT_UINT );
    }

    virtual void verification_pass() const override {
        if ( this->refExpr->qtype()->get_type_class() != TXTC_REFERENCE )
            CERROR( this->refExpr, "Argument of _typeid( ) is not a reference: " << this->refExpr->qtype() );
    }

public:
    TxExpressionNode* refExpr;

    TxRefTypeIdNode( const TxLocation& ploc, TxExpressionNode* refExpr )
            : TxExpressionNode( ploc ), refExpr( refExpr )  { }

    virtual TxRefTypeIdNode* make_ast_copy() const override {
        return new TxRefTypeIdNode( this->ploc, this->refExpr->make_ast_copy() );
    }

    virtual bool is_statically_constant() const override final {
        return this->refExpr->is_statically_constant();
    }

    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->refExpr->visit_ast( visitor, thisCursor, "ref", context );
    }
};

/** intrinsic for _typeid< type-expr > -> UInt */
class TxTypeExprTypeIdNode : public TxExpressionNode {
protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        return this->registry().get_builtin_type( TXBT_UINT );
    }

public:
    TxQualTypeExprNode* typeExpr;

    TxTypeExprTypeIdNode( const TxLocation& ploc, TxQualTypeExprNode* typeExpr )
            : TxExpressionNode( ploc ), typeExpr( typeExpr )  { }

    virtual TxTypeExprTypeIdNode* make_ast_copy() const override {
        return new TxTypeExprTypeIdNode( this->ploc, this->typeExpr->make_ast_copy() );
    }

    virtual bool is_statically_constant() const override final {
        if ( auto qtype = typeExpr->attempt_qtype() )
            return qtype->has_runtime_type_id();  // only true after type preparation & id has been assigned
        return false;
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->typeExpr->visit_ast( visitor, thisCursor, "typeexpr", context );
    }
};

/** intrinsic for _sizeof( expr ) -> UInt */
class TxSizeofExprNode : public TxExpressionNode {
protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        return this->registry().get_builtin_type( TXBT_UINT );
    }

public:
    TxExpressionNode* expr;

    TxSizeofExprNode( const TxLocation& ploc, TxExpressionNode* expr )
            : TxExpressionNode( ploc ), expr( expr )  { }

    virtual TxSizeofExprNode* make_ast_copy() const override {
        return new TxSizeofExprNode( this->ploc, this->expr->make_ast_copy() );
    }

    virtual bool is_statically_constant() const override final {
        return this->expr->is_statically_constant();
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->expr->visit_ast( visitor, thisCursor, "expr", context );
    }
};


#include "ast/type/ast_types.hpp"

/** intrinsic for _supertypes( typeId : UInt ) -> &Array<UInt> */
class TxSupertypesExprNode : public TxExpressionNode {
    TxTypeExpressionNode* typeExpr;
protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        return typeExpr->resolve_type( passInfo );
    }

    virtual void verification_pass() const override {
        if ( this->expr->qtype()->get_runtime_type_id() != TXBT_UINT )
            CERROR( this->expr, "Argument of _supertypes() is not a UInt: " << this->expr->qtype() );
    }

public:
    TxExpressionNode* expr;

    TxSupertypesExprNode( const TxLocation& ploc, TxExpressionNode* expr )
            : TxExpressionNode( ploc ), expr( expr )  {
        // TODO: this is a fixed type, can type registry assist with this instead of creating a bunch of nodes?
        auto targetTypeExpr = new TxQualTypeExprNode(
                new TxArrayTypeNode( ploc, new TxNamedTypeNode( this->ploc, "tx.UInt" ) ) );
        this->typeExpr = new TxReferenceTypeNode( this->ploc, nullptr,  targetTypeExpr );
    }

    virtual TxSupertypesExprNode* make_ast_copy() const override {
        return new TxSupertypesExprNode( this->ploc, this->expr->make_ast_copy() );
    }

    virtual bool is_statically_constant() const override final {
        return false; //this->expr->is_statically_constant();
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    //virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->typeExpr->visit_ast( visitor, thisCursor, "type", context );
        this->expr->visit_ast( visitor, thisCursor, "expr", context );
    }
};
