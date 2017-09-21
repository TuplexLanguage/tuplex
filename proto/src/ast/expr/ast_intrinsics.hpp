#pragma once

#include "ast_expr_node.hpp"
#include "ast/type/ast_typeexpr_node.hpp"

#include "symbol/qual_type.hpp"
#include "symbol/type_registry.hpp"

/** intrinsic for _address( ref ) -> ULong */
class TxRefAddressNode : public TxExpressionNode {
protected:
    virtual const TxQualType* define_type() override {
        return new TxQualType( this->registry().get_builtin_type( TXBT_ULONG ) );
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

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->refExpr->symbol_resolution_pass();
        if ( this->refExpr->qualtype()->get_type_class() != TXTC_REFERENCE ) {
            CERROR( this->refExpr, "Argument of _address( ) is not a reference: " << this->refExpr->qualtype() );
        }
    }

    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->refExpr->visit_ast( visitor, thisCursor, "ref", context );
    }
};

/** intrinsic for _typeid( ref ) -> UInt */
class TxRefTypeIdNode : public TxExpressionNode {
protected:
    virtual const TxQualType* define_type() override {
        return new TxQualType( this->registry().get_builtin_type( TXBT_UINT ) );
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

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->refExpr->symbol_resolution_pass();
        if ( this->refExpr->qualtype()->get_type_class() != TXTC_REFERENCE ) {
            CERROR( this->refExpr, "Argument of _typeid( ) is not a reference: " << this->refExpr->qualtype() );
        }
    }

    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->refExpr->visit_ast( visitor, thisCursor, "ref", context );
    }
};

/** intrinsic for _typeid< type-expr > -> UInt */
class TxTypeExprTypeIdNode : public TxExpressionNode {
protected:
    virtual const TxQualType* define_type() override {
        return new TxQualType( this->registry().get_builtin_type( TXBT_UINT ) );
    }

public:
    TxTypeExpressionNode* typeExpr;

    TxTypeExprTypeIdNode( const TxLocation& ploc, TxTypeExpressionNode* refExpr )
            : TxExpressionNode( ploc ), typeExpr( refExpr )  { }

    virtual TxTypeExprTypeIdNode* make_ast_copy() const override {
        return new TxTypeExprTypeIdNode( this->ploc, this->typeExpr->make_ast_copy() );
    }

    virtual bool is_statically_constant() const override final {
        return true;
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->typeExpr->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->typeExpr->visit_ast( visitor, thisCursor, "typeexpr", context );
    }
};

/** intrinsic for _sizeof( expr ) -> UInt */
class TxSizeofExprNode : public TxExpressionNode {
protected:
    virtual const TxQualType* define_type() override {
        return new TxQualType( this->registry().get_builtin_type( TXBT_UINT ) );
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

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->expr->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->expr->visit_ast( visitor, thisCursor, "expr", context );
    }
};


#include "ast/type/ast_types.hpp"

/** intrinsic for _supertypes( typeId : UInt ) -> &Array<UInt> */
class TxSupertypesExprNode : public TxExpressionNode {
    TxTypeExpressionNode* typeExpr;
protected:
    virtual const TxQualType* define_type() override {
        return typeExpr->resolve_type();
    }

public:
    TxExpressionNode* expr;

    TxSupertypesExprNode( const TxLocation& ploc, TxExpressionNode* expr )
            : TxExpressionNode( ploc ), expr( expr )  {
        this->typeExpr = new TxReferenceTypeNode( this->ploc, nullptr, new TxArrayTypeNode( this->ploc, new TxNamedTypeNode( this->ploc, "tx.UInt" ) ) );
    }

    virtual TxSupertypesExprNode* make_ast_copy() const override {
        return new TxSupertypesExprNode( this->ploc, this->expr->make_ast_copy() );
    }

    virtual bool is_statically_constant() const override final {
        return false; //this->expr->is_statically_constant();
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->typeExpr->symbol_resolution_pass();
        this->expr->symbol_resolution_pass();
        if ( this->expr->qualtype()->get_type_id() != TXBT_UINT ) {
            CERROR( this->expr, "Argument of _supertypes() is not a UInt: " << this->expr->qualtype() );
        }
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    //virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->typeExpr->visit_ast( visitor, thisCursor, "type", context );
        this->expr->visit_ast( visitor, thisCursor, "expr", context );
    }
};
