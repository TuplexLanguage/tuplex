#pragma once

#include "ast/ast_entitydecls.hpp"


/** Wraps the provided original expression with a new conversion expression node if necessary and permitted.
 *
 * Assumes declaration pass has already run on originalExpr.
 * If a conversion node is created, the caller must insert it and run compilation passes as appropriate.
 *
 * Does not return NULL.
 * If the types don't match and conversion is not possible, a compilation error is thrown.
 *
 * @param explic if true, forces conversion between types that don't permit implicit conversion
 * @return a new conversion node that wraps the original node, or the original node itself if no conversion needed to be applied
 */
TxExpressionNode* make_conversion( TxExpressionNode* originalExpr, TxQualType requiredType, bool explic );

/** Evaluates if originalExpr may be automatically (implicitly) converted to the required type. */
bool auto_converts_to( TxExpressionNode* originalExpr, TxQualType requiredType );


/** A specific conversion of an expression to a resulting type. */
class TxConversionNode : public TxExpressionNode {
protected:
    TxQualType resultType;
    TxExpressionNode* expr;

    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return this->resultType;
    }
public:

    TxConversionNode( TxExpressionNode* expr, TxQualType resultType )
            : TxExpressionNode( expr->ploc ), resultType( resultType ), expr( expr ) {
        ASSERT( resultType, "NULL resultType" );
    }

    TxConversionNode* make_ast_copy() const override {
        ASSERT( false, "Can't make AST copy of a TxConversionNode: " << this );
        return nullptr;
    }

    bool is_statically_constant() const override {
        return this->expr->is_statically_constant();
    }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->expr->visit_ast( visitor, cursor, "convertee", aux );
    }
};

class TxScalarConvNode : public TxConversionNode {
public:
    TxScalarConvNode( TxExpressionNode* expr, TxQualType scalarResultType )
            : TxConversionNode( expr, scalarResultType ) {
    }

    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
};

class TxBoolConvNode : public TxConversionNode {
public:
    TxBoolConvNode( TxExpressionNode* expr, TxQualType boolResultType )
            : TxConversionNode( expr, boolResultType ) {
    }

    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
};

class TxReferenceConvNode : public TxConversionNode {
    const TxActualType* adapterType = nullptr;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

public:
    TxReferenceConvNode( TxExpressionNode* expr, TxQualType refResultType )
            : TxConversionNode( expr, refResultType ) {
    }
    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
};

/** Casts (not converts) between object specializations (across type parameters and inheritance). */
class TxObjSpecCastNode : public TxConversionNode {
public:
    TxObjSpecCastNode( TxExpressionNode* expr, TxQualType resultType )
            : TxConversionNode( expr, resultType ) {
    }
    TxFieldStorage get_storage() const override {
        return this->expr->get_storage();
    }
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->expr->code_gen_dyn_value( context, scope );
    }
    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override {
        return this->expr->code_gen_const_value( context );
    }
    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->expr->code_gen_dyn_address( context, scope );
    }
    llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override {
        return this->expr->code_gen_const_address( context );
    }
};

/*
/ ** A non-conversion "placeholder conversion". * /
class TxNoConversionNode : public TxConversionNode {
public:
    TxNoConversionNode( TxExpressionNode* expr, TxQualType resultType )
            : TxConversionNode( expr, resultType ) {
    }
    TxFieldStorage get_storage() const override {
        return this->expr->get_storage();
    }
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->expr->code_gen_dyn_value( context, scope );
    }
    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override {
        return this->expr->code_gen_const_value( context );
    }
    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->expr->code_gen_dyn_address( context, scope );
    }
    llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override {
        return this->expr->code_gen_const_address( context );
    }
};
*/
