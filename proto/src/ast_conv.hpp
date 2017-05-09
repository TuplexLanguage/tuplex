#pragma once

#include "ast_declbase.hpp"

/** Wraps the provided original expression with a new conversion expression node if necessary and permitted.
 * If a conversion node is created, symbol declaration pass is run on it.
 * Generates a compilation error if the types don't match and conversion is not possible.
 * @param _explicit if true, forces conversion between types that don't permit implicit conversion
 * @return a new conversion node that wraps the original node, or the original node itself if no conversion was applied
 */
TxExpressionNode* make_conversion( TxExpressionNode* originalExpr, const TxType* resultType, bool _explicit );

/** A specific conversion of an expression to a resulting type. */
class TxConversionNode : public TxExpressionNode {
protected:
    virtual const TxType* define_type() override {
        return this->resultType;
    }
public:
    TxExpressionNode* expr;
    TxType const * const resultType;

    TxConversionNode( TxExpressionNode* expr, const TxType* resultType )
            : TxExpressionNode( expr->parseLocation ), expr( expr ), resultType( resultType ) {
        ASSERT( resultType, "NULL resultType" );
    }

    virtual TxConversionNode* make_ast_copy() const override {
        ASSERT( false, "Can't make AST copy of a TxConversionNode: " << this );
        return nullptr;
    }

    virtual void symbol_declaration_pass( const LexicalContext& lexContext ) override {
        this->set_context( lexContext );
        if ( !this->expr->is_context_set() )
            this->expr->symbol_declaration_pass( lexContext );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->expr->symbol_resolution_pass();
    }

    virtual bool is_statically_constant() const override {
        return this->expr->is_statically_constant();
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) const override {
        this->expr->visit_ast( visitor, thisCursor, "convertee", context );
    }
};

class TxScalarConvNode : public TxConversionNode {
    class ScalarConvConstantProxy : public TxConstantProxy {
        const TxScalarConvNode* convNode;
        public:
        ScalarConvConstantProxy( const TxScalarConvNode* convNode )
                : convNode( convNode ) {
        }

        virtual const TxType* get_type() const override {
            return this->convNode->resultType;
        }
        virtual uint32_t get_value_UInt() const override {
            auto constProxy = this->convNode->expr->get_static_constant_proxy();
            return constProxy->get_value_UInt();
        }
        virtual llvm::Constant* code_gen( LlvmGenerationContext& context, GenScope* scope ) const;
    };
    ScalarConvConstantProxy constProxy;

public:
    TxScalarConvNode( TxExpressionNode* expr, const TxType* scalarResultType )
            : TxConversionNode( expr, scalarResultType ), constProxy( this ) {
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        return ( this->expr->get_static_constant_proxy() ? &this->constProxy : nullptr );
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;
};

class TxBoolConvNode : public TxConversionNode {
public:
    TxBoolConvNode( TxExpressionNode* expr, const TxType* boolResultType )
            : TxConversionNode( expr, boolResultType ) {
    }
    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;
};

class TxReferenceConvNode : public TxConversionNode {
    const TxType* adapterType = nullptr;
    protected:
    virtual const TxType* define_type() override;

public:
    TxReferenceConvNode( TxExpressionNode* expr, const TxType* refResultType )
            : TxConversionNode( expr, refResultType ) {
    }
    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;
};

/** Casts (not converts) between object specializations (across type parameters and inheritance). */
class TxObjSpecCastNode : public TxConversionNode {
public:
    TxObjSpecCastNode( TxExpressionNode* expr, const TxType* resultType )
            : TxConversionNode( expr, resultType ) {
    }
    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override;
};

/** A non-conversion "placeholder conversion". */
class TxNoConversionNode : public TxConversionNode {
public:
    TxNoConversionNode( TxExpressionNode* expr, const TxType* resultType )
            : TxConversionNode( expr, resultType ) {
    }
    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->expr->code_gen( context, scope );
    }
};
