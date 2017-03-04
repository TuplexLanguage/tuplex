#pragma once

#include "ast_declbase.hpp"



/** Wraps the provided original expression with a new conversion expression node if necessary. */
TxExpressionNode* make_conversion( TxExpressionNode* originalExpr, const TxType* resultType, bool _explicit );


/** A specific conversion of an expression to a resulting type. */
class TxConversionNode : public TxExpressionNode {
protected:
    virtual const TxType* define_type() override {
        // FIXME: type equality logic
        //auto type = expr->resolve_type();
        //ASSERT(type && (*type) == (*this->resultType), "Mismatching types in " << this << ": \n" << type << " != \n" << this->resultType);
        return this->resultType;
    }
public:
    TxExpressionNode* expr;
    TxType const * const resultType;

    TxConversionNode( TxExpressionNode* expr, const TxType* resultType )
            : TxExpressionNode(expr->parseLocation), expr(expr), resultType(resultType) {
        ASSERT(resultType, "NULL resultType");
    }

    virtual TxConversionNode* make_ast_copy() const override {
        ASSERT(false, "Can't make AST copy of a TxConversionNode: " << this);
        return nullptr;
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        if (! this->expr->is_context_set())
            this->expr->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->expr->symbol_resolution_pass();
    }

    virtual bool is_statically_constant() const override { return this->expr->is_statically_constant(); }
};



class TxScalarConvNode : public TxConversionNode {
    class ScalarConvConstantProxy : public TxConstantProxy {
        const TxScalarConvNode* convNode;
        const TxConstantProxy* originalConstant;

    public:
        ScalarConvConstantProxy() : convNode(), originalConstant()  { }
        ScalarConvConstantProxy(const TxScalarConvNode* convNode, const TxConstantProxy* originalConstant)
            : convNode(convNode), originalConstant(originalConstant)  { }

        void init(const TxScalarConvNode* convNode, const TxConstantProxy* originalConstant) {
            this->convNode = convNode;
            this->originalConstant = originalConstant;
        }

        inline const TxConstantProxy* original_constant() const { return this->originalConstant; }
        virtual const TxType* get_type() const override { return this->convNode->resultType; }
        virtual uint32_t get_value_UInt() const override { return this->originalConstant->get_value_UInt(); }
        virtual llvm::Constant* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
    };

    ScalarConvConstantProxy constProxy;
public:
    TxScalarConvNode( TxExpressionNode* expr, const TxScalarType* resultType )
        : TxConversionNode( expr, resultType ), constProxy()  { }

    virtual void symbol_resolution_pass() override {
        TxConversionNode::symbol_resolution_pass();
        if (auto originalConstant = this->expr->get_static_constant_proxy())
            this->constProxy.init(this, originalConstant);
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        return (this->constProxy.original_constant() ? &this->constProxy : nullptr);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxBoolConvNode : public TxConversionNode {
public:
    TxBoolConvNode( TxExpressionNode* expr, const TxBoolType* resultType )
        : TxConversionNode( expr, resultType ) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxReferenceConvNode : public TxConversionNode {
    const TxType* adapterType = nullptr;
protected:
    virtual const TxType* define_type() override;

public:
    TxReferenceConvNode( TxExpressionNode* expr, const TxReferenceType* resultType )
        : TxConversionNode( expr, resultType ) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** Casts (not converts) between object specializations (across type parameters and inheritance). */
class TxObjSpecCastNode : public TxConversionNode {
public:
    TxObjSpecCastNode( TxExpressionNode* expr, const TxType* resultType )
        : TxConversionNode( expr, resultType ) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


/** A non-conversion "placeholder conversion". */
class TxNoConversionNode : public TxConversionNode {
public:
    TxNoConversionNode( TxExpressionNode* expr, const TxType* resultType )
        : TxConversionNode( expr, resultType ) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override {
        return this->expr->code_gen( context, scope );
    }
};
