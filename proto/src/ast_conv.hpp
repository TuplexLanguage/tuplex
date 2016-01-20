#pragma once

#include "ast_base.hpp"


class TxConversionNode : public TxExpressionNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        // FIXME: type equality logic
        //auto type = expr->resolve_type(six);
        //ASSERT(type && (*type) == (*this->resultType), "Mismatching types in " << this << ": \n" << type << " != \n" << this->resultType);
        return this->resultType;
    }
public:
    TxExpressionNode* expr;
    TxType const * const resultType;

    TxConversionNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxType* resultType)
            : TxExpressionNode(parseLocation), expr(expr), resultType(resultType) {
        ASSERT(resultType, "NULL resultType");
    }

    virtual bool has_predefined_type() const override { return this->resultType->get_symbol(); }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        if (! this->expr->is_context_set(six))
            this->expr->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxExpressionNode::symbol_resolution_pass(six);
        this->expr->symbol_resolution_pass(six);
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
    TxScalarConvNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxScalarType* resultType)
        : TxConversionNode(parseLocation, expr, resultType), constProxy()  { }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxConversionNode::symbol_resolution_pass(six);
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
    TxBoolConvNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxBoolType* resultType)
        : TxConversionNode(parseLocation, expr, resultType) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxReferenceConvNode : public TxConversionNode {
    const TxType* adapterType = nullptr;
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override;

public:
    TxReferenceConvNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxReferenceType* resultType)
        : TxConversionNode(parseLocation, expr, resultType) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** Casts (not converts) between object specializations (across type parameters and inheritance). */
class TxObjSpecCastNode : public TxConversionNode {
public:
    TxObjSpecCastNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxType* resultType)
        : TxConversionNode(parseLocation, expr, resultType) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};
