#pragma once

#include "ast_base.hpp"


/** A specific conversion of an expression to a resulting type. */
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

    TxConversionNode(const TxLocation& parseLocation, TxExpressionNode* expr, const TxType* resultType)
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


/** A generic conversion node which can hold a specific conversion node for each specialization index. */
class TxGenericConversionNode : public TxExpressionNode {
    std::vector<TxExpressionNode*> specificConvs;

protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        auto expr = this->get_spec_expression(six);
        return expr->resolve_type(six);
    }

public:
    TxExpressionNode* const originalExpr;

    TxGenericConversionNode(TxExpressionNode* originalExpr)
            : TxExpressionNode(originalExpr->parseLocation), originalExpr(originalExpr) {
        ASSERT(originalExpr, "NULL originalExpr");
    }

    void insert_conversion(TxSpecializationIndex six, const TxType* resultType, bool _explicit=false);

    TxExpressionNode* get_spec_expression(TxSpecializationIndex six) const {
        if (this->specificConvs.size() > six) {
            if (auto expr = this->specificConvs.at(six))
                return expr;
        }
        return this->originalExpr;
    }

    virtual bool has_predefined_type() const override { return false; }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        auto expr = this->get_spec_expression(six);
        if (! expr->is_context_set(six))
            expr->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxExpressionNode::symbol_resolution_pass(six);
        auto expr = this->get_spec_expression(six);
        expr->symbol_resolution_pass(six);
    }

    virtual bool is_statically_constant() const override {
        // make this method six-dependent?
        return this->get_spec_expression(0)->is_statically_constant();
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        // TODO: make this method six-dependent?
        return this->get_spec_expression(0)->get_static_constant_proxy();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
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
    TxScalarConvNode(const TxLocation& parseLocation, TxExpressionNode* expr, const TxScalarType* resultType)
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
    TxBoolConvNode(const TxLocation& parseLocation, TxExpressionNode* expr, const TxBoolType* resultType)
        : TxConversionNode(parseLocation, expr, resultType) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxReferenceConvNode : public TxConversionNode {
    const TxType* adapterType = nullptr;
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override;

public:
    TxReferenceConvNode(const TxLocation& parseLocation, TxExpressionNode* expr, const TxReferenceType* resultType)
        : TxConversionNode(parseLocation, expr, resultType) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** Casts (not converts) between object specializations (across type parameters and inheritance). */
class TxObjSpecCastNode : public TxConversionNode {
public:
    TxObjSpecCastNode(const TxLocation& parseLocation, TxExpressionNode* expr, const TxType* resultType)
        : TxConversionNode(parseLocation, expr, resultType) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};
