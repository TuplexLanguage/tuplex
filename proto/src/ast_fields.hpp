#pragma once

#include "ast_base.hpp"


class TxFieldValueNode : public TxExpressionNode {
    TxSymbolScope* cachedSymbol = nullptr;

    TxSymbolScope* resolve_symbol(ResolutionContext& resCtx);

protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override;

public:
    TxExpressionNode* baseExpr;
    const std::string memberName;

    TxFieldValueNode(const yy::location& parseLocation, TxExpressionNode* base, const std::string& memberName)
        : TxExpressionNode(parseLocation), baseExpr(base), memberName(memberName) {
    }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        if (this->baseExpr)
            this->baseExpr->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        if (this->baseExpr)
            this->baseExpr->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        if (this->baseExpr)
            this->baseExpr->semantic_pass();
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        this->LOGGER().trace("Getting static constant proxy for field %s", this->memberName.c_str());
        if (auto ent = this->get_field_entity())
            if (auto constProxy = ent->get_static_constant_proxy()) {
                this->LOGGER().debug("Returning static constant proxy for field %s", ent->get_full_name().to_string().c_str());
                return constProxy;
            }
        return nullptr;
    }

    virtual bool is_statically_constant() const override {
        if (auto ent = this->get_field_entity())
            return ent->is_statically_constant();
        return false;
    }

    inline const TxFieldEntity* get_field_entity() const { return dynamic_cast<TxFieldEntity*>(this->cachedSymbol); }


    virtual llvm::Value* code_gen_address(LlvmGenerationContext& context, GenScope* scope, bool foldStatics=false) const;
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxNewExprNode;

/** Substitute for TxFieldValueNode for calling constructors. */
class TxConstructorCalleeExprNode : public TxExpressionNode {
    const TxNewExprNode* newExpr;
    TxFieldEntity* constructorEntity = nullptr;

    mutable llvm::Value* objectPtrV = nullptr;
    friend TxNewExprNode;

protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override;

public:
    TxConstructorCalleeExprNode(const yy::location& parseLocation, const TxNewExprNode* newExpr)
            : TxExpressionNode(parseLocation), newExpr(newExpr) { }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
    }

    virtual void semantic_pass() override { }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};
