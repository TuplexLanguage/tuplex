#pragma once

#include "ast_base.hpp"


class TxFieldValueNode : public TxExpressionNode {
    const TxEntityDeclaration* declaration = nullptr;
    const TxField* field = nullptr;

    TxScopeSymbol* resolve_symbol(ResolutionContext& resCtx);
    const TxEntityDeclaration* resolve_decl(ResolutionContext& resCtx);

protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override;

public:
    TxExpressionNode* baseExpr;
    const std::string memberName;

    TxFieldValueNode(const yy::location& parseLocation, TxExpressionNode* base, const std::string& memberName)
        : TxExpressionNode(parseLocation), baseExpr(base), memberName(memberName) {
    }

    /** Returns the full identifier (dot-separated full name) as specified in the program text,
     * up to and including this name. */
    TxIdentifier get_full_identifier() const {
        if (auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>(this->baseExpr))
            return TxIdentifier(baseSymbolNode->get_full_identifier(), this->memberName);
        else
            return TxIdentifier(this->memberName);
    }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        if (this->baseExpr)
            this->baseExpr->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        // not invoking baseExpr->symbol_resolution_pass() since that is only done via define_type()
        //if (this->baseExpr)
        //    this->baseExpr->symbol_resolution_pass(resCtx);
        if (! this->get_type()) {
            if (this->declaration)
                CERROR(this, "Symbol is not a field: " << this->declaration);
            else {
                CERROR(this, "No such symbol: " << this->get_full_identifier());
            }
        }
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        this->LOGGER().trace("Getting static constant proxy for field %s", this->memberName.c_str());
        if (this->field)
            if (auto constProxy = this->field->get_static_constant_proxy()) {
                this->LOGGER().debug("Returning static constant proxy for field %s", this->field->get_symbol()->get_full_name().to_string().c_str());
                return constProxy;
            }
        return nullptr;
    }

    virtual bool is_statically_constant() const override {
        if (this->field)
            return this->field->is_statically_constant();
        return false;
    }

    // should not be called before symbol is resolved:
    inline const TxField* get_field() const { return this->field; }
    inline const TxFieldDeclaration* get_field_declaration() const { return (this->field ? this->field->get_declaration() : nullptr); }


    virtual llvm::Value* code_gen_address(LlvmGenerationContext& context, GenScope* scope, bool foldStatics=false) const;
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};
