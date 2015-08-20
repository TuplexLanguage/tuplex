#pragma once

#include "ast_base.hpp"


class TxFieldValueNode : public TxExpressionNode {
    TxScopeSymbol* resolve_symbol(TxSpecializationIndex six, ResolutionContext& resCtx);
    const TxEntityDeclaration* resolve_decl(TxSpecializationIndex six, ResolutionContext& resCtx);

protected:
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override;

public:
    TxExpressionNode* baseExpr;
    const std::string symbolName;

    TxFieldValueNode(const yy::location& parseLocation, TxExpressionNode* base, const std::string& memberName)
        : TxExpressionNode(parseLocation), baseExpr(base), symbolName(memberName) {
    }

    /** Returns the full identifier (dot-separated full name) as specified in the program text,
     * up to and including this name. */
    TxIdentifier get_full_identifier() const {
        if (auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>(this->baseExpr))
            return TxIdentifier(baseSymbolNode->get_full_identifier(), this->symbolName);
        else
            return TxIdentifier(this->symbolName);
    }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        if (this->baseExpr)
            this->baseExpr->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(six, resCtx);
        // not invoking baseExpr->symbol_resolution_pass() since that is only done via define_type()
        //if (this->baseExpr)
        //    this->baseExpr->symbol_resolution_pass(six, resCtx);
        if (auto typeDecl = dynamic_cast<const TxTypeDeclaration*>(this->get_spec(six)->declaration))
            CERROR(this, "'" << get_full_identifier() << "' resolved to a type, not a field: " << typeDecl);
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        this->LOGGER().trace("Getting static constant proxy for field %s", this->symbolName.c_str());
        if (auto field = this->get_field(0))
            if (auto constProxy = field->get_static_constant_proxy()) {
                this->LOGGER().debug("Returning static constant proxy for field %s", field->get_symbol()->get_full_name().to_string().c_str());
                return constProxy;
            }
        return nullptr;
    }

    virtual bool is_statically_constant() const override {
        if (auto field = this->get_field(0))
            return field->is_statically_constant();
        return false;
    }

    // should not be called before symbol is resolved:
    inline const TxField* get_field(TxSpecializationIndex six) const { return this->get_spec(six)->field; }
    inline const TxFieldDeclaration* get_field_declaration(TxSpecializationIndex six) const {
        return dynamic_cast<const TxFieldDeclaration*>(this->get_spec(six)->declaration);
    }


    virtual llvm::Value* code_gen_address(LlvmGenerationContext& context, GenScope* scope, bool foldStatics=false) const;
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};
