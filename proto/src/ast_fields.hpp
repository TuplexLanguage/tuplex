#pragma once

#include "ast_base.hpp"


class TxFieldValueNode : public TxExpressionNode {
    const TxField* field = nullptr;
    const TxEntityDeclaration* declaration = nullptr;

    TxScopeSymbol* resolve_symbol();
    const TxEntityDeclaration* resolve_decl();

protected:
    virtual const TxType* define_type() override;

public:
    TxExpressionNode* baseExpr;
    const TxIdentifier* symbolName;

    /** Creates a new TxFieldValueNode.
     * @param base is the base expression (preceding expression adjoined with the '.' operator), or NULL if none
     * @param member is the specified literal field name
     */
    TxFieldValueNode( const TxLocation& parseLocation, TxExpressionNode* base, const std::string& memberName )
        : TxExpressionNode(parseLocation), baseExpr(base), symbolName( new TxIdentifier( memberName ) ) {
    }

    virtual TxFieldValueNode* make_ast_copy() const override {
        return new TxFieldValueNode( this->parseLocation, ( this->baseExpr ? this->baseExpr->make_ast_copy() : nullptr ), this->symbolName->str() );
    }

    /** Returns the full identifier (dot-separated full name) as specified in the program text,
     * up to and including this name. */
    TxIdentifier get_full_identifier() const {
        if (auto baseSymbolNode = dynamic_cast<TxFieldValueNode*>(this->baseExpr))
            return TxIdentifier(baseSymbolNode->get_full_identifier(), this->symbolName->str());
        else
            return *this->symbolName;
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        if (this->baseExpr)
            this->baseExpr->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        // not invoking baseExpr->symbol_resolution_pass() since that is only done via define_type()
        //if (this->baseExpr)
        //    this->baseExpr->symbol_resolution_pass(six);
        if (auto typeDecl = dynamic_cast<const TxTypeDeclaration*>(this->declaration))
            CERROR(this, "'" << get_full_identifier() << "' resolved to a type, not a field: " << typeDecl);
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        this->LOGGER().trace("Getting static constant proxy for field %s", this->symbolName->c_str());
        if (auto field = this->get_field())
            if (auto constProxy = field->get_static_constant_proxy()) {
                this->LOGGER().debug("Returning static constant proxy for field %s", field->get_symbol()->get_full_name().str().c_str());
                return constProxy;
            }
        return nullptr;
    }

    virtual bool is_statically_constant() const override {
        //std::cerr << "is_statically_constant() in " << this << std::endl;
        if (auto field = this->get_field())
            return field->is_statically_constant();
        return false;
    }

    // should not be called before symbol is resolved:
    inline const TxField* get_field() const { return this->field; }
    inline const TxFieldDeclaration* get_field_declaration() const {
        return dynamic_cast<const TxFieldDeclaration*>(this->declaration);
    }


    virtual llvm::Value* code_gen_address(LlvmGenerationContext& context, GenScope* scope, bool foldStatics=false) const;
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;


    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        if (this->baseExpr)
            this->baseExpr->visit_ast( visitor, thisAsParent, "base", context );
    }

    virtual const TxIdentifier* get_identifier() const override { return this->symbolName; }
};
