#pragma once

#include "ast_declbase.hpp"

#include "symbol/type_registry.hpp"


/** Wraps a TxTypeDefiningNode as an TxTypeExpressionNode.
 * The declaration and resolution pass calls won't be forwarded,
 * allowing the wrapped node to be added as a TxTypeExpressionNode child to additional parent nodes.
 * Only used for very special cases, currently only for $Self and $Super definitions.
 */
class TxTypeExprWrapperNode : public TxTypeExpressionNode {
    TxTypeDefiningNode* const typeDefNode;
protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext,
                                                      TxDeclarationFlags declFlags ) override { }

    virtual const TxType* define_type() override {
        auto type = this->typeDefNode->resolve_type();
        if (!type)
            return nullptr;
        else if (auto declEnt = this->get_declaration()) {
            // if there is a declaration, create empty specialization (uniquely named but identical type)
            if (! type->is_modifiable())
                return this->types().get_empty_specialization(declEnt, type);
        }
        return type;
    }

public:
    TxTypeExprWrapperNode( TxTypeDefiningNode* typeDefNode )
        : TxTypeExpressionNode( typeDefNode->parseLocation ), typeDefNode(typeDefNode)  { }

    virtual TxTypeExprWrapperNode* make_ast_copy() const override {
        // since declaration and resolution passes aren't forwarded, the wrapped type definition doesn't need copying
        return new TxTypeExprWrapperNode( this->typeDefNode );
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override {
        return this->typeDefNode->code_gen( context, scope );
    }
};


/** Wraps a TxEntityDeclaration as a TxTypeExpressionNode. */
class TxTypeDeclWrapperNode : public TxTypeExpressionNode {
    TxEntityDeclaration const * const typeDecl;
protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext,
                                                      TxDeclarationFlags declFlags ) override { }

    virtual const TxType* define_type() override {
        auto type = this->typeDecl->get_definer()->resolve_type();
        if (!type)
            return nullptr;
        else if (auto declEnt = this->get_declaration()) {
            // if there is a declaration, create empty specialization (uniquely named but identical type)
            if (! type->is_modifiable())
                return this->types().get_empty_specialization(declEnt, type);
        }
        return type;
    }

public:
    TxTypeDeclWrapperNode( const TxEntityDeclaration* typeDecl )
        : TxTypeExpressionNode( typeDecl->get_definer()->get_parse_location() ), typeDecl( typeDecl )  { }

    virtual TxTypeDeclWrapperNode* make_ast_copy() const override {
        return new TxTypeDeclWrapperNode( this->typeDecl );
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
};


///** Wraps a TxType as a TxTypeDefiningNode. */
//class TxTypeWrapperNode : public TxTypeDefiningNode {
//    const TxType* type;
//
//protected:
//    virtual const TxType* define_type() override { return this->type; }
//
//public:
//    TxTypeWrapperNode(const TxType* type) : TxTypeDefiningNode( type->get_parse_location() ), type(type)  {
//        ASSERT(type, "NULL type"); ASSERT(type->get_declaration(), "NULL type declaration");
//        this->resolve_type();  // auto-resolves
//    }
//
//    virtual TxTypeDefiningNode* make_ast_copy() const override {
//        return new TxTypeWrapperNode( this->type );
//    }
//
//    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override {
//        return nullptr;
//    }
//};


///** Wraps a TxExpressionNode within another node which will not forward declaration and resolution pass calls.
// * This allows the wrapped node to be added as a child to additional parent nodes / specializations. */
//// FUTURE: Perhaps make a TxValueDefiner instead, analogous to TxTypeDefiner
//class TxExprWrapperNode : public TxExpressionNode {
//    TxExpressionNode* const expr;
//    TxSpecializationIndex const six;
//protected:
//    virtual const TxType* define_type() override {
//        return this->expr->resolve_type();
//    }
//
//public:
//    TxExprWrapperNode(TxExpressionNode* expr, TxSpecializationIndex six)
//        : TxExpressionNode(expr->parseLocation), expr(expr), six(six)  { }
//
//    inline TxExpressionNode* get_wrapped() const { return this->expr; }
//    inline TxSpecializationIndex get_six() const { return this->six; }
//
//    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
//        this->set_context( lexContext); }
//
//    virtual bool is_statically_constant() const { return this->expr->is_statically_constant(); }
//    virtual bool has_predefined_type() const override { return this->expr->has_predefined_type(); }
//    virtual const TxConstantProxy* get_static_constant_proxy() const override { return this->expr->get_static_constant_proxy(); }
//
//    virtual std::vector<const TxType*>* get_applied_func_arg_types(TxSpecializationIndex six) {
//        return this->expr->get_applied_func_arg_types(this->six); }
//    virtual void set_applied_func_arg_types(TxSpecializationIndex six, std::vector<const TxType*>* appliedFuncArgTypes) {
//        this->expr->set_applied_func_arg_types(this->six, appliedFuncArgTypes); }
//
//    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override {
//        return this->expr->code_gen(context, scope); }
//    virtual llvm::Value* code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const override {
//        return this->expr->code_gen_typeid(context, scope); }
//};
