#pragma once

#include "ast_declbase.hpp"

#include "symbol/type_registry.hpp"


/** Wraps a TxTypeDefiningNode as an TxTypeExpressionNode.
 * The declaration and resolution pass calls won't be forwarded,
 * allowing the wrapped node to be added as a TxTypeExpressionNode child to additional parent nodes.
 * Only used for very special cases, currently only for $Self and $Super definitions.
 */
class TxTypeExprWrapperNode : public TxTypeExpressionNode {
    TxTypeExpressionNode* const typeExprNode;
protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override { }

    virtual const TxType* define_type() override {
        auto type = this->typeExprNode->resolve_type();
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
    TxTypeExprWrapperNode( TxTypeExpressionNode* typeExprNode )
        : TxTypeExpressionNode( typeExprNode->parseLocation ), typeExprNode(typeExprNode)  { }

    virtual TxTypeExprWrapperNode* make_ast_copy() const override {
        // since declaration and resolution passes aren't forwarded, the wrapped type definition doesn't need copying
        return new TxTypeExprWrapperNode( this->typeExprNode );
    }

    virtual std::string get_auto_type_name() const override {
        return this->typeExprNode->get_auto_type_name();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override {
        return this->typeExprNode->code_gen( context, scope );
    }

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        //this->typeExprNode->visit_ast( visitor, thisAsParent, "wrappedtypenode", context );
    }
};


/** Wraps a TxEntityDeclaration as a TxTypeExpressionNode. */
class TxTypeDeclWrapperNode : public TxTypeExpressionNode {
    TxEntityDeclaration const * const typeDecl;
protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override { }

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
    TxTypeDeclWrapperNode( const TxLocation& parseLocation, const TxEntityDeclaration* typeDecl )
        : TxTypeExpressionNode( parseLocation ), typeDecl( typeDecl )  { }

    virtual TxTypeDeclWrapperNode* make_ast_copy() const override {
        return new TxTypeDeclWrapperNode( this->parseLocation, this->typeDecl );
    }

    virtual std::string get_auto_type_name() const override {
        return hashify( this->typeDecl->get_unique_full_name() );
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }


    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {}
};
