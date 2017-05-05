#pragma once

#include "ast_declbase.hpp"

#include "symbol/type_registry.hpp"


/** Wraps a TxTypeDefiningNode as an TxTypeExpressionNode.
 * The declaration and resolution pass calls won't be forwarded,
 * allowing the wrapped node to be added as a TxTypeExpressionNode child to additional parent nodes.
 * Only used for very special cases, currently only for $Self and $Super definitions.
 */
class TxTypeExprWrapperNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& lexContext ) override { }

    virtual const TxType* define_type() override {
        return this->typeDefNode->resolve_type();
    }

public:
    TxTypeDefiningNode* const typeDefNode;

    TxTypeExprWrapperNode( TxTypeDefiningNode* typeExprNode )
        : TxTypeExpressionNode( typeExprNode->parseLocation ), typeDefNode(typeExprNode)  { }

    virtual TxTypeExprWrapperNode* make_ast_copy() const override {
        // since declaration and resolution passes aren't forwarded, the wrapped type definition doesn't need copying
        return new TxTypeExprWrapperNode( this->typeDefNode );
    }

    virtual std::string get_auto_type_name() const override {
        if (auto typeExpr = dynamic_cast<TxTypeExpressionNode*>( this->typeDefNode ))
            return typeExpr->get_auto_type_name();
        else
            // attempt to resolve type here - experimental, but perhaps avoids type resolution recursion since only a value expression
            if (auto decl = this->typeDefNode->resolve_type()->get_declaration())
                return decl->get_unique_full_name();
            else {
                LOG(this->LOGGER(), WARN, "couldn't determine an auto-name for node: " << this->typeDefNode);
                return "?";
            }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override {
        return this->typeDefNode->code_gen( context, scope );
    }

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        // Traversal does not proceed to the wrapped node from here since it should be visited via its original AST location.
    }
};


/** Wraps a TxEntityDeclaration as a TxTypeExpressionNode.
 * If this wrapper is used to declare a type name, that name will effectively be a type alias. */
class TxTypeDeclWrapperNode : public TxTypeExpressionNode {
    TxEntityDeclaration const * const typeDecl;
protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& lexContext ) override { }

    virtual const TxType* define_type() override {
        return this->typeDecl->get_definer()->resolve_type();
    }

public:
    TxTypeDeclWrapperNode( const TxLocation& parseLocation, const TxEntityDeclaration* typeDecl )
        : TxTypeExpressionNode( parseLocation ), typeDecl( typeDecl )  { }

    virtual TxTypeDeclWrapperNode* make_ast_copy() const override {
        return new TxTypeDeclWrapperNode( this->parseLocation, this->typeDecl );
    }

    virtual std::string get_auto_type_name() const override {
        return this->typeDecl->get_unique_full_name();
    }

    const TxEntityDeclaration* get_wrapped_declaration() const {
        return this->typeDecl;
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }


    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {}
};
