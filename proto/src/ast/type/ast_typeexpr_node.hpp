#pragma once

#include "ast/ast_entitydefs.hpp"

/**
 * The context of this node refers to its outer scope. This node's entity, if any, refers to its inner scope.
 */
class TxTypeExpressionNode : public TxTypeDefiningNode {
    const TxTypeDeclaration* declaration = nullptr;

protected:
    /** Returns true if this type expression requires the produced type to be mutable. Used by subclasses upon type creation. */
    bool requires_mutable_type() const;

    bool get_decl_interface_kw() const;

    virtual void declaration_pass() override final;

    /** Performs declaration pass operations on this type expression node. To be overridden by subclasses as necessary. */
    virtual void typeexpr_declaration_pass() { }

public:
    TxTypeExpressionNode( const TxLocation& parseLocation )
            : TxTypeDefiningNode( parseLocation ) {
    }

    virtual TxTypeExpressionNode* make_ast_copy() const override = 0;

    /** Gets the type declaration of this type expression, if any. */
    inline const TxTypeDeclaration* get_declaration() const {
        return this->declaration;
    }

    inline void set_declaration( const TxTypeDeclaration* declaration ) {  // FUTURE: refactor & remove
        this->declaration = declaration;
    }

    virtual void symbol_resolution_pass() {
        this->resolve_type();
    }

    /** Performs the code generation pass for this type expression and its sub-expressions.
     * This propagates code generation to type members.
     * Note that the LLVM types are generated by the TxActualType instances, not the AST.
     */
    virtual void code_gen_type( LlvmGenerationContext& context ) const = 0;
};
