#pragma once

#include "ast_expr_node.hpp"
#include "ast/type/ast_typeexpr_node.hpp"

class TxStringLitNode : public TxExpressionNode {
    std::vector<uint8_t> utf8data;
    TxTypeExpressionNode* stringTypeNode;
    TxTypeExpressionNode* arrayTypeNode;

protected:
    virtual const TxType* define_type() override {
        return this->stringTypeNode->resolve_type();
    }

public:
    const std::string literal;

    TxStringLitNode( const TxLocation& parseLocation, const std::string& literal );

    virtual TxStringLitNode* make_ast_copy() const override {
        return new TxStringLitNode( this->parseLocation, this->literal );
    }

    virtual bool is_statically_constant() const override final {
        return true;
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->stringTypeNode->symbol_resolution_pass();
        this->arrayTypeNode->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->stringTypeNode->visit_ast( visitor, thisCursor, "strtype", context );
        this->arrayTypeNode->visit_ast( visitor, thisCursor, "strtype", context );
    }

    virtual std::string get_identifier() const override {
        return this->literal;
    }
};
