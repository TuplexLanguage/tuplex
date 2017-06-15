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

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->stringTypeNode->visit_ast( visitor, thisCursor, "strtype", context );
        this->arrayTypeNode->visit_ast( visitor, thisCursor, "strtype", context );
    }

    virtual std::string get_identifier() const override {
        return this->literal;
    }
};


class TxCStringLitNode : public TxExpressionNode {
    const size_t arrayCapacity;  // note: array capacity includes the null terminator
    TxTypeExpressionNode* cstringTypeNode;  // implicit type definer

    static TxTypeExpressionNode* make_cstring_type_expr( const TxLocation& parseLocation, const std::string& literal );

protected:
    virtual const TxType* define_type() override {
        return this->cstringTypeNode->resolve_type();
    }

public:
    const std::string literal;
    const std::string value;

    TxCStringLitNode( const TxLocation& parseLocation, const std::string& literal )
            : TxExpressionNode( parseLocation ), arrayCapacity( literal.length() - 2 ),
              cstringTypeNode( make_cstring_type_expr( parseLocation, literal ) ),
              literal( literal ), value( literal, 2, literal.length() - 3 ) {
    }
    // TODO: properly parse string literal

    virtual TxCStringLitNode* make_ast_copy() const override {
        return new TxCStringLitNode( this->parseLocation, this->literal );
    }

    virtual bool is_statically_constant() const override final {
        return true;
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->cstringTypeNode->visit_ast( visitor, thisCursor, "cstrtype", context );
    }

    virtual std::string get_identifier() const override {
        return this->literal;
    }
};
