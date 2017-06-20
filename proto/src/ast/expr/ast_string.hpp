#pragma once

#include "ast_expr_node.hpp"
#include "ast_exprs.hpp"
#include "ast/type/ast_typeexpr_node.hpp"
#include "ast/ast_util.hpp"

#include "symbol/type_registry.hpp"


class TxStringLitNode : public TxExpressionNode {
    std::vector<uint8_t> utf8data;
    TxTypeExpressionNode* arrayTypeNode;

protected:
    virtual const TxType* define_type() override {
        return this->registry().get_string_type();
    }

public:
    const std::string literal;

    TxStringLitNode( const TxLocation& ploc, const std::string& literal );

    virtual TxStringLitNode* make_ast_copy() const override {
        return new TxStringLitNode( this->ploc, this->literal );
    }

    virtual bool is_statically_constant() const override final {
        return true;
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->arrayTypeNode->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->arrayTypeNode->visit_ast( visitor, thisCursor, "strtype", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->literal;
    }
};


class TxConcatenateStringsNode : public TxExpressionNode {
    TxStackConstructionNode* stackConstr = nullptr;
    std::vector<TxExpressionNode*> stringNodes;

protected:
    virtual void declaration_pass() override {
        this->stackConstr = new TxStackConstructionNode( this->ploc, new TxTypeExprWrapperNode( this ),
                                                         &this->stringNodes );
    }
    virtual const TxType* define_type() override {
        return this->registry().get_string_type();
    }

public:
    TxConcatenateStringsNode( const TxLocation& ploc, const std::vector<TxExpressionNode*>& stringNodes  );

    static TxConcatenateStringsNode* make_strcat_node( const TxLocation& ploc, TxExpressionNode* stringA, TxExpressionNode* stringB ) {
        if ( auto strCatNode = dynamic_cast<TxConcatenateStringsNode*>( stringA ) ) {
            strCatNode->stringNodes.push_back( stringB );
            return strCatNode;
        }
        return new TxConcatenateStringsNode( ploc, { stringA, stringB } );
    }

    virtual TxConcatenateStringsNode* make_ast_copy() const override {
        return new TxConcatenateStringsNode( this->ploc, make_node_vec_copy( this->stringNodes ) );
    }

    virtual bool is_statically_constant() const override final {
        return false;
    }

    virtual TxFieldStorage get_storage() const {
        return TXS_STACK;
    }

    virtual bool is_stack_allocation_expression() const override {
        return true;
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
//        for ( auto node : stringNodes )
//            node->symbol_resolution_pass();
        this->stackConstr->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
//        for ( auto node : stringNodes )
//            node->visit_ast( visitor, thisCursor, "string", context );
        this->stackConstr->visit_ast( visitor, thisCursor, "constr", context );
    }
};


class TxCStringLitNode : public TxExpressionNode {
    const size_t arrayCapacity;  // note: array capacity includes the null terminator
    TxTypeExpressionNode* cstringTypeNode;  // implicit type definer

    static TxTypeExpressionNode* make_cstring_type_expr( const TxLocation& ploc, const std::string& literal );

protected:
    virtual const TxType* define_type() override {
        return this->cstringTypeNode->resolve_type();
    }

public:
    const std::string literal;
    const std::string value;

    TxCStringLitNode( const TxLocation& ploc, const std::string& literal )
            : TxExpressionNode( ploc ), arrayCapacity( literal.length() - 2 ),
              cstringTypeNode( make_cstring_type_expr( ploc, literal ) ),
              literal( literal ), value( literal, 2, literal.length() - 3 ) {
    }
    // TODO: properly parse string literal

    virtual TxCStringLitNode* make_ast_copy() const override {
        return new TxCStringLitNode( this->ploc, this->literal );
    }

    virtual bool is_statically_constant() const override final {
        return true;
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->cstringTypeNode->visit_ast( visitor, thisCursor, "cstrtype", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->literal;
    }
};
