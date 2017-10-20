#pragma once

#include "ast_expr_node.hpp"
#include "ast_exprs.hpp"
#include "ast_lit.hpp"
#include "ast/type/ast_typeexpr_node.hpp"
#include "ast/type/ast_types.hpp"
#include "ast/ast_util.hpp"

#include "symbol/type_registry.hpp"


/** Parses a string / char literal, replacing escape sequences with the intended characters.
 * Skips the specified number of characters at the start and end of the source string. */
std::string parse_string_literal( const std::string& source, unsigned startOffset, unsigned endOffset  );


class TxStringLitNode : public TxExpressionNode {
    std::vector<uint8_t> utf8data;
    TxTypeExpressionNode* arrayTypeNode;

protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override {
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

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
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
        auto typeExpr = new TxQualTypeExprNode( new TxNamedTypeNode( this->ploc, "tx.MultiStringer") );
        this->stackConstr = new TxStackConstructionNode( this->ploc, typeExpr, &this->stringNodes );
    }
    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        return this->stackConstr->resolve_type( passInfo );
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

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->stackConstr->visit_ast( visitor, thisCursor, "constr", context );
    }
};


enum StringFormatFlags {
    SF_NONE = 0,
    SF_MINUS = 1,
    SF_PLUS = 2,
    SF_SPACE = 4,
    SF_ZERO = 8,
    SF_HASH = 16
};

class TxStringFormatNode : public TxExpressionNode {
    StringFormatFlags flags;
    const std::string width;
    const std::string precision;
    const char typeCh;
    TxStackConstructionNode* stackConstr = nullptr;

protected:
    virtual void declaration_pass() override {
        auto args = new std::vector<TxExpressionNode*>( {
            new TxIntegerLitNode( this->ploc, this->flags, false, TXBT_UBYTE ),
            new TxIntegerLitNode( this->ploc, this->width, false ),
            new TxIntegerLitNode( this->ploc, this->precision, false ),
            new TxIntegerLitNode( this->ploc, this->typeCh, false, TXBT_UBYTE ),
        } );
        auto typeExpr = new TxQualTypeExprNode( new TxNamedTypeNode( this->ploc, "tx.StringFormat") );
        this->stackConstr = new TxStackConstructionNode( this->ploc, typeExpr, args );
    }

    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        return this->stackConstr->resolve_type( passInfo );
    }

public:
    TxStringFormatNode( const TxLocation& ploc, StringFormatFlags flags, const std::string& width, const std::string& precision, const char typeCh )
        : TxExpressionNode( ploc ), flags( flags ), width( width ), precision( precision ), typeCh( typeCh ) {
    }

    virtual TxStringFormatNode* make_ast_copy() const override {
        return new TxStringFormatNode( this->ploc, flags, width, precision, typeCh );
    }

    virtual bool is_statically_constant() const override final {
        return false;
    }

    virtual TxFieldStorage get_storage() const {
        return TXS_STACK;
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->stackConstr->visit_ast( visitor, thisCursor, "constr", context );
    }
};



class TxCStringLitNode : public TxExpressionNode {
public:
    const std::string literal;
    const std::string value;

private:
    TxTypeExpressionNode* cstringTypeNode;  // implicit type definer


protected:
    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        return this->cstringTypeNode->resolve_type( passInfo );
    }

public:
    TxCStringLitNode( const TxLocation& ploc, const std::string& literal );

    virtual TxCStringLitNode* make_ast_copy() const override {
        return new TxCStringLitNode( this->ploc, this->literal );
    }

    virtual bool is_statically_constant() const override final {
        return true;
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->cstringTypeNode->visit_ast( visitor, thisCursor, "cstrtype", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->literal;
    }
};
