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
    TxTypeExpressionNode* stringTypeNode;
    TxTypeExpressionNode* arrayTypeNode;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return this->stringTypeNode->resolve_type( typeResLevel );
    }

public:
    const std::string literal;

    TxStringLitNode( const TxLocation& ploc, const std::string& literal );

    TxStringLitNode* make_ast_copy() const override {
        return new TxStringLitNode( this->ploc, this->literal );
    }

    bool is_statically_constant() const final {
        return true;
    }

    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->stringTypeNode->visit_ast( visitor, cursor, "string-type", aux );
        this->arrayTypeNode->visit_ast( visitor, cursor, "byte-array-type", aux );
    }

    const std::string& get_descriptor() const override {
        return this->literal;
    }
};


class TxConcatenateStringsNode : public TxExpressionNode {
    TxStackConstructionNode* stackConstr = nullptr;
    std::vector<TxExpressionNode*> stringNodes;

protected:
    void declaration_pass() override {
        auto typeExpr = new TxQualTypeExprNode( new TxNamedTypeNode( this->ploc, "tx.MultiStringer") );
        this->stackConstr = new TxStackConstructionNode( this->ploc, typeExpr, &this->stringNodes );
    }
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return this->stackConstr->resolve_type( typeResLevel );
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

    TxConcatenateStringsNode* make_ast_copy() const override {
        return new TxConcatenateStringsNode( this->ploc, make_node_vec_copy( this->stringNodes ) );
    }

    bool is_statically_constant() const final {
        return false;
    }

    TxFieldStorage get_storage() const override {
        return TXS_UNBOUND_STACK;
    }

    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->stackConstr->visit_ast( visitor, cursor, "constr", aux );
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
    void declaration_pass() override {
        auto args = new std::vector<TxExpressionNode*>( {
            new TxIntegerLitNode( this->ploc, this->flags, false, TXBT_UBYTE ),
            new TxIntegerLitNode( this->ploc, this->width, false ),
            new TxIntegerLitNode( this->ploc, this->precision, false ),
            new TxIntegerLitNode( this->ploc, this->typeCh, false, TXBT_UBYTE ),
        } );
        auto typeExpr = new TxQualTypeExprNode( new TxNamedTypeNode( this->ploc, "tx.StringFormat") );
        this->stackConstr = new TxStackConstructionNode( this->ploc, typeExpr, args );
    }

    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return this->stackConstr->resolve_type( typeResLevel );
    }

public:
    TxStringFormatNode( const TxLocation& ploc, StringFormatFlags flags,
                        const std::string& width, const std::string& precision, const char typeCh )
        : TxExpressionNode( ploc ), flags( flags ), width( width ), precision( precision ), typeCh( typeCh ) {
    }

    TxStringFormatNode* make_ast_copy() const override {
        return new TxStringFormatNode( this->ploc, flags, width, precision, typeCh );
    }

    bool is_statically_constant() const final {
        return false;
    }

    TxFieldStorage get_storage() const override {
        return TXS_UNBOUND_STACK;
    }

    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->stackConstr->visit_ast( visitor, cursor, "constr", aux );
    }
};



class TxCStringLitNode : public TxExpressionNode {
public:
    const std::string literal;
    const std::string value;

private:
    TxTypeExpressionNode* cstringTypeNode;  // implicit type definer


protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return this->cstringTypeNode->resolve_type( typeResLevel );
    }

public:
    TxCStringLitNode( const TxLocation& ploc, const std::string& literal );

    TxCStringLitNode* make_ast_copy() const override {
        return new TxCStringLitNode( this->ploc, this->literal );
    }

    bool is_statically_constant() const final {
        return true;
    }

    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->cstringTypeNode->visit_ast( visitor, cursor, "cstrtype", aux );
    }

    const std::string& get_descriptor() const override {
        return this->literal;
    }
};
