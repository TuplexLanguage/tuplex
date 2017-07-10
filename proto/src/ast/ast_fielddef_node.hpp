#pragma once

#include "ast_entitydefs.hpp"
#include "type/ast_typeexpr_node.hpp"
#include "expr/ast_expr_node.hpp"
#include "expr/ast_maybe_conv_node.hpp"

namespace llvm {
class Constant;
}

class TxFieldDefNode : public TxFieldDefiningNode {
protected:
    const TxFieldDeclaration* declaration = nullptr;
    mutable llvm::Constant* cachedConstantInitializer = nullptr;

    virtual const TxQualType* define_type() override;

    virtual const TxField* define_field() override;

public:
    const TxIdentifier* fieldName;
    const bool modifiable;  // true if field name explicitly declared modifiable
    TxTypeExpressionNode* typeExpression;
    TxMaybeConversionNode* initExpression;

    TxFieldDefNode( const TxLocation& ploc, const std::string& fieldName,
                    TxTypeExpressionNode* typeExpression,
                    TxExpressionNode* initExpression, bool modifiable = false )
            : TxFieldDefiningNode( ploc ), fieldName( new TxIdentifier( fieldName ) ), modifiable( modifiable ) {
        this->typeExpression = typeExpression;
        if ( initExpression ) {
            initExpression->set_field_def_node( this );
            this->initExpression = new TxMaybeConversionNode( initExpression );
        }
        else {
            ASSERT( typeExpression, "At least one of typeExpression and initExpression must be specified" );
            this->initExpression = nullptr;
        }
    }

    virtual TxFieldDefNode* make_ast_copy() const override = 0;

    /** Performs the declaration of the field defined by this node. To be run before declaration pass is run on this node. */
    inline void declare_field( TxScopeSymbol* scope, TxDeclarationFlags declFlags, TxFieldStorage storage ) {
        this->declare_field( this->fieldName->str(), scope, declFlags, storage );
    }

    /** Performs the declaration of the field defined by this node. To be run before declaration pass is run on this node. */
    inline void declare_field( const std::string& name, TxScopeSymbol* scope, TxDeclarationFlags declFlags, TxFieldStorage storage ) {
        this->declaration = scope->declare_field( name, this, declFlags, storage, TxIdentifier() );
    }

    virtual void symbol_resolution_pass() override;

    virtual TxExpressionNode* get_init_expression() const override {
        return this->initExpression;
    }

    const TxFieldDeclaration* get_declaration() const {
        ASSERT( this->declaration, "field declaration not initialized for " << this->fieldName );
        return this->declaration;
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->typeExpression )
            this->typeExpression->visit_ast( visitor, thisCursor, "type", context );
        if ( this->initExpression )
            this->initExpression->visit_ast( visitor, thisCursor, "initializer", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->fieldName->str();
    }

    /** Generates this field, potentially only as a declaration without initializer. Invoked from code referencing this field. */
    virtual llvm::Value* code_gen_field_decl( LlvmGenerationContext& context ) const = 0;

    /** Generates / retrieves the code generated constant value of this field's init expression,
     * if it has one and it is constant.
     * May be called multiple times, it caches the result to ensures the constant value is only generated once.
     * Only valid to call on nodes for which is_statically_constant() returns true. */
    llvm::Constant* code_gen_const_init_value( LlvmGenerationContext& context, bool genBody=false ) const;
};

class TxLocalFieldDefNode : public TxFieldDefNode {
public:
    TxLocalFieldDefNode( const TxLocation& ploc, const std::string& fieldName,
                      TxTypeExpressionNode* typeExpression,
                      TxExpressionNode* initExpression, bool modifiable = false )
            : TxFieldDefNode( ploc, fieldName, typeExpression, initExpression, modifiable ) {
    }

    virtual TxLocalFieldDefNode* make_ast_copy() const override {
        TxTypeExpressionNode* typeExpr = ( this->typeExpression ? this->typeExpression->make_ast_copy() : nullptr );
        TxExpressionNode* initExpr = ( this->initExpression ? this->initExpression->originalExpr->make_ast_copy() : nullptr );
        return new TxLocalFieldDefNode( this->ploc, this->fieldName->str(), typeExpr, initExpr, this->modifiable );
    }

    virtual llvm::Value* code_gen_field_decl( LlvmGenerationContext& context ) const override;

    void code_gen_field( LlvmGenerationContext& context, GenScope* scope ) const;
};

class TxNonLocalFieldDefNode : public TxFieldDefNode {
    void inner_code_gen_field( LlvmGenerationContext& context, bool genBody ) const;

public:
    TxNonLocalFieldDefNode( const TxLocation& ploc, const std::string& fieldName,
                      TxTypeExpressionNode* typeExpression,
                      TxExpressionNode* initExpression, bool modifiable = false )
            : TxFieldDefNode( ploc, fieldName, typeExpression, initExpression, modifiable ) {
    }

    virtual TxNonLocalFieldDefNode* make_ast_copy() const override {
        TxTypeExpressionNode* typeExpr = ( this->typeExpression ? this->typeExpression->make_ast_copy() : nullptr );
        TxExpressionNode* initExpr = ( this->initExpression ? this->initExpression->originalExpr->make_ast_copy() : nullptr );
        return new TxNonLocalFieldDefNode( this->ploc, this->fieldName->str(), typeExpr, initExpr, this->modifiable );
    }

    /** Generates this field, potentially only as a declaration without initializer. Invoked from code referencing this field. */
    virtual llvm::Value* code_gen_field_decl( LlvmGenerationContext& context ) const override;

    /** Fully generates this field, declaration and body. Invoked from it's AST parent. */
    void code_gen_field( LlvmGenerationContext& context ) const;
};
