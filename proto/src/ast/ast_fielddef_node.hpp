#pragma once

#include "ast_entitydefs.hpp"
#include "type/ast_typeexpr_node.hpp"
#include "expr/ast_expr_node.hpp"
#include "expr/ast_maybe_conv_node.hpp"

namespace llvm {
class Constant;
}

class TxFieldDefNode : public TxFieldDefiningNode {

    static bool validateFieldName( TxNode* node, const std::string& name ) {
    // TODO
    //    if (! islower( name.at(0) ))
    //        CWARNING(node, "The first letter of field names should be lowercase: " << name);
        if ( name.empty() ) {
            CERROR( node, "Name string is empty." );
            return false;
        }
        return true;
    }

protected:
    const TxFieldDeclaration* declaration = nullptr;
    mutable llvm::Constant* cachedConstantInitializer = nullptr;

    llvm::Value* make_constant_nonlocal_field( LlvmGenerationContext& context, llvm::Constant* constantInitializer ) const;

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
        validateFieldName( this, fieldName );
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

    /** Generates / retrieves the code generated constant value of this field's init expression,
     * if it has one and it is constant.
     * May be called multiple times, it caches the result to ensures the constant value is only generated once.
     * Only valid to call on nodes for which is_statically_constant() returns true. */
    llvm::Constant* code_gen_const_init_value( LlvmGenerationContext& context ) const;
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

    void code_gen_field( LlvmGenerationContext& context, GenScope* scope ) const;
};

class TxNonLocalFieldDefNode : public TxFieldDefNode {
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

    void code_gen_field( LlvmGenerationContext& context ) const;
};
