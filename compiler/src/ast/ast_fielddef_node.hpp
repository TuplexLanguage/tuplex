#pragma once

#include "ast_entitydefs.hpp"
#include "ast_identifier.hpp"
#include "type/ast_qualtypes.hpp"
#include "expr/ast_expr_node.hpp"
#include "expr/ast_maybe_conv_node.hpp"

namespace llvm {
class Constant;
}


/** Special concreteness and assignability checks since we allow assignment between arrays
 *  of different capacities (i.e. sizes).
 *  Used e.g. by local field definitions and assignment statements.
 */
void verify_array_assignment( const TxNode* origin, const TxActualType* ltype, const TxActualType* rtype );


class TxFieldDefiningNode : public TxEntityResolvingNode {
    const TxField* _field = nullptr;
    bool startedRslv = false;  // guard against recursive resolution
    bool hasResolved = false;  // to prevent multiple identical error messages

protected:
    const bool _explicit;
    const TxFieldDeclaration* declaration = nullptr;
    mutable llvm::Constant* cachedConstantInitializer = nullptr;

    virtual TxQualType define_type( TxTypeResLevel typeResLevel );

    virtual const TxField* define_field();

    virtual void resolution_pass() override {
        this->resolve_field();
    }

    virtual void verification_pass() const override;

    TxFieldDefiningNode( const TxLocation& ploc, TxIdentifierNode* fieldName,
                    TxTypeExpressionNode* typeExpression, TxExpressionNode* initExpression, bool modifiable, bool _explicit )
            : TxEntityResolvingNode( ploc ), _explicit( _explicit ), fieldName( fieldName ), modifiable( modifiable ),
              typeExpression( typeExpression ) {
//        if ( typeExpression ) {
//            typeExpression->set_requires_mutable( modifiable );
//        }
        if ( initExpression ) {
            initExpression->set_field_def_node( this );
            this->initExpression = new TxMaybeConversionNode( initExpression );
        }
        else {
            ASSERT( typeExpression, "At least one of typeExpression and initExpression must be specified" );
            this->initExpression = nullptr;
        }
    }

public:
    TxIdentifierNode* fieldName;
    const bool modifiable;  // true if field name explicitly declared modifiable
    TxTypeExpressionNode* typeExpression;
    TxMaybeConversionNode* initExpression;

    virtual TxFieldDefiningNode* make_ast_copy() const override = 0;

    /** Resolves the type and returns the field entity of this field-defining node.
     * @return a valid field pointer (exception is thrown upon failure) */
    virtual const TxField* resolve_field() final;

    virtual const TxField* attempt_field() const final {
        return this->_field;
    }

    virtual const TxField* field() const final {
        ASSERT( this->_field, "field definer not resolved: " << this );
        return this->_field;
    }

    /** Returns the type (as specific as can be known) of the value this field-defining node produces/uses.
     * @return a valid type pointer (exception is thrown upon failure) */
    virtual TxQualType resolve_type( TxTypeResLevel typeResLevel ) override final {
        this->resolve_field();
        return this->_type;
    }

    /** Returns true if this field has a constant initialization expression that can be evaluated at compile time. */
    virtual bool is_statically_constant() const;


    /** Performs the declaration of the field defined by this node. To be run before declaration pass is run on this node. */
    virtual void declare_field( const TxNode* declarer, TxScopeSymbol* scope, TxDeclarationFlags declFlags, TxFieldStorage storage ) = 0;

    virtual TxExpressionNode* get_init_expression() const {
        return this->initExpression;
    }

    const TxFieldDeclaration* get_declaration() const {
        ASSERT( this->declaration, "field declaration not initialized for " << this->fieldName );
        return this->declaration;
    }

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->typeExpression )
            this->typeExpression->visit_ast( visitor, thisCursor, "type", context );
        if ( this->initExpression )
            this->initExpression->visit_ast( visitor, thisCursor, "initializer", context );
    }

    virtual const std::string& get_descriptor() const override {
        return this->fieldName->get_descriptor();
    }

    /** Generates this field, potentially only as a declaration without initializer. Invoked from code referencing this field. */
    virtual llvm::Value* code_gen_field_decl( LlvmGenerationContext& context ) const = 0;

    /** Generates / retrieves the code generated constant value of this field's init expression,
     * if it has one and it is constant.
     * May be called multiple times, it caches the result to ensures the constant value is only generated once.
     * Only valid to call on nodes for which is_statically_constant() returns true. */
    llvm::Constant* code_gen_const_init_value( LlvmGenerationContext& context, bool genBody=false ) const;
};

class TxLocalFieldDefNode : public TxFieldDefiningNode {
    TxLocalFieldDefNode( const TxLocation& ploc, TxIdentifierNode* fieldName,
                         TxTypeExpressionNode* typeExpression, TxExpressionNode* initExpression, bool modifiable, bool _explicit )
            : TxFieldDefiningNode( ploc, fieldName, typeExpression, initExpression, modifiable, _explicit ) {
    }

public:
    TxLocalFieldDefNode( const TxLocation& ploc, TxIdentifierNode* fieldName,
                         TxTypeExpressionNode* typeExpression, TxExpressionNode* initExpression, bool _explicit = false )
            : TxLocalFieldDefNode( ploc, fieldName, typeExpression, initExpression, false, _explicit ) {
    }

    TxLocalFieldDefNode( const TxLocation& ploc, TxIdentifierNode* fieldName,
                         TxExpressionNode* initExpression, bool modifiable )
        : TxLocalFieldDefNode( ploc, fieldName, nullptr, initExpression, modifiable, false ) {
    }

    virtual TxLocalFieldDefNode* make_ast_copy() const override {
        TxTypeExpressionNode* typeExpr = ( this->typeExpression ? this->typeExpression->make_ast_copy() : nullptr );
        TxExpressionNode* initExpr = ( this->initExpression ? this->initExpression->originalExpr->make_ast_copy() : nullptr );
        return new TxLocalFieldDefNode( this->ploc, this->fieldName->make_ast_copy(), typeExpr, initExpr, this->modifiable, this->_explicit );
    }

    virtual void declare_field( const TxNode* declarer, TxScopeSymbol* scope, TxDeclarationFlags declFlags, TxFieldStorage storage ) override;

    virtual llvm::Value* code_gen_field_decl( LlvmGenerationContext& context ) const override;

    void code_gen_field( LlvmGenerationContext& context, GenScope* scope ) const;
};

class TxNonLocalFieldDefNode : public TxFieldDefiningNode {
    void inner_code_gen_field( LlvmGenerationContext& context, bool genBody ) const;

    TxNonLocalFieldDefNode( const TxLocation& ploc, TxIdentifierNode* fieldName,
                            TxTypeExpressionNode* typeExpression, TxExpressionNode* initExpression, bool modifiable )
            : TxFieldDefiningNode( ploc, fieldName, typeExpression, initExpression, modifiable, false ) {
    }

public:
    TxNonLocalFieldDefNode( const TxLocation& ploc, TxIdentifierNode* fieldName,
                            TxTypeExpressionNode* typeExpression, TxExpressionNode* initExpression )
        : TxNonLocalFieldDefNode( ploc, fieldName, typeExpression, initExpression, false ) {
    }

    TxNonLocalFieldDefNode( const TxLocation& ploc, TxIdentifierNode* fieldName,
                            TxExpressionNode* initExpression, bool modifiable )
        : TxNonLocalFieldDefNode( ploc, fieldName, nullptr, initExpression, modifiable ) {
    }

    virtual TxNonLocalFieldDefNode* make_ast_copy() const override {
        TxTypeExpressionNode* typeExpr = ( this->typeExpression ? this->typeExpression->make_ast_copy() : nullptr );
        TxExpressionNode* initExpr = ( this->initExpression ? this->initExpression->originalExpr->make_ast_copy() : nullptr );
        return new TxNonLocalFieldDefNode( this->ploc, this->fieldName->make_ast_copy(), typeExpr, initExpr, this->modifiable );
    }

    virtual void declare_field( const TxNode* declarer, TxScopeSymbol* scope, TxDeclarationFlags declFlags, TxFieldStorage storage ) override;

    /** Generates this field, potentially only as a declaration without initializer. Invoked from code referencing this field. */
    virtual llvm::Value* code_gen_field_decl( LlvmGenerationContext& context ) const override;

    /** Fully generates this field, declaration and body. Invoked from it's AST parent. */
    void code_gen_field( LlvmGenerationContext& context ) const;
};
