#pragma once

#include "ast/ast_entitydefs.hpp"

/** Describes an arg name and type - however does not declare or define a field entity.
 * This is used for function arguments and return values, they are not distinct declarations / entities,
 * rather they are part of the function type definition.
 */
class TxArgTypeDefNode : public TxTypeDefiningNode {
protected:
    virtual const TxType* define_type() override {
        LOG_TRACE( this->LOGGER(), "defining  type  of " << this );
        return this->typeExpression->resolve_type();
    }

public:
    const std::string fieldName;
    TxTypeExpressionNode* typeExpression;

    TxArgTypeDefNode( const TxLocation& parseLocation, const std::string& fieldName, TxTypeExpressionNode* typeExpression )
            : TxTypeDefiningNode( parseLocation ), fieldName( fieldName ), typeExpression( typeExpression ) {
        ASSERT( typeExpression, "typeExpression must be specified" );
    }

    virtual TxArgTypeDefNode* make_ast_copy() const override {
        return new TxArgTypeDefNode( this->parseLocation, this->fieldName, this->typeExpression->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() {
        this->resolve_type();
        this->typeExpression->symbol_resolution_pass();
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->typeExpression->visit_ast( visitor, thisCursor, "type", context );
    }

    virtual std::string get_identifier() const override {
        return std::string( this->fieldName );
    }
};
