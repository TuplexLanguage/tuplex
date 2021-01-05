#pragma once

#include "ast/ast_entitydefs.hpp"

/** Describes an arg name and type - however does not declare or define a field entity.
 * This is used for function arguments and return values, they are not distinct declarations / entities,
 * rather they are part of the function type definition.
 */
class TxArgTypeDefNode : public TxTypeResolvingNode {
protected:
    virtual TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        LOG_TRACE( this->LOGGER(), "defining  type  of " << this );
        return this->typeExpression->resolve_type( typeResLevel );
    }

//    virtual void resolution_pass() override {
//        this->resolve_type( typeResLevel );
//    }

public:
    TxIdentifierNode* fieldName;
    TxTypeExpressionNode* typeExpression;

    TxArgTypeDefNode( const TxLocation& ploc, TxIdentifierNode* fieldName, TxTypeExpressionNode* typeExpression )
            : TxTypeResolvingNode( ploc ), fieldName( fieldName ), typeExpression( typeExpression ) {
        ASSERT( typeExpression, "typeExpression must be specified" );
    }
    TxArgTypeDefNode( const TxLocation& ploc, const std::string& fieldName, TxTypeExpressionNode* typeExpression )
            : TxArgTypeDefNode( ploc, new TxIdentifierNode( ploc, fieldName ), typeExpression ) {
    }

    virtual TxArgTypeDefNode* make_ast_copy() const override {
        return new TxArgTypeDefNode( this->ploc, this->fieldName->make_ast_copy(), this->typeExpression->make_ast_copy() );
    }

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->typeExpression->visit_ast( visitor, cursor, "type", aux );
    }

    virtual const std::string& get_descriptor() const override {
        return this->fieldName->get_descriptor();
    }
};
