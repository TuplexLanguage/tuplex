#pragma once

#include "ast_expr_node.hpp"
#include "ast_exprs.hpp"
#include "ast_lit.hpp"
#include "ast/type/ast_types.hpp"
#include "ast/ast_declpass.hpp"

class TxERangeLitNode : public TxExpressionNode {
    TxStackConstructionNode* stackConstr;
//    TxExpressionNode* startValue;
//    TxMaybeConversionNode* endValue;
//    TxMaybeConversionNode* stepValue;

    TxERangeLitNode( const TxLocation& parseLocation, TxStackConstructionNode* stackConstr )
            : TxExpressionNode( parseLocation ), stackConstr( stackConstr ) {
    }

protected:
    virtual const TxType* define_type() override {
        auto startValue = this->stackConstr->constructorCall->origArgsExprList->front();
//        auto elemType = startValue->resolve_type();
//        this->endValue->insert_conversion( elemType );
//        if ( this->stepValue )
//            this->stepValue->insert_conversion( this->registry().get_builtin_type( TXBT_LONG ) );

        auto baseTypeNode = new TxNamedTypeNode( this->parseLocation, "tx.ERange" );
        run_declaration_pass( baseTypeNode, this, "basetype" );
        baseTypeNode->symbol_resolution_pass();
        auto baseType = baseTypeNode->resolve_type();

        auto binding = new TxTypeTypeArgumentNode( new TxTypeExprWrapperNode( startValue ) );
        run_declaration_pass( binding, this, "binding" );
        std::vector<const TxTypeArgumentNode*> bindings( { binding } );

        auto rangeType = this->registry().get_type_specialization( this, baseType, bindings, false );
        return rangeType;
    }

public:
    TxERangeLitNode( const TxLocation& parseLocation, TxExpressionNode* startValue, TxExpressionNode* endValue, TxExpressionNode* stepValue )
            : TxERangeLitNode( parseLocation,
                               new TxStackConstructionNode( this->parseLocation, new TxTypeExprWrapperNode( this ),
                                                            new std::vector<TxExpressionNode*>( { startValue, endValue, stepValue } ) ) ) {
    }
//              startValue( startValue ), endValue( endValue ), stepValue( stepValue )  { }
//              stepValue( stepValue ? new TxMaybeConversionNode( stepValue ) : nullptr )  { }

    TxERangeLitNode( const TxLocation& parseLocation, TxExpressionNode* startValue, TxExpressionNode* endValue )
            : TxERangeLitNode( parseLocation, startValue, endValue, new TxIntegerLitNode( endValue->parseLocation, 1, true, TXBT_LONG ) ) {
    }

    virtual TxERangeLitNode* make_ast_copy() const override {
        return new TxERangeLitNode( this->parseLocation, stackConstr->make_ast_copy() );
    }

//    virtual bool is_statically_constant() const override final {
//        return ( this->startValue->is_statically_constant() && this->endValue->is_statically_constant()
//                 && ( this->stepValue == nullptr || this->stepValue->is_statically_constant() ) );
//    }

    virtual bool is_stack_allocation_expression() const override {
        return true;
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->stackConstr->visit_ast( visitor, thisCursor, "constr", context );
//        this->startValue->visit_ast( visitor, thisCursor, "start", context );
//        this->endValue->visit_ast( visitor, thisCursor, "end", context );
//        if (this->stepValue)
//            this->stepValue->visit_ast( visitor, thisCursor, "step", context );
    }
};
