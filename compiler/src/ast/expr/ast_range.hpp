#pragma once

#include "ast_expr_node.hpp"
#include "ast_exprs.hpp"
#include "ast_lit.hpp"
#include "ast_conv.hpp"
#include "ast/type/ast_types.hpp"
#include "ast/ast_declpass.hpp"

class TxERangeLitNode final : public TxTypeDefiningValExprNode {
    TxNamedTypeNode* baseTypeNode = nullptr;
    TxStackConstructionNode* stackConstr = nullptr;
    TxExpressionNode* startValue;
    TxExpressionNode* endValue;
    TxExpressionNode* stepValue;
    bool inclusive;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        TxExpressionNode* limitTypeExpr;
        {
            auto ltype = this->startValue->resolve_type( typeResLevel );
            auto rtype = this->endValue->resolve_type( typeResLevel );
            if ( ltype == rtype ) {
                limitTypeExpr = this->startValue;
            }
            else {
                if ( auto_converts_to( this->endValue, ltype ) ) {
                    limitTypeExpr = this->startValue;
                }
                else if ( auto_converts_to( this->startValue, rtype ) ) {
                    limitTypeExpr = this->endValue;
                }
                else
                    CERR_THROWRES( this, "Invalid or mutually incompatible range limit types: " << ltype << "  and  " << rtype );
            }
        }

        auto binding = new TxTypeArgumentNode( new TxQualTypeExprNode( new TxTypeExprWrapperNode( limitTypeExpr ) ) );
        inserted_node( binding, this, "binding" );
        return this->registry().get_specialized_type( this, baseTypeNode, { binding }, false );
    }

public:
    TxERangeLitNode( const TxLocation& ploc, TxExpressionNode* startValue, TxExpressionNode* endValue,
                     TxExpressionNode* stepValue = nullptr, bool inclusive = false )
            : TxTypeDefiningValExprNode( ploc ), startValue( startValue ), endValue( endValue ), stepValue( stepValue ),
              inclusive( inclusive ) {
        this->baseTypeNode = new TxNamedTypeNode( this->ploc, "tx.ERange" );
        if ( this->inclusive && !this->stepValue )
            this->stepValue = new TxIntegerLitNode( endValue->ploc, 1, true, TXBT_LONG );
        if ( this->stepValue ) {
            auto inclNode = new TxBoolLitNode( endValue->ploc, this->inclusive );
            this->stackConstr = new TxStackConstructionNode( ploc, new TxQualTypeExprNode( new TxTypeExprWrapperNode( this )),
                                     new std::vector<TxExpressionNode*>( { this->startValue, this->endValue,
                                                                           this->stepValue, inclNode } ));
        }
        else
            this->stackConstr = new TxStackConstructionNode( ploc, new TxQualTypeExprNode( new TxTypeExprWrapperNode( this ) ),
                                     new std::vector<TxExpressionNode*>( { this->startValue, this->endValue } ) );
    }

    TxERangeLitNode* make_ast_copy() const override {
        return new TxERangeLitNode( this->ploc, this->startValue->make_ast_copy(), this->endValue->make_ast_copy(),
                                    ( this->stepValue ? this->stepValue->make_ast_copy() : nullptr ), this->inclusive );
    }

// Implementation note: Since we currently implement this by invoking the ERange constructor,
// it needs to be allocated in memory and thus isn't statically constant.

//    virtual bool is_statically_constant() const override final {
//        return ( this->startValue->is_statically_constant() && this->endValue->is_statically_constant()
//                 && ( this->stepValue == nullptr || this->stepValue->is_statically_constant() ) );
//    }

    TxFieldStorage get_storage() const override {
        return TXS_UNBOUND_STACK;
    }

    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->baseTypeNode->visit_ast( visitor, cursor, "basetype", aux );
        this->stackConstr->visit_ast( visitor, cursor, "litconstr", aux );
    }
};
