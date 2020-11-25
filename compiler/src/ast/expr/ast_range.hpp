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
    bool _implicitStep = false;

protected:
//    virtual void declaration_pass() override {
//        if ( !this->stepValue )
//            this->stepValue = new TxIntegerLitNode( endValue->ploc, 1, true, TXBT_LONG );
//        this->stackConstr = new TxStackConstructionNode( ploc, new TxTypeExprWrapperNode( this ),
//                                                         new std::vector<TxExpressionNode*>( { startValue, endValue, stepValue } ) );
//    }

    virtual TxQualType define_type( TxPassInfo passInfo ) override {
        TxExpressionNode* limitTypeExpr;
        {
            auto ltype = this->startValue->resolve_type( passInfo );
            auto rtype = this->endValue->resolve_type( passInfo );
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

//        auto baseTypeNode = new TxNamedTypeNode( this->ploc, "tx.ERange" );
//        run_declaration_pass( baseTypeNode, this, "basetype" );
//        baseTypeNode->resolution_pass();
//        auto baseType = baseTypeNode->resolve_type( passInfo );

        auto binding = new TxTypeArgumentNode( new TxQualTypeExprNode( new TxTypeExprWrapperNode( limitTypeExpr ) ) );
        run_declaration_pass( binding, this, "binding" );
        return this->registry().instantiate_type( this, baseTypeNode, { binding }, false );
    }

public:
    TxERangeLitNode( const TxLocation& ploc, TxExpressionNode* startValue, TxExpressionNode* endValue,
                     TxExpressionNode* stepValue = nullptr )
            : TxTypeDefiningValExprNode( ploc ), startValue( startValue ), endValue( endValue ), stepValue( stepValue ) {
        this->baseTypeNode = new TxNamedTypeNode( this->ploc, "tx.ERange" );
        if ( !this->stepValue ) {
            this->stepValue = new TxIntegerLitNode( endValue->ploc, 1, true, TXBT_LONG );
            this->_implicitStep = true;
        }
        this->stackConstr = new TxStackConstructionNode( ploc, new TxQualTypeExprNode( new TxTypeExprWrapperNode( this ) ),
                                 new std::vector<TxExpressionNode*>( { this->startValue, this->endValue, this->stepValue } ) );
    }

    /** factory method that folds ( start .. ( step .. end ) ) grammar match into a single range node */
    static TxERangeLitNode* make_range_node( const TxLocation& ploc, TxExpressionNode* startValue, TxExpressionNode* endValue ) {
        if ( auto otherRange = dynamic_cast<TxERangeLitNode*>( endValue ) ) {
            if ( !otherRange->_implicitStep ) {
                // error, too many subsequent .. operators
                CERROR( otherRange, "Too many subsequent .. operators in range expression" );
            }
            new(otherRange) TxERangeLitNode( ploc, startValue, otherRange->endValue, otherRange->startValue );
//            TxERangeLitNode tmp( ploc, startValue, otherRange->endValue, otherRange->startValue );
//            memcpy( otherRange, &tmp, sizeof( TxERangeLitNode ) );
            return otherRange;
        }
        else {
            return new TxERangeLitNode( ploc, startValue, endValue );
        }
    }

    virtual TxERangeLitNode* make_ast_copy() const override {
        return new TxERangeLitNode( this->ploc, this->startValue->make_ast_copy(), this->endValue->make_ast_copy(),
                                    this->stepValue->make_ast_copy() );
    }

// Implementation note: Since we currently implement this by invoking the ERange constructor,
// it needs to be allocated in memory and thus isn't statically constant.

//    virtual bool is_statically_constant() const override final {
//        return ( this->startValue->is_statically_constant() && this->endValue->is_statically_constant()
//                 && ( this->stepValue == nullptr || this->stepValue->is_statically_constant() ) );
//    }

    virtual TxFieldStorage get_storage() const override {
        return TXS_UNBOUND_STACK;
    }

    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->baseTypeNode->visit_ast( visitor, thisCursor, "basetype", context );
        this->stackConstr->visit_ast( visitor, thisCursor, "litconstr", context );
    }
};
