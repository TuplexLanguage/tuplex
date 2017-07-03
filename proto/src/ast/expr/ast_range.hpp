#pragma once

#include "ast_expr_node.hpp"
#include "ast_exprs.hpp"
#include "ast_lit.hpp"
#include "ast_conv.hpp"
#include "ast/type/ast_types.hpp"
#include "ast/ast_declpass.hpp"

class TxERangeLitNode final : public TxExpressionNode {
    TxStackConstructionNode* stackConstr = nullptr;
    TxExpressionNode* startValue;
    TxExpressionNode* endValue;
    TxExpressionNode* stepValue;

protected:
    virtual void declaration_pass() override {
        if ( !this->stepValue )
            this->stepValue = new TxIntegerLitNode( endValue->ploc, 1, true, TXBT_LONG );
        this->stackConstr = new TxStackConstructionNode( ploc, new TxTypeExprWrapperNode( this ),
                                                         new std::vector<TxExpressionNode*>( { startValue, endValue, stepValue } ) );
    }

    virtual const TxQualType* define_type() override {
        TxExpressionNode* limitTypeExpr;
        {
            auto ltype = this->startValue->resolve_type()->type();
            auto rtype = this->endValue->resolve_type()->type();
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

        auto baseTypeNode = new TxNamedTypeNode( this->ploc, "tx.ERange" );
        run_declaration_pass( baseTypeNode, this, "basetype" );
        baseTypeNode->symbol_resolution_pass();
        auto baseType = baseTypeNode->resolve_type()->type();

        auto binding = new TxTypeTypeArgumentNode( new TxTypeExprWrapperNode( limitTypeExpr ) );
        run_declaration_pass( binding, this, "binding" );
        std::vector<const TxTypeArgumentNode*> bindings( { binding } );

        auto rangeType = this->registry().get_type_specialization( this, baseType, bindings, false );
        return new TxQualType( rangeType );
    }

public:
    TxERangeLitNode( const TxLocation& ploc, TxExpressionNode* startValue, TxExpressionNode* endValue,
                     TxExpressionNode* stepValue = nullptr )
            : TxExpressionNode( ploc ), startValue( startValue ), endValue( endValue ), stepValue( stepValue ) {
    }

    /** factory method that folds ( start .. ( step .. end ) ) grammar match into a single range node */
    static TxERangeLitNode* make_range_node( const TxLocation& ploc, TxExpressionNode* startValue, TxExpressionNode* endValue ) {
        if ( auto otherRange = dynamic_cast<TxERangeLitNode*>( endValue ) ) {
            if ( otherRange->stepValue ) {
                // error, too many subsequent .. operators
                CERROR( otherRange, "Too many subsequent .. operators in range expression" );
            }
            TxERangeLitNode tmp( ploc, startValue, otherRange->endValue, otherRange->startValue );
            memcpy( otherRange, &tmp, sizeof( TxERangeLitNode ) );
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

    virtual TxFieldStorage get_storage() const {
        return TXS_STACK;
    }

    virtual bool is_stack_allocation_expression() const override {
        return true;
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->stackConstr->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->stackConstr->visit_ast( visitor, thisCursor, "litconstr", context );
    }
};
