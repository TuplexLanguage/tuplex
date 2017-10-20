#pragma once

#include "ast/ast_entitydefs.hpp"
#include "symbol/qual_type.hpp"
#include "symbol/type_base.hpp"

class TxAssigneeNode : public TxTypeResolvingNode {
public:
    TxAssigneeNode( const TxLocation& ploc )
            : TxTypeResolvingNode( ploc ) {
    }

    virtual TxAssigneeNode* make_ast_copy() const override = 0;

    bool is_mutable() const {
        auto qtype = this->qtype();
        if ( !qtype.is_modifiable() && !qtype->is_generic_param() ) {
            CERROR( this, "Assignee is not modifiable: " << qtype );
            return false;
        }
        if ( auto origin = this->get_data_graph_origin_expr() )
            return origin->check_chain_mutable();
        return true;
    }

    /** Gets the sub-expression of this expression that determines which data graph (if any) this value is stored in. */
    virtual const TxExpressionNode* get_data_graph_origin_expr() const  = 0;

    /** Generates code that produces the value of this expression, which is an assignable L-value, i.e. address. */
    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const = 0;

    /** Generates code that produces the type id of this assignee. */
    virtual llvm::Value* code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const;
};
