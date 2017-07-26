#include "ast_typeexpr_node.hpp"

#include "../ast_entitydecls.hpp"
#include "ast_types.hpp"


void TxTypeExpressionNode::declaration_pass() {
    // The context of this node represents its outer scope.
    // The type expression's created type entity, if any, represents its inner scope.
    if (this->declaration)
        this->lexContext._scope = this->declaration->get_symbol();
    this->typeexpr_declaration_pass();
}


bool is_not_properly_concrete( const TxNode* node, const TxType* type ) {
    if ( !type->is_concrete() ) {
        //if ( type->acttype()->is_generic() || !type->acttype()->is_generic_dependent() )
        if ( !node->context().is_generic() && !type->acttype()->is_generic_param() )
            return true;
        else
            LOG_DEBUG( node->LOGGER(), node << " " << node->context().scope() << " (Not error since generic context) Object is not concrete: " << type );
    }
    return false;
}
