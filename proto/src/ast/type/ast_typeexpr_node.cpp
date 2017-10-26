#include "ast_typeexpr_node.hpp"

#include "../ast_entitydecls.hpp"
#include "ast_types.hpp"


bool is_not_properly_concrete( const TxNode* node, const TxQualType type ) {
    if ( !type->is_concrete() ) {
        //if ( type->is_generic() || !type->is_generic_dependent() )
        if ( !node->context().is_generic() && !type->is_generic_param() )
            return true;
        else
            LOG_DEBUG( node->LOGGER(), node << " " << node->context().scope() << " (Not error since generic context) Object is not concrete: " << type );
    }
    return false;
}
