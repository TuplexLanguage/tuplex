#include "ast_typeexpr_node.hpp"

#include "../ast_entitydecls.hpp"
#include "ast_types.hpp"

TxTypeClass TxTypeExpressionNode::resolve_type_class() {
    std::cerr << "#### Default implementation invoked on " << this << std::endl;
    return this->resolve_type( TXR_TYPE_CREATION )->get_type_class();
}

bool is_not_properly_concrete( const TxNode* node, TxQualType type ) {
    if ( !type->is_concrete() ) {
        if ( !node->context().is_generic_dependent() /*&& !type->is_generic_param()*/ )
            return true;
        else
            LOG_DEBUG( node->LOGGER(), node << " " << node->context().scope() << " (Not error since generic context) Object is not concrete: " << type );
    }
    return false;
}
