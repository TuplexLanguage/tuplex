#include "ast_expr_node.hpp"

#include "../../symbol/qual_type.hpp"

/** Checks if this expression produces a modifiable type usage; this requires the whole access chain to be mutable.
 * Generates an error message if it is not and returns false.
 * Note: Transitive across the object graph via references, regardless of mutability of references' *pointer values*.
 */
bool TxExpressionNode::check_chain_mutable() const {
    // The transitive mutability rule for references is that whether the reference itself
    // is modifiable does not matter (i.e. whether it can be changed to point to another object),
    // however the container of the reference must be mutable in order for the reference target
    // to be considered mutable.
    for ( const TxExpressionNode* origin = this; origin; origin = origin->get_data_graph_origin_expr() ) {
        auto type = origin->qualtype();
        if ( !( type->get_type_class() == TXTC_REFERENCE || type->is_modifiable() ) && !type->type()->is_generic_param() ) {
            CERROR( this, "Expression is not modifiable: " << type );
            return false;
        }
    }
    return true;
}
