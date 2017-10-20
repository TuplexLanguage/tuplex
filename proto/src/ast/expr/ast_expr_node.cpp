#include "ast_expr_node.hpp"

#include "symbol/qual_type.hpp"
#include "symbol/type_base.hpp"


/** Checks if this expression produces a modifiable type usage; this requires the whole access chain to be mutable.
 * Generates an error message if it is not and returns false.
 * Note: Transitive across the object graph via references, regardless of mutability of references' *pointer values*.
 */
bool TxExpressionNode::check_chain_mutable() const {
    // The transitive mutability rule for references is that whether the reference itself
    // is modifiable does not matter (i.e. whether it can be changed to point to another object),
    // however the container of the reference must be mutable in order for the reference target
    // to be considered mutable.
    // Generic TYPE type parameters are potentially modifiable and accepted; correctness will be verified for each specialization.
    for ( const TxExpressionNode* origin = this; origin; origin = origin->get_data_graph_origin_expr() ) {
        auto qtype = origin->qtype();
        if ( !( qtype->get_type_class() == TXTC_REFERENCE || qtype.is_modifiable() || qtype->is_generic_param() ) ) {
            CERROR( this, "Expression is not modifiable: " << qtype );
            return false;
        }
    }
    return true;
}


//void TxTypeDefiningValExprNode::type_pass() {
//    //std::cerr << "type_pass() of " << this << std::endl;
//    this->resolve_type( passInfo );
//}
