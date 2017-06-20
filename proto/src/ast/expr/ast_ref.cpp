#include "ast_ref.hpp"


void TxReferenceToNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();
    this->target->symbol_resolution_pass();

    // In current model it is "difficult" to predict in an efficient manner and without various virtual "canThisExprDoThat()" methods
    // whether sub-expressions are capable of generating a pointer to their value.
    // So this check is disabled and replaced with code-check cerror in TxExpressionNode::code_gen_dyn_address().
    // TODO: Review model...

//    if ( !( this->target->get_storage() == TXS_GLOBAL || this->target->get_storage() == TXS_STATIC ) )
//        std::cerr << "Unexpected storage type: " << this->target->get_storage() << ": " << this->target << std::endl;

//    auto targetNode = this->target;
//    if ( auto wrapperNode = dynamic_cast<TxExprWrapperNode*>( targetNode ) )
//        targetNode = wrapperNode->exprNode;
//    if ( !( dynamic_cast<TxFieldValueNode*>( targetNode )
//            || dynamic_cast<TxElemDerefNode*>( targetNode )
//            || targetNode->is_statically_constant() ) ) {
//        CERROR( this, "Can't construct reference to non-addressable expression / rvalue: " << targetNode );
//    }
}
