#include "ast_ref.hpp"


void TxReferenceToNode::symbol_resolution_pass() {
    TxExpressionNode::symbol_resolution_pass();
    this->target->symbol_resolution_pass();

//    if ( !( this->target->get_storage() == TXS_GLOBAL || this->target->get_storage() == TXS_STATIC ) )
//        std::cerr << "Unexpected storage type: " << this->target->get_storage() << ": " << this->target << std::endl;
    auto targetNode = this->target;
    if ( auto wrapperNode = dynamic_cast<TxExprWrapperNode*>( targetNode ) )
        targetNode = wrapperNode->exprNode;
    if ( !( dynamic_cast<TxFieldValueNode*>( targetNode )
            || dynamic_cast<TxElemDerefNode*>( targetNode )
            || targetNode->is_statically_constant() ) ) {
        CERROR( this, "Can't construct reference to non-addressable expression / rvalue: " << targetNode );
    }
}
