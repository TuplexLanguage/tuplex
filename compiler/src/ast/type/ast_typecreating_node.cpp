#include "ast_typecreating_node.hpp"

#include "ast/ast_entitydecls.hpp"

#include "symbol/type_registry.hpp"

#include "tx_logging.hpp"
#include "tx_error.hpp"


void TxTypeCreatingNode::typeexpr_declaration_pass() {
    // The context of this node represents its outer scope.
    // The type expression's created type entity, if any, represents its inner scope.
    if (this->declaration)
        this->lexContext._scope = this->declaration->get_symbol();
}

void TxTypeCreatingNode::type_pass() {
    auto type = this->resolve_type( TXR_TYPE_CREATION );
}

TxQualType TxTypeCreatingNode::define_type( TxTypeResLevel typeResLevel ) {
    return this->create_type( typeResLevel );
}

// an attempt to make types resolve base types immediately, and parameters later (here):
//TxQualType TxTypeCreatingNode::resolve_type( TxPassInfo typeResLevel ) {
//    bool previouslyCreated = bool( this->_type );
//    auto type = TxTypeExpressionNode::resolve_type( typeResLevel );
//    if ( !previouslyCreated && typeResLevel == TXP_TYPE )
//        const_cast<TxActualType*>(type.type())->resolve_params( TXP_TYPE );
//    return type;
//}


const TxActualType* TxAliasTypeNode::create_type( TxTypeResLevel typeResLevel ) {
    // create alias (a declaration referring to a type already declared and defined elsewhere)
    return this->baseTypeNode->resolve_type( typeResLevel ).type();
}


void TxGenParamTypeNode::set_requires_mutable( bool mut ) {
    TxTypeExpressionNode::set_requires_mutable( mut );
    this->constraintTypeNode->set_requires_mutable( mut );  // FIXME: investigate how to determine this
}

const TxActualType* TxGenParamTypeNode::create_type( TxTypeResLevel typeResLevel ) {
    // create empty specialization (uniquely named but identical type)
    return this->registry().create_type( this->get_declaration(), this->constraintTypeNode, {}, true /*this->requires_mutable_type()*/ );
}
