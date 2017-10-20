#include "ast_typecreating_node.hpp"

#include "symbol/type_registry.hpp"

#include "tx_logging.hpp"
#include "tx_error.hpp"


void TxTypeCreatingNode::type_pass() {
//    std::cerr << "type_pass() of " << this << std::endl;
    auto type = this->resolve_type( TXP_TYPE );
    const_cast<TxActualType*>(type.type())->integrate();
}

TxQualType TxTypeCreatingNode::define_type( TxPassInfo passInfo ) {
    return this->create_type( passInfo );
}


void TxEmptyDerivedTypeNode::set_requires_mutable( bool mut ) {
    TxTypeExpressionNode::set_requires_mutable( mut );
    this->baseTypeNode->set_requires_mutable( mut );
}

TxActualType* TxEmptyDerivedTypeNode::create_type( TxPassInfo passInfo ) {
    // create empty specialization (uniquely named but identical type)
    return this->registry().instantiate_type( this->get_declaration(), this->baseTypeNode, {}, this->requires_mutable_type() );
}


TxActualType* TxAliasTypeNode::create_type( TxPassInfo passInfo ) {
    // create alias (a declaration referring to a type already declared and defined elsewhere)
    return const_cast<TxActualType*>( this->baseTypeNode->resolve_type( passInfo ).type() );
}


void TxGenParamTypeNode::set_requires_mutable( bool mut ) {
    TxTypeExpressionNode::set_requires_mutable( mut );
    this->constraintTypeNode->set_requires_mutable( mut );
}

TxActualType* TxGenParamTypeNode::create_type( TxPassInfo passInfo ) {
    // create empty specialization (uniquely named but identical type)
    return this->registry().instantiate_type( this->get_declaration(), this->constraintTypeNode, {}, this->requires_mutable_type() );
}
