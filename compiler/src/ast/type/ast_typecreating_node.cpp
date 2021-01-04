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
//    std::cerr << "type_pass() of " << this << std::endl;
    auto type = this->resolve_type( TXP_TYPE );
    const_cast<TxActualType*>(type.type())->integrate();
}

TxQualType TxTypeCreatingNode::define_type( TxPassInfo passInfo ) {
    return this->create_type( passInfo );
}


//void TxEmptyDerivedTypeNode::set_requires_mutable( bool mut ) {
//    TxTypeExpressionNode::set_requires_mutable( mut );
//    this->baseTypeNode->set_requires_mutable( mut );
//}
//
//TxActualType* TxEmptyDerivedTypeNode::create_type( TxPassInfo passInfo ) {
//    // create empty specialization (uniquely named but identical type)
//    return this->registry().instantiate_type( this->get_declaration(), this->baseTypeNode, {}, this->requires_mutable_type() );
//}
//
//void TxEmptyDerivedTypeNode::visit_descendants( const AstVisitor& visitor, const AstCursor& thisCursor, const std::string& role, void* context ) {
//    this->baseTypeNode->visit_ast( visitor, thisCursor, "basetype", context );
//
//    if ( auto qtype = this->attempt_qtype() ) {
//        for ( auto implConstr : qtype->get_implicit_constructors() )
//            implConstr->visit_ast( visitor, thisCursor, "initializer", context );
//    }
//}


TxActualType* TxAliasTypeNode::create_type( TxPassInfo passInfo ) {
    // create alias (a declaration referring to a type already declared and defined elsewhere)
    return const_cast<TxActualType*>( this->baseTypeNode->resolve_type( passInfo ).type() );
}


void TxGenParamTypeNode::set_requires_mutable( bool mut ) {
    TxTypeExpressionNode::set_requires_mutable( mut );
    this->constraintTypeNode->set_requires_mutable( mut );  // FIXME: investigate how to determine this
}

TxActualType* TxGenParamTypeNode::create_type( TxPassInfo passInfo ) {
    // create empty specialization (uniquely named but identical type)
    return this->registry().create_type( this->get_declaration(), this->constraintTypeNode, {}, true /*this->requires_mutable_type()*/ );
}
