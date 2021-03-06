#include "ast_qualtypes.hpp"

#include "ast_types.hpp"


void TxQualTypeExprNode::verification_pass() const {
    if ( auto qtype = this->attempt_qtype() ) {
        if ( qtype.is_modifiable() && !qtype->is_mutable() )
        {
            //if ( qtype->is_generic_param())
            //    std::cerr << "Here is an immutable generic param type: " << qtype << std::endl;
            CERROR( this, "Can't declare immutable type as modifiable: " << qtype.type() );
        }
    }
}


TxQualType TxSetQualTypeExprNode::define_type( TxTypeResLevel typeResLevel ) {
    auto qtype = _typeNode->resolve_type( typeResLevel );
    if ( _modifiable ) {
        if ( qtype.is_modifiable() ) {
            LOG_INFO( this->LOGGER(), "Double modifiability qualifier for type: " << qtype );
            return qtype;
        }
        else
            return TxQualType( qtype.type(), true );
    }
    else
        return TxQualType( qtype.type(), false );
}

TxQualType TxFlexModTypeExprNode::define_type( TxTypeResLevel typeResLevel ) {
    auto qtype = _typeNode->resolve_type( typeResLevel );
    return TxQualType( qtype.type(), qtype->is_mutable() );
}

TxQualType TxModifiableTypeNode::define_type( TxTypeResLevel typeResLevel ) {
    auto qtype = _typeNode->resolve_type( typeResLevel );
    if ( this->is_modifiable() )
        return TxQualType( qtype.type(), true );
    else
        return qtype;  // pass through qualifiers of type expression
}

void TxModifiableTypeNode::typeexpr_declaration_pass() {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if ( auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>( this->_typeNode ) ) {
        if ( auto maybeModElem = dynamic_cast<TxMaybeModTypeNode*>( arrayBaseType->elementTypeNode->type_expr_node() ) ) {
            // (can this spuriously add Modifiable node to predeclared modifiable type, generating error?)
            this->LOGGER()->debug( "Implicitly declaring Array Element modifiable at %s", this->str().c_str() );
            maybeModElem->set_modifiable( true );
        }
    }
}


void TxMaybeModTypeNode::typeexpr_declaration_pass() {
    // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
    if ( !this->_modifiable ) {
        if ( auto arrayBaseType = dynamic_cast<TxArrayTypeNode*>( this->_typeNode ) )
            if ( typeid(*arrayBaseType->elementTypeNode->type_expr_node()) == typeid(TxModifiableTypeNode) ) {
                this->LOGGER()->debug( "Implicitly declaring Array modifiable at %s", this->str().c_str() );
                this->set_modifiable( true );
            }
    }

    if ( this->_modifiable )
        TxModifiableTypeNode::typeexpr_declaration_pass();
}
