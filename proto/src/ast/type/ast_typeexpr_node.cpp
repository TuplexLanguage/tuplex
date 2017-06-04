#include "ast_typeexpr_node.hpp"

#include "../ast_decls.hpp"
#include "ast_types.hpp"


bool TxTypeExpressionNode::requires_mutable_type() const {
    const TxNode* p = this->parent();
    if ( auto d = dynamic_cast<const TxTypeDeclNode*>( p ) )
        return d->mutableType;
    if ( auto m = dynamic_cast<const TxModifiableTypeNode*>( p ) ) {
        if ( m->is_modifiable() )
            return true;
        if ( auto m = dynamic_cast<const TxMaybeModTypeNode*>( p ) ) {
            if ( auto d = dynamic_cast<const TxTypeDeclNode*>( m->parent() ) )
                return d->mutableType;
        }
    }
    if ( auto d = dynamic_cast<const TxFieldDefNode*>( p ) )
        return d->modifiable;
    return false;
}

bool TxTypeExpressionNode::get_decl_interface_kw() const {
    const TxNode* p = this->parent();
    if ( auto d = dynamic_cast<const TxTypeDeclNode*>( p ) )
        return d->interfaceKW;
    if ( auto m = dynamic_cast<const TxModifiableTypeNode*>( p ) ) {
        if ( auto d = dynamic_cast<const TxTypeDeclNode*>( m->parent() ) )
            return d->interfaceKW;
    }
    return false;
}

void TxTypeExpressionNode::declaration_pass() {
    // The context of this node represents its outer scope.
    // The type expression's created type entity, if any, represents its inner scope.
    if (this->declaration)
        this->lexContext._scope = this->declaration->get_symbol();
    this->typeexpr_declaration_pass();
}
