#include "context.hpp"
#include "module.hpp"
#include "ast_base.hpp"


TxModule* LexicalContext::get_module(TxScopeSymbol* scope) {
    ASSERT(scope, "scope is NULL");
    if (TxModule* module = dynamic_cast<TxModule*>(scope))
        return module;
    else
        return get_module(scope->get_outer());
}

//TxTypeDeclaration* LexicalContext::outer_type() const {
//    if (auto entitySymbol = dynamic_cast<TxEntitySymbol*>(this->_scope))
//        return entitySymbol->get_type_decl();
//    return nullptr;
//}



TxTypeDefiningNode* TxSpecializationTypeDefiner::get_node() const {
    return dynamic_cast<TxTypeDefiningNode*>(this->specDefiner);
}
