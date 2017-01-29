#include "context.hpp"
#include "symbol/module.hpp"
#include "ast_base.hpp"


TxModule* LexicalContext::get_module(TxScopeSymbol* scope) {
    ASSERT(scope, "scope is NULL");
    if (TxModule* module = dynamic_cast<TxModule*>(scope))
        return module;
    else
        return get_module(scope->get_outer());
}


TxTypeDefiningNode* TxSpecializationTypeDefiner::get_node() const {
    return static_cast<TxTypeDefiningNode*>(this->specDefiner);  // FUTURE: maybe change type of member instead
}
