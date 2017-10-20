#pragma once

#include "symbol.hpp"


/** Looks up a name (qualified or simple) among the members of the specified scope.
 * Unlike search_symbol() this doesn't do a global search of the leading name segment.
 * Unlike lookup_inherited_member() this doesn't search parent types for inherited symbols. */
TxScopeSymbol* lookup_member( TxScopeSymbol* vantageScope, TxScopeSymbol* scope, const TxIdentifier& ident );

/** Looks up a simple name in the specified scope, and if the scope represents a type also searches the parent types. */
TxScopeSymbol* lookup_inherited_member( TxScopeSymbol* vantageScope, TxScopeSymbol* scope, const std::string& name );

class TxActualType;
/** match against the type's direct members, and then its inherited members, returning the first found */
TxEntitySymbol* lookup_inherited_member( TxScopeSymbol* vantageScope, const TxActualType* type, const std::string& name );

/** Searches for the symbol with the specified name, starting from vantageScope and proceeding through its outer scopes,
 * finally doing a global search assuming the name is a global fully qualified name.
 */
TxScopeSymbol* search_symbol( TxScopeSymbol* vantageScope, const TxIdentifier& ident );
