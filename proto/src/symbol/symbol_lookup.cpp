#include "symbol_lookup.hpp"

#include "declaration.hpp"
#include "qual_type.hpp"
#include "type_base.hpp"
#include "package.hpp"


/* Notes on namespace / lookup semantics:

Private virtual can be truly virtual.
Public virtual is virtual, but referencing them without a base expression is non-polymorphic.

## Type resolution is virtual and non-polymorphic
Type . Type
SFIELD . Type  => decltype(SFIELD).TYPE
field . Type   => decltype(field).TYPE
ref . Type     => decltype(ref).TYPE

## The types of all the following are determined statically:
Type . SFIELD  ## of course non-polymorphic
Type . field   ## invalid; produces field's declared type

SFIELD . SFIELD  ## "polymorphic" (but statically known)
SFIELD . field   ## non-polymorphic

field . SFIELD  ## "polymorphic" (but statically known)
field . field   ## non-polymorphic

ref . SFIELD  ## polymorphic     (determined by the actual object type)
ref . field   ## non-polymorphic (determined by the declared type of ref)

*/

static const TxEntityDeclaration* get_symbols_declaration( TxEntitySymbol* entitySymbol ) {
    const TxEntityDeclaration* decl = entitySymbol->get_type_decl();
    if ( !decl ) {
        if ( !entitySymbol->is_overloaded() )
            decl = entitySymbol->get_first_field_decl();
        else
            // overloaded field, can't match
            return nullptr;
    }
    return decl;
}

static TxScopeSymbol* inner_search_symbol( TxScopeSymbol* vantageScope, const TxIdentifier& ident );

static TxScopeSymbol* get_explicit_outer( TxScopeSymbol* scope ) {
//    std::cout << "From '" << scope->get_full_name() << "': get_explicit_outer()" << std::endl;
    for ( auto outer = scope->get_outer(); outer; outer = outer->get_outer() ) {
//        std::cout << "     '" << scope->get_full_name() << "': get_explicit_outer()" << std::endl;
        if ( outer->get_name().find( '$' ) == std::string::npos ) { // skips implicit scopes
            LOG_NOTE( scope->LOGGER(), "Substituting '#' with '" << outer << "'" );
            return outer;
        }
    }
    THROW_LOGIC( "Missing explicit outer scope" );
}

static TxScopeSymbol* special_lookup( TxScopeSymbol* scope, const std::string& name) {
    if ( name == "#" )
        return get_explicit_outer( scope );
    else if ( auto entScope = dynamic_cast<TxEntitySymbol*>( scope ) ) {
        if ( name.find( '#' ) != std::string::npos ) {
            // sought name is a hashified, fully qualified name (e.g. my#SType#E)
            auto fullName = dehashify( name );
            if ( auto hashedSym = inner_search_symbol( scope, TxIdentifier( fullName ) ) ) {
                if ( auto hashedEntSym = dynamic_cast<TxEntitySymbol*>( hashedSym ) ) {
                    if ( auto hashedDecl = get_symbols_declaration( hashedEntSym ) ) {
                        if ( hashedDecl->get_decl_flags() & TXD_GENPARAM ) {
                            // symbol distinctly (non-overloaded) refers to a GENPARAM
                            // Resolves e.g:  my#SType#E = my.SType.E  to binding  -P---- ---B--  my.$SType<tx#~Float,tx#~Double>.E
                            if ( auto thisDecl = get_symbols_declaration( entScope ) ) {
                                if ( auto thisType = thisDecl->get_definer()->qtype() ) {
                                    if ( auto bindingDecl = thisType->lookup_param_binding( hashedDecl ) ) {
                                        //scope->LOGGER()->debug( "Resolved %-16s = %-16s to binding\t%s", name.c_str(),
                                        //                         hashedSym->get_full_name().str().c_str(), bindingDecl->str().c_str() );
                                        return bindingDecl->get_symbol();
                                    }
                                }
                            }
                        }
                    }
                }
                return hashedSym;
            }
        }
    }
    return nullptr;
}

static inline TxScopeSymbol* inner_lookup_member( TxScopeSymbol* scope, const std::string& name ) {
    //std::cout << "From '" << scope->get_full_name() << "': inner_lookup_member(" << name << ")" << std::endl;
    return scope->get_member_symbol( name );
}

static TxScopeSymbol* inner_lookup_inherited_member( const TxActualType* type, const std::string& name ) {
    ASSERT( name != CONSTR_IDENT, "Can't look up constructors as *inherited* members; in: " << type );
    //std::cerr << "lookup_inherited_member(" << name << ")" << std::endl;
    for ( ; type; type = type->get_base_type() ) {
        if ( auto member = inner_lookup_member( type->get_declaration()->get_symbol(), name ) )
            return member;
        ASSERT( type->is_integrated(), "In inherited member lookup for '" << name << "' - type not integrated: " << type );
        for ( auto interf : type->get_interfaces() ) {
            if ( auto memberEnt = inner_lookup_inherited_member( interf, name ) )
                return memberEnt;
        }
    }
    return nullptr;
}

static TxScopeSymbol* inner_lookup_inherited_member( TxScopeSymbol* scope, const std::string& name )  {
    if ( auto symbol = inner_lookup_member( scope, name ) )
        return symbol;

    if ( auto entSym = dynamic_cast<TxEntitySymbol*>( scope ) ) {
        if ( const TxEntityDeclaration* entDecl = get_symbols_declaration( entSym ) ) {
            // Note: The starting scope might not be resolved at this point and we don't (and shouldn't need to) force-resolve it here.
            //       (since when e.g. resolving base types recursion error would occur)
            if ( auto qtype = entDecl->get_definer()->attempt_qtype() ) {
                if ( qtype->is_integrated() ) {
                    return inner_lookup_inherited_member( qtype.type(), name );
                }
                else {
                    //std::cerr << "In search for '" << name << "' - scope's type not integrated: " << qtype << std::endl;
                    return inner_lookup_member( qtype->get_declaration()->get_symbol(), name );
                }
            }
            //else
            //    std::cerr << "In search for '" << name << "' - scope's type not resolved: " << entDecl << std::endl;
        }
    }
    return nullptr;
}

static TxScopeSymbol* inner_search_name( TxScopeSymbol* vantageScope, const std::string& name ) {
    // Inherited symbols take precedence over symbols in outer namespaces.
    // As we search the lexical namespaces from inner-most and outwards, if the namespace is a type then look for inherited symbols.

    if ( auto sym = special_lookup( vantageScope, name ) )
        return sym;
    for ( auto scope = vantageScope; scope; scope = scope->get_outer() ) {
        if ( auto member = inner_lookup_inherited_member( scope, name ) ) {
            return member;
        }
        else if ( dynamic_cast<TxModule*>( scope ) ) {
            // member lookup within a module failed - skip parent modules and do global lookup via root namespace (package)
            return inner_lookup_member( scope->get_root_scope(), name );
        }
    }
    return nullptr;
}

/** (capable of searching for compound identifiers) */
static TxScopeSymbol* inner_search_symbol( TxScopeSymbol* vantageScope, const TxIdentifier& ident ) {
    auto member = inner_search_name( vantageScope, ident.segment( 0 ) );
    if ( member && !ident.is_plain() ) {
        TxIdentifier rest( ident, 1 );
        for ( auto it = rest.segments_cbegin(); it != rest.segments_cend(); ++it ) {
            member = inner_lookup_member( member, *it );
            if ( !member )
                break;
        }
        //return inner_lookup_member( member, TxIdentifier( ident, 1 ) );
    }
    return member;
}

TxScopeSymbol* lookup_member( TxScopeSymbol* vantageScope, TxScopeSymbol* scope, const std::string& name ) {
    auto symbol = special_lookup( scope, name );
    if ( !symbol )
        symbol = inner_lookup_member( scope, name );
    // FUTURE: implement visibility check
    return symbol;
}

TxScopeSymbol* lookup_inherited_member( TxScopeSymbol* vantageScope, TxScopeSymbol* scope, const std::string& name )  {
    auto symbol = special_lookup( scope, name );
    if ( !symbol )
        symbol = inner_lookup_inherited_member( scope, name );
    // FUTURE: implement visibility check
    return symbol;
}

TxScopeSymbol* lookup_inherited_member( TxScopeSymbol* vantageScope, const TxActualType* type, const std::string& name )  {
    auto symbol = special_lookup( type->get_declaration()->get_symbol(), name );
    if ( !symbol )
        symbol = inner_lookup_inherited_member( type, name );
    // FUTURE: implement visibility check
    return symbol;
}

TxScopeSymbol* search_name( TxScopeSymbol* vantageScope, const std::string& name ) {
    auto symbol = inner_search_name( vantageScope, name );
    // FUTURE: implement visibility check
    return symbol;
}

/** (capable of searching for compound identifiers) */
TxScopeSymbol* search_symbol( TxScopeSymbol* vantageScope, const std::string& identifier ) {
    auto symbol = inner_search_symbol( vantageScope, identifier );
    // FUTURE: implement visibility check
    return symbol;
}
