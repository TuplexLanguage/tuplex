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

static TxScopeSymbol* get_member_symbol( TxScopeSymbol* scope, const std::string& name) {
    if ( auto entScope = dynamic_cast<TxEntitySymbol*>( scope ) ) {
        if ( name.find_first_of( '#' ) != std::string::npos ) {
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

    return scope->get_member_symbol( name );
}

static TxScopeSymbol* inner_lookup_member( TxScopeSymbol* scope, const TxIdentifier& ident ) {
    //std::cout << "From '" << scope->get_full_name() << "': lookup_member(" << ident << ")" << std::endl;
    if ( auto member = get_member_symbol( scope, ident.segment( 0 ) ) ) {
        if ( ident.is_plain() )
            return member;
        else
            return inner_lookup_member( member, TxIdentifier( ident, 1 ) );
    }
    return nullptr;
}

static TxEntitySymbol* inner_lookup_inherited_member( const TxActualType* type, const std::string& name ) {
    ASSERT( name != CONSTR_IDENT, "Can't look up constructors as *inherited* members; in: " << type );
    //std::cerr << "lookup_inherited_member(" << name << ")" << std::endl;
    for ( ; type; type = type->get_base_type() ) {
        if ( auto memberEnt = dynamic_cast<TxEntitySymbol*>( inner_lookup_member( type->get_declaration()->get_symbol(), name ) ) )
            return memberEnt;
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

static TxScopeSymbol* inner_search_symbol( TxScopeSymbol* vantageScope, const TxIdentifier& ident ) {
    // Inherited symbols take precedence over symbols in outer namespaces.
    // As we search the lexical namespaces from inner-most and outwards, if the namespace is a type then look for inherited symbols.

    for ( auto scope = vantageScope; scope; scope = scope->get_outer() ) {
        if ( auto member = inner_lookup_inherited_member( scope, ident.segment( 0 ) ) ) {
            if ( ident.is_plain() )
                return member;
            else
                return inner_lookup_member( member, TxIdentifier( ident, 1 ) );
        }
        else if ( dynamic_cast<TxModule*>( scope ) ) {
            // if member lookup within a module fails, skip parent modules and do global lookup via root namespace (package)
            return inner_lookup_member( scope->get_root_scope(), ident );
        }
    }

    return nullptr;
}

TxScopeSymbol* lookup_member( TxScopeSymbol* vantageScope, TxScopeSymbol* scope, const TxIdentifier& ident ) {
    auto symbol = inner_lookup_member( scope, ident );
    // FUTURE: implement visibility check
    return symbol;
}

TxScopeSymbol* lookup_inherited_member( TxScopeSymbol* vantageScope, TxScopeSymbol* scope, const std::string& name )  {
    auto symbol = inner_lookup_inherited_member( scope, name );
    // FUTURE: implement visibility check
    return symbol;
}

TxEntitySymbol* lookup_inherited_member( TxScopeSymbol* vantageScope, const TxActualType* type, const std::string& name )  {
    auto symbol = inner_lookup_inherited_member( type, name );
    // FUTURE: implement visibility check
    return symbol;
}

TxScopeSymbol* search_symbol( TxScopeSymbol* vantageScope, const TxIdentifier& ident ) {
    auto symbol = inner_search_symbol( vantageScope, ident );
    // FUTURE: implement visibility check
    return symbol;
}
