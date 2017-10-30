#include "module.hpp"

#include "symbol_lookup.hpp"
#include "package.hpp"
#include "entity.hpp"
#include "driver.hpp"

TxModule::TxModule( TxModule* parent, const std::string& name, const TxParseOrigin& origin, bool declared )
        : TxScopeSymbol( parent, name ), declared( declared ), origin( origin ) {
    if ( parent ) {  // if not the root package
        ASSERT( dynamic_cast<TxModule*>( parent ), "Illegal to declare a module under a non-module parent: " << this->get_full_name() );
        if ( this->get_full_name().str() != BUILTIN_NS ) {
            this->import_symbol( origin, TxIdentifier( BUILTIN_NS ".*" ) );
        }
    }
}

void TxModule::declare_symbol( const TxParseOrigin& origin, TxScopeSymbol* symbol ) {
    if ( !this->declared ) {
        // only modules allowed within namespace of non-declared "module"
        if ( !dynamic_cast<TxModule*>( symbol ) ) {
            CERR_THROWDECL( origin, "Can't add non-module symbol " << symbol << " to undeclared namespace " << this->get_full_name() );
        }
    }
    TxScopeSymbol::declare_symbol( origin, symbol );
}

TxModule* TxModule::declare_module( const TxParseOrigin& origin, const TxIdentifier& ident, bool builtin ) {
    if ( !this->get_outer() ) {
        // this is the namespace root - the tuplex package
    }
    else {
        if ( ident.begins_with( LOCAL_NS ) )
            CERROR( origin, "Can't declare or extend $local namespace under other than the namespace root: '" << this->get_full_name() << "'" );
    }

// currently validated directly from the grammar parse
//    if (! builtin) {
//        if (! this->get_root_scope()->driver().validate_module_name(ident))
//            return nullptr;
//    }

    return this->inner_declare_module( origin, ident, builtin );
}

TxModule* TxModule::inner_declare_module( const TxParseOrigin& origin, const TxIdentifier& ident, bool builtin ) {
    if ( ident.is_qualified() ) {
        // declare submodule further down the namespace hierarchy (not a direct child of this one)
        if ( !( ident.begins_with( this->get_full_name() ) && ident.segment_count() > this->get_full_name().segment_count() ) ) {
            CERROR( origin, "Can't declare module " << ident << " as a submodule of " << this->get_full_name() );
            return nullptr;
        }
        auto subName = TxIdentifier( ident, this->get_full_name().segment_count() );
        if ( subName.is_plain() )
            return this->inner_declare_module( origin, subName, builtin );
        else {
            auto nextName = subName.segment( 0 );
            TxModule* nextModule = dynamic_cast<TxModule*>( this->get_symbol( nextName ) );
            if ( !nextModule ) {
                // create undeclared submodule scope
                nextModule = new TxModule( this, nextName, origin, false );
                this->declare_symbol( origin, nextModule );
            }
            return nextModule->inner_declare_module( origin, ident, builtin );
        }
    }

    // declare submodule that is direct child of this one
    std::string name = ident.str();
    if ( auto prev = this->get_symbol( name ) ) {
        if ( auto prevMod = dynamic_cast<TxModule*>( prev ) ) {
            if ( !prevMod->is_declared() ) {
                prevMod->set_declared();
                this->LOGGER()->debug( "Declared module %s", prevMod->get_full_name().str().c_str() );
            }
            else
                this->LOGGER()->debug( "Continued declaration of module %s", prev->get_full_name().str().c_str() );
            return prevMod;
        }
        else {
            CERROR( origin, "Name collision upon declaring module " << prev->get_full_name() );
            return nullptr;
        }
    }
    else {
        auto module = new TxModule( this, name, origin, true );
        this->declare_symbol( origin, module );
        this->LOGGER()->debug( "Declared module %s", module->get_full_name().str().c_str() );
        return module;
    }
}

TxModule* TxModule::lookup_module( const TxIdentifier& fullName ) {
    if ( auto member = this->get_member_symbol( fullName.segment( 0 ) ) ) {
        if ( auto module = dynamic_cast<TxModule*>( member ) ) {
            if ( fullName.is_plain() )
                return module;
            else
                return module->lookup_module( TxIdentifier( fullName, 1 ) );
        }
        CERROR( this->origin, "Symbol is not a Module: " << member );
    }
    return nullptr;
}

TxScopeSymbol* TxModule::get_member_symbol( const std::string& name ) {
    // overrides in order to inject alias lookup
    //std::cout << "In module '" << this->get_full_name() << "': get_member_symbol(" << name << ")" << std::endl;
    if ( auto symbol = this->TxScopeSymbol::get_member_symbol( name ) )
        return symbol;
    else if ( this->usedNames.count( name ) ) {  // attempt to find aliased match
        const TxIdentifier& aliasedName( this->usedNames.at( name ) );
        //std::cout << "In module '" << this->get_full_name() << "': matching alias " << name << " == " << aliasedName << std::endl;
        return search_symbol( this->get_root_scope(), aliasedName.str() );
    }
    return nullptr;
}

/*--- registering imports & aliases ---*/

bool TxModule::use_symbol( const TxParseOrigin& origin, const TxModule* imported, const std::string& plainName ) {
    // in future this could support "use tx.qualified.name as myalias"
    // (in which case we must guard against circular aliases here)
    if ( auto symbol = imported->get_symbol( plainName ) ) {
        if ( !dynamic_cast<const TxModule*>( symbol ) ) {  // if not a submodule name
            this->usedNames[plainName] = symbol->get_full_name();  // adds or replaces existing mapping
            if ( !symbol->get_full_name().begins_with( BUILTIN_NS ) )
                this->LOGGER()->debug( "Imported symbol %-16s %s", plainName.c_str(), symbol->get_full_name().str().c_str() );
            return true;
        }
    }
    else
        CERROR( origin, "Imported symbol '" << plainName << "' does not exist in module " << imported->get_full_name() );
    return false;
}

bool TxModule::import_symbol( const TxParseOrigin& origin, const TxIdentifier& identifier ) {
    // Note: the imported symbols are only visible within current module
    ASSERT( identifier.is_qualified(), "can't import unqualified identifier: " << identifier );
    auto otherModule = this->get_root_scope()->lookup_module( identifier.parent() );
    if ( !otherModule )
        return false;
    else if ( identifier.name() == "*" ) {
        for ( auto & symName : otherModule->get_decl_order_names() ) {
            if ( symName[0] != '~'  // not a modifiable derivation
                 && symName.find_first_of( '$' ) == std::string::npos )  // not an internal name
                this->use_symbol( origin, otherModule, symName );
        }
        return true;
    }
    else
        return this->use_symbol( origin, otherModule, identifier.name() );
}

void TxModule::register_import( const TxParseOrigin& origin, const TxIdentifier& identifier ) {
    this->registeredImports.emplace_back( origin, identifier );
}

void TxModule::prepare_modules() {
    this->LOGGER()->debug( "Preparing imports in module '%s'", this->get_full_name().str().c_str() );
    for ( auto & import : this->registeredImports ) {
        if ( !this->import_symbol( import.origin, import.name ) )
            CERROR( import.origin, "Failed to import " << import.name );
    }
    for ( auto & symName : this->get_decl_order_names() ) {
        auto symbol = this->get_symbol( symName );
        if ( auto submod = dynamic_cast<TxModule*>( symbol ) )
            submod->prepare_modules();
    }
}

/** Prints all the symbols of this scope and its descendants to stdout. */
void TxModule::dump_symbols() const {
    const TxIdentifier builtinNamespace( BUILTIN_NS );
    if ( this->is_declared() && this->get_full_name() != builtinNamespace ) {
        printf( "=== symbols of '%s' ===\n", this->get_full_name().str().c_str() );
        {
            bool headerprinted = false;
            for ( auto & pair : this->usedNames ) {
                if ( pair.second.begins_with( builtinNamespace ) && !this->get_root_scope()->driver().get_options().dump_tx_symbols )
                    continue;

                if ( !headerprinted ) {
                    printf( "--- aliases ---\n" );
                    headerprinted = true;
                }
                printf( "%-14s %s\n", pair.first.c_str(), pair.second.str().c_str() );
            }
        }
        printf( "--- entities ---\n" );
    }
    TxScopeSymbol::dump_symbols();
}
