#include "module.hpp"
#include "package.hpp"
#include "entity.hpp"


TxModule::TxModule(TxModule* parent, const std::string& name, bool declared)
        : TxScopeSymbol(parent, name), declared(declared) {
    if (parent) {  // if not the root package
        ASSERT(dynamic_cast<TxModule*>(parent), "Illegal to declare a module under a non-module parent: " << this->get_full_name());
        // FUTURE: disallow tx... namespace declarations in "user mode"
        //if (moduleNamespace.begins_with(BUILTIN_NS))
        //    parser_error(this->parseLocation, "Illegal namespace name, must not begin with \"" BUILTIN_NS "\"");
        if (this->get_full_name().to_string() != BUILTIN_NS) {
            this->import_symbol(TxIdentifier(BUILTIN_NS ".*"));
        }
    }
}


bool TxModule::declare_symbol(TxScopeSymbol* symbol) {
    if (! this->declared) {
        // only modules allowed within namespace of non-declared "module"
        if (! dynamic_cast<TxModule*>(symbol)) {
            CERROR(this->get_root_scope()->types().get_builtin_location(), "Can't add non-module symbol " << symbol
                   << " to undeclared namespace " << this->get_full_name());
            return false;
        }
    }
    return TxScopeSymbol::declare_symbol(symbol);
}


TxModule* TxModule::declare_module(const TxIdentifier& ident, bool builtin) {
    //this->LOGGER().debug("Declaring module %s at %s", ident.to_string().c_str(), this->get_full_name().to_string().c_str());
    if (! this->get_outer()) {
        // this is the namespace root - the tuplex package
        if (! builtin) {
            if (ident.begins_with(BUILTIN_NS) && !this->get_root_scope()->driver().get_options().allow_tx)
                CERROR(this->get_root_scope()->types().get_builtin_location(), "Can't declare or extend built-in namespace from user code: '" << ident << "'");
        }
    }
    else {
        if (ident.begins_with(LOCAL_NS))
            CERROR(this->get_root_scope()->types().get_builtin_location(), "Can't declare or extend $local namespace under other than the namespace root: '" << this->get_full_name() << "'");
    }

// currently validated directly from the grammar parse
//    if (! builtin) {
//        if (! this->get_root_scope()->driver().validate_module_name(ident))
//            return nullptr;
//    }

    return this->inner_declare_module(ident, builtin);
}

TxModule* TxModule::inner_declare_module(const TxIdentifier& ident, bool builtin) {
    if (ident.is_qualified()) {
        // declare submodule further down the namespace hierarchy (not a direct child of this one)
        if (! (ident.begins_with(this->get_full_name()) && ident.segment_count() > this->get_full_name().segment_count())) {
            CERROR(this->get_root_scope()->types().get_builtin_location(), "Can't declare module " << ident << " as a submodule of " << this->get_full_name());
            return nullptr;
        }
        auto subName = TxIdentifier(ident, this->get_full_name().segment_count());
        if (subName.is_plain())
            return this->inner_declare_module(subName, builtin);
        else {
            auto nextName = subName.segment(0);
            TxModule* nextModule = dynamic_cast<TxModule*>(this->get_symbol(nextName));
            if (! nextModule) {
                // create undeclared submodule scope
                nextModule = new TxModule(this, nextName, false);
                if (! this->declare_symbol(nextModule)) {
                    delete nextModule;
                    return nullptr;
                }
            }
            return nextModule->inner_declare_module(ident, builtin);
        }
    }

    // declare submodule that is direct child of this one
    std::string name = ident.to_string();
    if (auto prev = this->get_symbol(name)) {
        if (auto prevMod = dynamic_cast<TxModule*>(prev)) {
            if (! prevMod->is_declared()) {
                prevMod->set_declared();
                this->LOGGER().debug("Declared module %s", prevMod->get_full_name().to_string().c_str());
            }
            else
                this->LOGGER().debug("Continued declaration of module %s", prev->get_full_name().to_string().c_str());
            return prevMod;
        }
        else {
            CERROR(this->get_root_scope()->types().get_builtin_location(), "Name collision upon declaring module " << prev->get_full_name());
            return nullptr;
        }
    }
    else {
        auto module = new TxModule(this, name, true);
        if (this->declare_symbol(module)) {
            this->LOGGER().debug("Declared module %s", module->get_full_name().to_string().c_str());
            return module;
        }
        delete module;
        return nullptr;
    }
}


TxModule* TxModule::lookup_module(const TxIdentifier& fullName) {
    if (auto member = this->get_member_symbol(fullName.segment(0))) {
        if (auto module = dynamic_cast<TxModule*>(member)) {
            if (fullName.is_plain())
                return module;
            else
                return module->lookup_module(TxIdentifier(fullName, 1));
        }
        CERROR(this->get_root_scope()->types().get_builtin_location(), "Symbol is not a Module: " << member);
    }
    return nullptr;
}



TxScopeSymbol* TxModule::get_member_symbol(const std::string& name) {
    // overrides in order to inject alias lookup
    //std::cout << "In module '" << this->get_full_name() << "': get_member_symbol(" << name << ")" << std::endl;
    if (auto symbol = this->TxScopeSymbol::get_member_symbol(name))
        return symbol;
    else if (this->usedNames.count(name)) {  // attempt to find aliased match
        const TxIdentifier& aliasedName(this->usedNames.at(name));
        //std::cout << "In module '" << this->get_full_name() << "': matching alias " << name << " == " << aliasedName << std::endl;
        return lookup_symbol(this->get_root_scope(), aliasedName);
    }
    return nullptr;
}



/*--- registering imports & aliases ---*/

bool TxModule::use_symbol(const TxModule* imported, const std::string& plainName) {
    // in future this could support "use tx.qualified.name as myalias"
    // (in which case we must guard against circular aliases here)
    if (auto symbol = imported->get_symbol(plainName)) {
        if (! dynamic_cast<const TxModule*>(symbol)) {  // if not a submodule name
            auto result = this->usedNames.emplace(plainName, symbol->get_full_name());
            if (! symbol->get_full_name().begins_with(BUILTIN_NS))
                this->LOGGER().debug("Imported symbol %-16s %s", plainName.c_str(), symbol->get_full_name().to_string().c_str());
            return result.second;
        }
    }
    else
        CERROR(this->get_root_scope()->types().get_builtin_location(), "Imported symbol '" << plainName << "' does not exist in module " << imported->get_full_name());
    return false;
}

bool TxModule::import_symbol(const TxIdentifier& identifier) {
    // Note: the imported symbols are only visible within current module
    ASSERT(identifier.is_qualified(), "can't import unqualified identifier: " << identifier);
    auto otherModule = this->get_root_scope()->lookup_module(identifier.parent());
    if (! otherModule)
        return false;
    else if (identifier.name() == "*") {
        for (auto siter = otherModule->symbols_cbegin(); siter != otherModule->symbols_cend(); siter++)
            if (siter->first[0] != '~'  // not a modifiable derivation
                    && siter->first.find_first_of('$') == std::string::npos)  // not an internal name
                this->use_symbol(otherModule, siter->first);
        return true;
    }
    else
        return this->use_symbol(otherModule, identifier.name());
}

void TxModule::register_import(const TxIdentifier& identifier) {
    this->registeredImports.push_back(identifier);
}

void TxModule::prepare_modules() {
    this->LOGGER().debug("Preparing module %s", this->get_full_name().to_string().c_str());
    for (auto import : this->registeredImports) {
        if (! this->import_symbol(import))
            CERROR(this->get_root_scope()->types().get_builtin_location(), "Failed to import " << import.to_string());
    }
    for (auto entry = this->symbols_begin(); entry != this->symbols_end(); entry++) {
        if (auto submod = dynamic_cast<TxModule*>(entry->second))
            submod->prepare_modules();
    }
}


/*--- validation and debugging ---*/

/** Prints all the symbols of this scope and its descendants to stdout. */
void TxModule::dump_symbols() const {
    const TxIdentifier builtinNamespace(BUILTIN_NS);
    if (this->is_declared() && this->get_full_name() != builtinNamespace) {
        printf("=== symbols of '%s' ===\n", this->get_full_name().to_string().c_str());
        {
            bool headerprinted = false;
            for (auto & pair : this->usedNames) {
                if (pair.second.parent() != builtinNamespace || this->get_root_scope()->driver().get_options().dump_tx_symbols) {
                    if (! headerprinted) {
                        printf("--- aliases ---\n");
                        headerprinted = true;
                    }
                    printf("%-14s %s\n", pair.first.c_str(), pair.second.to_string().c_str());
                }
            }
        }
        printf("--- entities ---\n");
    }
    TxScopeSymbol::dump_symbols();
}
