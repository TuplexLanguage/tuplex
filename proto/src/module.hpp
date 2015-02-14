#pragma once

#include <vector>
#include <unordered_map>

#include "txassert.hpp"
#include "logging.hpp"

#include "identifier.hpp"
#include "type.hpp"
#include "scope.hpp"


class TxPackage;


/** Represents a Tuplex Module.
 */
class TxModule : public TxSymbolScope {
    /** true if this module has been declared; false if it so far is only a namespace parent of declared module(s) */
    bool declared;

    /** This module's registered imports. */
    std::vector<TxIdentifier> registeredImports;
	/** This module's imported names. It maps plain names to fully qualified names. */
    std::unordered_map<std::string, const TxIdentifier> usedNames;

    void set_declared() {
        ASSERT(!this->declared, "module " << this << " has already been declared");
        this->declared = true;
    }

    /** Imports the identified symbol as a plain name alias in this module's namespace.
     * Cannot be invoked before the symbol table pass, as the imported module might not be parsed yet.
     * If the symbol's module was not previously imported, this causes it to be loaded and
     * included in the compilation of this package.
     */
    bool import_symbol(const TxIdentifier& identifier);

    bool use_symbol(const TxModule* imported, const std::string& plainName);

protected:
    virtual bool declare_symbol(TxSymbolScope* symbol) override;

    virtual TxSymbolScope* lookup_symbol(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) override;

public:
    TxModule(TxModule* parent, const std::string& name, bool declared);

    /** Gets the TuplexPackage root scope. */
    const TxPackage* get_package() const;
    TxPackage* get_package();

    /** Returns true if this module has been declared; false if it so far
     * is only a namespace parent of declared module(s) */
    inline bool is_declared() const { return this->declared; }


    /*--- sub-module handling ---*/

    TxModule* declare_module(const TxIdentifier& name);

    TxModule* lookup_module(const TxIdentifier& name);


    /*--- registering imports & aliases ---*/

    /** Registers an import. Can be invoked before the symbol table pass. */
    void register_import(const TxIdentifier& identifier);


    /*--- validation and debugging ---*/

    virtual bool prepare_symbol() override;

    virtual void dump_symbols() const override;


    virtual std::string to_string() const override {
	    return "<module> " + this->get_full_name().to_string();
	}
};



/** Represents the lexical source scope of a syntax node / entity.
 */
class LexicalContext : public Printable {
    TxSymbolScope* _scope;

    static TxModule* get_module(TxSymbolScope* scope) {
        ASSERT(scope, "scope is NULL");
        if (TxModule* module = dynamic_cast<TxModule*>(scope))
            return module;
        else
            return get_module(scope->get_outer());
    }

public:
    LexicalContext() : _scope()  { }

    LexicalContext(TxSymbolScope* scope) : _scope(scope)  {
        ASSERT(scope, "scope is NULL");
    }

    inline TxSymbolScope* scope() const { return this->_scope; }

    void scope(TxSymbolScope* scope) { this->_scope = scope; }


    inline TxModule* module() const { return get_module(this->_scope); }

    const TxPackage* package() const;
    TxPackage* package();

    inline virtual bool operator==(const LexicalContext& other) const {
        return this->_scope == other._scope;
    }
    inline virtual bool operator!=(const LexicalContext& other) const {
        return ! this->operator==(other);
    }

    std::string to_string() const {
        return _scope->to_string();
    }
};
