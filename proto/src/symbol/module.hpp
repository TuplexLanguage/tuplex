#pragma once

#include <vector>
#include <unordered_map>

#include "util/assert.hpp"
#include "util/logging.hpp"

#include "identifier.hpp"
#include "symbol.hpp"



/** Represents a Tuplex Module.
 */
class TxModule : public TxScopeSymbol {
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

    TxModule* inner_declare_module(const TxIdentifier& ident, bool builtin);

protected:
    virtual bool declare_symbol(TxScopeSymbol* symbol) override;

public:
    TxModule(TxModule* parent, const std::string& name, bool declared);

    /** Returns true if this module has been declared; false if it so far
     * is only a namespace parent of declared module(s) */
    inline bool is_declared() const { return this->declared; }


    virtual TxScopeSymbol* get_member_symbol(const std::string& name) override;


    /*--- sub-module handling ---*/

    /** Declares a sub-module to this module.
     * @param builtin set to true if this is a built-in module (not defined by user code)
     */
    TxModule* declare_module(const TxIdentifier& qualName, bool builtin=false);

    TxModule* lookup_module(const TxIdentifier& fullName);


    /*--- registering imports & aliases ---*/

    /** Registers an import. Can be invoked before the symbol table pass. */
    void register_import(const TxIdentifier& identifier);

    /** Prepares this module and its submodules, resolving imports and aliases. */
    virtual void prepare_modules();



    /*--- validation and debugging ---*/

    virtual void dump_symbols() const override;


    virtual std::string description_string() const override {
        return "module";
    }
};
