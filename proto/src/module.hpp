#pragma once

#include <vector>
#include <unordered_map>

#include "assert.hpp"
#include "logging.hpp"

#include "identifier.hpp"
#include "symbol.hpp"
#include "type.hpp"


class TxPackage;


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

protected:
    virtual bool declare_symbol(TxScopeSymbol* symbol) override;

public:
    TxModule(TxModule* parent, const std::string& name, bool declared);

    /** Returns true if this module has been declared; false if it so far
     * is only a namespace parent of declared module(s) */
    inline bool is_declared() const { return this->declared; }


    virtual TxScopeSymbol* get_member_symbol(const std::string& name) override;


    /*--- sub-module handling ---*/

    TxModule* declare_module(const TxIdentifier& fullName);

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



/** Represents the lexical source scope of a syntax node / entity.
 */
class LexicalContext : public Printable {
    TxScopeSymbol* _scope;
    TxTypeDeclaration* constructedObjTypeDecl;

    static TxModule* get_module(TxScopeSymbol* scope) {
        ASSERT(scope, "scope is NULL");
        if (TxModule* module = dynamic_cast<TxModule*>(scope))
            return module;
        else
            return get_module(scope->get_outer());
    }

public:
    LexicalContext() : _scope(), constructedObjTypeDecl()  { }

    LexicalContext(TxScopeSymbol* scope, TxTypeDeclaration* constructedEntity=nullptr)
            : _scope(scope), constructedObjTypeDecl(constructedEntity)  {
        ASSERT(scope, "scope is NULL");
    }

    inline TxScopeSymbol* scope() const { return this->_scope; }

    void scope(TxScopeSymbol* scope) { this->_scope = scope; }


    /** If this scope is a type declaration, return it. */
    inline TxTypeDeclaration* outer_type() const {
        if (auto entitySymbol = dynamic_cast<TxEntitySymbol*>(this->_scope))
            return entitySymbol->get_type_decl();
        return nullptr;
    }

    inline TxModule* module() const { return get_module(this->_scope); }

    const TxPackage* package() const;
    TxPackage* package();

    /** If non-null, this context is within a constructor and the declaration for the constructed object type is returned. */
    inline TxTypeDeclaration* get_constructed() { return this->constructedObjTypeDecl; }

    inline void set_constructed(TxTypeDeclaration* constructedEntity) { this->constructedObjTypeDecl = constructedEntity; }

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
