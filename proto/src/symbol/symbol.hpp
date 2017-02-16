#pragma once

#include <unordered_map>

#include "util/logging.hpp"

#include "identifier.hpp"
#include "tx_declaration_flags.hpp"
#include "tx_field_storage.hpp"
#include "tx_error.hpp"


class TxPackage;
class TxEntitySymbol;
class TxEntityDeclaration;
class TxTypeDeclaration;
class TxFieldDeclaration;
class TxType;
class TxNode;
class TxTypeDefiningNode;
class TxFieldDefiningNode;


/** Represents a Tuplex symbol and namespace/scope.
 * For example package (global namespace), modules, entities, code blocks.
 * A symbol has a unique qualified name, an outer (parent) scope (unless root),
 * and a table of named members (i.e. it is a namespace).
 *
 * A symbol has a "plain" name (single segment, no '.') which is unique within
 * its outer scope.
 * A symbol's globally unique, qualified name corresponds to the concatenation of all the
 * plain names of the symbol scopes along its path from the root scope, separated by '.'.
 * For example: tx.c.puts
 *
 *
 * Name resolution algorithm:
 *
 * 1. Local matching: Attempt to match the leading name segment as a plain name from within the
 * lexical context in which it is stated.
 *
 * 2. Aliasing: If the leading name segment is an alias, perform the alias substitution
 * and continue with the next step, global matching.
 * (Since second step, aliasing can never shadow fields accessed via their plain names.)
 *
 * 3. Global matching: Attempt to match the name as a fully qualified name within global scope.
 *
 * 4. Repeat while the stated identifier has additional name segments:
 * If an entity has been successfully matched, attempt to match the next name segment
 * against the entity's members (including its base types' visible members).
 *
 * When matching a plain name (step 1 above):
 * The current lexical scope stack is traversed until the first match is found.
 * (If a lexical scope block represents a static entity (i.e. type),
 * it includes the type's static members, as well as its base types' (visible) static members.)
 * The bottom element in the lexical scope stack is the current module.
 *
 * Resolution is typically performed from within the lexical context in which the identifier
 * is stated to enforce visibility/access rules.
 * There is also a purely global and public resolution method, e.g. cross-package / API usage.
 */
class TxScopeSymbol : public Printable {
public:
    typedef std::unordered_map<std::string, TxScopeSymbol*> SymbolMap;

private:
    static Logger& LOG;

    /** Plain name of this symbol, which is unique within its outer (parent) scope. Does not contain any '.' characters. */
    const std::string name;
    /** The outer (parent) scope within which this symbol is defined (NULL if this is a root scope). */
    TxScopeSymbol* const outer;
    /** The fully qualified name of this symbol. The last segment equals this scope's plain name. */
    TxIdentifier fullName;

    /** The root (outer-most) scope which is the TxPackage (equal to this if this is the root scope). */
    TxPackage* root;

    /** This scope's member symbols. The identifier keys are the symbols' plain names within this namespace. */
    SymbolMap symbols;
    /** Internal vector containing this module's symbol names in insertion order. */
    std::vector<std::string> symbolNames;

    /** Adds a symbol to this scope's namespace.
     *  If there was already a symbol with that name, it is replaced with the new one
     *  and the previous one is returned.
     */
    TxScopeSymbol* add_symbol(TxScopeSymbol* symbol);

protected:
    virtual bool has_symbol(const std::string& name) const final;

    virtual const TxScopeSymbol* get_symbol(const std::string& name) const final;

    virtual inline TxScopeSymbol* get_symbol(const std::string& name) final {
        return const_cast<TxScopeSymbol*>(static_cast<const TxScopeSymbol *>(this)->get_symbol(name));
    }

    /** Declares a new symbol in this scope. Its name must be unique.
     * Subclasses may override this method and add additional rules.
     * @return true if successful, false otherwise
     */
    virtual bool declare_symbol(const TxParseOrigin& origin, TxScopeSymbol* symbol);

    /** Prepares an entity declaration (adding to an existing or a newly created entity symbol within this scope). */
    virtual TxEntitySymbol* declare_entity(const std::string& plainName, TxNode* definingNode);

    //virtual TxDistinctEntity* overload_entity(TxDistinctEntity* entity, TxSymbolScope* prevSymbol);

    /** Looks up a symbol via this scope. */
    //virtual TxScopeSymbol* lookup_symbol(std::vector<TxScopeSymbol*>& path, const TxIdentifier& ident);

public:
    TxScopeSymbol(TxScopeSymbol* parentScope, const std::string& name);

    virtual ~TxScopeSymbol() = default;


    inline Logger& LOGGER() const { return this->LOG; }


    /** Returns true if this symbol has an outer (parent) scope, or false if it is a root scope. */
    inline bool has_outer() const { return this->outer; }

    /** Gets the outer (parent) scope of this symbol, or NULL if it is a root scope. */
    inline TxScopeSymbol* get_outer() const { return this->outer; }

    inline const std::string& get_name() const { return this->name; }

    inline const TxIdentifier& get_full_name() const { return this->fullName; }

    /** Gets the top-most outer scope of this symbol, which is the root "package" scope. */
    inline TxPackage* get_root_scope() const { return this->root; }


    /*--- lexical scope tracking ---*/

    /** Gets a name that is unique in this scope, starting with the provided base-name.
     * Note, this method does not declare or reserve the returned name.
     * If suppressZeroSuffix is true, don't append '0' if provided base name is non-empty and unique.
     */
    std::string make_unique_name(const std::string& baseName, bool suppressZeroSuffix=false) const;

    TxScopeSymbol* create_code_block_scope( const TxParseOrigin& origin, const std::string& plainName = "" );


    /*--- symbol table handling  ---*/

    virtual TxTypeDeclaration* declare_type(const std::string& plainName, TxTypeDefiningNode* typeDefiner,
                                            TxDeclarationFlags declFlags);

    virtual TxFieldDeclaration* declare_field(const std::string& plainName, TxFieldDefiningNode* fieldDefiner,
                                              TxDeclarationFlags declFlags, TxFieldStorage storage,
                                              const TxIdentifier& dataspace);



    /** Gets a symbol from this namespace. */
    virtual TxScopeSymbol* get_member_symbol(const std::string& name) {
        return this->get_symbol(name);
    }

//    virtual TxScopeSymbol* resolve_generic(TxScopeSymbol* vantageScope, TxScopeSymbol* scope) { return this; }



    /** Returns a read-only, order-of-declaration iterator that points to the first declared symbol name. */
    inline std::vector<std::string>::const_iterator symbol_names_cbegin() const { return this->symbolNames.cbegin(); }
    /** Returns a read-only, order-of-declaration iterator that points to one past the last declared symbol name. */
    inline std::vector<std::string>::const_iterator symbol_names_cend()   const { return this->symbolNames.cend(); }

    /** Returns a read/write, unordered iterator that points to the first symbol mapping. */
    inline SymbolMap::iterator symbols_begin() { return this->symbols.begin(); }
    /** Returns a read/write, unordered iterator that points one past the last symbol mapping. */
    inline SymbolMap::iterator symbols_end()   { return this->symbols.end(); }

    /** Returns a read-only, unordered iterator that points to the first symbol mapping. */
    inline SymbolMap::const_iterator symbols_cbegin() const { return this->symbols.cbegin(); }
    /** Returns a read-only, unordered iterator that points one past the last symbol mapping. */
    inline SymbolMap::const_iterator symbols_cend()   const { return this->symbols.cend(); }


    /*--- symbol resolution, validation and debugging ---*/

    bool symbol_validation_pass() const;

    /** Validates this scope symbol.
     * Checks that this symbol is valid and consistent.
     * This includes name collision and overloading checks.
     * To be overridden by subclasses for specific checks (they must also invoke super implementation).
     */
    virtual bool validate_symbol() const;

    /** Prints all the symbols of this scope and its descendants to stdout. */
    virtual void dump_symbols() const;


    virtual bool operator==(const TxScopeSymbol& other) const {
        return this->get_full_name() == other.get_full_name();
    }
    inline bool operator!=(const TxScopeSymbol& other) const  { return ! this->operator ==(other); }


    virtual std::string declaration_string() const { return ""; }

    virtual std::string description_string() const { return ""; };

    virtual std::string to_string() const {
        return this->get_full_name().to_string();
    }
};



/** A symbol that represents an entity (several if overloaded). */
class TxEntitySymbol : public TxScopeSymbol {
    TxTypeDeclaration* typeDeclaration;
    std::vector<TxFieldDeclaration*> fieldDeclarations;

    TxEntityDeclaration* get_distinct_decl() const;

protected:
    virtual bool declare_symbol( const TxParseOrigin& origin, TxScopeSymbol* symbol ) override {
        if (this->typeDeclaration)
            return TxScopeSymbol::declare_symbol( origin, symbol );
        else {
            ASSERT(false, "Can't add member symbol (" << symbol << ") to a field symbol: " << this->get_full_name());
            return false;
        }
    }

public:
    TxEntitySymbol(TxScopeSymbol* parentScope, const std::string& name)
            : TxScopeSymbol(parentScope, name), typeDeclaration(), fieldDeclarations() {
    }

    bool add_type(TxTypeDeclaration* typeDeclaration);

    bool add_field(TxFieldDeclaration* fieldDeclaration);

    inline bool is_overloaded() const { return this->count() > 1; }

    inline size_t field_count() const { return this->fieldDeclarations.size(); }

    inline size_t count() const { return this->fieldDeclarations.size() + (this->typeDeclaration ? 1 : 0); }

    inline TxTypeDeclaration* get_type_decl() const { return this->typeDeclaration; }

    inline TxFieldDeclaration* get_first_field_decl() const { return (this->fieldDeclarations.empty() ? nullptr : this->fieldDeclarations.front()); }

    inline std::vector<TxFieldDeclaration*>::const_iterator fields_cbegin() const noexcept { return this->fieldDeclarations.cbegin(); }
    inline std::vector<TxFieldDeclaration*>::const_iterator fields_cend() const noexcept { return this->fieldDeclarations.cend(); }


    virtual TxScopeSymbol* get_member_symbol(const std::string& name) override;

//    TxScopeSymbol* resolve_generic(TxScopeSymbol* vantageScope, TxScopeSymbol* scope) override;

//    virtual TxSymbolScope* lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) override {
//        // for now: match against this overloaded symbol's type entity, if present, otherwise fail
//        // (if/when functions can have "members", or non-function fields can be overloaded, this would need to change)
//        if (this->typeDefiner) {
//            ASSERT(this == path.back(), "Expected this to equal last entity in path: " << *this << " != " << *path.back());
//            path[path.size()-1] = this->typeEntity;
//            return this->typeDefiner->lookup_member(path, ident);
//        }
//        this->LOGGER().warning("Attempted to lookup member %s in overloaded fields symbol %s", ident.to_string().c_str(), this->get_full_name().to_string().c_str());
//        return nullptr;
//    }

    virtual bool validate_symbol() const override;

    virtual void dump_symbols() const override;

    virtual std::string declaration_string() const override;

    virtual std::string description_string() const override;

    virtual std::string to_string() const override {
        return this->declaration_string() + " " + this->get_full_name().to_string();
    }
};



inline std::string hashify(const std::string str) {
    std::string hname(str);
    std::replace(hname.begin(), hname.end(), '.', '#');
    return hname;
}

inline std::string dehashify(const std::string str) {
    std::string hname(str);
    std::replace(hname.begin(), hname.end(), '#', '.');
    return hname;
}



// TODO: investigate if these need ever be called with longer than a plain name:

/** like lookup_symbol() but doesn't do search of first name segment */
TxScopeSymbol* lookup_member(TxScopeSymbol* vantageScope, TxScopeSymbol* scope, const TxIdentifier& ident);

TxScopeSymbol* lookup_symbol(TxScopeSymbol* vantageScope, const TxIdentifier& ident);

TxTypeDeclaration* lookup_type(TxScopeSymbol* vantageScope, const TxIdentifier& ident);

TxFieldDeclaration* lookup_field(TxScopeSymbol* vantageScope, const TxIdentifier& ident,
                                 const std::vector<const TxType*>* typeParameters = nullptr);

/** Attempts to resolve an identified symbol, that is potentially overloaded, as a field using the provided type parameters. */
TxFieldDeclaration* resolve_field_lookup(TxScopeSymbol* symbol, const std::vector<const TxType*>* typeParameters);
