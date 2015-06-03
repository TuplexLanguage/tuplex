#pragma once

#include <unordered_map>

#include "logging.hpp"
#include "identifier.hpp"
#include "tx_declaration_flags.hpp"
#include "tx_field_storage.hpp"

#include "entity_proxy.hpp"


class TxAliasSymbol;
class TxEntitySymbol;
class TxEntityDeclaration;
class TxTypeDeclaration;
class TxFieldDeclaration;


class ResolutionContext {
};


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
 * (Thus, aliasing can never shadow fields accessed via their plain names.)
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
    Logger& LOG;

    /** Plain name of this symbol, which is unique within its outer (parent) scope. Does not contain any '.' characters. */
    const std::string name;
    /** The outer (parent) scope within which this symbol is defined (NULL if this is a root scope). */
    TxScopeSymbol* const outer;
    /** The fully qualified name of this symbol. The last segment equals this scope's plain name. */
    TxIdentifier fullName;

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
    virtual bool has_symbol(const std::string& name) const;

    virtual const TxScopeSymbol* get_symbol(const std::string& name) const;

    virtual inline TxScopeSymbol* get_symbol(const std::string& name) {
        return const_cast<TxScopeSymbol*>(static_cast<const TxScopeSymbol *>(this)->get_symbol(name));
    }

    /** Declares a new symbol in this scope. Its name must be unique.
     * Subclasses may override this method and add additional rules.
     * @return true if successful, false otherwise
     */
    virtual bool declare_symbol(TxScopeSymbol* symbol);

    /** Prepares an entity declaration (adding to an existing or a newly created entity symbol within this scope). */
    virtual TxEntitySymbol* declare_entity(const std::string& plainName);

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

    /** Gets the top-most outer scope of this symbol, which is a root scope. */
    const TxScopeSymbol* get_root_scope() const;
    /** Gets the top-most outer scope of this symbol, which is a root scope. */
    TxScopeSymbol* get_root_scope();


    /*--- lexical scope tracking ---*/

    /** Gets a name that is unique in this scope, starting with the provided base-name.
     * Note, this method does not declare or reserve the returned name. */
    std::string get_unique_name(const std::string& baseName) const;

    TxScopeSymbol* create_code_block_scope(const std::string& plainName = "");


    /*--- symbol table handling  ---*/

    virtual TxTypeDeclaration* declare_type(const std::string& plainName, TxTypeDefiner* typeDefiner,
                                            TxDeclarationFlags declFlags);

    virtual TxFieldDeclaration* declare_field(const std::string& plainName, TxFieldDefiner* fieldDefiner,
                                              TxDeclarationFlags declFlags, TxFieldStorage storage,
                                              const TxIdentifier& dataspace);

    virtual TxAliasSymbol* declare_alias(const std::string& plainName, TxDeclarationFlags declFlags, TxEntityDeclaration* aliasedDeclaration);



    /** Gets a symbol from this namespace. */
    virtual TxScopeSymbol* get_member_symbol(const std::string& name) {
        return this->get_symbol(name);
    }

    virtual TxScopeSymbol* resolve_generic(TxScopeSymbol* vantageScope) { return this; }

//    /** Resolves a symbol from the vantage point of this scope.
//     * This is the main symbol lookup entry point and implements the language's
//     * namespace lookup and visibility rules.
//     */
//    virtual TxSymbolScope* start_lookup_symbol(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident);
//
//    /** Performs a downward lookup of the a member, member's member, and so on, within this symbol scope.
//     * (The head segment of the identifier is matched against the direct members of this symbol scope.)
//     * Performs an exact match - aliases are not considered and if a symbol along the path is overloaded
//     * it is not resolved.
//     */
//    virtual TxSymbolScope* lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident);
//
//
//    /** Wrapper around lookup_symbol that only matches against type symbols.
//     * (This implicitly handles overloaded symbols since no more than a single type can be assigned
//     * to a given symbol.)
//     */
//    TxTypeEntity* lookup_type(ResolutionContext& resCtx, std::vector<TxSymbolScope*>& path, const TxIdentifier& ident);
//    inline TxTypeEntity* lookup_type(ResolutionContext& resCtx, const TxIdentifier& ident) {
//        std::vector<TxSymbolScope*> tmpPath;  return this->lookup_type(resCtx, tmpPath, ident);
//    }
//
//    virtual TxSymbolScope* resolve_generic(TxSymbolScope* vantageScope) { return this; }
//
//
//    /** Wrapper around lookup_symbol that only matches against type symbols.
//     * (This implicitly handles overloaded symbols since no more than a single type can be assigned
//     * to a given symbol.)
//     */
//    TxTypeDeclaration* lookup_type(ResolutionContext& resCtx, std::vector<TxSymbolScope*>& path, const TxIdentifier& ident);
//    inline TxTypeDeclaration* lookup_type(ResolutionContext& resCtx, const TxIdentifier& ident) {
//        std::vector<TxSymbolScope*> tmpPath;  return this->lookup_type(resCtx, tmpPath, ident);
//    }
//
//    /** Wrapper around lookup_symbol that only matches against field symbols.
//     * It also implements the language's field symbol overloading capability;
//     * type parameters must be provided in order to resolve an overloaded symbol.
//     */
//    TxFieldDeclaration* lookup_field(ResolutionContext& resCtx, std::vector<TxSymbolScope*>& path, const TxIdentifier& ident,
//                                     const std::vector<const TxType*>* typeParameters = nullptr);
//    inline TxFieldDeclaration* lookup_field(ResolutionContext& resCtx, const TxIdentifier& ident,
//                                            const std::vector<const TxType*>* typeParameters = nullptr) {
//        std::vector<TxSymbolScope*> tmpPath;  return this->lookup_field(resCtx, tmpPath, ident, typeParameters);
//    }


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


    virtual inline std::string symbol_class_string() const { return "<scope>"; }

    virtual std::string to_string() const {
        return this->symbol_class_string() + " " + this->get_full_name().to_string();
    }
};



/** Represents a symbol that is an alias for another entity symbol. */
class TxAliasSymbol : public TxScopeSymbol {
    const TxDeclarationFlags declFlags;
    TxEntityDeclaration* aliasedDeclaration;

protected:
    virtual bool declare_symbol(TxScopeSymbol* symbol) override {
        this->LOGGER().error("Can't add member symbol (%s) directly to an alias symbol: %s",
                             symbol->to_string().c_str(), this->get_full_name().to_string().c_str());
        return false;
    }

public:
    TxAliasSymbol(TxScopeSymbol* parentScope, const std::string& name, TxDeclarationFlags declFlags, TxEntityDeclaration* aliasedDeclaration)
            : TxScopeSymbol(parentScope, name), declFlags(declFlags), aliasedDeclaration(aliasedDeclaration) { }

    TxDeclarationFlags get_decl_flags() const { return this->declFlags; }

    //const TxIdentifier& get_aliased_name() const { return this->aliasedDeclaration->get_symbol()->get_full_name(); }

    TxEntityDeclaration* get_aliased_declaration() const { return this->aliasedDeclaration; }

    TxScopeSymbol* get_aliased_symbol() const;

    TxScopeSymbol* resolve_generic(TxScopeSymbol* vantageScope) override;

//    virtual const TxType* resolve_symbol_type(ResolutionContext& resCtx) override {
//        return this->aliasedEntity->resolve_symbol_type(resCtx);
//    }
//
//    virtual const TxType* get_type() const override {
//        return this->aliasedEntity->get_type();
//    }

    virtual std::string symbol_class_string() const override {
        return "<alias>";
    }
//    virtual std::string to_string() const override {
//        return "<alias>          " + this->get_full_name().to_string() + " = TODO"; // + this->get_aliased_name().to_string();
//    }
};



/** A symbol that represents an entity (several if overloaded). */
class TxEntitySymbol : public TxScopeSymbol {
    TxTypeDeclaration* typeDeclaration;
    std::vector<TxFieldDeclaration*> fieldDeclarations;

    TxEntityDeclaration* get_distinct_decl() const;

protected:
    virtual bool declare_symbol(TxScopeSymbol* symbol) override {
        if (this->typeDeclaration)
            return TxScopeSymbol::declare_symbol(symbol);
        else {
            this->LOGGER().error("Can't add member symbol (%s) to a field symbol: %s",
                                 symbol->to_string().c_str(), this->get_full_name().to_string().c_str());
            return false;
        }
    }

public:
    TxEntitySymbol(TxScopeSymbol* parentScope, const std::string& name)
            : TxScopeSymbol(parentScope, name), typeDeclaration(), fieldDeclarations() {
    }

    bool add_type(TxTypeDeclaration* typeDeclaration) {
        if (this->typeDeclaration) {
            this->LOGGER().error("Can't overload several type declarations under the same name: %s", this->get_full_name().to_string().c_str());
            return false;
        }
        this->typeDeclaration = typeDeclaration;
        return true;
    }

    bool add_field(TxFieldDeclaration* fieldDeclaration) {
        this->fieldDeclarations.push_back(fieldDeclaration);
        return true;
    }

//    /** Adds a distinct entity declaration to this overloaded symbol, after performing some validity checking.
//     * Note that the provided entity does not get this overloaded symbol as its parent scope.
//     */
//    bool add(TxDistinctEntity* entity) {
//        // verify that the names are equal except for the internal "$..." suffix:
//        auto entName = entity->get_full_name().to_string();
//        ASSERT(this->get_full_name().to_string() == entName.substr(0, this->get_full_name().to_string().length()),
//               "Overloaded entity declarations must have the same qualified name (except for $ suffix): "
//               << entName << "!=" << this->get_full_name().to_string());
//
//        if (auto typeEnt = dynamic_cast<TxTypeEntity*>(entity)) {
//            if (this->typeEntity) {
//                this->LOGGER().error("Can't overload several type declarations on the same name: %s", this->get_full_name().to_string().c_str());
//                return false;
//            }
//            this->typeEntity = typeEnt;
//        }
//        else if (auto fieldEnt = dynamic_cast<TxFieldEntity*>(entity))
//            this->fieldEntities.push_back(fieldEnt);
//        else {  // shouldn't happen...
//            this->LOGGER().error("Unknown TxDistinctEntity type: %s", entity->to_string().c_str());
//            return false;
//        }
//        return true;
//    }

    inline bool is_overloaded() const { return this->count() > 1; }

    inline size_t field_count() const { return this->fieldDeclarations.size(); }

    inline size_t count() const { return this->fieldDeclarations.size() + (this->typeDeclaration ? 1 : 0); }

    inline TxTypeDeclaration* get_type_decl() const { return this->typeDeclaration; }

    inline TxFieldDeclaration* get_first_field_decl() const { return (this->fieldDeclarations.empty() ? nullptr : this->fieldDeclarations.front()); }

    inline std::vector<TxFieldDeclaration*>::const_iterator fields_cbegin() const noexcept { return this->fieldDeclarations.cbegin(); }
    inline std::vector<TxFieldDeclaration*>::const_iterator fields_cend() const noexcept { return this->fieldDeclarations.cend(); }


//    virtual const TxType* resolve_symbol_type(ResolutionContext& resCtx) override {
//        return this->typeDefiner ? this->typeDefiner->resolve_symbol_type(resCtx) : nullptr;
//    }
//
//    virtual const TxType* get_type() const override {
//        return this->typeDefiner ? this->typeDefiner->get_type() : nullptr;
//    }


    virtual TxScopeSymbol* get_member_symbol(const std::string& name) override;

    TxScopeSymbol* resolve_generic(TxScopeSymbol* vantageScope) override;

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

    virtual std::string symbol_class_string() const override;
};



// TODO: investigate if these need ever be called with longer than a plain name:

/** like lookup_symbol() but doesn't do search of first name segment */
TxScopeSymbol* lookup_member(TxScopeSymbol* vantageScope, const TxIdentifier& ident);

TxScopeSymbol* lookup_symbol(TxScopeSymbol* vantageScope, const TxIdentifier& ident);

TxTypeDeclaration* lookup_type(TxScopeSymbol* vantageScope, const TxIdentifier& ident);

TxFieldDeclaration* lookup_field(TxScopeSymbol* vantageScope, const TxIdentifier& ident,
                                 const std::vector<const TxType*>* typeParameters = nullptr);

/** Attempts to resolve an identified symbol, that is potentially overloaded, as a field using the provided type parameters. */
TxFieldDeclaration* resolve_field_lookup(ResolutionContext& resCtx, TxScopeSymbol* symbol, const std::vector<const TxType*>* typeParameters);
