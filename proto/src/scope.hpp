#pragma once

#include <unordered_map>

#include "logging.hpp"
#include "identifier.hpp"
#include "tx_declaration_flags.hpp"
#include "type.hpp"


class TxSymbolScope;
class TxEntity;
class TxDistinctEntity;
class TxFieldEntity;
class TxTypeEntity;
class TxAliasEntity;
enum TxFieldStorage : int;


class ResolutionContext {

};


class TxFieldDefiner : public TxTypeDefiner {
public:
    /** Gets the TxExpressionNode that defines the initialization value for this field.
     * Returns nullptr if there is no initializer.
     */
    virtual const TxExpressionNode* get_init_expression() const = 0;
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
 * (Thus, aliasing can intentionally never shadow fields accessed via their plain names.)
 *
 * 3. Global matching: Attempt to match the name as a fully qualified name within global scope.
 *
 * 4. Repeat while the stated identifier has additional name segments:
 * If an entity has been successfully matched, attempt to match the next name segment
 * against the entity's members (including its base types' visible members).
 *
 * When matching a plain name:
 * The current lexical scope stack is traversed until the first match is found.
 * (If a lexical scope block represents a static entity (i.e. type),
 * it includes the type's static members, as well as its base types' (visible) static members.)
 * The bottom element in the lexical scope stack is the current module.
 *
 * Resolution is typically performed from within the lexical context in which the identifier
 * is stated to enforce visibility/access rules.
 * There is also a purely global and public resolution method, e.g. cross-package / API usage.
 */
class TxSymbolScope : public Printable {
public:
    typedef std::unordered_map<std::string, TxSymbolScope*> SymbolMap;

private:
    // (So far) non-represented symbol categories: DATASPACE

    Logger& LOG;

    /** Plain name of this symbol, which is unique within its outer (parent) scope. Does not contain any '.' characters. */
    const std::string name;
    /** The outer (parent) scope within which this symbol is defined (NULL if this is a root scope). */
    TxSymbolScope* const outer;
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
    TxSymbolScope* add_symbol(TxSymbolScope* symbol);

protected:
    virtual bool has_symbol(const std::string& name) const;

    virtual const TxSymbolScope* get_symbol(const std::string& name) const;

    virtual inline TxSymbolScope* get_symbol(const std::string& name) {
        return const_cast<TxSymbolScope*>(static_cast<const TxSymbolScope *>(this)->get_symbol(name));
    }

    /** Declares a new symbol in this scope. Its name must be unique.
     * Subclasses may override this method and add additional rules.
     * @return true if successful, false otherwise
     */
    virtual bool declare_symbol(TxSymbolScope* symbol);

    /** Adds a specific entity declaration to this scope.
     * This method is capable of overloading symbol names if applicable restrictions as fulfilled.
     */
    virtual TxDistinctEntity* declare_entity(TxDistinctEntity* entity);

    virtual TxDistinctEntity* overload_entity(TxDistinctEntity* entity, TxSymbolScope* prevSymbol);

public:
    TxSymbolScope(TxSymbolScope* parentScope, const std::string& name);

    virtual ~TxSymbolScope() = default;


    inline Logger& LOGGER() const { return this->LOG; }


    /** Returns true if this symbol has an outer (parent) scope, or false if it is a root scope. */
    inline bool has_outer() const { return this->outer; }

    /** Gets the outer (parent) scope of this symbol, or NULL if it is a root scope. */
    inline TxSymbolScope* get_outer() const { return this->outer; }

    inline const std::string& get_name() const { return this->name; }

    inline const TxIdentifier& get_full_name() const { return this->fullName; }

    /** Gets the top-most outer scope of this symbol, which is a root scope. */
    const TxSymbolScope* get_root_scope() const;
    /** Gets the top-most outer scope of this symbol, which is a root scope. */
    TxSymbolScope* get_root_scope();


    /*--- lexical scope tracking ---*/

    /** Gets a name that is unique in this scope, starting with the provided base-name.
     * Note, this method does not declare or reserve the returned name. */
    std::string get_unique_name(const std::string& baseName) const;

    TxSymbolScope* create_code_block_scope(const std::string& plainName = "");


    /*--- symbol table handling  ---*/

    virtual TxTypeEntity*  declare_type( const std::string& plainName, TxTypeDefiner* entityDefiner,
                                         TxDeclarationFlags declFlags);

    virtual TxFieldEntity* declare_field(const std::string& plainName, TxFieldDefiner* entityDefiner,
                                         TxDeclarationFlags declFlags, TxFieldStorage storage,
                                         const TxIdentifier& dataspace);

    virtual TxAliasEntity* declare_alias(const std::string& plainName, TxDeclarationFlags declFlags, TxDistinctEntity* aliasedEntity);



    virtual TxSymbolScope* resolve_generic(TxSymbolScope* vantageScope) { return this; }

    //virtual const TxIdentifier* get_alias() { return nullptr; }


    /** Resolves a symbol from the vantage point of this scope.
     * This is the main symbol lookup entry point and implements the language's
     * namespace lookup and visibility rules.
     */
    virtual TxSymbolScope* start_lookup_symbol(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident);

    /** Looks up a symbol via this scope. */
    virtual TxSymbolScope* lookup_symbol(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident);

    /** Performs a downward lookup of the a member, member's member, and so on, within this symbol scope.
     * (The head segment of the identifier is matched against the direct members of this symbol scope.)
     * Performs an exact match - aliases are not considered and if a symbol along the path is overloaded
     * it is not resolved.
     */
    virtual TxSymbolScope* lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident);


    /** Wrapper around lookup_symbol that only matches against type symbols.
     * (This implicitly handles overloaded symbols since no more than a single type can be assigned
     * to a given symbol.)
     */
    TxTypeEntity* lookup_type(ResolutionContext& resCtx, std::vector<TxSymbolScope*>& path, const TxIdentifier& ident);
    inline TxTypeEntity* lookup_type(ResolutionContext& resCtx, const TxIdentifier& ident) {
        std::vector<TxSymbolScope*> tmpPath;  return this->lookup_type(resCtx, tmpPath, ident);
    }

    /** Wrapper around lookup_symbol that only matches against field symbols.
     * It also implements the language's field symbol overloading capability;
     * type parameters must be provided in order to resolve an overloaded symbol.
     */
    TxFieldEntity* lookup_field(ResolutionContext& resCtx, std::vector<TxSymbolScope*>& path, const TxIdentifier& ident,
                                 const std::vector<const TxType*>* typeParameters = nullptr);
    inline TxFieldEntity* lookup_field(ResolutionContext& resCtx, const TxIdentifier& ident,
                                       const std::vector<const TxType*>* typeParameters = nullptr) {
        std::vector<TxSymbolScope*> tmpPath;  return this->lookup_field(resCtx, tmpPath, ident, typeParameters);
    }

    /** Attempts to resolve an identified symbol, that is potentially overloaded, as a field using the provided type parameters. */
    TxFieldEntity* resolve_field_lookup(ResolutionContext& resCtx, TxSymbolScope* symbol, const std::vector<const TxType*>* typeParameters);


    inline SymbolMap::iterator symbols_begin() { return this->symbols.begin(); }
    inline SymbolMap::iterator symbols_end()   { return this->symbols.end(); }
    inline SymbolMap::const_iterator symbols_cbegin() const { return this->symbols.cbegin(); }
    inline SymbolMap::const_iterator symbols_cend()   const { return this->symbols.cend(); }


    /*--- symbol resolution, validation and debugging ---*/

    virtual bool symbol_validation_pass(ResolutionContext& resCtx) final;

    /** Validates this scope symbol.
     * Checks that this symbol is valid and consistent.
     * This includes name collision and overloading checks.
     * To be overridden by subclasses for specific checks (they must also invoke super implementation).
     */
    virtual bool validate_symbol(ResolutionContext& resCtx);

    /** Prints all the symbols of this scope and its descendants to stdout. */
    virtual void dump_symbols() const;


    virtual bool operator==(const TxSymbolScope& other) const {
        return this->get_full_name() == other.get_full_name();
    }
    inline bool operator!=(const TxSymbolScope& other) const  { return ! this->operator ==(other); }


    virtual std::string to_string() const {
        return this->get_full_name().to_string();
    }
};
