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
enum TxFieldStorage : int;


class TxTypeDefiner : public TxTypeProxy {
public:
    /** Returns true if this type definer "is ready" - has a defined type.
     * If this method returns false, calls to TxTypeProxy::get_type() have undefined results.
     */
    virtual bool is_type_defined() const = 0;
};


/** Represents a Tuplex symbol and namespace/scope.
 * For example package (global namespace), modules, entities, code blocks.
 * A symbol has a unique qualified name, a parent scope (unless root),
 * and a table of named members (i.e. it is a namespace).
 *
 * A symbol has a "plain" name (single segment, no '.') which is unique within
 * its parent's scope.
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

    /** plain name of this symbol */
    const std::string name;
    /** The parent scope within which this symbol is defined. (NULL if this is a root scope) */
    TxSymbolScope* const parent;
    /** The fully qualified name of this scope. The last segment is this scope's member name under the parent. */
    TxIdentifier fullName;

    /** This module's symbols. The identifier keys are the fully qualified names of the entities. */
    SymbolMap symbols;
    /** Internal vector containing this module's symbol names in insertion order. */
    std::vector<std::string> symbolNames;


    TxSymbolScope* inner_enter_scope(const std::string& baseName, int counter);

    /** Adds a symbol to this scope's symbol table.
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


//    /** Gets the full names of the symbols defined directly in this scope (excluding subscopes). */
//    std::vector<const TxIdentifier*> get_symbol_full_names() const;  // FIXME: refactor
//    friend TxFieldEntity;

public:
    TxSymbolScope(TxSymbolScope* parent, const std::string& name);

    virtual ~TxSymbolScope() = default;


    inline Logger& LOGGER() const { return this->LOG; }


    /** Returns true if this symbol has a parent scope in which it is defined, or false if it is a top level name. */
    inline bool has_parent() const { return this->parent; }

    inline TxSymbolScope* get_parent() const { return this->parent; }

    inline const std::string& get_name() const { return this->name; }

    inline const TxIdentifier& get_full_name() const { return this->fullName; }

    const TxSymbolScope* get_root_scope() const;
    TxSymbolScope* get_root_scope();


    /*--- lexical scope tracking ---*/

    TxSymbolScope* create_code_block_scope(const std::string& plainName = "");


    /*--- symbol table handling  ---*/

    virtual TxTypeEntity*  declare_type( const std::string& plainName, const TxTypeDefiner* typeDefiner,
                                         TxDeclarationFlags modifiers);
    virtual TxFieldEntity* declare_field(const std::string& plainName, const TxTypeDefiner* typeDefiner,
                                         TxDeclarationFlags modifiers, TxFieldStorage storage,
                                         const TxIdentifier& dataspace);


    /** experimental */
    virtual const TxSymbolScope* resolve_generic(const TxSymbolScope* vantageScope) const { return this; }
    virtual bool is_alias() const { return false; }
    virtual const TxIdentifier* get_alias() const { return nullptr; }


    /** Resolves a symbol from the vantage point of this scope.
     * This is the main symbol lookup entry point and implements the language's
     * namespace lookup and visibility rules.
     */
    virtual const TxSymbolScope* resolve_symbol(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const;

    /** Looks up a symbol via this scope. */
    virtual const TxSymbolScope* lookup_symbol(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const;

    /** Performs a downward lookup of the a member, member's member, and so on, within this symbol scope.
     * (The head segment of the identifier is matched against the direct members of this symbol scope.)
     * Performs an exact match - aliases are not considered and if a symbol along the path is overloaded
     * it is not resolved.
     */
    virtual const TxSymbolScope* lookup_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const;


    /** Wrapper around lookup_symbol that only matches against type symbols.
     * (This implicitly handles overloaded symbols since no more than a single type can be assigned
     * to a given symbol.)
     */
    const TxTypeEntity* resolve_type(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const;
    inline const TxTypeEntity* resolve_type(const TxIdentifier& ident) const {
        std::vector<const TxSymbolScope*> tmp;  return this->resolve_type(tmp, ident);
    }

    /** Wrapper around lookup_symbol that only matches against field symbols.
     * It also implements the language's field symbol overloading capability;
     * type parameters must be provided in order to resolve an overloaded symbol.
     */
    const TxFieldEntity* resolve_field(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident,
                                       const std::vector<const TxType*>* typeParameters = nullptr) const;
    inline const TxFieldEntity* resolve_field(const TxIdentifier& ident,
                                              const std::vector<const TxType*>* typeParameters = nullptr) const {
        std::vector<const TxSymbolScope*> tmp;  return this->resolve_field(tmp, ident, typeParameters);
    }

    /** Attempts to resolve an identified symbol, that is potentially overloaded, as a field using the provided type parameters. */
    const TxFieldEntity* resolve_symbol_as_field(const TxSymbolScope* symbol, const std::vector<const TxType*>* typeParameters) const;


    inline SymbolMap::const_iterator symbols_cbegin() const { return this->symbols.cbegin(); }
    inline SymbolMap::const_iterator symbols_cend()   const { return this->symbols.cend(); }


    /*--- validation and debugging ---*/

    /** Performs the symbol table pass on this scope and its descendants.
     * Checks that all this scope's symbols are valid and consistent.
     * This includes name collision and overloading checks.
     */
    virtual bool prepare_symbol_table();

    /** Performs the symbol table pass on this scope symbol.
     * Checks that this symbol is valid and consistent.
     * This includes name collision and overloading checks.
     * To be overridden by subclasses for specific checks (they must also invoke super implementation).
     */
    virtual bool prepare_symbol();

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
