#pragma once

#include "util/assert.hpp"

#include "symbol/symbol.hpp"

class TxModule;
class TxPackage;
class TxTypeDeclaration;
class LexicalContext;

/* forward declarations pertaining to LLVM code generation */
class LlvmGenerationContext;
class GenScope;
namespace llvm {
    class Value;
}


/** Represents the lexical source scope of a syntax node / entity.
 */
class LexicalContext : public Printable {
    TxScopeSymbol* _scope;
    TxTypeDeclaration* constructedObjTypeDecl;
    TxDeclarationFlags ctxFlags;  // currently only used for TXD_EXPERRBLOCK

    static TxModule* get_module(TxScopeSymbol* scope);

public:
    /** Constructs an "uninitialized" lexical context. */
    LexicalContext() : _scope(), constructedObjTypeDecl(), ctxFlags()  { }

    /** Copy constructor. */
    LexicalContext(const LexicalContext& context)
        : _scope(context._scope), constructedObjTypeDecl(context.constructedObjTypeDecl), ctxFlags(context.ctxFlags) { }

    /** Constructs a lexical context for the provided module.
     * (A module context does not require a parent context.) */
    LexicalContext(TxModule* module) : _scope((TxScopeSymbol*)module), constructedObjTypeDecl(), ctxFlags()  {
        ASSERT(module, "module is NULL");
    }

    /** Constructs a lexical context that is a sub-context of the provided context.
     * The provided scope must be the same or a sub-scope of the parent's scope. */
    LexicalContext(const LexicalContext& parentContext, TxScopeSymbol* scope, bool experrScope=false)
            : _scope(scope), constructedObjTypeDecl(parentContext.constructedObjTypeDecl), ctxFlags(parentContext.ctxFlags)  {
        ASSERT(scope, "scope is NULL");
        if (experrScope)
            this->ctxFlags = TXD_EXPERRBLOCK;
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

    inline TxPackage* package() const { return this->_scope->get_root_scope(); }

    /** Returns declaration flags that shall be added to declarations within this context. */
    inline TxDeclarationFlags decl_flags() const { return this->ctxFlags; }

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
