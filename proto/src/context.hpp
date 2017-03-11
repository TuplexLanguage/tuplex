#pragma once

#include "util/assert.hpp"

#include "tx_error.hpp"

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
    bool reinterpretation;  // true if this is a reinterpretation (specialization) of an AST
    ExpectedErrorClause* expErrCtx;

    // FUTURE: Maybe represent/refer to the closest surrounding statement here, which is not equivalent to scope
    //         (a scope can have several statements, a statement may create subscopes).
    //         This might make multi-context handling (defContext / lexContext) and declaration flag handling cleaner.

    static TxModule* get_module(TxScopeSymbol* scope);

public:
    /** Constructs an "uninitialized" lexical context. */
    LexicalContext() //= default;
        : _scope(), constructedObjTypeDecl(), reinterpretation(), expErrCtx()  { }

    /** Copy constructor. */
    LexicalContext( const LexicalContext& context ) = default;

    /** Constructs a lexical context for the provided module.
     * (A module context does not require a parent context.) */
    LexicalContext( TxModule* module )
        : _scope((TxScopeSymbol*)module), constructedObjTypeDecl(), reinterpretation(), expErrCtx()  {
        ASSERT(module, "module is NULL");
    }

    /** Constructs a lexical context that is a sub-context of the provided context.
     * The provided scope must be the same or a sub-scope of the parent's scope.
     * This initializes the contextual (inherited) declaration flags. */
    LexicalContext( const LexicalContext& parentContext, TxScopeSymbol* scope )
            : _scope(scope), constructedObjTypeDecl(parentContext.constructedObjTypeDecl),
              reinterpretation(parentContext.reinterpretation), expErrCtx(parentContext.expErrCtx)  {
        ASSERT(scope, "scope is NULL");
    }

    /** Constructs a lexical context that is a sub-context of the provided context, and is an expected-error context.
     * The provided scope must be the same or a sub-scope of the parent's scope. */
    LexicalContext( const LexicalContext& parentContext, TxScopeSymbol* scope, ExpectedErrorClause* expErrCtx )
            : _scope(scope), constructedObjTypeDecl(parentContext.constructedObjTypeDecl),
              reinterpretation(parentContext.reinterpretation), expErrCtx(expErrCtx)  {
        ASSERT(scope, "scope is NULL");
    }

    /** Constructs a new lexical context for a given scope, and that may represent a reinterpretation of a lexical unit. */
    LexicalContext( TxScopeSymbol* scope, ExpectedErrorClause* expErrCtx, bool reinterpretation )
        : _scope(scope), constructedObjTypeDecl(), reinterpretation(reinterpretation), expErrCtx(expErrCtx) { }


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

    /** Returns true if this is within a reinterpretation (specialization) of an AST. */
    inline bool is_reinterpretation() const { return ( this->reinterpretation ); }

    /** Returns the ExpectedErrorClause if this context is within an expected-error declaration / statement / block,
     * otherwise nullptr. */
    inline ExpectedErrorClause* exp_error() const { return expErrCtx; }

    /** If non-null, this context is within a constructor and the declaration for the constructed object type is returned. */
    inline TxTypeDeclaration* get_constructed() { return this->constructedObjTypeDecl; }

    inline void set_constructed(TxTypeDeclaration* constructedEntity) { this->constructedObjTypeDecl = constructedEntity; }

    inline virtual bool operator==(const LexicalContext& other) const {
        return this->_scope == other._scope;
    }
    inline virtual bool operator!=(const LexicalContext& other) const {
        return ! this->operator==(other);
    }

    std::string str() const {
        return _scope->str();
    }
};
