#pragma once

#include "util/assert.hpp"

#include "tx_error.hpp"

#include "symbol/symbol.hpp"
#include "symbol/module.hpp"

class TxModule;
class TxPackage;
class TxLambdaExprNode;

/** Represents the lexical source scope of a syntax node / entity.
 */
class LexicalContext : public Printable {
public:
    TxScopeSymbol* _scope;
    bool generic;  // true if this is within a generic type definition (note, can be true also when reinterpreted, if not all params bound)
    const TxTypeDefiningNode* reinterpretationDefiner;  // non-null if this is a reinterpretation (specialization) of an AST
    ExpectedErrorClause* expErrCtx;
    TxLambdaExprNode* enclosingLambda;  // the nearest enclosing lambda node, or null if none

    /** Constructs an "uninitialized" lexical context. */
    LexicalContext() //= default;
            : _scope(), generic(), reinterpretationDefiner(), expErrCtx(), enclosingLambda() {
    }

    /** Copy constructor. */
    LexicalContext( const LexicalContext& context ) = default;

    /** Constructs a lexical context for the provided module.
     * (A module context does not require a parent context.) */
    LexicalContext( TxModule* module )
            : _scope( (TxScopeSymbol*) module ), generic(), reinterpretationDefiner(), expErrCtx(), enclosingLambda() {
        ASSERT( module, "module is NULL" );
    }

    /** Constructs a lexical context that is a sub-context of the provided context.
     * The provided scope must be the same or a sub-scope of the parent's scope. */
    LexicalContext( const LexicalContext& parentContext, TxScopeSymbol* scope )
            : _scope( scope ), generic( parentContext.generic ), reinterpretationDefiner( parentContext.reinterpretationDefiner ),
              expErrCtx( parentContext.expErrCtx ), enclosingLambda( parentContext.enclosingLambda ) {
        ASSERT( scope, "scope is NULL" );
    }

    /** Constructs a new lexical context for a given scope, and that may represent a reinterpretationDefiner of a lexical unit. */
    LexicalContext( TxScopeSymbol* scope, ExpectedErrorClause* expErrCtx, bool generic, const TxTypeDefiningNode* reinterpretationDefiner )
            : _scope( scope ), generic( generic ), reinterpretationDefiner( reinterpretationDefiner ), expErrCtx( expErrCtx ), enclosingLambda() {
        ASSERT( scope, "scope is NULL" );
    }


    inline TxScopeSymbol* scope() const {
        return this->_scope;
    }

    /** Returns the closest enclosing module. */
    inline TxModule* module() const {
        ASSERT( this->_scope, "scope is NULL" );
        for ( auto scope = this->_scope; true; scope = scope->get_outer() ) {
            if ( auto module = dynamic_cast<TxModule*>( scope ) )
                return module;
        }
    }

    inline TxPackage* package() const {
        return this->_scope->get_root_scope();
    }


    /** Returns true if this is within a generic type definition (a generic type whose parameters are not all bound). */
    inline bool is_generic() const {
        return ( this->generic );
    }

    /** Returns true if this is within a reinterpretationDefiner (specialization) of an AST. */
    inline bool is_reinterpretation() const {
        return ( this->reinterpretationDefiner );
    }

    /** If this is reinterpreted, the definer of the reinterpretation (that bound the type parameters) is returned; otherwise null. */
    inline const TxTypeDefiningNode* reinterpretation_definer() const {
        return ( this->reinterpretationDefiner );
    }

    /** Returns the ExpectedErrorClause if this context is within an expected-error declaration / statement / block,
     * otherwise nullptr. */
    inline ExpectedErrorClause* exp_error() const {
        return expErrCtx;
    }

    inline TxLambdaExprNode* enclosing_lambda() const {
        return this->enclosingLambda;
    }

    std::string str() const override {
        return _scope->str();
    }
};
