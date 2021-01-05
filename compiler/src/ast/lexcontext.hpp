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
    const TxTypeResolvingNode* reinterpretationDefiner;  // non-null if this is a reinterpretation (specialization) of an AST
    ExpectedErrorClause* expErrCtx;
    TxLambdaExprNode* enclosingLambda;  // the nearest enclosing lambda node, or null if none

    /** True if this is within a type-generic type definition. (Note, also true when reinterpreted if not all params are bound.) */
    bool typeGeneric;
    /** True if this is within a value-generic type definition. (Note, also true when reinterpreted if not all params are bound.) */
    bool valueGeneric;

    /** True if this is within a reinterpretation made with type-generic-dependent bindings. */
    bool typeGenDepBindings;
    /** True if this is within a reinterpretation made with value-generic-dependent bindings. */
    bool valueGenDepBindings;

    /** Constructs an "uninitialized" lexical context. */
    LexicalContext()
            : _scope(), reinterpretationDefiner(), expErrCtx(), enclosingLambda(),
              typeGeneric(), valueGeneric(), typeGenDepBindings(), valueGenDepBindings() {
    }

    /** Copy constructor. */
    LexicalContext( const LexicalContext& context ) = default;

    /** Move constructor. */
    LexicalContext( LexicalContext&& context ) = default;

    /** Constructs a lexical context for the provided module.
     * (A module context does not require a parent context.) */
    explicit LexicalContext( TxModule* module )
            : _scope( (TxScopeSymbol*) module ), reinterpretationDefiner(), expErrCtx(), enclosingLambda(),
              typeGeneric(), valueGeneric(), typeGenDepBindings(), valueGenDepBindings() {
        ASSERT( module, "module is NULL" );
    }

//    /** Constructs a lexical context that is a sub-context of the provided context.
//     * The provided scope must be the same or a sub-scope of the parent's scope. */
//    LexicalContext( const LexicalContext& parentContext, TxScopeSymbol* scope )
//            : _scope( scope ), reinterpretationDefiner( parentContext.reinterpretationDefiner ),
//              expErrCtx( parentContext.expErrCtx ), enclosingLambda( parentContext.enclosingLambda ),
//              typeGeneric( parentContext.typeGeneric ), valueGeneric( parentContext.valueGeneric ),
//              typeGenDepBindings( parentContext.typeGenDepBindings ), valueGenDepBindings( parentContext.valueGenDepBindings  ) {
//        ASSERT( scope, "scope is NULL" );
//    }

    /** Constructs a new lexical context for a given scope, and that may represent a reinterpretationDefiner of a lexical unit. */
    LexicalContext( TxScopeSymbol* scope, ExpectedErrorClause* expErrCtx, const TxTypeResolvingNode* reinterpretationDefiner,
                    bool typeGeneric, bool valueGeneric, bool typeGenDepBindings, bool valueGenDepBindings )
            : _scope( scope ), reinterpretationDefiner( reinterpretationDefiner ), expErrCtx( expErrCtx ), enclosingLambda(),
              typeGeneric( typeGeneric ), valueGeneric( valueGeneric ),
              typeGenDepBindings( typeGenDepBindings ), valueGenDepBindings( valueGenDepBindings ) {
        ASSERT( scope, "scope is NULL" );
    }

    /** Copy assignment operator. */
    LexicalContext& operator=( const LexicalContext& ) = default;

    /** Move assignment operator. */
    LexicalContext& operator=( LexicalContext && ) = default;


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


    /** Returns true if this context is within a generic type definition (a generic type whose parameters are not all bound)
     * or is dependent on generic bindings. */
    inline bool is_generic_dependent() const {
        return ( this->typeGeneric || this->valueGeneric || this->typeGenDepBindings || this->valueGenDepBindings );
    }

    inline bool is_type_generic() const {
        return ( this->typeGeneric );
    }
    inline bool is_value_generic() const {
        return ( this->valueGeneric );
    }
    inline bool is_type_gen_dep_bindings() const {
        return ( this->typeGenDepBindings );
    }
    inline bool is_value_gen_dep_bindings() const {
        return ( this->valueGenDepBindings );
    }

    /** Returns true if this is within a reinterpretationDefiner (specialization) of an AST. */
    inline bool is_reinterpretation() const {
        return ( this->reinterpretationDefiner );
    }

    /** If this is reinterpreted, the definer of the reinterpretation (that bound the type parameters) is returned; otherwise null. */
    inline const TxTypeResolvingNode* reinterpretation_definer() const {
        return ( this->reinterpretationDefiner );
    }

    /** Returns the ExpectedErrorClause if this context is within an expected-error declaration / statement / block,
     * otherwise nullptr. */
    inline ExpectedErrorClause* exp_error() const {
        return expErrCtx;
    }

    /** Returns the closest enclosing lambda of this context (the function body this is direct member of). */
    inline TxLambdaExprNode* enclosing_lambda() const {
        return this->enclosingLambda;
    }

    std::string str() const override {
        return _scope->str();
    }
};
