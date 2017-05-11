#pragma once

#include "util/assert.hpp"

#include "tx_error.hpp"

#include "symbol/symbol.hpp"
#include "symbol/module.hpp"

class TxModule;
class TxPackage;
class TxTypeDeclaration;
class TxStatementNode;
class TxLambdaExprNode;
class TxTypeDeclNode;

/* forward declarations pertaining to LLVM code generation */
class LlvmGenerationContext;
class GenScope;
namespace llvm {
class Value;
}

/** Represents the lexical source scope of a syntax node / entity.
 */
class LexicalContext : public Printable {
public:
    TxScopeSymbol* _scope;
    const TxTypeDeclaration* constructedObjTypeDecl;
    bool generic;  // true if this is within a generic type definition (note, can be true also when reinterpreted, if not all params bound)
    bool reinterpretation;  // true if this is a reinterpretation (specialization) of an AST
    ExpectedErrorClause* expErrCtx;

    /** Constructs an "uninitialized" lexical context. */
    LexicalContext() //= default;
            : _scope(), constructedObjTypeDecl(), generic(), reinterpretation(), expErrCtx() {
    }

    /** Copy constructor. */
    LexicalContext( const LexicalContext& context ) = default;

    /** Constructs a lexical context for the provided module.
     * (A module context does not require a parent context.) */
    LexicalContext( TxModule* module )
            : _scope( (TxScopeSymbol*) module ), constructedObjTypeDecl(), generic(), reinterpretation(), expErrCtx() {
        ASSERT( module, "module is NULL" );
    }

    /** Constructs a lexical context that is a sub-context of the provided context.
     * The provided scope must be the same or a sub-scope of the parent's scope. */
    LexicalContext( const LexicalContext& parentContext, TxScopeSymbol* scope )
            : _scope( scope ), constructedObjTypeDecl( parentContext.constructedObjTypeDecl ),
              generic( parentContext.generic ),
              reinterpretation( parentContext.reinterpretation ),
              expErrCtx( parentContext.expErrCtx ) {
        ASSERT( scope, "scope is NULL" );
    }

//    /** Constructs a lexical context that is a sub-context of the provided context, and may be in context of a constructor.
//     * The provided scope must be the same or a sub-scope of the parent's scope. */
//    LexicalContext( const LexicalContext& parentContext, TxScopeSymbol* scope, const TxTypeDeclaration* constructedObjTypeDecl )
//            : _scope( scope ), constructedObjTypeDecl( constructedObjTypeDecl ),
//              generic( parentContext.generic ),
//              reinterpretation( parentContext.reinterpretation ),
//              expErrCtx( parentContext.expErrCtx ) {
//        ASSERT( scope, "scope is NULL" );
//    }

//    /** Constructs a lexical context that is a sub-context of the provided context, and is an expected-error context.
//     * The provided scope must be the same or a sub-scope of the parent's scope. */
//    LexicalContext( const LexicalContext& parentContext, TxScopeSymbol* scope, ExpectedErrorClause* expErrCtx )
//            : _scope( scope ), constructedObjTypeDecl( parentContext.constructedObjTypeDecl ),
//              generic( parentContext.generic ),
//              reinterpretation( parentContext.reinterpretation ),
//              expErrCtx( expErrCtx ) {
//        ASSERT( scope, "scope is NULL" );
//    }

//    /** Constructs a lexical context that is a copy of the provided context and has the specified generic flag. */
//    LexicalContext( const LexicalContext& parentContext, bool generic )
//            : _scope( parentContext._scope ), constructedObjTypeDecl( parentContext.constructedObjTypeDecl ),
//              generic( generic ), reinterpretation( parentContext.reinterpretation ), expErrCtx( parentContext.expErrCtx ) {
//    }

    /** Constructs a new lexical context for a given scope, and that may represent a reinterpretation of a lexical unit. */
    LexicalContext( TxScopeSymbol* scope, ExpectedErrorClause* expErrCtx, bool generic, bool reinterpretation )
            : _scope( scope ), constructedObjTypeDecl(), generic( generic ), reinterpretation( reinterpretation ), expErrCtx( expErrCtx ) {
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

    /** Returns true if this is within a reinterpretation (specialization) of an AST. */
    inline bool is_reinterpretation() const {
        return ( this->reinterpretation );
    }

    /** Returns the ExpectedErrorClause if this context is within an expected-error declaration / statement / block,
     * otherwise nullptr. */
    inline ExpectedErrorClause* exp_error() const {
        return expErrCtx;
    }

    /** If non-null, this context is within a constructor and the declaration for the constructed object type is returned. */
    inline const TxTypeDeclaration* get_constructed() const {
        return this->constructedObjTypeDecl;
    }

    std::string str() const override {
        return _scope->str();
    }
};
