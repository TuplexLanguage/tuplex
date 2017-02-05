#pragma once

#include "util/assert.hpp"


#include "symbol/symbol.hpp"

class TxModule;
class TxPackage;
class TxTypeDeclaration;
class TxEntityDeclaration;
class TxType;
class TxField;
class TxExpressionNode;
class LexicalContext;

//class TxNode;
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



/** Index of the current specialization pass. 0 represents the initial parse pass. */
typedef unsigned TxSpecializationIndex;


class TxSpecializationPass {
public:
    LexicalContext lexContext;
    LexicalContext defContext;
    const TxEntityDeclaration* declaration = nullptr;
    const TxType* type = nullptr;
    const TxField* field = nullptr;
    bool startedRslv = false;  // during development - guard against recursive resolution
    bool hasResolved = false;  // to prevent multiple identical error messages
    TxExpressionNode* inlinedExpression = nullptr;  // substitutes the function/constructor call if non-null
    std::vector<const TxType*>* appliedFuncArgTypes = nullptr;  // injected by outer expression if applicable

    TxSpecializationPass() : lexContext(), defContext()  { }
};


/** Trait that represents a specializable, type defining node. */
class TxSpecializableTypeDefiner : public virtual TxParseOrigin {
public:
    virtual ~TxSpecializableTypeDefiner() = default;

    virtual const TxType* resolve_type(TxSpecializationIndex six) = 0;

    virtual const TxType* attempt_get_type(TxSpecializationIndex six) const = 0;

    virtual const TxType* get_type(TxSpecializationIndex six) const = 0;
};


/** Trait that represents a specializable, field defining node. */
class TxSpecializableFieldDefiner : public TxSpecializableTypeDefiner {
public:
    virtual const TxExpressionNode* get_init_expression() const = 0;

    virtual const TxField* resolve_field(TxSpecializationIndex six) = 0;

    virtual const TxField* get_field(TxSpecializationIndex six) const = 0;
};


/** Proxy that represents a specific specialization of TxSpecializableTypeDefiner as a TxTypeDefiner. */
class TxSpecializationTypeDefiner : public TxTypeDefiner {
    TxSpecializableTypeDefiner* specDefiner;
    const TxSpecializationIndex six;

public:
    TxSpecializationTypeDefiner(TxSpecializableTypeDefiner* specDefiner, const TxSpecializationIndex six)
        : specDefiner(specDefiner), six(six)  { }

    virtual TxDriver* get_driver() const override { return specDefiner->get_driver(); }
    virtual const TxLocation& get_parse_location() const override { return specDefiner->get_parse_location(); }

    virtual const TxType* resolve_type() override { return specDefiner->resolve_type(six); }

    virtual const TxType* attempt_get_type() const override { return specDefiner->attempt_get_type(six); }

    virtual const TxType* get_type() const override { return specDefiner->get_type(six); }

    virtual TxTypeDefiningNode* get_node() const override;
    virtual TxSpecializationIndex get_six() const override { return this->six; }
};


/** Proxy that represents a specific specialization of TxSpecializableFieldDefiner as a TxFieldDefiner. */
class TxSpecializationFieldDefiner : public TxFieldDefiner {
    TxSpecializableFieldDefiner* specDefiner;
    const TxSpecializationIndex six;

public:
    TxSpecializationFieldDefiner(TxSpecializableFieldDefiner* specDefiner, const TxSpecializationIndex six)
        : specDefiner(specDefiner), six(six)  { }

    virtual TxDriver* get_driver() const override { return specDefiner->get_driver(); }
    virtual const TxLocation& get_parse_location() const override { return specDefiner->get_parse_location(); }

    virtual const TxExpressionNode* get_init_expression() const override { return specDefiner->get_init_expression(); }

    virtual const TxField* resolve_field() override { return specDefiner->resolve_field(six); }

    virtual const TxField* get_field() const override { return specDefiner->get_field(six); }

    virtual const TxType* resolve_type() override { return specDefiner->resolve_type(six); }

    virtual const TxType* attempt_get_type() const override { return specDefiner->attempt_get_type(six); }

    virtual const TxType* get_type() const override { return specDefiner->get_type(six); }
};
