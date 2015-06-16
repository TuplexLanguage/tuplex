#pragma once

#include "assert.hpp"


#include "symbol.hpp"

class TxModule;
class TxPackage;
class TxTypeDeclaration;
class TxEntityDeclaration;
class TxType;
class TxField;
class TxExpressionNode;


/** Represents the lexical source scope of a syntax node / entity.
 */
class LexicalContext : public Printable {
    TxScopeSymbol* _scope;
    TxTypeDeclaration* constructedObjTypeDecl;

    static TxModule* get_module(TxScopeSymbol* scope);

public:
    LexicalContext() : _scope(), constructedObjTypeDecl()  { }

    LexicalContext(TxScopeSymbol* scope, TxTypeDeclaration* constructedEntity=nullptr)
            : _scope(scope), constructedObjTypeDecl(constructedEntity)  {
        ASSERT(scope, "scope is NULL");
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

    virtual const TxType* resolve_type(TxSpecializationIndex six, ResolutionContext& resCtx) = 0;

    virtual const TxType* attempt_get_type(TxSpecializationIndex six) const = 0;

    virtual const TxType* get_type(TxSpecializationIndex six) const = 0;
};


/** Trait that represents a specializable, field defining node. */
class TxSpecializableFieldDefiner : public TxSpecializableTypeDefiner {
public:
    virtual const TxExpressionNode* get_init_expression() const = 0;

    virtual const TxField* resolve_field(TxSpecializationIndex six, ResolutionContext& resCtx) = 0;

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
    virtual const yy::location& get_parse_location() const override { return specDefiner->get_parse_location(); }

    virtual const TxType* resolve_type(ResolutionContext& resCtx) override { return specDefiner->resolve_type(six, resCtx); }

    virtual const TxType* attempt_get_type() const override { return specDefiner->attempt_get_type(six); }

    virtual const TxType* get_type() const override { return specDefiner->get_type(six); }

    virtual TxTypeDefiningNode* get_node() const override;
};


/** Proxy that represents a specific specialization of TxSpecializableFieldDefiner as a TxFieldDefiner. */
class TxSpecializationFieldDefiner : public TxFieldDefiner {
    TxSpecializableFieldDefiner* specDefiner;
    const TxSpecializationIndex six;

public:
    TxSpecializationFieldDefiner(TxSpecializableFieldDefiner* specDefiner, const TxSpecializationIndex six)
        : specDefiner(specDefiner), six(six)  { }

    virtual TxDriver* get_driver() const override { return specDefiner->get_driver(); }
    virtual const yy::location& get_parse_location() const override { return specDefiner->get_parse_location(); }

    virtual const TxExpressionNode* get_init_expression() const override { return specDefiner->get_init_expression(); }

    virtual const TxField* resolve_field(ResolutionContext& resCtx) override { return specDefiner->resolve_field(six, resCtx); }

    virtual const TxField* get_field() const override { return specDefiner->get_field(six); }

    virtual const TxType* resolve_type(ResolutionContext& resCtx) override { return specDefiner->resolve_type(six, resCtx); }

    virtual const TxType* attempt_get_type() const override { return specDefiner->attempt_get_type(six); }

    virtual const TxType* get_type() const override { return specDefiner->get_type(six); }
};


/** TxSpecializableTypeDefiner wrapper for a "fixed" TxTypeDefiner (independent of specialization index). */
class TxFixedTypeDefiner : public TxSpecializableTypeDefiner {  // currently unused, remove?
    TxTypeDefiner* const definer;

public:
    TxFixedTypeDefiner(TxTypeDefiner* definer) : definer(definer)  { }

    virtual TxDriver* get_driver() const override final { return definer->get_driver(); }
    virtual const yy::location& get_parse_location() const override final { return definer->get_parse_location(); }

    virtual const TxType* resolve_type(TxSpecializationIndex six, ResolutionContext& resCtx) override final { return this->definer->resolve_type(resCtx); }

    virtual const TxType* attempt_get_type(TxSpecializationIndex six) const override final { return this->definer->attempt_get_type(); }

    virtual const TxType* get_type(TxSpecializationIndex six) const override final { return this->definer->get_type(); }
};


//class TxNode;
//class TxTypeExpressionNode;
//class TxExpressionNode;
//class TxFieldDefNode;
//class TxAssigneeNode;
//class TxTypeArgumentNode;
//class TxPredefinedTypeNode;
//class TxFieldValueNode;
//class TxReferenceDerefNode;
//class TxConstructorCalleeExprNode;


//class TxNode : public virtual TxParseOrigin, public Printable {
//    LexicalContext lexContext;

//class TxTypeExpressionNode : public TxNode, public TxTypeDefiner {
//    bool gettingType = false;  // during development - guard against recursive calls to get_type()
//    bool gottenType = false;  // to prevent multiple identical error messages
//    TxType const * cachedType = nullptr;
//    TxTypeDeclaration* typeDeclaration = nullptr;  // null unless initialized in symbol declaration pass

//class TxExpressionNode : public TxNode, public TxTypeDefiner {
//    bool gettingType = false;  // during development - guard against recursive calls to get_type()
//    bool gottenType = false;  // to prevent multiple identical error messages
//    TxType const * cachedType = nullptr;
//    std::vector<const TxType*>* appliedFuncArgTypes = nullptr; // injected by expression context if applicable

//class TxFieldDefNode : public TxNode, public TxFieldDefiner {
//    TxField const * cachedField = nullptr;
//    TxType const * cachedType = nullptr;
//    TxFieldDeclaration* fieldDeclaration = nullptr;  // null until initialized in symbol declaration pass

//class TxAssigneeNode : public TxNode {
//    bool resolved = false;
//    TxType const * cachedType = nullptr;

//class TxTypeArgumentNode : public TxNode {
//    LexicalContext defContext;

//class TxPredefinedTypeNode : public TxTypeExpressionNode {
//    LexicalContext defContext;


//class TxFieldValueNode : public TxExpressionNode {
//    TxScopeSymbol* cachedSymbol = nullptr;
//    const TxField* cachedField = nullptr;
//    bool hasRunResolve = false;

//class TxReferenceDerefNode : public TxExpressionNode {
    /** internal "cache" to prevent multiple code generations */
    //mutable llvm::Value* refExprValue = nullptr;

//class TxConstructorCalleeExprNode : public TxExpressionNode {
//    /** The constructor method's declaration */
//    const TxFieldDeclaration* constructorDecl = nullptr;
    //mutable llvm::Value* objectPtrV = nullptr;
