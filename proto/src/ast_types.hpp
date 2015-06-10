#pragma once

#include <algorithm>

#include "ast_base.hpp"


std::string make_generic_binding_name(const std::string& originalName);


/** Represents a binding for a type parameter. Can be either a Type or a Value parameter binding. */
class TxTypeArgumentNode : public TxNode {
    LexicalContext defContext;

    std::string paramDeclName;
    TxTypeDeclNode* typeDeclNode;
    TxFieldDeclNode* fieldDeclNode;
    bool bound = false;  // during development: checks invocation order

    /** To be called after symbol_declaration_pass() and before symbol_resolution_pass(). */
    void setup_declarations(const TxIdentifier& fullBaseTypeName, const TxTypeParam& param) {
        std::string qualPName = fullBaseTypeName.to_string() + "." + param.param_name();
        std::string declName = make_generic_binding_name(qualPName);
        if (! this->paramDeclName.empty()) {
            // the declaration nodes have already been set up in the initial generic pass
            if (declName != this->paramDeclName)
                // can happen if the parameterized type itself is a type parameter; this would change the full name;
                // unsupported (may cause structural differences between specializations?)
                CERROR(this, "Specialization of generic type parameter does not result in same parameter name: " << declName << "!=" << this->paramDeclName);
            return;
        }

        LOGGER().debug("%s: Binding %s in ctx %s", this->parse_loc_string().c_str(), declName.c_str(), this->context().scope()->get_full_name().to_string().c_str());
        ASSERT(!this->bound, "make_binding() called more than once for " << this);
        this->bound = true;
        if (this->typeExprNode) {
            if (param.meta_type() != param.TXB_TYPE)
                CERROR(this, "Provided a TYPE argument to VALUE parameter " << declName);
            // Shall be below the parent, like so:
            // $local.main$.$0.d$type.tx#Ref#T.tx#Array#E
            this->typeDeclNode = new TxTypeDeclNode(this->typeExprNode->parseLocation, TXD_PUBLIC | TXD_IMPLICIT,
                    declName, nullptr, this->typeExprNode);
        }
        else {
            ASSERT(this->valueExprNode, "Value expression not set in VALUE type parameter " << this);
            if (param.meta_type() != param.TXB_VALUE)
                CERROR(this, "Provided a TYPE argument to VALUE parameter " << declName);
            auto fieldDef = new TxFieldDefNode(this->valueExprNode->parseLocation, declName, this->valueExprNode, false, param.get_constraint_type_definer());
            // (passes the param's type-definer to the field def, so that proper type checking is done)
            this->fieldDeclNode = new TxFieldDeclNode(this->valueExprNode->parseLocation, TXD_PUBLIC | TXD_IMPLICIT, fieldDef);
        }
    }

public:
    TxTypeExpressionNode* typeExprNode;
    TxExpressionNode* valueExprNode;

    TxTypeArgumentNode(TxTypeExpressionNode* typeExprNode)
        : TxNode(typeExprNode->parseLocation), typeDeclNode(), fieldDeclNode(),
          typeExprNode(typeExprNode), valueExprNode() { }

    TxTypeArgumentNode(TxExpressionNode* valueExprNode)
        : TxNode(valueExprNode->parseLocation), typeDeclNode(), fieldDeclNode(),
          typeExprNode(), valueExprNode(valueExprNode) { }

    virtual void symbol_declaration_pass(LexicalContext& defContext, LexicalContext& lexContext) {
        this->defContext = defContext;
        this->set_context(lexContext);
    }

    /** Creates, registers and returns a newly created TxTypeBinding.
     * To be called after symbol_declaration_pass() and before symbol_resolution_pass(). */
    TxGenericBinding make_binding(const TxIdentifier& fullBaseTypeName, const TxTypeParam& param) {
        this->setup_declarations(fullBaseTypeName, param);

        if (this->typeDeclNode) {
            this->typeDeclNode->symbol_declaration_pass(this->defContext, this->context());
            return TxGenericBinding::make_type_binding(param.param_name(), this->typeExprNode);
        }
        else {
            ASSERT(this->fieldDeclNode, "Value expression not set in VALUE type parameter " << this);
            this->fieldDeclNode->symbol_declaration_pass(this->context());
            return TxGenericBinding::make_value_binding(param.param_name(), this->valueExprNode);
        }
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) {
        ASSERT(this->bound, "make_binding() has not been invoked on type argument " << this);
        if (this->typeDeclNode)
            this->typeDeclNode->symbol_resolution_pass(resCtx);
        else
            this->fieldDeclNode->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() {
        ASSERT(this->bound, "make_binding() has not been invoked on type argument " << this);
        if (this->typeDeclNode)
            this->typeDeclNode->semantic_pass();
        else
            this->fieldDeclNode->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** Represents a directly named, predefined type, or a pure specialization of a predefined generic type
 * (binding one or more type parameters).
 *
 * Note on generic capability:

type Abstr<E,C> { ... }

## we shall propably not allow *extension* of generic parameters:
type Field<E,C,V derives Abstr,L> derives Array<V<E,C>,L> {
    ## array elements are Abstr derivations stored by value
}

## Allowing this would probably require major redesign
## of the TxType specialization hierarchy.

## the closest we can support is:
type Field<E,C,L> derives Array<Ref<Abstr<E,C>>,L> {
    ## array elements are Abstr derivations stored by reference
}

 */
class TxPredefinedTypeNode : public TxTypeExpressionNode {
    LexicalContext defContext;

    const TxType* define_identified_type(ResolutionContext& resCtx);

    const TxType* define_generic_specialization_type(ResolutionContext& resCtx);

protected:
    virtual void symbol_declaration_pass_descendants(LexicalContext& defContext, LexicalContext& lexContext,
                                                     TxDeclarationFlags declFlags) override {
        for (TxTypeArgumentNode* tp : *this->typeArgs) {
            tp->symbol_declaration_pass(defContext, lexContext);
        }
    }

    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        const TxType* type;
        if (!this->typeArgs->empty())
            type = this->define_generic_specialization_type(resCtx);
        else
            type = this->define_identified_type(resCtx);
        if (! type)
            CERROR(this, "Unknown type: " << this->identNode->ident << " (from " << this->context().scope() << ")");
        return type;
    }

public:
    const TxIdentifierNode* identNode;
    const std::vector<TxTypeArgumentNode*>* const typeArgs;

    TxPredefinedTypeNode(const yy::location& parseLocation, const TxIdentifierNode* identifier,
                         const std::vector<TxTypeArgumentNode*>* typeArgs)
            : TxTypeExpressionNode(parseLocation), identNode(identifier), typeArgs(typeArgs)  {
        ASSERT(typeArgs, "NULL typeargs");
    }

    TxPredefinedTypeNode(const yy::location& parseLocation, const TxIdentifierNode* identifier)
        : TxTypeExpressionNode(parseLocation), identNode(identifier),
          typeArgs(new std::vector<TxTypeArgumentNode*>())  { }

    TxPredefinedTypeNode(const yy::location& parseLocation, const std::string& identifier)
        : TxTypeExpressionNode(parseLocation), identNode(new TxIdentifierNode(parseLocation, identifier)),
          typeArgs(new std::vector<TxTypeArgumentNode*>())  { }

    virtual bool has_predefined_type() const override {
        return this->typeArgs->empty();
    }

    virtual void symbol_declaration_pass(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                         const std::string designatedTypeName = std::string()) override;

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxTypeExpressionNode::symbol_resolution_pass(resCtx);
        for (TxTypeArgumentNode* ta : *this->typeArgs)
            ta->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        for (TxTypeArgumentNode* ta : *this->typeArgs)
            ta->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



/** Common superclass for specializations of the built-in types Ref and Array. */
class TxBuiltinTypeSpecNode : public TxTypeExpressionNode {
public:
    TxBuiltinTypeSpecNode(const yy::location& parseLocation) : TxTypeExpressionNode(parseLocation)  { }

    virtual void symbol_declaration_pass(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                         const std::string designatedTypeName = std::string()) override {
        if (designatedTypeName.empty()) {
            // ensure generic type specializations always have a declared type (handles e.g. Ref<Ref<Int>>)
            std::string typeName = "$type";
            TxTypeExpressionNode::symbol_declaration_pass(defContext, lexContext, declFlags | TXD_IMPLICIT, typeName);
        }
        else
            TxTypeExpressionNode::symbol_declaration_pass(defContext, lexContext, declFlags, designatedTypeName);
    }
};

/**
 * Custom AST node needed to handle dataspaces. */
class TxReferenceTypeNode : public TxBuiltinTypeSpecNode {
protected:
    virtual void symbol_declaration_pass_descendants(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        this->targetTypeNode->symbol_declaration_pass(defContext, lexContext);
    }

    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        auto baseType = this->types().get_builtin_type(REFERENCE);
        auto baseTypeName = baseType->get_declaration()->get_symbol()->get_full_name();
        TxGenericBinding binding = this->targetTypeNode->make_binding(baseTypeName, baseType->get_type_param("T"));
        const TxIdentifier* dataspace = (this->dataspace ? &this->dataspace->ident : nullptr);
        //cwarning("Dataspace: %s", (this->dataspace ? this->dataspace->ident.to_string().c_str() : "NULL"));
        return this->types().get_reference_type(this->get_declaration(), binding, dataspace);
    }

public:
    const TxIdentifierNode* dataspace;
    TxTypeArgumentNode* targetTypeNode;

    TxReferenceTypeNode(const yy::location& parseLocation, const TxIdentifierNode* dataspace,
                        TxTypeExpressionNode* targetType)
        : TxBuiltinTypeSpecNode(parseLocation), dataspace(dataspace),
          targetTypeNode(new TxTypeArgumentNode(targetType))  { }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxBuiltinTypeSpecNode::symbol_resolution_pass(resCtx);
        this->targetTypeNode->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() { targetTypeNode->semantic_pass(); }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/**
 * Custom AST node needed to provide syntactic sugar for modifiable declaration. */
class TxArrayTypeNode : public TxBuiltinTypeSpecNode {
protected:
    virtual void symbol_declaration_pass_descendants(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags) override;

    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        auto baseType = this->types().get_builtin_type(ARRAY);
        auto baseTypeName = baseType->get_declaration()->get_symbol()->get_full_name();
        TxGenericBinding elementBinding = this->elementTypeNode->make_binding(baseTypeName, baseType->get_type_param("E"));
        if (this->lengthNode) {
            TxGenericBinding lengthBinding = this->lengthNode->make_binding(baseTypeName, baseType->get_type_param("L"));
            return this->types().get_array_type(this->get_declaration(), elementBinding, lengthBinding);
        }
        else
            return this->types().get_array_type(this->get_declaration(), elementBinding);
    }

public:
    TxTypeArgumentNode* elementTypeNode;
    TxTypeArgumentNode* lengthNode;

    TxArrayTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* elementType, TxExpressionNode* lengthExpr=nullptr)
        : TxBuiltinTypeSpecNode(parseLocation),
          elementTypeNode(new TxTypeArgumentNode(elementType)),
          lengthNode(lengthExpr ? new TxTypeArgumentNode(lengthExpr) : nullptr)  { }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxBuiltinTypeSpecNode::symbol_resolution_pass(resCtx);
        this->elementTypeNode->symbol_resolution_pass(resCtx);
        if (this->lengthNode) {
            this->lengthNode->symbol_resolution_pass(resCtx);
            //if (! this->lengthNode->valueExprNode->is_statically_constant())
            //    CERROR(this, "Non-constant array length specifier not yet supported.");
        }
    }

    virtual void semantic_pass() {
        this->elementTypeNode->semantic_pass();
        if (this->lengthNode)
            this->lengthNode->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxDerivedTypeNode : public TxTypeExpressionNode {

    /** Wraps a TxTypeDefiner within a TxTypeExpressionNode. */
    class TxTypeWrapperNode : public TxTypeExpressionNode {
        TxTypeDefiner* typeDefiner;
    protected:
        virtual void symbol_declaration_pass_descendants(LexicalContext& defContext, LexicalContext& lexContext,
                                                         TxDeclarationFlags declFlags) override { }

        virtual const TxType* define_type(ResolutionContext& resCtx) override {
            return this->typeDefiner->resolve_type(resCtx);
        }

    public:
        TxTypeWrapperNode(const yy::location& parseLocation, TxTypeDefiner* typeDefiner)
            : TxTypeExpressionNode(parseLocation), typeDefiner(typeDefiner)  { }

        virtual void semantic_pass() override { }

        virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }
    };


    void init_implicit_types() {
        // implicit type members '$Self' and '$Super' for types with a body:
        auto selfTypeExprN = new TxTypeWrapperNode(this->parseLocation, this);
        auto selfRefTypeExprN = new TxReferenceTypeNode(this->parseLocation, nullptr, selfTypeExprN);
        const std::string selfTypeName = "$Self";
        this->selfRefTypeNode = new TxTypeDeclNode(this->parseLocation, TXD_IMPLICIT, selfTypeName, nullptr, selfRefTypeExprN);

        TxTypeDefiner* superDef = this->baseTypes->empty() ? new TxTypeDefWrapper(this->types().get_builtin_type(TUPLE))
                                                           : static_cast<TxTypeDefiner*>(this->baseTypes->at(0));
        auto superTypeExprN = new TxTypeWrapperNode(this->parseLocation, superDef);
        auto superRefTypeExprN = new TxReferenceTypeNode(this->parseLocation, nullptr, superTypeExprN);
        const std::string superTypeName = "$Super";
        this->superRefTypeNode = new TxTypeDeclNode(this->parseLocation, TXD_IMPLICIT, superTypeName, nullptr, superRefTypeExprN);
    }

protected:
    virtual void symbol_declaration_pass_descendants(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        for (auto baseType : *this->baseTypes)
            baseType->symbol_declaration_pass(defContext, lexContext, declFlags);

        this->selfRefTypeNode->symbol_declaration_pass(lexContext);
        this->superRefTypeNode->symbol_declaration_pass(lexContext);

        for (auto member : *this->members)
            member->symbol_declaration_pass(lexContext);

//        bool explicitConstructor = false;
//        for (auto member : *this->members) {
//            member->symbol_declaration_pass(lexContext);
//            if (auto fieldMember = dynamic_cast<TxFieldDeclNode*>(member)) {
//                if (fieldMember->field->get_field_name() == "$init")
//                    explicitConstructor = true;
//            }
//        }
//        if (! explicitConstructor) {
//            // add default constructor
//            auto funcType;
//            auto suite;
//            auto lambdaExpr = new TxLambdaExprNode(this->parseLocation, funcType, suite, true);
//            auto fieldDef = new TxFieldDefNode(this->parseLocation, "self", nullptr, lambdaExpr);
//            auto fieldDecl = new TxFieldDeclNode(this->parseLocation, TXD_PUBLIC, fieldDef, true);
//        }
    }

    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        auto declaration = this->get_declaration();
        ASSERT(declaration, "No declaration for derived type " << *this);
        // FUTURE: support interfaces
        const TxType* baseObjType = this->baseTypes->empty() ? this->types().get_builtin_type(TUPLE)
                                                             : this->baseTypes->at(0)->resolve_type(resCtx);
        if (! baseObjType)
            return nullptr;
        TxTypeSpecialization specialization(baseObjType, std::vector<TxGenericBinding>());
        // Note: Does not specify explicit type parameter bindings; any unbound type parameters
        //       of the base types are expected to match the declared type params.  FIXME: review
        // Note: Members are not necessarily resolved at this point.
        auto type = this->types().get_type_specialization(declaration, specialization, this->_mutable, this->declTypeParams);
        return type;
    }

public:
    const bool _mutable;
    std::vector<TxPredefinedTypeNode*>* baseTypes;
    std::vector<TxDeclarationNode*>* members;
    TxTypeDeclNode* selfRefTypeNode = nullptr;
    TxTypeDeclNode* superRefTypeNode = nullptr;

    TxDerivedTypeNode(const yy::location& parseLocation, const bool _mutable,
                      std::vector<TxPredefinedTypeNode*>* baseTypes,
                      std::vector<TxDeclarationNode*>* members)
            : TxTypeExpressionNode(parseLocation), _mutable(_mutable),
              baseTypes(baseTypes), members(members)  {
        this->init_implicit_types();
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxTypeExpressionNode::symbol_resolution_pass(resCtx);
        for (auto type : *this->baseTypes) {
            type->symbol_resolution_pass(resCtx);
            // TO DO: validity checks
        }

        this->selfRefTypeNode->symbol_resolution_pass(resCtx);
        this->superRefTypeNode->symbol_resolution_pass(resCtx);

        for (auto member : *this->members) {
            member->symbol_resolution_pass(resCtx);
            // TODO: can't put immutable instance member in non-immutable type (except via reference)
            //       (OR: disable whole-object-assignment)
        }
    }

    virtual void semantic_pass() {
        for (auto type : *this->baseTypes)
            type->semantic_pass();
        this->selfRefTypeNode->semantic_pass();
        this->superRefTypeNode->semantic_pass();
        for (auto member : *this->members)
            member->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxFunctionTypeNode : public TxTypeExpressionNode {
    // Note: the field names aren't part of a function's formal type definition
    /** If non-null, this function is a constructor and this is the object type that is being constructed. */
    TxTypeDeclaration* constructedObjTypeDecl = nullptr;

    static TxFieldDefNode* make_return_field(TxTypeExpressionNode* returnType) {
        if (returnType)
            return new TxFieldDefNode(returnType->parseLocation, "$return", returnType, nullptr);
        else
            return nullptr;
    }

protected:
    virtual void symbol_declaration_pass_descendants(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        // (processed as a function type and therefore doesn't declare (create entities for) the function args)
        for (auto argDef : *this->arguments)
            argDef->symbol_declaration_pass_functype_arg(lexContext);
        if (this->returnField)
            this->returnField->symbol_declaration_pass_functype_arg(lexContext);
    }

    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        std::vector<const TxType*> argumentTypes;
        for (auto argDefNode : *this->arguments)
            argumentTypes.push_back(argDefNode->resolve_type(resCtx));
        if (this->constructedObjTypeDecl)
            return this->types().get_constructor_type(this->get_declaration(), argumentTypes, this->constructedObjTypeDecl);
        else if (this->returnField)
            return this->types().get_function_type(this->get_declaration(), argumentTypes, this->returnField->resolve_type(resCtx), modifiable);
        else
            return this->types().get_function_type(this->get_declaration(), argumentTypes, modifiable);
    }

public:
    //const TxNamespaceIdentNode* dataspace;
    const bool modifiable;
    std::vector<TxFieldDefNode*>* arguments;
    TxFieldDefNode* returnField;
    // FUTURE: raised exceptions

    TxFunctionTypeNode(const yy::location& parseLocation, const bool modifiable,
                       std::vector<TxFieldDefNode*>* arguments,
                       TxTypeExpressionNode* returnType)
        : TxTypeExpressionNode(parseLocation), modifiable(modifiable),
          arguments(arguments), returnField(make_return_field(returnType)) { }

    void symbol_declaration_pass_func_header(LexicalContext& defContext, LexicalContext& lexContext, TxTypeDeclaration* constructedObjTypeDecl = nullptr) {
        // (processed as the function instance header, so declare the function args, and the return type if any)
        this->set_context(lexContext);
        this->constructedObjTypeDecl = constructedObjTypeDecl;
        for (auto argField : *this->arguments)
            argField->symbol_declaration_pass_local_field(lexContext, false);
        if (this->returnField)
            this->returnField->symbol_declaration_pass_local_field(lexContext, false);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxTypeExpressionNode::symbol_resolution_pass(resCtx);
        for (auto argDef : *this->arguments)
            argDef->symbol_resolution_pass(resCtx);
        if (this->returnField)
            this->returnField->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() {
        for (auto argDef : *this->arguments)
            argDef->semantic_pass();
        if (this->returnField)
            this->returnField->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxModifiableTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_declaration_pass_descendants(LexicalContext& defContext, LexicalContext& lexContext,
                                                     TxDeclarationFlags declFlags) override {
        this->baseType->symbol_declaration_pass(defContext, lexContext, declFlags);
    }

    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        if (auto bType = this->baseType->resolve_type(resCtx)) {
            if (bType->is_modifiable()) {
                CERROR(this, "'modifiable' specified more than once for type: " << bType);
                return bType;
            }
            else if (! bType->is_immutable())
                return this->types().get_modifiable_type(this->get_declaration(), bType);
            else
                CERROR(this, "Can't declare immutable type as modifiable: " << bType);
        }
        return nullptr;
    }

public:
    TxTypeExpressionNode* baseType;
    TxModifiableTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* baseType)
        : TxTypeExpressionNode(parseLocation), baseType(baseType) { }

    /** pass-through to baseType */
    virtual void setTypeParams(const std::vector<TxDeclarationNode*>* typeParamDeclNodes) override {
        this->baseType->setTypeParams(typeParamDeclNodes);
    }

    virtual void symbol_declaration_pass(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                         const std::string designatedTypeName = std::string()) override;

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxTypeExpressionNode::symbol_resolution_pass(resCtx);
        this->baseType->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() { this->baseType->semantic_pass(); }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** A potentially modifiable type expression, depending on syntactic sugar rules.
 * This node should not have TxModifiableTypeNode as parent, and vice versa. */
class TxMaybeModTypeNode : public TxModifiableTypeNode {
protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
        if (this->isModifiable)
            return TxModifiableTypeNode::define_type(resCtx);
        else
            return this->baseType->resolve_type(resCtx);
    }

public:
    bool isModifiable = false;

    TxMaybeModTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* baseType)
        : TxModifiableTypeNode(parseLocation, baseType) { }

    virtual bool has_predefined_type() const override;

    virtual void symbol_declaration_pass(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                         const std::string designatedTypeName = std::string()) override;
};
