#pragma once

#include "ast_base.hpp"



/** Represents a binding for a type parameter. Can be either a Type or a Value parameter binding. */
class TxTypeArgumentNode : public TxSpecializableNode {
public:
    TxTypeExpressionNode* typeExprNode;
    TxExpressionNode* valueExprNode;

    TxTypeArgumentNode(TxTypeExpressionNode* typeExprNode)
        : TxSpecializableNode(typeExprNode->parseLocation),
          typeExprNode(typeExprNode), valueExprNode() { }

    TxTypeArgumentNode(TxExpressionNode* valueExprNode)
        : TxSpecializableNode(valueExprNode->parseLocation),
          typeExprNode(), valueExprNode(valueExprNode) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& defContext, LexicalContext& lexContext) {
        this->get_spec(six)->defContext = defContext;
        this->set_context(six, lexContext);
    }

    /** Creates, registers and returns a newly created TxTypeBinding.
     * To be called after symbol_declaration_pass() and before symbol_resolution_pass(). */
    TxGenericBinding make_binding(TxSpecializationIndex six, const TxIdentifier& fullBaseTypeName, const TxEntityDeclaration* paramDecl) {
        //this->setup_declarations(fullBaseTypeName, paramDecl);
        if (this->typeExprNode) {
            auto spec = this->get_spec(six);
            this->typeExprNode->symbol_declaration_pass(six, spec->defContext, spec->lexContext, TXD_PUBLIC, "", nullptr);
            return TxGenericBinding::make_type_binding(paramDecl->get_unique_name(), this->typeExprNode->get_type_definer(six));
        }
        else {
            ASSERT(this->valueExprNode, "Value expression not set in VALUE type parameter " << this);
            this->valueExprNode->symbol_declaration_pass(six, this->context(six));
            return TxGenericBinding::make_value_binding(paramDecl->get_unique_name(), this->valueExprNode->get_value_definer(six));
        }
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) {
        if (this->typeExprNode)
            this->typeExprNode->symbol_resolution_pass(six, resCtx);
        else
            this->valueExprNode->symbol_resolution_pass(six, resCtx);
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
    const TxType* define_identified_type(TxSpecializationIndex six, ResolutionContext& resCtx);

    const TxType* define_generic_specialization_type(TxSpecializationIndex six, ResolutionContext& resCtx);

protected:
    virtual void symbol_declaration_pass_descendants(TxSpecializationIndex six, LexicalContext& defContext,
                                                     LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        for (TxTypeArgumentNode* tp : *this->typeArgs) {
            tp->symbol_declaration_pass(six, defContext, lexContext);
        }
    }

    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        const TxType* type;
        if (!this->typeArgs->empty())
            type = this->define_generic_specialization_type(six, resCtx);
        else
            type = this->define_identified_type(six, resCtx);
        if (! type)
            CERROR(this, "Unknown type: " << this->identNode->ident << " (from " << this->context(six).scope() << ")");
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

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                         const std::string designatedTypeName,
                                         const std::vector<TxDeclarationNode*>* typeParamDeclNodes) override;

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxTypeExpressionNode::symbol_resolution_pass(six, resCtx);
        for (TxTypeArgumentNode* ta : *this->typeArgs)
            ta->symbol_resolution_pass(six, resCtx);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



/** Common superclass for specializations of the built-in types Ref and Array. */
class TxBuiltinTypeSpecNode : public TxTypeExpressionNode {
public:
    TxBuiltinTypeSpecNode(const yy::location& parseLocation) : TxTypeExpressionNode(parseLocation)  { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& defContext, LexicalContext& lexContext,
                                         TxDeclarationFlags declFlags, const std::string designatedTypeName,
                                         const std::vector<TxDeclarationNode*>* typeParamDeclNodes) override {
        if (designatedTypeName.empty()) {
            // ensure generic type specializations always have a declared type (handles e.g. Ref<Ref<Int>>)
            std::string typeName = "$type";
            TxTypeExpressionNode::symbol_declaration_pass(six, defContext, lexContext, declFlags | TXD_IMPLICIT, typeName, typeParamDeclNodes);
        }
        else
            TxTypeExpressionNode::symbol_declaration_pass(six, defContext, lexContext, declFlags, designatedTypeName, typeParamDeclNodes);
    }
};

/**
 * Custom AST node needed to handle dataspaces. */
class TxReferenceTypeNode : public TxBuiltinTypeSpecNode {
protected:
    virtual void symbol_declaration_pass_descendants(TxSpecializationIndex six, LexicalContext& defContext,
                                                     LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        this->targetTypeNode->symbol_declaration_pass(six, defContext, lexContext);
    }

    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        auto baseType = this->types().get_builtin_type(REFERENCE);
        auto baseTypeName = baseType->get_declaration()->get_symbol()->get_full_name();
        TxGenericBinding binding = this->targetTypeNode->make_binding(six, baseTypeName, baseType->get_type_param_decl("T"));
        const TxIdentifier* dataspace = (this->dataspace ? &this->dataspace->ident : nullptr);
        //cwarning("Dataspace: %s", (this->dataspace ? this->dataspace->ident.to_string().c_str() : "NULL"));
        return this->types().get_reference_type(this->get_declaration(six), binding, dataspace);
    }

public:
    const TxIdentifierNode* dataspace;
    TxTypeArgumentNode* targetTypeNode;

    TxReferenceTypeNode(const yy::location& parseLocation, const TxIdentifierNode* dataspace,
                        TxTypeExpressionNode* targetType)
        : TxBuiltinTypeSpecNode(parseLocation), dataspace(dataspace),
          targetTypeNode(new TxTypeArgumentNode(targetType))  { }

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxBuiltinTypeSpecNode::symbol_resolution_pass(six, resCtx);
        this->targetTypeNode->symbol_resolution_pass(six, resCtx);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/**
 * Custom AST node needed to provide syntactic sugar for modifiable declaration. */
class TxArrayTypeNode : public TxBuiltinTypeSpecNode {
protected:
    virtual void symbol_declaration_pass_descendants(TxSpecializationIndex six, LexicalContext& defContext,
                                                     LexicalContext& lexContext, TxDeclarationFlags declFlags) override;

    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        auto baseType = this->types().get_builtin_type(ARRAY);
        auto baseTypeName = baseType->get_declaration()->get_symbol()->get_full_name();
        TxGenericBinding elementBinding = this->elementTypeNode->make_binding(six, baseTypeName, baseType->get_type_param_decl("E"));
        if (this->lengthNode) {
            TxGenericBinding lengthBinding = this->lengthNode->make_binding(six, baseTypeName, baseType->get_type_param_decl("L"));
            return this->types().get_array_type(this->get_declaration(six), elementBinding, lengthBinding);
        }
        else
            return this->types().get_array_type(this->get_declaration(six), elementBinding);
    }

public:
    TxTypeArgumentNode* elementTypeNode;
    TxTypeArgumentNode* lengthNode;

    TxArrayTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* elementType, TxExpressionNode* lengthExpr=nullptr)
        : TxBuiltinTypeSpecNode(parseLocation),
          elementTypeNode(new TxTypeArgumentNode(elementType)),
          lengthNode(lengthExpr ? new TxTypeArgumentNode(lengthExpr) : nullptr)  { }

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxBuiltinTypeSpecNode::symbol_resolution_pass(six, resCtx);
        this->elementTypeNode->symbol_resolution_pass(six, resCtx);
        if (this->lengthNode) {
            this->lengthNode->symbol_resolution_pass(six, resCtx);
            //if (! this->lengthNode->valueExprNode->is_statically_constant())
            //    CERROR(this, "Non-constant array length specifier not yet supported.");
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxDerivedTypeNode : public TxTypeExpressionNode {
    /** Initialized implicit type members such as '$Self' and '$Super' for types with a body. */
    void init_implicit_types();

protected:
    virtual void symbol_declaration_pass_descendants(TxSpecializationIndex six, LexicalContext& defContext,
                                                     LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        for (auto baseType : *this->baseTypes)
            baseType->symbol_declaration_pass(six, defContext, lexContext, declFlags, "", nullptr);

        this->selfRefTypeNode->symbol_declaration_pass(six, lexContext);
        this->superRefTypeNode->symbol_declaration_pass(six, lexContext);

        for (auto member : *this->members)
            member->symbol_declaration_pass(six, lexContext);

//        bool explicitConstructor = false;
//        for (auto member : *this->members) {
//            member->symbol_declaration_pass(six, lexContext);
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

    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        auto declaration = this->get_declaration(six);
        ASSERT(declaration, "No declaration for derived type " << *this);

        const TxType* baseObjType = nullptr;
        std::vector<TxTypeSpecialization> interfaces;
        if (this->baseTypes->empty())
            baseObjType = this->types().get_builtin_type(TUPLE);
        else {
            interfaces.reserve(this->baseTypes->size()-1);
            for (size_t i = 0; i < this->baseTypes->size(); i++) {
                if (auto baseType = this->baseTypes->at(i)->resolve_type(six, resCtx)) {
                    if (i == 0)
                        baseObjType = baseType;
                    else {
                        if (baseType->get_type_class() != TXTC_INTERFACE)
                            CERROR(this, "Only the first derived-from type can be a non-interface type: " << baseType);
                        interfaces.emplace_back(baseType);
                    }
                }
                else
                    return nullptr;
            }
        }

        auto declTypeParams = make_decl_type_params(six);
        auto type = this->types().get_type_specialization(declaration, baseObjType, interfaces, nullptr, declTypeParams, this->_mutable);
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

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxTypeExpressionNode::symbol_resolution_pass(six, resCtx);
        for (auto type : *this->baseTypes) {
            type->symbol_resolution_pass(six, resCtx);
        }

        this->selfRefTypeNode->symbol_resolution_pass(six, resCtx);
        this->superRefTypeNode->symbol_resolution_pass(six, resCtx);

        for (auto member : *this->members) {
            member->symbol_resolution_pass(six, resCtx);
            // TODO: can't put immutable instance member in non-immutable type (except via reference)
            //       (OR: disable whole-object-assignment)
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxFunctionTypeNode : public TxTypeExpressionNode {
    // Note: the field names aren't part of a function's formal type definition

    static TxFieldDefNode* make_return_field(TxTypeExpressionNode* returnType) {
        if (returnType)
            return new TxFieldDefNode(returnType->parseLocation, "$return", returnType, nullptr);
        else
            return nullptr;
    }

protected:
    virtual void symbol_declaration_pass_descendants(TxSpecializationIndex six, LexicalContext& defContext,
                                                     LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        // (processed as a function type and therefore doesn't declare (create entities for) the function args)
        for (auto argDef : *this->arguments)
            argDef->symbol_declaration_pass_functype_arg(six, lexContext);
        if (this->returnField)
            this->returnField->symbol_declaration_pass_functype_arg(six, lexContext);
    }

    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        std::vector<const TxType*> argumentTypes;
        for (auto argDefNode : *this->arguments)
            argumentTypes.push_back(argDefNode->resolve_type(six, resCtx));
        if (this->context(six).get_constructed())
            return this->types().get_constructor_type(this->get_declaration(six), argumentTypes, this->context(six).get_constructed());
        else if (this->returnField)
            return this->types().get_function_type(this->get_declaration(six), argumentTypes, this->returnField->resolve_type(six, resCtx), modifiable);
        else
            return this->types().get_function_type(this->get_declaration(six), argumentTypes, modifiable);
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

    void symbol_declaration_pass_func_header(TxSpecializationIndex six, LexicalContext& lexContext) {
        // (processed as the function instance header, so declare the function args, and the return type if any)
        this->set_context(six, lexContext);
        for (auto argField : *this->arguments)
            argField->symbol_declaration_pass_local_field(six, lexContext, false);
        if (this->returnField)
            this->returnField->symbol_declaration_pass_local_field(six, lexContext, false, TXD_IMPLICIT);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxTypeExpressionNode::symbol_resolution_pass(six, resCtx);
        for (auto argDef : *this->arguments)
            argDef->symbol_resolution_pass(six, resCtx);
        if (this->returnField)
            this->returnField->symbol_resolution_pass(six, resCtx);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxModifiableTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_declaration_pass_descendants(TxSpecializationIndex six, LexicalContext& defContext,
                                                     LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        this->baseType->symbol_declaration_pass(six, defContext, lexContext, declFlags, "", nullptr);
    }

    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        if (auto bType = this->baseType->resolve_type(six, resCtx)) {
            if (bType->is_modifiable()) {
                CERROR(this, "'modifiable' specified more than once for type: " << bType);
                return bType;
            }
            else if (! bType->is_immutable())
                return this->types(six).get_modifiable_type(this->get_declaration(six), bType);
            else
                CERROR(this, "Can't declare immutable type as modifiable: " << bType);
        }
        return nullptr;
    }

public:
    TxTypeExpressionNode* baseType;
    TxModifiableTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* baseType)
        : TxTypeExpressionNode(parseLocation), baseType(baseType) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                         const std::string designatedTypeName,
                                         const std::vector<TxDeclarationNode*>* typeParamDeclNodes) override;

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxTypeExpressionNode::symbol_resolution_pass(six, resCtx);
        this->baseType->symbol_resolution_pass(six, resCtx);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** A potentially modifiable type expression, depending on syntactic sugar rules.
 * This node should not have TxModifiableTypeNode as parent, and vice versa. */
class TxMaybeModTypeNode : public TxModifiableTypeNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
        if (this->isModifiable)
            return TxModifiableTypeNode::define_type(six, resCtx);
        else
            return this->baseType->resolve_type(six, resCtx);
    }

public:
    bool isModifiable = false;

    TxMaybeModTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* baseType)
        : TxModifiableTypeNode(parseLocation, baseType) { }

    virtual bool has_predefined_type() const override;

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                         const std::string designatedTypeName,
                                         const std::vector<TxDeclarationNode*>* typeParamDeclNodes) override;
};
