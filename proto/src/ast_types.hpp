#pragma once

#include <algorithm>

#include "ast_base.hpp"


std::string make_generic_binding_name(const std::string& originalName);


/** Represents a binding for a type parameter. Can be either a Type or a Value parameter binding. */
class TxTypeArgumentNode : public TxNode {
    LexicalContext defContext;
    TxTypeDeclNode* typeDeclNode;
    TxFieldDeclNode* fieldDeclNode;
    bool bound = false;  // during development: checks invocation order
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

    /** Creates, registers and returns a newly created TxTypeBinding. May only be called once. */
    virtual TxGenericBinding make_binding(ResolutionContext& resCtx,
                                          const TxType* baseType, const TxTypeParam& param) {
        auto baseTypeEntity = baseType->entity();
        ASSERT(baseTypeEntity, "baseType has no entity: " << baseType);
        std::string qualPName = baseTypeEntity->get_full_name().to_string() + "." + param.param_name();
        std::string pname = make_generic_binding_name(qualPName);
        LOGGER().debug("%s: Binding %s in ctx %s", this->parse_loc_string().c_str(), pname.c_str(), this->context().scope()->get_full_name().to_string().c_str());
        ASSERT(!this->bound, "make_binding() called more than once for " << this);
        this->bound = true;
        if (this->typeExprNode) {
            if (param.meta_type() != param.TXB_TYPE)
                cerror("Provided a TYPE argument to VALUE parameter %s", pname.c_str());
            // Shall be below the parent, like so:
            // $local.main$.$0.d$type.tx#Ref#T.tx#Array#E
            this->typeDeclNode = new TxTypeDeclNode(this->typeExprNode->parseLocation, TXD_PUBLIC | TXD_IMPLICIT,
                                                    pname, nullptr, this->typeExprNode);
            this->typeDeclNode->symbol_declaration_pass(this->defContext, this->context());
            // best way to avoid over-resolving target type of Ref?
            if (! dynamic_cast<const TxReferenceType*>(baseType))
                this->typeExprNode->resolve_type(resCtx);
            else
                this->LOGGER().debug("%s: Skipping resolve_type() of reference", this->parse_loc_string().c_str());
            return TxGenericBinding::make_type_binding(param.param_name(), this->typeExprNode);
        }
        else {
            ASSERT(this->valueExprNode, "Value expression not set in VALUE type parameter " << this);
            if (param.meta_type() != param.TXB_VALUE)
                cerror("Provided a TYPE argument to VALUE parameter %s", pname.c_str());
            auto fieldDef = new TxFieldDefNode(this->valueExprNode->parseLocation, pname, this->valueExprNode, false, param.get_base_type_definer());
            // (passes the param's type-definer to the field def, so that proper type checking is done)
            this->fieldDeclNode = new TxFieldDeclNode(this->valueExprNode->parseLocation, TXD_PUBLIC | TXD_IMPLICIT, fieldDef);
            this->fieldDeclNode->symbol_declaration_pass(this->context());
            this->valueExprNode->resolve_type(resCtx);
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
    virtual void symbol_declaration_pass_descendants(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
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
            cerror("Unknown type: %s (from %s)", this->identNode->ident.to_string().c_str(), this->context().scope()->to_string().c_str());
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

    virtual bool has_predefined_type() const override {
        return this->typeArgs->empty();
    }

    virtual void symbol_declaration_pass(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                         const std::string designatedTypeName = std::string(),
                                         const std::vector<TxDeclarationNode*>* typeParamDecls = nullptr) override;

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
                                         const std::string designatedTypeName = std::string(),
                                         const std::vector<TxDeclarationNode*>* typeParamDecls = nullptr) override {
        if (designatedTypeName.empty()) {
            // ensure generic type specializations always have a declared type (handles e.g. Ref<Ref<Int>>)
            std::string typeName = "$type";
            TxTypeExpressionNode::symbol_declaration_pass(defContext, lexContext, declFlags | TXD_IMPLICIT, typeName, typeParamDecls);
        }
        else
            TxTypeExpressionNode::symbol_declaration_pass(defContext, lexContext, declFlags, designatedTypeName, typeParamDecls);
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
        TxGenericBinding binding = this->targetTypeNode->make_binding(resCtx, baseType, baseType->get_type_param("T"));
        const TxIdentifier* dataspace = (this->dataspace ? &this->dataspace->ident : nullptr);
        //cwarning("Dataspace: %s", (this->dataspace ? this->dataspace->ident.to_string().c_str() : "NULL"));
        return this->types().get_reference_type(this->get_entity(), binding, dataspace);
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
        TxGenericBinding elementBinding = this->elementTypeNode->make_binding(resCtx, baseType, baseType->get_type_param("E"));
        if (this->lengthNode) {
            TxGenericBinding lengthBinding = this->lengthNode->make_binding(resCtx, baseType, baseType->get_type_param("L"));
            return this->types().get_array_type(this->get_entity(), elementBinding, lengthBinding);
        }
        else
            return this->types().get_array_type(this->get_entity(), elementBinding);
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
            //    cerror("Non-constant array length specifier not yet supported.");
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
protected:
    virtual void symbol_declaration_pass_descendants(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        for (auto baseType : *this->baseTypes)
            baseType->symbol_declaration_pass(defContext, lexContext, declFlags);

        for (auto member : *this->members)
            member->symbol_declaration_pass(lexContext);
    }

    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        auto entity = this->get_entity();
        ASSERT(entity, "No entity declared for derived type " << *this);
        // FUTURE: support interfaces
        const TxType* baseObjType = this->baseTypes->empty() ? this->types().get_builtin_type(TUPLE)
                                                             : this->baseTypes->at(0)->resolve_type(resCtx);
        if (! baseObjType)
            return nullptr;
        TxTypeSpecialization specialization(baseObjType, std::vector<TxGenericBinding>());
        // Note: Does not specify explicit type parameter bindings; any unbound type parameters
        //       of the base types are expected to match the declared type params.  FIXME: review
        // Note: Members are not necessarily resolved at this point.
        auto type = this->types().get_type_specialization(entity, specialization, this->_mutable, this->declTypeParams);
        return type;
    }

public:
    const bool _mutable;
    std::vector<TxPredefinedTypeNode*>* baseTypes;
    std::vector<TxDeclarationNode*>* members;

    TxDerivedTypeNode(const yy::location& parseLocation, const bool _mutable,
                      std::vector<TxPredefinedTypeNode*>* baseTypes,
                      std::vector<TxDeclarationNode*>* members)
            : TxTypeExpressionNode(parseLocation), _mutable(_mutable),
              baseTypes(baseTypes), members(members)  {
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxTypeExpressionNode::symbol_resolution_pass(resCtx);
        for (auto type : *this->baseTypes)
            type->symbol_resolution_pass(resCtx);
        for (auto member : *this->members)
            member->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() {
        for (auto type : *this->baseTypes) {
            type->semantic_pass();
            // TO DO: validity checks
        }
        for (auto member : *this->members) {
            member->semantic_pass();
            // validity checks:
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
        if (this->returnField)
            return this->types().get_function_type(this->get_entity(), argumentTypes, this->returnField->resolve_type(resCtx), modifiable);
        else
            return this->types().get_function_type(this->get_entity(), argumentTypes, modifiable);
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

    void symbol_declaration_pass_func_header(LexicalContext& defContext, LexicalContext& lexContext) {
        // (processed as the function header, so declare the function args, and the return type if any)
        this->set_context(lexContext);
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
    virtual void symbol_declaration_pass_descendants(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        this->baseType->symbol_declaration_pass(defContext, lexContext, declFlags);
    }

    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        if (auto bType = this->baseType->resolve_type(resCtx)) {
            if (bType->is_modifiable()) {
                cerror("'modifiable' specified more than once for type: %s", bType->to_string().c_str());
                return bType;
            }
            else if (! bType->is_immutable())
                return this->types().get_modifiable_type(this->get_entity(), bType);
            else
                cerror("Can't declare immutable type as modifiable: %s", bType->to_string().c_str());
        }
        return nullptr;
    }

public:
    TxTypeExpressionNode* baseType;
    TxModifiableTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* baseType)
        : TxTypeExpressionNode(parseLocation), baseType(baseType) { }

    virtual void symbol_declaration_pass(LexicalContext& defContext, LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                         const std::string designatedTypeName = std::string(),
                                         const std::vector<TxDeclarationNode*>* typeParamDecls = nullptr) override;

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
                                         const std::string designatedTypeName = std::string(),
                                         const std::vector<TxDeclarationNode*>* typeParamDecls = nullptr) override;
};
