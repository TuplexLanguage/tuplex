#pragma once

#include <algorithm>

#include "ast_base.hpp"


/** Common superclass for TxSpecializedTypeNode and TxIdentifiedTypeNode */
class TxPredefinedTypeNode : public TxTypeExpressionNode {
protected:
    TxPredefinedTypeNode(const yy::location& parseLocation) : TxTypeExpressionNode(parseLocation)  { }
};


/** Represents a binding for a type parameter. Can be either a Type or a Value parameter binding. */
class TxTypeArgumentNode : public TxNode {
    TxTypeDeclNode* typeDeclNode;
    TxFieldDeclNode* fieldDeclNode;
    bool bound = false;
public:
    TxTypeExpressionNode* typeExprNode;
    TxExpressionNode* valueExprNode;

    TxTypeArgumentNode(TxTypeExpressionNode* typeExprNode)
        : TxNode(typeExprNode->parseLocation), typeDeclNode(), fieldDeclNode(),
          typeExprNode(typeExprNode), valueExprNode() { }

    TxTypeArgumentNode(TxExpressionNode* valueExprNode)
        : TxNode(valueExprNode->parseLocation), typeDeclNode(), fieldDeclNode(),
          typeExprNode(), valueExprNode(valueExprNode) { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
    }

    inline TxTypeBinding make_binding(const TxTypeEntity* baseTypeEntity, const TxTypeParam& param) {
        std::string pname = baseTypeEntity->get_full_name().to_string();
        std::replace(pname.begin(), pname.end(), '.', '#');
        pname += '#';
        pname += param.param_name();
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
            this->typeDeclNode->symbol_table_pass(this->context());
            return TxTypeBinding(param.param_name(), this->typeExprNode);
        }
        else {
            ASSERT(this->valueExprNode, "Value expression not set in VALUE type parameter " << this);
            if (param.meta_type() != param.TXB_VALUE)
                cerror("Provided a TYPE argument to VALUE parameter %s", pname.c_str());
            auto fieldDef = new TxFieldDefNode(this->valueExprNode->parseLocation, pname, this->valueExprNode);
            this->fieldDeclNode = new TxFieldDeclNode(this->valueExprNode->parseLocation, TXD_PUBLIC | TXD_STATIC | TXD_IMPLICIT, fieldDef);
            this->fieldDeclNode->symbol_table_pass(this->context());
            return TxTypeBinding(param.param_name(), this->valueExprNode);
        }
    }

//    virtual TxTypeEntity* get_entity() const {
//        return (this->typeNode ? this->typeNode->get_entity() : nullptr);
//    }
//
//    virtual const TxType* get_type() const {
//        return (this->typeNode ? this->typeNode->get_type() : this->valueNode->get_type());
//    }

    virtual void semantic_pass() {
        ASSERT(this->bound, "make_binding() has not been invoked on type argument " << this);
        if (this->typeDeclNode)
            this->typeDeclNode->semantic_pass();
        else
            this->fieldDeclNode->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};

/** Represents a specialization of a generic type - binding one or more type parameters of a predefined, generic type.
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
class TxSpecializedTypeNode : public TxPredefinedTypeNode {
protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        for (TxTypeArgumentNode* tp : *this->typeArgs) {
            tp->symbol_table_pass(lexContext);
        }
    }

public:
    const TxIdentifierNode* identNode;
    const std::vector<TxTypeArgumentNode*>* const typeArgs;

    TxSpecializedTypeNode(const yy::location& parseLocation, const TxIdentifierNode* identifier,
                          const std::vector<TxTypeArgumentNode*>* typeArgs)
            : TxPredefinedTypeNode(parseLocation), identNode(identifier), typeArgs(typeArgs)  {
    }

    virtual void symbol_table_pass(LexicalContext& lexContext, TxDeclarationFlags declFlags, TxTypeEntity* declaredEntity = nullptr,
                                   const std::vector<TxDeclarationNode*>* typeParamDecls = nullptr) override {
        if (! declaredEntity) {
            // ensure generic type specializations always have a declared type (handles e.g. Ref<Ref<Int>>)
            std::string typeName = "$type";
            //printf("%s: Declaring '%s' in '%s' as implicit specialized type\n", this->parse_loc_string().c_str(), typeName.c_str(), lexContext.scope()->get_full_name().to_string().c_str());
            auto declaredEntity = lexContext.scope()->declare_type(typeName, this, TXD_PUBLIC);
            if (!declaredEntity)
                cerror("Failed to declare implicit type %s", typeName.c_str());
            LexicalContext typeCtx(declaredEntity ? declaredEntity : lexContext.scope());  // (in case declare_type() yields NULL)
            TxPredefinedTypeNode::symbol_table_pass(typeCtx, declFlags, declaredEntity, typeParamDecls);
        }
        else
            TxPredefinedTypeNode::symbol_table_pass(lexContext, declFlags, declaredEntity, typeParamDecls);
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        auto baseTypeEntity = this->context().scope()->resolve_type(this->identNode->ident);
        if (! baseTypeEntity) {
            if (errorMsg)
                errorMsg->append("Unknown type: " + this->identNode->ident.to_string() + " (from " + this->context().scope()->to_string() + ")");
            return nullptr;
        }
        auto baseType = baseTypeEntity->get_type();
        if (baseType->type_params().size() < this->typeArgs->size()) {
            cerror("Too many generic type arguments specified for type %s", identNode->ident.to_string().c_str());
            return nullptr;
        }
        std::vector<TxTypeBinding> bindings; // e.g. { TxTypeBinding("E", elemType), TxTypeBinding("L", length) }
        for (int i = 0; i < this->typeArgs->size(); i++) {
            //for (TxTypeParam tp : baseType->type_params()) {
            bindings.push_back(this->typeArgs->at(i)->make_binding(baseTypeEntity, baseType->type_params().at(i)));
        }
        TxTypeSpecialization specialization(baseType, bindings);
        return this->types().get_type_specialization(this->get_entity(), specialization, false, this->declTypeParams, errorMsg);
    }

    virtual void semantic_pass() override {
        if (! this->get_type())
            cerror("Unknown type or incorrect generic type arguments: %s (from %s)", this->identNode->ident.to_string().c_str(), this->context().scope()->to_string().c_str());
        for (TxTypeArgumentNode* ta : *this->typeArgs)
            ta->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxIdentifiedTypeNode : public TxPredefinedTypeNode {
protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
    }

public:
    const TxIdentifierNode* identNode;

    TxIdentifiedTypeNode(const yy::location& parseLocation, const TxIdentifierNode* identifier)
        : TxPredefinedTypeNode(parseLocation), identNode(identifier) { }

    virtual bool has_predefined_type() const override { return true; }

    virtual TxTypeEntity* get_entity() const override {
        if (auto declEnt = TxPredefinedTypeNode::get_entity())
            return declEnt;
        return const_cast<TxTypeEntity*>(this->context().scope()->resolve_type(this->identNode->ident));
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        if (auto identifiedEntity = this->context().scope()->resolve_type(this->identNode->ident)) {
            auto identifiedType = identifiedEntity->get_type();
            if (identifiedType->is_generic()) {
                cerror("Referenced generic type without type argument expression <>: %s", identifiedType->to_string().c_str());
            }
            else if (auto declEnt = TxPredefinedTypeNode::get_entity()) {
                ASSERT(!declTypeParams || declTypeParams->empty(), "declTypeParams can't be set for 'empty' specialization: " << *this);
                if (identifiedEntity->get_decl_flags() & TXD_GENPARAM) {
                    // let this entity be an alias for the generic type parameter (no unique type is created)
                    declEnt->set_alias();
                    LOGGER().debug("%s: Declared type '%s' as alias for GENPARAM %s", this->parse_loc_string().c_str(),
                                   declEnt->get_full_name().to_string().c_str(), identifiedEntity->to_string().c_str());
                    return identifiedType;
                }
                else {
                    // create empty specialization (uniquely named but identical type)
                    return this->types().get_type_specialization(declEnt, TxTypeSpecialization(identifiedType),
                                                                 false, this->declTypeParams, errorMsg);
                }
            }
            else {
                if (identifiedEntity->get_decl_flags() & TXD_GENPARAM) {
                    // Should not happen unless source refers directly to unbound type parameter
                    cwarning("'%s' references unbound generic type parameter %s", this->identNode->ident.to_string().c_str(), identifiedEntity->to_string().c_str());
                    // (But if legal use case exists, how let this be an alias entity for the generic type parameter?)
                    //LOGGER().error("%s: Can't declare type '%s' as alias for GENPARAM %s since no entity declared for this type node",
                    //               this->parse_loc_string().c_str(), this->identNode->ident.to_string().c_str(), identifiedEntity->to_string().c_str());
                }
                return identifiedType;
            }
        }
        if (errorMsg)
            errorMsg->append("Unknown type: " + this->identNode->ident.to_string() + " (from " + this->context().scope()->to_string() + ")");
        return nullptr;
    }

    virtual void semantic_pass() {
        if (! this->get_entity())
            cerror("Unknown type: %s (from %s)", this->identNode->ident.to_string().c_str(), this->context().scope()->to_string().c_str());
        this->get_type();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const { return nullptr; }
};


/** Common superclass for specializations of the built-in types Ref and Array. */
class TxBuiltinTypeSpecNode : public TxTypeExpressionNode {
public:
    TxBuiltinTypeSpecNode(const yy::location& parseLocation) : TxTypeExpressionNode(parseLocation)  { }

    virtual void symbol_table_pass(LexicalContext& lexContext, TxDeclarationFlags declFlags, TxTypeEntity* declaredEntity = nullptr,
                                   const std::vector<TxDeclarationNode*>* typeParamDecls = nullptr) override {
        if (! declaredEntity) {
            // ensure generic type specializations always have a declared type (handles e.g. Ref<Ref<Int>>)
            std::string typeName = "$type";
            //printf("%s: Declaring '%s' in '%s' as implicit specialized type\n", this->parse_loc_string().c_str(), typeName.c_str(), lexContext.scope()->get_full_name().to_string().c_str());
            auto declaredEntity = lexContext.scope()->declare_type(typeName, this, TXD_PUBLIC);
            if (!declaredEntity)
                cerror("Failed to declare implicit type %s", typeName.c_str());
            LexicalContext typeCtx(declaredEntity ? declaredEntity : lexContext.scope());  // (in case declare_type() yields NULL)
            TxTypeExpressionNode::symbol_table_pass(typeCtx, declFlags, declaredEntity, typeParamDecls);
        }
        else
            TxTypeExpressionNode::symbol_table_pass(lexContext, declFlags, declaredEntity, typeParamDecls);
    }
};

/**
 * Custom AST node needed to handle dataspaces. */
class TxReferenceTypeNode : public TxBuiltinTypeSpecNode {
protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        this->targetTypeNode->symbol_table_pass(lexContext);
    }

public:
    const TxIdentifierNode* dataspace;
    TxTypeArgumentNode* targetTypeNode;

    TxReferenceTypeNode(const yy::location& parseLocation, const TxIdentifierNode* dataspace,
                        TxTypeExpressionNode* targetType)
        : TxBuiltinTypeSpecNode(parseLocation), dataspace(dataspace),
          targetTypeNode(new TxTypeArgumentNode(targetType))  { }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        auto baseType = this->types().get_builtin_type(REFERENCE);
        auto baseTypeEntity = baseType->entity();
        TxTypeBinding binding = this->targetTypeNode->make_binding(baseTypeEntity, baseType->get_type_param("T"));
        return this->types().get_reference_type(this->get_entity(), binding);
    }

    virtual void semantic_pass() { targetTypeNode->semantic_pass(); }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};

/**
 * Custom AST node needed to provide syntactic sugar for modifiable declaration. */
class TxArrayTypeNode : public TxBuiltinTypeSpecNode {
protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        this->elementTypeNode->symbol_table_pass(lexContext);
        if (this->lengthNode)
            this->lengthNode->symbol_table_pass(lexContext);
    }

public:
    TxTypeArgumentNode* elementTypeNode;
    TxTypeArgumentNode* lengthNode;

    TxArrayTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* elementType, TxExpressionNode* lengthExpr=nullptr)
        : TxBuiltinTypeSpecNode(parseLocation),
          elementTypeNode(new TxTypeArgumentNode(elementType)),
          lengthNode(lengthExpr ? new TxTypeArgumentNode(lengthExpr) : nullptr)  { }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        auto baseType = this->types().get_builtin_type(ARRAY);
        auto baseTypeEntity = baseType->entity();
        TxTypeBinding elementBinding = this->elementTypeNode->make_binding(baseTypeEntity, baseType->get_type_param("E"));
        if (this->lengthNode) {
            TxTypeBinding lengthBinding = this->lengthNode->make_binding(baseTypeEntity, baseType->get_type_param("L"));
            return this->types().get_array_type(this->get_entity(), elementBinding, lengthBinding);
        }
        else
            return this->types().get_array_type(this->get_entity(), elementBinding);
    }

    virtual void semantic_pass() {
        this->elementTypeNode->semantic_pass();
        if (this->lengthNode) {
            this->lengthNode->semantic_pass();
            if (! this->lengthNode->valueExprNode->is_statically_constant())
                cerror("Non-constant array length specifier not yet supported.");
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};



class TxDerivedTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        //LexicalContext derivedTypeContext(declScope);
        {
            //LexicalContext parentContext(lexContext.scope()->get_parent());
            //std::string basename = "$base"; //this->get_entity()->get_name() + "$base";
            //int b = 0;
            for (auto baseType : *this->baseTypes)
                baseType->symbol_table_pass(lexContext, declFlags);
        }

        for (auto member : *this->instanceMembers) {
            member->symbol_table_pass(lexContext);
        }
        auto memIter = this->staticMembers->begin();
        while (memIter != this->staticMembers->end()) {
            auto member = *memIter;
            member->symbol_table_pass(lexContext);
            if (auto fieldDecl = dynamic_cast<TxFieldDeclNode*>(member))
                if (auto entity = fieldDecl->field->get_entity()) {
                    if (entity->get_storage() == TXS_INSTANCE) {
                        // move to instanceMembers list
                        this->instanceMembers->push_back(fieldDecl);
                        memIter = this->staticMembers->erase(memIter);
                        //std::cout << "instance member: " << fieldDecl->field->ident << std::endl;
                        continue;
                    }
                    //else
                    //    std::cout << "static member: " << fieldDecl->field->ident << std::endl;
                }
            memIter++;
        }
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        auto entity = this->get_entity();
        ASSERT(entity, "No entity declared for derived type " << *this);
        // FUTURE: support interfaces
        const TxType* baseObjType = this->baseTypes->empty() ? this->types().get_builtin_type(TUPLE)
                                                             : this->baseTypes->at(0)->get_type();
        if (! baseObjType)
            return nullptr;
        TxTypeSpecialization specialization(baseObjType, std::vector<TxTypeBinding>());
        // Note: does not specify explicit type parameter bindings; any unbound type parameters
        // of the base types are rebound automatically (and are expected to match the declared type params).
        auto type = this->types().get_type_specialization(entity, specialization, this->_mutable, this->declTypeParams, errorMsg);
        return type;
    }

public:
    const bool _mutable;
    std::vector<TxPredefinedTypeNode*>* baseTypes;
    std::vector<TxDeclarationNode*>* staticMembers;
    std::vector<TxFieldDeclNode*>* instanceMembers;

    TxDerivedTypeNode(const yy::location& parseLocation, const bool _mutable,
                      std::vector<TxPredefinedTypeNode*>* baseTypes,
                      std::vector<TxDeclarationNode*>* members)
            : TxTypeExpressionNode(parseLocation), _mutable(_mutable),
              baseTypes(baseTypes), staticMembers(members)  {
        instanceMembers = new std::vector<TxFieldDeclNode*>();
    }

    virtual void semantic_pass() {
        for (auto type : *this->baseTypes) {
            type->semantic_pass();
            // TODO: validity checks
        }
        for (auto member : *this->staticMembers) {
            member->semantic_pass();
            // TODO: validity checks
        }
        for (auto member : *this->instanceMembers) {
            member->semantic_pass();
            // validity checks:
            // TODO: can't put immutable member in non-immutable type (except via reference)
            //       (OR: disable whole-object-assignment)
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxFunctionTypeNode : public TxTypeExpressionNode {
    // Note: implicit reference type
    // Note: the field names aren't part of a function's formal type definition

    static TxFieldDefNode* make_return_field(TxTypeExpressionNode* returnType) {
        if (returnType)
            return new TxFieldDefNode(returnType->parseLocation, "$return", returnType, nullptr);
        else
            return nullptr;
    }

protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        // (processed as a function type and therefore doesn't declare (create entities for) the function args)
        for (auto argDef : *this->arguments)
            argDef->symbol_table_pass(lexContext);
        if (this->returnField)
            this->returnField->symbol_table_pass(lexContext);
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        std::vector<const TxType*> argumentTypes;
        for (auto argDefNode : *this->arguments)
            argumentTypes.push_back(argDefNode->get_type());
        if (this->returnField)
            return this->types().get_function_type(this->get_entity(), argumentTypes, this->returnField->get_type(), modifiable);
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

    void symbol_table_pass_func_header(LexicalContext& lexContext) {
        // (processed as the function header, so declare the function args, and the return type if any)
        this->set_context(lexContext);
        for (auto argField : *this->arguments)
            argField->symbol_table_pass_local_field(lexContext, false);
        if (this->returnField)
            this->returnField->symbol_table_pass_local_field(lexContext, false);
    }

    virtual void semantic_pass() {
        for (auto argDef : *this->arguments)
            argDef->semantic_pass();
        if (this->returnField)
            this->returnField->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxModifiableTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        this->baseType->symbol_table_pass(lexContext, declFlags);
    }

public:
    TxTypeExpressionNode* baseType;
    TxModifiableTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* baseType)
        : TxTypeExpressionNode(parseLocation), baseType(baseType) { }

    virtual void symbol_table_pass(LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                   TxTypeEntity* declaredEntity = nullptr,
                                   const std::vector<TxDeclarationNode*>* typeParamDecls = nullptr) override;

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        if (auto bType = this->baseType->get_type()) {
            if (bType->is_modifiable()) {
                if (errorMsg)
                    errorMsg->append("'modifiable' specified more than once for type: " + bType->to_string());
                else
                    cerror("'modifiable' specified more than once for type: %s", bType->to_string().c_str());
                return bType;
            }
            else if (! bType->is_immutable())
                return this->types().get_modifiable_type(this->get_entity(), bType, errorMsg);
            else if (errorMsg)
                errorMsg->append("Can't declare immutable type as modifiable: " + bType->to_string());
            else
                cerror("Can't declare immutable type as modifiable: %s", bType->to_string().c_str());
        }
        return nullptr;
    }

    virtual void semantic_pass() { baseType->semantic_pass(); }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};

/** A potentially modifiable type expression, depending on syntactic sugar rules.
 * This node should not have TxModifiableTypeNode as parent, and vice versa. */
class TxMaybeModTypeNode : public TxModifiableTypeNode {
public:
    bool isModifiable = false;

    TxMaybeModTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* baseType)
        : TxModifiableTypeNode(parseLocation, baseType) { }

    virtual bool has_predefined_type() const override;

    virtual void symbol_table_pass(LexicalContext& lexContext, TxDeclarationFlags declFlags,
                                   TxTypeEntity* declaredEntity = nullptr,
                                   const std::vector<TxDeclarationNode*>* typeParamDecls = nullptr) override;

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
        if (this->isModifiable)
            return TxModifiableTypeNode::define_type(errorMsg);
        else
            return this->baseType->get_type();
    }
};
