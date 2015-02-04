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
public:
    TxTypeExpressionNode* typeExprNode;
    TxExpressionNode* valueExprNode;

    TxTypeArgumentNode(const yy::location& parseLocation, TxTypeExpressionNode* typeExprNode)
        : TxNode(parseLocation), typeDeclNode(), fieldDeclNode(),
          typeExprNode(typeExprNode), valueExprNode() { }

    TxTypeArgumentNode(const yy::location& parseLocation, TxExpressionNode* valueExprNode)
        : TxNode(parseLocation), typeDeclNode(), fieldDeclNode(),
          typeExprNode(), valueExprNode(valueExprNode) { }

    virtual void symbol_table_pass(LexicalContext& lexContext, int argNo) {
        this->set_context(lexContext);
    }

    inline TxTypeBinding make_binding(const TxTypeEntity* baseTypeEntity, const TxTypeParam& param) {
        // FIXME: properly qualified name & lookup of base type parameter bindings
//        std::string pname = baseTypeEntity->get_full_name().to_string();
//        std::replace(pname.begin(), pname.end(), '.', '$');
//        pname += '$';
//        pname += param.param_name();
        std::string pname = param.param_name();
        if (this->typeExprNode) {
            if (param.meta_type() != param.TXB_TYPE)
                parser_error(this->parseLocation, "Provided a TYPE argument to VALUE parameter %s", pname.c_str());
            this->typeDeclNode = new TxTypeDeclNode(this->typeExprNode->parseLocation, TXD_PUBLIC, pname, nullptr, this->typeExprNode);
            this->typeDeclNode->symbol_table_pass(this->context());
            return TxTypeBinding(param.param_name(), this->typeExprNode);
        }
        else {
            if (param.meta_type() != param.TXB_VALUE)
                parser_error(this->parseLocation, "Provided a TYPE argument to VALUE parameter %s", pname.c_str());
            auto fieldDef = new TxFieldDefNode(this->valueExprNode->parseLocation, pname, this->valueExprNode);
            this->fieldDeclNode = new TxFieldDeclNode(this->valueExprNode->parseLocation, TXD_PUBLIC | TXD_STATIC, fieldDef);
            this->fieldDeclNode->symbol_table_pass(this->context());
            return TxTypeBinding(param.param_name(), static_cast<TxConstantProxy*>(this->valueExprNode));
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
        if (this->typeDeclNode)
            this->typeDeclNode->semantic_pass();
        else
            this->fieldDeclNode->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};

/** Represents a specialization of a generic type - binding one or more type parameters of a predefined, generic type. */
class TxSpecializedTypeNode : public TxPredefinedTypeNode {
protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        int argNo = 0;
        for (TxTypeArgumentNode* tp : *this->typeArgs) {
            tp->symbol_table_pass(lexContext, argNo++);
        }
    }

public:
    const TxIdentifierNode* identNode;
    const std::vector<TxTypeArgumentNode*>* const typeArgs;

    TxSpecializedTypeNode(const yy::location& parseLocation, const TxIdentifierNode* identifier,
                          const std::vector<TxTypeArgumentNode*>* typeParams)
            : TxPredefinedTypeNode(parseLocation), identNode(identifier), typeArgs(typeParams)  {
        ASSERT(! this->typeArgs->empty(), "No type parameters specified");
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        auto baseTypeEntity = this->context().scope()->lookup_type(this->identNode->ident);
        if (! baseTypeEntity) {
            if (errorMsg)
                errorMsg->append("Unknown type: " + this->identNode->ident.to_string() + " (from " + this->context().scope()->to_string() + ")");
            return nullptr;
        }
        auto baseType = baseTypeEntity->get_type();
        if (baseType->type_params().size() != this->typeArgs->size()) {
            parser_error(this->parseLocation, "Incorrect number of type parameters specified for type %s", identNode->ident.to_string().c_str());
            return nullptr;
        }
        std::vector<TxTypeBinding> bindings; // e.g. { TxTypeBinding("E", elemType), TxTypeBinding("L", length) }
        for (TxTypeParam tp : baseType->type_params()) {
            bindings.push_back(this->typeArgs->at(bindings.size())->make_binding(baseTypeEntity, tp));
        }
        TxTypeSpecialization specialization(baseType, bindings);
        return this->types().get_type_specialization(this->get_entity(), specialization, false, this->declTypeParams, errorMsg);
    }

    virtual void semantic_pass() override {
        for (TxTypeArgumentNode* ta : *this->typeArgs)
            ta->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxIdentifiedTypeNode : public TxPredefinedTypeNode {
protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override { ASSERT(false, "should not be invoked"); }

public:
    const TxIdentifierNode* identNode;

    TxIdentifiedTypeNode(const yy::location& parseLocation, const TxIdentifierNode* identifier)
        : TxPredefinedTypeNode(parseLocation), identNode(identifier) { }

    // identified type does not declare a new entity - override and do nothing
    virtual TxTypeEntity* symbol_table_pass(LexicalContext& lexContext, const std::string& typeName, TxDeclarationFlags declFlags,
                                            const std::vector<TxDeclarationNode*>* typeParamDecls = nullptr) override {
        this->set_context(lexContext);
        if (typeParamDecls && !typeParamDecls->empty())
            parser_error(this->parseLocation, "Unexpected type parameters declaration.");
        return nullptr;
    }

    virtual TxTypeEntity* get_entity() const override {
        return const_cast<TxTypeEntity*>(this->context().scope()->lookup_type(this->identNode->ident));
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        if (auto entity = this->context().scope()->lookup_type(this->identNode->ident))
            return entity->get_type();
        if (errorMsg)
            errorMsg->append("Unknown type: " + this->identNode->ident.to_string() + " (from " + this->context().scope()->to_string() + ")");
        return nullptr;
    }

    virtual void semantic_pass() {
        if (! this->get_entity())
            parser_error(this->parseLocation, "Unknown type: %s (from %s)", this->identNode->ident.to_string().c_str(), this->context().scope()->to_string().c_str());
    }
};


/** Produces a new, "empty" specialization of the underlying type for use by a type alias declaration. */
class TxAliasedTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        std::string basename = "$aliased"; //this->get_entity()->get_name() + "$aliased";
        this->baseType->symbol_table_pass(lexContext, basename, declFlags);
    }

public:
    TxTypeExpressionNode* baseType;
    TxAliasedTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* baseType)
        : TxTypeExpressionNode(parseLocation), baseType(baseType) { }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        ASSERT(!declTypeParams || declTypeParams->empty(), "declTypeParams can't be set for 'empty' specialization: " << *this);
        auto bType = this->baseType->get_type();
        return this->types().get_type_specialization(this->get_entity(), TxTypeSpecialization(bType),
                                                     false, this->declTypeParams, errorMsg);
    }

    virtual void semantic_pass() { baseType->semantic_pass(); }
};



class TxReferenceTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        std::string basename = "$T"; //this->get_entity()->get_name() + "$T";
        this->targetType->symbol_table_pass(lexContext, basename, declFlags);
    }

public:
    const TxIdentifierNode* dataspace;
    TxTypeExpressionNode* targetType;
    TxReferenceTypeNode(const yy::location& parseLocation, const TxIdentifierNode* dataspace,
                        TxTypeExpressionNode* targetType)
        : TxTypeExpressionNode(parseLocation), dataspace(dataspace), targetType(targetType)  { }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        return this->types().get_reference_type(this->get_entity(), this->targetType->get_type());
    }

    virtual void semantic_pass() { targetType->semantic_pass(); }
};

class TxArrayTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        std::string basename = "$E"; //this->get_entity()->get_name() + "$E";
        this->elementType->symbol_table_pass(lexContext, "$E", declFlags);
        if (this->lengthExpr)
            this->lengthExpr->symbol_table_pass(lexContext);
    }

public:
    TxTypeExpressionNode* elementType;
    TxExpressionNode* lengthExpr;
    TxArrayTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* elementType)
        : TxTypeExpressionNode(parseLocation), elementType(elementType), lengthExpr(nullptr)  { }
    TxArrayTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* elementType, TxExpressionNode* lengthExpr)
        : TxTypeExpressionNode(parseLocation), elementType(elementType), lengthExpr(lengthExpr)  { }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        if (this->lengthExpr)
            return this->types().get_array_type(this->get_entity(), this->elementType->get_type(), this->lengthExpr);
        else
            return this->types().get_array_type(this->get_entity(), this->elementType->get_type());
    }

    virtual void semantic_pass() {
        this->elementType->semantic_pass();
        if (this->lengthExpr) {
            this->lengthExpr->semantic_pass();
            if (! this->lengthExpr->is_statically_constant())
                parser_error(this->parseLocation, "Non-constant array length specifier not yet supported.");
        }
    }
};

class TxDerivedTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override {
        {
            //LexicalContext parentContext(lexContext.scope()->get_parent());
            std::string basename = "$base"; //this->get_entity()->get_name() + "$base";
            int b = 0;
            for (auto baseType : *this->baseTypes)
                baseType->symbol_table_pass(lexContext, basename + std::to_string(b++), declFlags);
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
};


class TxModifiableTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_table_pass_descendants(LexicalContext& lexContext, TxDeclarationFlags declFlags) override { ASSERT(false, "should not be invoked"); }

public:
    TxTypeExpressionNode* baseType;
    TxModifiableTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* baseType)
        : TxTypeExpressionNode(parseLocation), baseType(baseType) { }

    virtual TxTypeEntity* symbol_table_pass(LexicalContext& lexContext, const std::string& typeName, TxDeclarationFlags declFlags,
                                            const std::vector<TxDeclarationNode*>* typeParamDecls = nullptr) override;

    virtual TxTypeEntity* get_entity() const override {
        return this->baseType->get_entity();
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        if (auto bType = this->baseType->get_type()) {
            if (bType->is_modifiable())
                return bType;
            else if (! bType->is_immutable())
                return this->types().get_modifiable_type(bType, errorMsg);
            else if (errorMsg)
                errorMsg->append("Can't declare immutable type as modifiable: " + bType->to_string());
            else
                parser_error(this->parseLocation, "Can't declare immutable type as modifiable: %s", bType->to_string().c_str());
        }
        return nullptr;
    }

    virtual void semantic_pass() { baseType->semantic_pass(); }
};

/** A potentially modifiable type expression, depending on syntactic sugar rules.
 * This node should not have TxModifiableTypeNode as parent, and vice versa. */
class TxMaybeModTypeNode : public TxModifiableTypeNode {
public:
    bool isModifiable = false;

    TxMaybeModTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* baseType)
        : TxModifiableTypeNode(parseLocation, baseType) { }

    virtual TxTypeEntity* symbol_table_pass(LexicalContext& lexContext, const std::string& typeName, TxDeclarationFlags declFlags,
                                            const std::vector<TxDeclarationNode*>* typeParamDecls = nullptr) override;

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
        if (this->isModifiable)
            return TxModifiableTypeNode::define_type(errorMsg);
        else
            return this->baseType->get_type();
    }
};
