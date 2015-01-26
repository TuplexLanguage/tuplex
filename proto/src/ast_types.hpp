#pragma once

#include "ast_base.hpp"


class TxTypeArgumentNode : public TxNode {
public:
    TxTypeExpressionNode* typeNode;
    TxExpressionNode* valueNode;

    TxTypeArgumentNode(const yy::location& parseLocation, TxTypeExpressionNode* typeNode)
        : TxNode(parseLocation), typeNode(typeNode), valueNode() { }

    TxTypeArgumentNode(const yy::location& parseLocation, TxExpressionNode* valueNode)
        : TxNode(parseLocation), typeNode(), valueNode(valueNode) { }

    virtual void symbol_table_pass(LexicalContext& lexContext, const std::string& implicitTypeName, TxDeclarationFlags declFlags) {
        // "pass through" to the underlying type
        this->set_context(lexContext);
        if (this->typeNode)
            this->typeNode->symbol_table_pass(lexContext, implicitTypeName, declFlags);
        else
            this->valueNode->symbol_table_pass(lexContext);
    }

    virtual TxTypeEntity* get_entity() const {
        return (this->typeNode ? this->typeNode->get_entity() : nullptr);
    }

    virtual const TxType* get_type() const {
        return (this->typeNode ? this->typeNode->get_type() : this->valueNode->get_type());
    }

    virtual void semantic_pass() {
        if (this->typeNode)
            this->typeNode->semantic_pass();
        else
            this->valueNode->semantic_pass();
    }
};

class TxIdentifiedTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_table_pass(LexicalContext& lexContext) override {
        int pno = 0;
        for (TxTypeArgumentNode* tp : *this->typeParams) {
            tp->symbol_table_pass(lexContext, this->get_entity()->get_name() + "$arg" + std::to_string(pno++),
                                  this->get_entity()->get_decl_flags() & (TXD_PUBLIC | TXD_PROTECTED));
        }
    }

public:
    const TxIdentifierNode* identNode;
    const std::vector<TxTypeArgumentNode*>* const typeParams;

    TxIdentifiedTypeNode(const yy::location& parseLocation, const TxIdentifierNode* identifier)
        : TxTypeExpressionNode(parseLocation), identNode(identifier), typeParams(new std::vector<TxTypeArgumentNode*>)  { }

    TxIdentifiedTypeNode(const yy::location& parseLocation, const TxIdentifierNode* identifier,
                         const std::vector<TxTypeArgumentNode*>* typeParams)
        : TxTypeExpressionNode(parseLocation), identNode(identifier), typeParams(typeParams)  { }

    virtual void symbol_table_pass(LexicalContext& lexContext, const std::string& implicitTypeName, TxDeclarationFlags declFlags) override {
        // unless type parameters are bound in this type expression, no new type entity is declared
        if (! this->typeParams->empty()) {
            TxTypeExpressionNode::symbol_table_pass(lexContext, implicitTypeName, declFlags);
            //TxTypeEntity* entity = lexContext.scope()->declare_type(implicitTypeName, declFlags));
        }
        else {
            this->set_context(lexContext);
            this->symbolTablePassed = true;
        }
    }

    virtual TxTypeEntity* get_entity() const override {
        if (! this->typeParams->empty())
            return TxTypeExpressionNode::get_entity();
        return const_cast<TxTypeEntity*>(this->context().scope()->lookup_type(this->identNode->ident));
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        auto baseTypeEntity = this->context().scope()->lookup_type(this->identNode->ident);
        if (! baseTypeEntity)
            return nullptr;
        auto baseType = baseTypeEntity->get_type();
        if (this->typeParams->empty())
            return baseType;
        if (baseType->typeParams.size() != this->typeParams->size()) {
            parser_error(this->parseLocation, "Incorrect number of type parameters specified for type %s", identNode->ident.to_string().c_str());
            return nullptr;
        }
        std::vector<TxTypeBinding> bindings; // e.g. { TxTypeBinding("E", elemType), TxTypeBinding("L", length) }
        for (TxTypeParam tp : baseType->typeParams) {
            bindings.push_back(TxTypeBinding(tp.param_name(), this->typeParams->at(bindings.size())->get_type()));
        }
        TxTypeSpecialization specialization(baseType, bindings);
        return this->types().get_type_specialization(specialization, errorMsg);
    }

    virtual void semantic_pass() {
        if (this->is_context_set()) {  // (if declaration error happened in previous pass)
            if (! this->get_entity())
                parser_error(this->parseLocation, "Unknown type: %s", this->identNode->ident.to_string().c_str());
            for (TxTypeArgumentNode* tp : *this->typeParams) {
                tp->semantic_pass();
            }
        }
    }
};


class TxModifiableTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_table_pass(LexicalContext& lexContext) override { }

public:
    TxTypeExpressionNode* baseType;
    TxModifiableTypeNode(const yy::location& parseLocation, TxTypeExpressionNode* baseType)
        : TxTypeExpressionNode(parseLocation), baseType(baseType) { }

    // modifiable type specialization is not an actual data type - "pass through" entity declaration to the underlying type
    virtual void symbol_table_pass(LexicalContext& lexContext, const std::string& implicitTypeName, TxDeclarationFlags declFlags) override {
        this->set_context(lexContext);
        this->baseType->symbol_table_pass(lexContext, implicitTypeName, declFlags);
        this->symbolTablePassed = true;
    }
    virtual void symbol_table_pass(LexicalContext& lexContext, TxTypeEntity* entity) override {
        this->set_context(lexContext);
        this->baseType->symbol_table_pass(lexContext, entity);
        this->symbolTablePassed = true;
    }

    virtual TxTypeEntity* get_entity() const override {
        return this->baseType->get_entity();
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        return this->types().get_type_specialization(TxTypeSpecialization(this->baseType->get_type(), true), errorMsg);
    }

    virtual void semantic_pass() { baseType->semantic_pass(); }
};



class TxReferenceTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_table_pass(LexicalContext& lexContext) override {
        this->targetType->symbol_table_pass(lexContext, this->get_entity()->get_name() + "$T",
                                           this->get_entity()->get_decl_flags() & (TXD_PUBLIC | TXD_PROTECTED));
    }

public:
    const TxIdentifierNode* dataspace;
    TxTypeExpressionNode* targetType;
    TxReferenceTypeNode(const yy::location& parseLocation, const TxIdentifierNode* dataspace,
                        TxTypeExpressionNode* targetType)
        : TxTypeExpressionNode(parseLocation), dataspace(dataspace), targetType(targetType)  { }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        return this->types().get_reference_type(this->targetType->get_type());
    }

    virtual void semantic_pass() { targetType->semantic_pass(); }
};

class TxArrayTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_table_pass(LexicalContext& lexContext) override {
        this->elementType->symbol_table_pass(lexContext, this->get_entity()->get_name() + "$E",
                                             this->get_entity()->get_decl_flags() & (TXD_PUBLIC | TXD_PROTECTED));
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
            return this->types().get_array_type(this->elementType->get_type(), this->lengthExpr);
        else
            return this->types().get_array_type(this->elementType->get_type());
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

class TxTupleTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_table_pass(LexicalContext& lexContext) override {
        {
            std::string basename = this->get_entity()->get_name() + "$base";
            TxDeclarationFlags declFlags = this->get_entity()->get_decl_flags() & (TXD_PUBLIC | TXD_PROTECTED);
            int b = 0;
            for (auto baseType : *this->baseTypes)
                baseType->symbol_table_pass(lexContext, basename + std::to_string(b++), declFlags);
        }

        LexicalContext typeExtensionCtx(this->get_entity());
        for (auto member : *this->instanceMembers) {
            member->symbol_table_pass(typeExtensionCtx);
        }
        auto memIter = this->staticMembers->begin();
        while (memIter != this->staticMembers->end()) {
            auto member = *memIter;
            member->symbol_table_pass(typeExtensionCtx);
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
        ASSERT(entity, "No entity declared for tuple type " << *this);
        return this->types().get_tuple_type(entity, this->modifiable);
    }

public:
    const bool modifiable;
    std::vector<TxIdentifiedTypeNode*>* baseTypes;
    std::vector<TxDeclarationNode*>* staticMembers;
    std::vector<TxFieldDeclNode*>* instanceMembers;

    /** Full Tuple type creation. */
    TxTupleTypeNode(const yy::location& parseLocation, const bool modifiable,
                    std::vector<TxIdentifiedTypeNode*>* baseTypes,
                    std::vector<TxDeclarationNode*>* members)
            : TxTypeExpressionNode(parseLocation), modifiable(modifiable),
              baseTypes(baseTypes), staticMembers(members)  {
        instanceMembers = new std::vector<TxFieldDeclNode*>();
    }

    /** Simple Tuple type creation. */
    TxTupleTypeNode(const yy::location& parseLocation, const bool modifiable,
                    std::vector<TxFieldDefNode*>* fieldDefs)
            : TxTypeExpressionNode(parseLocation), modifiable(modifiable),
              baseTypes(new std::vector<TxIdentifiedTypeNode*>()),
              staticMembers(new std::vector<TxDeclarationNode*>()),
              instanceMembers(new std::vector<TxFieldDeclNode*>()) {
        for (auto fieldDef : *fieldDefs)
            instanceMembers->push_back(new TxFieldDeclNode(fieldDef->parseLocation, TXD_PUBLIC, fieldDef));
    }

    virtual void semantic_pass() {
        for (auto type : *this->baseTypes)
            type->semantic_pass();
        for (auto member : *this->staticMembers)
            member->semantic_pass();
        for (auto member : *this->instanceMembers)
            member->semantic_pass();
    }

    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
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
    virtual void symbol_table_pass(LexicalContext& lexContext) override {
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
            return this->types().get_function_type(argumentTypes, this->returnField->get_type(), modifiable);
        else
            return this->types().get_function_type(argumentTypes, modifiable);
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
        this->symbolTablePassed = true;
    }

    virtual void semantic_pass() {
        for (auto argDef : *this->arguments)
            argDef->semantic_pass();
        if (this->returnField)
            this->returnField->semantic_pass();
    }
};
