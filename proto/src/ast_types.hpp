#pragma once

#include "ast_declbase.hpp"



/** Represents a binding for a type parameter. Can be either a Type or a Value parameter binding. */
class TxTypeArgumentNode : public TxNode {
protected:
    TxTypeArgumentNode( const TxLocation& parseLocation ) : TxNode(parseLocation) { }

public:
    virtual TxTypeArgumentNode* make_ast_copy() const override = 0;

    virtual std::string get_auto_type_name() const = 0;

    virtual void symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext ) = 0;

    virtual void symbol_resolution_pass() = 0;

    /** Creates, registers and returns a newly created TxTypeBinding.
     * To be called after symbol_declaration_pass() and before symbol_resolution_pass(). */
    virtual TxGenericBinding make_binding( const std::string& paramName ) = 0;
};


class TxTypeTypeArgumentNode : public TxTypeArgumentNode {
public:
    TxTypeExpressionNode* typeExprNode;

    TxTypeTypeArgumentNode( TxTypeExpressionNode* typeExprNode )
        : TxTypeArgumentNode(typeExprNode->parseLocation), typeExprNode(typeExprNode) { }

    virtual TxTypeTypeArgumentNode* make_ast_copy() const override {
        return new TxTypeTypeArgumentNode( this->typeExprNode->make_ast_copy() );
    }

    virtual std::string get_auto_type_name() const override {
        return this->typeExprNode->get_auto_type_name();
    }

    /** Creates, registers and returns a newly created TxTypeBinding.
     * To be called after symbol_declaration_pass() and before symbol_resolution_pass(). */
    virtual TxGenericBinding make_binding( const std::string& paramName ) override;

    virtual void symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext ) override {
        this->set_context( lexContext );
        this->typeExprNode->symbol_declaration_pass( defContext, lexContext, nullptr );
    }

    virtual void symbol_resolution_pass() override {
        this->typeExprNode->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxValueTypeArgumentNode : public TxTypeArgumentNode {
public:
    TxExpressionNode* valueExprNode;

    TxValueTypeArgumentNode( TxExpressionNode* valueExprNode )
        : TxTypeArgumentNode(valueExprNode->parseLocation), valueExprNode(valueExprNode) { }

    virtual TxValueTypeArgumentNode* make_ast_copy() const override {
        return new TxValueTypeArgumentNode( this->valueExprNode->make_ast_copy() );
    }

    virtual std::string get_auto_type_name() const override {
        if (auto bindingValueProxy = this->valueExprNode->get_static_constant_proxy()) {
            uint32_t bindingValue = bindingValueProxy->get_value_UInt();
            return std::to_string( bindingValue );  // statically known value
        }
        else {
            return "$VALUE";
            // implementation note: a distinct compile time type is registered which holds this specific dynamic value expression
        }
    }

    /** Creates, registers and returns a newly created TxTypeBinding.
     * To be called after symbol_declaration_pass() and before symbol_resolution_pass(). */
    virtual TxGenericBinding make_binding( const std::string& paramName ) override;

    virtual void symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext ) override {
        this->set_context( lexContext );
        this->valueExprNode->symbol_declaration_pass( lexContext );
    }

    virtual void symbol_resolution_pass() override {
        this->valueExprNode->symbol_resolution_pass();
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
class TxGenSpecializationTypeNode : public TxTypeExpressionNode {
    const TxType* define_generic_specialization_type();

protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override {
        for (TxTypeArgumentNode* tp : *this->typeArgs) {
            tp->symbol_declaration_pass( defContext, lexContext);
        }
    }

    virtual const TxType* define_type() override {
        const TxType* type = this->define_generic_specialization_type();
        if (! type)
            CERROR(this, "Unknown type: " << this->identNode->ident << " (from " << this->context().scope() << ")");
        return type;
    }

public:
    const TxIdentifierNode* identNode;
    const std::vector<TxTypeArgumentNode*>* const typeArgs;

    TxGenSpecializationTypeNode( const TxLocation& parseLocation, const TxIdentifierNode* identifier,
                                 const std::vector<TxTypeArgumentNode*>* typeArgs )
            : TxTypeExpressionNode(parseLocation), identNode(identifier), typeArgs(typeArgs)  {
        ASSERT(typeArgs && !typeArgs->empty(), "NULL or empty typeargs");
    }

    virtual TxGenSpecializationTypeNode* make_ast_copy() const override {
        return new TxGenSpecializationTypeNode( this->parseLocation, this->identNode->make_ast_copy(), make_node_vec_copy( this->typeArgs ) );
    }

    virtual std::string get_auto_type_name() const override {
        auto identifiedTypeDecl = lookup_type( this->context().scope(), this->identNode->ident );
        std::string name = ( identifiedTypeDecl ? hashify( identifiedTypeDecl->get_unique_full_name() ) : "$UNKNOWN" );
        int ix = 0;
        for (TxTypeArgumentNode* ta : *this->typeArgs) {
            name += ( (ix++ == 0) ? "<" : "," );
            name += ta->get_auto_type_name();
        }
        name += ">";
        return name;
    }

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        for (TxTypeArgumentNode* ta : *this->typeArgs)
            ta->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxIdentifiedTypeNode : public TxTypeExpressionNode {
    const TxType* define_identified_type();

protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override {
    }

    virtual const TxType* define_type() override {
        const TxType* type = this->define_identified_type();
        if (! type)
            CERROR(this, "Unknown type: " << this->identNode->ident << " (from " << this->context().scope() << ")");
        return type;
    }

public:
    const TxIdentifierNode* identNode;

    TxIdentifiedTypeNode(const TxLocation& parseLocation, const TxIdentifierNode* identifier)
        : TxTypeExpressionNode(parseLocation), identNode(identifier)  { }

    TxIdentifiedTypeNode(const TxLocation& parseLocation, const std::string& identifier)
        : TxTypeExpressionNode(parseLocation), identNode(new TxIdentifierNode(parseLocation, identifier))  { }

    virtual TxIdentifiedTypeNode* make_ast_copy() const override {
        return new TxIdentifiedTypeNode( this->parseLocation, this->identNode->make_ast_copy() );
    }

    virtual std::string get_auto_type_name() const override {
        auto identifiedTypeDecl = lookup_type( this->context().scope(), this->identNode->ident );
        return ( identifiedTypeDecl ? hashify( identifiedTypeDecl->get_unique_full_name() ) : "$UNKNOWN" );
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


/** Common superclass for specializations of the built-in types Ref and Array. */
class TxBuiltinTypeSpecNode : public TxTypeExpressionNode {
public:
    TxBuiltinTypeSpecNode(const TxLocation& parseLocation) : TxTypeExpressionNode(parseLocation)  { }
};

/**
 * Custom AST node needed to handle dataspaces. */
class TxReferenceTypeNode : public TxBuiltinTypeSpecNode {
    TxReferenceTypeNode( const TxLocation& parseLocation, const TxIdentifierNode* dataspace, TxTypeTypeArgumentNode* targetTypeArg )
        : TxBuiltinTypeSpecNode(parseLocation), dataspace(dataspace), targetTypeNode(targetTypeArg)  { }

protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override {
        this->targetTypeNode->symbol_declaration_pass( defContext, lexContext);
    }

    virtual const TxType* define_type() override {
        auto baseType = this->types().get_builtin_type(REFERENCE);
        auto baseTypeName = baseType->get_declaration()->get_symbol()->get_full_name();
        TxGenericBinding binding = this->targetTypeNode->make_binding( "T" );
        const TxIdentifier* dataspace = (this->dataspace ? &this->dataspace->ident : nullptr);
        //cwarning("Dataspace: %s", (this->dataspace ? this->dataspace->ident.to_string().c_str() : "NULL"));
        return this->types().get_reference_type( this, binding, dataspace );
    }

public:
    const TxIdentifierNode* dataspace;
    TxTypeTypeArgumentNode* targetTypeNode;

    TxReferenceTypeNode(const TxLocation& parseLocation, const TxIdentifierNode* dataspace, TxTypeExpressionNode* targetType)
        : TxReferenceTypeNode(parseLocation, dataspace, new TxTypeTypeArgumentNode(targetType) )  { }

    virtual TxReferenceTypeNode* make_ast_copy() const override {
        const TxIdentifierNode* ds = ( this->dataspace ? this->dataspace->make_ast_copy() : nullptr );
        return new TxReferenceTypeNode( this->parseLocation, ds, this->targetTypeNode->make_ast_copy() );
    }

    virtual std::string get_auto_type_name() const override {
        return "Ref<" + this->targetTypeNode->get_auto_type_name() + ">";
    }

    virtual void symbol_resolution_pass() override {
        TxBuiltinTypeSpecNode::symbol_resolution_pass();
        this->targetTypeNode->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/**
 * Custom AST node needed to provide syntactic sugar for modifiable declaration. */
class TxArrayTypeNode : public TxBuiltinTypeSpecNode {
    TxArrayTypeNode(const TxLocation& parseLocation, TxTypeTypeArgumentNode* elementTypeArg, TxValueTypeArgumentNode* lengthExprArg)
        : TxBuiltinTypeSpecNode(parseLocation), elementTypeNode(elementTypeArg), lengthNode(lengthExprArg) { }

protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override;

    virtual const TxType* define_type() override {
        auto baseType = this->types().get_builtin_type(ARRAY);
        auto baseTypeName = baseType->get_declaration()->get_symbol()->get_full_name();
        TxGenericBinding elementBinding = this->elementTypeNode->make_binding( "E" );
        if (this->lengthNode) {
            TxGenericBinding lengthBinding = this->lengthNode->make_binding( "L" );
            return this->types().get_array_type( this, elementBinding, lengthBinding );
        }
        else
            return this->types().get_array_type( this, elementBinding );
    }

public:
    TxTypeTypeArgumentNode* elementTypeNode;
    TxValueTypeArgumentNode* lengthNode;

    TxArrayTypeNode(const TxLocation& parseLocation, TxTypeExpressionNode* elementType, TxExpressionNode* lengthExpr=nullptr)
        : TxArrayTypeNode( parseLocation, new TxTypeTypeArgumentNode(elementType), (lengthExpr ? new TxValueTypeArgumentNode(lengthExpr) : nullptr) ) { }

    virtual TxArrayTypeNode* make_ast_copy() const override {
        return new TxArrayTypeNode( this->parseLocation, this->elementTypeNode->make_ast_copy(),
                                    ( this->lengthNode ? this->lengthNode->make_ast_copy() : nullptr ) );
    }

    virtual std::string get_auto_type_name() const override {
        if (this->lengthNode)
            return "Array<" + this->elementTypeNode->get_auto_type_name() + "," + this->lengthNode->get_auto_type_name() + ">";
        else
            return "Array<" + this->elementTypeNode->get_auto_type_name() + ">";
    }

    virtual void symbol_resolution_pass() override {
        TxBuiltinTypeSpecNode::symbol_resolution_pass();
        this->elementTypeNode->symbol_resolution_pass();
        if (this->lengthNode) {
            this->lengthNode->symbol_resolution_pass();
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
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override {
        for (auto baseType : *this->baseTypes)
            baseType->symbol_declaration_pass( defContext, lexContext, nullptr );

        bool isExpError = (this->get_declaration()->get_decl_flags() & TXD_EXPERRBLOCK);
        this->selfRefTypeNode->symbol_declaration_pass( lexContext, isExpError );
        this->superRefTypeNode->symbol_declaration_pass( lexContext, isExpError );

        for (auto member : *this->members)
            member->symbol_declaration_pass( lexContext);

//        bool explicitConstructor = false;
//        for (auto member : *this->members) {
//            member->symbol_declaration_pass( lexContext);
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

    virtual const TxType* define_type() override {
        auto declaration = this->get_declaration();
        ASSERT(declaration, "No declaration for derived type " << *this);

        const TxType* baseObjType = nullptr;
        std::vector<TxTypeSpecialization> interfaces;
        if (this->baseTypes->empty())
            baseObjType = this->types().get_builtin_type(TUPLE);
        else {
            interfaces.reserve(this->baseTypes->size()-1);
            for (size_t i = 0; i < this->baseTypes->size(); i++) {
                if (auto baseType = this->baseTypes->at(i)->resolve_type()) {
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

        auto type = this->types().get_type_derivation( this, baseObjType, interfaces, this->_mutable );
        return type;
    }

public:
    const bool _mutable;
    std::vector<TxTypeExpressionNode*>* baseTypes;
    std::vector<TxDeclarationNode*>* members;
    TxTypeDeclNode* selfRefTypeNode = nullptr;
    TxTypeDeclNode* superRefTypeNode = nullptr;

    TxDerivedTypeNode(const TxLocation& parseLocation, const bool _mutable,
                      std::vector<TxTypeExpressionNode*>* baseTypes,
                      std::vector<TxDeclarationNode*>* members)
            : TxTypeExpressionNode(parseLocation), _mutable(_mutable),
              baseTypes(baseTypes), members(members)  {
        this->init_implicit_types();
    }

    virtual TxDerivedTypeNode* make_ast_copy() const override {
        return new TxDerivedTypeNode( this->parseLocation, this->_mutable,
                                      make_node_vec_copy( this->baseTypes ), make_node_vec_copy( this->members ) );
    }

    virtual std::string get_auto_type_name() const override {
        return this->get_declaration()->get_unique_name();
    }

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        for (auto type : *this->baseTypes) {
            type->symbol_resolution_pass();
        }

        this->selfRefTypeNode->symbol_resolution_pass();
        this->superRefTypeNode->symbol_resolution_pass();

        for (auto member : *this->members) {
            member->symbol_resolution_pass();
            // TODO: can't put immutable instance member in non-immutable type (except via reference)
            //       (OR: disable whole-object-assignment)
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


/**
 * Custom AST node needed to resolve to a type's super type. */
class TxSuperTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override {
        this->derivedTypeNode->symbol_declaration_pass( defContext, lexContext, nullptr );
    }

    virtual const TxType* define_type() override {
        if (auto dType = this->derivedTypeNode->resolve_type()) {
            if (dType->has_base_type())
                return dType->get_semantic_base_type();
            CERROR(this, "Can't refer to 'super type' of a type that has no base type: " << dType);
        }
        return nullptr;
    }

public:
    TxTypeExpressionNode* derivedTypeNode;

    TxSuperTypeNode(const TxLocation& parseLocation, TxTypeExpressionNode* derivedTypeNode)
        : TxTypeExpressionNode(parseLocation), derivedTypeNode(derivedTypeNode)  { }

    virtual TxSuperTypeNode* make_ast_copy() const override {
        return new TxSuperTypeNode( this->parseLocation, this->derivedTypeNode->make_ast_copy() );
    }

    virtual std::string get_auto_type_name() const override {
        return "$Base";  // TODO: review
    }

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        this->derivedTypeNode->symbol_resolution_pass();
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
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override {
        // (processed as a function type and therefore doesn't declare (create entities for) the function args)
        for (auto argDef : *this->arguments)
            argDef->symbol_declaration_pass_functype_arg( lexContext );
        if (this->returnField)
            this->returnField->symbol_declaration_pass_functype_arg( lexContext );
    }

    virtual const TxType* define_type() override {
        std::vector<const TxType*> argumentTypes;
        for (auto argDefNode : *this->arguments)
            argumentTypes.push_back(argDefNode->resolve_type());
        if (this->context().get_constructed())
            return this->types().get_constructor_type(this->get_declaration(), argumentTypes, this->context().get_constructed());
        else if (this->returnField)
            return this->types().get_function_type(this->get_declaration(), argumentTypes, this->returnField->resolve_type(), modifiable);
        else
            return this->types().get_function_type(this->get_declaration(), argumentTypes, modifiable);
    }

public:
    //const TxNamespaceIdentNode* dataspace;
    const bool modifiable;
    std::vector<TxFieldDefNode*>* arguments;
    TxFieldDefNode* returnField;
    // FUTURE: raised exceptions

    TxFunctionTypeNode(const TxLocation& parseLocation, const bool modifiable,
                       std::vector<TxFieldDefNode*>* arguments,
                       TxTypeExpressionNode* returnType)
        : TxTypeExpressionNode(parseLocation), modifiable(modifiable),
          arguments(arguments), returnField(make_return_field(returnType)) { }

    virtual TxFunctionTypeNode* make_ast_copy() const override {
        return new TxFunctionTypeNode( this->parseLocation, this->modifiable, make_node_vec_copy( this->arguments ),
                                       ( this->returnField ? this->returnField->typeExpression->make_ast_copy() : nullptr ) );
    }

    virtual std::string get_auto_type_name() const override {
        ASSERT(this->get_declaration(), "NULL declaration in TxFunctionTypeNode: " << this);
        return this->get_declared_name();
    }

    void symbol_declaration_pass_func_header( LexicalContext& lexContext ) {
        // (processed as the function instance header, so declare the function args, and the return type if any)
        this->set_context( lexContext);
        for (auto argField : *this->arguments)
            argField->symbol_declaration_pass_local_field( lexContext, false );
        if (this->returnField)
            this->returnField->symbol_declaration_pass_local_field( lexContext, false, TXD_IMPLICIT );

// experiment with implicit func type declarations
//        ASSERT(!this->get_declaration(), "Unexpected declaration in " << this);
//        //TxDeclarationFlags flags = (isExpErrorDecl ? TXD_IMPLICIT | TXD_EXPERRBLOCK : TXD_IMPLICIT);
//        std::string typeName = "$ftype";
//        auto declaration = this->context().scope()->declare_type( typeName, this, TXD_IMPLICIT );
//        if (! declaration) {
//            CERROR(this, "Failed to declare type " << typeName);
//            return;
//        }
//        this->set_declaration( declaration );
//        this->LOGGER().note("%s: Declared type %-16s: %s", this->parse_loc_string().c_str(), typeName.c_str(),
//                             declaration->to_string().c_str());
    }

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        for (auto argDef : *this->arguments)
            argDef->symbol_resolution_pass();
        if (this->returnField)
            this->returnField->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxModifiableTypeNode : public TxTypeExpressionNode {
protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override {
        this->baseType->symbol_declaration_pass( defContext, lexContext, nullptr );
    }

    virtual const TxType* define_type() override {
        if (auto bType = this->baseType->resolve_type()) {
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
    TxModifiableTypeNode(const TxLocation& parseLocation, TxTypeExpressionNode* baseType)
        : TxTypeExpressionNode(parseLocation), baseType(baseType) { }

    virtual TxModifiableTypeNode* make_ast_copy() const override {
        return new TxModifiableTypeNode( this->parseLocation, this->baseType->make_ast_copy() );
    }

    virtual std::string get_auto_type_name() const override {
        return "~" + this->baseType->get_auto_type_name();
    }

    virtual void symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext,
                                          const TxTypeDeclaration* owningDeclaration ) override;

    virtual void symbol_resolution_pass() override {
        TxTypeExpressionNode::symbol_resolution_pass();
        this->baseType->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** A potentially modifiable type expression, depending on syntactic sugar rules.
 * This node should not have TxModifiableTypeNode as parent, and vice versa. */
class TxMaybeModTypeNode : public TxModifiableTypeNode {
    bool isModifiable = false;

protected:
    virtual const TxType* define_type() override {
        // syntactic sugar to make these equivalent: ~[]~ElemT  ~[]ElemT  []~ElemT
        if (this->is_modifiable())
            return TxModifiableTypeNode::define_type();
        else
            return this->baseType->resolve_type();
    }

public:
    TxMaybeModTypeNode(const TxLocation& parseLocation, TxTypeExpressionNode* baseType)
        : TxModifiableTypeNode(parseLocation, baseType) { }

    virtual TxMaybeModTypeNode* make_ast_copy() const override {
        return new TxMaybeModTypeNode( this->parseLocation, this->baseType->make_ast_copy() );
    }

    virtual std::string get_auto_type_name() const override {
        return ( this->is_modifiable() ? "~" + this->baseType->get_auto_type_name() : this->baseType->get_auto_type_name() );
    }

    virtual void symbol_declaration_pass( LexicalContext& defContext, LexicalContext& lexContext,
                                          const TxTypeDeclaration* owningDeclaration ) override;

    inline bool is_modifiable() const { return this->isModifiable; }

    void set_modifiable(bool mod) {
        this->isModifiable = mod;
    }
};
