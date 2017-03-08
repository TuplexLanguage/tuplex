#include "util/assert.hpp"

#include "type_registry.hpp"

#include "package.hpp"
#include "entity.hpp"
#include "ast.hpp"
#include "builtin/builtin_types.hpp"

#include "llvm_generator.hpp"



/** the flags that may be inherited when specializing a type */
static const TxDeclarationFlags DECL_FLAG_FILTER = TXD_STATIC | TXD_PUBLIC | TXD_PROTECTED | TXD_ABSTRACT | TXD_FINAL | TXD_IMPLICIT | TXD_EXPERRBLOCK;




TypeRegistry::TypeRegistry(TxPackage& package)
        : package(package) {
    this->createdTypes = new std::vector<TxType*>();
}


const TxLocation& TypeRegistry::get_builtin_location() const {
    return this->package.builtin_types()->get_builtin_location();
}


void TypeRegistry::enqueued_resolution_pass() {
    // Note: Queue can be appended to during processing.
    for (unsigned i = 0; i != this->enqueuedSpecializations.size(); i++) {
        //std::cerr << "Nof enqueued specializations: " << this->enqueuedSpecializations.size() << std::endl;
        auto specDecl = this->enqueuedSpecializations.at(i);
        this->package.LOGGER().debug("Resolving enqueued specialization: %s", specDecl->str().c_str());
        specDecl->symbol_resolution_pass();
    }
}


void TypeRegistry::add_type(TxType* type) {
    this->createdTypes->push_back(type);
}

void TypeRegistry::prepare_types() {
    auto createdTypes = this->createdTypes;
    this->createdTypes = new std::vector<TxType*>();
    for (auto type : *createdTypes) {
        //std::cerr << "Preparing type: " << type << std::endl;
        type->prepare_type_members();
        if (type->is_builtin()) {
            ASSERT(type->runtimeTypeId == this->staticTypes.size(), "preparing built-in type in wrong order / id: " << type->runtimeTypeId << ": " << type);
        }
        else {
            // Types that are distinct in instance data type, or vtable, get distinct runtime type id and vtable.
            if (type->get_type_class() == TXTC_FUNCTION)
                continue;
            if (type->is_equivalent_derivation()) {
                //std::cerr << "Not registering distinct runtime type id for equivalent derivation: " << type << std::endl;
                continue;
            }
            type->runtimeTypeId = this->staticTypes.size();
        }
        this->staticTypes.push_back(type);
        //std::cerr << "Registering static type " << type << " with distinct runtime type id " << type->runtimeTypeId << std::endl;
    }
    ASSERT(this->createdTypes->empty(), "'Extra' types were created while register_types() was running");
    //for (auto type : *this->createdTypes)
    //    std::cerr << "'EXTRA' CREATED TYPE: " << type << std::endl;
    delete this->createdTypes;
    this->createdTypes = createdTypes;
}


const TxType* TypeRegistry::get_builtin_type(const BuiltinTypeId id, bool mod) {
    auto biType = this->package.builtin_types()->get_builtin_type( id );
    return ( mod ? this->get_modifiable_type( nullptr, biType ) : biType );
}



TxType* TypeRegistry::make_specialized_type(const TxTypeDeclaration* declaration,
                                            const TxTypeSpecialization& specialization,
                                            const std::vector<TxTypeSpecialization>& interfaces) {
    auto newType = specialization.type->make_specialized_type(declaration, specialization, interfaces);
    this->add_type(newType);
    return newType;
}



const TxType* TypeRegistry::get_modifiable_type(const TxTypeDeclaration* declaration, const TxType* type) {
    // 'modifiable' is always a distinct 'specialization' (no parameter bindings (or type extensions))
    while (type->is_empty_derivation() && !type->get_explicit_declaration())  //!type->is_explicit_nongen_declaration())
        type = type->get_base_type();
    ASSERT(!type->is_modifiable(), "Can't make a modifiable specialization of a modifiable type: " << type);

    if (! declaration) {
        std::string prefix = "~";
        std::string name = prefix + type->get_declaration()->get_unique_name();
        TxScopeSymbol* scope = type->get_declaration()->get_symbol()->get_outer();
        if (auto entitySymbol = dynamic_cast<TxEntitySymbol*>(scope->get_member_symbol(name))) {
            if (auto typeDecl = entitySymbol->get_type_decl()) {
                if (auto existingType = typeDecl->get_definer()->resolve_type()) {
                    if (existingType->is_modifiable() && *existingType->get_base_type() == *type)
                        return existingType;
                    //std::cerr << "existing: " << existingType << "  new: " << type << std::endl;
                }
            }
            type->LOGGER().warning("Name collision when trying to declare implicit MOD type, preexisting symbol: %s", entitySymbol->str().c_str());
            name = scope->make_unique_name(name);
        }

        const TxLocation& loc = ( declaration ? declaration->get_definer()->get_parse_location() : type->get_parse_location() );
        auto typeDefiner = type->get_declaration()->get_definer();
        auto & ctx = typeDefiner->context();
        auto modNode = new TxModifiableTypeNode(loc, new TxIdentifiedTypeNode(loc, type->get_declaration()->get_unique_name()));
        TxDeclarationFlags newDeclFlags = ( type->get_decl_flags() & DECL_FLAG_FILTER ); // | TXD_IMPLICIT;
        auto modDeclNode = new TxTypeDeclNode(loc, newDeclFlags, name, nullptr, modNode);
        modDeclNode->symbol_declaration_pass( ctx, ctx );
        modDeclNode->symbol_resolution_pass();
        return modNode->get_type();
    }

    return this->make_specialized_type(declaration, TxTypeSpecialization(type, true));
}

const TxType* TypeRegistry::get_empty_specialization(const TxTypeDeclaration* declaration, const TxType* type) {
    //std::cerr << "MAKING EMPTY TYPE: " << declaration->to_string() << std::endl;
    ASSERT(!type->is_modifiable(), "Can't specialize a 'modifiable' base type: " << type);
    if (type->is_modifiable()) {
        CERROR(declaration->get_definer(), "Can't declare specialization / alias of a modifiable type: "<< type);
        return nullptr;
    }
    while (type->is_empty_derivation() && !type->get_explicit_declaration())  //!type->is_explicit_nongen_declaration())
        type = type->get_base_type();
    return this->make_specialized_type(declaration, TxTypeSpecialization(type, false));
}



std::string encode_type_name(const TxType* type) {
    std::stringstream boundTypeName;
    if (type->is_modifiable() && !type->get_declaration()) {
        boundTypeName << "~";
        type = type->get_base_type();
    }
    ASSERT(type->get_declaration(), "Type does not have declaration: " << type);
    boundTypeName << hashify( type->get_declaration()->get_unique_full_name() );
    return boundTypeName.str();
}


static const TxType* get_existing_type(const TxType* baseType, const std::vector<TxDeclarationNode*>* bindingDeclNodes,
                                       TxScopeSymbol* baseScope, const std::string& newBaseName) {
    if (bindingDeclNodes->size() == baseType->type_params().size()) {
        // if generic type specialization is equivalent to the generic base type, reuse it:
        //std::cerr << "existingBaseType    0: " << baseType << std::endl;
        bool matchOK = true;
        for (auto bindingDeclNode : *bindingDeclNodes) {
            if (auto typeDeclNode = dynamic_cast<const TxTypeDeclNode*>( bindingDeclNode )) {
                auto paramName = typeDeclNode->typeName->str();
                auto paramDecl = baseType->get_type_param_decl( paramName );
                if (auto typeParamDecl = dynamic_cast<const TxTypeDeclaration*>(paramDecl)) {
                    auto bindingType = typeDeclNode->typeExpression->resolve_type();
                    auto constraintType = typeParamDecl->get_definer()->resolve_type();
                    ASSERT(constraintType, "NULL constraint type for type parameter " << typeParamDecl);
                    //baseScope->LOGGER().trace("bindingType decl:    %s", bindingType->get_declaration()->to_string().c_str());
                    //baseScope->LOGGER().trace("constraintType decl: %s", constraintType->get_declaration()->to_string().c_str());
                    if (auto bindingDecl = bindingType->get_declaration()) {
                        if (bindingDecl == constraintType->get_declaration())
                            continue;  // binding refers to "itself" (its parameter declaration)
                        if (! (bindingDecl->get_decl_flags() & TXD_GENPARAM)) {
                            // (if binding is to another type parameter it is an explicit unique type)
                            if (*constraintType == *bindingType)
                                continue;
                        }
                    }
                    else if (*constraintType == *bindingType)
                        continue;
                }
            }
            else {  // TxFieldDeclNode
                // VALUE parameters don't have "defaults"
            }
            matchOK = false;
            break;
        }
        if (matchOK) {
            baseScope->LOGGER().debug("new specialization equal to the generic base type, reusing: %s", baseType->str().c_str());
            return baseType;
        }
    }

    // if name already exists and specialization is equal, reuse it:
    if (auto existingBaseSymbol = dynamic_cast<TxEntitySymbol*>(baseScope->get_member_symbol(newBaseName))) {
        if (auto typeDecl = existingBaseSymbol->get_type_decl()) {
            auto existingBaseType = typeDecl->get_definer()->resolve_type();
            //std::cerr << "existingBaseType    1: " << existingBaseType << std::endl;
            auto existingGenBaseType = existingBaseType->get_semantic_base_type();
            //std::cerr << "existingGenBaseType 2: " << existingGenBaseType << std::endl;
            if (existingGenBaseType->is_empty_derivation()) {
                existingGenBaseType = existingGenBaseType->get_semantic_base_type();
                //std::cerr << "existingGenBaseType 3: " << existingGenBaseType << std::endl;
            }
            if (*existingGenBaseType == *baseType) {
                bool matchOK = true;
                for (auto bindingDeclNode : *bindingDeclNodes) {
                    if (auto typeDeclNode = dynamic_cast<const TxTypeDeclNode*>( bindingDeclNode )) {
                        auto paramName = typeDeclNode->typeName->str();
                        if (auto existingBinding = existingBaseType->get_binding( paramName )) {
                            //std::cerr << "found existing binding: " << existingBinding << std::endl;
                        //if (auto typeArgDecl = dynamic_cast<const TxTypeDeclaration*>( argDecl )) {
                            if (const TxType* membType = static_cast<const TxTypeDeclaration*>(existingBinding)->get_definer()->resolve_type()) {
                                //std::cerr << "COMPARING: " << membType << " AND " << typeArg->typeExprNode->get_type() << std::endl;
                                if (*membType == *typeDeclNode->typeExpression->resolve_type())
                                    continue;
                                //else
                                //    std::cerr << "BINDING MISMATCH: " << membType << " != " << typeArg->typeExprNode->get_type() << std::endl;
                            }
                            else
                                baseScope->LOGGER().warning("NULL type for member symbol %s", existingBinding->get_symbol()->str().c_str());
                        }
                    }
                    else {  // TxFieldDeclNode
                        // (For now, statically constant VALUE specializations with diff. values don't share the same static type.)
                        auto fieldDeclNode = dynamic_cast<const TxFieldDeclNode*>( bindingDeclNode );
                        auto paramName = fieldDeclNode->field->fieldName->str();
                        if (auto existingBinding = existingBaseType->get_binding( paramName )) {
                            auto existingFieldDecl = static_cast<const TxFieldDeclaration*>(existingBinding);
                            uint32_t newValue = 0;  // zero means dynamically specified value
                            uint32_t existingValue = 0;
                            if (auto newConstantValueProxy = fieldDeclNode->field->get_init_expression()->get_static_constant_proxy()) {
                                // new binding has statically constant value
                                newValue = newConstantValueProxy->get_value_UInt();
                            }
                            if (auto existingInitializer = existingFieldDecl->get_definer()->get_init_expression()) {
                                if (auto existingConstantValueProxy = existingInitializer->get_static_constant_proxy()) {
                                    // existing binding has statically constant value
                                    existingValue = existingConstantValueProxy->get_value_UInt();
                                }
                            }
                            // dynamic VALUE specializations get distinct compile time types, which hold the specific VALUE expressions
                            if (newValue != 0 && newValue == existingValue)
                                continue;
                        }
                    }
                    //std::cerr << "NOT ACCEPTING PRE-EXISTING TYPE " << existingBaseType << " SINCE " << std::endl;
                    matchOK = false;
                    break;
                }
                if (matchOK) {
                    baseScope->LOGGER().debug("new specialization equal to preexisting one, reusing: %s", existingBaseType->str().c_str());
                    return existingBaseType;
                }
            }
            baseScope->LOGGER().warning("Found existing but mismatching type with sought name: %s", typeDecl->str().c_str());
        }
    }
    return nullptr;
}


/** Makes a TYPE type parameter declaration node (both for bindings and param redeclarations). */
static TxDeclarationNode* make_type_type_param_decl_node( const TxLocation& parseLoc, const std::string& paramName, TxDeclarationFlags flags,
                                                          const TxTypeDeclaration* typeDecl ) {
    auto typeExpr = new TxTypeDeclWrapperNode( parseLoc, typeDecl );
    auto declNode = new TxTypeDeclNode(parseLoc, flags | TXD_PUBLIC, paramName, nullptr, typeExpr);
    return declNode;
}

/** Makes a VALUE type parameter declaration node (both for bindings and param redeclarations). */
static TxDeclarationNode* make_value_type_param_decl_node( const TxLocation& parseLoc, const std::string& paramName, TxDeclarationFlags flags,
                                                           const TxEntityDeclaration* paramValueTypeDecl, TxExpressionNode* valueDefiner=nullptr) {
    auto paramTypeNode = new TxTypeDeclWrapperNode( parseLoc, paramValueTypeDecl );
    auto fieldDef = new TxFieldDefNode(parseLoc, paramName, paramTypeNode, valueDefiner, false);
    auto declNode = new TxFieldDeclNode(parseLoc, flags | TXD_PUBLIC | TXD_IMPLICIT, fieldDef);
    return declNode;
}


const TxType* TypeRegistry::get_type_specialization( const TxTypeDefiningNode* definer, const TxType* baseType,
                                                     const std::vector<const TxTypeArgumentNode*>* bindings ) {
                                                     //const std::vector<TxGenericBinding>* bindings ) {
    // Note: type specialization is never applied to a modifiable-specialization (legal only on non-modifiable base type)
    // Note: A non-parameterized type (without any declared type parameters) is not necessarily non-generic:
    //       It may have members that refer to generic parameters declared in an outer scope.
    ASSERT(!baseType->is_modifiable(), "Can't specialize a 'modifiable' base type: " << baseType);
    ASSERT(bindings && !bindings->empty(), "NULL or empty bindings list when specializing baseType: " << baseType);
    ASSERT(definer, "NULL type definer");

    const TxTypeDeclaration* declaration = nullptr;
    if (auto typeExprNode = dynamic_cast<const TxTypeExpressionNode*>(definer))
        declaration = typeExprNode->get_declaration();

    while (baseType->is_empty_derivation() && !baseType->get_explicit_declaration())  //!baseType->is_explicit_nongen_declaration())
        baseType = baseType->get_base_type();

    if (baseType->get_decl_flags() & (TXD_GENPARAM | TXD_GENBINDING)) {
        // only empty derivation allowed from generic type parameter
        CERROR(definer, "Can't specialize a generic type parameter: " << baseType);
        return nullptr;
    }

    auto baseTypeParams = baseType->type_params();
    if (baseTypeParams.size() < bindings->size()) {
        CERROR(definer, "Too many type arguments specifified for generic base type " << baseType);
        return nullptr;
    }

    // Note: Binding of ref-constrained type parameters don't make the code generation more concrete,
    //       but accesses to the members still need the specialized version of them (e.g. the correct ref-target).

    // re-base the new type on new non-generic specialization of the base type:
    // (this replaces the type parameter bindings with direct declarations within the new type)
    this->package.LOGGER().debug("Re-basing non-parameterized type %s by specializing its parameterized base type %s",
                                 (declaration ? declaration->get_unique_full_name().c_str() : "--"), baseType->str().c_str());

    auto baseDecl = baseType->get_declaration();
    ASSERT(baseDecl, "base type has no declaration: " << baseType);

    if (baseType->get_type_class() == TXTC_REFERENCE) {
        //this->package.LOGGER().note("Specializing a reference and resolving binding's target type: %s", bindings->at(0).to_string().c_str());
        // TODO: Support recursive type definition by reference (CRTP) (e.g. type Bar Foo< Ref<Bar> >)
    }

    std::stringstream newBaseTypeName;
    TxDeclarationFlags newDeclFlags;
    // Note: The same generic type specialization may be produced by multiple statements,
    // both within ExpErr constructs and without. Therefore the type name must distinguish between them.
    if (declaration && (declaration->get_decl_flags() & TXD_EXPERRBLOCK)) {
        newBaseTypeName << "$EE$" << baseDecl->get_unique_name() << "<";
        newDeclFlags = ( baseDecl->get_decl_flags() & DECL_FLAG_FILTER ) | TXD_IMPLICIT | TXD_EXPERRBLOCK;
    }
    else {
        newBaseTypeName << "$" << baseDecl->get_unique_name() << "<";
        newDeclFlags = ( baseDecl->get_decl_flags() & DECL_FLAG_FILTER ) | TXD_IMPLICIT;
    }

    // make new parameter declarations that resolve to the bindings:
    auto bindingDeclNodes = new std::vector<TxDeclarationNode*>();
    unsigned bindCount = 0;
    for (auto binding : *bindings) {
        auto paramDecl = baseTypeParams.at( bindCount );
        auto paramName = paramDecl->get_unique_name();
        if (bindCount++)  newBaseTypeName << ",";

        if (auto typeArg = dynamic_cast<const TxTypeTypeArgumentNode*>( binding )) {
//            // experimentation with not resolving the bound type:
//            auto bindingDef = new TxTypeExprWrapperNode( static_cast<TxTypeExpressionNode*>( &binding.type_definer() ) );
//            auto bindingDecl = new TxTypeDeclNode( binding.get_parse_location(), TXD_GENBINDING | TXD_PUBLIC , binding.param_name(), nullptr, bindingDef );
//            bindingDeclNodes->push_back( bindingDecl );
//            newBaseTypeName << bindingDef->get_declared_name();
//            newBindings.emplace_back( binding );  // (newBindings are identical to bindings unless we want to bypass empty derivations)
//            continue;

            // we want to bypass empty, implicit derivations:
            auto btype = typeArg->typeExprNode->resolve_type();
            if (! btype)
                return nullptr;  // specialization fails if a binding fails resolve
            while (btype->is_empty_derivation()
                    && ( (! btype->get_declaration())
                         || (btype->get_declaration()->get_decl_flags() & (TXD_IMPLICIT | TXD_GENBINDING) ) ) ) {
                // if no declaration or implicit declaration, or
                // if binding refers to a previous binding (i.e. in surrounding scope), fold it
                //std::cerr << "###### skipping bindingType to base type: " << btype << std::endl;
                btype = btype->get_base_type();
            }

            newBaseTypeName << encode_type_name(btype);

            bindingDeclNodes->push_back( make_type_type_param_decl_node( typeArg->get_parse_location(), paramName,
                                                                         TXD_GENBINDING, btype->get_declaration() ) );
            package.LOGGER().trace("Re-bound base type %s parameter '%s' with %s", baseDecl->get_unique_full_name().c_str(),
                                   paramName.c_str(), btype->str().c_str());
        }
        else {
            auto valueArg = static_cast<const TxValueTypeArgumentNode*>( binding );

            // implementation note: binding's value expression not necessarily 'resolved' at this point
            if (auto bindingValueProxy = valueArg->valueExprNode->get_static_constant_proxy()) {
                uint32_t bindingValue = bindingValueProxy->get_value_UInt();
                newBaseTypeName << bindingValue;  // statically known value
            }
            else {
                newBaseTypeName << "$VALUE";  // dynamic value
                // implementation note: a distinct compile time type is registered which holds this specific dynamic value expression
            }

            bindingDeclNodes->push_back( make_value_type_param_decl_node( valueArg->get_parse_location(), "$" + paramName,
                                                                          TXD_GENBINDING, paramDecl, valueArg->valueExprNode ) );
            package.LOGGER().trace("Re-bound base type %s parameter '%s' with %s", baseDecl->get_unique_full_name().c_str(),
                                   paramName.c_str(), valueArg->valueExprNode->str().c_str());
        }
    }
    newBaseTypeName << ">";
    auto newBaseTypeNameStr = newBaseTypeName.str();

    // if equivalent specialized type already exists then reuse it, otherwise create new one:
    auto baseScope = baseDecl->get_symbol()->get_outer();
    const TxType* specializedBaseType = get_existing_type(baseType, bindingDeclNodes, baseScope, newBaseTypeNameStr);
    if (! specializedBaseType) {
        // If any parameter is not bound, the parameter is redeclared (inherited) as still-unbound type parameter:
        // (Type args are always specified from left to right, so unbound params will be at end of param list.)
        for (auto unboundParamI = baseType->type_params().cbegin() + bindings->size();
                unboundParamI != baseType->type_params().cend(); unboundParamI++ ) {
            auto unboundParamDecl = *unboundParamI;
            package.LOGGER().note("Implicitly inheriting (redeclaring) type parameter %s in type %s",
                                  unboundParamDecl->get_unique_full_name().c_str(), newBaseTypeNameStr.c_str());
            if (auto typeDecl = dynamic_cast<const TxTypeDeclaration*>(unboundParamDecl)) {
                bindingDeclNodes->push_back( make_type_type_param_decl_node( definer->get_parse_location(), typeDecl->get_unique_name(),
                                                                             TXD_GENPARAM, typeDecl ) );
            }
            else {
                auto fieldDecl = static_cast<const TxFieldDeclaration*>(unboundParamDecl);
                auto paramDecl = baseType->get_type_param_decl(fieldDecl->get_unique_name());
                bindingDeclNodes->push_back( make_value_type_param_decl_node( definer->get_parse_location(), fieldDecl->get_unique_name(),
                                                                              TXD_GENPARAM, paramDecl ) );
            }
        }

        {   // pass on the generic base type to the new specialization via member named $GenericBase:
            auto baseTypeExpr = new TxTypeDeclWrapperNode( definer->get_parse_location(), baseDecl );
            auto declNode = new TxTypeDeclNode( definer->get_parse_location(), TXD_PUBLIC | TXD_IMPLICIT, "$GenericBase", nullptr, baseTypeExpr );
            bindingDeclNodes->push_back( declNode );
        }

        // process new specialization of the base type:
        //std::cerr << "specializing base " << newBaseTypeNameStr << ": " << baseType << std::endl;
        auto baseTypeExpr = dynamic_cast<TxTypeExpressionNode*>( baseDecl->get_definer() );
        ASSERT(baseTypeExpr, "baseTypeExpression not a TxTypeExpressionNode: " << baseDecl->get_definer());
        ASSERT(baseTypeExpr->context().scope() == baseScope, "Unexpected lexical scope: " << baseTypeExpr->context().scope() << " != " << baseScope);
        LexicalContext newBaseContext = LexicalContext( baseScope, definer->exp_err_ctx(), true );

        newBaseTypeNameStr = newBaseContext.scope()->make_unique_name(newBaseTypeNameStr, true);
        auto newBaseTypeExpr = baseTypeExpr->make_ast_copy();
        auto newBaseTypeDecl = new TxTypeDeclNode( definer->get_parse_location(), newDeclFlags, newBaseTypeNameStr, bindingDeclNodes, newBaseTypeExpr );
        newBaseTypeDecl->symbol_declaration_pass( newBaseContext, newBaseContext );

        // Invoking the resolution pass here can cause infinite recursion
        // (since the same source text construct may be recursively reprocessed),
        // so we enqueue this "specialization pass" for later processing.
        //std::cerr << "enqueuing specialization " << newBaseTypeNameStr << "  " << newBaseTypeDecl << std::endl;
        this->enqueuedSpecializations.emplace_back( newBaseTypeDecl );
        specializedBaseType = newBaseTypeExpr->resolve_type();
    }
    // TODO: else bindingDeclNodes thrown away...

    if (declaration)
        return this->make_specialized_type( declaration, TxTypeSpecialization(specializedBaseType) );
    else
        return specializedBaseType;
}


const TxType* TypeRegistry::get_type_derivation( const TxTypeExpressionNode* definer, const TxType* baseType,
                                                 const std::vector<TxTypeSpecialization>& interfaces, bool _mutable ) {
    ASSERT(definer->get_declaration(), "NULL declaration in " << definer);
    ASSERT(!baseType->is_modifiable(), "Can't specialize a 'modifiable' base type: " << baseType);

    while (baseType->is_empty_derivation() && !baseType->get_explicit_declaration())
        baseType = baseType->get_base_type();

    if (baseType->get_decl_flags() & (TXD_GENPARAM | TXD_GENBINDING)) {
        // only empty derivation allowed from generic type parameter
        if (!interfaces.empty()) {
            CERROR(definer, "Can't specialize a generic type parameter: " << baseType);
            return nullptr;
        }
    }

    // TODO: pass _mutable flag to type extensions

    return this->make_specialized_type(definer->get_declaration(), TxTypeSpecialization(baseType), interfaces);
}


const TxType* TypeRegistry::get_interface_adapter(const TxType* interfaceType, const TxType* adaptedType) {
    auto modIf = interfaceType->is_modifiable();
    while (interfaceType->is_same_vtable_type())
        interfaceType = interfaceType->get_semantic_base_type();
    while (adaptedType->is_same_vtable_type())
        adaptedType = adaptedType->get_semantic_base_type();

    auto adapterType = this->package.builtin_types()->inner_get_interface_adapter(interfaceType, adaptedType);
//    ASSERT(adapterType->adapted_type()->get_type_id() == adaptedType->get_type_id(),
//           "Mismatching type ids between adapter and adaptee: " << adapterType->adapted_type()->get_type_id() << " != " << adaptedType->get_type_id());
    if (modIf)
        return this->get_modifiable_type(nullptr, adapterType);
    else
        return adapterType;
}



// FUTURE: maybe remove the declaration argument, disallowing 'user' type names for references?
const TxReferenceType* TypeRegistry::get_reference_type( const TxTypeDefiningNode* definer, const TxTypeTypeArgumentNode* targetTypeBinding,
                                                         const TxIdentifier* dataspace ) {
    std::vector<const TxTypeArgumentNode*> bindings( { targetTypeBinding } );
    return static_cast<const TxReferenceType*>( this->get_type_specialization( definer, this->get_builtin_type( REFERENCE ), &bindings ) );
}



const TxArrayType* TypeRegistry::get_array_type( const TxTypeDefiningNode* definer, const TxTypeTypeArgumentNode* elemTypeBinding,
                                                 const TxValueTypeArgumentNode* lengthBinding ) {
    std::vector<const TxTypeArgumentNode*> bindings( { elemTypeBinding, lengthBinding } );
    return static_cast<const TxArrayType*>( this->get_type_specialization( definer, this->get_builtin_type( ARRAY ), &bindings ) );
}

const TxArrayType* TypeRegistry::get_array_type( const TxTypeDefiningNode* definer, const TxTypeTypeArgumentNode* elemTypeBinding ) {
    std::vector<const TxTypeArgumentNode*> bindings( { elemTypeBinding } );
    return static_cast<const TxArrayType*>( this->get_type_specialization( definer, this->get_builtin_type( ARRAY ), &bindings ) );
}



const TxFunctionType* TypeRegistry::get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, const TxType* returnType,
                                                      bool modifiableClosure) {
    auto funcType = new TxFunctionType(declaration, this->get_builtin_type( FUNCTION ), argumentTypes, returnType, modifiableClosure);
    this->add_type(funcType);
    return funcType;
}

const TxFunctionType* TypeRegistry::get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                                      bool modifiableClosure) {
    auto funcType = new TxFunctionType(declaration, this->get_builtin_type( FUNCTION ), argumentTypes, nullptr, modifiableClosure);
    this->add_type(funcType);
    return funcType;
}

const TxConstructorType* TypeRegistry::get_constructor_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                                            TxTypeDeclaration* objectTypeDecl) {
    auto type = new TxConstructorType(declaration, this->get_builtin_type( FUNCTION ), argumentTypes, objectTypeDecl);
    this->add_type(type);
    return type;
}
