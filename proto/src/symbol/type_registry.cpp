#include "util/assert.hpp"

#include "type_registry.hpp"

#include "tx_logging.hpp"
#include "package.hpp"
#include "entity.hpp"
#include "ast.hpp"
#include "builtin/builtin_types.hpp"

#include "llvm_generator.hpp"



/** the flags that may be inherited when specializing a type */
static const TxDeclarationFlags DECL_FLAG_FILTER = TXD_STATIC | TXD_PUBLIC | TXD_PROTECTED | TXD_ABSTRACT | TXD_FINAL | TXD_IMPLICIT | TXD_EXPERRBLOCK;




TypeRegistry::TypeRegistry(TxPackage& package)
        : package(package) {
    this->createdTypes = new std::vector<TxActualType*>();
}


const TxLocation& TypeRegistry::get_builtin_location() const {
    return this->package.builtin_types()->get_builtin_location();
}


//static bool matches_existing_type( const TxType* newBaseType, const TxType* existingBaseType );

void TypeRegistry::deferred_type_resolution_pass() {
    this->resolve_deferred_types();

    this->prepare_types();
}


void TypeRegistry::resolve_deferred_types() {
    // Note: Queues can be appended to during processing.
    unsigned typeIx = 0;
    unsigned specIx = 0;
    do {
        for ( ; typeIx != this->usedTypes.size(); typeIx++ ) {
            //std::cerr << "Nof used types: " << this->usedTypes.size() << std::endl;
            auto type = this->usedTypes.at( typeIx );
            if (! type->is_builtin())
                LOG_TRACE( (&this->package.LOGGER()), "Resolving deferred type: " << type );
            type->type();
        }

        for ( ; specIx != this->enqueuedSpecializations.size(); specIx++) {
            //std::cerr << "Nof enqueued specializations: " << this->enqueuedSpecializations.size() << std::endl;
            auto specDecl = this->enqueuedSpecializations.at( specIx );
            LOG( (&this->package.LOGGER()), INFO, "Resolving enqueued specialization: " << specDecl );
            specDecl->symbol_resolution_pass();
        }
    } while ( typeIx != this->usedTypes.size() );
}


//void TypeRegistry::enqueued_resolution_pass() {
////    std::unordered_multimap<std::string,const TxType*> existingSpecializations;
//    // Note: Queue can be appended to during processing.
//    for (unsigned i = 0; i != this->enqueuedSpecializations.size(); i++) {
//        //std::cerr << "Nof enqueued specializations: " << this->enqueuedSpecializations.size() << std::endl;
//        auto specDecl = this->enqueuedSpecializations.at(i);
//
////        auto newType = const_cast<TxType*>( specDecl->typeExpression->get_type() );
////        auto genericBase = newType->get_semantic_base_type();
////        auto baseName = genericBase->get_declaration()->get_unique_full_name();
////
////        // if new type is equal to an existing one, merge them
////        auto range = existingSpecializations.equal_range( baseName );
////        for( auto si = range.first; si != range.second; si++ ) {
////            auto existingType = si->second;
////            //std::cerr << "comparing against " << existingType << std::endl;
////            if ( matches_existing_type( newType, existingType ) ) {
////                this->package.LOGGER().info("new specialization equal to preexisting one, reusing: %s", existingType->str().c_str());
////                std::cerr << "  new spec: " << newType << std::endl;
////                std::cerr << " prev spec: " << existingType << std::endl;
////                newType->rewire( existingType );
////                goto SKIP_ENQUEUED;
////            }
////        }
////        existingSpecializations.emplace( baseName, newType );
//
//        this->package.LOGGER().info("Resolving enqueued specialization: %s", specDecl->str().c_str());
//        specDecl->symbol_resolution_pass();
//
////        SKIP_ENQUEUED:
////        ;
//    }
//}


void TypeRegistry::add_type_usage(TxType* type) {
    ASSERT (!this->startedPreparingTypes, "Can't create new types when type preparation phase has started: " << type);
    this->usedTypes.push_back(type);
}


void TypeRegistry::add_type(TxActualType* type) {
    ASSERT (!this->startedPreparingTypes, "Can't create new types when type preparation phase has started: " << type);
    this->createdTypes->push_back(type);
}

void TypeRegistry::prepare_types() {
    this->startedPreparingTypes = true;
    auto createdTypes = this->createdTypes;
    this->createdTypes = new std::vector<TxActualType*>();
    for (auto type : *createdTypes) {
        //std::cerr << "Preparing type: " << type << std::endl;
        type->prepare_members();
        if (type->is_builtin() && !type->is_modifiable()) {
            ASSERT(type->staticTypeId == this->staticTypes.size(), "preparing built-in type in wrong order / id: " << type->staticTypeId << ": " << type);
        }
        else {
            // Types that are distinct in instance data type, or vtable, get distinct static type id and vtable.
            if (type->get_declaration() && ( type->get_declaration()->get_decl_flags() & TXD_EXPERRBLOCK ))
                continue;
            if (type->get_type_class() == TXTC_FUNCTION)
                continue;
            if (type->is_equivalent_derivation()) {
                //std::cerr << "Not registering distinct static type id for equivalent derivation: " << type << std::endl;
                continue;
            }
            type->staticTypeId = this->staticTypes.size();
        }
        this->staticTypes.push_back(type);
        //std::cerr << "Registering static type " << type << " with distinct type id " << type->runtimeTypeId << std::endl;
    }
    //ASSERT(this->createdTypes->empty(), "'Extra' types were created while prepare_types() was running");
    //for (auto type : *this->createdTypes)
    //    std::cerr << "'EXTRA' CREATED TYPE: " << type << std::endl;
    delete this->createdTypes;
    this->createdTypes = createdTypes;
}


const TxType* TypeRegistry::get_builtin_type(const BuiltinTypeId id, bool mod) {
    auto biType = this->package.builtin_types()->get_builtin_type( id );
    return ( mod ? this->get_modifiable_type( nullptr, biType ) : biType );
}



TxActualType* TypeRegistry::make_specialized_type( const TxTypeDeclaration* declaration,
                                                   const TxActualType* baseType,
                                                   const std::vector<const TxType*>& interfaces,
                                                   bool modifiable ) {
    std::vector<TxTypeSpecialization> interfaceSpecializations;
    for (auto in : interfaces)
        interfaceSpecializations.emplace_back( in->type() );
    auto newType = baseType->make_specialized_type( declaration, TxTypeSpecialization( baseType, modifiable ), interfaceSpecializations );
    this->add_type( newType );
    return newType;
}



const TxType* TypeRegistry::get_modifiable_type(const TxTypeDeclaration* declaration, const TxType* type) {
    auto actualType = type->type();
    // 'modifiable' is always a distinct 'specialization' (no parameter bindings (or type extensions))
    while (actualType->is_empty_derivation() && !actualType->get_explicit_declaration())  //!type->is_explicit_nongen_declaration())
        actualType = actualType->get_base_type();
    ASSERT(!actualType->is_modifiable(), "Can't make a modifiable specialization of a modifiable type: " << actualType);

    if (! declaration) {
        std::string prefix = "~";
        std::string name = prefix + actualType->get_declaration()->get_unique_name();
        TxScopeSymbol* scope = actualType->get_declaration()->get_symbol()->get_outer();
        if (auto entitySymbol = dynamic_cast<TxEntitySymbol*>(scope->get_member_symbol(name))) {
            if (auto typeDecl = entitySymbol->get_type_decl()) {
                if (auto existingType = typeDecl->get_definer()->resolve_type()) {
                    if (existingType->is_modifiable() && *existingType->get_base_type()->type() == *actualType)
                        return existingType;
                    //std::cerr << "existing: " << existingType << "  new: " << type << std::endl;
                }
            }
            LOG(actualType->LOGGER(), WARN, "Name collision when trying to declare implicit MOD type, preexisting symbol: " << entitySymbol);
            name = scope->make_unique_name(name);
        }

        const TxLocation& loc = ( declaration ? declaration->get_definer()->get_parse_location() : actualType->get_parse_location() );
        auto typeDefiner = actualType->get_declaration()->get_definer();
        auto & ctx = typeDefiner->context();
        auto modNode = new TxModifiableTypeNode(loc, new TxIdentifiedTypeNode(loc, actualType->get_declaration()->get_unique_name()));
        TxDeclarationFlags newDeclFlags = ( actualType->get_declaration()->get_decl_flags() & DECL_FLAG_FILTER ); // | TXD_IMPLICIT;
        auto modDeclNode = new TxTypeDeclNode(loc, newDeclFlags, name, nullptr, modNode);
        modDeclNode->symbol_declaration_pass( ctx, ctx );
        modDeclNode->symbol_resolution_pass();
        return modNode->get_type();
    }

    return new TxType( this->make_specialized_type( declaration, actualType, {}, true ) );
}

const TxType* TypeRegistry::get_empty_specialization(const TxTypeDeclaration* declaration, const TxType* type) {
    return new TxType( declaration->get_definer(),
                       [ this, declaration, type ] () {
                           return this->get_actual_empty_specialization( declaration, type->type() );
                       } );
}

const TxActualType* TypeRegistry::get_actual_empty_specialization(const TxTypeDeclaration* declaration, const TxActualType* actualType) {
    //std::cerr << "MAKING EMPTY TYPE: " << declaration->to_string() << std::endl;
    ASSERT(!actualType->is_modifiable(), "Can't specialize a 'modifiable' base type: " << actualType);
    if (actualType->is_modifiable()) {
        CERROR(declaration->get_definer(), "Can't declare specialization / alias of a modifiable type: "<< actualType);
        return nullptr;
    }
    while (actualType->is_empty_derivation() && !actualType->get_explicit_declaration())  //!type->is_explicit_nongen_declaration())
        actualType = actualType->get_base_type();
    return this->make_specialized_type( declaration, actualType );
}



std::string encode_type_name( const TxTypeDeclaration* typeDecl ) {
    std::stringstream boundTypeName;
    boundTypeName << hashify( typeDecl->get_unique_full_name() );
    return boundTypeName.str();
}


static const TxActualType* matches_existing_type( TxEntitySymbol* existingBaseSymbol, const TxActualType* baseType,
                                                  const std::vector<TxDeclarationNode*>* bindingDeclNodes );

/**
 * Note: The provided binding declaration nodes are "node candidates", and have not run declaration or resolution passes.
 */
static const TxActualType* get_existing_type( const TxActualType* baseType, const std::vector<TxDeclarationNode*>* bindingDeclNodes,
                                              TxScopeSymbol* baseScope, const std::string& newBaseName ) {
    // pre-verify that all type bindings resolve:
    for (auto bindingDeclNode : *bindingDeclNodes) {
        if (auto typeDeclNode = dynamic_cast<const TxTypeDeclNode*>( bindingDeclNode )) {
            if (typeDeclNode->typeExpression->resolve_type()) {
                continue;
            }
            //std::cerr << "NULL binding: " << bindingDeclNode << std::endl;
            return nullptr;
        }
    }

    if (bindingDeclNodes->size() == baseType->type_params().size()) { // && baseType->get_type_class() != TXTC_REFERENCE) {
        // if generic type specialization is equivalent to the generic base type, reuse it:
        //std::cerr << "existingBaseType    0: " << baseType << std::endl;
        bool matchOK = true;
        for (auto bindingDeclNode : *bindingDeclNodes) {
            if (auto typeDeclNode = dynamic_cast<const TxTypeDeclNode*>( bindingDeclNode )) {
                auto paramName = typeDeclNode->typeName->str();
                auto paramDecl = baseType->get_type_param_decl( paramName );
                if (auto typeParamDecl = dynamic_cast<const TxTypeDeclaration*>(paramDecl)) {
                    auto constraintType = typeParamDecl->get_definer()->resolve_type();
                    ASSERT(constraintType, "NULL constraint type for type parameter " << typeParamDecl);
                    //ASSERT(dynamic_cast<TxTypeDeclWrapperNode*>(typeDeclNode->typeExpression), "Expected TxTypeDeclWrapperNode* but was " << typeDeclNode->typeExpression);
//                    std::cerr << "constraintType: " << constraintType << std::endl;
                    if (auto declWrapper = dynamic_cast<TxTypeDeclWrapperNode*>( typeDeclNode->typeExpression )) {
                        std::cerr << "    bound type: " << typeDeclNode->typeExpression->resolve_type() << std::endl;
                        if (auto bindingDecl = dynamic_cast<const TxTypeDeclaration*>( declWrapper->get_wrapped_declaration() )) {
                            if (bindingDecl == constraintType->get_declaration()) {
                                //std::cerr << "binding refers to 'itself' (its parameter declaration): " << bindingDecl << std::endl;
                                continue;  // binding refers to "itself" (its parameter declaration)
                            }
                        }
                    }
                    else if (dynamic_cast<TxTypeExprWrapperNode*>( typeDeclNode->typeExpression )) {
                        if (auto bindingType = typeDeclNode->typeExpression->resolve_type()) {
//                        std::cerr << "    bound type: " << bindingType << std::endl;
                        if (bindingType->get_decl_flags() & TXD_GENPARAM) {
                            auto bindingDecl = bindingType->get_declaration();
                            if (bindingDecl == constraintType->get_declaration()) {
                                //std::cerr << "binding refers to 'itself' (its parameter declaration): " << bindingDecl << std::endl;
                                continue;  // binding refers to "itself" (its parameter declaration)
                            }
                            // else binding is to a different type parameter and is effectively an explicit unique type
                        }
                        else if (constraintType == bindingType)
                            continue;
                        //}
//                        else
//                            std::cerr << " not considering binding to GENPARAM: " << bindingType << std::endl;
//                    else if (*constraintType == *bindingType)
//                        continue;
                        }
                    }
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
    //std::cerr << "looking for '" << newBaseName << "'" << std::endl;
    std::string upperBound = newBaseName;
    upperBound[upperBound.size()-1] += 1;
    for (auto existingBaseNameI = baseScope->alpha_order_names_lower( newBaseName );
             existingBaseNameI != baseScope->alpha_order_names_upper( upperBound ); existingBaseNameI++ ) {
        //std::cerr << "      found '" << *existingBaseNameI << "'" << std::endl;
        if (auto existingBaseSymbol = dynamic_cast<TxEntitySymbol*>( baseScope->get_member_symbol( *existingBaseNameI ) )) {
            if (auto matchingType = matches_existing_type( existingBaseSymbol, baseType, bindingDeclNodes ))
                return matchingType;
        }
    }
    return nullptr;
}


static const TxActualType* matches_existing_type( TxEntitySymbol* existingBaseSymbol, const TxActualType* baseType,
                                                  const std::vector<TxDeclarationNode*>* bindingDeclNodes ) {
        if (auto typeDecl = existingBaseSymbol->get_type_decl()) {
            auto existingBaseType = typeDecl->get_definer()->resolve_type()->type();
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
                        if (auto existingTypeDecl = dynamic_cast<const TxTypeDeclaration*>( existingBaseType->get_binding( paramName ) )) {
                            if (auto newBindingType = typeDeclNode->typeExpression->resolve_type()) {
                                auto existingBindingType = existingTypeDecl->get_definer()->resolve_type();
//                                if (!existingBindingType->type())
//                                    return nullptr;
                                //std::cerr << "new bound type:      " << newBindingType << std::endl
                                //          << "existing bound type: " << existingBindingType << std::endl;
                                if (newBindingType == existingBindingType)
                                    continue;
                            }
                        }
                    }
                    else {  // TxFieldDeclNode
                        // (For now, statically constant VALUE specializations with diff. values don't share the same static type.)
                        auto fieldDeclNode = dynamic_cast<const TxFieldDeclNode*>( bindingDeclNode );
                        auto paramName = fieldDeclNode->field->fieldName->str();
                        if (auto existingFieldDecl = dynamic_cast<const TxFieldDeclaration*>( existingBaseType->get_binding( paramName ) )) {
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
                    existingBaseSymbol->LOGGER().debug("new specialization equal to preexisting one, reusing: %s", existingBaseType->str().c_str());
                    return existingBaseType;
                }
            }
            //existingBaseSymbol->LOGGER().info("Found existing but mismatching type with sought name: %s", existingBaseType->str().c_str());
        }

    return nullptr;
}


/** Makes a TYPE type parameter declaration node (both for bindings and param redeclarations).
 * Note, since this creates a wrapper around an existing declaration, it creates a type alias. */
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
                                                     const std::vector<const TxTypeArgumentNode*>& bindings ) {
    ASSERT(bindings.at(0)->is_context_set(), "context not set for binding " << bindings.at(0));
    return new TxType( definer,
                       [ this, definer, baseType, bindings ] () {
                           return this->get_actual_type_specialization( definer, baseType->type(), &bindings );
                       } );
}

const TxActualType* TypeRegistry::get_actual_type_specialization( const TxTypeDefiningNode* definer, const TxActualType* baseType,
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

    if (baseType->get_declaration() && (baseType->get_declaration()->get_decl_flags() & (TXD_GENPARAM | TXD_GENBINDING))) {
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
    this->package.LOGGER().debug("Re-basing the new type %s by specializing its generic base type %s",
                                 (declaration ? declaration->get_unique_full_name().c_str() : "(unnamed)"), baseType->str().c_str());

    auto baseDecl = baseType->get_declaration();
    ASSERT(baseDecl, "base type has no declaration: " << baseType);

    std::stringstream newBaseTypeName;
    TxDeclarationFlags newDeclFlags;
    // Note: The same generic type specialization may be produced by multiple statements,
    // both within ExpErr constructs and without. Therefore the type name must distinguish between them.
    // Note: Implicit types (without explicit declaration) produced by ExpErr statements are identified by presence of ExpErrCtx.
    if ( ( declaration && (declaration->get_decl_flags() & TXD_EXPERRBLOCK) )
         || ( !declaration && definer->exp_err_ctx() ) ) {
        newBaseTypeName << "$EE$" << baseDecl->get_unique_name() << "<";
        newDeclFlags = ( baseDecl->get_decl_flags() & DECL_FLAG_FILTER ) | TXD_IMPLICIT | TXD_EXPERRBLOCK;
        //definer->get_parse_location().parserCtx->begin_exp_err( definer );
    }
    else {
        newBaseTypeName << baseDecl->get_unique_name() << "<";
        newDeclFlags = ( baseDecl->get_decl_flags() & DECL_FLAG_FILTER ) | TXD_IMPLICIT;
    }

    // make new parameter declarations that resolve to the bindings:
    auto bindingDeclNodes = new std::vector<TxDeclarationNode*>();
    unsigned bindCount = 0;
    for (auto binding : *bindings) {
        ASSERT(binding->is_context_set(), "Binding must have run declaration pass before being used in type specialization: " << binding);
        auto paramDecl = baseTypeParams.at( bindCount );
        auto paramName = paramDecl->get_unique_name();
        if (bindCount++)  newBaseTypeName << ",";

        if (auto typeArg = dynamic_cast<const TxTypeTypeArgumentNode*>( binding )) {
            TxTypeDefiningNode* btypeDefNode = typeArg->typeExprNode;
            TxTypeExpressionNode* btypeExprNode;
//            if (baseType->get_type_class() == TXTC_REFERENCE) {
//                // For references we may not resolve the bound target type, but we do fold/bypass 'alias' declarations.
                do {
                    if ( auto maybeModNode = dynamic_cast<TxMaybeModTypeNode*>( btypeDefNode) ) {
                        if (! maybeModNode->is_modifiable()) {
                            btypeDefNode = maybeModNode->baseType;
                            continue;
                        }
                    }
                    else if ( auto idTypeNode = dynamic_cast<TxIdentifiedTypeNode*>( btypeDefNode) ) {
                        // we don't resolve the bound type, but we do perform a lookup step
                        if (auto identifiedTypeDecl = lookup_type(idTypeNode->context().scope(), *idTypeNode->ident)) {
                            if (idTypeNode != identifiedTypeDecl->get_definer()) {
                                // an alias, bypass once
                                btypeDefNode = identifiedTypeDecl->get_definer();
                            }
                        }
                    }
                    break;
                } while(true);
                btypeExprNode = new TxTypeExprWrapperNode( btypeDefNode );
//            }
//            else {
//                // For non-references we resolve the bindings (this also catches invalid recursive type definitions).
//                auto btype = typeArg->typeExprNode->resolve_type();
//                if (! btype)
//                    return nullptr;  // specialization fails if a binding fails resolve
//                btypeExprNode = new TxTypeDeclWrapperNode( typeArg->get_parse_location(), btype->get_declaration() );
//            }

//            auto encodedTypeName = hashify( btypeExprNode->get_auto_type_name() );
//            newBaseTypeName << encodedTypeName;
//            TxTypeExpressionNode* btypeExprNode = new TxTypeExprWrapperNode( btypeDefNode );
            newBaseTypeName << "$";

//            bindingDeclNodes->push_back( make_type_type_param_decl_node( typeArg->get_parse_location(), paramName,
//                                                                         TXD_GENBINDING, btype->get_declaration() ) );
            bindingDeclNodes->push_back( new TxTypeDeclNode( typeArg->get_parse_location(), TXD_GENBINDING | TXD_PUBLIC, paramName, nullptr,
                                                             btypeExprNode ) );

            package.LOGGER().trace("Re-bound base type %s parameter '%s' with %s", baseDecl->get_unique_full_name().c_str(),
                                   paramName.c_str(), typeArg->typeExprNode->str().c_str());
        }
        else {
            auto valueArg = static_cast<const TxValueTypeArgumentNode*>( binding );

            // implementation note: binding's value expression not necessarily 'resolved' at this point
            if (auto bindingValueProxy = valueArg->valueExprNode->get_static_constant_proxy()) {
                uint32_t bindingValue = bindingValueProxy->get_value_UInt();
                newBaseTypeName << bindingValue;  // statically known value
            }
            else {
                newBaseTypeName << "$V";  // dynamic value
                // implementation note: a distinct compile time type is registered which holds this specific dynamic value expression
            }

            bindingDeclNodes->push_back( make_value_type_param_decl_node( valueArg->get_parse_location(), paramName,
                                                                          TXD_GENBINDING, paramDecl, valueArg->valueExprNode ) );
            package.LOGGER().trace("Re-bound base type %s parameter '%s' with %s", baseDecl->get_unique_full_name().c_str(),
                                   paramName.c_str(), valueArg->valueExprNode->str().c_str());
        }
    }
    newBaseTypeName << ">";
    auto newBaseTypeNameStr = newBaseTypeName.str();

    // if equivalent specialized type already exists then reuse it, otherwise create new one:
    auto baseScope = baseDecl->get_symbol()->get_outer();
    const TxActualType* specializedBaseType = get_existing_type(baseType, bindingDeclNodes, baseScope, newBaseTypeNameStr);
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
        newBaseTypeNameStr = baseScope->make_unique_name(newBaseTypeNameStr, true);

        auto newBaseTypeExpr = baseTypeExpr->make_ast_copy();
        auto newBaseTypeDecl = new TxTypeDeclNode( definer->get_parse_location(), newDeclFlags, newBaseTypeNameStr, bindingDeclNodes, newBaseTypeExpr );

        LexicalContext newBaseContext = LexicalContext( baseScope, definer->exp_err_ctx(), true );
        newBaseTypeDecl->symbol_declaration_pass( newBaseContext, newBaseContext );
        specializedBaseType = newBaseTypeExpr->resolve_type()->type();
        this->package.LOGGER().debug("Created new specialized type %s", specializedBaseType->str().c_str());

        // Invoking the resolution pass here can cause infinite recursion
        // (since the same source text construct may be recursively reprocessed,
        //  and the bindings may refer to this type's declaration),
        // so we enqueue this "specialization resolution pass" for later processing.
        //std::cerr << "enqueuing specialization " << newBaseTypeDecl << std::endl;
        this->enqueuedSpecializations.emplace_back( newBaseTypeDecl );
    }
    // TODO: else bindingDeclNodes thrown away...

    if (declaration)
        specializedBaseType = this->make_specialized_type( declaration, specializedBaseType );

//    if (newDeclFlags & TXD_EXPERRBLOCK) {
//        definer->get_parse_location().parserCtx->end_exp_err( definer->get_parse_location() );
//    }

    return specializedBaseType;
}



const TxType* TypeRegistry::get_type_derivation( const TxTypeExpressionNode* definer, const TxType* baseType,
                                                 const std::vector<const TxType*>& interfaces, bool _mutable ) {
    return new TxType( definer,
                       [ this, definer, baseType, interfaces, _mutable ] () {
                           return this->get_actual_type_derivation( definer, baseType->type(), interfaces, _mutable );
                       } );
}

const TxActualType* TypeRegistry::get_actual_type_derivation( const TxTypeExpressionNode* definer, const TxActualType* baseType,
                                                              const std::vector<const TxType*>& interfaces, bool _mutable ) {
    ASSERT(definer->get_declaration(), "NULL declaration in " << definer);
    ASSERT(!baseType->is_modifiable(), "Can't specialize a 'modifiable' base type: " << baseType);

    while (baseType->is_empty_derivation() && !baseType->get_explicit_declaration())
        baseType = baseType->get_base_type();

    if (baseType->get_declaration()->get_decl_flags() & (TXD_GENPARAM | TXD_GENBINDING)) {
        // only empty derivation allowed from generic type parameter
        if (!interfaces.empty()) {
            CERROR(definer, "Can't specialize a generic type parameter: " << baseType);
            return nullptr;
        }
    }

    // TODO: pass _mutable flag to type extensions

    return this->make_specialized_type( definer->get_declaration(), baseType, interfaces );
}



const TxType* TypeRegistry::get_interface_adapter(const TxType* interface, const TxType* adapted) {
    const TxActualType* interfaceType = interface->type();
    const TxActualType* adaptedType = adapted->type();
    auto modIf = interfaceType->is_modifiable();
    while (interfaceType->is_same_vtable_type())
        interfaceType = interfaceType->get_semantic_base_type();
    while (adaptedType->is_same_vtable_type())
        adaptedType = adaptedType->get_semantic_base_type();

    auto adapterType = this->package.builtin_types()->inner_get_interface_adapter( interfaceType, adaptedType );
//    ASSERT(adapterType->adapted_type()->get_type_id() == adaptedType->get_type_id(),
//           "Mismatching type ids between adapter and adaptee: " << adapterType->adapted_type()->get_type_id() << " != " << adaptedType->get_type_id());
    if (modIf)
        return this->get_modifiable_type(nullptr, adapterType);
    else
        return adapterType;
}



// FUTURE: maybe remove the declaration argument, disallowing 'user' type names for references?
const TxType* TypeRegistry::get_reference_type( const TxTypeDefiningNode* definer, const TxTypeTypeArgumentNode* targetTypeBinding,
                                                const TxIdentifier* dataspace ) {
    std::vector<const TxTypeArgumentNode*> bindings( { targetTypeBinding } );
    return this->get_type_specialization( definer, this->get_builtin_type( REFERENCE ), bindings );
}



const TxType* TypeRegistry::get_array_type( const TxTypeDefiningNode* definer, const TxTypeTypeArgumentNode* elemTypeBinding,
                                            const TxValueTypeArgumentNode* lengthBinding ) {
    std::vector<const TxTypeArgumentNode*> bindings( { elemTypeBinding, lengthBinding } );
    return this->get_type_specialization( definer, this->get_builtin_type( ARRAY ), bindings );
}

const TxType* TypeRegistry::get_array_type( const TxTypeDefiningNode* definer, const TxTypeTypeArgumentNode* elemTypeBinding ) {
    std::vector<const TxTypeArgumentNode*> bindings( { elemTypeBinding } );
    return this->get_type_specialization( definer, this->get_builtin_type( ARRAY ), bindings );
}



static std::vector<const TxActualType*> make_actual_arg_types( const std::vector<const TxType*>& argumentTypes ) {
    std::vector<const TxActualType*> actTypes;
    for (auto a : argumentTypes)
        actTypes.push_back(a->type());
    return actTypes;
}

const TxType* TypeRegistry::get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, const TxType* returnType,
                                              bool modifiableClosure) {
    auto funcType = new TxFunctionType( declaration, this->get_builtin_type( FUNCTION )->type(),
                                        make_actual_arg_types( argumentTypes ), returnType->type(), modifiableClosure );
    this->add_type(funcType);
    return new TxType( funcType );
}

const TxType* TypeRegistry::get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                              bool modifiableClosure) {
    auto funcType = new TxFunctionType( declaration, this->get_builtin_type( FUNCTION )->type(),
                                        make_actual_arg_types( argumentTypes ), nullptr, modifiableClosure );
    this->add_type(funcType);
    return new TxType( funcType );
}

const TxType* TypeRegistry::get_constructor_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                                 TxTypeDeclaration* objectTypeDecl) {
    auto type = new TxConstructorType( declaration, this->get_builtin_type( FUNCTION )->type(),
                                       make_actual_arg_types( argumentTypes ), objectTypeDecl );
    this->add_type(type);
    return new TxType( type );
}
