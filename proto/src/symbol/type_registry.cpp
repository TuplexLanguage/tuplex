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



Logger& TypeRegistry::_LOG = Logger::get("REGISTRY");


TypeRegistry::TypeRegistry(TxPackage& package)
        : _package(package) {
    this->createdTypes = new std::vector<TxActualType*>();
}


const TxLocation& TypeRegistry::get_builtin_location() const {
    return this->_package.builtins().get_builtin_location();
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
            try {
                type->type();
            }
            catch (const resolution_error& err) {
                LOG(this->LOGGER(), NOTE, "Caught resolution error resolving deferred type " << type << ": " << err);
            }
        }

        for ( ; specIx != this->enqueuedSpecializations.size(); specIx++) {
            //std::cerr << "Nof enqueued specializations: " << this->enqueuedSpecializations.size() << std::endl;
            auto specDecl = this->enqueuedSpecializations.at( specIx );
            LOG( this->LOGGER(), DEBUG, "Resolving enqueued specialization: " << specDecl );
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
////                this->LOGGER()->info("new specialization equal to preexisting one, reusing: %s", existingType->str().c_str());
////                std::cerr << "  new spec: " << newType << std::endl;
////                std::cerr << " prev spec: " << existingType << std::endl;
////                newType->rewire( existingType );
////                goto SKIP_ENQUEUED;
////            }
////        }
////        existingSpecializations.emplace( baseName, newType );
//
//        this->LOGGER()->info("Resolving enqueued specialization: %s", specDecl->str().c_str());
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
            if (( type->get_declaration()->get_decl_flags() & TXD_EXPERRBLOCK ))
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
    delete this->createdTypes;
    this->createdTypes = createdTypes;
}


const TxType* TypeRegistry::get_builtin_type(const BuiltinTypeId id, bool mod) {
    auto biType = this->_package.builtins().get_builtin_type( id );
    return ( mod ? this->get_modifiable_type( nullptr, biType ) : biType );
}



TxType* TypeRegistry::make_type_entity( const TxActualType* actualType ) {
    return new TxType( actualType );
}



TxActualType* TypeRegistry::make_actual_type( const TxTypeDeclaration* declaration,
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



//static const TxActualType* get_derivable_base_type(const TxActualType* baseType) {
//    // FIX ME: dead code, replace with assert
//    while (baseType->is_empty_derivation() && !baseType->get_explicit_declaration()) {
//        std::cerr << "#### Note - in specialization, bypassing empty base type " << baseType << std::endl;
//        baseType = baseType->get_base_type();
//    }
//    return baseType;
//}


const TxType* TypeRegistry::get_modifiable_type(const TxTypeDeclaration* declaration, const TxType* type) {
    auto actualType = type->type();
    ASSERT(!actualType->is_modifiable(), "Can't make a modifiable specialization of a modifiable type: " << actualType);
    ASSERT(!(actualType->is_empty_derivation() && !actualType->get_explicit_declaration()), "Can't derive from implicit empty base type: " << actualType);
    // 'modifiable' is always a distinct 'specialization' (no parameter bindings (or type extensions))

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
            LOG(this->LOGGER(), WARN, "Name collision when trying to declare implicit MOD type, preexisting symbol: " << entitySymbol);
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

    return new TxType( this->make_actual_type( declaration, actualType, {}, true ) );
}



const TxType* TypeRegistry::make_empty_derivation(const TxTypeDeclaration* declaration, const TxType* type) {
    ASSERT(declaration, "empty type specialization doesn't have declaration: " << type);
    return new TxType( declaration->get_definer(),
                       [ this, declaration, type ] () {
                           return this->make_actual_empty_derivation( declaration, type->type() );
                       } );
}

const TxActualType* TypeRegistry::make_actual_empty_derivation(const TxTypeDeclaration* declaration, const TxActualType* baseType) {
    ASSERT(!baseType->is_modifiable(), "Can't specialize a 'modifiable' base type: " << baseType);
    ASSERT(!(baseType->is_empty_derivation() && !baseType->get_explicit_declaration()), "Can't derive from implicit empty base type: " << baseType);

    return this->make_actual_type( declaration, baseType );
}



const TxType* TypeRegistry::make_type_derivation( const TxTypeExpressionNode* definer, const TxType* baseType,
                                                 const std::vector<const TxType*>& interfaces, bool _mutable ) {
    ASSERT(definer->get_declaration(), "type derivation doesn't have declaration: " << definer);
    return new TxType( definer,
                       [ this, definer, baseType, interfaces, _mutable ] () {
                           return this->make_actual_type_derivation( definer, baseType->type(), interfaces, _mutable );
                       } );
}

const TxActualType* TypeRegistry::make_actual_type_derivation( const TxTypeExpressionNode* definer, const TxActualType* baseType,
                                                               const std::vector<const TxType*>& interfaces, bool _mutable ) {
    ASSERT(!baseType->is_modifiable(), "Can't specialize a 'modifiable' base type: " << baseType);
    ASSERT(!(baseType->is_empty_derivation() && !baseType->get_explicit_declaration()), "Can't derive from implicit empty base type: " << baseType);

    if (baseType->get_declaration()->get_decl_flags() & (TXD_GENPARAM | TXD_GENBINDING)) {
        // only empty derivation allowed from generic type parameter
        if (!interfaces.empty())
            CERR_THROWRES(definer, "Can't specialize a generic type parameter: " << baseType);
    }

    // TODO: pass _mutable flag to type extensions

    return this->make_actual_type( definer->get_declaration(), baseType, interfaces );
}



const TxType* TypeRegistry::get_type_specialization( const TxTypeDefiningNode* definer, const TxType* baseType,
                                                     const std::vector<const TxTypeArgumentNode*>& bindings ) {
    ASSERT(!bindings.empty(), "Empty bindings list when specializing baseType: " << baseType);
    ASSERT(bindings.at(0)->is_context_set(), "context not set for binding " << bindings.at(0));
    return new TxType( definer,
                       [ this, definer, baseType, bindings ] () {
                           return this->get_actual_type_specialization( definer, baseType->type(), &bindings );
                       } );
}


const TxActualType* TypeRegistry::get_actual_type_specialization( const TxTypeDefiningNode* definer, const TxActualType* baseType,
                                                                  const std::vector<const TxTypeArgumentNode*>* bindings ) {
    // Note: type specialization is never applied to a modifiable-specialization (legal only on non-modifiable base type)
    ASSERT(!baseType->is_modifiable(), "Can't specialize a 'modifiable' base type: " << baseType);
    ASSERT(!(baseType->is_empty_derivation() && !baseType->get_explicit_declaration()), "Can't derive from implicit empty base type: " << baseType);

    const TxTypeDeclaration* declaration = nullptr;
    if (auto typeExprNode = dynamic_cast<const TxTypeExpressionNode*>(definer))
        declaration = typeExprNode->get_declaration();

    // Note: The same generic type specialization may be produced by multiple statements,
    //       both within ExpErr constructs and without. Therefore the type name must distinguish between them.
    // Note: Implicit types (without explicit declaration) produced by ExpErr statements are identified by presence of ExpErrCtx.
    bool isExpErrType = ( ( declaration && (declaration->get_decl_flags() & TXD_EXPERRBLOCK) )
                          || ( !declaration && definer->exp_err_ctx() ) );

    const TxActualType* specializedBaseType = get_inner_type_specialization( definer, baseType, bindings, isExpErrType );

    if (declaration) {
        // create empty type specialization with explicit (unique) name
        specializedBaseType = this->make_actual_type( declaration, specializedBaseType );
    }

    return specializedBaseType;
}



std::string encode_type_name( const TxTypeDeclaration* typeDecl ) {
    std::stringstream boundTypeName;
    boundTypeName << hashify( typeDecl->get_unique_full_name() );
    return boundTypeName.str();
}



static const TxActualType* matches_existing_type( TxEntitySymbol* existingBaseSymbol, const TxActualType* baseType,
                                                  const std::vector<const TxTypeArgumentNode*>* bindings ) {
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
            auto existingBaseTypeBindings = existingBaseType->get_bindings();

            bool matchOK = true;
            for ( unsigned ix = 0; ix < bindings->size(); ix++ ) {
                auto binding = bindings->at( ix );
                if (auto typeBinding = dynamic_cast<const TxTypeTypeArgumentNode*>( binding )) {
                    if (auto existingBindingTypeDecl = dynamic_cast<const TxTypeDeclaration*>( existingBaseTypeBindings.at( ix ) )) {
                        auto newBindingType = typeBinding->typeExprNode->get_type();
                        auto existingBindingType = existingBindingTypeDecl->get_definer()->resolve_type();
                        if (newBindingType == existingBindingType)
                            continue;
                    }
                }
                else {  // TxValueTypeArgumentNode
                    // (For now, statically constant VALUE specializations with diff. values don't share the same static type.)
                    auto valueBinding = static_cast<const TxValueTypeArgumentNode*>( binding );
                    if (auto existingFieldDecl = dynamic_cast<const TxFieldDeclaration*>( existingBaseTypeBindings.at( ix ) )) {
                        uint32_t newValue = 0;  // zero means dynamically specified value
                        uint32_t existingValue = 0;
                        if (auto newConstantValueProxy = valueBinding->valueExprNode->get_static_constant_proxy()) {
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
                LOG_DEBUG(existingBaseSymbol->get_root_scope()->registry().LOGGER(), "new specialization equal to preexisting one, reusing: " << existingBaseType);
                return existingBaseType;
            }
        }
        //LOG(existingBaseSymbol->get_root_scope()->registry().LOGGER(), INFO, "Found existing but mismatching type with sought name: " << existingBaseType);
    }

    return nullptr;
}


static const TxActualType* get_existing_type( const TxActualType* baseType, const std::vector<const TxTypeArgumentNode*>* bindings,
                                              TxScopeSymbol* baseScope, const std::string& newBaseName ) {
    auto baseTypeParams = baseType->type_params();
    if (bindings->size() == baseTypeParams.size()) {
        // if generic type specialization is equivalent to the generic base type, reuse it:
        bool matchOK = true;
        for ( unsigned ix = 0; ix < bindings->size(); ix++ ) {
            auto binding = bindings->at( ix );
            auto paramDecl = baseTypeParams.at( ix );
            if (auto typeBinding = dynamic_cast<const TxTypeTypeArgumentNode*>( binding )) {
                auto typeParamDecl = static_cast<const TxTypeDeclaration*>(paramDecl);
                auto constraintType = typeParamDecl->get_definer()->get_type();
                auto bindingType = typeBinding->typeExprNode->get_type();
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
            }
            else {  // TxValueTypeArgumentNode
                // VALUE parameters don't have "defaults"
            }
            matchOK = false;
            break;
        }
        if (matchOK) {
            LOG_DEBUG( baseScope->get_root_scope()->registry().LOGGER(), "new specialization equal to the generic base type, reusing: " << baseType);
            return baseType;
        }
    }

    // if name already exists and specialization is equal, reuse it:
    std::string upperBound = newBaseName;
    upperBound[upperBound.size()-1] += 1;
    for (auto existingBaseNameI = baseScope->alpha_order_names_lower( newBaseName );
             existingBaseNameI != baseScope->alpha_order_names_upper( upperBound ); existingBaseNameI++ ) {
        if (auto existingBaseSymbol = dynamic_cast<TxEntitySymbol*>( baseScope->get_member_symbol( *existingBaseNameI ) )) {
            if (auto matchingType = matches_existing_type( existingBaseSymbol, baseType, bindings ))
                return matchingType;
        }
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



const TxActualType* TypeRegistry::get_inner_type_specialization( const TxTypeDefiningNode* definer, const TxActualType* baseType,
                                                                 const std::vector<const TxTypeArgumentNode*>* bindings,
                                                                 bool isExpErrType ) {
    // Note: A non-parameterized type (without any declared type parameters) is not necessarily non-generic:
    //       It may have members that refer to generic parameters declared in an outer scope.
    // Note: Binding of ref-constrained type parameters doesn't necessarily affect code generation,
    //       but accesses to the members still need the specialized version of them in semantic analysis (e.g. the correct ref-target).

    auto baseDecl = baseType->get_declaration();
    if (baseDecl->get_decl_flags() & ( TXD_GENPARAM | TXD_GENBINDING )) {
        // only empty derivation allowed from generic type parameter
        CERR_THROWRES(definer, "Can't specialize a generic type parameter: " << baseType);
    }

    // re-base the new type on new non-generic specialization of the base type:
    // (this replaces the type parameter bindings with direct declarations within the new type)
    LOG_DEBUG(this->LOGGER(), "Re-basing the new type of [ " << definer << " ] by specializing the generic base type " << baseType);

    std::stringstream newBaseTypeName;
    if (isExpErrType)
        newBaseTypeName << "$EE$";
    newBaseTypeName << baseDecl->get_unique_name() << "<";

    // do shallow validation that bindings match base type's parameters:
    auto baseTypeParams = baseType->type_params();
    if (baseTypeParams.size() < bindings->size()) {
        CERR_THROWRES(definer, "Too many type arguments specifified for generic base type " << baseType);
    }

    for ( unsigned ix = 0; ix < bindings->size(); ix++ ) {
        auto binding = bindings->at( ix );
        auto paramDecl = baseTypeParams.at( ix );
        if (ix)  newBaseTypeName << ",";
        ASSERT(binding->is_context_set(), "Binding must have run declaration pass before being used in type specialization: " << binding);
        paramDecl->get_definer()->resolve_type();  // ensure param is resolved (and verify that it does resolve)

        if (auto typeArg = dynamic_cast<const TxTypeTypeArgumentNode*>( binding )) {
            if (! dynamic_cast<const TxTypeDeclaration*>(paramDecl))
                CERR_THROWRES(binding, "Can't bind a VALUE base type parameter using a TYPE: " << typeArg->typeExprNode->resolve_type());

            typeArg->typeExprNode->resolve_type();  // ensure binding is resolved (and verify that it does resolve)
            newBaseTypeName << "$";
        }
        else {  // binding is TxValueTypeArgumentNode
            if (! dynamic_cast<const TxFieldDeclaration*>(paramDecl))
                CERR_THROWRES(binding, "Can't bind a VALUE base type parameter using a TYPE: " << typeArg->typeExprNode->resolve_type());

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
        }
    }
    newBaseTypeName << ">";
    auto newBaseTypeNameStr = newBaseTypeName.str();

    // if equivalent specialized type already exists then reuse it, otherwise create new one:
    auto baseScope = baseDecl->get_symbol()->get_outer();
    const TxActualType* specializedBaseType = get_existing_type(baseType, bindings, baseScope, newBaseTypeNameStr);
    if (! specializedBaseType) {
        specializedBaseType = make_type_specialization( definer, baseType, bindings, isExpErrType, newBaseTypeNameStr );
    }
    return specializedBaseType;
}

const TxActualType* TypeRegistry::make_type_specialization( const TxTypeDefiningNode* definer, const TxActualType* baseType,
                                                            const std::vector<const TxTypeArgumentNode*>* bindings,
                                                            bool isExpErrType, const std::string& newBaseTypeNameStr ) {
    auto baseDecl = baseType->get_declaration();
    auto baseScope = baseDecl->get_symbol()->get_outer();
    auto baseTypeParams = baseType->type_params();

    TxDeclarationFlags newDeclFlags;

    if (isExpErrType)
        newDeclFlags = ( baseDecl->get_decl_flags() & DECL_FLAG_FILTER ) | TXD_IMPLICIT | TXD_EXPERRBLOCK;
    else
        newDeclFlags = ( baseDecl->get_decl_flags() & DECL_FLAG_FILTER ) | TXD_IMPLICIT;

    // create binding declaration nodes:
    auto bindingDeclNodes = new std::vector<TxDeclarationNode*>();
    for ( unsigned ix = 0; ix < bindings->size(); ix++ ) {
        auto binding = bindings->at( ix );
        auto paramDecl = baseTypeParams.at( ix );
        auto paramName = paramDecl->get_unique_name();

        if (auto typeArg = dynamic_cast<const TxTypeTypeArgumentNode*>( binding )) {
            TxTypeDefiningNode* btypeDefNode = typeArg->typeExprNode;
            TxTypeExpressionNode* btypeExprNode = new TxTypeExprWrapperNode( btypeDefNode );
            bindingDeclNodes->push_back( new TxTypeDeclNode( typeArg->get_parse_location(), TXD_GENBINDING | TXD_PUBLIC, paramName, nullptr,
                                                             btypeExprNode ) );
            LOG_TRACE(this->LOGGER(), "Re-bound base type " << baseDecl->get_unique_full_name() << " parameter '" << paramName << "' with " << typeArg->typeExprNode);
        }
        else {
            auto valueArg = static_cast<const TxValueTypeArgumentNode*>( binding );
            bindingDeclNodes->push_back( make_value_type_param_decl_node( valueArg->get_parse_location(), paramName,
                                                                          TXD_GENBINDING, paramDecl, valueArg->valueExprNode ) );
            LOG_TRACE(this->LOGGER(), "Re-bound base type " << baseDecl->get_unique_full_name() << " parameter '" << paramName << "' with " << valueArg->valueExprNode);
        }
    }


    // If any parameter is not bound, the parameter is redeclared (inherited) as still-unbound type parameter:
    // (Type args are always specified from left to right, so unbound params will be at end of param list.)
    for (auto unboundParamI = baseTypeParams.cbegin() + bindings->size();
            unboundParamI != baseTypeParams.cend(); unboundParamI++ ) {
        auto unboundParamDecl = *unboundParamI;
        LOG_DEBUG(this->LOGGER(), "Implicitly inheriting (redeclaring) type parameters " << unboundParamDecl->get_unique_full_name() << " in type " << newBaseTypeNameStr);
        if (auto typeDecl = dynamic_cast<const TxTypeDeclaration*>(unboundParamDecl)) {
            bindingDeclNodes->push_back( make_type_type_param_decl_node( definer->get_parse_location(), typeDecl->get_unique_name(),
                                                                         TXD_GENPARAM, typeDecl ) );
        }
        else {
            auto fieldDecl = static_cast<const TxFieldDeclaration*>(unboundParamDecl);
            bindingDeclNodes->push_back( make_value_type_param_decl_node( definer->get_parse_location(), fieldDecl->get_unique_name(),
                                                                          TXD_GENPARAM, fieldDecl ) );
        }
    }

    {   // pass on the generic base type to the new specialization via member named $GenericBase:
        auto baseTypeExpr = new TxTypeDeclWrapperNode( definer->get_parse_location(), baseDecl );
        auto declNode = new TxTypeDeclNode( definer->get_parse_location(), TXD_PUBLIC | TXD_IMPLICIT, "$GenericBase", nullptr, baseTypeExpr );
        bindingDeclNodes->push_back( declNode );
    }

    // process new specialization of the base type:
    //std::cerr << "specializing base " << newBaseTypeNameStr << ": " << baseType << std::endl;
    ASSERT(dynamic_cast<TxTypeExpressionNode*>( baseDecl->get_definer() ), "baseType's definer is not a TxTypeExpressionNode: " << baseDecl->get_definer());
    ASSERT(baseDecl->get_definer()->context().scope() == baseScope, "Unexpected lexical scope: " << baseDecl->get_definer()->context().scope() << " != " << baseScope);
    auto baseTypeExpr = static_cast<TxTypeExpressionNode*>( baseDecl->get_definer() );
    auto newBaseTypeExpr = baseTypeExpr->make_ast_copy();
    auto uniqueBaseTypeNameStr = baseScope->make_unique_name(newBaseTypeNameStr);
    auto newBaseTypeDecl = new TxTypeDeclNode( definer->get_parse_location(), newDeclFlags, uniqueBaseTypeNameStr, bindingDeclNodes, newBaseTypeExpr );

    LexicalContext newBaseContext = LexicalContext( baseScope, definer->exp_err_ctx(), true );
    newBaseTypeDecl->symbol_declaration_pass( newBaseContext, newBaseContext );
    const TxActualType* specializedBaseType = newBaseTypeExpr->resolve_type()->type();
    LOG_DEBUG(this->LOGGER(), "Created new specialized type " << specializedBaseType);

    // Invoking the resolution pass here can cause infinite recursion
    // (since the same source text construct may be recursively reprocessed,
    //  and the bindings may refer to this type's declaration),
    // so we enqueue this "specialization resolution pass" for later processing.
    //std::cerr << "enqueuing specialization " << newBaseTypeDecl << std::endl;
    this->enqueuedSpecializations.emplace_back( newBaseTypeDecl );

    return specializedBaseType;
}




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
                                        make_actual_arg_types( argumentTypes ), modifiableClosure );
    this->add_type(funcType);
    return new TxType( funcType );
}

const TxType* TypeRegistry::get_constructor_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                                 const TxTypeDeclaration* objectTypeDecl) {
    auto type = new TxConstructorType( declaration, this->get_builtin_type( FUNCTION )->type(),
                                       make_actual_arg_types( argumentTypes ), objectTypeDecl );
    this->add_type(type);
    return new TxType( type );
}




/*----- interface adapter -----*/

class TxAdapterTypeNode final : public TxTypeExpressionNode {
    const TxActualType* interfaceType;
    const TxActualType* adaptedType;

protected:
    virtual void symbol_declaration_pass_descendants( LexicalContext& defContext, LexicalContext& lexContext ) override { }

    virtual const TxType* define_type() override {
        auto adapterActType = new TxInterfaceAdapterType( this->get_declaration(), interfaceType, adaptedType );
        this->registry().add_type( adapterActType );
        return this->registry().make_type_entity( adapterActType );
    }

public:
    TxAdapterTypeNode( const TxLocation& parseLocation, const TxActualType* interfaceType, const TxActualType* adaptedType )
            : TxTypeExpressionNode(parseLocation), interfaceType(interfaceType), adaptedType(adaptedType) { }

    /** Creates a copy of this node and all its descendants for purpose of generic specialization. */
    virtual TxAdapterTypeNode* make_ast_copy() const override {
        ASSERT(false, "unexpected reinterpretation of " << this);
        return nullptr; //new TxAdapterTypeNode( this );
    }

    virtual std::string get_auto_type_name() const override {
        return (this->get_declaration() ? this->get_declaration()->get_unique_full_name() : "");
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override { return nullptr; }

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override { }
};



const TxType* TypeRegistry::get_interface_adapter(const TxType* interface, const TxType* adapted) {
    // for now we don't defer it, we create it right away
    auto adapterType = this->get_actual_interface_adapter( interface->type(), adapted->type() );
    return adapterType;
}


const TxType* TypeRegistry::get_actual_interface_adapter(const TxActualType* interfaceType, const TxActualType* adaptedType) {
    auto modIf = interfaceType->is_modifiable();

    while (interfaceType->is_same_vtable_type())
        interfaceType = interfaceType->get_semantic_base_type();
    while (adaptedType->is_same_vtable_type())
        adaptedType = adaptedType->get_semantic_base_type();

    ASSERT(! interfaceType->is_modifiable(), "Shouldn't create adapter for 'modifiable' interface type: " << interfaceType);
    ASSERT(!   adaptedType->is_modifiable(), "Shouldn't create adapter for 'modifiable' adaptee type: "   << adaptedType);
    ASSERT(*interfaceType != *adaptedType,   "Shouldn't create adapter between equivalent types");
    ASSERT(!(adaptedType->is_empty_derivation() && !adaptedType->get_explicit_declaration()), "Can't derive from implicit empty base type: " << adaptedType);

    auto ifDecl = interfaceType->get_declaration();
    auto scope = ifDecl->get_symbol()->get_outer();
    std::string adapterName = ifDecl->get_unique_name() + "$if$" + encode_type_name( adaptedType->get_declaration() );

    if (auto existingAdapterSymbol = dynamic_cast<TxEntitySymbol*>(scope->get_member_symbol(adapterName))) {
        if (auto typeDecl = existingAdapterSymbol->get_type_decl()) {
            auto adapterType = typeDecl->get_definer()->resolve_type();
            //std::cerr << "Getting existing interface adapter: " << adapterType << std::endl;
            return adapterType;
        }
    }

    LOG(this->LOGGER(), INFO, "Creating interface adapter: " << adapterName << "\n\tfrom " << adaptedType << "\n\tto   " << interfaceType);
    // TODO: combine flags from adapted and adaptee types, including TXD_EXPERRBLOCK

    auto & loc = this->get_builtin_location();
    auto adapterTypeNode = new TxAdapterTypeNode( loc, interfaceType, adaptedType );
    auto adapterDeclNode = new TxTypeDeclNode( loc, (TXD_PUBLIC | TXD_IMPLICIT), adapterName, nullptr, adapterTypeNode );

    adapterDeclNode->symbol_declaration_pass( ifDecl->get_definer()->context() );
    {   // override the adaptee type id virtual field member:
        TxDeclarationFlags fieldDeclFlags = TXD_PUBLIC | TXD_STATIC | TXD_OVERRIDE | TXD_IMPLICIT;
        auto fieldDecl = new TxFieldDeclNode( loc, fieldDeclFlags,
                                              new TxFieldDefNode( loc, "$adTypeId", new TxIdentifiedTypeNode( loc, "tx.UInt" ), nullptr ) );
        auto ctx = LexicalContext( adapterDeclNode->get_declaration()->get_symbol(), nullptr, false );
        fieldDecl->symbol_declaration_pass( ctx, false);
        fieldDecl->symbol_resolution_pass();
    }
    adapterDeclNode->symbol_resolution_pass();

    auto adapterType = adapterTypeNode->resolve_type();
//    ASSERT(adapterType->adapted_type()->get_type_id() == adaptedType->get_type_id(),
//           "Mismatching type ids between adapter and adaptee: " << adapterType->adapted_type()->get_type_id() << " != " << adaptedType->get_type_id());
    if (modIf)
        return this->get_modifiable_type(nullptr, adapterType);
    else
        return adapterType;
}
