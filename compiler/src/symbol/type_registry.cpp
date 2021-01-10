#include "type_registry.hpp"

#include "package.hpp"
#include "entity.hpp"

#include "util/util.hpp"
#include "util/assert.hpp"

#include "ast/ast_entitydecls.hpp"
#include "ast/ast_declpass.hpp"
#include "ast/ast_wrappers.hpp"
#include "ast/type/ast_types.hpp"
#include "ast/expr/ast_constexpr.hpp"

#include "builtin/builtin_types.hpp"

#include "tx_logging.hpp"
#include "tx_error.hpp"

#include "llvm_generator.hpp"
#include "driver.hpp"


#define VALUE_SPECS_SHARE_CODE


/** the flags that may be inherited when specializing a type */
static const TxDeclarationFlags DECL_FLAG_FILTER = TXD_STATIC | TXD_PUBLIC | TXD_PROTECTED | TXD_ABSTRACT | TXD_FINAL | TXD_IMPLICIT
                                                   | TXD_EXPERROR;

Logger& TypeRegistry::REGLOGGER = Logger::get( "REGISTRY" );

TypeRegistry::TypeRegistry( TxPackage& package )
        : _package( package ) {
}

void TypeRegistry::add_reinterpretation( TxTypeDeclNode* node ) {
    this->enqueuedSpecializations.push_back( node );
}

void TypeRegistry::add_type( TxActualType* type ) {
    ASSERT( !this->startedPreparingTypes, "Can't create new types when type preparation phase has started: " << type );
    if ( type->runtimeTypeId < BuiltinTypeId_COUNT ) {
        ASSERT( type->runtimeTypeId == this->runtimeTypes.size(), "adding built-in type in wrong order: type id="
                << type->runtimeTypeId << "; staticTypes.size()=" << this->runtimeTypes.size() );
        this->runtimeTypes.push_back( type );
    }
    this->createdTypes.push_back( type );
}

unsigned TypeRegistry::get_unintegrated_type_count() const {
    return this->createdTypes.size() - this->integratedTypesEndIx;
}

void TypeRegistry::integrate_types( bool expectOnlyRefs ) {
    LOG_DEBUG( this->LOGGER(), "Starting integrating types, created types count=" << this->createdTypes.size() );
    for ( auto ix = this->integratedTypesEndIx; ix < this->createdTypes.size(); ix++ ) {
        auto type = this->createdTypes.at( ix );
        try {
            type->integrate();
            if ( expectOnlyRefs && type->get_type_class() != TXTC_REFERENCE )
                LOG( this->LOGGER(), WARN, "Integrating non-ref type: " << type << " from " << type->get_declaration()->get_definer() );
        }
        catch ( const resolution_error& err ) {
            if ( type->get_declaration()->get_decl_flags() & TXD_EXPERROR )
                LOG_DEBUG( this->LOGGER(), "Caught resolution error integrating ExpErr-type " << type << ": " << err );
            else {
                CERROR( type, "Caught resolution error integrating type " << type << ": " << err );
            }
        }
    }
    this->integratedTypesEndIx = this->createdTypes.size();
}

void TypeRegistry::prepare_types() {
    this->startedPreparingTypes = true;

    /* The total set of types is subdivided into categories based on their runtime representation,
     * which also drives their Id order:
     * built-in types < data types < vtable types < runtime types < all types
     *
     * The runtime types are all the valid types that will hence exist in runtime.
     * This excludes expected-error types and implicit type aliases.
     *
     * The vtable types are all runtime types that have a static vtable-type-definition.
     * This excludes references, functions, and generic parameters (which are explicit aliases).
     *
     * The data types are all vtable types that can be instantiated, i.e. are concrete.
     * This excludes abstract types, interfaces, and generic types.
     *
     * The built-in types are special in that they are all considered to be data types,
     * even though some of them are abstract/generic.
     *
     * (Note that not all vtable types will have their own runtime vtable;
     * each distinct vtable is only generated once and referenced by all equivalent descendant types.)
     */
    std::vector<TxActualType*> dataTypes;
    std::vector<TxActualType*> vtableTypes;
    std::vector<TxActualType*> concreteRefTypes;
    std::vector<TxActualType*> concreteFuncTypes;
    std::vector<TxActualType*> validTypes;

    for ( auto type : this->createdTypes ) {
        try {
            type->prepare_members();
        }
        catch ( const resolution_error& err ) {
            // if this happens, investigate why it wasn't caught before this type was added to the types list
            LOG( this->LOGGER(), INFO, "Caught resolution error preparing members of type " << type << ": " << err );
            continue;
        }

        if ( type->runtimeTypeId < BuiltinTypeId_COUNT ) {
            // the built-in types are already added to runtimeTypes
            if ( type->runtimeTypeId == TXBT_ARRAY )
                type->suppressCodeGen = true;
            continue;
        }

        if ( type->get_declaration()->get_definer()->exp_err_ctx() ) {
            LOG_DEBUG( this->LOGGER(), "Not registering type with ExpErr context as runtime type: " << type);
            continue;
        }
        if ( type->get_declaration()->get_decl_flags() & TXD_EXPERROR ) {
            // there shouldn't be a TXD_EXPERRBLOCK declaration without exp-err-ctx set unless it is declared within a reinterpreted construct:
            ASSERT( type->get_declaration()->get_definer()->context().is_reinterpretation(),
                    "Unexpected TXD_EXPERRBLOCK flag in non-reinterpreted type "<< type );
            LOG_DEBUG( this->LOGGER(), "Not registering type with ExpErr flag as runtime type: " << type );
            continue;
        }

        if ( type->is_empty_derivation() ) {
            // (Note: Self and Super are aliases, not proper types and shouldn't occur here.)
            ASSERT( !(type->get_declaration()->get_decl_flags() & TXD_IMPLICIT), "Implicit empty derivation: " << type );
            if ( type->get_declaration()->get_decl_flags() & TXD_GENPARAM ) {
                validTypes.push_back( type );
                continue;
            }
        }

        if ( type->get_type_class() == TXTC_FUNCTION ) {
            concreteFuncTypes.push_back( type );
            continue;
        }

        if ( type->get_type_class() == TXTC_REFERENCE ) {
            concreteRefTypes.push_back( type );
            continue;
        }

        // Notes:
        //  - Not including full-sized runtime type information about equivalent types is a potential footprint optimization,
        //    but also leads to problems.
        //  - Pure value specializations need distinct runtime type information
        //    (though they don't necessitate distinct code generation and vtable).

        if ( type->is_type_generic_dependent() ) {
            // Note that this skips codegen for the entire AST of all type-generic-dependent types,
            // which means none of their members are generated, including any statically declared inner/local types.
            // FUTURE: Evaluate capability for generic types to have global static members
            //         (e.g. inner types independent of the outer type parameters).
            type->suppressCodeGen = true;
            vtableTypes.push_back( type );
            continue;
        }
#ifdef VALUE_SPECS_SHARE_CODE
        else if ( type->is_value_generic_dependent() ) {
            // Note: There are not "concrete" since size may be unknown, but they can be code-generated
            dataTypes.push_back( type );
            continue;
        }
        else if ( type->is_pure_value_specialization() ) {
            // If type is a value specialization, don't code-generate it, but create RTTI if concrete
            type->suppressCodeGen = true;
        }
#else
        else if ( type->is_value_generic_dependent() ) {
            vtableTypes.push_back( type );
            continue;
        }
#endif

        if ( type->is_abstract() ) {  // includes interfaces
            vtableTypes.push_back( type );
            continue;
        }

        ASSERT( type->is_concrete(), "non-concrete data type: " << type );
        dataTypes.push_back( type );
    }

    // (built-in types have already been added to runtime types)
    this->builtinTypesCount = this->runtimeTypes.size();
    for ( auto type : dataTypes ) {
        type->runtimeTypeId = this->runtimeTypes.size();
        this->runtimeTypes.push_back( type );
    }
    this->dataTypesCount = this->runtimeTypes.size();
    for ( auto type : vtableTypes ) {
        type->runtimeTypeId = this->runtimeTypes.size();
        this->runtimeTypes.push_back( type );
    }
    this->vtableTypesCount = this->runtimeTypes.size();
    for ( auto type : concreteRefTypes ) {
        type->runtimeTypeId = this->runtimeTypes.size();
        this->runtimeTypes.push_back( type );
    }
    this->refTypesLimit = this->runtimeTypes.size();
    for ( auto type : concreteFuncTypes ) {
        type->runtimeTypeId = this->runtimeTypes.size();
        this->runtimeTypes.push_back( type );
    }
    this->funcTypesLimit = this->runtimeTypes.size();
    for ( auto type : validTypes ) {
        type->runtimeTypeId = this->runtimeTypes.size();
        this->runtimeTypes.push_back( type );
    }

    LOG_INFO( this->LOGGER(), "Number of data types: " << this->data_types_count()
              << "   Number of vtable types: " << this->vtable_types_count()
              << "   Number of runtime types: " << this->runtime_types_count() );
}

static void print_type( const TxActualType* type ) {
    std::string stat;
    if ( type->is_static() )
        stat = "stat-concr";
    else if ( type->is_dynamic() )
        stat = "dyn-concr";
    else if ( type->is_type_generic() )
        stat = "type-gen";
    else if ( type->is_value_generic() )
        stat = "value-gen";
    else if ( !type->is_same_vtable_type() )
        stat = "abstr/vtab";  // abstract, but with distinct vtable type
    printf( "%4d  %s  %-10s  %10s  %c  %s\n",
            type->get_runtime_type_id(),
            ::to_string( type->get_declaration()->get_decl_flags() ).c_str(),
            to_string( type->get_type_class() ).c_str(),
            stat.c_str(),
            ( type->suppress_code_gen() ? 'S' : ' ' ),
            type->str( false ).c_str() );
}

void TypeRegistry::dump_types() const {
    auto typeI = this->runtime_types_cbegin();
    std::cout << "For types marked S, code generation is suppressed." << std::endl;
    std::cout << "runtime types > vtable types > data types > built-in types:" << std::endl;
    for ( ; typeI != this->builtin_types_cend(); typeI++ ) {
        print_type( *typeI );
    }
    std::cout << "runtime types > vtable types > data types:" << std::endl;
    for ( ; typeI != this->data_types_cend(); typeI++ ) {
        print_type( *typeI );
    }
    std::cout << "runtime types > vtable types:" << std::endl;
    for ( ; typeI != this->vtable_types_cend(); typeI++ ) {
        print_type( *typeI );
    }
    std::cout << "runtime types:" << std::endl;
    for ( ; typeI != this->runtime_types_cend(); typeI++ ) {
        print_type( *typeI );
    }
}

const TxActualType* TypeRegistry::get_builtin_type( const BuiltinTypeId id ) {
    return this->_package.driver().builtins().get_builtin_type( id );
}

/** Makes a TYPE type parameter declaration node (both for bindings and param redeclarations).
 * Note, since this creates a wrapper around an existing declaration, it creates a type alias. */
static TxDeclarationNode* make_type_type_param_decl_node( const TxLocation& parseLoc, const std::string& paramName,
                                                          const TxTypeDeclaration* typeDecl ) {
    auto typeExpr = new TxDerivedTypeNode( parseLoc, new TxTypeDeclWrapperNode( parseLoc, typeDecl ) );
    auto declNode = new TxTypeDeclNode( parseLoc, TXD_GENPARAM | TXD_IMPLICIT | TXD_PUBLIC,
                                        new TxIdentifierNode( parseLoc, paramName ), nullptr, typeExpr );
    return declNode;
}

/** Makes a VALUE type parameter declaration node (both for bindings and param redeclarations). */
static TxDeclarationNode* make_value_type_param_decl_node( const TxLocation& parseLoc, const std::string& paramName, TxDeclarationFlags flags,
                                                           const TxEntityDeclaration* paramValueTypeDecl,
                                                           TxExpressionNode* valueDefiner = nullptr ) {
    if (valueDefiner)
        valueDefiner = new TxExprWrapperNode( valueDefiner );
    auto paramTypeNode = new TxTypeDeclWrapperNode( parseLoc, paramValueTypeDecl );
    auto fieldDef = new TxNonLocalFieldDefNode( parseLoc, new TxIdentifierNode( parseLoc, paramName ), paramTypeNode, valueDefiner );
    auto declNode = new TxFieldDeclNode( parseLoc, flags | TXD_PUBLIC, fieldDef );
    return declNode;
}

static std::string trim_base_type_name( const std::string& baseName ) {
    auto startPos = ( begins_with( baseName, "M$" ) ? 2 : 0 );
    auto endPos = baseName.find_last_of( '>' );
    if ( endPos == std::string::npos )
        return std::string( baseName, startPos ) + '<';
    else
        return std::string( baseName, startPos, endPos-startPos ) + ',';
}

std::string encode_type_name( const TxTypeDeclaration* typeDecl ) {
    std::stringstream boundTypeName;
    boundTypeName << hashify( typeDecl->get_unique_full_name() );
    return boundTypeName.str();
}


/////////////////////////////////////////////////////////////

static const TxActualType* matches_existing_specialization( const TxActualType* genBaseType,
                                                            const TxActualType* existingSpecType,
                                                            const std::vector<const TxTypeArgumentNode*>& bindings ) {
    auto & existingSpecBindings = existingSpecType->get_bindings();
    bool matchOK = true;
    // (Note: We know that the existing specialization and the new candidate have equal number of bindings.)
    for ( unsigned ix = 0; ix < bindings.size(); ix++ ) {
        auto binding = bindings.at( ix );
        if ( !binding->is_value() ) {
            if ( auto existingBindingTypeDecl = dynamic_cast<const TxTypeDeclaration*>( existingSpecBindings.at( ix ) ) ) {
                const TxQualType newBindingQT = binding->type_expr_node()->qtype();
                const TxQualType existingBindingQT = existingBindingTypeDecl->get_definer()->qtype();
                if ( newBindingQT == existingBindingQT )
                    continue;
            }
        }
        else {  // TxTypeArgumentNode
            // Statically constant VALUE specializations with distinct values are distinct types.
            // Dynamic VALUE specializations with distinct value expressions are distinct (we presume inequality in this implementation).
            if ( auto existingFieldDecl = dynamic_cast<const TxFieldDeclaration*>( existingSpecBindings.at( ix ) ) ) {
                // to match, both need to be statically constant and with equal value
                if ( auto existingInitializer = existingFieldDecl->get_definer()->get_init_expression() ) {
                    if ( binding->value_expr_node()->is_statically_constant() && existingInitializer->is_statically_constant() ) {
                        auto actType = binding->value_expr_node()->qtype().type();
                        if ( actType->has_runtime_type_id() && is_concrete_uinteger_type( actType ) )
                            if ( eval_unsigned_int_constant( binding->value_expr_node() ) == eval_unsigned_int_constant( existingInitializer ) )
                                continue;
                    }
//                    if ( is_static_equal( valueBinding->valueExprNode, existingInitializer ) )
//                        continue;
                }
            }
        }
        matchOK = false;
        break;
    }
    if ( matchOK ) {
        LOG_DEBUG( existingSpecType->get_declaration()->get_symbol()->get_root_scope()->registry().LOGGER(),
                   "new specialization equal to preexisting one, reusing: " << existingSpecType );
        return existingSpecType;
    }
    return nullptr;
}

static const TxActualType* get_existing_type( const TxActualType* genBaseType,
                                              const std::vector<const TxTypeArgumentNode*>& bindings,
                                              TxScopeSymbol* baseScope, const std::string& newBaseName,
                                              bool expError, bool mutableType ) {
    auto & baseTypeParams = genBaseType->get_type_params();
    if ( bindings.size() <= baseTypeParams.size() ) {
        // if generic type specialization is equivalent to the generic base type, reuse it:
        bool matchOK = true;
        for ( unsigned ix = 0; ix < bindings.size(); ix++ ) {
            auto binding = bindings.at( ix );
            auto paramDecl = baseTypeParams.at( ix );
            if ( !binding->is_value() ) {
                auto typeParamDecl = static_cast<const TxTypeDeclaration*>( paramDecl );
                const TxQualType constraintQType( typeParamDecl->get_definer()->qtype() );
                const TxQualType bindingQType = binding->type_expr_node()->qtype();
                if ( bindingQType->get_declaration()->get_decl_flags() & TXD_GENPARAM ) {
                    auto bindingDecl = bindingQType->get_declaration();
                    if ( bindingDecl == constraintQType->get_declaration() ) {
                        //std::cerr << "binding refers to 'itself' (its parameter declaration): " << bindingDecl << std::endl;
                        continue;  // binding refers to "itself" (its parameter declaration)
                    }
                    // else binding is to a different type parameter and is effectively an explicit unique type
                }
                else if ( constraintQType == bindingQType )
                    continue;
            }
            else {  // TxTypeArgumentNode
                // VALUE parameters don't have "defaults"
            }
            matchOK = false;
            break;
        }
        if ( matchOK ) {
            LOG_DEBUG( baseScope->get_root_scope()->registry().LOGGER(),
                       "new specialization equal to the generic base type, reusing: " << genBaseType );
            return genBaseType;
        }
    }

    // if name already exists and specialization is equal, reuse it:
    for ( auto existingSpecDeclI = genBaseType->get_declaration()->get_symbol()->type_spec_cbegin();
            existingSpecDeclI != genBaseType->get_declaration()->get_symbol()->type_spec_cend();
            existingSpecDeclI++ ) {
        auto existingType = (*existingSpecDeclI)->get_definer()->qtype().type();
        // Note, not guaranteed to know type class here (since not guaranteed to be integrated).
        // Interfaces are implicitly mutable.
        if ( ( existingType->is_mutable() || !mutableType )
            && bool( existingType->get_decl_flags() & TXD_EXPERROR ) == expError
            && existingType->get_bindings().size() == bindings.size())
            if ( auto matchingType = matches_existing_specialization( genBaseType, existingType, bindings ))
                return matchingType;
    }
    return nullptr;
}

const TxActualType* TypeRegistry::get_inner_type_specialization( const TxTypeResolvingNode* definer, const TxActualType* genBaseType,
                                                           const std::vector<const TxTypeArgumentNode*>& bindings, bool mutableType ) {
    // Note: A non-parameterized type (without any declared type parameters) is not necessarily non-generic:
    //       It may have members that refer to generic parameters declared in an outer scope.
    // Note: Binding of ref-constrained type parameters doesn't necessarily affect code generation,
    //       but accesses to the members still need the specialized version of them in semantic analysis (e.g. the correct ref-target).

    auto baseDecl = genBaseType->get_declaration();

    // Note: The same generic type specialization may be produced by multiple statements,
    //       both within ExpErr constructs and without, so we must distinguish between them.
    // If either the generic type or its specialization site is defined within an exp-err-context,
    // the specialization inherits that exp-err-context:
    ExpectedErrorClause* expErrCtx = definer->exp_err_ctx();
    if ( !expErrCtx )
        expErrCtx = baseDecl->get_definer()->exp_err_ctx();

    std::stringstream typeSpecTypeName;
    if ( expErrCtx ) {
        //if ( genBaseType->get_type_class() != TXTC_REFERENCE )
            typeSpecTypeName << "$EE$";
    }
    typeSpecTypeName << trim_base_type_name( baseDecl->get_unique_name() );
    std::stringstream valueSpecTypeName;
    valueSpecTypeName << typeSpecTypeName.str();

    std::vector<const TxTypeArgumentNode*> typeBindings;
    std::vector<const TxTypeArgumentNode*> valueBindings;

    if ( genBaseType->get_type_params().size() < bindings.size() )
        CERR_THROWRES( definer, "Too many type arguments (" << bindings.size() << ") for generic type " << genBaseType );

    // sort TYPE and VALUE bindings; create specialization name
    for ( unsigned ix = 0; ix < bindings.size(); ix++ ) {
        auto binding = bindings.at( ix );
        ASSERT( binding->is_context_set(), "Binding must have run declaration pass before being used in type specialization: " << binding );
        // Note: Base type's parameters cannot be resolved at this point, since that can cause infinite recursion.

        if ( !binding->is_value() ) {
            // ensure binding is resolved (and verify that it does resolve):
            binding->type_expr_node()->resolve_type( TXR_TYPE_CREATION );
            typeSpecTypeName << ( typeBindings.empty() ? "$" : ",$" );
            valueSpecTypeName << ( ix == 0 ? "$" : ",$" );
            typeBindings.push_back( binding );
        }
        else {  // binding is TxTypeArgumentNode
            binding->value_expr_node()->resolve_type( TXR_TYPE_CREATION );  // ensure binding is resolved (and verify that it does resolve)
            if ( ix > 0 )
                valueSpecTypeName << ",";
            if ( binding->value_expr_node()->is_statically_constant() ) {
                uint32_t bindingValue = eval_unsigned_int_constant( binding->value_expr_node() );
                valueSpecTypeName << bindingValue;  // statically known value
            }
            else {
                valueSpecTypeName << "?";  // dynamic value
                // implementation note: a distinct compile time type is registered which holds this specific dynamic value expression
            }
            valueBindings.push_back( binding );
        }
    }
    typeSpecTypeName << ">";
    valueSpecTypeName << ">";

    std::string newTypeNameStr;
    const std::vector<const TxTypeArgumentNode*>* bindingsPtr;
#ifdef VALUE_SPECS_SHARE_CODE
    if ( !valueBindings.empty() ) {
        // This specialization binds VALUE type parameters, so a new base type which binds only the TYPE parameters
        // is injected as intermediate base type.
        if ( !typeBindings.empty() ) {
            genBaseType = get_inner_type_specialization( definer, genBaseType, typeBindings, mutableType );
            //std::cerr << "Made intermediate type " << baseType << ";  value spec name='" << newTypeNameStr << "'" << std::endl;
        }
        newTypeNameStr = valueSpecTypeName.str();
        bindingsPtr = &valueBindings;
        // create shallow type specialization (without a distinct AST copy and code-generation) with explicit (unique) name
    }
    else {
        newTypeNameStr = typeSpecTypeName.str();
        bindingsPtr = &bindings;
    }
#else
    newTypeNameStr = valueSpecTypeName.str();
    bindingsPtr = &bindings;
#endif
    //LOG_DEBUG( this->LOGGER(), "Specializing generic type " << baseDecl << " as " << newTypeNameStr );

    // if equivalent specialized type already exists then reuse it, otherwise create new one:
    auto baseScope = baseDecl->get_symbol()->get_outer();
    auto specializedType = get_existing_type( genBaseType, *bindingsPtr, baseScope, newTypeNameStr, expErrCtx, mutableType );
    if ( !specializedType ) {
        specializedType = make_type_specialization( definer, genBaseType, *bindingsPtr, expErrCtx, newTypeNameStr, mutableType );
    }
    return specializedType;
}

const TxActualType* TypeRegistry::make_type_specialization( const TxTypeResolvingNode* definer, const TxActualType* genBaseType,
                                                      const std::vector<const TxTypeArgumentNode*>& bindings, ExpectedErrorClause* expErrCtx,
                                                      const std::string& newSpecTypeNameStr, bool mutableType ) {
    auto & baseTypeParams = genBaseType->get_type_params();
    auto baseDecl = genBaseType->get_declaration();
    auto baseTypeExpr = baseDecl->get_definer();
    auto baseScope = baseDecl->get_symbol()->get_outer();

    TxDeclarationFlags newDeclFlags;

    if ( expErrCtx )
        newDeclFlags = ( baseDecl->get_decl_flags() & DECL_FLAG_FILTER ) | TXD_IMPLICIT | TXD_EXPERROR;
    else
        newDeclFlags = ( baseDecl->get_decl_flags() & DECL_FLAG_FILTER ) | TXD_IMPLICIT;

    // create binding declaration nodes:
    bool typeBindings = false;
    bool bindingsTypeGenDependent = false;
    bool bindingsValueGenDependent = false;
    auto bindingDeclNodes = new std::vector<TxDeclarationNode*>();
    for ( unsigned ix = 0; ix < bindings.size(); ix++ ) {
        auto binding = bindings.at( ix );
        auto paramDecl = baseTypeParams.at( ix );
        auto paramName = paramDecl->get_unique_name();

        if ( !binding->is_value() ) {
            bindingsTypeGenDependent |= binding->type_expr_node()->qtype()->is_type_generic_dependent();
            bindingsValueGenDependent |= binding->type_expr_node()->qtype()->is_value_generic_dependent();

            auto btypeExprNode = new TxGenBindingAliasTypeNode( binding->get_parse_location(), binding->type_expr_node() );
            bindingDeclNodes->push_back( new TxTypeDeclNode( binding->get_parse_location(), TXD_GENBINDING | TXD_PUBLIC,
                                                             new TxIdentifierNode( binding->get_parse_location(), paramName ),
                                                             nullptr, btypeExprNode ) );
            typeBindings = true;
            LOG_TRACE( this->LOGGER(), "Re-bound base type " << baseDecl->get_unique_full_name() << " parameter '" << paramName
                       << "' with " << binding->type_expr_node() );
        }
        else {
            bindingsTypeGenDependent |= binding->value_expr_node()->qtype()->is_type_generic_dependent();
            bindingsValueGenDependent |= binding->value_expr_node()->qtype()->is_value_generic_dependent();
            // FIXME bindingsValueGenDependent |= binding->valueExprNode->is_value_gen_dependent();

            bindingDeclNodes->push_back( make_value_type_param_decl_node( binding->get_parse_location(), paramName,
                                                                          TXD_GENBINDING | TXD_PUBLIC, paramDecl, binding->value_expr_node() ) );
            LOG_TRACE( this->LOGGER(), "Re-bound base type " << baseDecl->get_unique_full_name() << " parameter '" << paramName
                       << "' with " << binding->value_expr_node() );
        }
    }

    // If any parameter is not bound, the parameter is redeclared (inherited) as still-unbound type parameter:
    // (Type args are always specified from left to right, so unbound params will be at end of param list.)
    for ( auto unboundParamI = baseTypeParams.cbegin() + bindings.size();
            unboundParamI != baseTypeParams.cend(); unboundParamI++ ) {
        auto unboundParamDecl = *unboundParamI;
        LOG_DEBUG( this->LOGGER(), "Implicitly inheriting (redeclaring) type parameters " << unboundParamDecl->get_unique_full_name()
                   << " in type " << newSpecTypeNameStr );
        if ( auto typeDecl = dynamic_cast<const TxTypeDeclaration*>( unboundParamDecl ) ) {
            bindingDeclNodes->push_back( make_type_type_param_decl_node( definer->get_parse_location(), typeDecl->get_unique_name(),
                                                                         typeDecl ) );
        }
        else {
            auto fieldDecl = static_cast<const TxFieldDeclaration*>( unboundParamDecl );
            bindingDeclNodes->push_back( make_value_type_param_decl_node( definer->get_parse_location(), fieldDecl->get_unique_name(),
                                                                          TXD_GENPARAM | TXD_IMPLICIT | TXD_PUBLIC, fieldDecl ) );
        }
    }

    // process new specialization of the base type:
    //std::cerr << "specializing base " << newBaseTypeNameStr << ": " << baseType << std::endl;
    ASSERT( dynamic_cast<const TxTypeDeclNode*>( baseTypeExpr->parent() ),
            "baseType definer's parent is not a TxTypeDeclNode: " << baseTypeExpr->parent() );
    auto baseDeclNode = static_cast<const TxTypeDeclNode*>( baseTypeExpr->parent() );
    TxTypeCreatingNode* specTypeExpr;
#ifdef VALUE_SPECS_SHARE_CODE
    if ( typeBindings )
        specTypeExpr = baseTypeExpr->make_ast_copy();
    else {
        // shallow specialization (no AST reinterpretation copy) when only VALUE params are bound
        auto shallowBaseTypeExpr = new TxTypeDeclWrapperNode( definer->get_parse_location(), baseDecl );
        specTypeExpr = new TxDerivedTypeNode( definer->get_parse_location(), shallowBaseTypeExpr, new std::vector<TxDeclarationNode*>() );
    }
#else
    specTypeExpr = baseTypeExpr->make_ast_copy();
#endif

    {   // pass on the generic base type to the new specialization via member named $GenericBase:
        // identify the "source" semantic base type - the nearest one without bindings:
        auto genBaseTypeExpr = new TxAliasTypeNode( definer->get_parse_location(),
                                                    new TxTypeDeclWrapperNode( definer->get_parse_location(), baseDecl ) );
        auto declNode = new TxTypeDeclNode( definer->get_parse_location(), TXD_PUBLIC | TXD_IMPLICIT,
                                            new TxIdentifierNode( definer->get_parse_location(), "$GenericBase" ),
                                            nullptr, genBaseTypeExpr );
        bindingDeclNodes->push_back( declNode );
    }

    auto uniqueSpecTypeNameStr = baseScope->make_unique_name( newSpecTypeNameStr );
    auto newSpecTypeDecl = new TxTypeDeclNode( definer->get_parse_location(), newDeclFlags,
                                               new TxIdentifierNode( definer->get_parse_location(), uniqueSpecTypeNameStr ),
                                               bindingDeclNodes, specTypeExpr, baseDeclNode->interfaceKW, mutableType );

    // Note: The specialized type only has a generic context if its generic base type's declaration
    //       has an outer generic-dependent context.
    //       The specialized type definer's context (whether it's generic) is not taken into account here,
    //       instead the actual bindings are analyzed whether they are generic-dependent.
    //       Also, the base type may be a specialization with generic-dependent bindings,
    //       in which case we inherit those flags.
    // Note: Base type's definer's parent is its declaration node; we're checking whether its outer scope is a generic context.
    bool outerTypeGeneric = baseDeclNode->parent()->context().is_type_generic();
    bool outerValueGeneric = baseDeclNode->parent()->context().is_value_generic();
    bindingsTypeGenDependent |= baseTypeExpr->context().is_type_gen_dep_bindings();
    bindingsValueGenDependent |= baseTypeExpr->context().is_value_gen_dep_bindings();
    LexicalContext specContext( baseScope, expErrCtx, definer, outerTypeGeneric, outerValueGeneric,
                                bindingsTypeGenDependent, bindingsValueGenDependent );
    auto internalRoot = new TxInternalRootNode( newSpecTypeDecl, specContext );
    run_declaration_pass( newSpecTypeDecl, internalRoot, "specialization" );
    this->add_reinterpretation( newSpecTypeDecl );
    baseDecl->get_symbol()->add_type_specialization( newSpecTypeDecl->get_declaration() );

    const TxActualType* specializedType = specTypeExpr->resolve_type( TXR_TYPE_CREATION ).type();
    LOG_DEBUG( this->LOGGER(), "Created new specialized type " << specializedType << " with base type " << genBaseType );
    // Invoking the full type resolution pass here can cause infinite recursion
    // (since the same source text construct may be recursively reprocessed),
    // so we enqueue this "specialization resolution pass" for later processing.
    // Note: Although reference specializations don't have a conventional "body" to be reinterpreted,
    //       they do have specialized initializers.

    return specializedType;
}


const TxActualType* TypeRegistry::create_type( const TxTypeDeclaration* declaration, const TxTypeExpressionNode* baseTypeExpr,
                                               std::vector<const TxTypeExpressionNode*>&& interfaces, bool mutableType ) {
    auto type = new TxActualType( declaration, mutableType, baseTypeExpr, std::move( interfaces ));
    this->add_type( type );
    return type;
}

const TxActualType* TypeRegistry::get_specialized_type( const TxTypeResolvingNode* definer, TxTypeExpressionNode* baseTypeExpr,
                                                        const std::vector<const TxTypeArgumentNode*>& typeArguments, bool mutableType ) {
    const TxActualType* genBaseType = baseTypeExpr->resolve_type( TXR_TYPE_CREATION ).type();
    return this->get_inner_type_specialization( definer, genBaseType, typeArguments, mutableType );
}


const TxActualType* TypeRegistry::get_reference_type( TxTypeResolvingNode* definer, const TxTypeArgumentNode* targetTypeBinding,
                                                      const TxIdentifierNode* dataspace ) {
    auto baseType = this->get_builtin_type( TXBT_REFERENCE );
    auto type = this->get_inner_type_specialization( definer, baseType,
                                                     { targetTypeBinding }, true );
    return type;
}

const TxActualType* TypeRegistry::get_array_type( TxTypeResolvingNode* definer, const TxTypeArgumentNode* elemTypeBinding,
                                                  const TxTypeArgumentNode* capBinding, bool mutableType ) {
    auto baseType = this->get_builtin_type( TXBT_ARRAY );
    auto type = this->get_inner_type_specialization( definer, baseType,
                                                     { elemTypeBinding, capBinding }, mutableType );
    return type;
}

const TxActualType* TypeRegistry::get_array_type( TxTypeResolvingNode* definer, const TxTypeArgumentNode* elemTypeBinding, bool mutableType ) {
    auto baseType = this->get_builtin_type( TXBT_ARRAY );
    auto type = this->get_inner_type_specialization( definer, baseType,
                                                     { elemTypeBinding }, mutableType );
    return type;
}

const TxActualType* TypeRegistry::create_function_type( const TxTypeDeclaration* declaration, const std::vector<const TxActualType*>& argumentTypes,
                                                        const TxActualType* returnType, bool modifying ) {
    auto funcType = new TxFunctionType( declaration, this->get_builtin_type( TXBT_FUNCTION ),
                                        argumentTypes, returnType, modifying );
    this->add_type( funcType );
    return funcType;
}

const TxActualType* TypeRegistry::create_function_type( const TxTypeDeclaration* declaration, const std::vector<const TxActualType*>& argumentTypes,
                                                        bool modifying ) {
    auto funcType = new TxFunctionType( declaration, this->get_builtin_type( TXBT_FUNCTION ),
                                        argumentTypes, modifying );
    this->add_type( funcType );
    return funcType;
}

const TxActualType* TypeRegistry::create_constructor_type( const TxTypeDeclaration* declaration, const std::vector<const TxActualType*>& argumentTypes,
                                                           const TxTypeDeclaration* constructedObjTypeDecl ) {
    auto type = new TxConstructorType( declaration, this->get_builtin_type( TXBT_FUNCTION ),
                                       argumentTypes, constructedObjTypeDecl );
    this->add_type( type );
    return type;
}

const TxActualType* TypeRegistry::create_externc_function_type( const TxTypeDeclaration* declaration, const std::vector<const TxActualType*>& argumentTypes,
                                                                const TxActualType* returnType ) {
    TxActualType* acttype;
    if ( returnType )
        acttype = new TxExternCFunctionType( declaration, this->get_builtin_type( TXBT_FUNCTION ),
                                             argumentTypes, returnType );
    else
        acttype = new TxExternCFunctionType( declaration, this->get_builtin_type( TXBT_FUNCTION ),
                                             argumentTypes, this->get_builtin_type( TXBT_VOID ));
    this->add_type( acttype );
    return acttype;
}


/*----- interface adapter -----*/

class TxAdapterTypeNode final : public TxTypeCreatingNode {
    const TxActualType* interfaceType;
    const TxActualType* adaptedType;

protected:
    virtual TxActualType* create_type( TxTypeResLevel typeResLevel ) override {
        auto adapterActType = new TxInterfaceAdapterType( this->get_declaration(), interfaceType, adaptedType );
        this->registry().add_type( adapterActType );
        return adapterActType;
    }

public:
    TxAdapterTypeNode( const TxLocation& ploc, const TxActualType* interfaceType, const TxActualType* adaptedType )
            : TxTypeCreatingNode( ploc ), interfaceType( interfaceType ), adaptedType( adaptedType ) {
    }

    TxTypeClass resolve_type_class() override { return TXTC_INTERFACEADAPTER; }

    /** Creates a copy of this node and all its descendants for purpose of generic specialization. */
    TxAdapterTypeNode* make_ast_copy() const override {
        THROW_LOGIC( "Can't reinterpret TxAdapterTypeNode: " << this );
    }

    void code_gen_type( LlvmGenerationContext& context ) const override { }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
    }
};

const TxActualType* TypeRegistry::get_interface_adapter( const TxNode* origin, const TxActualType* interfaceType,
                                                         const TxActualType* adaptedType ) {
    while ( interfaceType->is_same_vtable_type() && !interfaceType->is_explicit_declaration() )
        interfaceType = interfaceType->get_semantic_base_type();
    while ( adaptedType->is_same_vtable_type() && !adaptedType->is_explicit_declaration() )
        adaptedType = adaptedType->get_semantic_base_type();

    ASSERT( interfaceType->get_type_class() == TXTC_INTERFACE, "interfaceType not an interface type: " << interfaceType );
    ASSERT( adaptedType->get_type_class() != TXTC_INTERFACE, "adaptedType can't be an interface type: " << adaptedType );
    ASSERT( *interfaceType != *adaptedType, "Shouldn't create adapter between equivalent types" );
    ASSERT( !( adaptedType->is_empty_derivation() && !adaptedType->is_explicit_declaration() ),
            "Can't derive from implicit empty base type: " << adaptedType );

    auto ifDecl = interfaceType->get_declaration();
    auto scope = ifDecl->get_symbol()->get_outer();
    std::string adapterName = ifDecl->get_unique_name() + "$if$" + encode_type_name( adaptedType->get_declaration() );
    if ( origin->context().exp_error() )
        adapterName.append( "$EE$" );

    if ( auto existingAdapterSymbol = dynamic_cast<TxEntitySymbol*>( scope->get_member_symbol( adapterName ) ) ) {
        if ( auto typeDecl = existingAdapterSymbol->get_type_decl() ) {
            auto adapterType = typeDecl->get_definer()->resolve_type( TXR_TYPE_CREATION ).type();
            //std::cerr << "Getting existing interface adapter: " << adapterType << std::endl;
            return adapterType;
        }
    }

    LOG_TRACE( this->LOGGER(), "Creating interface adapter: " << adapterName << "\n\tfrom " << adaptedType << "\n\tto   " << interfaceType );

    auto & loc = origin->get_parse_location();
    auto adapterTypeNode = new TxAdapterTypeNode( loc, interfaceType, adaptedType );

    // override the adaptee type id virtual field member:
    auto tidFieldDecl = new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_STATIC | TXD_OVERRIDE | TXD_IMPLICIT,
                                             new TxNonLocalFieldDefNode( loc, new TxIdentifierNode( loc, "$adTypeId" ),
                                                                         new TxNamedTypeNode( loc, "tx.UInt" ), nullptr ) );
    auto fieldDecls = new std::vector<TxDeclarationNode*>( { tidFieldDecl } );

    // TODO: combine TXD_EXPERROR flag from adapted and adaptee types?
    auto adapterDeclNode = new TxTypeDeclNode( loc, TXD_PUBLIC | TXD_IMPLICIT,
                                               new TxIdentifierNode( loc, adapterName ), fieldDecls, adapterTypeNode );

    auto & adaptedTypeCtx = adaptedType->get_declaration()->get_definer()->context();
    auto * errCtx = origin->context().exp_error() ? origin->context().exp_error() : adaptedTypeCtx.exp_error();
    LexicalContext adapterCtx( scope, errCtx, adaptedTypeCtx.reinterpretation_definer(),
                               adaptedTypeCtx.is_type_generic(), adaptedTypeCtx.is_value_generic(),
                               adaptedTypeCtx.is_type_gen_dep_bindings(), adaptedTypeCtx.is_value_gen_dep_bindings() );

    auto internalRoot = new TxInternalRootNode( adapterDeclNode, adapterCtx );
    run_declaration_pass( adapterDeclNode, internalRoot, "adapter" );
    this->add_reinterpretation( adapterDeclNode );

    auto adapterType = adapterTypeNode->resolve_type( TXR_TYPE_CREATION ).type();
    return adapterType;
}
