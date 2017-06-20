#include "util/util.hpp"
#include "util/assert.hpp"

#include "type_registry.hpp"

#include "../ast/ast_entitydecls.hpp"
#include "ast/ast_declpass.hpp"
#include "tx_logging.hpp"
#include "tx_error.hpp"
#include "package.hpp"
#include "entity.hpp"
#include "builtin/builtin_types.hpp"

#include "llvm_generator.hpp"

#include "ast/ast_wrappers.hpp"
#include "ast/type/ast_types.hpp"
#include "ast/expr/ast_constexpr.hpp"


/** the flags that may be inherited when specializing a type */
static const TxDeclarationFlags DECL_FLAG_FILTER = TXD_STATIC | TXD_PUBLIC | TXD_PROTECTED | TXD_ABSTRACT | TXD_FINAL | TXD_IMPLICIT
                                                   | TXD_EXPERRBLOCK;

Logger& TypeRegistry::_LOG = Logger::get( "REGISTRY" );

TypeRegistry::TypeRegistry( TxPackage& package )
        : _package( package ) {
}

const TxLocation& TypeRegistry::get_builtin_location() const {
    return this->_package.builtins().get_builtin_location();
}

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
                ScopedExpErrClause scopedEEClause( type->get_definer(), type->get_definer()->exp_err_ctx() );
                type->type();
            }
            catch ( const resolution_error& err ) {
                LOG( this->LOGGER(), NOTE, "Caught resolution error resolving deferred type " << type << ": " << err );
            }
        }

        for ( ; specIx != this->enqueuedSpecializations.size(); specIx++ ) {
            //std::cerr << "Nof enqueued specializations: " << this->enqueuedSpecializations.size() << std::endl;
            auto specDecl = this->enqueuedSpecializations.at( specIx );
            LOG_DEBUG( this->LOGGER(), "Resolving enqueued specialization: " << specDecl << ( specDecl->exp_err_ctx() ? " (has ExpErr context)" : "" ));
            try {
                ScopedExpErrClause scopedEEClause( specDecl, specDecl->exp_err_ctx() );
                specDecl->symbol_resolution_pass();
            }
            catch ( const resolution_error& err ) {
                // if this happens, investigate why it wasn't caught before this type was added to the types queue
                LOG( this->LOGGER(), ALERT, "Caught resolution error resolving enqueued type specialization " << specDecl << ": " << err );
            }
        }
    }while ( typeIx != this->usedTypes.size() );
}

void TypeRegistry::add_type_usage( TxType* type ) {
    ASSERT( !this->startedPreparingTypes, "Can't create new types when type preparation phase has started: " << type );
    this->usedTypes.push_back( type );
}

void TypeRegistry::add_type( TxActualType* type ) {
    ASSERT( !this->startedPreparingTypes, "Can't create new types when type preparation phase has started: " << type );
    if ( type->formalTypeId < BuiltinTypeId_COUNT ) {
        ASSERT( type->formalTypeId == this->formalTypes.size(), "adding built-in type in wrong order: type id="
                << type->formalTypeId << "; staticTypes.size()=" << this->formalTypes.size() );
        this->formalTypes.push_back( type );
    }
    this->createdTypes.push_back( type );
}

void TypeRegistry::prepare_types() {
    this->startedPreparingTypes = true;
    std::map<uint32_t, TxActualType*> statics;
    for ( auto type : this->createdTypes ) {
        try {
            //std::cerr << "Preparing type: " << type << std::endl;
            type->prepare_members();
        }
        catch ( const resolution_error& err ) {
            // if this happens, investigate why it wasn't caught before this type was added to the types list
            LOG( this->LOGGER(), ALERT, "Caught resolution error preparing members of type " << type << ": " << err );
            continue;
        }

        if ( type->formalTypeId < BuiltinTypeId_COUNT )  // the built-in types are already handled
            continue;

        // Types that are distinct in instance data type, or vtable, get distinct type id and vtable.
        // Certain types that will not be represented as runtime types are filtered out here:

        if ( type->get_declaration()->get_definer()->exp_err_ctx() ) {
            LOG_DEBUG( this->LOGGER(), "Not registering type with ExpErr context as formal type: " << type);
            continue;
        }
        // there shouldn't be a TXD_EXPERRBLOCK declaration without exp-err-ctx set:
        ASSERT( !(type->get_declaration()->get_decl_flags() & TXD_EXPERRBLOCK), "Unexpected TXD_EXPERRBLOCK flag in type "<< type);

        if ( type->get_type_class() == TXTC_FUNCTION )
            continue;

        if ( type->get_type_class() == TXTC_REFERENCE )
            continue;

        if ( type->is_equivalent_derivation() ) {  // includes empty and modifiable derivations
            //std::cerr << "Not registering distinct type id for equivalent derivation: " << type << std::endl;
            continue;
        }

//        if ( type->is_generic_dependent() ) {
//            // FUTURE: review whether generic base types should be a vtable parent of their specializations
//            LOG_DEBUG( this->LOGGER(), "Not registering generic-dependent type as formal type: " << type );
//            continue;
//        }

        type->formalTypeId = this->formalTypes.size();
        this->formalTypes.push_back( type );
        //std::cerr << "Registering formal type " << type << " with distinct type id " << type->formalTypeId << std::endl;
    }
}

const TxType* TypeRegistry::get_builtin_type( const BuiltinTypeId id, bool mod ) {
    auto biType = this->_package.builtins().get_builtin_type( id );
    return ( mod ? this->get_modifiable_type( nullptr, biType ) : biType );
}

const TxType* TypeRegistry::get_string_type() {
    if (! this->stringTypeNode ) {
        stringTypeNode = new TxNamedTypeNode( this->get_builtin_location(), "tx.String" );
        run_declaration_pass( stringTypeNode, LexicalContext( this->_package.get_member_symbol( "tx" ), nullptr, false, nullptr ) );
        return this->stringTypeNode->resolve_type();
    }
    return this->stringTypeNode->get_type();
}

TxType* TypeRegistry::make_type_entity( const TxActualType* actualType ) {
    return new TxType( actualType );
}

TxActualType* TypeRegistry::make_modifiable_type( const TxTypeDeclaration* declaration, const TxActualType* baseType ) {
    auto newType = baseType->make_specialized_type( declaration, TxTypeSpecialization( baseType, true ) );
    this->add_type( newType );
    return newType;
}

TxActualType* TypeRegistry::make_actual_type( const TxTypeDeclaration* declaration, const TxActualType* baseType, bool mutableType,
                                              const std::vector<const TxType*>& interfaces ) {
    std::vector<TxTypeSpecialization> interfaceSpecializations;
    for ( auto in : interfaces )
        interfaceSpecializations.emplace_back( in->type() );
    auto newType = baseType->make_specialized_type( declaration, TxTypeSpecialization( baseType ), mutableType, interfaceSpecializations );
    this->add_type( newType );
    return newType;
}

const TxType* TypeRegistry::get_modifiable_type( const TxTypeDeclaration* declaration, const TxType* type ) {
    auto actualType = type->type();
    ASSERT( !actualType->is_modifiable(), "Can't make a modifiable specialization of a modifiable type: " << actualType );
    ASSERT( !( actualType->is_empty_derivation() && !actualType->get_explicit_declaration() ),
            "Can't derive from implicit empty base type: " << actualType );
    // 'modifiable' is always a distinct 'specialization' (no parameter bindings (or type extensions))

    if ( declaration ) {
        return new TxType( this->make_modifiable_type( declaration, actualType ) );
    }
    else {
        std::string prefix = "~";
        std::string name = prefix + actualType->get_declaration()->get_unique_name();
        TxScopeSymbol* scope = actualType->get_declaration()->get_symbol()->get_outer();
        if ( auto entitySymbol = dynamic_cast<TxEntitySymbol*>( scope->get_member_symbol( name ) ) ) {
            if ( auto typeDecl = entitySymbol->get_type_decl() ) {
                if ( auto existingType = typeDecl->get_definer()->resolve_type() ) {
                    if ( existingType->is_modifiable() && *existingType->get_base_type()->type() == *actualType )
                        return existingType;
                    //std::cerr << "existing: " << existingType << "  new: " << type << std::endl;
                }
            }
            LOG( this->LOGGER(), WARN, "Name collision when trying to declare implicit MOD type, preexisting symbol: " << entitySymbol );
            name = scope->make_unique_name( name );
        }

        const TxLocation& loc = actualType->get_parse_location();
        auto modNode = new TxModifiableTypeNode( loc, new TxNamedTypeNode( loc, actualType->get_declaration()->get_unique_name() ) );
        TxDeclarationFlags newDeclFlags = ( actualType->get_declaration()->get_decl_flags() & DECL_FLAG_FILTER ); // | TXD_IMPLICIT;
        auto modDeclNode = new TxTypeDeclNode( loc, newDeclFlags, name, nullptr, modNode );
        auto expErrCtx = actualType->get_declaration()->get_definer()->context().exp_error();
        run_declaration_pass( modDeclNode, LexicalContext( scope, expErrCtx, false, nullptr ) );
        modDeclNode->symbol_resolution_pass();  // (causes get_modifiable_type() to be invoked again, with this implicit declaration)
        return modNode->get_type();
    }
}

const TxType* TypeRegistry::make_empty_derivation( const TxTypeDeclaration* declaration, const TxType* baseType, bool mutableType ) {
    ASSERT( declaration, "empty type specialization doesn't have declaration: " << baseType );
    return new TxType( declaration->get_definer(),
                       [ this, declaration, baseType, mutableType ] () {
                           return this->make_actual_empty_derivation( declaration, baseType->type(), mutableType );
                       } );
}

const TxActualType* TypeRegistry::make_actual_empty_derivation( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                                                bool mutableType ) {
    ASSERT( !baseType->is_modifiable(), "Can't specialize a 'modifiable' base type: " << baseType );
    ASSERT( !( baseType->is_empty_derivation() && !baseType->get_explicit_declaration() ),
            "Can't derive from implicit empty base type: " << baseType );

    return this->make_actual_type( declaration, baseType, mutableType );
}

const TxType* TypeRegistry::make_type_derivation( TxTypeExpressionNode* definer, const TxType* baseType,
                                                  const std::vector<const TxType*>& interfaces, bool mutableType ) {
    ASSERT( definer->get_declaration(), "type derivation doesn't have declaration: " << definer );
    return new TxType( definer,
                       [ this, definer, baseType, interfaces, mutableType ] () {
                           return this->make_actual_type_derivation( definer, baseType->type(), interfaces, mutableType );
                       } );
}

const TxActualType* TypeRegistry::make_actual_type_derivation( const TxTypeExpressionNode* definer, const TxActualType* baseType,
                                                               const std::vector<const TxType*>& interfaces, bool mutableType ) {
    ASSERT( !( baseType->is_empty_derivation() && !baseType->get_explicit_declaration() ),
            "Can't derive from implicit empty base type: " << baseType );

    if ( baseType->is_modifiable() )
        CERR_THROWRES( definer, "Can't derive from a 'modifiable' base type (derive from the actual type instead): " << baseType );
    for ( auto interface : interfaces ) {
        if ( interface->is_modifiable() )
            CERROR( definer, "Can't implement a 'modifiable' interface (implement the actual interface type instead): " << interface );
    }
    if ( baseType->get_declaration()->get_decl_flags() & ( TXD_GENPARAM | TXD_GENBINDING ) ) {
        // only empty derivation allowed from generic type parameter
        if ( !interfaces.empty() )
            CERR_THROWRES( definer, "Can't specialize a generic type parameter: " << baseType );
    }

    return this->make_actual_type( definer->get_declaration(), baseType, mutableType, interfaces );
}

const TxType* TypeRegistry::get_type_specialization( TxTypeDefiningNode* definer, const TxType* baseType,
                                                     const std::vector<const TxTypeArgumentNode*>& bindings, bool mutableType ) {
    ASSERT( !bindings.empty(), "Empty bindings list when specializing baseType: " << baseType );
    ASSERT( bindings.at( 0 )->is_context_set(), "context not set for binding " << bindings.at(0) );
    return new TxType( definer,
                       [ this, definer, baseType, bindings, mutableType ] () {
                           return this->get_actual_type_specialization( definer, baseType->type(), &bindings, mutableType );
                       } );
}

const TxActualType* TypeRegistry::get_actual_type_specialization( const TxTypeDefiningNode* definer, const TxActualType* baseType,
                                                                  const std::vector<const TxTypeArgumentNode*>* bindings, bool mutableType ) {
    // Note: type specialization is never applied to a modifiable-specialization (legal only on non-modifiable base type)
    ASSERT( !baseType->is_modifiable(), "Can't specialize a 'modifiable' base type: " << baseType );
    ASSERT( !( baseType->is_empty_derivation() && !baseType->get_explicit_declaration() ),
            "Can't derive from implicit empty base type: " << baseType );

    const TxTypeDeclaration* declaration = nullptr;
    if ( auto typeExprNode = dynamic_cast<const TxTypeExpressionNode*>( definer ) )
        declaration = typeExprNode->get_declaration();

    const TxActualType* specializedType = get_inner_type_specialization( definer, baseType, bindings, mutableType );

    if ( declaration ) {
        // create empty type specialization with explicit (unique) name
        specializedType = this->make_actual_type( declaration, specializedType, mutableType );
    }

    return specializedType;
}

std::string encode_type_name( const TxTypeDeclaration* typeDecl ) {
    std::stringstream boundTypeName;
    boundTypeName << hashify( typeDecl->get_unique_full_name() );
    return boundTypeName.str();
}

static const TxActualType* matches_existing_type( TxEntitySymbol* existingBaseSymbol, const TxActualType* baseType,
                                                  const std::vector<const TxTypeArgumentNode*>* bindings ) {
    if ( auto typeDecl = existingBaseSymbol->get_type_decl() ) {
        auto existingBaseType = typeDecl->get_definer()->resolve_type()->type();
        //std::cerr << "existingBaseType    1: " << existingBaseType << std::endl;
        auto existingGenBaseType = existingBaseType->get_semantic_base_type();
        //std::cerr << "existingGenBaseType 2: " << existingGenBaseType << std::endl;
        if ( existingGenBaseType->is_empty_derivation() ) {
            existingGenBaseType = existingGenBaseType->get_semantic_base_type();
            //std::cerr << "existingGenBaseType 3: " << existingGenBaseType << std::endl;
        }
        if ( *existingGenBaseType == *baseType ) {
            auto existingBaseTypeBindings = existingBaseType->get_bindings();

            bool matchOK = true;
            for ( unsigned ix = 0; ix < bindings->size(); ix++ ) {
                auto binding = bindings->at( ix );
                if ( auto typeBinding = dynamic_cast<const TxTypeTypeArgumentNode*>( binding ) ) {
                    if ( auto existingBindingTypeDecl = dynamic_cast<const TxTypeDeclaration*>( existingBaseTypeBindings.at( ix ) ) ) {
                        auto newBindingType = typeBinding->typeExprNode->get_type();
                        auto existingBindingType = existingBindingTypeDecl->get_definer()->resolve_type();
                        if ( newBindingType->shallow_equals( existingBindingType ) )
                            continue;
                    }
                }
                else {  // TxValueTypeArgumentNode
                    // Statically constant VALUE specializations with distinct values are distinct types.
                    // Dynamic VALUE specializations with distinct value expressions are distinct (we presume inequality in this implementation).
                    auto valueBinding = static_cast<const TxValueTypeArgumentNode*>( binding );
                    if ( auto existingFieldDecl = dynamic_cast<const TxFieldDeclaration*>( existingBaseTypeBindings.at( ix ) ) ) {
                        // to match, both need to be statically constant and with equal value
                        ASSERT(valueBinding->valueExprNode, "valueBinding->valueExprNode is null for " << existingBaseSymbol);
                        if ( auto existingInitializer = existingFieldDecl->get_definer()->get_init_expression() ) {
                            if ( valueBinding->valueExprNode->is_statically_constant() && existingInitializer->is_statically_constant() ) {
                                auto actType = valueBinding->valueExprNode->get_type()->type();
                                if ( actType->has_type_id() && is_concrete_uinteger_type( (BuiltinTypeId) actType->get_type_id() ) )
                                    if ( eval_unsigned_int_constant( valueBinding->valueExprNode ) == eval_unsigned_int_constant( existingInitializer ) )
                                        continue;
                            }
//                            if ( is_static_equal( valueBinding->valueExprNode, existingInitializer ) )
//                                continue;
                        }
                    }
                }
                //std::cerr << "NOT ACCEPTING PRE-EXISTING TYPE " << existingBaseType << " SINCE " << std::endl;
                matchOK = false;
                break;
            }
            if ( matchOK ) {
                LOG_DEBUG( existingBaseSymbol->get_root_scope()->registry().LOGGER(),
                           "new specialization equal to preexisting one, reusing: " << existingBaseType );
                return existingBaseType;
            }
        }
        //LOG(existingBaseSymbol->get_root_scope()->registry().LOGGER(), INFO, "Found existing but mismatching type with sought name: " << existingBaseType);
    }

    return nullptr;
}

static const TxActualType* get_existing_type( const TxActualType* baseType, const std::vector<const TxTypeArgumentNode*>* bindings,
                                              TxScopeSymbol* baseScope, const std::string& newBaseName ) {
    auto baseTypeParams = baseType->get_type_params();
    if ( bindings->size() == baseTypeParams.size() ) {
        // if generic type specialization is equivalent to the generic base type, reuse it:
        bool matchOK = true;
        for ( unsigned ix = 0; ix < bindings->size(); ix++ ) {
            auto binding = bindings->at( ix );
            auto paramDecl = baseTypeParams.at( ix );
            if ( auto typeBinding = dynamic_cast<const TxTypeTypeArgumentNode*>( binding ) ) {
                auto typeParamDecl = static_cast<const TxTypeDeclaration*>( paramDecl );
                auto constraintType = typeParamDecl->get_definer()->get_type();
                auto bindingType = typeBinding->typeExprNode->get_type();
                if ( bindingType->get_decl_flags() & TXD_GENPARAM ) {
                    auto bindingDecl = bindingType->get_declaration();
                    if ( bindingDecl == constraintType->get_declaration() ) {
                        //std::cerr << "binding refers to 'itself' (its parameter declaration): " << bindingDecl << std::endl;
                        continue;// binding refers to "itself" (its parameter declaration)
                    }
                    // else binding is to a different type parameter and is effectively an explicit unique type
                }
                else if ( constraintType == bindingType )
                    continue;
            }
            else {  // TxValueTypeArgumentNode
                // VALUE parameters don't have "defaults"
            }
            matchOK = false;
            break;
        }
        if ( matchOK ) {
            LOG_DEBUG( baseScope->get_root_scope()->registry().LOGGER(), "new specialization equal to the generic base type, reusing: " << baseType );
            return baseType;
        }
    }

    // if name already exists and specialization is equal, reuse it:
    std::string upperBound = newBaseName;
    upperBound[upperBound.size() - 1] += 1;
    for ( auto existingBaseNameI = baseScope->alpha_order_names_lower( newBaseName );
            existingBaseNameI != baseScope->alpha_order_names_upper( upperBound ); existingBaseNameI++ ) {
        if ( auto existingBaseSymbol = dynamic_cast<TxEntitySymbol*>( baseScope->get_member_symbol( *existingBaseNameI ) ) ) {
            if ( auto matchingType = matches_existing_type( existingBaseSymbol, baseType, bindings ) )
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
    auto declNode = new TxTypeDeclNode( parseLoc, flags | TXD_PUBLIC, paramName, nullptr, typeExpr );
    return declNode;
}

/** Makes a VALUE type parameter declaration node (both for bindings and param redeclarations). */
static TxDeclarationNode* make_value_type_param_decl_node( const TxLocation& parseLoc, const std::string& paramName, TxDeclarationFlags flags,
                                                           const TxEntityDeclaration* paramValueTypeDecl,
                                                           TxExpressionNode* valueDefiner = nullptr ) {
    if (valueDefiner)
        valueDefiner = new TxExprWrapperNode( valueDefiner );
    auto paramTypeNode = new TxTypeDeclWrapperNode( parseLoc, paramValueTypeDecl );
    auto fieldDef = new TxFieldDefNode( parseLoc, paramName, paramTypeNode, valueDefiner, false );
    auto declNode = new TxFieldDeclNode( parseLoc, flags | TXD_PUBLIC, fieldDef );
    return declNode;
}

const TxActualType* TypeRegistry::get_inner_type_specialization( const TxTypeDefiningNode* definer, const TxActualType* baseType,
                                                                 const std::vector<const TxTypeArgumentNode*>* bindings, bool mutableType ) {
    // Note: A non-parameterized type (without any declared type parameters) is not necessarily non-generic:
    //       It may have members that refer to generic parameters declared in an outer scope.
    // Note: Binding of ref-constrained type parameters doesn't necessarily affect code generation,
    //       but accesses to the members still need the specialized version of them in semantic analysis (e.g. the correct ref-target).

    auto baseDecl = baseType->get_declaration();
    if ( baseDecl->get_decl_flags() & ( TXD_GENPARAM | TXD_GENBINDING ) ) {
        // only empty derivation allowed from generic type parameter
        CERR_THROWRES( definer, "Can't specialize a generic type parameter: " << baseType );
    }

    // re-base the new type on new non-generic specialization of the base type:
    // (this replaces the type parameter bindings with direct declarations within the new type)
    LOG_DEBUG( this->LOGGER(), "Re-basing the new type of [ " << definer << " ] by specializing the generic base type " << baseType );

    // Note: The same generic type specialization may be produced by multiple statements,
    //       both within ExpErr constructs and without. Therefore the type name must distinguish between them.
    // If either the generic type or its specialization site is defined within an exp-err-context,
    // the specialization inherits that exp-err-context:
    ExpectedErrorClause* expErrCtx = definer->exp_err_ctx();
    if (! expErrCtx)
        expErrCtx = baseDecl->get_definer()->exp_err_ctx();

    std::stringstream newBaseTypeName;
    if ( expErrCtx )
        newBaseTypeName << "$EE$";
    if (mutableType && baseType->get_type_class() != TXTC_REFERENCE && baseType->get_type_class() != TXTC_INTERFACE ) {
        // (References and interfaces are always mutable, don't distinguish them be name)
        newBaseTypeName << "M$";  // distinguish mutable and immutable specializations by name
    }
    newBaseTypeName << baseDecl->get_unique_name() << "<";

    // do shallow validation that bindings match base type's parameters:
    auto baseTypeParams = baseType->get_type_params();
    if ( baseTypeParams.size() < bindings->size() ) {
        CERR_THROWRES( definer, "Too many type arguments specifified for generic base type " << baseType );
    }

    for ( unsigned ix = 0; ix < bindings->size(); ix++ ) {
        auto binding = bindings->at( ix );
        auto paramDecl = baseTypeParams.at( ix );
        if ( ix )
            newBaseTypeName << ",";
        ASSERT( binding->is_context_set(), "Binding must have run declaration pass before being used in type specialization: " << binding );
        paramDecl->get_definer()->resolve_type();  // ensure param is resolved (and verify that it does resolve)

        if ( auto typeArg = dynamic_cast<const TxTypeTypeArgumentNode*>( binding ) ) {
            if ( !dynamic_cast<const TxTypeDeclaration*>( paramDecl ) )
                CERR_THROWRES( binding, "Can't bind a VALUE base type parameter using a TYPE: " << paramDecl->get_unique_full_name() );

            typeArg->typeExprNode->resolve_type();  // ensure binding is resolved (and verify that it does resolve)
            newBaseTypeName << "$";
        }
        else {  // binding is TxValueTypeArgumentNode
            auto valueArg = static_cast<const TxValueTypeArgumentNode*>( binding );
            if ( !dynamic_cast<const TxFieldDeclaration*>( paramDecl ) )
                CERR_THROWRES( binding, "Can't bind a TYPE base type parameter using a VALUE: " << paramDecl->get_unique_full_name() );

            valueArg->valueExprNode->resolve_type();  // ensure binding is resolved (and verify that it does resolve)
            if ( valueArg->valueExprNode->is_statically_constant() ) {
                uint32_t bindingValue = eval_unsigned_int_constant( valueArg->valueExprNode );
                newBaseTypeName << bindingValue;  // statically known value
            }
            else {
                newBaseTypeName << "?";  // dynamic value
                // implementation note: a distinct compile time type is registered which holds this specific dynamic value expression
            }
        }
    }
    newBaseTypeName << ">";
    auto newBaseTypeNameStr = newBaseTypeName.str();

    // if equivalent specialized type already exists then reuse it, otherwise create new one:
    auto baseScope = baseDecl->get_symbol()->get_outer();
    const TxActualType* specializedType = get_existing_type( baseType, bindings, baseScope, newBaseTypeNameStr );
    if ( !specializedType ) {
        specializedType = make_type_specialization( definer, baseType, bindings, expErrCtx, newBaseTypeNameStr, mutableType );
    }
    return specializedType;
}

const TxActualType* TypeRegistry::make_type_specialization( const TxTypeDefiningNode* definer, const TxActualType* baseType,
                                                            const std::vector<const TxTypeArgumentNode*>* bindings, ExpectedErrorClause* expErrCtx,
                                                            const std::string& newSpecTypeNameStr, bool mutableType ) {
    auto baseDecl = baseType->get_declaration();
    auto baseScope = baseDecl->get_symbol()->get_outer();
    auto baseTypeParams = baseType->get_type_params();

    TxDeclarationFlags newDeclFlags;

    if ( expErrCtx )
        newDeclFlags = ( baseDecl->get_decl_flags() & DECL_FLAG_FILTER ) | TXD_IMPLICIT | TXD_EXPERRBLOCK;
    else
        newDeclFlags = ( baseDecl->get_decl_flags() & DECL_FLAG_FILTER ) | TXD_IMPLICIT;

    // create binding declaration nodes:
    auto bindingDeclNodes = new std::vector<TxDeclarationNode*>();
    for ( unsigned ix = 0; ix < bindings->size(); ix++ ) {
        auto binding = bindings->at( ix );
        auto paramDecl = baseTypeParams.at( ix );
        auto paramName = paramDecl->get_unique_name();

        if ( auto typeArg = dynamic_cast<const TxTypeTypeArgumentNode*>( binding ) ) {
            TxTypeDefiningNode* btypeDefNode = typeArg->typeExprNode;
            TxTypeExpressionNode* btypeExprNode = new TxTypeExprWrapperNode( btypeDefNode );
            bindingDeclNodes->push_back( new TxTypeDeclNode( typeArg->get_parse_location(), TXD_GENBINDING | TXD_PUBLIC, paramName, nullptr,
                                                             btypeExprNode ) );
            LOG_TRACE( this->LOGGER(), "Re-bound base type " << baseDecl->get_unique_full_name() << " parameter '" << paramName
                       << "' with " << typeArg->typeExprNode );
        }
        else {
            auto valueArg = static_cast<const TxValueTypeArgumentNode*>( binding );
            bindingDeclNodes->push_back( make_value_type_param_decl_node( valueArg->get_parse_location(), paramName, TXD_GENBINDING,
                                                                          paramDecl, valueArg->valueExprNode ) );
            LOG_TRACE( this->LOGGER(), "Re-bound base type " << baseDecl->get_unique_full_name() << " parameter '" << paramName
                       << "' with " << valueArg->valueExprNode );
        }
    }

    // If any parameter is not bound, the parameter is redeclared (inherited) as still-unbound type parameter:
    // (Type args are always specified from left to right, so unbound params will be at end of param list.)
    for ( auto unboundParamI = baseTypeParams.cbegin() + bindings->size();
            unboundParamI != baseTypeParams.cend(); unboundParamI++ ) {
        auto unboundParamDecl = *unboundParamI;
        LOG_DEBUG( this->LOGGER(), "Implicitly inheriting (redeclaring) type parameters " << unboundParamDecl->get_unique_full_name()
                   << " in type " << newSpecTypeNameStr );
        if ( auto typeDecl = dynamic_cast<const TxTypeDeclaration*>( unboundParamDecl ) ) {
            bindingDeclNodes->push_back( make_type_type_param_decl_node( definer->get_parse_location(), typeDecl->get_unique_name(),
                                                                         TXD_GENPARAM, typeDecl ) );
        }
        else {
            auto fieldDecl = static_cast<const TxFieldDeclaration*>( unboundParamDecl );
            bindingDeclNodes->push_back( make_value_type_param_decl_node( definer->get_parse_location(), fieldDecl->get_unique_name(),
                                                                          TXD_GENPARAM, fieldDecl ) );
        }
    }

    // process new specialization of the base type:
    //std::cerr << "specializing base " << newBaseTypeNameStr << ": " << baseType << std::endl;
    ASSERT( dynamic_cast<TxTypeExpressionNode*>( baseDecl->get_definer() ),
            "baseType's definer is not a TxTypeExpressionNode: " << baseDecl->get_definer() );
    auto baseTypeExpr = static_cast<TxTypeExpressionNode*>( baseDecl->get_definer() );
    ASSERT( dynamic_cast<const TxTypeDeclNode*>( baseTypeExpr->parent() ),
            "baseType definer's parent is not a TxTypeDeclNode: " << baseTypeExpr->parent() );
    auto baseDeclNode = static_cast<const TxTypeDeclNode*>( baseTypeExpr->parent() );
    auto specTypeExpr = baseTypeExpr->make_ast_copy();

    {   // pass on the generic base type to the new specialization via member named $GenericBase:
        auto baseTypeExpr = new TxTypeDeclWrapperNode( definer->get_parse_location(), baseDecl );
        auto declNode = new TxTypeDeclNode( definer->get_parse_location(), TXD_PUBLIC | TXD_IMPLICIT, "$GenericBase", nullptr, baseTypeExpr );
        bindingDeclNodes->push_back( declNode );
    }

    auto uniqueSpecTypeNameStr = baseScope->make_unique_name( newSpecTypeNameStr );
    auto newSpecTypeDecl = new TxTypeDeclNode( definer->get_parse_location(), newDeclFlags, uniqueSpecTypeNameStr, bindingDeclNodes, specTypeExpr,
                                               baseDeclNode->interfaceKW, mutableType );

    bool outerIsGeneric = definer->parent()->context().is_generic();
    LexicalContext specContext( baseScope, expErrCtx, outerIsGeneric, definer );
    run_declaration_pass( newSpecTypeDecl, specContext );
    const TxActualType* specializedType = specTypeExpr->resolve_type()->type();
    LOG_DEBUG( this->LOGGER(), "Created new specialized type " << specializedType << " with base type " << baseType );

    // Invoking the resolution pass here can cause infinite recursion
    // (since the same source text construct may be recursively reprocessed,
    //  and the bindings may refer to this type's declaration),
    // so we enqueue this "specialization resolution pass" for later processing.
    //std::cerr << "enqueuing specialization " << newBaseTypeDecl << std::endl;
    this->enqueuedSpecializations.emplace_back( newSpecTypeDecl );

    return specializedType;
}

const TxType* TypeRegistry::get_reference_type( TxTypeDefiningNode* definer, const TxTypeTypeArgumentNode* targetTypeBinding,
                                                const TxIdentifier* dataspace ) {
    std::vector<const TxTypeArgumentNode*> bindings( { targetTypeBinding } );
    return this->get_type_specialization( definer, this->get_builtin_type( TXBT_REFERENCE ), bindings, true );
}

const TxType* TypeRegistry::get_array_type( TxTypeDefiningNode* definer, const TxTypeTypeArgumentNode* elemTypeBinding,
                                            const TxValueTypeArgumentNode* capBinding, bool mutableType ) {
    std::vector<const TxTypeArgumentNode*> bindings( { elemTypeBinding, capBinding } );
    return this->get_type_specialization( definer, this->get_builtin_type( TXBT_ARRAY ), bindings, mutableType );
}

const TxType* TypeRegistry::get_array_type( TxTypeDefiningNode* definer, const TxTypeTypeArgumentNode* elemTypeBinding, bool mutableType ) {
    std::vector<const TxTypeArgumentNode*> bindings( { elemTypeBinding } );
    return this->get_type_specialization( definer, this->get_builtin_type( TXBT_ARRAY ), bindings, mutableType );
}

static std::vector<const TxActualType*> make_actual_arg_types( const std::vector<const TxType*>& argumentTypes ) {
    std::vector<const TxActualType*> actTypes;
    for ( auto a : argumentTypes )
        actTypes.push_back( a->type() );
    return actTypes;
}

const TxType* TypeRegistry::get_function_type( const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                               const TxType* returnType,
                                               bool modifiableClosure ) {
    auto funcType = new TxFunctionType( declaration, this->get_builtin_type( TXBT_FUNCTION )->type(),
                                        make_actual_arg_types( argumentTypes ),
                                        returnType->type(), modifiableClosure );
    this->add_type( funcType );
    return new TxType( funcType );
}

const TxType* TypeRegistry::get_function_type( const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                               bool modifiableClosure ) {
    auto funcType = new TxFunctionType( declaration, this->get_builtin_type( TXBT_FUNCTION )->type(),
                                        make_actual_arg_types( argumentTypes ),
                                        modifiableClosure );
    this->add_type( funcType );
    return new TxType( funcType );
}

const TxType* TypeRegistry::get_constructor_type( const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes,
                                                  const TxTypeDeclaration* objectTypeDecl ) {
    auto type = new TxConstructorType( declaration, this->get_builtin_type( TXBT_FUNCTION )->type(),
                                       make_actual_arg_types( argumentTypes ),
                                       objectTypeDecl );
    this->add_type( type );
    return new TxType( type );
}

/*----- interface adapter -----*/

class TxAdapterTypeNode final : public TxTypeExpressionNode {
    const TxActualType* interfaceType;
    const TxActualType* adaptedType;

protected:
    virtual const TxType* define_type() override {
        auto adapterActType = new TxInterfaceAdapterType( this->get_declaration(), interfaceType, adaptedType );
        this->registry().add_type( adapterActType );
        return this->registry().make_type_entity( adapterActType );
    }

public:
    TxAdapterTypeNode( const TxLocation& ploc, const TxActualType* interfaceType, const TxActualType* adaptedType )
            : TxTypeExpressionNode( ploc ), interfaceType( interfaceType ), adaptedType( adaptedType ) {
    }

    /** Creates a copy of this node and all its descendants for purpose of generic specialization. */
    virtual TxAdapterTypeNode* make_ast_copy() const override {
        ASSERT( false, "unexpected reinterpretation of " << this );
        return nullptr; //new TxAdapterTypeNode( this );
    }

    virtual void code_gen_type( LlvmGenerationContext& context ) const override { }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }
};

const TxType* TypeRegistry::get_interface_adapter( const TxType* interface, const TxType* adapted ) {
    // for now we don't defer it, we create it right away
    auto adapterType = this->get_actual_interface_adapter( interface->type(), adapted->type() );
    return adapterType;
}

const TxType* TypeRegistry::get_actual_interface_adapter( const TxActualType* interfaceType, const TxActualType* adaptedType ) {
    auto modIf = interfaceType->is_modifiable();

    while ( interfaceType->is_same_vtable_type() )
        interfaceType = interfaceType->get_semantic_base_type();
    while ( adaptedType->is_same_vtable_type() )
        adaptedType = adaptedType->get_semantic_base_type();

    ASSERT( !interfaceType->is_modifiable(), "Shouldn't create adapter for 'modifiable' interface type: " << interfaceType );
    ASSERT( !adaptedType->is_modifiable(), "Shouldn't create adapter for 'modifiable' adaptee type: " << adaptedType );
    ASSERT( *interfaceType != *adaptedType, "Shouldn't create adapter between equivalent types" );
    ASSERT( !( adaptedType->is_empty_derivation() && !adaptedType->get_explicit_declaration() ),
            "Can't derive from implicit empty base type: " << adaptedType );

    auto ifDecl = interfaceType->get_declaration();
    auto scope = ifDecl->get_symbol()->get_outer();
    std::string adapterName = ifDecl->get_unique_name() + "$if$" + encode_type_name( adaptedType->get_declaration() );

    if ( auto existingAdapterSymbol = dynamic_cast<TxEntitySymbol*>( scope->get_member_symbol( adapterName ) ) ) {
        if ( auto typeDecl = existingAdapterSymbol->get_type_decl() ) {
            auto adapterType = typeDecl->get_definer()->resolve_type();
            //std::cerr << "Getting existing interface adapter: " << adapterType << std::endl;
            return adapterType;
        }
    }

    LOG_TRACE( this->LOGGER(), "Creating interface adapter: " << adapterName << "\n\tfrom " << adaptedType << "\n\tto   " << interfaceType );
    // TODO: combine flags from adapted and adaptee types, including TXD_EXPERRBLOCK

    auto & loc = this->get_builtin_location();  // FIXME: add origin (adapter use site) to function signature so that we can assign an appropriate location
    auto adapterTypeNode = new TxAdapterTypeNode( loc, interfaceType, adaptedType );
    auto adapterDeclNode = new TxTypeDeclNode( loc, ( TXD_PUBLIC | TXD_IMPLICIT ), adapterName, nullptr, adapterTypeNode );

    auto & adaptedTypeCtx = adaptedType->get_declaration()->get_definer()->context();
    LexicalContext adapterCtx( ifDecl->get_definer()->context().scope(), adaptedTypeCtx.exp_error(), adaptedTypeCtx.is_generic(),
                               adaptedTypeCtx.reinterpretation_definer() );
    run_declaration_pass( adapterDeclNode, adapterCtx );
    {   // override the adaptee type id virtual field member:
        // TODO: instead pass this as param decl node to adapterDeclNode
        TxDeclarationFlags fieldDeclFlags = TXD_PUBLIC | TXD_STATIC | TXD_OVERRIDE | TXD_IMPLICIT;
        auto fieldDecl = new TxFieldDeclNode( loc, fieldDeclFlags,
                                              new TxFieldDefNode( loc, "$adTypeId", new TxNamedTypeNode( loc, "tx.UInt" ), nullptr ) );
        LexicalContext ctx( adapterCtx, adapterDeclNode->get_declaration()->get_symbol() );
        run_declaration_pass( fieldDecl, ctx );
        fieldDecl->symbol_resolution_pass();
    }
    adapterDeclNode->symbol_resolution_pass();

    auto adapterType = adapterTypeNode->resolve_type();
//    ASSERT(adapterType->adapted_type()->get_type_id() == adaptedType->get_type_id(),
//           "Mismatching type ids between adapter and adaptee: " << adapterType->adapted_type()->get_type_id() << " != " << adaptedType->get_type_id());
    if ( modIf )
        return this->get_modifiable_type( nullptr, adapterType );
    else
        return adapterType;
}
