#include "util/logging.hpp"

#include "tx_error.hpp"
#include "tx_logging.hpp"

#include "type.hpp"
#include "type_registry.hpp"
#include "package.hpp"
#include "symbol_lookup.hpp"

#include "ast/expr/ast_expr_node.hpp"
#include "ast/expr/ast_constexpr.hpp"
#include "ast/expr/ast_conv.hpp"

#include "ast/expr/ast_lambda_node.hpp"
#include "ast/expr/ast_field.hpp"
#include "ast/type/ast_types.hpp"
#include "ast/stmt/ast_stmts.hpp"
#include "ast/ast_wrappers.hpp"
#include "ast/ast_declpass.hpp"


bool DataTupleDefinition::add_interface_fields( const DataTupleDefinition& interfaceFields ) {
    bool added = false;
    for ( auto & f : interfaceFields.fields ) {
        if ( f->get_unique_name() == "$adTypeId" )
            continue;
        if ( !this->has_field( f->get_unique_name() ) ) {
            this->add_field( f );
            added = true;
            //std::cerr << "** adding non-existing interface field " << f->get_unique_name() << std::endl;
        }
        //else
        //    std::cerr << "** not adding existing interface field " << f->get_unique_name() << std::endl;
    }
    return added;
}

void DataTupleDefinition::dump() const {
    unsigned ix = 0;
    for ( auto & f : this->fields ) {
        fprintf( stderr, "%-2d: %20s: %s\n", ix, f->get_unique_name().c_str(), f->str().c_str() );
        ix++;
    }
}


bool is_concrete_uinteger_type( const TxActualType* type ) {
    if ( type->is_builtin() ) {
        return is_builtin_concrete_uinteger_type( (BuiltinTypeId)type->get_runtime_type_id() );
    }
    else {
        return type->is_a( *type->get_declaration()->get_symbol()->get_root_scope()->registry().get_builtin_type( TXBT_INTEGER ) );
    }
}

bool is_concrete_sinteger_type( const TxActualType* type ) {
    if ( type->is_builtin() ) {
        return is_builtin_concrete_sinteger_type( (BuiltinTypeId)type->get_runtime_type_id() );
    }
    else {
        return type->is_a( *type->get_declaration()->get_symbol()->get_root_scope()->registry().get_builtin_type( TXBT_UNSIGNED ) );
    }
}

bool is_concrete_floating_type( const TxActualType* type ) {
    if ( type->is_builtin() ) {
        return is_builtin_concrete_floating_type( (BuiltinTypeId)type->get_runtime_type_id() );
    }
    else {
        return type->is_a( *type->get_declaration()->get_symbol()->get_root_scope()->registry().get_builtin_type( TXBT_FLOATINGPOINT ) );
    }
}



/*=== TxTypeClassHandler implementation ===*/

std::string TxTypeClassHandler::str() const {
    return to_string( this->_typeClass );
}



/*=== TxActualType implementation ===*/

/** Used to ensure proper resetting recursionGuard in type (RAII style). */
class ScopedRecursionGuardClause {
    const TxActualType* type;
public:
    ScopedRecursionGuardClause( const TxActualType* type ) : type( type ) {
        this->type->recursionGuard = true;
    }
    ~ScopedRecursionGuardClause() {
        this->type->recursionGuard = false;
    }
};

Logger& TxActualType::_LOG = Logger::get( "ENTITY" );

const TxNode* TxActualType::get_origin_node() const {
    return this->get_declaration()->get_definer();
}

const TxActualType* TxActualType::get_root_any_qtype() const {
    return this->get_declaration()->get_symbol()->get_root_scope()->registry().get_builtin_type( TXBT_ANY );
}

void TxActualType::examine_members() {
    LOG_TRACE( this->LOGGER(), "Examining members of type " << this->get_declaration()->get_unique_full_name() );

    auto typeDeclNamespace = this->get_declaration()->get_symbol();

    // perform shallow pass on type's member declarations to determine derivation characteristics:
    bool hasImplicitFieldMembers = false;

    for ( auto symname = typeDeclNamespace->decl_order_names_cbegin(); symname != typeDeclNamespace->decl_order_names_cend(); symname++ ) {
        if ( auto entitySym = dynamic_cast<TxEntitySymbol*>( typeDeclNamespace->get_member_symbol( *symname ) ) ) {
            if ( auto typeDecl = entitySym->get_type_decl() ) {
                if ( typeDecl->get_decl_flags() & TXD_GENPARAM ) {
                    typeDecl->get_definer()->resolve_type( TXP_TYPE );  // make sure generic params/bindings are resolved
                    this->params.emplace_back( typeDecl );
                    this->typeGeneric = true;
                }
                else if ( typeDecl->get_decl_flags() & TXD_GENBINDING ) {
                    typeDecl->get_definer()->resolve_type( TXP_TYPE );  // make sure generic params/bindings are resolved
                    this->bindings.emplace_back( typeDecl );
                    hasTypeBindings = true;
                }
                else if ( *symname == "$GenericBase" ) {
                    this->genericBaseType = typeDecl->get_definer()->resolve_type( TXP_TYPE ).type();
                }
            }

            for ( auto fieldDeclI = entitySym->fields_cbegin(); fieldDeclI != entitySym->fields_cend(); fieldDeclI++ ) {
                auto fieldDecl = *fieldDeclI;

                if ( fieldDecl->get_decl_flags() & TXD_GENPARAM ) {
                    fieldDecl->get_definer()->resolve_type( TXP_TYPE );  // make sure generic params/bindings are resolved
                    this->params.emplace_back( fieldDecl );
                    this->valueGeneric = true;
                }
                else if ( fieldDecl->get_decl_flags() & TXD_GENBINDING ) {
                    fieldDecl->get_definer()->resolve_type( TXP_TYPE );  // make sure generic params/bindings are resolved
                    this->bindings.emplace_back( fieldDecl );
                    hasValueBindings = true;
                }
                else if ( fieldDecl->get_decl_flags() & TXD_IMPLICIT )
                    hasImplicitFieldMembers = true;
                else if ( fieldDecl->get_decl_flags() & TXD_EXPERROR )
                    continue;
                else {
                    // Note: initializers currently count as fields, although they are not proper methods
                    hasExplicitFieldMembers = true;
                }

                if ( fieldDecl->get_decl_flags() & ( TXD_CONSTRUCTOR | TXD_INITIALIZER ) ) {
                    //std::cerr << "storage of constr/init: " << fieldDecl << " " << fieldDecl->get_storage() << std::endl;
                    this->constructors.push_back( fieldDecl );
                    continue;
                }

                switch ( fieldDecl->get_storage() ) {
                case TXS_INSTANCE:
                    // Note: VALUE bindings are only declared as instance members in generic base type,
                    // i.e. TXD_GENBINDING refers to a field already declared in a parent type,
                    // so they do not constitute "extensions" to the specialized subtypes.
                    if ( !( fieldDecl->get_decl_flags() & TXD_GENBINDING ) ) {
                        this->modifiesInstanceDatatype = true;
                        if ( !( fieldDecl->get_decl_flags() & TXD_GENPARAM ) )
                            this->instanceFieldsToInitialize.push_back( fieldDecl );
                    }
                    break;
                case TXS_INSTANCEMETHOD:
                case TXS_VIRTUAL:
                    this->modifiesVTable = true;
                    break;
                // (TXS_STATIC are like globals but within a type namespace and don't affect the derivation degree)
                default:
                    break;
                }
            }
        }
    }

    this->pureValueSpec = hasValueBindings && !( hasImplicitFieldMembers || hasExplicitFieldMembers || hasTypeBindings );
}

void TxActualType::initialize_type() {
    LOG_TRACE( this->LOGGER(), "Initializing type " << this );

    ASSERT( !baseType || baseType->hasInitialized, "baseType of " << this->get_declaration() << " has not initialized: " << baseType );
    ASSERT( !genericBaseType || genericBaseType->hasInitialized, "genericBaseType of " << this->get_declaration() << " has not initialized: " << genericBaseType );

    if ( !hasExplicitFieldMembers ) {
        if ( !this->bindings.empty() ) {
            this->pureDerivation = true;
        }
        else if ( !this->builtin && this->interfaces.empty() ) {
            if ( this->typeClass == TXTC_REFERENCE ) {
                // special case, a reference derivation is treated as empty if it has param that is merely redeclaration
                if ( this->params.empty()
                        || ( this->params.size() == 1 && dynamic_cast<const TxTypeDeclaration*>( this->params.front() ) ) ) {
                    this->emptyDerivation = true;
                }
            }
            else if ( this->typeClass != TXTC_FUNCTION && this->typeClass != TXTC_INTERFACEADAPTER ) {
                if ( this->params.empty() ) {
                    this->emptyDerivation = true;
                    ASSERT( !this->genericBaseType, "Empty derivation had a $GenericBase: " << this->genericBaseType );
                }
            }
        }
    }

    // determine datatype change:
    {
        if ( hasValueBindings ) {
            this->modifiesInstanceDatatype = true;
        }
        else if ( this->builtin ) {
            // Built-in implies a distinct instance type compared to the base type.
            this->modifiesInstanceDatatype = true;
        }
        else if ( this->typeClass == TXTC_FUNCTION ) {
            // function type implies a distinct instance type compared to the base type (for now)
            this->modifiesInstanceDatatype = true;
        }

        if ( hasTypeBindings ) {
            // Binding of a base type parameter implies reinterpretation of its members and thus
            // the chance of modified instance / vtable types.
            // Note, may cause false positives (a full graph analysis of contained members would be needed for full accuracy)
            // TODO: bindings for ref-constrained type parameters might not cause modified instance / vtable types.
            this->modifiesInstanceDatatype = true;
            this->modifiesVTable = true;
        }
        else if ( this->typeClass == TXTC_INTERFACEADAPTER ) {
            this->modifiesVTable = true;
        }
        else if ( !this->interfaces.empty() ) {
            // If there are interfaces we assume that will cause the vtable will be extended in preparation.
            // This may cause false positives, but we need to determine this flag in the type's initialization phase.
            this->modifiesVTable = true;
        }
    }

    this->hasInitialized = true;
}

void TxActualType::set_type_class( const TxTypeClassHandler* typeClassHandler ) {
    this->typeClassHandler = typeClassHandler;
    this->typeClass = typeClassHandler->type_class();
    this->initialize_type();
}

void TxActualType::integrate() {
    if ( !this->hasIntegrated ) {
        LOG_TRACE( this->LOGGER(), "Integrating type " << this );
        // connect with super types:
        if ( !this->baseType && this->baseTypeNode ) {
            this->baseType = const_cast<TxTypeExpressionNode*>(this->baseTypeNode)->resolve_type( TXP_RESOLUTION ).type();
            std::transform( this->interfaceNodes.cbegin(), this->interfaceNodes.cend(), std::back_inserter( this->interfaces ),
                            []( const TxTypeExpressionNode* n ) {
                                auto t = const_cast<TxTypeExpressionNode*>(n)->resolve_type( TXP_RESOLUTION ).type();
                                return t;
                            } );
        }

        // integrate super types:
        if ( this->genericBaseType )
            const_cast<TxActualType*>( this->genericBaseType )->integrate();
        if ( this->baseType )
            const_cast<TxActualType*>( this->baseType )->integrate();
        for ( auto interface : this->interfaces ) {
            const_cast<TxActualType*>( interface )->integrate();
        }

        // initialize this type:
        if ( !this->typeClassHandler ) {
            this->set_type_class( this->baseType->type_class_handler() );
        }
        ASSERT( this->hasInitialized, "Integrated but not initialized: " << this->get_declaration() );
        this->hasIntegrated = true;  // (setting it here allows params/bindings to refer back and integrate this type)

        // integrate type parameters, bindings:
        for ( auto paramDecl : this->params ) {
            const_cast<TxActualType*>( paramDecl->get_definer()->resolve_type( TXP_RESOLUTION ).type() )->integrate();
        }
        for ( auto bindingDecl : this->bindings ) {
            const_cast<TxActualType*>( bindingDecl->get_definer()->resolve_type( TXP_RESOLUTION ).type() )->integrate();
        }

        this->autogenerate_constructors();

        this->validate_type();
    }
}

void TxActualType::validate_type() const {
    //std::cerr << "validating type " << this << std::endl;
//    bool expErrWholeType = ( ( this->get_declaration()->get_decl_flags() & TXD_EXPERRBLOCK )
//                             || this->get_declaration()->get_definer()->exp_err_ctx() );

    if ( this->baseType ) {
        if ( this->is_mutable() && !this->get_semantic_base_type()->is_mutable() )
            CERROR( this, "Can't derive a mutable type from an immutable base type: " << this->get_semantic_base_type() );

        if ( this->baseType->runtimeTypeId == TXBT_ANY
             && !( this->builtin || ( this->get_declaration()->get_decl_flags() & TXD_GENPARAM )
                   || this->get_type_class() == TXTC_REFERENCE || this->get_type_class() == TXTC_ARRAY ) )
            CERROR( this, "Can't derive directly from the Any root type: " << this->get_declaration() );

        if ( this->typeClass == TXTC_REFERENCE
                && !( this->runtimeTypeId == TXBT_REFERENCE
                      || ( this->genericBaseType && this->genericBaseType->runtimeTypeId == TXBT_REFERENCE ) ) ) {
            // a user-defined derivation of Ref (or a specialization thereof)
            if ( !( this->pureDerivation || this->emptyDerivation ) )
                CERROR( this, "Can't derive non-empty subtype from the Ref type: " << this->get_declaration() );
        }

        ASSERT( this->baseType->runtimeTypeId == TXBT_ANY
                || ( this->get_type_class() == TXTC_INTERFACEADAPTER && this->baseType->get_type_class() == TXTC_INTERFACE )
                || this->get_type_class() == this->baseType->get_type_class(),
                "Specialized type's type class " << this << " not valid with base type's type class " << this->baseType->get_type_class() );
//        if (this->dataspace && this->baseType->get_type_class() != TXTC_REFERENCE)
//            CERROR(this, "Specified dataspace for non-reference base type " << this->baseType);

        if ( this->baseType->is_empty_derivation() && !this->baseType->is_explicit_declaration() ) {
            ASSERT( !( this->is_empty_derivation() && !this->is_explicit_declaration() ),
                    "anonymous or implicit, empty types may not be derived except as another anonymous or implicit, empty type: " << this );
        }

        // Verify that all parameters of base type are bound:
        // Note: The base type's parameters that have not been bound should be automatically redeclared by the type registry,
        //       unless this type (erroneously) derives from a generic type as were it non-generic.
        if ( !this->is_generic_param() ) {
            for ( auto & paramDecl : this->get_semantic_base_type()->get_type_params() ) {
                if ( dynamic_cast<const TxTypeDeclaration*>( paramDecl ) ) {
                    if ( !this->get_binding( paramDecl->get_unique_name() ) ) {
                        CERROR( this, "Missing binding of base type's non-ref TYPE parameter "
                                << paramDecl->get_unique_name() << " in " << this );
                    }
                }
            }
        }

        // validate the type parameter bindings
        for ( auto & bindingDecl : this->bindings ) {
            auto pname = bindingDecl->get_unique_name();
            if ( auto paramDecl = this->genericBaseType->get_type_param_decl( pname ) ) {
                if ( dynamic_cast<const TxTypeDeclaration*>( paramDecl ) ) {
                    if ( !dynamic_cast<const TxTypeDeclaration*>( bindingDecl ) )
                        CERROR( bindingDecl->get_definer(), "Binding for type parameter " << paramDecl << " is not a type: " << bindingDecl );
                }
                else {
                    if ( !dynamic_cast<const TxFieldDeclaration*>( bindingDecl ) )
                        CERROR( bindingDecl->get_definer(), "Binding for type parameter " << paramDecl << " is not a field/value: " << bindingDecl );
                }
            }
            else
                CERROR( bindingDecl->get_definer(),
                        "No type parameter of " << genericBaseType << " matches provided binding " << bindingDecl->get_unique_name() );
        }
    }
    // TODO: validate interfaces
    // check that generic interfaces can't be implemented multiple times throughout a type hierarchy,
    // unless their type arguments are exactly the same
    for ( auto & interf : this->interfaces ) {
        if ( interf->get_type_class() != TXTC_INTERFACE )
            CERROR( this, "Only the first derived-from type can be a non-interface type: " << interf );
        else {
            if ( interf->is_generic() )
                CERROR( this, "Can't implement a generic interface (with unbound type parameters): " << interf );
        }
    }
}

/*
static TxFieldDeclNode* generate_constructor_ast( const TxLocation& loc, const TxFieldDeclaration* baseConstructor,
                                                  std::vector<const TxFieldDeclaration*>& initializerArgs,
                                                  const std::string& initializerName ) {
    auto argNodes = new std::vector<TxArgTypeDefNode*>();
    auto superInitArgs = new std::vector<TxExpressionNode*>();
    auto statements = new std::vector<TxStatementNode*>();

    if ( baseConstructor ) {
        ASSERT( dynamic_cast<TxLambdaExprNode*>( baseConstructor->get_definer()->initExpression->originalExpr ),
                "Not lambda node: " << baseConstructor->get_definer()->initExpression->originalExpr );
        auto baseConstrLambdaNode = static_cast<TxLambdaExprNode*>( baseConstructor->get_definer()->initExpression->originalExpr );
        for ( auto baseArgDeclNode : *baseConstrLambdaNode->funcHeaderNode->arguments ) {
            auto argName = "sup$" + baseArgDeclNode->fieldName->ident();
            auto argType = new TxTypeExprWrapperNode( baseArgDeclNode );
            argNodes->push_back( new TxArgTypeDefNode( loc, argName, argType ) );

            superInitArgs->push_back( new TxFieldValueNode( loc, argName ) );
        }
        auto superInitCall = new TxFunctionCallNode( loc, new TxFieldValueNode( loc, "$super" ), superInitArgs );
        statements->push_back( new TxCallStmtNode( loc, superInitCall ) );
    }

    for ( auto initFieldDecl : initializerArgs ) {
        auto argName = initFieldDecl->get_unique_name();
        auto assigneeNode = new TxFieldAssigneeNode(
                loc, new TxFieldValueNode( loc, new TxFieldValueNode( loc, "$self" ),
                                           new TxIdentifierNode( loc, argName ) ) );

        if ( initFieldDecl->get_definer()->initExpression ) {
            // direct assignment (instance field has a constant initializer expression)
            statements->push_back( new TxAssignStmtNode( loc, assigneeNode,
                                                          new TxExprWrapperNode( initFieldDecl->get_definer()->initExpression ) ) );
        }
        else {
            auto argType = new TxTypeExprWrapperNode( initFieldDecl->get_definer() );
            argNodes->push_back( new TxArgTypeDefNode( loc, argName, argType ) );
            statements->push_back( new TxAssignStmtNode( loc, assigneeNode,
                                                          new TxFieldValueNode( loc, argName ) ) );
        }
    }

    auto constrType = new TxFunctionTypeNode( loc, false, argNodes, nullptr );
    auto lambdaExpr = new TxLambdaExprNode( loc, constrType, new TxSuiteNode( loc, statements ), true );
    auto constrDecl = new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_IMPLICIT | TXD_CONSTRUCTOR,
                                           new TxNonLocalFieldDefNode( loc, new TxIdentifierNode( loc, initializerName ),
                                                                       lambdaExpr, false ),
                                           true );  // method syntax since constructor
    return constrDecl;
}
*/

/** Creates an AST sub-tree that declares and defines an implicit constructor for the given base type constructor
 * and instance fields to initialize.
 */
static TxFieldDeclNode* generate_constructor_ast( const TxLocation& loc, const TxFieldDeclaration* baseConstructor,
                                                  std::vector<const TxFieldDeclaration*>& instanceFieldsToInitialize ) {
    auto argNodes = new std::vector<TxArgTypeDefNode*>();
    auto superInitArgs = new std::vector<TxExpressionNode*>();
    auto initClauseList = new std::vector<TxMemberInitNode*>();

    if ( baseConstructor ) {
        ASSERT( dynamic_cast<TxLambdaExprNode*>( baseConstructor->get_definer()->initExpression->originalExpr ),
                "Not lambda node: " << baseConstructor->get_definer()->initExpression->originalExpr );
        auto baseConstrLambdaNode = static_cast<TxLambdaExprNode*>( baseConstructor->get_definer()->initExpression->originalExpr );
        for ( auto baseArgDeclNode : *baseConstrLambdaNode->funcHeaderNode->arguments ) {
            auto argName = "sup$" + baseArgDeclNode->fieldName->ident();
            auto argType = new TxTypeExprWrapperNode( baseArgDeclNode );
            argNodes->push_back( new TxArgTypeDefNode( loc, argName, argType ) );

            superInitArgs->push_back( new TxNamedFieldNode( loc, argName ) );
        }
        initClauseList->push_back( new TxMemberInitNode( loc, new TxIdentifierNode( loc, "super" ), superInitArgs ) );
    }

    for ( auto instanceFieldDecl : instanceFieldsToInitialize ) {
        auto argName = instanceFieldDecl->get_unique_name();

        if ( instanceFieldDecl->get_definer()->initExpression ) {
            // direct assignment (instance field has a constant initializer expression)
            // TxInitStmtNode inserts these (same logic as for user-defined constructors)
//            auto initExpr = new TxExprWrapperNode( instanceFieldDecl->get_definer()->initExpression );
//            initClauseList->push_back( new TxMemberInitNode( loc, new TxIdentifierNode( loc, argName ),
//                                                             new std::vector<TxExpressionNode*>( { initExpr } ) ) );
        }
        else {
            argNodes->push_back( new TxArgTypeDefNode( loc, argName,
                                                       new TxTypeExprWrapperNode( instanceFieldDecl->get_definer() ) ) );
            initClauseList->push_back( new TxMemberInitNode( loc, new TxIdentifierNode( loc, argName ),
                                                             new std::vector<TxExpressionNode*>(
                                                                     { new TxNamedFieldNode( loc, argName ) } ) ) );
        }
    }

    auto constrBody = new TxSuiteNode( loc, new std::vector<TxStatementNode*>( { new TxInitStmtNode( loc, initClauseList ) } ) );
    auto constrType = new TxFunctionTypeNode( loc, false, argNodes, nullptr );
    auto lambdaExpr = new TxLambdaExprNode( loc, constrType, constrBody, true );
    auto constrDecl = new TxFieldDeclNode( loc, TXD_PUBLIC | TXD_IMPLICIT | TXD_CONSTRUCTOR,
                                           new TxNonLocalFieldDefNode( loc, new TxIdentifierNode( loc, CONSTR_IDENT ),
                                                                       lambdaExpr, false ),
                                           true );  // method syntax since constructor
    return constrDecl;
}

bool TxActualType::is_construction_type() const {
    return !( this->constructors.empty()
              && ( this->is_same_instance_type() || this->is_pure_value_specialization() ) );
}

const TxActualType* TxActualType::get_construction_type() const {
    ASSERT( this->hasIntegrated, "Can't get construction type of unintegrated type " << this->get_declaration() );
    auto allocType = this;
    while ( !allocType->is_construction_type() )
        allocType = allocType->get_base_type();
    return allocType;
}

void TxActualType::autogenerate_constructors() {
    const TxLocation& loc = this->get_declaration()->get_definer()->ploc;

    // If this type has no explicit constructors,
    // and all its instance fields that lack a direct initializer have a copy constructor,
    // auto-generate constructors that takes a copy-by-value initializer for each parent
    // constructor argument and current type instance member.

    if ( this->get_type_class() != TXTC_TUPLE
         && this->get_type_class() != TXTC_ARRAY
         && this->get_type_class() != TXTC_ELEMENTARY ) {
        return;
    }
    if ( !this->is_construction_type() )
        return;
    if ( !this->constructors.empty() )
        return;

    for ( auto instanceFieldDecl : instanceFieldsToInitialize ) {
        if ( !instanceFieldDecl->get_definer()->initExpression ) {
            auto fieldType = instanceFieldDecl->get_definer()->resolve_type( TXP_TYPE );
            if ( !fieldType->is_generic_param() ) {
                // check if instance field has a constructor that accepts a single argument of its own type
                bool hasCopyConstructor = false;
                auto fieldConstrType = fieldType->get_construction_type();
                for ( auto fieldConstr : fieldConstrType->constructors ) {
                    auto constrType = fieldConstr->get_definer()->resolve_type( TXP_TYPE );
                    if ( constrType->argument_types().size() == 1 ) {
                        // FUTURE: Perhaps accept copy constructors that take reference to own type
                        //         (probably requires dataspaces design to be detailed first)
                        if ( *constrType->argument_types().at(0) == *fieldConstrType ) {
                            hasCopyConstructor = true;  // this is a copy constructor
                            break;
                        }
                    }
                }
                if ( !hasCopyConstructor ) {
                    // can't auto-generate constructor
                    LOG_NOTE( this->LOGGER(), "Can't auto-generate constructor since field type has no copy constructor: "
                              << instanceFieldDecl->get_unique_full_name() << " : " << fieldType );
                    return;
                }
            }
        }
    }

    auto baseConstrType = this->get_base_type()->get_construction_type();
    if ( baseConstrType->constructors.empty() ) {
        if ( this->instanceFieldsToInitialize.empty() && this->is_abstract() ) {
            // skips some built-in abstract types (e.g. Scalar)
            //std::cerr << "SKIPPING " << this << std::endl;
            return;
        }
        auto constrDecl = generate_constructor_ast( loc, nullptr, this->instanceFieldsToInitialize );
        this->implicitConstructorNodes.push_back( constrDecl );
    }
    else {
        for ( auto baseConstructor : baseConstrType->constructors ) {
            auto constrDecl = generate_constructor_ast( loc, baseConstructor, this->instanceFieldsToInitialize );
            this->implicitConstructorNodes.push_back( constrDecl );
        }
    }

    for ( auto implConstructor : this->implicitConstructorNodes ) {
        run_declaration_pass( implConstructor, this->get_declaration()->get_definer(), "impl-constr" );
        this->constructors.push_back( implConstructor->get_declaration() );
    }
}

bool TxActualType::prepare_members() {
    if ( !this->hasPrepared ) {
        if ( this->startedPrepare ) {
            return true;
        }
        this->startedPrepare = true;
        bool rec = this->inner_prepare_members();
        this->hasPrepared = true;
        return rec;
    }
    return false;
}

bool TxActualType::inner_prepare_members() {
    LOG_TRACE( this->LOGGER(), "Preparing members of type " << this );
    bool recursionError = false;

    bool expErrWholeType = ( ( this->get_declaration()->get_decl_flags() & TXD_EXPERROR )
                             || this->get_declaration()->get_definer()->exp_err_ctx() );

    if ( !this->is_integrated() ) {
        if ( expErrWholeType )
            LOG_TRACE( this->LOGGER(), "Skipping preparation of EXPERR unintegrated type " << this );
        else
            LOG( this->LOGGER(), ERROR, "Skipping preparation of unintegrated type (without EXPERR context): " << this );
        return false;
    }

    // copy base type's virtual and instance field tuples (to which fields may be added / overridden):
    auto baseType = this->get_base_type();
    if ( baseType ) {
        //ASSERT(baseType->is_prepared(), "Base type " << baseType << " not prepared before sub type " << this);
        recursionError = const_cast<TxActualType*>( baseType )->prepare_members();
        this->virtualFields = baseType->virtualFields;
        this->instanceFields = baseType->instanceFields;
    }
    for ( auto & interf : this->interfaces ) {
        //ASSERT(interfSpec.type->is_prepared(), "Base i/f " << interfSpec.type << " not prepared before sub type " << this);
        recursionError |= const_cast<TxActualType*>( interf )->prepare_members();
        bool added = this->virtualFields.add_interface_fields( interf->virtualFields );
        if ( !added && !expErrWholeType )
            LOG_DEBUG( this->LOGGER(), "Type implements interface " << interf << " which doesn't cause the vtable to be extended: " << this );
//        if (added)
//            this->modifiesVTable = true;
    }
    //std::cerr << "Inherited virtual fields of " << this << std::endl;
    //this->virtualFields.dump();

    auto semBaseType = this->get_semantic_base_type();

    // for all the member names declared or redeclared in this type:
    auto typeDeclNamespace = this->get_declaration()->get_symbol();
    for ( auto symname = typeDeclNamespace->decl_order_names_cbegin(); symname != typeDeclNamespace->decl_order_names_cend(); symname++ ) {
        // this drives resolution of all this type's members

        auto entitySym = dynamic_cast<TxEntitySymbol*>( typeDeclNamespace->get_member_symbol( *symname ) );
        if ( !entitySym )
            continue;

        // prepare type members:
        if ( auto typeDecl = entitySym->get_type_decl() ) {
            if ( typeDecl->get_decl_flags() & TXD_GENBINDING ) {
                if ( auto paramDecl = semBaseType->get_type_param_decl( typeDecl->get_unique_name() ) ) {
                    auto constraintType = paramDecl->get_definer()->qtype();
                    //std::cerr << this << ": Constraint type for param " << paramDecl << ": " << "checking bound type "
                    //          << boundType << "\t against constraint type " << constraintType << std::endl;
                    auto qtype = typeDecl->get_definer()->qtype();
                    if ( !qtype->is_a( *constraintType ) ) {
                        // TODO: do this also for VALUE params, but array type expression needs auto-conversion support for that to work
                        CERROR( typeDecl->get_definer(),
                                "Bound type for type parameter " << paramDecl->get_unique_full_name() << ": " << qtype->str(false)
                                << std::endl << "  is not a derivation of contraint type: " << constraintType->str(false) );
                    }
                    if ( this->get_type_class() == TXTC_ARRAY ) {
                        if ( this->is_mutable() && !this->is_type_generic_dependent() ) {
                            if ( !qtype.is_modifiable() ) {
                                CERROR( typeDecl->get_definer(), "Inconsistent mutable array type: Element type not declared modifiable: " << qtype );
                            }
                        }
                    }
                }
            }
        }

        // prepare field members:
        for ( auto fieldDeclI = entitySym->fields_cbegin(); fieldDeclI != entitySym->fields_cend(); fieldDeclI++ ) {
            auto fieldDecl = *fieldDeclI;
            auto field = fieldDecl->get_definer()->attempt_field();

            bool expErrField = ( fieldDecl->get_decl_flags() & TXD_EXPERROR );
            if ( !field ) {
                if ( expErrField || expErrWholeType )
                    LOG_TRACE( this->LOGGER(), "Skipping preparation of EXPERR unresolved field " << fieldDecl );
                else
                    LOG( this->LOGGER(), ERROR, "Skipping preparation of unresolved field (without EXPERR context): " << fieldDecl );
                continue;
            }

            auto fieldType = field->qtype();

            // validate field's storage and declaration flags, and do layout:
            switch ( fieldDecl->get_storage() ) {
            case TXS_INSTANCE:
                LOG_DEBUG( this->LOGGER(), "Laying out instance field " << field << "  " << this->instanceFields.get_field_count() );
                if ( fieldDecl->get_decl_flags() & TXD_ABSTRACT ) {
                    CERROR( field, "Can't declare an instance field abstract: " << field );
                }
                if ( this->get_type_class() != TXTC_TUPLE ) {
                    if ( !( fieldDecl->get_decl_flags() & ( TXD_GENPARAM | TXD_GENBINDING | TXD_IMPLICIT ) ) )
                        CERROR( field, "Can't declare instance member in non-tuple type: " << field );
                }

                // recursively prepare instance member fields' types so that we identify recursive data type definitions:
                //std::cerr << "Recursing into " << field << "  of type " << field->get_type() << std::endl;
                if ( const_cast<TxActualType*>( fieldType.type() )->prepare_members() )
                    CERROR( field, "Recursive data type via field " << field->get_declaration()->get_unique_full_name() );

                else if ( !( !expErrField || expErrWholeType ) )
                    LOG_DEBUG( this->LOGGER(), "Skipping layout of exp-error instance field: " << field );

                else if ( fieldDecl->get_decl_flags() & TXD_GENBINDING ) {
                    // If this is a reinterpretation of the original generic base type, define the field;
                    // if this is a derivation of another specialization, override the field.
                    if ( this->instanceFields.has_field( field->get_unique_name() ) ) {
                        ASSERT( this->instanceFields.get_field( field->get_unique_name() )->get_decl_flags() & TXD_GENPARAM,
                                "Previous instance field entry is not a GENPARAM: " << this->instanceFields.get_field( field->get_unique_name() ) );
                        this->instanceFields.override_field( field );
                    }
                    else {
                        this->instanceFields.add_field( field );
                    }
                }
                else
                    this->instanceFields.add_field( field );

                if ( this->virtualFields.has_field( field->get_unique_name() ) )
                    CERROR( field, "Field declared non-virtual but shadows virtual parent field: " << field );
                break;
            case TXS_VIRTUAL:
            case TXS_INSTANCEMETHOD:
                ASSERT( !( fieldDecl->get_decl_flags() & TXD_INITIALIZER ), "initializers can't be virtual/instance method: " << fieldDecl );
                if ( fieldDecl->get_decl_flags() & TXD_CONSTRUCTOR )
                    break;  // skip, constructors aren't virtual

                if ( fieldDecl->get_decl_flags() & TXD_ABSTRACT ) {
                    if ( this->get_type_class() != TXTC_INTERFACE && !( this->get_declaration()->get_decl_flags() & TXD_ABSTRACT ) )
                        CERROR( fieldDecl->get_definer(),
                                "Can't declare abstract member '" << fieldDecl->get_unique_name() << "' in type that is not declared abstract: " << this );
                }

                if ( entitySym->is_overloaded() )
                    CERROR( field, "Overloading of virtual fields/methods not yet supported: " << field );

                //std::cerr << "virtual/instancemethod: " << field->get_unique_name() << std::endl;
                if ( this->virtualFields.has_field( field->get_unique_name() ) ) {
                    if ( !( fieldDecl->get_decl_flags() & TXD_OVERRIDE ) )
                        CERROR( field, "Field overrides but isn't declared 'override': " << field );
                    auto overriddenField = this->virtualFields.get_field( field->get_unique_name() );
                    if ( overriddenField->get_decl_flags() & TXD_FINAL )
                        CERROR( field, "Can't override a base type field that is declared 'final': " << field );
                    if ( overriddenField->get_decl_flags() & TXD_VIRTUAL
                         && !( fieldDecl->get_decl_flags() & TXD_VIRTUAL ) )
                        CERROR( field, "Field overrides a virtual field but is not declared 'virtual': " << field );
                    if ( !( field->qtype()->is_assignable_to( *overriddenField->qtype() ) ) )
                        CERROR( field, "Overriding member's type " << field->qtype() << std::endl
                                << "   not assignable to overridden member's type " << overriddenField->qtype() );
                    if ( !expErrField || expErrWholeType )
                        this->virtualFields.override_field( field );
                }
                else if ( this->instanceFields.has_field( field->get_unique_name() ) ) {
                    CERROR( field, "Field declared virtual but shadows non-virtual parent field: " << field );
                }
                else {
                    if ( ( fieldDecl->get_decl_flags() & ( TXD_OVERRIDE | TXD_BUILTIN ) ) == TXD_OVERRIDE )  // (suppressed for built-ins)
                        CERROR( field, "Field doesn't override but is declared 'override': " << field );
                    if ( !expErrField || expErrWholeType )
                        this->virtualFields.add_field( field );
                }
                this->LOGGER()->debug( "Adding/overriding virtual field %-40s  %s  %u", field->str().c_str(),
                                       field->qtype()->str().c_str(),
                                       this->virtualFields.get_field_count() );
                break;
            default:
                ASSERT( fieldDecl->get_storage() == TXS_STATIC,
                        "Invalid storage class " << fieldDecl->get_storage() << " for field member " << *field );
                if ( fieldDecl->get_decl_flags() & TXD_INITIALIZER )
                    break;  // skip, initializers are inlined and not actually added as static functions

                if ( fieldDecl->get_decl_flags() & TXD_ABSTRACT )
                    CERROR( field, "Can't declare a non-virtual field as abstract: " << field );
                if ( ( fieldDecl->get_decl_flags() & ( TXD_OVERRIDE | TXD_BUILTIN ) ) == TXD_OVERRIDE )  // (suppressed for built-ins)
                    CERROR( field, "Field doesn't override but is declared 'override': " << field );
                if ( !expErrField || expErrWholeType )
                    this->staticFields.add_field( field );
            }
        }
    }

    // (note, this condition is not the same as is_concrete())
    if ( !this->is_abstract() && this->get_type_class() != TXTC_INTERFACEADAPTER && !this->is_generic_param() ) {
        // check that all abstract members of base types & interfaces are implemented:
        auto virtualFields = this->get_virtual_fields();
        for ( auto & field : virtualFields.fieldMap ) {
            auto actualFieldEnt = virtualFields.get_field( field.second );
            if ( actualFieldEnt->get_decl_flags() & TXD_ABSTRACT ) {
                CERROR( this, "Concrete type " << this->str() << " doesn't implement abstract member " << actualFieldEnt );
            }
        }
    }

    return recursionError;
}

/** Returns true if this type has one or more (unbound) parameters that are not constrained to be a Ref type. */
static bool has_nonref_params( const TxActualType* type, bool allowValueParams ) {
    for ( auto & paramDecl : type->get_type_params() ) {
        if ( auto paramTypeDecl = dynamic_cast<const TxTypeDeclaration*>( paramDecl ) ) {
            auto constraintType = paramTypeDecl->get_definer()->resolve_type( TXP_RESOLUTION );
            ASSERT( constraintType, "NULL constraint type for param " << paramDecl << " of " << type );
            if ( constraintType->get_type_class() != TXTC_REFERENCE )
                return true;
        }
        else if ( !allowValueParams ) {
            return true;
            // Note, while unbound VALUE parameters can affect data type size, they do not necessarily affect code generation
        }
    }
    return false;
}

/** Returns true if this type is defined within an outer scope that has one or more (unbound) TYPE parameters
 * that are not constrained to be a Ref type. */
static bool has_outer_with_nonref_params( const TxActualType* type ) {
    TxScopeSymbol* scope = type->get_declaration()->get_symbol()->get_outer();
    while ( !dynamic_cast<TxModule*>( scope ) ) {
        if ( auto entitySymbol = dynamic_cast<TxEntitySymbol*>( scope ) ) {
            type = entitySymbol->get_type_decl()->get_definer()->qtype().type();
            if ( has_nonref_params( type, true ) )
                return true;
        }
        scope = scope->get_outer();
    }
    return false;
}

/** Returns true if this type is dependent on a VALUE type parameter binding with a dynamic value (not known at compile time),
 * either directly (one of its own bindings), or via a TYPE binding that is dynamic. */
static bool is_dynamic_binding_dependent( const TxActualType* type ) {
    for ( auto b : type->get_bindings() ) {
        if ( auto f = dynamic_cast<const TxFieldDeclaration*>( b ) ) {
            if ( auto initExpr = f->get_definer()->get_init_expression() ) {
                if ( initExpr->is_statically_constant() )
                    continue;
            }
            return true;
        }
        else {  // const TxTypeDeclaration*
            // a bound TYPE type parameter is always concrete (unless this is declared within a generic outer scope), but may be dynamic
            if ( is_dynamic_binding_dependent( static_cast<const TxTypeDeclaration*>( b )->get_definer()->resolve_type( TXP_RESOLUTION ).type() ) )
                return true;
        }
    }
    return false;
}

static const TxActualType* isa_concrete_type( const TxActualType* type ) {
    if ( type->is_abstract() )
        return nullptr;
    if ( has_nonref_params( type, false ) )
        return nullptr;  // if only Ref-constrained parameters, then being generic doesn't cause it to be non-concrete
    while ( type->is_equivalent_derivation() && !type->is_explicit_declaration() ) {
        type = type->get_base_type();
        if ( type->is_abstract() )
            return nullptr;
        if ( has_nonref_params( type, false ) )
            return nullptr;  // if only Ref-constrained parameters, then being generic doesn't cause it to be non-concrete
    }

    // TODO: has_outer_with_nonref_params() doesn't work for implicit partial specializations, e.g. in genericstest.tx:
    //       tx.Array<$,10>2 <10> : tx.Array<$>3 {C} <my.NBArray.E> : tx.Array {E,C}
    //       Their scope is the original generic type's scope, not the scope where they're construed.
    return ( !has_outer_with_nonref_params( type ) ? type : nullptr );
    //return ( !type->is_type_generic_dependent() ? type : nullptr );
}

bool TxActualType::is_concrete() const {
    if ( this->get_type_class() == TXTC_REFERENCE )
        return true;
    return isa_concrete_type( this );
}

bool TxActualType::is_static() const {
    if ( this->get_type_class() == TXTC_REFERENCE )
        return true;
    if ( auto type = isa_concrete_type( this) )
        return !is_dynamic_binding_dependent( type );
    else
        return false;
}

bool TxActualType::is_dynamic() const {
    if ( this->get_type_class() == TXTC_REFERENCE )
        return false;
    if ( auto type = isa_concrete_type( this) )
        return is_dynamic_binding_dependent( type );
    else
        return false;
}

bool TxActualType::is_type_generic_dependent() const {
    if ( this->typeGeneric )
        return true;

    if ( this->is_generic_param() )
        return true;

    if ( this->get_declaration()->get_definer()->context().is_type_generic() )
        // Note: This identities whether context (outer scope) is an original generic type declaration,
        //       but not whether outer scope is a specialization whose bindings are generic-dependent.
        return true;

    if ( this->get_declaration()->get_definer()->context().is_type_gen_dep_bindings() )
        return true;

    return false;
}

bool TxActualType::is_value_generic_dependent() const {
    if ( this->valueGeneric )
        return true;

    if ( this->is_generic_param() )
        return true;

    if ( this->get_declaration()->get_definer()->context().is_value_generic() )
        // Note: This identities whether context (outer scope) is an original generic type declaration,
        //       but not whether outer scope is a specialization whose bindings are generic-dependent.
        return true;

    if ( this->get_declaration()->get_definer()->context().is_value_gen_dep_bindings() )
        return true;

    return false;
}

/*
bool TxActualType::is_type_generic_dependent() const {
    if ( this->recursionGuard ) {
        //std::cerr << "Recursion guard triggered in is_type_generic_dependent() of " << this << std::endl;
        return false;
    }
    ScopedRecursionGuardClause recursionGuard( this );

    // Although different reference specializations can share code gen, here we need to determine whether
    // this type has any dependencies on type parameters, including references whose parameter is bound
    // to another type parameter (from an outer scope).
//    if ( this->get_type_class() == TXTC_REFERENCE )
//        return false;

    if ( this->is_generic_param() )
        return true;

    const TxActualType* type = this;
    if ( type->get_type_class() != TXTC_REFERENCE ) {
        if ( type->get_type_class() == TXTC_INTERFACEADAPTER ) {
            if ( static_cast<const TxInterfaceAdapterType*>( type )->adapted_type()->is_type_generic_dependent() )
                return true;
        }

        if ( type->is_type_generic() )
            return true;
        while ( type->is_equivalent_derivation() ) {
            type = type->get_base_type();
            if ( type->is_type_generic() )
                return true;
        }
    }

    if ( type->get_declaration()->get_definer()->context().is_type_generic() ) {
        // Note: This identities whether context (outer scope) is an original generic type declaration,
        //       but not whether outer scope is a specialization whose bindings are generic-dependent.
        return true;
    }

    for ( TxScopeSymbol* outerScope = type->get_declaration()->get_symbol()->get_outer();
            !dynamic_cast<TxModule*>( outerScope );
            outerScope = outerScope->get_outer() ) {
        if ( auto entitySymbol = dynamic_cast<TxEntitySymbol*>( outerScope ) ) {
            auto outerType = entitySymbol->get_type_decl()->get_definer()->qtype();
            if ( outerType->is_type_generic_dependent() ) {
                //if ( type->get_declaration()->get_unique_full_name().find( "tx.M$Array<$>1.ArrayIterator" ) != std::string::npos )
                //    std::cerr << "Outer scope of type " << this << " is generic-dependent type " << outerType << std::endl;
                return true;
            }
        }
    }

    while ( type->is_generic_specialization() ) {
        // a type that specializes a generic base type does not define new members - testing the non-ref bindings is sufficient
//        if ( type->nonRefBindings ) {
            for ( auto bdecl : type->get_bindings() ) {
                if ( auto typebdecl = dynamic_cast<const TxTypeDeclaration*>( bdecl ) ) {
//                    auto pname = bdecl->get_unique_name();
//                    auto paramDecl = type->genericBaseType->get_type_param_decl( pname );
//                    auto constraintType = paramDecl->get_definer()->resolve_type( passInfo );
                    if ( //constraintType->get_type_class() != TXTC_REFERENCE &&
                         typebdecl->get_definer()->qtype()->is_type_generic_dependent() )
                        return true;
                }
                else {
                    // Note: Don't currently know how to determine whether the bound value is generic-dependent
                    //       (parse the expression?)
                }
//            }
        }

        // examine whether base type (not "generic base type") has generic-dependent bindings,
        // this catches certain specializations done from a generic-dependent context not included in context().is_generic()
        type = type->get_base_type();
    }

    return false;
}
*/

bool TxActualType::is_empty_derivation() const {
    ASSERT( this->hasInitialized, "Can't determine derivation characteristics of uninitialized type " << this->get_declaration() );
    return this->emptyDerivation;
}

bool TxActualType::is_equivalent_derivation() const {
    return this->is_same_vtable_type() && this->is_same_instance_type();
}

bool TxActualType::is_virtual_derivation() const {
    return this->is_same_instance_type();
}

bool TxActualType::is_leaf_derivation() const {
    ASSERT( this->hasInitialized, "Can't determine derivation characteristics of uninitialized type " << this->get_declaration() );
    switch ( this->typeClass ) {
    case TXTC_ANY:
        return false;

    case TXTC_ELEMENTARY:
        switch ( this->runtimeTypeId ) {
        case TXBT_BYTE:
        case TXBT_SHORT:
        case TXBT_INT:
        case TXBT_LONG:
        case TXBT_UBYTE:
        case TXBT_USHORT:
        case TXBT_UINT:
        case TXBT_ULONG:
        case TXBT_HALF:
        case TXBT_FLOAT:
        case TXBT_DOUBLE:
        case TXBT_BOOL:
            return true;
        default:
            return false;
        }
        return false;

    case TXTC_REFERENCE:
        return !this->is_generic();

    case TXTC_ARRAY:
        return !this->is_generic();  // TODO: if user extends array, or if value-specializations share code, this is inaccurate

    case TXTC_TUPLE:
        return this->is_final();  // TODO: identify types that simply don't have any subtypes declared via Registry

    case TXTC_FUNCTION:
        return !this->is_builtin();

    case TXTC_INTERFACE:
        return false;
    case TXTC_INTERFACEADAPTER:
        return true;
    case TXTC_VOID:
        return true;
    default:
        THROW_LOGIC( "Undefined type class: " << this->typeClass );
    }
}


bool TxActualType::is_scalar() const {
    ASSERT( this->hasInitialized, "Can't determine derivation characteristics of uninitialized type " << this->get_declaration() );
    if ( this->typeClass != TXTC_ELEMENTARY )
        return false;
    if ( this->runtimeTypeId >= BuiltinTypeId_COUNT ) {
        // user derivation / alias of an elementary type
        auto scalar = this->get_declaration()->get_symbol()->get_root_scope()->registry().get_builtin_type( TXBT_SCALAR );
        return this->is_a( *scalar );
    }
    switch ( this->runtimeTypeId ) {
        case TXBT_SCALAR:
        case TXBT_INTEGER:
        case TXBT_SIGNED:
        case TXBT_BYTE:
        case TXBT_SHORT:
        case TXBT_INT:
        case TXBT_LONG:
        case TXBT_UNSIGNED:
        case TXBT_UBYTE:
        case TXBT_USHORT:
        case TXBT_UINT:
        case TXBT_ULONG:
        case TXBT_FLOATINGPOINT:
        case TXBT_HALF:
        case TXBT_FLOAT:
        case TXBT_DOUBLE:
        return true;
    default:
        return false;
    }
}


uint32_t TxActualType::get_elementary_type_id() const {
    ASSERT( this->hasInitialized, "Can't determine derivation characteristics of uninitialized type " << this->get_declaration() );
    if ( this->typeClass != TXTC_ELEMENTARY )
        return UINT32_MAX;
    auto type = this;
    while ( type->runtimeTypeId >= BuiltinTypeId_COUNT )
        type = type->get_base_type();
    return type->runtimeTypeId;
}


//const TxActualType* TxActualType::get_instance_base_type() const {
//    return ( this->is_same_instance_type() ? this->get_base_type()->get_instance_base_type() : this );
//}


//static const TxEntityDeclaration* get_type_param_decl( const std::vector<const TxEntityDeclaration*>& params, const std::string& fullParamName ) {
//    for ( auto & paramDecl : params )
//        if ( fullParamName == paramDecl->get_unique_full_name() )
//            return paramDecl;
//    return nullptr;
//}

static TxEntitySymbol* lookup_inherited_binding( const TxActualType* type, const std::string& fullParamName ) {
    TxIdentifier ident( fullParamName );
    auto parentName = ident.parent().str();
    auto paramName = ident.name();
    // search hierarchy for the top-most binding with the specified plain name
    // (this skips potential shadowing type parameters further down the type hierarchy)
    TxEntitySymbol* foundBoundSym = nullptr;
    const TxActualType* semBaseType = type->get_semantic_base_type();
    while ( semBaseType ) {
        if ( auto paramDecl = semBaseType->get_type_param_decl( paramName ) ) {
            if ( auto binding = type->get_binding( paramName ) )
                foundBoundSym = binding->get_symbol();
            if ( paramDecl->get_unique_full_name() == fullParamName )  // halt search when original type param declaration found
                return foundBoundSym;
        }
//        if ( get_type_param_decl( semBaseType->get_type_params(), fullParamName ) ) {
//            // semBaseType is the (nearest) type that declares the sought parameter
//            if ( auto binding = type->get_binding( paramName ) )
//                return binding->get_symbol();
//        }
//        else if ( semBaseType->get_declaration()->get_unique_full_name() == parentName )
//            LOG( type->LOGGER(), WARN, "Type parameter apparently unbound: " << fullParamName );

        if ( !type->is_integrated() )
            break;
        type = type->get_base_type();
        semBaseType = type->get_semantic_base_type();
    }
    return nullptr;
}

const TxFieldDeclaration* TxActualType::lookup_value_param_binding( const std::string& fullParamName ) const {
    if ( auto bindingSymbol = lookup_inherited_binding( this, fullParamName ) )
        return bindingSymbol->get_first_field_decl();
    return nullptr;
}
const TxTypeDeclaration* TxActualType::lookup_type_param_binding( const std::string& fullParamName ) const {
    if ( auto bindingSymbol = lookup_inherited_binding( this, fullParamName ) )
        return bindingSymbol->get_type_decl();
    return nullptr;
}

const TxEntityDeclaration* TxActualType::lookup_param_binding( const TxEntityDeclaration* paramDecl ) const {
    ASSERT( paramDecl->get_decl_flags() & TXD_GENPARAM, "Can't look up a binding for a 'param decl' that isn't GENPARAM: " << paramDecl );
    if ( auto bindingSymbol = lookup_inherited_binding( this, paramDecl->get_unique_full_name() ) ) {
        if ( dynamic_cast<const TxFieldDeclaration*>( paramDecl ) )
            return bindingSymbol->get_first_field_decl();
        else
            return bindingSymbol->get_type_decl();
    }
    return nullptr;
}

/** Returns true if this type is explicitly declared and is not a generic parameter nor generic binding. */
static inline bool is_explicit_nongen_declaration( const TxActualType* type ) {
    return ( !( type->get_declaration()->get_decl_flags() & ( TXD_IMPLICIT | TXD_GENPARAM | TXD_GENBINDING ) ) );
}

/* This implementation also checked structural equality, which we currently don't (generally) allow:
bool TxActualType::inner_equals( const TxActualType* thatType ) const {
    // note: both are assumed to have explicit declaration and/or be non-empty
    // (interfaces and members can only apply to a type with an explicit declaration, and an explicit declaration can have only one type instance)
    if ( this == thatType )
        return true;
    if ( ( this->get_declaration()->get_decl_flags() & TXD_IMPLICIT ) && ( thatType->get_declaration()->get_decl_flags() & TXD_IMPLICIT ) ) {
        // both are implicitly declared; compare structure:
        if ( this->baseType.modifiable == thatType->baseType.modifiable
             && this->is_mutable() == thatType->is_mutable()
             && this->get_semantic_base_type() == thatType->get_semantic_base_type()  // specializations of same semantic base
             && ( ( this->get_declaration()->get_decl_flags() & TXD_EXPERRBLOCK ) ==
                  ( thatType->get_declaration()->get_decl_flags() & TXD_EXPERRBLOCK ) ) )
        {
            auto thisBinds = this->get_bindings();
            auto thatBinds = thatType->get_bindings();
            if ( thisBinds.size() == thatBinds.size() ) {
                bool eq = std::equal( thisBinds.cbegin(), thisBinds.cend(), thatBinds.cbegin(),
                                      [this, thatType] ( const TxEntityDeclaration* aEntDecl, const TxEntityDeclaration* bEntDecl )->bool {
                    if (dynamic_cast<const TxTypeDeclaration*>( aEntDecl )) {
                        //std::cerr << "### comparing bindings.. " << aEntDecl->get_definer()->resolve_type( passInfo )->str(false) << std::endl
                        //          << "                         " << bEntDecl->get_definer()->resolve_type( passInfo )->str(false) << std::endl;
                        return ( aEntDecl->get_definer()->resolve_type( passInfo ) == bEntDecl->get_definer()->resolve_type( passInfo ) );
                    }
                    else if (auto aInitExpr = static_cast<const TxFieldDeclaration*>( aEntDecl )->get_definer()->get_init_expression()) {
                        if (auto bInitExpr = static_cast<const TxFieldDeclaration*>( bEntDecl )->get_definer()->get_init_expression()) {
                            if ( aInitExpr->is_statically_constant() && bInitExpr->is_statically_constant() ) {
                                if ( auto aBindExprType = aInitExpr->attempt_qualtype() ) {
                                    if ( aBindExprType->is_builtin( TXBT_UINT) )
                                        return ( eval_UInt_constant( aInitExpr ) == eval_UInt_constant( bInitExpr ) );
                                }
                            }
                        }
                    }
                    return false;  // to be regarded equal, both VALUE parameter bindings must have statically known, equal value
                } );
//                if (eq)
//                    std::cerr << "### structurally EQUAL:    " << this << std::endl
//                              << "                           " << thatType << std::endl;
//                else
//                    std::cerr << "### structurally UNEQUAL:  " << this->str(false) << std::endl
//                              << "                           " << thatType->str(false) << std::endl;
                return eq;
            }
        }
    }
    return false;
}
*/

bool TxActualType::operator==( const TxActualType& other ) const {
    // skips empty type derivations that aren't explicitly declared
    const TxActualType* thisType = this;
    const TxActualType* thatType = &other;
    while ( !is_explicit_nongen_declaration( thisType ) && thisType->is_empty_derivation() )
        thisType = thisType->get_base_type();
    while ( !is_explicit_nongen_declaration( thatType ) && thatType->is_empty_derivation() )
        thatType = thatType->get_base_type();
    return thisType->type_class_handler()->inner_equals( thisType, thatType );
}

bool TxActualType::is_assignable_to( const TxActualType& destination ) const {
    // fields must at least be the same instance data type
    // modifiability is disregarded (since this is in the context of copy-by-value)
    auto thisType = this;
    auto destType = &destination;
    while ( destType->is_empty_derivation() && !is_explicit_nongen_declaration( destType ) )
        destType = destType->get_base_type();
    if ( thisType->get_type_class() != destType->get_type_class() )
        return false;
    do {
        //std::cerr << thisType << "  IS-ASSIGNABLE-TO\n" << destType << std::endl;
        if ( thisType->type_class_handler()->inner_is_assignable_to( thisType, destType ) )
            return true;
        if ( !thisType->is_same_instance_type() )
            return false;
        thisType = thisType->get_base_type();
    } while ( true );
}



/** Returns the common base type of the types, if both are pure specializations of it. */
const TxActualType* TxTypeClassHandler::common_generic_base_type( const TxActualType* thisType, const TxActualType* thatType ) const {
    // find the nearest explicitly declared generic base type of each type and check if they're the same one:
    // (type parameters can only be present on explicitly declared types)
    while ( !thisType->is_explicit_declaration() && !thisType->get_bindings().empty() )
        thisType = thisType->get_semantic_base_type();
    while ( !thatType->is_explicit_declaration() && !thatType->get_bindings().empty() )
        thatType = thatType->get_semantic_base_type();
    if ( this->inner_equals( thisType, thatType ) )
        return thisType;
    return nullptr;
}

static inline TxQualType get_binding_or_parameter_qtype( const TxActualType* type, const TxEntityDeclaration* paramDecl ) {
    if ( auto qtDecl = type->lookup_type_param_binding( paramDecl->get_unique_full_name() ) )
        return qtDecl->get_definer()->qtype();
    return TxQualType( type );
}

bool TxTypeClassHandler::inner_is_a( const TxActualType* thisType, const TxActualType* thatType ) const {
    //std::cerr << thisType << "  IS-A\n" << thatType << std::endl;
    // by-pass anonymous, empty specializations:
    while ( !is_explicit_nongen_declaration( thisType ) && thisType->is_empty_derivation() )
        thisType = thisType->get_base_type();

    if ( this->inner_equals( thisType, thatType ) )
        return true;

    // check whether other is a more generic version of the same type:
    if ( thisType->is_gen_or_spec() && thatType->is_gen_or_spec() ) {
        if ( auto genBaseType = common_generic_base_type( thisType, thatType ) ) {
            //std::cerr << "Common generic base type " << genBaseType << std::endl << "\tthisType: " << thisType << std::endl << "\tthatType: " << thatType << std::endl;
            for ( auto paramDecl : genBaseType->get_type_params() ) {
                // each of other's type param shall either be unbound (redeclared) or *equal* to this type's param/binding
                // (is-a is not sufficient in general case)
                if ( auto thatBinding = thatType->lookup_param_binding( paramDecl ) ) {
                    if ( auto thatBindDecl = dynamic_cast<const TxTypeDeclaration*>( thatBinding ) ) {
                        // other has bound this TYPE param - check that it matches this type's param/binding
                        // - a MOD binding is considered to be is-a of a non-MOD binding
                        // - a binding may be to a type that is equal to the parameter's constraint type, i.e. equivalent to unbound parameter
                        // check whether both resolve to same type/value:
                        const TxQualType thisBTypeQT = get_binding_or_parameter_qtype( thisType, paramDecl );
                        const TxQualType thatBTypeQT = thatBindDecl->get_definer()->qtype();
                        if ( thatBTypeQT.is_modifiable() ) {
                            if ( !( thisBTypeQT.is_modifiable() && *thisBTypeQT.type() == *thatBTypeQT.type() ) )
                                return false;
                        }
                        else if ( *thisBTypeQT.type() != *thatBTypeQT.type() )
                            return false;
                    }
                    else {
                        // other has bound this VALUE param - must match this type's VALUE binding exactly
                        bool staticEqual = false;
                        auto thatFieldBinding = static_cast<const TxFieldDeclaration*>( thatBinding );
                        if ( auto thisFieldBinding = dynamic_cast<const TxFieldDeclaration*>( thisType->lookup_param_binding( paramDecl ) ) ) {
                            thisFieldBinding->get_definer()->resolve_type( TXP_RESOLUTION );
                            thatFieldBinding->get_definer()->resolve_type( TXP_RESOLUTION );
                            if ( auto thisInitExpr = thisFieldBinding->get_definer()->get_init_expression() ) {
                                if (auto thatInitExpr = thatFieldBinding->get_definer()->get_init_expression() ) {
                                    staticEqual = is_static_equal( thisInitExpr, thatInitExpr );
                                }
                            }
                        }
                        if (! staticEqual)
                            return false;
                    }
                }
            }
            return true;
        }
    }

    // check whether any ancestor type is-a the other type:
    if ( thatType->get_type_class() == TXTC_INTERFACE ) {
        for ( auto & interf : thisType->interfaces ) {
            if ( inner_is_a( interf, thatType ) )
                return true;
        }
    }
    if ( thisType->has_base_type() ) {
        if ( thisType->genericBaseType ) {
            if ( inner_is_a( thisType->genericBaseType, thatType ) )
                return true;
        }
        if ( inner_is_a( thisType->get_base_type(), thatType ) )
            return true;
    }
    return false;
}

bool TxTypeClassHandler::is_a( const TxActualType* type, const TxActualType* other ) const {
    const TxActualType* thisType = type;
    const TxActualType* thatType = other;
    //std::cerr << thisType << "  IS-A\n" << thatType << std::endl;

    // by-pass anonymous, empty specializations:
    while ( !is_explicit_nongen_declaration( thatType ) && thatType->is_empty_derivation() )
        thatType = thatType->get_base_type();

    return inner_is_a( thisType, thatType );
}

std::string TxActualType::str() const {
    return this->str( true );
}

static void type_params_string(std::stringstream& str, const std::vector<const TxEntityDeclaration*>& params) {
    str << " {";
    int ix = 0;
    for (auto & p : params) {
        if (ix++)  str << ",";
        str << p->get_unique_name();
    }
    str << "}";
}

static void type_bindings_string( std::stringstream& str, const std::vector<const TxEntityDeclaration*>& bindings ) {
    str << " <";
    int ix = 0;
    for ( auto b : bindings ) {
        if ( ix++ )
            str << ",";
        if ( auto valB = dynamic_cast<const TxFieldDeclaration*>( b ) ) {
            if ( auto initExpr = valB->get_definer()->get_init_expression() ) {
                if ( initExpr->is_statically_constant() ) {
                    // existing binding has statically constant value
                    // TODO: handle constants of different types
                    str << eval_unsigned_int_constant( initExpr );
                    continue;
                }
            }
            str << "?";
        }
        else if ( auto btype = b->get_definer()->attempt_qtype() )
            str << btype.str( true );
        else
            str << "'" << b->get_unique_full_name() << "'";
    }
    str << ">";
}

std::string TxActualType::str( bool brief ) const {
    std::stringstream str;
    this->self_string( str, brief );
    if ( !brief ) {
        if ( this->typeClass == TXTC_INTERFACE )
            str << " i/f";
        else if ( this->typeClass == TXTC_INTERFACEADAPTER )
            str << " i/f/ad";
        if ( this->is_mutable() )
            str << " MUT";
    }
    else if ( this->is_mutable() && this->hasInitialized && this->get_type_class() == TXTC_TUPLE )
        str << " MUT";
    return str.str();
}

void TxActualType::self_string( std::stringstream& str, bool brief ) const {
    bool expl = !( this->get_declaration()->get_decl_flags() & ( TXD_IMPLICIT | TXD_GENPARAM | TXD_GENBINDING ) );
    str << this->get_declaration()->get_unique_full_name();

    if ( !this->hasInitialized ) {
        str << " -uninitialized-";
        return;
    }

    if (! this->params.empty())
        type_params_string(str, this->params);
    if ( !expl && !brief ) {
        if ( !this->get_bindings().empty() ) {
            type_bindings_string( str, this->get_bindings() );
        }

        if ( this->has_base_type() ) {
            str << ( this->emptyDerivation ? " = " : " : " );
            if ( auto base = this->genericBaseType ? this->genericBaseType : this->baseType )
                base->self_string( str, false );  // set 'brief' to false to print entire type chain
            else
                str << "-unintegrated-";
        }
    }
//    else if ( this->typeClass == TXTC_REFERENCE ) {
//        if (! this->is_generic() ) {
//            type_bindings_string( str, this->get_bindings() );
//        }
//    }
    else if ( this->typeClass == TXTC_ARRAY ) {
        if (! this->is_type_generic() ) {
            auto elemType = this->element_type();
            str << " <" << ( elemType ? elemType.str( true ) : "_" );
            if (! this->is_generic() ) {
                auto initExpr = this->capacity();
                if ( !initExpr )
                    str << ",_";
                else if ( initExpr->is_statically_constant() )
                    str << "," << eval_unsigned_int_constant( initExpr );
                else
                    str << ",?";
            }
            str << ">";
        }
    }
    else {
        if ( !this->get_bindings().empty() ) {
            type_bindings_string( str, this->get_bindings() );
        }
    }
}



/*=== Array and Reference helper methods ===*/

const TxExpressionNode* TxActualType::capacity() const {
    if ( this->get_type_class() != TXTC_ARRAY )
        THROW_LOGIC( "Can't get capacity() of non-array type: " << this );
    if ( auto bindingDecl = this->lookup_value_param_binding( "tx.Array.C" ) ) {
        return bindingDecl->get_definer()->get_init_expression();
    }
    return nullptr;
}

TxQualType TxActualType::element_type() const {
    if ( this->get_type_class() != TXTC_ARRAY )
        THROW_LOGIC( "Can't get element_type() of non-array type: " << this );
//    if ( auto bindingDecl = this->lookup_type_param_binding( "tx.Array.E" ) ) {
//        return bindingDecl->get_definer()->qtype();
//    }
    if ( auto entSym = dynamic_cast<TxEntitySymbol*>( lookup_inherited_member( this->get_declaration()->get_symbol(), this, "tx#Array#E" ) ) ) {
        if ( auto typeDecl = entSym->get_type_decl() ) {
            //std::cerr << "Array.E type decl: " << typeDecl << std::endl;
            return typeDecl->get_definer()->qtype();
        }
    }
    LOG( this->LOGGER(), ERROR, "tx#Array#E not found in " << this->get_declaration() );
    return TxQualType( this->get_root_any_qtype() );  // we know the basic constraint type for element is Any
}


TxQualType TxActualType::target_type() const {
    if ( this->get_type_class() != TXTC_REFERENCE )
        THROW_LOGIC( "Can't get target_type() of non-reference type: " << this );
    if ( auto entSym = dynamic_cast<TxEntitySymbol*>( lookup_inherited_member( this->get_declaration()->get_symbol(), this, "tx#Ref#T" ) ) ) {
        if ( auto qtypeDecl = entSym->get_type_decl() ) {
            //std::cerr << "Ref.T type decl: " << typeDecl << std::endl;
            return qtypeDecl->get_definer()->qtype();
        }
    }
    LOG( this->LOGGER(), ERROR, "tx#Ref#T not found in " << this->get_declaration() );
    return TxQualType( this->get_root_any_qtype() );  // we know the basic constraint type for ref target is Any
}



/*=== Function type helper methods and Function types' implementation ===*/

static const TxFunctionType* get_function_base_type( const TxActualType* type ) {
    ASSERT( type->get_type_class() == TXTC_FUNCTION, "Not a function type: " << type );
    for ( ; type; type = type->get_base_type() ) {
        if ( auto ftype = dynamic_cast<const TxFunctionType*>( type ) ) {
            return ftype;
        }
    }
    THROW_LOGIC( "Can't invoke function type accessor of non-function type: " << type );
}

bool TxActualType::modifiable_closure() const {
    return get_function_base_type( this )->modifiable_closure();
}

const std::vector<const TxActualType*>& TxActualType::argument_types() const {
    return get_function_base_type( this )->argument_types();
}

/** Gets the return type of this function type. If the function does not return a value, this is Void. */
const TxActualType* TxActualType::return_type() const {
    return get_function_base_type( this )->return_type();
}

/** Returns false if this function type's return type is Void. */
bool TxActualType::has_return_value() const {
    return get_function_base_type( this )->has_return_value();
}

const TxActualType* TxActualType::vararg_elem_type() const {
    auto & argumentTypes = this->argument_types();
    if ( !argumentTypes.empty() ) {
        auto lastArgType = argumentTypes.back();
        if ( lastArgType->get_type_class() == TXTC_REFERENCE ) {
            auto refTargetType = lastArgType->target_type();
            if ( refTargetType->get_type_class() == TXTC_ARRAY ) {
                if ( !refTargetType->capacity() )  // only arrays of unspecified capacity apply to var-args syntactic sugar
                    return refTargetType->element_type().type();
            }
        }
    }
    return nullptr;
}

const TxActualType* TxActualType::fixed_array_arg_type() const {
    auto & argumentTypes = this->argument_types();
    if ( argumentTypes.size() == 1 ) {
        auto argType = argumentTypes.back();
        if ( argType->get_type_class() == TXTC_ARRAY ) {
            if ( auto lenExpr = argType->capacity() ) {
                if ( lenExpr->is_statically_constant() ) {
                    return argType;
                }
            }
        }
    }
    return nullptr;
}

std::string TxActualType::func_signature_str() const {
    return get_function_base_type( this )->func_signature_str();
}



TxFunctionType::TxFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                const std::vector<const TxActualType*>& argumentTypes,
                                bool modifiableClosure )
        : TxFunctionType( declaration, baseType, argumentTypes,
                          baseType->get_declaration()->get_symbol()->get_root_scope()->registry().get_builtin_type( TXBT_VOID ),
                          modifiableClosure ) {
}

TxExpressionNode* TxBuiltinConversionFunctionType::make_inline_expr( TxExpressionNode* calleeExpr,
                                                                     std::vector<TxMaybeConversionNode*>* argsExprList ) const {
    return make_conversion( argsExprList->front(), this->return_type(), true );
}

TxExpressionNode* TxBuiltinAssignInitializerType::make_inline_expr( TxExpressionNode* calleeExpr,
                                                                    std::vector<TxMaybeConversionNode*>* argsExprList ) const {
    return argsExprList->front();
}



/*=== InterfaceAdapter type implementation ===*/

bool TxInterfaceAdapterType::inner_prepare_members() {
    bool rec = TxActualType::inner_prepare_members();

    LOG_DEBUG( this->LOGGER(), "preparing adapter for " << this->adaptedType << " to interface " << this->get_semantic_base_type() );
    // The virtual fields of the abstract base interface type are overridden to refer to
    // the correspondingly named fields of the adapted type.

    auto & adapteeVirtualFields = this->adaptedType->get_virtual_fields();
    for ( auto & f : this->virtualFields.fieldMap ) {
        if ( f.first == "$adTypeId" )
            continue; // this field is not overridden to refer to an adaptee field (and it's initialized together with the vtable)
        else if ( !adapteeVirtualFields.has_field( f.first ) ) {
            auto protoField = this->virtualFields.get_field( f.second );
            if ( protoField->get_decl_flags() & TXD_ABSTRACT )
                CERROR( this, "Adapted type " << this->adaptedType << " does not define virtual field " << f.first );
            else
                // default implementation (mixin)
                LOG( this->LOGGER(), NOTE, "Adapted type " << this->adaptedType << " gets interface's default impl for field " << protoField );
        }
        else {
            auto targetField = adapteeVirtualFields.get_field( f.first );
            // TODO: verify that type matches
            this->virtualFields.override_field( /*f.first,*/ targetField );
        }
    }

    return rec;
}
