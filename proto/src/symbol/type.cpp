#include "util/logging.hpp"

#include "tx_error.hpp"

#include "type.hpp"
#include "type_registry.hpp"
#include "entity_type.hpp"
#include "package.hpp"

#include "ast/expr/ast_expr_node.hpp"
#include "ast/expr/ast_constexpr.hpp"
#include "ast/expr/ast_conv.hpp"

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

/*=== TxTypeSpecialization implementation ===*/

bool TxTypeSpecialization::operator==( const TxTypeSpecialization& other ) const {
    return ( ( this->type == other.type )  // instance equality required
             //|| ( this->type && other.type && *this->type == *other.type) )
             //&& *this->dataspace == *other.dataspace
             && this->modifiable == other.modifiable );
}

std::string TxTypeSpecialization::str() const {
    return "specialization of " + this->type->str();
}

/*=== TxActualType implementation ===*/

/** Used to ensure proper resetting recursionGuard in type (RAII style). */
class ScopedRecursionGuardClause {
    const TxActualType* type;
    public:
    ScopedRecursionGuardClause( const TxActualType* type )
            : type( type ) {
        this->type->recursionGuard = true;
    }
    ~ScopedRecursionGuardClause() {
        this->type->recursionGuard = false;
    }
};

Logger& TxActualType::_LOG = Logger::get( "ENTITY" );

//void TxActualType::rewire( const TxActualType* newBaseType ) {
//    ASSERT(this->typeClass == newBaseType->typeClass, "Mismatching type classes: " << this->typeClass << " != " << newBaseType->typeClass);
//    ASSERT(!this->builtin, "can't be built-in");
//    ASSERT(this->interfaces.empty(), "can't implement interfaces");
//    ASSERT(this->staticTypeId == UINT32_MAX, "can't have a set staticTypeId: " << this->staticTypeId);
//    ASSERT(this->params.empty(), "can't have type parameters");
//    ASSERT(!this->baseTypeSpec.modifiable, "can't be modifiable");
//    ASSERT(this->genericBaseType == newBaseType->get_semantic_base_type(), "mismatching generic base types: "
//            << this->genericBaseType << " != " << newBaseType->get_semantic_base_type());
//    ASSERT(this->virtualFields.fields.empty(), "non-empty virtual fields");
//    ASSERT(this->staticFields.fields.empty(), "non-empty static fields");
//    ASSERT(this->instanceFields.fields.empty(), "non-empty instance fields");
//
//    this->bindings.clear();
//
//    const_cast<TxTypeSpecialization*>( &this->baseTypeSpec )->type = newBaseType;
//    this->genericBaseType = nullptr;
//
//    nonRefBindings = false;
//    extendsInstanceDatatype = false;
//    modifiesVTable = false;
//    emptyDerivation = true;
//    pureDerivation = false;
//}

const TxNode* TxActualType::get_origin_node() const {
    return this->get_declaration()->get_definer();
}

const TxActualType* TxActualType::get_root_any_type() const {
    return this->get_declaration()->get_symbol()->get_root_scope()->registry().get_builtin_type( TXBT_ANY )->type();
}

void TxActualType::validate_type() const {
    //std::cerr << "validating type " << this << std::endl;
    if ( this->baseTypeSpec.type ) {
        if ( this->baseTypeSpec.type->is_modifiable() )
            CERROR( this, "Can't specialize a 'modifiable' type (specialize its base type instead): " << this->baseTypeSpec.type );

        if ( this->baseTypeSpec.modifiable ) {
            ASSERT( this->get_type_class() == this->baseTypeSpec.type->get_type_class(),
                    "'modifiable' specialization must have same TxActualType class as the base type: " << this->baseTypeSpec.type );
            if ( !this->baseTypeSpec.type->is_mutable() )
                CERROR( this, "Can't make an immutable type modifiable: " << this->baseTypeSpec.type );
            //if (this->dataspace)
            //    CERROR(this->type, "Can't specify dataspace for a 'modifiable' type specialization");

            // verify that this 'modifiable' type usage is an 'equivalent' specialization
            if ( !this->interfaces.empty() )
                CERROR( this, "'modifiable' specialization cannot add any interface base types" );
        }
        else {
            if ( this->is_mutable() && !this->get_semantic_base_type()->is_mutable() )
                CERROR( this, "Can't derive a mutable type from an immutable base type: " << this->get_semantic_base_type() );

            if ( this->baseTypeSpec.type->formalTypeId == TXBT_ANY
                 && !( this->builtin || ( this->get_declaration()->get_decl_flags() & TXD_GENPARAM )
                       || this->get_type_class() == TXTC_REFERENCE || this->get_type_class() == TXTC_ARRAY ) )
                CERROR( this, "Can't derive directly from the Any root type: " << this->get_declaration() );
            ASSERT( this->baseTypeSpec.type->formalTypeId == TXBT_ANY
                    || ( this->get_type_class() == TXTC_INTERFACEADAPTER && this->baseTypeSpec.type->get_type_class() == TXTC_INTERFACE )
                    || this->get_type_class() == this->baseTypeSpec.type->get_type_class(),
                    "Specialized type's type class " << this << " not valid with base type's type class " << this->baseTypeSpec.type->get_type_class() );
        }
//        if (this->dataspace && this->baseTypeSpec.type->get_type_class() != TXTC_REFERENCE)
//            CERROR(this, "Specified dataspace for non-reference base type " << this->baseTypeSpec.type);

        if ( this->baseTypeSpec.type->is_empty_derivation() && !this->baseTypeSpec.type->get_explicit_declaration() ) {
            ASSERT( !( this->is_empty_derivation() && !this->get_explicit_declaration() ),
                    "anonymous or implicit, empty types may not be derived except as another anonymous or implicit, empty type: " << this );
        }

        // if this is not an empty nor a modifiable derivation, verify that all parameters of base type are either bound, or redeclared:
        // Note: The base type's parameters that have not been bound should normally be automatically redeclared by the type registry.
        if ( !this->emptyDerivation && !this->is_modifiable() ) {
            for ( auto & paramDecl : this->get_semantic_base_type()->get_type_params() ) {
                if ( !this->get_binding( paramDecl->get_unique_name() ) ) {
                    if ( !this->has_type_param( paramDecl->get_unique_name() ) ) {
                        //this->params.emplace_back(paramDecl);
                        if ( this->get_type_class() != TXTC_INTERFACEADAPTER ) {
                            CERROR( this, "Missing binding or redeclaration of base type's type parameter "
                                    << paramDecl->get_unique_name() << " in " << this );
                            //LOG(this->LOGGER(), NOTE, "Implicitly inheriting (redeclaring) type parameter %s in type %s", paramDecl->get_unique_full_name().c_str(), this->to_string().c_str());
                        }
                    }
                }
            }
        }
    }
    // TODO: validate interfaces
    // check that generic interfaces can't be implemented multiple times throughout a type hierarchy,
    // unless their type arguments are exactly the same
    for ( auto & interfSpec : this->interfaces ) {
        if ( interfSpec.type->get_type_class() != TXTC_INTERFACE )
            CERROR( this, "Only the first derived-from type can be a non-interface type: " << interfSpec.type );
    }
}

void TxActualType::initialize_type() {
    LOG_TRACE( this->LOGGER(), "Initializing type " << this );

    ASSERT( this->declaration, "No declaration for actual type " << this );

    auto typeDeclNamespace = this->get_declaration()->get_symbol();
    const TxActualType* semBaseType = this->baseTypeSpec.type;

    // perform shallow pass on type's member declarations to determine derivation characteristics:
    bool hasExplicitFieldMembers = false;
    bool hasImplicitFieldMembers = false;
    for ( auto symname = typeDeclNamespace->decl_order_names_cbegin(); symname != typeDeclNamespace->decl_order_names_cend(); symname++ ) {
        if ( auto entitySym = dynamic_cast<TxEntitySymbol*>( typeDeclNamespace->get_member_symbol( *symname ) ) ) {
            if ( auto typeDecl = entitySym->get_type_decl() ) {
                if ( typeDecl->get_decl_flags() & TXD_GENPARAM ) {
                    this->params.emplace_back( typeDecl );
                    //std::cerr << "FOUND TYPE GENPARAM: " << typeDecl << std::endl;
                }
                else if ( typeDecl->get_decl_flags() & TXD_GENBINDING ) {
                    this->bindings.emplace_back( typeDecl );
                    //std::cerr << "FOUND TYPE GENBINDING: " << typeDecl << std::endl;
                }
                else if ( *symname == "$GenericBase" ) {
                    this->genericBaseType = typeDecl->get_definer()->resolve_type()->type();
                    semBaseType = this->genericBaseType;
                }
            }

            for ( auto fieldDeclI = entitySym->fields_cbegin(); fieldDeclI != entitySym->fields_cend(); fieldDeclI++ ) {
                auto fieldDecl = *fieldDeclI;

                if ( fieldDecl->get_decl_flags() & TXD_IMPLICIT )
                    hasImplicitFieldMembers = true;
                else
                    hasExplicitFieldMembers = true;

                if ( fieldDecl->get_decl_flags() & TXD_GENPARAM ) {
                    //std::cerr << "FOUND VALUE GENPARAM: " << typeDecl << std::endl;
                    this->params.emplace_back( fieldDecl );
                }
                else if ( fieldDecl->get_decl_flags() & TXD_GENBINDING ) {
                    //std::cerr << "FOUND VALUE GENBINDING: " << typeDecl << std::endl;
                    this->bindings.emplace_back( fieldDecl );
                }

                switch ( fieldDecl->get_storage() ) {
                case TXS_INSTANCE:
                    // Note: VALUE bindings are only declared as instance members in generic base type,
                    // so that they are not "extensions" to the specialized subtypes.
                    if ( !( fieldDecl->get_decl_flags() & TXD_GENBINDING ) ) {
                        this->extendsInstanceDatatype = true;
                    }
                    break;
                case TXS_INSTANCEMETHOD:
                    if ( fieldDecl->get_decl_flags() & TXD_CONSTRUCTOR )
                        break;
                    // no break
                case TXS_VIRTUAL:
                    this->modifiesVTable = true;
                    break;
                    // note: TXS_STATIC members are private, and like globals but with limited visibility,
                    // and don't affect the derivation degree.
                default:
                    break;
                }
            }
        }
    }

    if ( !hasExplicitFieldMembers ) {
        if ( !this->bindings.empty() ) {
            this->pureDerivation = true;
        }
        else if ( !this->is_builtin() && !this->is_modifiable() && this->interfaces.empty() && this->params.empty() ) {
            if ( this->get_type_class() == TXTC_FUNCTION ) {
                // do something?
            }
            else if ( this->get_type_class() == TXTC_INTERFACEADAPTER ) {
            }
            else {
                this->emptyDerivation = true;
                if ( hasImplicitFieldMembers ) {
                    LOG( this->LOGGER(), NOTE, "Type with only implicit field members: " << this );
                }
                ASSERT( !this->genericBaseType, "Empty derivation had a $GenericBase: " << this->genericBaseType );
            }
        }
    }

    { // validate the type parameter bindings (as much as we can without resolving this type's bindings at this point)
        for ( auto & bindingDecl : this->bindings ) {
            auto pname = bindingDecl->get_unique_name();
            if ( auto paramDecl = semBaseType->get_type_param_decl( pname ) ) {
                auto constraintType = paramDecl->get_definer()->resolve_type()->type();
                ASSERT( constraintType, "NULL constraint type for param " << paramDecl << " of " << semBaseType );
                //std::cerr << this << ": Constraint type for param " << paramDecl << ": " << "checking bound type "
                //          << boundType << "\tagainst constraint type " << constraintType << std::endl;

                if ( dynamic_cast<const TxTypeDeclaration*>( paramDecl ) ) {
                    if ( !dynamic_cast<const TxTypeDeclaration*>( bindingDecl ) )
                        CERROR( bindingDecl->get_definer(), "Binding for type parameter " << paramDecl << " is not a type: " << bindingDecl );
                    if ( constraintType->get_type_class() != TXTC_REFERENCE && this->get_type_class() != TXTC_REFERENCE )
                        this->nonRefBindings = true;
                }
                else {
                    if ( !dynamic_cast<const TxFieldDeclaration*>( bindingDecl ) )
                        CERROR( bindingDecl->get_definer(), "Binding for type parameter " << paramDecl << " is not a field/value: " << bindingDecl );
                    this->nonRefBindings = true;
                }
            }
            else
                CERROR( bindingDecl->get_definer(),
                        "No type parameter of " << semBaseType << " matches provided binding " << bindingDecl->get_unique_name() );
        }
    }

    // determine datatype change:
    if ( this->is_modifiable() ) {
        // a modifiable type is a usage form of its base type, and doesn't affect the instance nor the vtable type
    }
    else {
        if ( this->nonRefBindings ) {
            // Binding of a base type parameter implies reinterpretation of its members and thus
            // the chance of modified instance / vtable types (for non-ref-constrained parameters).
            // Note, may cause false positives (a full graph analysis of contained members would be needed for full accuracy)
            this->extendsInstanceDatatype = true;
            this->modifiesVTable = true;
        }
        else if ( this->is_builtin() ) {
            // Built-in implies a distinct instance type compared to the base type.
            this->extendsInstanceDatatype = true;
        }
        else if ( this->get_type_class() == TXTC_FUNCTION ) {
            // function type implies a distinct instance type compared to the base type (for now)
            this->extendsInstanceDatatype = true;
        }
        else if ( this->get_type_class() == TXTC_INTERFACEADAPTER ) {
            this->modifiesVTable = true;
        }

        if ( !this->interfaces.empty() ) {
            // If there are interfaces we assume that will cause the vtable will be extended in preparation.
            // This may cause false positives, but we need to determine this flag in the type's initialization phase.
            this->modifiesVTable = true;
        }
    }

    this->hasInitialized = true;

    this->validate_type();
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

    bool expErrWholeType = ( ( this->get_declaration()->get_decl_flags() & TXD_EXPERRBLOCK )
                             || this->get_declaration()->get_definer()->exp_err_ctx() );
    ScopedExpErrClause scopedEEWholeType( this->get_declaration()->get_definer(), expErrWholeType );

    // copy base type's virtual and instance field tuples (to which fields may be added / overridden):
    auto baseType = this->get_base_type();
    if ( baseType ) {
        //ASSERT(baseType->is_prepared(), "Base type " << baseType << " not prepared before sub type " << this);
        recursionError = const_cast<TxActualType*>( baseType )->prepare_members();
        this->virtualFields = baseType->virtualFields;
        this->instanceFields = baseType->instanceFields;
    }
    for ( auto & interfSpec : this->interfaces ) {
        //ASSERT(interfSpec.type->is_prepared(), "Base i/f " << interfSpec.type << " not prepared before sub type " << this);
        recursionError |= const_cast<TxActualType*>( baseType )->prepare_members();
        bool added = this->virtualFields.add_interface_fields( interfSpec.type->virtualFields );
        if ( !added )
            LOG( this->LOGGER(), NOTE, "Type implements interfaces but they don't cause the vtable to be extended: " << this );
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
            ScopedExpErrClause expErrClause( typeDecl->get_definer(), ( typeDecl->get_decl_flags() & TXD_EXPERRBLOCK ) );

            if ( typeDecl->get_decl_flags() & TXD_GENBINDING ) {
                if ( auto paramDecl = semBaseType->get_type_param_decl( typeDecl->get_unique_name() ) ) {
                    auto constraintType = paramDecl->get_definer()->get_type()->type();
                    auto type = typeDecl->get_definer()->get_type();
                    if ( !type->type()->is_a( *constraintType ) ) {
                        // TODO: do this also for VALUE params, but array type expression needs auto-conversion support for that to work
                        CERROR( typeDecl->get_definer(),
                                "Bound type for type parameter " << paramDecl->get_unique_full_name() << ": " << type->str(false)
                                << std::endl << "  is not a derivation of contraint type: " << constraintType->str(false) );
//                            std::cerr << "definer: " << typeDecl->get_definer() << std::endl;
                    }
// this special case check doesn't work for all cases
//                    if ( this->get_type_class() == TXTC_ARRAY ) {
//                        if ( this->is_mutable() && !this->is_generic_dependent() ) {
//                            if ( !type->is_modifiable() ) {
//                                CERROR( typeDecl->get_definer(), "Bound element type for mutable array is not modifiable: " << type );
//                            }
//                        }
//                    }
                }
            }
        }

        // prepare field members:
        for ( auto fieldDeclI = entitySym->fields_cbegin(); fieldDeclI != entitySym->fields_cend(); fieldDeclI++ ) {
            auto fieldDecl = *fieldDeclI;

            bool expErrField = ( fieldDecl->get_decl_flags() & TXD_EXPERRBLOCK );
            ScopedExpErrClause expErrClause( fieldDecl->get_definer(), expErrField );
            if ( expErrField && !fieldDecl->get_definer()->attempt_get_field() ) {
                LOG_TRACE( this->LOGGER(), "Skipping preparation of EXPERR unresolved field " << fieldDecl );
                continue;
            }

            auto field = fieldDecl->get_definer()->get_field();
            auto fieldType = field->get_type()->type();

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
                if ( const_cast<TxActualType*>( fieldType )->prepare_members() )
                    CERROR( field, "Recursive data type via field " << field->get_declaration()->get_unique_full_name() );

//                if ( false && ( fieldDecl->get_decl_flags() & TXD_GENBINDING ) )
//                    LOG_NOTE( this->LOGGER(), "Skipping layout of GENBINDING instance field: " << field );
//                else if ( false && ( fieldDecl->get_decl_flags() & TXD_GENPARAM )
//                          //&& this->get_type_class() == TXTC_ARRAY && this->staticTypeId != TXBT_ARRAY
//                          ) {
//                    // special case for the only built-in type with a VALUE param - Array
//                    // this handles specializations of Array where the capacity (C) has not been bound and another C field shall not be added
//                    LOG_NOTE( this->LOGGER(), "Skipping layout of Array GENPARAM instance field: " << field );
//                }
                else if ( !expErrField || expErrWholeType )
                    this->instanceFields.add_field( field );
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

                if ( this->virtualFields.has_field( field->get_unique_name() ) ) {
                    if ( !( fieldDecl->get_decl_flags() & TXD_OVERRIDE ) )
                        CWARNING( field, "Field overrides but isn't declared 'override': " << field );
                    auto overriddenField = this->virtualFields.get_field( field->get_unique_name() );
                    if ( overriddenField->get_decl_flags() & TXD_FINAL )
                        CERROR( field, "Can't override a base type field that is declared 'final': " << field );
                    if ( !( field->get_type()->type()->is_assignable_to( *overriddenField->get_type()->type() ) ) )
                        CERROR( field, "Overriding member's type " << field->get_type() << std::endl
                                << "   not assignable to overridden member's type " << overriddenField->get_type() );
                    if ( !expErrField || expErrWholeType )
                        this->virtualFields.override_field( field->get_unique_name(), field );
                }
                else {
                    if ( fieldDecl->get_decl_flags() & TXD_OVERRIDE )
                        CWARNING( field, "Field doesn't override but is declared 'override': " << field );
                    if ( !expErrField || expErrWholeType )
                        this->virtualFields.add_field( field );
                }
                this->LOGGER()->debug( "Adding/overriding virtual field %-40s  %s  %u", field->str().c_str(),
                                       field->get_type()->str().c_str(),
                                       this->virtualFields.get_field_count() );
                break;
            default:
                ASSERT( fieldDecl->get_storage() == TXS_STATIC,
                        "Invalid storage class " << fieldDecl->get_storage() << " for field member " << *field );
                if ( fieldDecl->get_decl_flags() & TXD_INITIALIZER )
                    break;  // skip, initializers are inlined and not actually added as static functions

                if ( fieldDecl->get_decl_flags() & TXD_ABSTRACT )
                    CERROR( field, "Can't declare a non-virtual field as abstract: " << field );
                if ( fieldDecl->get_decl_flags() & TXD_OVERRIDE )
                    CWARNING( field, "Field doesn't override but is declared 'override': " << field );
                if ( !expErrField || expErrWholeType )
                    this->staticFields.add_field( field );
            }
        }
    }

    // (note, this condition is not the same as is_concrete())
    if ( !this->is_abstract() && !this->is_modifiable() && this->get_type_class() != TXTC_INTERFACEADAPTER
         && !( this->get_declaration()->get_decl_flags() & TXD_GENPARAM ) ) {
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

/** Returns true if this type has one or more (unbound) TYPE parameters that are not constrained to be a Ref type,
 * or an unbound VALUE parameter. These mean the size of this type is neither statically nor dynamically known. */
static bool has_nonref_params( const TxActualType* type ) {
    for ( auto & paramDecl : type->get_type_params() ) {
        if ( auto paramTypeDecl = dynamic_cast<const TxTypeDeclaration*>( paramDecl ) ) {
            auto constraintType = paramTypeDecl->get_definer()->resolve_type();
            ASSERT( constraintType, "NULL constraint type for param " << paramDecl << " of " << type );
            if ( constraintType->get_type_class() != TXTC_REFERENCE )
                return true;
        }
        else
            return true;
    }
    return false;
}

/** Returns true if any the type's enclosing types is dependent on non-ref type parameters. */
static bool has_outer_with_nonref_params( const TxActualType* type ) {
    TxScopeSymbol* scope = type->get_declaration()->get_symbol()->get_outer();
    while ( !dynamic_cast<TxModule*>( scope ) ) {
        if ( auto entitySymbol = dynamic_cast<TxEntitySymbol*>( scope ) ) {
            type = entitySymbol->get_type_decl()->get_definer()->get_type()->type();
            if ( has_nonref_params( type ) )
                return true;
        }
        scope = scope->get_outer();
    }
    return false;
}

/** Returns true if this type is dependent on a VALUE type parameter binding with a dynamic value (not known at compile time),
 * either directly (one of its own bindings), or via a TYPE binding that is dynamic. */
static bool is_dynamic_binding_dependent( const TxActualType* type ) {
//    if ( recursionGuard ) {
//        LOG( this->LOGGER(), DEBUG,
//             "Infinite recursion (probably due to erroneously recursive type definition) detected in is_concrete() of type " << this );
//        return false;
//    }
//    ScopedRecursionGuardClause guard( this );
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
            if ( is_dynamic_binding_dependent( static_cast<const TxTypeDeclaration*>( b )->get_definer()->resolve_type()->type() ) )
                return true;
        }
    }
    return false;
}

bool TxActualType::is_concrete() const {
//    if (this->get_type_class() == TXTC_ARRAY)
//        std::cerr << this << std::endl;
    const TxActualType* type = this;
    if ( type->is_abstract() )
        return false;
    while ( type->is_equivalent_derivation() ) {
        type = type->get_base_type();
        if ( type->is_abstract() )
            return false;
    }

    if ( has_nonref_params( type ) )
        return false;  // if only Ref-constrained parameters, then being generic doesn't cause it to be non-concrete
    return !has_outer_with_nonref_params( type );
}

bool TxActualType::is_static() const {
    const TxActualType* type = this;
    if ( type->is_abstract() )
        return false;
    while ( type->is_equivalent_derivation() ) {
        type = type->get_base_type();
        if ( type->is_abstract() )
            return false;
    }

    if ( has_nonref_params( type ) )
        return false;  // if only Ref-constrained parameters, then being generic doesn't cause it to be non-concrete
    if ( has_outer_with_nonref_params( type ) )
        return false;
    return !is_dynamic_binding_dependent( type );
}

bool TxActualType::is_dynamic() const {
    const TxActualType* type = this;
    if ( type->is_abstract() )
        return false;
    while ( type->is_equivalent_derivation() ) {
        type = type->get_base_type();
        if ( type->is_abstract() )
            return false;
    }

    if ( has_nonref_params( type ) )
        return false;  // if only Ref-constrained parameters, then being generic doesn't cause it to be non-concrete
    if ( has_outer_with_nonref_params( type ) )
        return false;
    return is_dynamic_binding_dependent( type );
}

bool TxActualType::is_generic_dependent() const {
    if ( this->is_generic() )
        return true;
    return this->get_declaration()->get_definer()->context().is_generic();
}

bool TxActualType::is_empty_derivation() const {
    return this->emptyDerivation;
}

bool TxActualType::is_equivalent_derivation() const {
    return this->is_same_vtable_type() && this->is_same_instance_type();
}

bool TxActualType::is_virtual_derivation() const {
    return this->is_same_instance_type();
}

bool TxActualType::is_scalar() const {
    switch ( this->formalTypeId ) {
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
        return ( this->is_modifiable() && this->get_base_type()->is_scalar() );
    }
}

//bool TxActualType::is_mutable() const {
// In temporary version below, specializations were implicitly immutable if one or more of their TYPE arguments were immutable,
//    if ( !this->is_declared_mutable() )
//        return false;
//    if ( this->get_type_class() == TXTC_REFERENCE )
//        return true;
//    for ( auto b : this->get_bindings() ) {
//        if ( auto t = dynamic_cast<const TxTypeDeclaration*>( b ) ) {
//            if ( auto btype = t->get_definer()->resolve_type() ) {
//                if ( !btype->is_modifiable() ) {
//                    if ( !( btype->get_declaration()->get_decl_flags() & TXD_GENPARAM ) )
//                        return false;
//                }
//            }
//        }
//    }
//    return true;
//}

const TxActualType* TxActualType::get_instance_base_type() const {
    return ( this->is_same_instance_type() ? this->get_semantic_base_type()->get_instance_base_type() : this );
}

TxEntitySymbol* TxActualType::get_instance_member( const std::string& name ) const {
    return this->get_instance_member( this->get_declaration()->get_symbol(), name );
}

TxEntitySymbol* TxActualType::get_instance_member( TxScopeSymbol* vantageScope, const std::string& name ) const {
    if ( auto member = lookup_member( vantageScope, this->get_declaration()->get_symbol(), name ) ) {
        if ( auto memberEnt = dynamic_cast<TxEntitySymbol*>( member ) )
            return memberEnt;
        else
            LOG( this->LOGGER(), WARN, "Looked-up member is not an entity: " << member );
    }
    return nullptr;
}

TxEntitySymbol* TxActualType::lookup_inherited_instance_member( const std::string& name ) const {
    return this->lookup_inherited_instance_member( this->get_declaration()->get_symbol(), name );
}

TxEntitySymbol* TxActualType::lookup_inherited_instance_member( TxScopeSymbol* vantageScope, const std::string& name ) const {
    ASSERT( name != CONSTR_IDENT, "Can't look up constructors as *inherited* members; in: " << this );
    for ( const TxActualType* type = this; type; type = type->get_base_type() ) {
        if ( auto memberEnt = type->get_instance_member( vantageScope, name ) )
            return memberEnt;
        for ( auto & interfSpec : type->interfaces ) {
            if ( auto memberEnt = interfSpec.type->lookup_inherited_instance_member( vantageScope, name ) )
                return memberEnt;
        }
    }
    return nullptr;
}

static const TxEntityDeclaration* get_type_param_decl( const std::vector<const TxEntityDeclaration*>& params, const std::string& fullParamName ) {
    for ( auto & paramDecl : params )
        if ( fullParamName == paramDecl->get_unique_full_name() )
            return paramDecl;
    return nullptr;
}

static TxEntitySymbol* lookup_inherited_binding( const TxActualType* type, const std::string& fullParamName ) {
    TxIdentifier ident( fullParamName );
    auto parentName = ident.parent().str();
    auto paramName = ident.name();
    const TxActualType* semBaseType = type->get_semantic_base_type();
    while ( semBaseType ) {
        if ( get_type_param_decl( semBaseType->get_type_params(), fullParamName ) ) {
            // semBaseType is the (nearest) type that declares the sought parameter
            if ( auto binding = type->get_binding( paramName ) )
                return binding->get_symbol();
        }
        else if ( semBaseType->get_declaration()->get_unique_full_name() == parentName )
            LOG( type->LOGGER(), WARN, "Type parameter apparently unbound: " << fullParamName );

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
        if ( dynamic_cast<const TxTypeDeclaration*>( paramDecl ) )
            return bindingSymbol->get_type_decl();
        else
            return bindingSymbol->get_first_field_decl();
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
        if ( this->baseTypeSpec.modifiable == thatType->baseTypeSpec.modifiable
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
                        //std::cerr << "### comparing bindings.. " << aEntDecl->get_definer()->resolve_type()->type()->str(false) << std::endl
                        //          << "                         " << bEntDecl->get_definer()->resolve_type()->type()->str(false) << std::endl;
                        return ( aEntDecl->get_definer()->resolve_type()->type() == bEntDecl->get_definer()->resolve_type()->type() );
                    }
                    else if (auto aInitExpr = static_cast<const TxFieldDeclaration*>( aEntDecl )->get_definer()->get_init_expression()) {
                        if (auto bInitExpr = static_cast<const TxFieldDeclaration*>( bEntDecl )->get_definer()->get_init_expression()) {
                            if ( aInitExpr->is_statically_constant() && bInitExpr->is_statically_constant() ) {
                                if ( auto aBindExprType = aInitExpr->attempt_get_type() ) {
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
    return thisType->inner_equals( thatType );
}

bool TxActualType::is_assignable_to( const TxActualType& destination ) const {
    // fields must at least be the same instance data type
    // modifiability is disregarded (since this is in the context of copy-by-value)
    auto thisType = this;
    auto destType = &destination;
    if ( thisType->is_modifiable() )
        thisType = thisType->get_base_type();
    if ( destType->is_modifiable() )
        destType = destType->get_base_type();
    while ( destType->is_empty_derivation() && !is_explicit_nongen_declaration( destType ) )
        destType = destType->get_base_type();
    if ( thisType->get_type_class() != destType->get_type_class() )
        return false;
    do {
        //std::cerr << thisType << "  IS-ASSIGNABLE-TO\n" << destType << std::endl;
        if ( thisType->inner_equals( destType ) )
            return true;
        if ( thisType->inner_is_assignable_to( destType ) )
            return true;
        if ( !thisType->is_same_instance_type() )
            return false;
        thisType = thisType->get_base_type();
    } while ( true );
}

bool TxActualType::inner_is_assignable_to( const TxActualType* dest ) const {
    return false;
}

/** Returns the common base type of the types, if both are pure specializations of it. */
const TxActualType* TxActualType::common_generic_base_type( const TxActualType* thisType, const TxActualType* thatType ) {
    // find the nearest explicitly declared generic base type of each type and check if they're the same one:
    // (type parameters can only be present on explicitly declared types)
    while ( !thisType->get_explicit_declaration() && !thisType->get_bindings().empty() )
        thisType = thisType->get_semantic_base_type();
    while ( !thatType->get_explicit_declaration() && !thatType->get_bindings().empty() )
        thatType = thatType->get_semantic_base_type();
    if ( thisType->inner_equals( thatType ) )
        return thisType;
    return nullptr;
}

static inline const TxEntityDeclaration* get_binding_or_parameter( const TxActualType* type, const TxEntityDeclaration* paramDecl ) {
    if ( auto binding = type->lookup_param_binding( paramDecl ) )
        return binding;
    return paramDecl;
}

bool TxActualType::inner_is_a( const TxActualType* thisType, const TxActualType* thatType ) {
    //std::cerr << thisType << "  IS-A\n" << thatType << std::endl;
    // by-pass anonymous, empty specializations:
    while ( !is_explicit_nongen_declaration( thisType ) && thisType->is_empty_derivation() )
        thisType = thisType->get_base_type();

    if ( thisType->inner_equals( thatType ) )
        return true;

    // check whether other is a more generic version of the same type:
    if ( thisType->is_gen_or_spec() && thatType->is_gen_or_spec() ) {
        if ( auto genBaseType = common_generic_base_type( thisType, thatType ) ) {
            //std::cerr << "Common generic base type " << genBaseType << std::endl << "\tthisType: " << thisType << std::endl << "\tthatType: " << thatType << std::endl;
            for ( auto paramDecl : genBaseType->get_type_params() ) {
                // each of other's type param shall either be unbound (redeclared) or *equal* to this type's param/binding
                // (is-a is not sufficient in general case)
                if ( auto thatBinding = thatType->lookup_param_binding( paramDecl ) ) {
                    if ( dynamic_cast<const TxTypeDeclaration*>( thatBinding ) ) {
                        // other has bound this TYPE param - check that it matches this type's param/binding
                        // - a MOD binding is considered to be is-a of a non-MOD binding
                        // - a binding may be to a type that is equal to the parameter's constraint type, i.e. equivalent to unbound parameter
                        auto thisBindPar = get_binding_or_parameter( thisType, paramDecl );
                        // check whether both resolve to same type/value:
                        auto thisBType = thisBindPar->get_definer()->resolve_type();
                        auto thatBType = thatBinding->get_definer()->resolve_type();
                        if ( thisBType->is_modifiable() && !thatBType->is_modifiable() )
                            thisBType = thisBType->get_base_type();
                        if ( *thisBType != *thatBType )
                            return false;
                    }
                    else {
                        // other has bound this VALUE param - must match this type's VALUE binding exactly
                        bool staticEqual = false;
                        auto thatFieldBinding = static_cast<const TxFieldDeclaration*>( thatBinding );
                        if ( auto thisFieldBinding = dynamic_cast<const TxFieldDeclaration*>( thisType->lookup_param_binding( paramDecl ) ) ) {
                            thisFieldBinding->get_definer()->resolve_type();
                            thatFieldBinding->get_definer()->resolve_type();
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
        for ( auto & interfSpec : thisType->interfaces ) {
            if ( inner_is_a( interfSpec.type, thatType ) )
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

bool TxActualType::is_a( const TxActualType& other ) const {
    const TxActualType* thisType = this;
    const TxActualType* thatType = &other;
    //std::cerr << thisType << "  IS-A\n" << thatType << std::endl;

    // compare modifiability:
    if ( thisType->is_modifiable() ) {
        thisType = thisType->get_base_type();
        if ( thatType->is_modifiable() )
            thatType = thatType->get_base_type();
    }
    else if ( thatType->is_modifiable() )
        return false;  // a non-modifiable type "is not a" modifiable type

    // by-pass anonymous, empty specializations:
    while ( !is_explicit_nongen_declaration( thatType ) && thatType->is_empty_derivation() )
        thatType = thatType->get_base_type();

    return inner_is_a( thisType, thatType );
}

std::string TxActualType::str() const {
    return this->str( true );
}

//static void type_params_string(std::stringstream& str, const std::vector<const TxEntityDeclaration*>& params) {
//    str << "<";
//    int ix = 0;
//    for (auto & p : params) {
//        if (ix++)  str << ",";
//        str << p->get_unique_name();
//    }
//    str << ">";
//}

static void type_bindings_string( std::stringstream& str, const std::vector<const TxEntityDeclaration*>& bindings ) {
    str << " <";
    int ix = 0;
    for ( auto b : bindings ) {
        if ( ix++ )
            str << ",";
        if ( auto valB = dynamic_cast<const TxFieldDeclaration*>( b ) ) {
            if ( auto initializer = valB->get_definer()->get_init_expression() ) {
                if ( initializer->is_statically_constant() ) {
                    // existing binding has statically constant value
                    // TODO: handle constants of different types
                    str << eval_unsigned_int_constant( initializer );
                    continue;
                }
            }
            str << "?";
        }
        else if ( auto btype = b->get_definer()->attempt_get_type() )
            str << btype->str( true );
        else
            str << b->get_unique_full_name();
    }
    str << ">";
}

std::string TxActualType::str( bool brief ) const {
    std::stringstream str;
    this->self_string( str, brief );
    if ( !brief ) {
        if ( this->get_type_class() == TXTC_INTERFACE )
            str << " i/f";
        else if ( this->get_type_class() == TXTC_INTERFACEADAPTER )
            str << " i/f/ad";
        if ( this->is_mutable() )
            str << " MUT";
    }
    return str.str();
}

void TxActualType::self_string( std::stringstream& str, bool brief ) const {
    bool expl = !( this->get_declaration()->get_decl_flags() & ( TXD_IMPLICIT | TXD_GENPARAM | TXD_GENBINDING ) );

    if ( !expl && this->is_modifiable() ) {
        str << "MOD ";
        this->get_base_type()->self_string( str, brief );  // bypass modifiable derivation
        return;
    }

    str << this->get_declaration()->get_unique_full_name();

    if ( !this->hasInitialized ) {
        str << " -uninitialized-";
        return;
    }

    if ( !expl && !brief ) {
        //if (! this->params.empty())
        //    type_params_string(str, this->params);
        if ( !this->get_bindings().empty() ) {
            type_bindings_string( str, this->get_bindings() );
        }

        if ( this->has_base_type() ) {
            str << ( this->is_empty_derivation() ? " = " : " : " );

            this->get_semantic_base_type()->self_string( str, false );  // set 'brief' to false to print entire type chain
        }
    }
    else if ( this->get_type_class() == TXTC_REFERENCE || this->get_type_class() == TXTC_ARRAY ) {
        if ( !this->get_bindings().empty() ) {
            type_bindings_string( str, this->get_bindings() );
        }
    }
}

/*=== ArrayType and ReferenceType implementation ===*/

static bool array_assignable_from( const TxArrayType* toArray, const TxArrayType* fromArray ) {
    // if origin has unbound type params that destination does not, origin is more generic and can't be assigned to destination
    if ( auto toElem = toArray->element_type() ) {
        if ( auto fromElem = fromArray->element_type() ) {
            // note: is-a test insufficient for array elements, since assignable type (same instance data type) required
            if ( !fromElem->is_assignable_to( *toElem ) )
                return false;
        }
        else
            return false;  // origin has not bound E
    }
    if ( auto lenExpr = toArray->capacity() ) {
        if ( auto otherLenExpr = fromArray->capacity() ) {
            return ( lenExpr->is_statically_constant() && otherLenExpr->is_statically_constant()
                     && ( eval_unsigned_int_constant( lenExpr ) == eval_unsigned_int_constant( otherLenExpr ) ) );
        }
        else
            return false;  // origin has not bound C
    }
    return true;
}

static bool ref_assignable_from( const TxReferenceType* toRef, const TxReferenceType* fromRef ) {
    // if origin has unbound type params that destination does not, origin is more generic and can't be assigned to destination
    if ( auto toTarget = toRef->target_type() ) {
        if ( auto fromTarget = fromRef->target_type() ) {
            // is-a test sufficient for reference targets (it isn't for arrays, which require same concrete type)
            //std::cerr << "CHECKING REF ASSIGNABLE\n\tFROM " << fromTarget->str(false) << "\n\tTO   " << toTarget->str(false) << std::endl;
            if ( toTarget->is_modifiable() && !fromTarget->is_modifiable() )
                return false;  // can't lose non-modifiability of target type
            if ( fromTarget->is_a( *toTarget ) )
                return true;
            else
                return false;
        }
        else
            return false;  // origin has not bound T
    }
    else
        return true;
}

bool TxArrayType::inner_is_assignable_to( const TxActualType* destination ) const {
    auto toArray = static_cast<const TxArrayType*>( destination );
    return array_assignable_from( toArray, this );
}

bool TxReferenceType::inner_is_assignable_to( const TxActualType* destination ) const {
    auto toRef = static_cast<const TxReferenceType*>( destination );
    return ref_assignable_from( toRef, this );
}

//void TxArrayType::self_string( std::stringstream& str, bool brief ) const {
//    if (! (this->get_declaration()->get_decl_flags() & ( TXD_IMPLICIT ) ))
//        str << this->get_declaration()->get_unique_full_name() << " : ";
//    if (auto len = this->capacity()) {
//        if ( len->is_statically_constant() )
//            str << "[" << eval_UInt_constant( len ) << "] ";
//        else
//            str << "[?] ";
//    }
//    else
//        str << "[] ";
//    const TxType* elemType;  // note, we don't force actual type resolve from here
//    if (auto paramDecl = this->lookup_type_param_binding("tx.Array.E"))
//        elemType = paramDecl->get_definer()->resolve_type();
//    else
//        elemType = this->get_declaration()->get_symbol()->get_root_scope()->registry().get_builtin_type( ANY );  // we know the basic constraint type is Any
//    str << elemType->str( false );
//}
//
//void TxReferenceType::self_string( std::stringstream& str, bool brief ) const {
//    if (! (this->get_declaration()->get_decl_flags() & ( TXD_IMPLICIT ) ))
//        str << this->get_declaration()->get_unique_full_name() << " : ";
//    const TxType* targetType;  // note, we don't force actual type resolve from here
//    if (auto paramDecl = this->lookup_type_param_binding("tx.Ref.T"))
//        targetType = paramDecl->get_definer()->resolve_type();
//    else
//        targetType = this->get_declaration()->get_symbol()->get_root_scope()->registry().get_builtin_type( ANY );  // we know the basic constraint type is Any
//    std::cerr << "REF  " << this->get_declaration() << std::endl;
//    str << "& " << targetType->str();
//}

const TxExpressionNode* TxArrayType::capacity() const {
    if ( auto bindingDecl = this->lookup_value_param_binding( "tx.Array.C" ) ) {
        return bindingDecl->get_definer()->get_init_expression();
    }
    return nullptr;
}

const TxActualType* TxArrayType::element_type() const {
    if ( auto entSym = this->lookup_inherited_instance_member( "tx#Array#E" ) ) {
        if ( auto typeDecl = entSym->get_type_decl() ) {
            //std::cerr << "Array.E type decl: " << typeDecl << std::endl;
            return typeDecl->get_definer()->resolve_type()->type();
        }
    }
    LOG( this->LOGGER(), ERROR, "tx#Array#E not found in " << this );
//    if (auto bindingDecl = this->lookup_type_param_binding("tx.Array.E")) {
//        return bindingDecl->get_definer()->resolve_type()->type();
//    }
    return this->get_root_any_type();  // we know the basic constraint type for element is Any
}

const TxActualType* TxReferenceType::target_type() const {
    if ( auto entSym = this->lookup_inherited_instance_member( "tx#Ref#T" ) ) {
        if ( auto typeDecl = entSym->get_type_decl() ) {
            //std::cerr << "Ref.T type decl: " << typeDecl << std::endl;
            return typeDecl->get_definer()->resolve_type()->type();
        }
    }
    LOG( this->LOGGER(), ERROR, "tx#Ref#T not found in " << this );
//    if (auto paramDecl = this->lookup_type_param_binding("tx.Ref.T")) {
//        return paramDecl->get_definer()->resolve_type()->type();
//    }
    return this->get_root_any_type();  // we know the basic constraint type for ref target is Any
}

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
            // FIXME: verify that type matches
            this->virtualFields.override_field( f.first, targetField );
        }
    }

    return rec;
}

TxFunctionType::TxFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                const std::vector<const TxActualType*>& argumentTypes,
                                bool modifiableClosure )
        : TxFunctionType( declaration, baseType, argumentTypes,
                          baseType->get_declaration()->get_symbol()->get_root_scope()->registry().get_builtin_type( TXBT_VOID )->type(),
                          modifiableClosure ) {
}

bool TxFunctionType::inner_is_assignable_to( const TxActualType* other ) const {
    auto otherF = static_cast<const TxFunctionType*>( other );
    //std::cerr << "ASSIGNABLE RETURN TYPES?\n\t" << this->returnType << "\n\t" << otherF->returnType << std::endl;
    return ( ( this->returnType == otherF->returnType
               || ( this->returnType->is_assignable_to( *otherF->returnType ) ) )
             && this->argumentTypes.size() == otherF->argumentTypes.size()
             && std::equal( this->argumentTypes.cbegin(), this->argumentTypes.cend(),
                            otherF->argumentTypes.cbegin(),
                            [](const TxActualType* ta, const TxActualType* oa) {return oa->is_assignable_to( *ta );} ) );
    return false;
}

const TxActualType* TxFunctionType::vararg_elem_type() const {
    if ( !argumentTypes.empty() ) {
        auto lastArgType = argumentTypes.back();
        if ( lastArgType->get_type_class() == TXTC_REFERENCE ) {
            auto refTargetType = static_cast<const TxReferenceType*>( lastArgType )->target_type();
            if ( refTargetType->get_type_class() == TXTC_ARRAY ) {
                auto arrayType = static_cast<const TxArrayType*>( refTargetType );
                if ( !arrayType->capacity() )  // only arrays of unspecified capacity apply to var-args syntactic sugar
                    return arrayType->element_type();
            }
        }
    }
    return nullptr;
}

const TxArrayType* TxFunctionType::fixed_array_arg_type() const {
    if ( argumentTypes.size() == 1 ) {
        auto argType = argumentTypes.back();
        if ( argType->get_type_class() == TXTC_ARRAY ) {
            auto arrayType = static_cast<const TxArrayType*>( argType );
            if ( auto lenExpr = arrayType->capacity() ) {
                if ( lenExpr->is_statically_constant() ) {
                    return arrayType;
                }
            }
        }
    }
    return nullptr;
}

TxExpressionNode* TxBuiltinConversionFunctionType::make_inline_expr( TxExpressionNode* calleeExpr,
                                                                     std::vector<TxMaybeConversionNode*>* argsExprList ) const {
    return make_conversion( argsExprList->front(), this->returnType->get_type_entity(), true );
}

TxExpressionNode* TxBuiltinArrayInitializerType::make_inline_expr( TxExpressionNode* calleeExpr,
                                                                   std::vector<TxMaybeConversionNode*>* argsExprList ) const {
    return argsExprList->front();
}
