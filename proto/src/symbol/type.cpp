#include "util/logging.hpp"

#include "tx_error.hpp"

#include "type.hpp"
#include "ast.hpp"
#include "entity.hpp"


bool TxConstantProxy::operator==(const TxConstantProxy& other) const {
    // simple since we so far only support UInt values
    // FUTURE: improve as more value getters added (note that auto-conversion needs to be supported)
    return this->get_value_UInt() == other.get_value_UInt();
}


bool DataTupleDefinition::add_interface_fields(const DataTupleDefinition& interfaceFields) {
    bool added = false;
    for (auto & f : interfaceFields.fields) {
        if (f->get_unique_name() == "$adTypeId")
            continue;
        if (! this->has_field(f->get_unique_name())) {
            this->add_field(f);
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
    for (auto & f : this->fields) {
        fprintf(stderr, "%-2d: %20s: %s\n", ix, f->get_unique_name().c_str(), f->str().c_str());
        ix++;
    }
}



/*=== TxTypeSpecialization implementation ===*/

bool TxTypeSpecialization::operator==(const TxTypeSpecialization& other) const {
    return ( this->type == other.type
             || ( this->type && other.type && *this->type == *other.type) )
           //&& *this->dataspace == *other.dataspace
           && this->modifiable == other.modifiable;
}

std::string TxTypeSpecialization::str() const { return "specialization of " + this->type->str(); }



/*=== TxActualType implementation ===*/

Logger& TxActualType::_LOG = Logger::get("ENTITY");

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


const TxLocation& TxActualType::get_parse_location() const {
    return this->get_declaration()->get_definer()->get_parse_location();
}

ExpectedErrorClause* TxActualType::exp_err_ctx() const {
    return this->get_declaration()->get_definer()->exp_err_ctx();
}



void TxActualType::validate_type() const {
    //std::cerr << "validating type " << this << std::endl;
    if (this->baseTypeSpec.type) {
        if (this->baseTypeSpec.type->is_modifiable())
            CERROR(this, "Can't specialize a 'modifiable' type (specialize its base type instead): " << this->baseTypeSpec.type);

        if (this->baseTypeSpec.modifiable) {
            ASSERT(this->get_type_class() == this->baseTypeSpec.type->get_type_class(),
                   "'modifiable' specialization must have same TxActualType class as the base type: " << this->baseTypeSpec.type);
            if (this->baseTypeSpec.type->is_immutable())
                CERROR(this, "Can't make an immutable type modifiable: " << this->baseTypeSpec.type);
            //if (this->dataspace)
            //    CERROR(this->type, "Can't specify dataspace for a 'modifiable' type specialization");

            // verify that this 'modifiable' type usage is an 'equivalent' specialization
            if (! this->interfaces.empty())
                CERROR(this, "'modifiable' specialization cannot add any interface base types");
        }
        else {
            ASSERT(this->baseTypeSpec.type->staticTypeId == ANY
                   || (this->get_type_class() == TXTC_INTERFACEADAPTER && this->baseTypeSpec.type->get_type_class() == TXTC_INTERFACE)
                   || this->get_type_class() == this->baseTypeSpec.type->get_type_class(),
                   "Specialized type's type class " << this << " not valid with base type's type class " << this->baseTypeSpec.type->get_type_class());
        }
//        if (this->dataspace && this->baseTypeSpec.type->get_type_class() != TXTC_REFERENCE)
//            CERROR(this, "Specified dataspace for non-reference base type " << this->baseTypeSpec.type);

        if (this->baseTypeSpec.type->is_empty_derivation() && !this->baseTypeSpec.type->get_explicit_declaration()) {
            ASSERT(! (this->is_empty_derivation() && !this->get_explicit_declaration()),
                   "anonymous or implicit, empty types may not be derived except as another anonymous or implicit, empty type: " << this);
        }

        // if this is not an empty nor a modifiable derivation, verify that all parameters of base type are either bound, or redeclared:
        // Note: The base type's parameters that have not been bound should normally be automatically redeclared by the type registry.
        if (!this->emptyDerivation && !this->is_modifiable()) {
            for (auto & paramDecl : this->get_semantic_base_type()->type_params()) {
                if (! this->get_binding(paramDecl->get_unique_name())) {
                    if (! this->has_type_param(paramDecl->get_unique_name())) {
                        //this->params.emplace_back(paramDecl);
                        if (this->get_type_class() != TXTC_INTERFACEADAPTER) {
                            CERROR(this, "Missing binding or redeclaration of base type's type parameter "
                                   << paramDecl->get_unique_name() << " in " << this);
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
    for (auto & interfSpec : this->interfaces) {
        if (interfSpec.type->get_type_class() != TXTC_INTERFACE)
            CERROR(this, "Only the first derived-from type can be a non-interface type: " << interfSpec.type);
    }
}


void TxActualType::initialize_type() {
    LOG_TRACE(this->LOGGER(), "Initializing type " << this);

    ASSERT(this->declaration, "No declaration for actual type " << this);

    auto typeDeclNamespace = this->get_declaration()->get_symbol();
    // determine generic base type, if any:
    const TxActualType* semBaseType = this->baseTypeSpec.type;
    if (auto entitySym = dynamic_cast<TxEntitySymbol*>(typeDeclNamespace->get_member_symbol("$GenericBase"))) {
        if (auto typeDecl = entitySym->get_type_decl()) {
            this->genericBaseType = typeDecl->get_definer()->resolve_type()->type();
            semBaseType = this->genericBaseType;
        }
    }

    // perform shallow pass on type's member declarations to determine derivation characteristics:
    bool hasExplicitFieldMembers = false;
    bool hasImplicitFieldMembers = false;
    for (auto symname = typeDeclNamespace->decl_order_names_cbegin(); symname != typeDeclNamespace->decl_order_names_cend(); symname++) {
        if (auto entitySym = dynamic_cast<TxEntitySymbol*>(typeDeclNamespace->get_member_symbol(*symname))) {
            if (auto typeDecl = entitySym->get_type_decl()) {
                if (typeDecl->get_decl_flags() & TXD_GENPARAM) {
                    this->params.emplace_back( typeDecl );
                    //std::cerr << "FOUND TYPE GENPARAM: " << typeDecl << std::endl;
                }
                else if (typeDecl->get_decl_flags() & TXD_GENBINDING) {
                    this->bindings.emplace_back( typeDecl );
                    //std::cerr << "FOUND TYPE GENBINDING: " << typeDecl << std::endl;
                }
            }

            for (auto fieldDeclI = entitySym->fields_cbegin(); fieldDeclI != entitySym->fields_cend(); fieldDeclI++) {
                auto fieldDecl = *fieldDeclI;

                if (fieldDecl->get_decl_flags() & TXD_IMPLICIT)
                    hasImplicitFieldMembers = true;
                else
                    hasExplicitFieldMembers = true;

                if (fieldDecl->get_decl_flags() & TXD_GENPARAM) {
                    //std::cerr << "FOUND VALUE GENPARAM: " << typeDecl << std::endl;
                    this->params.emplace_back( fieldDecl );
                }
                else if (fieldDecl->get_decl_flags() & TXD_GENBINDING) {
                    //std::cerr << "FOUND VALUE GENBINDING: " << typeDecl << std::endl;
                    this->bindings.emplace_back( fieldDecl );
                }

                switch (fieldDecl->get_storage()) {
                case TXS_INSTANCE:
                    // Note: VALUE bindings are only declared as instance members in generic base type,
                    // so that they are not "extensions" to the specialized subtypes.
                    if (! (fieldDecl->get_decl_flags() & TXD_GENBINDING)) {
                        this->extendsInstanceDatatype = true;
                    }
                    break;
                case TXS_INSTANCEMETHOD:
                    if (fieldDecl->get_decl_flags() & TXD_CONSTRUCTOR)
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

    if (! hasExplicitFieldMembers) {
        if (! this->bindings.empty()) {
            this->pureDerivation = true;
        }
        else if (! this->is_builtin() && ! this->is_modifiable() && this->interfaces.empty() && this->params.empty()) {
            if (this->get_type_class() == TXTC_FUNCTION) {
                // do something?
            }
            else if (this->get_type_class() == TXTC_INTERFACEADAPTER) {
            }
            else {
                this->emptyDerivation = true;
                if (hasImplicitFieldMembers) {
                    LOG(this->LOGGER(), NOTE, "Type with only implicit field members: " << this);
                }
            }
        }
    }

    { // validate the type parameter bindings (as much as we can without resolving this type's bindings at this point)
        for (auto & bindingDecl : this->bindings) {
            auto pname = bindingDecl->get_unique_name();
            if (auto paramDecl = semBaseType->get_type_param_decl( pname )) {
                auto constraintType = paramDecl->get_definer()->resolve_type()->type();
                ASSERT(constraintType, "NULL constraint type for param " << paramDecl << " of " << semBaseType);
//                    auto boundType = bindingDecl->get_definer()->resolve_type();
//                    ASSERT(boundType,      "NULL binding type for param " << paramDecl << " of " << basetype);
                //std::cerr << this << ": Constraint type for param " << paramDecl << ": " << "checking bound type "
                //          << boundType << "\tagainst constraint type " << constraintType << std::endl;

                if (dynamic_cast<const TxTypeDeclaration*>(paramDecl)) {
                    if (! dynamic_cast<const TxTypeDeclaration*>(bindingDecl))
                        CERROR(bindingDecl->get_definer(), "Binding for type parameter " << paramDecl << " is not a type: " << bindingDecl);

//                        if (! boundType->is_a(*constraintType))
//                            // TO DO: do this also for VALUE params, but array type expression needs auto-conversion support for that to work
//                            CERROR(bindingDecl->get_definer(), "Bound type " << boundType << " for type parameter " << paramDecl
//                                                               << " is not a derivation of contraint type " << constraintType);
                    if (constraintType->get_type_class() != TXTC_REFERENCE)
                        this->nonRefBindings = true;
                }
                else {
                    if (! dynamic_cast<const TxFieldDeclaration*>(bindingDecl))
                        CERROR(bindingDecl->get_definer(), "Binding for type parameter " << paramDecl << " is not a field/value: " << bindingDecl);

                    this->nonRefBindings = true;
                }
            }
            else
                CERROR(bindingDecl->get_definer(), "No type parameter of " << semBaseType << " matches provided binding " << bindingDecl->get_unique_name());
        }
    }


    // determine datatype change:
    if (this->is_modifiable()) {
        // a modifiable type is a usage form of its base type, and doesn't affect the instance nor the vtable type
    }
    else {
        if (this->nonRefBindings) {
            // Binding of a base type parameter implies reinterpretation of its members and thus
            // the chance of modified instance / vtable types (for non-ref-constrained parameters).
            // Note, may cause false positives (a full graph analysis of contained members would be needed for full accuracy)
            this->extendsInstanceDatatype = true;
            this->modifiesVTable = true;
        }
        else if (this->is_builtin()) {
            // Built-in implies a distinct instance type compared to the base type.
            this->extendsInstanceDatatype = true;
        }
        else if (this->get_type_class() == TXTC_FUNCTION) {
            // function type implies a distinct instance type compared to the base type (for now)
            this->extendsInstanceDatatype = true;
        }
        else if (this->get_type_class() == TXTC_INTERFACEADAPTER) {
            this->modifiesVTable = true;
        }

        if (! this->interfaces.empty()) {
            // If there are interfaces we assume that will cause the vtable will be extended in preparation.
            // This may cause false positives, but we need to determine this flag in the type's initialization phase.
            this->modifiesVTable = true;
        }
    }

    this->hasInitialized = true;

    this->validate_type();
}



bool TxActualType::prepare_members() {
    if (!this->hasPrepared) {
        if (this->startedPrepare) {
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
    LOG(this->LOGGER(), TRACE, "Preparing members of type " << this);
    bool recursionError = false;

    bool expErrWholeType = ( this->get_declaration()->get_decl_flags() & TXD_EXPERRBLOCK );
    ScopedExpErrClause scopedEEWholeType( this->get_declaration()->get_definer(), expErrWholeType );


    // copy base type's virtual and instance field tuples (to which fields may be added / overridden):
    auto baseType = this->get_base_type();
    if (baseType) {
        //ASSERT(baseType->is_prepared(), "Base type " << baseType << " not prepared before sub type " << this);
        recursionError = const_cast<TxActualType*>( baseType )->prepare_members();
        this->virtualFields = baseType->virtualFields;
        this->instanceFields = baseType->instanceFields;
    }
    for (auto & interfSpec : this->interfaces) {
        //ASSERT(interfSpec.type->is_prepared(), "Base i/f " << interfSpec.type << " not prepared before sub type " << this);
        recursionError |= const_cast<TxActualType*>( baseType )->prepare_members();
        bool added = this->virtualFields.add_interface_fields( interfSpec.type->virtualFields );
        if (! added)
            LOG(this->LOGGER(), NOTE, "Type implements interfaces but they don't cause the vtable to be extended: " << this);
//        if (added)
//            this->modifiesVTable = true;
    }
    //std::cerr << "Inherited virtual fields of " << this << std::endl;
    //this->virtualFields.dump();


    auto semBaseType = this->get_semantic_base_type();

    // for all the member names declared or redeclared in this type:
    auto typeDeclNamespace = this->get_declaration()->get_symbol();
    for (auto symname = typeDeclNamespace->decl_order_names_cbegin(); symname != typeDeclNamespace->decl_order_names_cend(); symname++) {
        // this drives resolution of all this type's members

        auto entitySym = dynamic_cast<TxEntitySymbol*>(typeDeclNamespace->get_member_symbol(*symname));
        if (! entitySym)
            continue;

        // prepare type members:
        if (auto typeDecl = entitySym->get_type_decl()) {
            ScopedExpErrClause expErrClause( typeDecl->get_definer(), ( typeDecl->get_decl_flags() & TXD_EXPERRBLOCK ) );

            if (auto type = typeDecl->get_definer()->get_type()) {
                if (typeDecl->get_decl_flags() & TXD_GENBINDING) {
                    auto bname = typeDecl->get_unique_name();
                    if (auto paramDecl = semBaseType->get_type_param_decl( bname )) {
                        auto constraintType = paramDecl->get_definer()->get_type()->type();

                        if (! type->type()->is_a(*constraintType))
                            // TODO: do this also for VALUE params, but array type expression needs auto-conversion support for that to work
                            CERROR(typeDecl->get_definer(), "Bound type " << type << " for type parameter " << paramDecl
                                                            << " is not a derivation of contraint type " << constraintType);
                    }
                }
            }
        }

        // prepare field members:
        for (auto fieldDeclI = entitySym->fields_cbegin(); fieldDeclI != entitySym->fields_cend(); fieldDeclI++) {
            auto fieldDecl = *fieldDeclI;

            bool expErrField = ( fieldDecl->get_decl_flags() & TXD_EXPERRBLOCK );
            ScopedExpErrClause expErrClause( fieldDecl->get_definer(), expErrField );
            if (expErrField && ! fieldDecl->get_definer()->attempt_get_field()) {
                LOG_TRACE(this->LOGGER(), "Skipping preparation of EXPERR unresolved field " << fieldDecl);
                continue;
            }

            auto field = fieldDecl->get_definer()->get_field();
            auto fieldType = field->get_type()->type();

            { // validate field's type:
                if (! fieldType->is_concrete()) {
                    CERROR(field, "Can't declare a field of non-concrete type: " << field << " " << fieldType);
                }
                else if (fieldDecl->get_storage() == TXS_INSTANCE) {
                    if (! fieldType->is_statically_sized()) {
                        CERROR(field, "Instance fields that don't have statically determined size not yet supported: " << field);
                    }
                    else if (! (fieldDecl->get_decl_flags() & (TXD_GENPARAM | TXD_GENBINDING | TXD_IMPLICIT))) {
                        if (this->get_type_class() != TXTC_TUPLE)
                            //if (this->get_type_class() != TXTC_INTERFACE)  // (if not error already emitted above)
                                CERROR(field, "Can't declare instance member in non-tuple type: " << field);
                    }
                }
                else {  // static / virtual
                    if (! fieldType->is_statically_sized()) {
                        // since static fields are per generic base type, and not per specialization:
                        CERROR(field, "Static fields must have statically determined size: " << field);
                    }
                }
            }

            // validate field's storage and declaration flags, and do layout:
            switch (fieldDecl->get_storage()) {
            case TXS_INSTANCE:
                LOG_DEBUG(this->LOGGER(), "Laying out instance field " << field << "  " << this->instanceFields.get_field_count());
                if (fieldDecl->get_decl_flags() & TXD_ABSTRACT)
                    CERROR(field, "Can't declare an instance field abstract: " << field);

                // recursively prepare instance member fields' types so that we identify recursive data type definitions:
                //std::cerr << "Recursing into " << field << "  of type " << field->get_type() << std::endl;
                if (const_cast<TxActualType*>( fieldType )->prepare_members())
                    CERROR(field, "Recursive data type via field " << field->get_declaration()->get_unique_full_name());

                if (fieldDecl->get_decl_flags() & TXD_GENBINDING)
                    LOG_DEBUG(this->LOGGER(), "Skipping layout of GENBINDING instance field: " << field);
                else if (! expErrField || expErrWholeType)
                    this->instanceFields.add_field(field);
                break;
            case TXS_VIRTUAL:
            case TXS_INSTANCEMETHOD:
                ASSERT(! (fieldDecl->get_decl_flags() & TXD_INITIALIZER), "initializers can't be virtual/instance method: " << fieldDecl);
                if (fieldDecl->get_decl_flags() & TXD_CONSTRUCTOR)
                    break;  // skip, constructors aren't virtual

                if (fieldDecl->get_decl_flags() & TXD_ABSTRACT) {
                    if (this->get_type_class() != TXTC_INTERFACE && !(this->get_declaration()->get_decl_flags() & TXD_ABSTRACT))
                        CERROR(fieldDecl->get_definer(), "Can't declare abstract member '" << fieldDecl->get_unique_name() << "' in type that is not declared abstract: " << this);
                }

                if (entitySym->is_overloaded())
                    CERROR(field, "Overloading of virtual fields/methods not yet supported: " << field);

                if (this->virtualFields.has_field(field->get_unique_name())) {
                    if (! (fieldDecl->get_decl_flags() & TXD_OVERRIDE))
                        CWARNING(field, "Field overrides but isn't declared 'override': " << field);
                    auto overriddenField = this->virtualFields.get_field(field->get_unique_name());
                    if (overriddenField->get_decl_flags() & TXD_FINAL)
                        CERROR(field, "Can't override a base type field that is declared 'final': " << field);
                    if (! (field->get_type()->type()->is_assignable_to(*overriddenField->get_type()->type())))
                        CERROR(field, "Overriding member's type " << field->get_type() << std::endl
                                << "   not assignable to overridden member's type " << overriddenField->get_type());
                    if (! expErrField || expErrWholeType)
                        this->virtualFields.override_field(field->get_unique_name(), field);
                }
                else {
                    if (fieldDecl->get_decl_flags() & TXD_OVERRIDE)
                        CWARNING(field, "Field doesn't override but is declared 'override': " << field);
                    if (! expErrField || expErrWholeType)
                        this->virtualFields.add_field(field);
                }
                this->LOGGER()->debug("Adding/overriding virtual field %-40s  %s  %u", field->str().c_str(),
                                      field->get_type()->str().c_str(), this->virtualFields.get_field_count());
                break;
            default:
                ASSERT(fieldDecl->get_storage() == TXS_STATIC, "Invalid storage class " << fieldDecl->get_storage() << " for field member " << *field);
                if (fieldDecl->get_decl_flags() & TXD_INITIALIZER)
                    break;  // skip, initializers are inlined and not actually added as static functions

                if (fieldDecl->get_decl_flags() & TXD_ABSTRACT)
                    CERROR(field, "Can't declare a non-virtual field as abstract: " << field);
                if (fieldDecl->get_decl_flags() & TXD_OVERRIDE)
                    CWARNING(field, "Field doesn't override but is declared 'override': " << field);
                if (! expErrField || expErrWholeType)
                    this->staticFields.add_field(field);
            }
        }
    }

    if (!this->is_abstract() && !this->is_modifiable() && this->get_type_class() != TXTC_INTERFACEADAPTER
        && !( this->get_declaration()->get_decl_flags() & TXD_GENPARAM )) {
        // check that all abstract members of base types & interfaces are implemented:
        auto virtualFields = this->get_virtual_fields();
        for (auto & field : virtualFields.fieldMap) {
            auto actualFieldEnt = virtualFields.get_field(field.second);
            if (actualFieldEnt->get_decl_flags() & TXD_ABSTRACT) {
                CERROR(this, "Concrete type " << this->str() << " doesn't implement abstract member " << actualFieldEnt);
            }
        }
    }

    return recursionError;
}



bool TxActualType::is_abstract() const {
    return ( this->get_declaration()->get_decl_flags() & TXD_ABSTRACT );
}

bool TxActualType::is_concrete() const {
    // A concrete type is not declared abstract, nor usually generic (references may be concrete while generic).
    if (this->is_abstract())
        return false;
    if (this->typeClass == TXTC_ARRAY) {
        // If array of concrete elements with known length, return true
        auto array = static_cast<const TxArrayType*>( this );
        return ( array->element_type()->is_concrete() && array->length() );
    }
    if (this->typeClass != TXTC_REFERENCE && this->is_generic()) {
        // TODO: If all members concrete, then return true
        return false;
    }
    return true;
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

bool TxActualType::is_statically_sized() const {
    return this->is_concrete();
}


bool TxActualType::is_scalar() const {
    switch( this->staticTypeId ) {
    case SCALAR:
    case INTEGER:
    case SIGNED:
    case BYTE:
    case SHORT:
    case INT:
    case LONG:
    case UNSIGNED:
    case UBYTE:
    case USHORT:
    case UINT:
    case ULONG:
    case FLOATINGPOINT:
    case HALF:
    case FLOAT:
    case DOUBLE:
        return true;
    default:
        return ( this->is_modifiable() && this->get_base_type()->is_scalar() );
    }
}


const TxActualType* TxActualType::get_instance_base_type() const {
    return (this->is_same_instance_type() ? this->get_semantic_base_type()->get_instance_base_type() : this);
}


TxEntitySymbol* TxActualType::get_instance_member(const std::string& name) const {
    return this->get_instance_member( this->get_declaration()->get_symbol(), name );
}

TxEntitySymbol* TxActualType::get_instance_member(TxScopeSymbol* vantageScope, const std::string& name) const {
    if (auto member = lookup_member( vantageScope, this->get_declaration()->get_symbol(), name )) {
        if (auto memberEnt = dynamic_cast<TxEntitySymbol*>(member))
            return memberEnt;
        else
            LOG(this->LOGGER(), WARN, "Looked-up member is not an entity: " << member);
    }
    return nullptr;
}

TxEntitySymbol* TxActualType::lookup_inherited_instance_member(const std::string& name) const {
    return this->lookup_inherited_instance_member( this->get_declaration()->get_symbol(), name );
}

TxEntitySymbol* TxActualType::lookup_inherited_instance_member(TxScopeSymbol* vantageScope, const std::string& name) const {
    ASSERT(name != CONSTR_IDENT, "Can't look up constructors as *inherited* members; in: " << this);
    for (const TxActualType* type = this; type; type = type->get_base_type()) {
        if (auto memberEnt = type->get_instance_member(vantageScope, name))
            return memberEnt;
        for (auto & interfSpec : type->interfaces) {
            if (auto memberEnt = interfSpec.type->lookup_inherited_instance_member(vantageScope, name))
                return memberEnt;
        }
    }
    return nullptr;
}


static const TxEntityDeclaration* get_type_param_decl(const std::vector<const TxEntityDeclaration*>& params, const std::string& fullParamName) {
    for (auto & paramDecl : params)
        if (fullParamName == paramDecl->get_unique_full_name())
            return paramDecl;
    return nullptr;
}

static TxEntitySymbol* lookup_inherited_binding(const TxActualType* type, const std::string& fullParamName) {
    TxIdentifier ident( fullParamName );
    auto parentName = ident.parent().str();
    auto paramName = ident.name();
    const TxActualType* semBaseType = type->get_semantic_base_type();
    while (semBaseType) {
        if (get_type_param_decl(semBaseType->type_params(), fullParamName)) {
            // semBaseType is the (nearest) type that declares the sought parameter
            if (auto binding = type->get_binding(paramName))
                return binding->get_symbol();
        }
        else if (semBaseType->get_declaration()->get_unique_full_name() == parentName)
            LOG(type->LOGGER(), WARN, "Type parameter apparently unbound: " << fullParamName);

        type = type->get_base_type();
        semBaseType = type->get_semantic_base_type();
    }
    return nullptr;
}

const TxFieldDeclaration* TxActualType::lookup_value_param_binding(const std::string& fullParamName) const {
    if (auto bindingSymbol = lookup_inherited_binding(this, fullParamName))
        return bindingSymbol->get_first_field_decl();
    return nullptr;
}
const TxTypeDeclaration* TxActualType::lookup_type_param_binding(const std::string& fullParamName) const {
    if (auto bindingSymbol = lookup_inherited_binding(this, fullParamName))
        return bindingSymbol->get_type_decl();
    return nullptr;
}

const TxEntityDeclaration* TxActualType::lookup_param_binding(const TxEntityDeclaration* paramDecl) const {
    ASSERT(paramDecl->get_decl_flags() & TXD_GENPARAM, "Can't look up a binding for a 'param decl' that isn't GENPARAM: " << paramDecl);
    if (auto bindingSymbol = lookup_inherited_binding(this, paramDecl->get_unique_full_name())) {
        if (dynamic_cast<const TxTypeDeclaration*>(paramDecl))
            return bindingSymbol->get_type_decl();
        else
            return bindingSymbol->get_first_field_decl();
    }
    return nullptr;
}


//bool TxActualType::inner_equals(const TxActualType& otherType) const {
//    // note: both are assumed to have explicit declaration and/or be non-empty
//    // note: an implicit declaration is effectively unique if it is non-empty (we don't structurally compare them
//    // if either has an explicit declaration, they both must have the same explicit declaration
//    auto decl = this->get_declaration();
//    auto otherDecl = otherType.get_declaration();
//    if ( !( decl->get_decl_flags() & TXD_IMPLICIT ) || !( otherDecl->get_decl_flags() & TXD_IMPLICIT ) ) {
//        return decl == otherDecl;  // same explicit declaration
//    }
//
//    if ( decl || otherDecl ) {
//        // if either has an explicit declaration, they both must have the same declaration
//        return decl == otherDecl;  // same explicit declaration
//    }
//
//    if (explDecl)
//            return true;
//
//        if ( this->is_empty_derivation() && otherType.is_empty_derivation()
//             && this->baseTypeSpec == otherType.baseTypeSpec )  // non-explicitly declared but identical, empty specializations
//            return true;
//    }
//    return false;
//    // (interfaces and members can only apply to a type with an explicit declaration, and an explicit declaration can have only one type instance)
//}
bool TxActualType::inner_equals(const TxActualType& otherType) const {
    if (this == &otherType)
        return true;
    // note: both are assumed to have explicit declaration and/or be non-empty
    // note: an implicit declaration is effectively unique if it is non-empty (we don't structurally compare them
    // if either has an explicit declaration, they both must have the same explicit declaration
    auto explDecl = this->get_explicit_declaration();
    if (explDecl == otherType.get_explicit_declaration()) {  // same explicit declaration or both null
       if (explDecl)  // same explicit declaration
           return true;  // should this happen if instance equality above was false?

       // compare structure:
       if (this->baseTypeSpec == otherType.baseTypeSpec) {  // specializations of same base
           auto thisBinds = this->get_bindings();
           auto otherBinds = otherType.get_bindings();
           if (thisBinds.size() == otherBinds.size()) {
               if (thisBinds.size() == 0)
                   return true;
               // for now just return unequal for all instances with bindings
               std::cerr << "### structurally 'unequal': " << this << std::endl
                         << "                            " << otherType << std::endl;
//               return std::equal( thisBinds.cbegin(), thisBinds.cend(), otherBinds.cbegin(),
//                                  [](TxEntityDeclaration* aEntDecl, TxEntityDeclaration* bEntDecl) {
//                                      return TO DO;
//                                  } );
           }
       }
    }
    return false;
    // (interfaces and members can only apply to a type with an explicit declaration, and an explicit declaration can have only one type instance)
}

bool TxActualType::operator==(const TxActualType& other) const {
    // skips empty type derivations that aren't explicitly declared
    const TxActualType* thisType = this;
    const TxActualType* otherType = &other;
    while (!thisType->is_explicit_nongen_declaration() && thisType->is_empty_derivation())
        thisType = thisType->get_semantic_base_type();
    while (!otherType->is_explicit_nongen_declaration() && otherType->is_empty_derivation())
        otherType = otherType->get_semantic_base_type();
    return thisType->inner_equals(*otherType);
}


bool TxActualType::is_assignable_to(const TxActualType& destination) const {
    // fields must at least be the same instance data type
    // modifiability is disregarded (since this is in the context of copy-by-value)
    auto thisType = this;
    auto destType = &destination;
    if (thisType->is_modifiable())
        thisType = thisType->get_base_type();
    if (destType->is_modifiable())
        destType = destType->get_base_type();
    do {
        if (*thisType == *destType)
            return true;
        if (thisType->is_same_instance_type())
            thisType = thisType->get_base_type();
        else
            return false;
    } while (true);
}


/*
static std::string type_name(const TxActualType* type) {
    if (auto decl = type->get_declaration())
        return decl->to_string();
    else
        return typeid(*type).name();
}
static void print_hierarchy(const TxActualType* type)
{
    while (type) {
        if (type->is_empty_derivation())
            std::cerr << type_name(type) << "\t (EMPTY)" << std::endl;
        else if (type->is_modifiable())
            std::cerr << type_name(type) << "\t (MOD)" << std::endl;
        else
            std::cerr << type_name(type) << std::endl;
        type = type->get_semantic_base_type();
    }
}
*/


bool TxActualType::is_a(const TxActualType& other) const {
    //std::cerr << *this << "  IS-A\n" << other << std::endl;
//    ASSERT(! this->genericBaseType, "Invoked is_a() on concrete specialization type: " << this);
//    ASSERT(! other.genericBaseType, "Invoked is_a() with other being a concrete specialization type: " << this);

    const TxActualType* thisType = this;
    const TxActualType* otherType = &other;

    // compare modifiability:
    if (thisType->is_modifiable()) {
        thisType = thisType->get_base_type();
        if (otherType->is_modifiable())
            otherType = otherType->get_base_type();
    }
    else if (otherType->is_modifiable())
        return false;  // a non-modifiable type "is not a" modifiable type

    // by-pass anonymous, empty specializations:
    while (!thisType->is_explicit_nongen_declaration() && thisType->is_empty_derivation())
        thisType = thisType->get_semantic_base_type();
    while (!otherType->is_explicit_nongen_declaration() && otherType->is_empty_derivation())
        otherType = otherType->get_semantic_base_type();

    if (thisType->inner_equals(*otherType))
        return true;

    // check whether other is a more generic version of the same type:
    if (auto genBaseType = thisType->common_generic_base_type(*otherType)) {
        for (auto paramDecl : genBaseType->type_params()) {
            // other's param shall either be redeclared (generic) or *equal* to this (is-a is not sufficient in general case)
            // - a MOD binding is considered to be is-a of a non-MOD binding
            // TODO: more thorough analysis of which additional cases may be compatible
            if (auto otherBinding = otherType->lookup_param_binding(paramDecl)) {
                if (auto thisBinding = thisType->lookup_param_binding(paramDecl)) {
                    // check whether both bindings resolve to same type/value:
                    auto thisBType = thisBinding->get_definer()->resolve_type();
                    auto thatBType = otherBinding->get_definer()->resolve_type();
                    if (thisBType->is_modifiable() && !thatBType->is_modifiable())
                        thisBType = thisBType->get_base_type();
                    if (typeid(*thisBinding) != typeid(*otherBinding)  // both TYPE or both VALUE declarations
                            || *thisBType != *thatBType)
                        return false;
                }
                else
                    return false;
            }
        }
        return true;
    }

    // check whether any ancestor type is-a the other type:
    if (otherType->get_type_class() == TXTC_INTERFACE)
        return this->derives_interface(otherType);
    else
        return this->derives_object(otherType);
}

const TxActualType* TxActualType::common_generic_base_type(const TxActualType& other) const {
    // !!! this implementation assumes that if a type has bindings, it is a pure specialization !!!
    //std::cerr << "this: " << this << "   other: " << other << std::endl;
    const TxActualType* thisType = this;
    const TxActualType* otherType = &other;
    while (!thisType->is_explicit_nongen_declaration() && !thisType->get_bindings().empty())
        thisType = thisType->get_semantic_base_type();
    while (!otherType->is_explicit_nongen_declaration() && !otherType->get_bindings().empty())
        otherType = otherType->get_semantic_base_type();
    if (thisType->inner_equals(*otherType))
        return thisType;
//    if (! this->get_explicit_declaration() && this->is_pure_specialization())
//        return this->get_semantic_base_type()->common_generic_base_type(other);
//    if (! other.get_explicit_declaration() && other.is_pure_specialization())
//        return this->common_generic_base_type(*other.get_semantic_base_type());
//    if (*this == other)
//        return this;
    return nullptr;
}

bool TxActualType::derives_object(const TxActualType* otherType) const {
    auto thisType = this;
    while (thisType->has_base_type()) {
        thisType = thisType->get_semantic_base_type();
        if (thisType->inner_equals(*otherType))
            return true;
    }
    return false;
}

bool TxActualType::derives_interface(const TxActualType* otherType) const {
    if (this->inner_equals(*otherType))
        return true;
    for (auto & interfSpec : this->interfaces) {
        // TODO: This may not suffice - does not take modifiable, bindings, or dataspace of spec into account
        if (interfSpec.type->derives_interface(otherType))
            return true;
    }
    if (this->has_base_type())
        return this->get_semantic_base_type()->derives_interface(otherType);
    else
        return false;
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

static void type_bindings_string(std::stringstream& str, const std::vector<const TxEntityDeclaration*>& bindings) {
    str << "<";
    int ix = 0;
    for (auto b : bindings) {
        if (ix++)  str << ",";
        if (auto btype = b->get_definer()->attempt_get_type())
            str << btype->str( true );
        else
            str << b->get_unique_full_name();
    }
    str << ">";
}

std::string TxActualType::str( bool brief ) const {
    std::stringstream str;
    if (this->get_type_class() == TXTC_INTERFACE)
        str << "i/f ";
    else if (this->get_type_class() == TXTC_INTERFACEADAPTER)
        str << "i/f/ad ";
    else if (this->is_abstract())
        str << "ABSTRACT ";
    if (this->is_immutable())
        str << "IMMUTABLE ";
    this->self_string( str, brief );
    return str.str();
}

void TxActualType::self_string( std::stringstream& str, bool brief ) const {
    if (this->is_modifiable()) {
        str << "MOD ";
        this->get_base_type()->self_string( str, brief );  // bypass modifiable derivation
        return;
    }

    str << this->get_declaration()->get_unique_full_name();

    if (! this->hasInitialized) {
        str << " -uninitialized-";
        return;
    }

    //if (! this->params.empty())
    //    type_params_string(str, this->params);
    if (! this->get_bindings().empty())
        type_bindings_string(str, this->get_bindings());

    if (this->has_base_type() && !brief) {
        str << (this->is_empty_derivation() ? " = " : " : ");

        this->get_semantic_base_type()->self_string( str, false );  // set 'brief' to false to print entire type chain
    }
}



/*=== ArrayType and ReferenceType implementation ===*/

static bool array_assignable_from(const TxArrayType* toArray, const TxArrayType* fromArray) {
    // if origin has unbound type params that destination does not, origin is more generic and can't be assigned to destination
    if (auto toElem = toArray->element_type()) {
        if (auto fromElem = fromArray->element_type()) {
            // note: is-a test insufficient for array elements, since assignable type (same instance data type) required
            if (! fromElem->is_assignable_to(*toElem))
                return false;
        }
        else
            return false;  // origin has not bound E
    }
    if (auto len = toArray->length()) {
        if (auto otherLen = fromArray->length()) {
            return (len->get_static_constant_proxy() && otherLen->get_static_constant_proxy()
                    && *len->get_static_constant_proxy() == *otherLen->get_static_constant_proxy());
        }
        else
            return false;  // origin has not bound L
    }
    return true;
}

static bool ref_assignable_from(const TxReferenceType* toRef, const TxReferenceType* fromRef) {
    // if origin has unbound type params that destination does not, origin is more generic and can't be assigned to destination
    if (auto toTarget = toRef->target_type()) {
        if (auto fromTarget = fromRef->target_type()) {
            // is-a test sufficient for reference targets (it isn't for arrays, which require same concrete type)
            //std::cout << "CHECKING REF ASSIGNABLE\n\tFROM " << *fromTarget << "\n\tTO   " << *toTarget << std::endl;
            //std::cerr << "CHECKING REF ASSIGNABLE from\n"; print_hierarchy(fromTarget); std::cerr << "to:\n"; print_hierarchy(toTarget);
            if (! fromTarget->is_a(*toTarget))
                return false;
            else if (toTarget->is_modifiable() && !fromTarget->is_modifiable())
                return false;  // can't lose non-modifiability of target type
            else
                return true;
        }
        else
            return false;  // origin has not bound T
    }
    else
        return true;
}

bool TxArrayType::is_assignable_to(const TxActualType& destination) const {
    if (const TxArrayType* toArray = dynamic_cast<const TxArrayType*>(&destination))
        return array_assignable_from(toArray, this);
    else
        return false;
}

bool TxReferenceType::is_assignable_to(const TxActualType& destination) const {
    if (const TxReferenceType* toRef = dynamic_cast<const TxReferenceType*>(&destination))
        return ref_assignable_from(toRef, this);
    else
        return false;
}



bool TxArrayType::is_statically_sized() const {
    return this->is_concrete() && this->element_type()->is_statically_sized()
           && this->length()->is_statically_constant();
}


const TxExpressionNode* TxArrayType::length() const {
    if (auto bindingDecl = this->lookup_value_param_binding("tx.Array.L")) {
        return bindingDecl->get_definer()->get_init_expression();
    }
    LOG(this->LOGGER(), NOTE, "Unbound length for array type " << this);
    return nullptr;
}

const TxActualType* TxArrayType::element_type() const {
    if (auto bindingDecl = this->lookup_type_param_binding("tx.Array.E")) {
        return bindingDecl->get_definer()->resolve_type()->type();
    }
    LOG(this->LOGGER(), NOTE, "Unbound element type for array type " << this);
    ASSERT(this->is_generic(), "Unbound element type for NON-GENERIC array type " << this);
    return this->get_root_any_type();  // we know the basic constraint type for element is Any
}


const TxActualType* TxReferenceType::target_type() const {
    //std::string declstr = (this->get_declaration() ? this->get_declaration()->get_unique_full_name() : "nodecl");
    //std::cerr << "getting target type of " << declstr << ": " << this << std::endl;
    if (auto paramDecl = this->lookup_type_param_binding("tx.Ref.T")) {
        return paramDecl->get_definer()->resolve_type()->type();
    }
    else {
        LOG_DEBUG(this->LOGGER(), "Unbound target type for reference type " << this);
        ASSERT(this->is_generic(), "Unbound target type for NON-GENERIC reference type " << this);
    }
    return this->get_root_any_type();  // we know the basic constraint type for ref target is Any
}



bool TxInterfaceAdapterType::inner_prepare_members() {
    bool rec = TxActualType::inner_prepare_members();

    LOG_DEBUG(this->LOGGER(), "preparing adapter for " << this->adaptedType << " to interface " << this->get_semantic_base_type());
    // The virtual fields of the abstract base interface type are overridden to refer to
    // the correspondingly named fields of the adapted type.

    auto & adapteeVirtualFields = this->adaptedType->get_virtual_fields();
    for (auto & f : this->virtualFields.fieldMap) {
        if (f.first == "$adTypeId")
            continue; // this field is not overridden to refer to an adaptee field (and it's initialized together with the vtable)
        else if (! adapteeVirtualFields.has_field(f.first)) {
            auto protoField = this->virtualFields.get_field(f.second);
            if (protoField->get_decl_flags() & TXD_ABSTRACT)
                CERROR(this, "Adapted type " << this->adaptedType << " does not define virtual field " << f.first);
            else  // default implementation (mixin)
                LOG(this->LOGGER(), NOTE, "Adapted type " << this->adaptedType << " gets interface's default impl for field " << protoField);
        }
        else {
            auto targetField = adapteeVirtualFields.get_field(f.first);
            // FIXME: verify that type matches
            this->virtualFields.override_field(f.first, targetField);
        }
    }

    return rec;
}



TxFunctionType::TxFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                const std::vector<const TxActualType*>& argumentTypes, bool modifiableClosure )
    : TxFunctionType( declaration, baseType, argumentTypes,
                      baseType->get_declaration()->get_symbol()->get_root_scope()->registry().get_builtin_type( VOID )->type(),
                      modifiableClosure )  { }



TxExpressionNode* TxBuiltinConversionFunctionType::make_inline_expr( TxExpressionNode* calleeExpr,
                                                                     std::vector<TxMaybeConversionNode*>* argsExprList ) const {
    return make_conversion( argsExprList->front(), this->returnType->get_type_entity(), true );
}
