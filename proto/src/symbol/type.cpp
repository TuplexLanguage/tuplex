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



/*=== TxType implementation ===*/

const TxLocation& TxType::get_parse_location() const {
    return (this->get_declaration() ? this->get_declaration()->get_definer()->get_parse_location()
                                    : this->get_nearest_declaration()->get_symbol()->get_root_scope()->root_origin().get_parse_location());
}

ExpectedErrorClause* TxType::exp_err_ctx() const {
    return (this->get_declaration() ? this->get_declaration()->get_definer()->exp_err_ctx() : nullptr);
}

TxParserContext* TxType::get_parser_context() const {
    return this->get_nearest_declaration()->get_definer()->get_parse_location().parserCtx;
}


void TxType::validate_type() const {
    //std::cerr << "validating type " << this << std::endl;
    if (this->baseTypeSpec.type) {
        if (this->baseTypeSpec.type->is_modifiable())
            CERROR(this, "Can't specialize a 'modifiable' type (specialize its base type instead): " << this->baseTypeSpec.type);

        if (this->baseTypeSpec.modifiable) {
            ASSERT(this->get_type_class() == this->baseTypeSpec.type->get_type_class(),
                   "'modifiable' specialization must have same TxType class as the base type: " << this->baseTypeSpec.type);
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
    }
    // TODO: validate interfaces
    // check that generic interfaces can't be implemented multiple times throughout a type hierarchy,
    // unless their type arguments are exactly the same
}


void TxType::initialize_type() {
    LOGGER().debug("Initializing type %s", this->str().c_str());

    if (this->get_declaration()) {
        auto typeDeclNamespace = this->get_declaration()->get_symbol();
        // determine generic base type, if any:
        if (auto entitySym = dynamic_cast<TxEntitySymbol*>(typeDeclNamespace->get_member_symbol("$GenericBase"))) {
            if (auto typeDecl = entitySym->get_type_decl()) {
                this->genericBaseType = typeDecl->get_definer()->resolve_type();
                //LOGGER().alert("Generic base type of %s is %s", entitySym->get_full_name().to_string().c_str(), this->genericBaseType->to_string().c_str());
            }
        }

        // perform shallow pass on type's member declarations to determine derivation characteristics:
        bool hasExplicitFieldMembers = false;
        bool hasImplicitFieldMembers = false;
        for (auto symname = typeDeclNamespace->symbol_names_cbegin(); symname != typeDeclNamespace->symbol_names_cend(); symname++) {
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
                        LOGGER().note("Type with only implicit field members: %s", this->str().c_str());
                    }
                }
            }
        }

        // if this is not an empty nor a modifiable derivation, verify that all parameters of base type are either bound, or redeclared:
        // Note: The base type's parameters that have not been bound should normally be automatically redeclared by the type registry.
        if (this->has_base_type() && !this->emptyDerivation && !this->is_modifiable()) {
            for (auto & paramDecl : this->get_semantic_base_type()->type_params()) {
                if (! this->get_binding(paramDecl->get_unique_name())) {
                    if (! this->has_type_param(paramDecl->get_unique_name())) {
                        //this->params.emplace_back(paramDecl);
                        if (this->get_type_class() != TXTC_INTERFACEADAPTER) {
                            CERROR(this, "Missing binding or redeclaration of base type's type parameter "
                                   << paramDecl->get_unique_name() << " in " << this);
                            //LOGGER().note("Implicitly inheriting (redeclaring) type parameter %s in type %s", paramDecl->get_unique_full_name().c_str(), this->to_string().c_str());
                        }
                    }
                }
            }
        }

        { // validate the type parameter bindings (as much as we can without resolving this type's bindings at this point)
            auto basetype = this->get_semantic_base_type();
            for (auto & bindingDecl : this->get_bindings()) {
                auto pname = bindingDecl->get_unique_name();
                if (auto paramDecl = basetype->get_type_param_decl( pname )) {
                    auto constraintType = paramDecl->get_definer()->resolve_type();
                    ASSERT(constraintType, "NULL constraint type for param " << paramDecl << " of " << basetype);
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
                    CERROR(bindingDecl->get_definer(), "No type parameter of " << basetype << " matches provided binding " << bindingDecl->get_unique_name());
            }
        }
    }
    else {
        bool madeConcrete = !this->is_generic() && this->get_semantic_base_type() && this->get_semantic_base_type()->is_generic();
        ASSERT(! madeConcrete, "Type is concrete specialization of generic base type, but unnamed (undeclared): " << this  << "   sem.basetype: " << this->get_semantic_base_type());
        if (this->get_type_class() != TXTC_FUNCTION)
            LOGGER().alert("No declaration for non-func type (nearest decl: %s)\t%s", this->get_nearest_declaration()->str().c_str(),
                           this->str().c_str());
    }

    // determine datatype change:
    if (this->is_modifiable()) {
        // a modifiable type is a usage form of its base type, and doesn't affect the instance nor the vtable type
    }
    else if (this->nonRefBindings) {
        // Binding of a base type parameter implies reinterpretation of its members and thus
        // the chance of modified instance / vtable types (for non-ref-constrained parameters).
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

    this->validate_type();
}

void TxType::prepare_members() {
    LOGGER().debug("Preparing members of type %s", this->str().c_str());

    ASSERT(! this->prepared, "Can't prepare type more than once: " << this);
    this->prepared = true;

    ExpectedErrorClause* expErrWholeType = nullptr;
    if (this->get_decl_flags() & TXD_EXPERRBLOCK) {
        expErrWholeType = this->get_declaration()->get_definer()->context().exp_error();
        ASSERT(expErrWholeType, "TXD_EXPERRBLOCK flag set but type definer has no ExpErr context: " << this->get_declaration());
        this->get_parser_context()->begin_exp_err( this->get_declaration()->get_definer() );
    }


    // copy base type's virtual and instance field tuples (to which fields may be added / overridden):
    auto baseType = this->get_base_type();
    if (baseType) {
        this->virtualFields = baseType->virtualFields;
        this->instanceFields = baseType->instanceFields;
    }
    for (auto & interfSpec : this->interfaces) {
        if (this->virtualFields.add_interface_fields( interfSpec.type->virtualFields ))
            this->modifiesVTable = true;
    }
    //std::cerr << "Inherited virtual fields of " << this << std::endl;
    //this->virtualFields.dump();

    if (! this->get_declaration()) {
        return;
    }

    auto semBaseType = this->get_semantic_base_type();

    // for all the member names declared or redeclared in this type:
    auto typeDeclNamespace = this->get_declaration()->get_symbol();
    for (auto symname = typeDeclNamespace->symbol_names_cbegin(); symname != typeDeclNamespace->symbol_names_cend(); symname++) {
        // this drives resolution of all this type's members

        auto entitySym = dynamic_cast<TxEntitySymbol*>(typeDeclNamespace->get_member_symbol(*symname));
        if (! entitySym)
            continue;

        // prepare type members:
        if (auto typeDecl = entitySym->get_type_decl()) {
            ExpectedErrorClause* expErr = nullptr;
            if (typeDecl->get_decl_flags() & TXD_EXPERRBLOCK) {
                expErr = typeDecl->get_definer()->context().exp_error();
                ASSERT(expErr, "TXD_EXPERRBLOCK flag set but type definer has no ExpErr context: " << typeDecl);
                this->get_parser_context()->begin_exp_err( typeDecl->get_definer() );
            }

            if (auto type = typeDecl->get_definer()->resolve_type()) {
                if (typeDecl->get_decl_flags() & TXD_GENBINDING) {
                    auto bname = typeDecl->get_unique_name();
                    if (auto paramDecl = semBaseType->get_type_param_decl( bname )) {
                        auto constraintType = paramDecl->get_definer()->get_type();

                        if (! type->is_a(*constraintType))
                            // TODO: do this also for VALUE params, but array type expression needs auto-conversion support for that to work
                            CERROR(typeDecl->get_definer(), "Bound type " << type << " for type parameter " << paramDecl
                                                            << " is not a derivation of contraint type " << constraintType);
                    }
                }
            }

            if (expErr && !expErrWholeType) {
                this->get_parser_context()->end_exp_err( typeDecl->get_definer()->get_parse_location() );
            }
        }

        // prepare field members:
        for (auto fieldDeclI = entitySym->fields_cbegin(); fieldDeclI != entitySym->fields_cend(); fieldDeclI++) {
            auto fieldDecl = *fieldDeclI;

            ExpectedErrorClause* expErrField = nullptr;
            if (fieldDecl->get_decl_flags() & TXD_EXPERRBLOCK) {
                expErrField = fieldDecl->get_definer()->context().exp_error();
                ASSERT(expErrField, "TXD_EXPERRBLOCK flag set but field definer has no ExpErr context: " << fieldDecl);
                this->get_parser_context()->begin_exp_err( fieldDecl->get_definer() );
            }

            if (auto field = fieldDecl->get_definer()->resolve_field()) {
                // validate type:
                if (auto fieldType = field->get_type()) {
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

                // layout:
                switch (fieldDecl->get_storage()) {
                case TXS_INSTANCE:
                    LOGGER().debug("Laying out instance field %-40s  %s  %u", field->str().c_str(),
                                   field->get_type()->str(true).c_str(), this->instanceFields.get_field_count());
                    if (fieldDecl->get_decl_flags() & TXD_ABSTRACT)
                        CERROR(field, "Can't declare an instance field as abstract: " << field);
                    if (fieldDecl->get_decl_flags() & TXD_GENBINDING)
                        LOGGER().debug("Skipping layout of GENBINDING instance field: %s", field->str().c_str());
                    else if (!expErrField || expErrWholeType)
                        this->instanceFields.add_field(field);
                    break;
                case TXS_VIRTUAL:
                case TXS_INSTANCEMETHOD:
                    if (fieldDecl->get_decl_flags() & TXD_CONSTRUCTOR)
                        break;  // skip, constructors aren't virtual

                    if (fieldDecl->get_decl_flags() & TXD_ABSTRACT) {
                        if (this->get_type_class() != TXTC_INTERFACE && !(this->get_declaration()->get_decl_flags() & TXD_ABSTRACT))
                            CERROR(fieldDecl->get_definer(), "Can't declare abstract member '" << fieldDecl->get_unique_name() << "' in type that is not declared abstract: " << this);
                    }
                    // permit this for now
                    //else if (this->get_type_class() == TXTC_INTERFACE)
                    //    CERROR(fieldDecl->get_definer(), "Can't declare non-abstract virtual member '" << fieldDecl->get_unique_name() << "' in interface type: " << this);

                    if (this->virtualFields.has_field(field->get_unique_name())) {
                        if (! (fieldDecl->get_decl_flags() & TXD_OVERRIDE))
                            CWARNING(field, "Field overrides but isn't declared 'override': " << field);
                        auto overriddenField = this->virtualFields.get_field(field->get_unique_name());
                        if (overriddenField->get_decl_flags() & TXD_FINAL)
                            CERROR(field, "Can't override a base type field that is declared 'final': " << field);
                        if (! (field->get_type()->is_assignable_to(*overriddenField->get_type())))
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
                    LOGGER().debug("Adding/overriding virtual field %-40s  %s  %u", field->str().c_str(),
                                   field->get_type()->str(true).c_str(), this->virtualFields.get_field_count());
                    break;
                default:
                    ASSERT(fieldDecl->get_storage() == TXS_STATIC, "Invalid storage class " << fieldDecl->get_storage() << " for field member " << *field);
                    if (fieldDecl->get_decl_flags() & TXD_ABSTRACT)
                        CERROR(field, "Can't declare a non-virtual field as abstract: " << field);
                    if (fieldDecl->get_decl_flags() & TXD_OVERRIDE)
                        CWARNING(field, "Field doesn't override but is declared 'override': " << field);
                    if (! expErrField || expErrWholeType)
                        this->staticFields.add_field(field);
                }
            }

            if (expErrField && !expErrWholeType) {
                this->get_parser_context()->end_exp_err(fieldDecl->get_definer()->get_parse_location());
            }
        }
    }

    if (! this->is_abstract() && ! this->is_modifiable() && this->get_type_class() != TXTC_INTERFACEADAPTER) {
        // check that all abstract members of base types & interfaces are implemented:
        auto virtualFields = this->get_virtual_fields();
        for (auto & field : virtualFields.fieldMap) {
            auto actualFieldEnt = virtualFields.get_field(field.second);
            if (actualFieldEnt->get_decl_flags() & TXD_ABSTRACT) {
                CERROR(this, "Concrete type " << this->str(true) << " doesn't implement abstract member " << actualFieldEnt);
            }
        }
    }

    if (expErrWholeType) {
        this->get_parser_context()->end_exp_err(this->get_declaration()->get_definer()->get_parse_location());
    }
}


const TxTypeDeclaration* TxType::get_explicit_declaration() const {
    if (this->get_declaration() && !(this->get_declaration()->get_decl_flags() & TXD_IMPLICIT))
        return this->get_declaration();
    return nullptr;
}

bool TxType::is_explicit_nongen_declaration() const {
    return (this->get_declaration() && !(this->get_declaration()->get_decl_flags() & (TXD_IMPLICIT | TXD_GENPARAM | TXD_GENBINDING)));
}


//bool TxType::is_reinterpreted() const {
//    return ( this->get_declaration()
//             && this->get_declaration()->get_definer()->get_six() > 0 );
//}
//
//bool TxType::is_equivalent_reinterpreted_specialization() const {
//    if (this->typeClass == TXTC_REFERENCE)
//        return false;  // FIX ME
//    else if (this->typeClass == TXTC_ARRAY)
//        return false;  // FIX ME
//    else if (this->genericBaseType)
//        return !this->genericBaseType->nonRefParameters;
//    else
//        return (this->get_declaration() && this->get_declaration()->get_definer()->get_six() > 0 );
//}

bool TxType::is_abstract() const {
    return this->get_nearest_declaration()->get_decl_flags() & TXD_ABSTRACT;
}

bool TxType::is_concrete() const {
    // A concrete type is not abstract, nor usually generic (references may be concrete while generic).
    if (this->typeClass == TXTC_REFERENCE)
        return true;
    // TODO: If array of reference elements with known length, return true
    if (this->is_abstract())
        return false;
    if (this->is_generic()) {
        // TODO: If all members concrete, then return true
        return false;
    }
    return true;
}

//bool TxType::is_pure_specialization() const {
//    ASSERT(this->prepared, "Can't determine specialization degree of unprepared type: " << this);
//    return ( this->baseTypeSpec.modifiable
//             || ( this->has_base_type()
//                  && !this->is_builtin()  // this being built-in implies that it is more concrete than base class
//                  && typeid(*this) == typeid(*this->baseTypeSpec.type)
//                  && ( this->genericBaseType
//                       || ( !this->extendsInstanceDatatype && !this->modifiesVTable ) ) ) );
//}

bool TxType::is_empty_derivation() const {
    return this->emptyDerivation;
    /*
    ASSERT(this->prepared, "Can't determine specialization degree of unprepared type: " << this);
    return ( this->has_base_type()
             && this->interfaces.empty()
             && !this->baseTypeSpec.modifiable
             && !this->is_builtin()  // being built-in implies that it is more specialized than base class
             && typeid(*this) == typeid(*this->baseTypeSpec.type)
             && this->get_bindings().empty()
             && !this->genericBaseType
             && !this->extendsInstanceDatatype
             && !this->modifiesVTable );
     */
}

bool TxType::is_equivalent_derivation() const {
    ASSERT(this->prepared, "Can't determine specialization degree of unprepared type: " << this);
    bool oldCond = ( this->baseTypeSpec.modifiable
             || ( this->has_base_type()
                  && !this->is_builtin()  // being built-in implies that it is more specialized than base class
                  && typeid(*this) == typeid(*this->baseTypeSpec.type)
                  && !this->nonRefBindings
                  && ( !this->extendsInstanceDatatype && !this->modifiesVTable ) ) );
    bool newCond = this->is_same_vtable_type() && this->is_same_instance_type();
    ASSERT(oldCond == newCond, "Unexpected is_equivalent_derivation() condition difference: old=" << oldCond << " != new=" << newCond);
    return newCond;
}

bool TxType::is_virtual_derivation() const {
    ASSERT(this->prepared, "Can't determine specialization degree of unprepared type: " << this);
    bool oldCond = ( this->baseTypeSpec.modifiable
             || ( this->has_base_type()
                  && !this->is_builtin()  // being built-in implies that it is more specialized than base class
                  && typeid(*this) == typeid(*this->baseTypeSpec.type)
                  && !this->nonRefBindings
                  && !this->extendsInstanceDatatype ) );
    bool newCond = this->is_same_instance_type();
    ASSERT(oldCond == newCond, "Unexpected is_virtual_derivation() condition difference: old=" << oldCond << " != new=" << newCond);
    return newCond;
}

bool TxType::is_statically_sized() const {
    return this->is_concrete();
}


const TxType* TxType::get_instance_base_type() const {
    return (this->is_same_instance_type() ? this->get_semantic_base_type()->get_instance_base_type() : this);
}


TxEntitySymbol* TxType::get_instance_member(const std::string& name) const {
    return this->get_instance_member(this->get_nearest_declaration()->get_symbol(), name);
}

TxEntitySymbol* TxType::get_instance_member(TxScopeSymbol* vantageScope, const std::string& name) const {
    if (auto decl = this->get_declaration()) {
        if (auto member = lookup_member(vantageScope, decl->get_symbol(), name)) {
            if (auto memberEnt = dynamic_cast<TxEntitySymbol*>(member))
                return memberEnt;
            else
                LOGGER().warning("Looked-up member is not an entity: %s",  member->str().c_str());
        }
    }
    return nullptr;
}

TxEntitySymbol* TxType::lookup_inherited_instance_member(const std::string& name) const {
    return this->lookup_inherited_instance_member(this->get_nearest_declaration()->get_symbol(), name);
}

TxEntitySymbol* TxType::lookup_inherited_instance_member(TxScopeSymbol* vantageScope, const std::string& name) const {
    ASSERT(name != "$init", "Can't look up constructors as *inherited* members; in: " << this);
    for (const TxType* type = this; type; type = type->get_base_type()) {
        if (auto memberEnt = type->get_instance_member(vantageScope, name))
            return memberEnt;
        for (auto & interfSpec : type->interfaces) {
            if (auto memberEnt = interfSpec.type->lookup_inherited_instance_member(vantageScope, name))
                return memberEnt;
        }
    }
    return nullptr;
}


static const TxEntityDeclaration* get_type_param_decl(const std::vector<TxEntityDeclaration*>& params, const std::string& fullParamName) {
    for (auto & paramDecl : params)
        if (fullParamName == paramDecl->get_unique_full_name())
            return paramDecl;
    return nullptr;
}

static TxEntitySymbol* lookup_inherited_binding(const TxType* type, const std::string& fullParamName) {
    TxIdentifier ident( fullParamName );
    auto parentName = ident.parent().str();
    auto paramName = ident.name();
    const TxType* semBaseType = type->get_semantic_base_type();
    while (semBaseType) {
        if (get_type_param_decl(semBaseType->type_params(), fullParamName)) {
            // semBaseType is the (nearest) type that declares the sought parameter
            if (auto binding = type->get_binding(paramName))
                return binding->get_symbol();
        }
        else if (semBaseType->get_declaration() && semBaseType->get_declaration()->get_unique_full_name() == parentName)
            type->LOGGER().warning("Type parameter %s apparently unbound", fullParamName.c_str());

        type = type->get_base_type();
        semBaseType = type->get_semantic_base_type();
    }
    return nullptr;
}

const TxFieldDeclaration* TxType::lookup_value_param_binding(const std::string& fullParamName) const {
    if (auto bindingSymbol = lookup_inherited_binding(this, fullParamName))
        return bindingSymbol->get_first_field_decl();
    return nullptr;
}
const TxTypeDeclaration* TxType::lookup_type_param_binding(const std::string& fullParamName) const {
    if (auto bindingSymbol = lookup_inherited_binding(this, fullParamName))
        return bindingSymbol->get_type_decl();
    return nullptr;
}

const TxEntityDeclaration* TxType::lookup_param_binding(const TxEntityDeclaration* paramDecl) const {
    ASSERT(paramDecl->get_decl_flags() & TXD_GENPARAM, "Can't look up a binding for a 'param decl' that isn't GENPARAM: " << paramDecl);
    if (auto bindingSymbol = lookup_inherited_binding(this, paramDecl->get_unique_full_name())) {
        if (dynamic_cast<const TxTypeDeclaration*>(paramDecl))
            return bindingSymbol->get_type_decl();
        else
            return bindingSymbol->get_first_field_decl();
    }
    return nullptr;
}


bool TxType::inner_equals(const TxType& otherType) const {
    auto explDecl = this->get_explicit_declaration();
    return explDecl == otherType.get_explicit_declaration()  // same declaration or both null
           && ( explDecl  // same explicit declaration
                || ( this->baseTypeSpec == otherType.baseTypeSpec ) );  // unnamed but identical, pure specializations
    // (interfaces and members can only apply to a type with an explicit declaration, and an explicit declaration can have only one type instance)
}

bool TxType::operator==(const TxType& other) const {
    // skips empty type derivations that aren't explicitly declared
    const TxType* thisType = this;
    const TxType* otherType = &other;
    while (!thisType->is_explicit_nongen_declaration() && thisType->is_empty_derivation())
        thisType = thisType->get_semantic_base_type();
    while (!otherType->is_explicit_nongen_declaration() && otherType->is_empty_derivation())
        otherType = otherType->get_semantic_base_type();
    return thisType->inner_equals(*otherType);
}


bool TxType::is_assignable_to(const TxType& destination) const {
    // fields must at least be the same instance data type
    auto thisType = this;
    do {
        if (*thisType == destination)
            return true;
        if (thisType->is_same_instance_type())
            thisType = thisType->get_semantic_base_type();
        else
            return false;
    } while (true);
}


/*
static std::string type_name(const TxType* type) {
    if (auto decl = type->get_declaration())
        return decl->to_string();
    else
        return typeid(*type).name();
}
static void print_hierarchy(const TxType* type)
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


bool TxType::is_a(const TxType& other) const {
    //std::cerr << *this << "  IS-A\n" << other << std::endl;
//    ASSERT(! this->genericBaseType, "Invoked is_a() on concrete specialization type: " << this);
//    ASSERT(! other.genericBaseType, "Invoked is_a() with other being a concrete specialization type: " << this);

    const TxType* thisType = this;
    const TxType* otherType = &other;

    // compare modifiability:
    bool thisMod = thisType->is_modifiable();
    bool otherMod = otherType->is_modifiable();
    if (thisMod) {
        thisType = thisType->get_semantic_base_type();
        if (otherMod)
            otherType = otherType->get_semantic_base_type();
    }
    else if (otherMod)
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
            // TODO: more thorough analysis of which additional cases may be compatible
            if (auto otherBinding = otherType->lookup_param_binding(paramDecl)) {
                if (auto thisBinding = thisType->lookup_param_binding(paramDecl)) {
                    // check whether both bindings resolve to same type/value:
                    if (typeid(*thisBinding) != typeid(*otherBinding)
                            || thisBinding->get_definer()->get_type() == otherBinding->get_definer()->get_type())
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

const TxType* TxType::common_generic_base_type(const TxType& other) const {
    // !!! this implementation assumes that if a type has bindings, it is a pure specialization !!!
    //std::cerr << "this: " << this << "   other: " << other << std::endl;
    const TxType* thisType = this;
    const TxType* otherType = &other;
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

bool TxType::derives_object(const TxType* otherType) const {
    auto thisType = this;
    while (thisType->has_base_type()) {
        thisType = thisType->get_semantic_base_type();
        if (thisType->inner_equals(*otherType))
            return true;
    }
    return false;
}

bool TxType::derives_interface(const TxType* otherType) const {
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


static void type_params_string(std::stringstream& str, const std::vector<TxEntityDeclaration*>& params) {
    str << "<";
    int ix = 0;
    for (auto & p : params) {
        if (ix++)  str << ",";
        str << p->get_unique_name();
    }
    str << ">";
}

//static void type_bindings_string(std::stringstream& str, const std::vector<TxEntityDeclaration*>& bindings) {
//    str << "<";
//    int ix = 0;
//    for (auto b : bindings) {
//        if (ix++)  str << ",";
//        str << b->get_unique_full_name();
//        //b->get_definer()->resolve_type();
//        //str << b->get_definer()->get_type()->to_string(true, true);
//    }
//    str << ">";
//}

std::string TxType::str(bool brief, bool skipFirstName, bool skipImplicitNames) const {
    std::stringstream str;
    if (this->get_type_class() == TXTC_INTERFACE)
        str << "i/f ";
    else if (this->get_type_class() == TXTC_INTERFACEADAPTER)
        str << "i/f/ad ";
    else if (this->is_abstract())
        str << "ABSTRACT ";
    if (this->is_immutable())
        str << "IMMUTABLE ";
    this->self_string(str, brief, skipFirstName, skipImplicitNames);
    return str.str();
}

void TxType::self_string(std::stringstream& str, bool brief, bool skipFirstName, bool skipImplicitNames) const {
    if (this->is_modifiable())
        str << "MOD ";

    if (! skipFirstName) {
        if (auto decl = this->get_declaration()) {
            if (! (skipImplicitNames && (decl->get_decl_flags() & TXD_IMPLICIT)))
                str << decl->get_unique_full_name();
        }
        else
            str << "unnamed";
    }

    if (! this->params.empty())
        type_params_string(str, this->params);

    if (this->has_base_type() && (!brief || skipFirstName)) {
        str << (this->is_empty_derivation() ? " = " : " : ");

        //if (! this->get_bindings().empty())
        //    type_bindings_string(str, this->get_bindings());

        this->get_base_type()->self_string(str, false, false, skipImplicitNames);  // set 'brief' to false to print entire type chain
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

bool TxArrayType::is_assignable_to(const TxType& destination) const {
    if (const TxArrayType* toArray = dynamic_cast<const TxArrayType*>(&destination))
        return array_assignable_from(toArray, this);
    else
        return false;
}

bool TxReferenceType::is_assignable_to(const TxType& destination) const {
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
    LOGGER().note("Unbound length for array type %s", this->str().c_str());
    return nullptr;
}

const TxType* TxArrayType::element_type() const {
    if (auto bindingDecl = this->lookup_type_param_binding("tx.Array.E")) {
        if (auto type = bindingDecl->get_definer()->resolve_type())
            return type;
    }
    LOGGER().note("Unbound element type for array type %s", this->str().c_str());
    ASSERT(this->is_generic(), "Unbound element type for NON-GENERIC array type " << this);
    return this->get_root_any_type();  // we know the basic constraint type for element is Any
}


const TxType* TxReferenceType::target_type() const {
    //std::string declstr = (this->get_declaration() ? this->get_declaration()->get_unique_full_name() : "nodecl");
    //std::cerr << "getting target type of " << declstr << ": " << this << std::endl;
    if (auto paramDecl = this->lookup_type_param_binding("tx.Ref.T")) {
        if (auto type = paramDecl->get_definer()->resolve_type())
            return type;
    }
    LOGGER().debug("Unbound target type for reference type %s", this->str().c_str());
    ASSERT(this->is_generic(), "Unbound target type for NON-GENERIC reference type " << this);
    return this->get_root_any_type();  // we know the basic constraint type for ref target is Any
}



void TxInterfaceAdapterType::prepare_members() {
    if (! this->is_modifiable()) {
        this->modifiesVTable = true;
    }

    TxType::prepare_members();

    LOGGER().debug("preparing adapter for %s to interface %s", this->adaptedType->str().c_str(), this->get_semantic_base_type()->str().c_str());
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
                LOGGER().note("Adapted type %s gets interface's default impl for field %s",
                              this->adaptedType->str().c_str(), protoField->str().c_str());
        }
        else {
            auto targetField = adapteeVirtualFields.get_field(f.first);
            // FIXME: verify that type matches
            this->virtualFields.override_field(f.first, targetField);
        }
    }
}



TxExpressionNode* TxBuiltinConversionFunctionType::make_inline_expr( TxExpressionNode* calleeExpr,
                                                                     std::vector<TxMaybeConversionNode*>* argsExprList ) const {
    return make_conversion( argsExprList->front(), this->returnType, true );
}
