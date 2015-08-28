#include "logging.hpp"
#include "tx_error.hpp"

#include "type.hpp"
#include "ast.hpp"
#include "entity.hpp"


bool TxConstantProxy::operator==(const TxConstantProxy& other) const {
    // simple since we so far only support UInt values
    // FUTURE: improve as more value getters added (note that auto-conversion needs to be supported)
    return this->get_value_UInt() == other.get_value_UInt();
}



/*=== TxTypeSpecialization implementation ===*/

bool TxTypeSpecialization::operator==(const TxTypeSpecialization& other) const {
    return ( this->type == other.type
             || ( this->type && other.type && *this->type == *other.type) )
           //&& *this->dataspace == *other.dataspace
           && this->empty == other.empty
           && this->modifiable == other.modifiable;
}

std::string TxTypeSpecialization::to_string() const { return "specialization of " + this->type->to_string(); }



/*=== TxType implementation ===*/

void TxType::prepare_type_validation() const {
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

//        if (this->dataspace && this->baseTypeSpec.type->get_type_class() != TXTC_REFERENCE)
//            CERROR(this, "Specified dataspace for non-reference base type " << this->baseTypeSpec.type);

        if (! this->baseTypeSpec.type->get_declaration())
            // also validate any anonymous base types (otherwise their validate() won't be called)
            this->baseTypeSpec.type->validate();

        if (this->baseTypeSpec.type->is_empty_derivation() && !this->baseTypeSpec.type->is_explicit_nongen_declaration()) {
            ASSERT(! (this->is_empty_derivation() && !this->is_explicit_nongen_declaration()),
                   "anonymous or implicit, empty types may not be derived except as another anonymous or implicit, empty type: " << this);
        }
    }
    // TODO: validate interfaces
    // check that generic interfaces can't be implemented multiple times throughout a type hierarchy,
    // unless their type arguments are exactly the same
}


void TxType::prepare_type() {
    LOGGER().debug("Preparing type %s", this->to_string().c_str());

    if (this->get_declaration()) {
        auto typeDeclNamespace = this->get_declaration()->get_symbol();
        // determine generic base type, if any:
        if (auto entitySym = dynamic_cast<TxEntitySymbol*>(typeDeclNamespace->get_member_symbol("$GenericBase"))) {
            if (auto typeDecl = entitySym->get_type_decl()) {
                ResolutionContext resCtx;
                this->genericBaseType = typeDecl->get_definer()->resolve_type(resCtx);
                //LOGGER().alert("Generic base type of %s is %s", entitySym->get_full_name().to_string().c_str(), this->genericBaseType->to_string().c_str());
            }
        }

        // perform shallow pass on type's member declarations to determine derivation characteristics:
        bool hasExplicitFieldMembers = false;
        bool hasImplicitFieldMembers = false;
        for (auto symname = typeDeclNamespace->symbol_names_cbegin(); symname != typeDeclNamespace->symbol_names_cend(); symname++) {
            if (auto entitySym = dynamic_cast<TxEntitySymbol*>(typeDeclNamespace->get_member_symbol(*symname))) {
                if (auto typeDecl = entitySym->get_type_decl()) {
                    if (typeDecl->get_decl_flags() & TXD_GENPARAM)
                        this->params.emplace_back( typeDecl );
                    else if (typeDecl->get_decl_flags() & TXD_GENBINDING)
                        this->bindings.emplace_back( typeDecl );
                }

                for (auto fieldDeclI = entitySym->fields_cbegin(); fieldDeclI != entitySym->fields_cend(); fieldDeclI++) {
                    auto fieldDecl = *fieldDeclI;

                    if (fieldDecl->get_decl_flags() & TXD_IMPLICIT)
                        hasImplicitFieldMembers = true;
                    else
                        hasExplicitFieldMembers = true;

                    if (fieldDecl->get_decl_flags() & TXD_GENPARAM)
                        this->params.emplace_back( fieldDecl );
                    else if (fieldDecl->get_decl_flags() & TXD_GENBINDING)
                        this->bindings.emplace_back( fieldDecl );

                    switch (fieldDecl->get_storage()) {
                    case TXS_INSTANCE:
                        if (fieldDecl->get_decl_flags() & TXD_GENBINDING)
                            // FIXME: make VALUE type params be declared as instance members in generic base type,
                            // so that they are not "extensions" to the specialized subtypes, and remove this '#' test
                            break;
                        this->extendsInstanceDatatype = true;
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
            if (hasImplicitFieldMembers) {
                //LOGGER().alert("Type with only implicit field members: %s", this->to_string().c_str());
            }
            if (! this->bindings.empty()) {
                this->pureDerivation = true;
            }
            else if (! this->is_builtin() && ! this->is_modifiable() && this->interfaces.empty() && this->params.empty()) {
                this->emptyDerivation = true;
                if (! this->baseTypeSpec.empty)
                    LOGGER().alert("Type without field members not pre-specified as empty: %s", this->to_string().c_str());
            }
        }

        // if this is not an empty nor a modifiable derivation, verify that all parameters of base type are either bound, or redeclared:
        // (current approach is to inherit, i.e. automatically redeclare, the base type's parameters that have not been bound)
        if (this->has_base_type() && !this->emptyDerivation && !this->is_modifiable()) {
            for (auto & paramDecl : this->get_base_type()->type_params()) {
                if (! this->get_binding(paramDecl->get_unique_name())) {
                    //CERROR(this, "Missing binding or redeclaration of base type's type parameter " << paramDecl->get_unique_name());
                    this->params.emplace_back(paramDecl);
                    LOGGER().note("Inheriting type parameter %s in type %s", paramDecl->get_unique_full_name().c_str(), this->to_string().c_str());
                }
            }
        }
    }
    else {
        bool madeConcrete = !this->is_generic() && this->get_semantic_base_type() && this->get_semantic_base_type()->is_generic();
        ASSERT(! madeConcrete, "Type is concrete specialization of generic base type, but unnamed (undeclared): " << this  << "   sem.basetype: " << this->get_semantic_base_type());
        if (this->get_type_class() != TXTC_FUNCTION)
            LOGGER().alert("No declaration for non-func type (nearest decl: %s)\t%s", this->get_nearest_declaration()->to_string().c_str(),
                           this->to_string().c_str());
    }

    // determine datatype change:
    if (this->is_modifiable()) {
        // a modifiable type is a usage form of its base type, and doesn't affect the instance nor the vtable type
    }
    else if (! this->get_bindings().empty()) {
        // Binding of a base type parameter implies reinterpretation of its members and thus
        // the chance of modified instance / vtable types.
        // FUTURE: at least distinguish between binding ref-constrained parameters and others
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

    this->prepare_type_validation();
}

void TxType::prepare_type_members() {
    LOGGER().debug("Preparing members of type %s", this->to_string().c_str());

    ASSERT(! this->prepared, "Can't prepare type more than once: " << this);
    this->prepared = true;

    const bool expErrWholeType = ( this->get_declaration() && ( this->get_declaration()->get_decl_flags() & TXD_EXPERRBLOCK ) );
    if (expErrWholeType)
        this->get_driver()->begin_exp_err(this->get_declaration()->get_definer()->get_parse_location());

    // resolve and validate type parameters
    ResolutionContext resCtx;
    for (auto & paramDecl : this->params) {
        auto constraintType = paramDecl->get_definer()->resolve_type(resCtx);
        if (dynamic_cast<TxFieldDeclaration*>(paramDecl))
            if (constraintType->get_type_class() != TXTC_REFERENCE)
                this->nonRefParameters = true;
    }

    // resolve and validate type parameter bindings
    auto basetype = this->get_semantic_base_type();
    for (auto & bindingDecl : this->get_bindings()) {
        auto pname = bindingDecl->get_unique_name();
        if (auto paramDecl = basetype->get_type_param_decl( pname )) {
            // validate metatype and constraints
            if (meta_type_of(bindingDecl) != meta_type_of(paramDecl))
                CERROR(bindingDecl->get_definer(), "Binding for type parameter " << paramDecl << " of wrong meta-type (TYPE vs VALUE)");

            if (meta_type_of(bindingDecl) == MetaType::TXB_VALUE) {
                // TODO: check: VALUE parameters can not be of modifiable type
                this->nonRefBindings = true;
            }
            else {  // TxTypeParam::MetaType::TXB_TYPE
                auto constraintType = paramDecl->get_definer()->resolve_type(resCtx);
                auto boundType = bindingDecl->get_definer()->resolve_type(resCtx);
                ASSERT(constraintType, "NULL constraint type for param " << paramDecl << " of " << basetype);
                ASSERT(boundType,      "NULL binding for param " << paramDecl << " of " << basetype);
                //std::cerr << this << ": Constraint type for param " << paramDecl << ": " << "checking bound type "
                //          << boundType << "\tagainst constraint type " << constraintType << std::endl;
                if (! boundType->is_a(*constraintType))
                    CERROR(bindingDecl->get_definer(), "Bound type " << boundType << " for type parameter " << paramDecl
                                                       << " is not a derivation of contraint type " << constraintType);
                if (constraintType->get_type_class() != TXTC_REFERENCE)
                    this->nonRefBindings = true;
            }
        }
        else
            CERROR(bindingDecl->get_definer(), "No type parameter of " << basetype << " matches provided binding " << bindingDecl->get_unique_name());
    }


    // copy base type's virtual and instance field tuples (to which fields may be added / overridden):
    auto baseType = this->get_base_type();
    if (baseType) {
        this->virtualFields = baseType->virtualFields;
        this->instanceFields = baseType->instanceFields;
    }

    if (! this->get_declaration()) {
        return;
    }

    // for all the member names declared or redeclared in this type:
    auto typeDeclNamespace = this->get_declaration()->get_symbol();
    for (auto symname = typeDeclNamespace->symbol_names_cbegin(); symname != typeDeclNamespace->symbol_names_cend(); symname++) {
        // this drives resolution of all this type's members

        auto entitySym = dynamic_cast<TxEntitySymbol*>(typeDeclNamespace->get_member_symbol(*symname));
        if (! entitySym)
            continue;

// resolving type members from here shouldn't be necessary
//        if (auto typeDecl = entitySym->get_type_decl()) {
//            if (*symname != "tx#Ref#T") {  // prevents infinite recursion
//                //LOGGER().alert("resolving member type %s", entitySym->get_full_name().to_string().c_str());
//                if (typeDecl->get_decl_flags() & TXD_EXPERRBLOCK)
//                    this->get_driver()->begin_exp_err(typeDecl->get_definer()->get_parse_location());
//
//                typeDecl->get_definer()->get_type(); //resolve_type(resCtx);
//
//                if (typeDecl->get_decl_flags() & TXD_EXPERRBLOCK) {
//                    /*int encountered_error_count =*/ this->get_driver()->end_exp_err(typeDecl->get_definer()->get_parse_location());
//                    // FIX ME: include in expected error count
//                }
//            }
//        }

        for (auto fieldDeclI = entitySym->fields_cbegin(); fieldDeclI != entitySym->fields_cend(); fieldDeclI++) {
            auto fieldDecl = *fieldDeclI;

            const bool expErrField = ( !expErrWholeType && ( fieldDecl->get_decl_flags() & TXD_EXPERRBLOCK ) );
            if (expErrField)
                this->get_driver()->begin_exp_err(fieldDecl->get_definer()->get_parse_location());

            if (auto field = fieldDecl->get_definer()->resolve_field(resCtx)) {
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
                    LOGGER().debug("Laying out instance field %-40s  %s  %u", field->to_string().c_str(),
                                   field->get_type()->to_string(true).c_str(), this->instanceFields.get_field_count());
                    if (fieldDecl->get_decl_flags() & TXD_ABSTRACT)
                        CERROR(field, "Can't declare an instance field as abstract: " << field);
                    if (! expErrField)
                        this->instanceFields.add_field(field->get_unique_name(), field);
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
                            CERROR(field, "Overriding member's type may not override overridden member's type: " << field->get_type());
                        if (! expErrField)
                            this->virtualFields.override_field(field->get_unique_name(), field);
                    }
                    else {
                        if (fieldDecl->get_decl_flags() & TXD_OVERRIDE)
                            CWARNING(field, "Field doesn't override but is declared 'override': " << field);
                        if (! expErrField)
                            this->virtualFields.add_field(field->get_unique_name(), field);
                    }
                    LOGGER().debug("Adding/overriding virtual field %-40s  %s  %u", field->to_string().c_str(),
                                   field->get_type()->to_string(true).c_str(), this->virtualFields.get_field_count());
                    break;
                default:
                    ASSERT(fieldDecl->get_storage() == TXS_STATIC, "Invalid storage class " << fieldDecl->get_storage() << " for field member " << *field);
                    if (fieldDecl->get_decl_flags() & TXD_ABSTRACT)
                        CERROR(field, "Can't declare a non-virtual field as abstract: " << field);
                    if (fieldDecl->get_decl_flags() & TXD_OVERRIDE)
                        CWARNING(field, "Field doesn't override but is declared 'override': " << field);
                    if (! expErrField)
                        this->staticFields.add_field(field->get_unique_name(), field);
                }
            }

            if (expErrField) {
                /*int encountered_error_count =*/ this->get_driver()->end_exp_err(fieldDecl->get_definer()->get_parse_location());
                // FIXME: include in expected error count
            }
        }
    }

    if (! (this->get_declaration()->get_decl_flags() & TXD_ABSTRACT)) {
        // FIXME: check that all abstract members of base types & interfaces are implemented
    }

    if (expErrWholeType) {
        /*int encountered_error_count =*/ this->get_driver()->end_exp_err(this->get_declaration()->get_definer()->get_parse_location());
        // FIXME: include in expected error count
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

bool TxType::is_pure_specialization() const {
    ASSERT(this->prepared, "Can't determine specialization degree of unprepared type: " << this);
    return ( this->baseTypeSpec.modifiable
             || ( this->has_base_type()
                  && !this->is_builtin()  // this being built-in implies that it is more concrete than base class
                  && typeid(*this) == typeid(*this->baseTypeSpec.type)
                  && ( this->genericBaseType
                       || ( !this->extendsInstanceDatatype && !this->modifiesVTable ) ) ) );
}

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
    return ( this->baseTypeSpec.modifiable
             || ( this->has_base_type()
                  && !this->is_builtin()  // being built-in implies that it is more specialized than base class
                  && typeid(*this) == typeid(*this->baseTypeSpec.type)
                  && !this->nonRefBindings
                  && ( ( this->genericBaseType && !this->genericBaseType->nonRefParameters )
                       || ( !this->extendsInstanceDatatype && !this->modifiesVTable ) ) ) );
}

bool TxType::is_virtual_derivation() const {
    ASSERT(this->prepared, "Can't determine specialization degree of unprepared type: " << this);
    return ( this->baseTypeSpec.modifiable
             || ( this->has_base_type()
                  && !this->is_builtin()  // being built-in implies that it is more specialized than base class
                  && typeid(*this) == typeid(*this->baseTypeSpec.type)
                  && !this->nonRefBindings
                  && !this->extendsInstanceDatatype ) );
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
                // FIXME: handle alias??
                LOGGER().warning("Looked-up member is not an entity: %s",  member->to_string().c_str());
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
    auto parentName = ident.parent().to_string();
    auto paramName = ident.name();
    TxScopeSymbol* vantageScope = type->get_nearest_declaration()->get_symbol();
    const TxType* semBaseType = type->get_semantic_base_type();
    while (semBaseType) {
        if (get_type_param_decl(semBaseType->type_params(), fullParamName)) {
            // baseType is the (nearest) type that declares the sought parameter
            if (auto memberEnt = type->get_instance_member(vantageScope, paramName))
                return memberEnt;
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
//const TxGenericBinding* TxType::resolve_param_binding(const std::string& paramName) const {
//    // note: does not check for transitive modifiability
//    ASSERT(paramName.find_last_of('#') == std::string::npos, "Non-plain type parameter name provided: " << paramName);
//    if (this->has_type_param(paramName))
//        return nullptr;  // type parameter is unbound  // TODO: return constraint base type instead
//    else if (auto baseType = this->get_semantic_base_type()) {
//        if (this->baseTypeSpec.has_binding(paramName))
//            return &this->baseTypeSpec.get_binding(paramName);
//        return baseType->resolve_param_binding(paramName);
//    }
//    else
//        return nullptr;  // no such type parameter name in type specialization hierarchy
//}


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
    while (!thisType->get_explicit_declaration() && thisType->is_empty_derivation())
        thisType = thisType->get_semantic_base_type();
    while (!otherType->get_explicit_declaration() && otherType->is_empty_derivation())
        otherType = otherType->get_semantic_base_type();
    return thisType->inner_equals(*otherType);
}


bool TxType::is_assignable_to(const TxType& destination) const {
    // fields must at least be the same instance data type
    auto thisType = this;
    do {
        if (*thisType == destination)
            return true;
        if (this->is_same_instance_type())
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
    ASSERT(! this->genericBaseType, "Invoked is_a() on concrete specialization type: " << this);
    ASSERT(! other.genericBaseType, "Invoked is_a() with other being a concrete specialization type: " << this);

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

static void type_bindings_string(std::stringstream& str, const std::vector<TxEntityDeclaration*>& bindings) {
    str << "<";
    int ix = 0;
    for (auto b : bindings) {
        if (ix++)  str << ",";
        //str << b->get_unique_full_name();
        str << b->get_definer()->get_type()->to_string(true, true);
    }
    str << ">";
}

std::string TxType::to_string(bool brief, bool skipFirstName, bool skipImplicitNames) const {
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

        if (! this->get_bindings().empty())
            type_bindings_string(str, this->get_bindings());

        this->get_base_type()->self_string(str, true, false, skipImplicitNames);  // set 'brief' to false to print entire type chain
    }
}

/*
void TxType::self_string(std::stringstream& str, bool brief, bool skipFirstName) const {
    if (this->is_modifiable())
        str << "MOD ";
    TxEntitySymbol* explSymbol = nullptr;
    if (! skipFirstName)
        if (auto explDecl = this->get_declaration())
            explSymbol = explDecl->get_symbol();
    if (brief && explSymbol) {
        str << explSymbol->get_full_name();
        if (this->is_generic())
            str << type_params_string(this->type_params());
    }
    else if (this->has_base_type()) {
        bool separator = false;
        if (explSymbol) {
            str << explSymbol->get_full_name();
            if (this->is_generic())
                str << type_params_string(this->type_params());
            separator = true;
        }
        //else if (!this->is_pure_specialization() || this->is_generic() || typeid(*this) != typeid(*baseTypeSpec.type)) {
        else if (this->is_generic() || typeid(*this) != typeid(*baseTypeSpec.type)) {
            str << typeid(*this).name();
            if (this->is_generic())
                str << type_params_string(this->type_params());
            separator = true;
        }

        if (! this->get_bindings().empty()) {
            type_bindings_string(str, this->baseTypeSpec, this->get_bindings());
            separator = true;
        }
        if (separator)
            str << " : ";
        else if (skipFirstName)
            str << ": ";
        this->baseTypeSpec.type->self_string(str, true, false);  // set 'brief' to false to print entire type chain
    }
    else if (explSymbol)
        str << explSymbol->get_full_name();
    else
        str << typeid(*this).name();
}
*/



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
    if (auto paramDecl = this->lookup_value_param_binding("tx.Array.L")) {
        return paramDecl->get_definer()->get_init_expression();
    }
    LOGGER().note("Unbound length for array type %s", this->to_string().c_str());
    return nullptr;
}

const TxType* TxArrayType::element_type() const {
    if (auto paramDecl = this->lookup_type_param_binding("tx.Array.E")) {
        ResolutionContext resCtx;
        if (auto type = paramDecl->get_definer()->resolve_type(resCtx))
            return type;
    }
    LOGGER().note("Unbound element type for array type %s", this->to_string().c_str());
    ASSERT(this->is_generic(), "Unbound element type for NON-GENERIC array type " << this);
    return this->get_root_any_type();  // we know the basic constraint type for element is Any
    /*
    if (! type) {
        const TxGenericBinding* binding = this->resolve_param_binding("E");
        if (binding && binding->meta_type() == TxTypeParam::MetaType::TXB_TYPE) {
            type = binding->type_definer().resolve_type(resCtx);
        }
    }

    if (type) {
        if (! this->is_modifiable()) {  // TODO: review, uncertain if this has effect
            // non-modifiability transitively applies to TYPE type parameters (NOTE: except for references)
            LOGGER().debug("transitive non-modifiability for array %s", this->to_string().c_str());
            return type->is_modifiable() ? type->get_semantic_base_type() : type;
        }
        return type;
    }
    LOGGER().warning("NULL element type for array %s", this->to_string().c_str());
    return nullptr;  // no such type parameter name in type specialization hierarchy
    */
}


const TxType* TxReferenceType::target_type() const {
    std::string declstr = (this->get_declaration() ? this->get_declaration()->get_unique_full_name() : "nodecl");
    //std::cerr << "getting target type of " << declstr << ": " << this << std::endl;
    if (auto paramDecl = this->lookup_type_param_binding("tx.Ref.T")) {
        ResolutionContext resCtx;
        if (auto type = paramDecl->get_definer()->resolve_type(resCtx))
            return type;
    }
    if (this->get_base_type()->get_type_class() != TXTC_ANY)
        LOGGER().warning("Unbound target type for reference type %s", this->to_string().c_str());
    ASSERT(this->is_generic(), "Unbound target type for NON-GENERIC reference type " << this);
    return this->get_root_any_type();  // we know the basic constraint type for ref target is Any
    /*
    // special resolution implementation for the tx#Ref#T member - this forces resolution of the reference target type
    const std::string targetMemName = "tx#Ref#T";
    for ( const TxType* type = this; type; type = type->get_base_type() ) {
        if (auto decl = type->get_declaration()) {
            //LOGGER().note("Looking up tx#Ref#T in type %s", decl->get_symbol()->get_full_name().to_string().c_str());
            if (auto member = lookup_member(decl->get_symbol(), decl->get_symbol(), targetMemName)) {
                TxTypeDeclaration* typeDecl = nullptr;
                if (auto memberEnt = dynamic_cast<TxEntitySymbol*>(member)) {
                    ASSERT(memberEnt->get_type_decl(), "expected symbol to represent a type declaration: " << member->to_string());
                    typeDecl = memberEnt->get_type_decl();
                }
                else if (auto memberAlias = dynamic_cast<TxAliasSymbol*>(member)) {
                    LOGGER().note("Substituting alias %s with %s", memberAlias->get_full_name().to_string().c_str(),
                                  memberAlias->get_aliased_declaration()->to_string().c_str());
                    typeDecl = dynamic_cast<TxTypeDeclaration*>(memberAlias->get_aliased_declaration());
                }
                else
                    CERROR(this, "Ref target '" << targetMemName << "' is not an entity or alias: " << member);

                if (typeDecl) {
                    ResolutionContext resCtx;
                    if (auto ttype = typeDecl->get_definer()->resolve_type(resCtx)) {
                        //LOGGER().note("Resolved target type of reference type %s to %s", this->to_string().c_str(), ttype->to_string().c_str());
                        return ttype;
                    }
                    else
                        LOGGER().alert("Failed to resolve target type of reference type %s", this->to_string().c_str());
                }
                break;
            }
        }
    }
    //const TxType* ttype = this->resolve_param_type(resCtx, "tx#Ref#T", true);

    const TxType* ttype = nullptr;
    auto binding = this->resolve_param_binding("T");
    if (binding && binding->meta_type() == TxTypeParam::MetaType::TXB_TYPE) {
        ResolutionContext resCtx;
        ttype = binding->type_definer().resolve_type(resCtx);
        if (ttype)
            LOGGER().debug("Resolved target type of reference type to %s", ttype->to_string().c_str());
        else
            LOGGER().alert("Failed to resolve target type for reference type %s", this->to_string().c_str());
    }
    else {
        LOGGER().debug("Unbound target type for reference type %s", this->to_string().c_str());
        ttype = this->get_root_any_type();  // we know the basic constraint type for ref target is Any
    }
    return ttype;
    */
}



void TxInterfaceAdapterType::prepare_type_members() {
    TxType::prepare_type_members();

    LOGGER().debug("preparing adapter for %s to interface %s", this->adaptedType->to_string().c_str(), this->get_semantic_base_type()->to_string().c_str());
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
                              this->adaptedType->to_string().c_str(), protoField->to_string().c_str());
        }
        else {
            auto targetField = adapteeVirtualFields.get_field(f.first);
            // FIXME: verify that type matches
            this->virtualFields.override_field(f.first, targetField);
        }
    }

    this->modifiesVTable = true;
}
