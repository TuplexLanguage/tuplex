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
           && this->modifiable == other.modifiable
           && this->bindings == other.bindings;
}

std::string TxTypeSpecialization::to_string() const { return "specialization of " + this->type->to_string(); }

void TxTypeSpecialization::validate() const {
    if (this->type->is_modifiable())
        CERROR(this->type, "Can't specialize a 'modifiable' type (specialize its base type instead): " << this->type);
    if (this->modifiable) {
        if (this->type->is_immutable())
            CERROR(this->type, "Can't make an immutable type modifiable: " << this->type);
        if (! this->bindings.empty())
            CERROR(this->type, "Can't bind type parameters on top of a 'modifiable' type.");
        if (this->dataspace)
            CERROR(this->type, "Can't specify dataspace for a 'modifiable' specialization");
        return;
    }
    if (this->dataspace && this->type->get_type_class() != TXTC_REFERENCE)
        CERROR(this->type, "Specified dataspace for non-referernce type " << this->type->to_string());
    ResolutionContext resCtx;
    for (auto & b : this->bindings) {
        if (this->type->has_type_param(b.param_name())) {
            // validate metatype and constraints
            auto & p = this->type->get_type_param(b.param_name());
            if (b.meta_type() != p.meta_type())
                CERROR(this->type, "Binding for type parameter " << p.to_string() << " of wrong meta-type (TYPE vs VALUE)");
            if (b.meta_type() == TxTypeParam::MetaType::TXB_VALUE) {
                // TODO: check: VALUE parameters can not be of modifiable type
            }
            else {
                auto constraintType = p.get_constraint_type_definer()->resolve_type(resCtx);
                auto boundType = b.type_definer().resolve_type(resCtx);
                //std::cerr << this->type << ": Constraint type for param " << p.param_name() << ": " << "checking bound type " << boundType << "\tagainst constraint type " << constraintType << std::endl;
                if (boundType && !boundType->is_a(*constraintType))
                    CERROR(boundType, "Bound type " << boundType->to_string() << " for type parameter " << p.to_string() << " is not a derivation of type " + constraintType->to_string());
                    // FIXME: can we move this check to where the binding is provided in the source text?
            }
        }
        else
            CERROR(this->type, "No type parameter matches provided binding " + b.to_string());
    }
}



/*=== TxType implementation ===*/

bool TxType::validate() const {
    //std::cerr << "validating type " << this << std::endl;
    if (this->baseTypeSpec.type) {
        this->baseTypeSpec.validate();

        if (this->baseTypeSpec.modifiable) {
            // verify that this 'modifiable' type usage is a pure specialization
            if (typeid(*this) != typeid(*this->baseTypeSpec.type))
                CERROR(this, "'modifiable' specialization must have same TxType class as the base type: " << this->baseTypeSpec.type->to_string());
            if (! this->interfaces.empty())
                CERROR(this, "'modifiable' specialization cannot add any interface base types");
        }
        else {
            // verify that all parameters of base type are either bound, or redeclared
            for (auto & p : this->baseTypeSpec.type->type_params()) {
                if (! this->baseTypeSpec.has_binding(p.param_name()))
                    if (! this->has_type_param(p.param_name()))
                        CERROR(this, "No binding or redeclaration of base type's type parameter " + p.to_string());
            }
        }
        if (! this->baseTypeSpec.type->get_declaration())
            // also validate any anonymous base types (otherwise their validate() won't be called)
            this->baseTypeSpec.type->validate();
    }
    // FUTURE: validate interfaces
    // check that generic interfaces can't be implemented multiple times throughout a type hierarchy,
    // unless their type arguments are exactly the same

    return true;  // do away with these return values?
}


void TxType::prepare_type() {
    LOGGER().debug("Preparing type %s", this->to_string().c_str());

    // resolve all this type's parameters and bindings
    ResolutionContext resCtx;
    for (auto & p : this->typeParams) {
        auto constraintType = p.get_constraint_type_definer()->resolve_type(resCtx);
        if (p.meta_type() != TxTypeParam::MetaType::TXB_TYPE || constraintType->get_type_class() != TXTC_REFERENCE)
            this->nonRefParameters = true;
    }
    for (auto & b : this->baseTypeSpec.bindings) {
        if (b.meta_type() == TxTypeParam::MetaType::TXB_TYPE) {
            //std::cout << "resolving binding " << b.param_name() << " of " << this->to_string() << std::endl;
            if (! (this->get_type_class() == TXTC_REFERENCE && b.param_name() == "T")) {
                b.type_definer().resolve_type(resCtx);

                auto & param = this->baseTypeSpec.type->get_type_param(b.param_name());
                if (param.get_constraint_type_definer()->get_type()->get_type_class() != TXTC_REFERENCE) {
                    this->nonRefBindings = true;
                    //LOGGER().info("Binding for parameter '%s' is non-virtual", b.param_name().c_str());
                }
            }
        }
        else
            this->nonRefBindings = true;
    }

    // copy base type's virtual and instance field tuples (to which we will add and override fields):
    auto baseType = this->get_base_data_type();
    if (baseType) {
        this->virtualFields = baseType->virtualFields;
        this->instanceMethods = baseType->instanceMethods;
        this->instanceFields = baseType->instanceFields;
    }

    if (! this->get_declaration()) {
        bool madeConcrete = !this->is_generic() && this->get_base_type() && this->get_base_type()->is_generic();
        ASSERT(! madeConcrete, "Type is concrete specialization of generic base type, but unnamed (undeclared): " << this);
        return;
    }

    // for all the member names declared or redeclared in this type:
    auto typeDeclNamespace = this->get_declaration()->get_symbol();
    for (auto symname = typeDeclNamespace->symbol_names_cbegin(); symname != typeDeclNamespace->symbol_names_cend(); symname++) {
        // this drives resolution of all this type's members

        auto entitySym = dynamic_cast<TxEntitySymbol*>(typeDeclNamespace->get_member_symbol(*symname));
        if (! entitySym)
            continue;

        if (auto typeDecl = entitySym->get_type_decl()) {
            if (*symname != "tx#Ref#T") {  // prevents infinite recursion
                //LOGGER().alert("resolving member type %s", entitySym->get_full_name().to_string().c_str());
                auto type = typeDecl->get_definer()->resolve_type(resCtx);

                if (typeDecl->get_unique_name() == "$GenericBase") {
                    //LOGGER().alert("Generic base type of %s is %s", entitySym->get_full_name().to_string().c_str(), type->to_string().c_str());
                    this->genericBaseType = type;
                }
            }
        }

        for (auto fieldDecl = entitySym->fields_cbegin(); fieldDecl != entitySym->fields_cend(); fieldDecl++) {
            auto field = (*fieldDecl)->get_definer()->resolve_field(resCtx);

            if ((*fieldDecl)->get_decl_flags() & TXD_ABSTRACT)
                if (this->get_type_class() != TXTC_INTERFACE && !(this->get_declaration()->get_decl_flags() & TXD_ABSTRACT))
                    CERROR(this, "Can't declare abstract member in type that is not declared abstract: " << field);

            if (field) {
                // validate type:
                if (auto fieldType = field->get_type()) {
                    if (! fieldType->is_concrete()) {
                        CERROR(this, "Can't declare a field of non-concrete type: " << field << " " << fieldType);
                    }
                    else if (field->get_storage() == TXS_INSTANCE) {
                        if (! fieldType->is_statically_sized()) {
                            CERROR(this, "Instance fields that don't have statically determined size not yet supported: " << field);
                        }
                        else if (! (field->get_decl_flags() & (TXD_GENPARAM | TXD_IMPLICIT))) {
                            if (this->get_type_class() != TXTC_TUPLE)
                                CERROR(this, "Can't declare instance member in non-tuple type: " << field);
                        }
                    }
                    else {  // static / virtual
                        if (! fieldType->is_statically_sized()) {
                            // since static fields are per generic base type, and not per specialization:
                            CERROR(this, "Static fields must have statically determined size: " << field);
                        }
                    }
                }

                // layout:
                if (field->get_storage() == TXS_INSTANCE) {
                    LOGGER().debug("Laying out instance field %-40s  %s  %u", field->to_string().c_str(),
                                   field->get_type()->to_string(true).c_str(), this->instanceFields.get_field_count());
                    if (field->get_decl_flags() & TXD_ABSTRACT)
                        CERROR(this, "Can't declare an instance field as abstract: " << field);
                    this->instanceFields.add_field(field->get_unique_name(), field);
                    if (field->get_unique_name().find('#') == std::string::npos)
                        // FIXME: make VALUE type params be declared as instance members in generic base type,
                        // so that they are not "extensions" to the specialized subtypes, and remove this '#' test
                        this->extendsInstanceDatatype = true;
                }
                else if (field->get_storage() == TXS_INSTANCEMETHOD) {
                    if (field->get_decl_flags() & TXD_CONSTRUCTOR) {
                        // skip, constructors aren't virtual
                    }
                    else if (this->virtualFields.has_field(field->get_unique_name())) {
                        CERROR(this, "A non-static method may not override a static parent field: " << field);
                    }
                    else if (this->instanceMethods.has_field(field->get_unique_name())) {
                        if (! (field->get_decl_flags() & TXD_OVERRIDE))
                            CWARNING(this, "Field overrides but isn't declared 'override': " << field);
                        auto overriddenField = this->instanceMethods.get_field(field->get_unique_name());
                        if (overriddenField->get_decl_flags() & TXD_FINAL)
                            CERROR(this, "Can't override a base type field that is declared 'final': " << field);
                        if (! (field->get_type()->is_a(*overriddenField->get_type())))
                            CERROR(this, "Overriding member's type does not derive from overridden member's type: " << field->get_type());
                        this->instanceMethods.override_field(field->get_unique_name(), field);
                        this->modifiesVTable = true;
                    }
                    else {
                        if (field->get_decl_flags() & TXD_OVERRIDE)
                            CWARNING(this, "Field doesn't override but is declared 'override': " << field);
                        this->instanceMethods.add_field(field->get_unique_name(), field);
                        this->modifiesVTable = true;
                    }
                }
                else if (field->get_storage() == TXS_VIRTUAL) {
                    if (this->instanceMethods.has_field(field->get_unique_name())) {
                        CERROR(this, "A static field may not override a non-static parent method: " << field);
                    }
                    else if (this->virtualFields.has_field(field->get_unique_name())) {
                        if (! (field->get_decl_flags() & TXD_OVERRIDE))
                            CWARNING(this, "Field overrides but isn't declared 'override': " << field);
                        auto overriddenField = this->virtualFields.get_field(field->get_unique_name());
                        if (overriddenField->get_decl_flags() & TXD_FINAL)
                            CERROR(this, "Can't override a base type field that is declared 'final': " << field);
                        if (! (field->get_type()->is_a(*overriddenField->get_type())))
                            CERROR(this, "Overriding member's type does not derive from overridden member's type: " << field->get_type());
                        this->virtualFields.override_field(field->get_unique_name(), field);
                        this->modifiesVTable = true;
                    }
                    else {
                        if (field->get_decl_flags() & TXD_OVERRIDE)
                            CWARNING(this, "Field doesn't override but is declared 'override': " << field);
                        this->virtualFields.add_field(field->get_unique_name(), field);
                        this->modifiesVTable = true;
                    }
                }
                else {
                    ASSERT(field->get_storage() == TXS_STATIC, "Invalid storage class " << field->get_storage() << " for field member " << *field);
                    if (field->get_decl_flags() & TXD_ABSTRACT)
                        CERROR(this, "Can't declare a non-virtual field as abstract: " << field);
                    if (field->get_decl_flags() & TXD_OVERRIDE)
                        CWARNING(this, "Field doesn't override but is declared 'override': " << field);
                    this->staticFields.add_field(field->get_unique_name(), field);
                }
            }
        }
    }

    if (! (this->get_declaration()->get_decl_flags() & TXD_ABSTRACT)) {
        // FIXME: check that all abstract members of base types & interfaces are implemented
    }
}


const TxTypeDeclaration* TxType::get_explicit_declaration() const {
    if (this->get_declaration() && !(this->get_declaration()->get_decl_flags() & (TXD_IMPLICIT | TXD_GENPARAM)))
        return this->get_declaration();
    return nullptr;
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
    return ( this->baseTypeSpec.modifiable
             || ( this->has_base_type() && this->interfaces.empty()
                  && !this->is_builtin()  // this being built-in implies that it is more concrete than base class
                  && typeid(*this) == typeid(*this->baseTypeSpec.type)
                  && ( this->genericBaseType
                       || ( !this->extendsInstanceDatatype && !this->modifiesVTable ) ) ) );
}

bool TxType::is_empty_derivation() const {
    return ( this->has_base_type()
             && this->interfaces.empty()
             && !this->baseTypeSpec.modifiable
             && !this->is_builtin()  // this being built-in implies that it is more concrete than base class
             && typeid(*this) == typeid(*this->baseTypeSpec.type)
             && this->baseTypeSpec.bindings.empty()
             && !this->genericBaseType
             && !this->extendsInstanceDatatype
             && !this->modifiesVTable );
}

bool TxType::is_equivalent_derivation() const {
    return ( this->baseTypeSpec.modifiable
             || ( this->has_base_type()
                  && this->interfaces.empty()
                  && !this->is_builtin()  // this being built-in implies that it is more concrete than base class
                  && typeid(*this) == typeid(*this->baseTypeSpec.type)
                  && !this->nonRefBindings
                  && ( ( this->genericBaseType && !this->genericBaseType->nonRefParameters )
                       || ( !this->extendsInstanceDatatype && !this->modifiesVTable ) ) ) );
}

bool TxType::is_virtual_derivation() const {
    return ( this->baseTypeSpec.modifiable
             || ( this->has_base_type()
                  && !this->is_builtin()  // this being built-in implies that it is more concrete than base class
                  && typeid(*this) == typeid(*this->baseTypeSpec.type)
                  && !this->nonRefBindings
                  && !this->extendsInstanceDatatype ) );
}

bool TxType::is_statically_sized() const {
    return this->is_concrete();
}



TxEntitySymbol* TxType::lookup_instance_member(const std::string& name) const {
    return this->lookup_instance_member(this->get_nearest_declaration()->get_symbol(), name);
}

TxEntitySymbol* TxType::lookup_instance_member(TxScopeSymbol* vantageScope, const std::string& name) const {
    for (const TxType* type = this; type; type = type->baseTypeSpec.type) {
        if (auto decl = type->get_declaration()) {
            if (auto member = lookup_member(vantageScope, decl->get_symbol(), name)) {
                if (auto memberEnt = dynamic_cast<TxEntitySymbol*>(member))
                    return memberEnt;
                else
                    // FIXME: handle alias??
                    LOGGER().warning("Looked-up member is not an entity: %s",  member->to_string().c_str());
                break;
            }
        }
    }
    return nullptr;

//    // FIX ME: the provided symbol name won't match the field's unique name if it's overloaded
//    if (this->instanceMethods.has_field(name))
//        return this->instanceMethods.get_field(name)->get_declaration()->get_symbol();
//    if (this->instanceFields.has_field(name))
//        return this->instanceFields.get_field(name)->get_declaration()->get_symbol();
//    if (this->staticFields.has_field(name))
//        return this->staticFields.get_field(name)->get_declaration()->get_symbol();
//    if (this->virtualFields.has_field(name))
//        return this->virtualFields.get_field(name)->get_declaration()->get_symbol();
//    if (auto type = this->lookup_member_type(name))
//        return type->get_declaration()->get_symbol();
//    return nullptr;
}


const TxGenericBinding* TxType::resolve_param_binding(const std::string& paramName) const {
    // note: does not check for transitive modifiability
    ASSERT(paramName.find_last_of('#') == std::string::npos, "Non-plain type parameter name provided: " << paramName);
    if (this->has_type_param(paramName))
        return nullptr;  // type parameter is unbound  // TODO: return constraint base type instead
    else if (auto baseType = this->get_base_type()) {
        if (this->baseTypeSpec.has_binding(paramName))
            return &this->baseTypeSpec.get_binding(paramName);
        return baseType->resolve_param_binding(paramName);
    }
    else
        return nullptr;  // no such type parameter name in type specialization hierarchy
}


bool TxType::operator==(const TxType& other) const {
    // skips empty type derivations that aren't explicitly declared
    const TxType* thisType = this;
    const TxType* otherType = &other;
    while (!thisType->get_explicit_declaration() && thisType->is_empty_derivation())
        thisType = thisType->get_base_type();
    while (!otherType->get_explicit_declaration() && otherType->is_empty_derivation())
        otherType = otherType->get_base_type();
    auto explDecl = thisType->get_explicit_declaration();
    return explDecl == otherType->get_explicit_declaration()  // same declaration or both null
           && ( explDecl
                // if unnamed but identical, pure specialization:
                || ( typeid(*thisType) == typeid(*otherType)
                     && thisType->baseTypeSpec == otherType->baseTypeSpec
                     && thisType->type_params() == otherType->type_params() ) );
    // (interfaces and members can only apply to a type with an explicit declaration, and an explicit declaration can have only one type instance)
}


bool TxType::is_a(const TxType& other) const {
    //std::cerr << *this << "  IS-A\n" << other << std::endl;
    if (other.get_type_class() == TXTC_INTERFACE)
        return this->derives_interface(other);

    if (*this == other)
        return true;

    // by-pass anonymous, virtual specializations:
//    if (! this->get_explicit_declaration()) {
//        if (this->is_virtual_derivation())
//            return this->get_base_type()->is_a(other);
//    }
    if (! other.get_explicit_declaration()) {
        if (other.is_virtual_derivation())
            return this->is_a(*other.get_base_type());
    }

    // check whether other is a more generic version of the same type:
    if (auto genBaseType = this->common_generic_base_type(other)) {
        for (auto & param : genBaseType->type_params()) {
            // other's param shall either be redeclared (generic) or *equal* to this (is-a is not sufficient in general case)
            // TODO: more thorough analysis of which additional cases may be compatible
            if (auto otherBinding = other.resolve_param_binding(param.param_name())) {
                if (auto thisBinding = this->resolve_param_binding(param.param_name())) {
                    if (*thisBinding != *otherBinding)  // checks whether both bindings resolve to same type/value
                        return false;
                }
                else
                    return false;
            }
        }
        return true;
    }

    return (this->has_base_type() && this->get_base_type()->inner_is_a(other));
}

const TxType* TxType::common_generic_base_type(const TxType& other) const {
    if (! this->get_explicit_declaration() && this->is_pure_specialization())
        return this->get_base_type()->common_generic_base_type(other);
    if (! other.get_explicit_declaration() && other.is_pure_specialization())
        return this->common_generic_base_type(*other.get_base_type());
    if (*this == other)
        return this;
    return nullptr;
}

bool TxType::inner_is_a(const TxType& other) const {
    // check whether any parent type that this type specializes is-a of the other type:
    if (*this == other)
        return true;
    if (this->has_base_type())
        if (this->get_base_type()->inner_is_a(other))
            return true;
    return false;
}

bool TxType::derives_interface(const TxType& interface) const {
    // check whether any parent type that this type specializes is-a of the other type:
    //std::cerr << *this << "  DERIVES-INTERFACE\n" << interface << std::endl;
    if (*this == interface)
        return true;

    // by-pass anonymous, empty specializations:
    if (! interface.get_explicit_declaration()) {
        if (interface.is_equivalent_derivation())
            return this->derives_interface(*interface.get_base_type());
    }

    if (this->has_base_type()) {
        if (this->get_base_type()->derives_interface(interface))
            return true;
        for (auto & interfSpec : this->interfaces) {
            // TODO: This may not suffice - does not take modifiable, bindings, or dataspace of spec into account
            if (interfSpec.type->derives_interface(interface))
                return true;
        }
    }
    return false;
}


static void type_bindings_string(std::stringstream& str, const TxTypeSpecialization& specialization) {
    if (specialization.dataspace)
        str << specialization.dataspace << "& ";
    str << "<";
    int ix = 0;
    for (auto & b : specialization.bindings) {
        if (ix++)  str << ",";
        str << b;
    }
    str << ">";
}

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
            str << this->type_params_string();
    }
    else if (this->has_base_type()) {
        bool separator = false;
        if (explSymbol) {
            str << explSymbol->get_full_name();
            if (this->is_generic())
                str << this->type_params_string();
            separator = true;
        }
        else if (!this->is_pure_specialization() || this->is_generic() || typeid(*this) != typeid(*baseTypeSpec.type)) {
            str << typeid(*this).name();
            if (this->is_generic())
                str << this->type_params_string();
            separator = true;
        }

        if (! this->baseTypeSpec.bindings.empty()) {
            type_bindings_string(str, this->baseTypeSpec);
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



/*=== ArrayType and ReferenceType implementation ===*/

bool TxArrayType::is_statically_sized() const {
    return this->is_concrete() && this->element_type()->is_statically_sized()
           && this->length()->is_statically_constant();
}

bool TxArrayType::innerAutoConvertsFrom(const TxType& otherType) const {
    if (const TxArrayType* otherArray = dynamic_cast<const TxArrayType*>(&otherType)) {
        // if other has unbound type params that this does not, other is more generic and can't be auto-converted to this
        if (auto e = this->element_type()) {
            if (auto otherE = otherArray->element_type()) {
                // note: is-a test insufficient for array elements, since same concrete type (same size) required
                if (*e != *otherE)
                    return false;
            }
            else
                return false;  // other has not bound E
        }
        if (auto len = this->length()) {
            if (auto otherLen = otherArray->length()) {
                return (len->get_static_constant_proxy() && otherLen->get_static_constant_proxy()
                        && *len->get_static_constant_proxy() == *otherLen->get_static_constant_proxy());
            }
            else
                return false;  // other has not bound L
        }
        return true;
    }
    return false;
}


bool TxReferenceType::innerAutoConvertsFrom(const TxType& otherType) const {
    if (const TxReferenceType* otherRef = dynamic_cast<const TxReferenceType*>(&otherType)) {
        // if other has unbound type params that this does not, other is more generic and can't be auto-converted to this
        if (auto target = this->target_type()) {
            if (auto otherTarget = otherRef->target_type()) {
                // is-a test sufficient for reference targets (it isn't for arrays, which require same concrete type)
                //std::cout << "CHECKING AUTOCONV FROM\n" << *otherTarget->get_type() << "\nTO\n" << *target->get_type() << std::endl;
                if (! otherTarget->is_a(*target))
                    return false;
                else if (target->is_modifiable() && !otherTarget->is_modifiable())
                    return false;  // can't lose modifiable attribute of target
                else
                    return true;
            }
            else
                return false;  // other has not bound T
        }
        else
            return true;
    }
    else
        return false;
}



const TxExpressionNode* TxArrayType::length() const {
    // TODO: resolve via symbol table
    if (auto binding = this->resolve_param_binding("L")) {
        if (binding->meta_type() == TxTypeParam::MetaType::TXB_VALUE) {
            const TxExpressionNode* len = &binding->value_definer();
            return len;
        }
    }
    return nullptr;  // no such type parameter name in type specialization hierarchy
}

const TxType* TxArrayType::element_type() const {
    const TxType* type = nullptr;
    if (auto memberEnt = this->lookup_instance_member("tx#Array#E")) {
        if (memberEnt->get_type_decl())
            type = memberEnt->get_type_decl()->get_definer()->get_type();
    }
    if (! type) {
        const TxGenericBinding* binding = this->resolve_param_binding("E");
        if (binding && binding->meta_type() == TxTypeParam::MetaType::TXB_TYPE) {
            ResolutionContext resCtx;
            type = binding->type_definer().resolve_type(resCtx);
        }
    }

    if (type) {
        if (! this->is_modifiable()) {  // TODO: review, uncertain if this has effect
            // non-modifiability transitively applies to TYPE type parameters (NOTE: except for references)
            LOGGER().debug("transitive non-modifiability for array %s", this->to_string().c_str());
            return type->is_modifiable() ? type->get_base_type() : type;
        }
        return type;
    }
    LOGGER().warning("NULL element type for array %s", this->to_string().c_str());
    return nullptr;  // no such type parameter name in type specialization hierarchy
}


const TxType* TxReferenceType::target_type() const {
    // special resolution implementation for the tx#Ref#T member - this forces resolution of the reference target type
    const std::string targetMemName = "tx#Ref#T";
    for ( const TxType* type = this; type; type = type->get_base_type() ) {
        if (auto decl = type->get_declaration()) {
            if (auto member = lookup_member(decl->get_symbol(), decl->get_symbol(), targetMemName)) {
                TxTypeDeclaration* typeDecl = nullptr;
                if (auto memberEnt = dynamic_cast<TxEntitySymbol*>(member)) {
                    ASSERT(memberEnt->get_type_decl(), "expected symbol to represent a type declaration: " << member->to_string());
                    typeDecl = memberEnt->get_type_decl();
                }
                else if (auto memberAlias = dynamic_cast<TxAliasSymbol*>(member)) {
                    LOGGER().alert("Substituting alias %s with %s", memberAlias->get_full_name().to_string().c_str(),
                                   memberAlias->get_aliased_declaration()->to_string().c_str());
                    typeDecl = dynamic_cast<TxTypeDeclaration*>(memberAlias->get_aliased_declaration());
                }
                else
                    CERROR(this, "Ref target '" << targetMemName << "' is not an entity or alias: " << member);

                if (typeDecl) {
                    ResolutionContext resCtx;
                    if (auto ttype = typeDecl->get_definer()->resolve_type(resCtx)) {
                        //LOGGER().alert("Resolved target type of reference type %s to %s", this->to_string().c_str(), ttype->to_string().c_str());
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
}


void TxInterfaceAdapterType::prepare_adapter() {
    LOGGER().debug("preparing adapter for %s to interface %s", this->adaptedType->to_string().c_str(), this->get_base_type()->to_string().c_str());
    // The virtual fields of the abstract base interface type are overridden to refer to
    // the correspondingly named fields of the adapted type.

    auto & adapteeInstanceMethods = this->adaptedType->get_instance_methods();
    for (auto & f : this->instanceMethods.fieldMap) {
        if (! adapteeInstanceMethods.has_field(f.first))
            CERROR(this, "Adapted type " << this->adaptedType << " does not define instance method " << f.first << " of " << this->get_base_type());
        else {
            auto targetField = adapteeInstanceMethods.get_field(f.first);
            // FIXME: verify that type matches
            this->instanceMethods.override_field(f.first, targetField);
        }
    }

    auto & adapteeVirtualFields = this->adaptedType->get_virtual_fields();
    for (auto & f : this->virtualFields.fieldMap) {
        if (f.first == "$adTypeId")
            continue; // this field is not overridden to refer to an adaptee field (and it's initialized together with the vtable)
        else if (! adapteeVirtualFields.has_field(f.first))
            CERROR(this, "Adapted type " << this->adaptedType << " does not define virtual field " << f.first);
        else {
            auto targetField = adapteeVirtualFields.get_field(f.first);
            // FIXME: verify that type matches
            this->virtualFields.override_field(f.first, targetField);
        }
    }

    this->modifiesVTable = true;
}
