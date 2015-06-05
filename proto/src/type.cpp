#include "logging.hpp"
#include "tx_error.hpp"

#include "type.hpp"
#include "ast.hpp"
#include "entity.hpp"


//static Logger* LOGGER() {
//    static Logger* LOG; // = Logger::get("PARSER");
//    if (! LOG)
//        LOG = &Logger::get("PARSER");
//    return LOG;
//}


bool TxConstantProxy::operator==(const TxConstantProxy& other) const {
    // simple since we so far only support UInt values
    // FUTURE: improve as more value getters added (note that auto-conversion needs to be supported)
    return this->get_value_UInt() == other.get_value_UInt();
}


//const TxType* TxNonModTypeProxy::get_type() const {
//    auto type = wrappedProxy->get_type();
//    return type->is_modifiable() ? type->get_base_type() : type;
//}



bool TxTypeSpecialization::operator==(const TxTypeSpecialization& other) const {
    return ( this->type == other.type
             || ( this->type && other.type && *this->type == *other.type) )
           && this->modifiable == other.modifiable
           && this->bindings == other.bindings;
}

std::string TxTypeSpecialization::to_string() const { return "specialization of " + this->type->to_string(); }

std::string TxTypeSpecialization::validate() const {
    if (this->type->is_modifiable())
        return std::string("Can't specialize a 'modifiable' type (specialize its base type instead).");
    if (this->modifiable) {
        if (this->type->is_immutable())
            return std::string("Can't make an immutable type modifiable.");
        if (! this->bindings.empty())
            return std::string("Can't bind type parameters on top of a 'modifiable' type.");
        if (this->dataspace)
            return std::string("Can't specify dataspace for a 'modifiable' specialization");
        return std::string();
    }
    if (this->dataspace && this->type->get_type_class() != TXTC_REFERENCE)
        return std::string("Specified dataspace for non-referernce type ") + this->type->to_string();
    if (! this->bindings.empty()) {
        for (auto & b : this->bindings) {
            if (this->type->has_type_param(b.param_name())) {
                // validate metatype and constraints
                auto p = this->type->get_type_param(b.param_name());
                if (b.meta_type() != p.meta_type())
                    return std::string("Binding for type parameter ") + p.to_string() + " of wrong meta-type (TYPE vs VALUE)";
                if (b.meta_type() == TxTypeParam::MetaType::TXB_VALUE) {
                    // TODO: check: VALUE parameters can not be of modifiable type
                }
                else {
                    if (p.has_base_type_definer()) {
                        auto constraintType = p.get_base_type_definer()->get_type();
                        auto boundType = b.type_definer().get_type();
                        if (boundType && ! boundType->is_a(*constraintType))
                            return std::string("Bound type ") + boundType->to_string() + " for type parameter " + p.to_string() + " is not a derivation of type " + constraintType->to_string();
                    }
                }
            }
            else
                return std::string("No type parameter matches provided binding " + b.to_string());
        }
    }
    return std::string();
}


std::string TxType::inner_validate() const {
    //std::cout << "validating type " << typeid(*this).name() << std::endl;
    if (auto baseType = this->get_base_type()) {
        auto res = this->baseTypeSpec.validate();
        if (! res.empty())
            return std::string("Invalid specialization of " + baseType->to_string() + ": " + res);
        if (this->baseTypeSpec.modifiable) {
            // verify that this 'modifiable' type usage is a pure specialization
            if (typeid(*this) != typeid(*this->baseTypeSpec.type))
                return std::string("'modifiable' specialization must have same TxType class as the base type: " + baseType->to_string());
            if (! this->interfaces.empty())
                return std::string("'modifiable' specialization cannot add any interface base types");
        }
        else {
            // verify that all parameters of base type are either bound, or redeclared
            for (auto & p : baseType->type_params()) {
                if (! this->baseTypeSpec.has_binding(p.param_name()))
                    if (! this->has_type_param(p.param_name()))
                        return std::string("No binding or redeclaration of base type's type parameter " + p.to_string());
            }
        }
        if (! this->baseTypeSpec.type->get_symbol())
            // also validate any anonymous base types (otherwise their validate() won't be called)
            return this->baseTypeSpec.type->inner_validate();
    }
    // FUTURE: validate interfaces
    // check that generic interfaces can't be implemented multiple times throughout a type hierarchy,
    // unless their type arguments are exactly the same
    return std::string();
}

bool TxType::validate() const {
    auto res = this->inner_validate();
    if (! res.empty()) {
        CERROR(this, res);
        return false;
    }
    return true;
}


const TxTypeDeclaration* TxType::get_explicit_declaration() const {
    if (this->get_declaration() && !(this->get_declaration()->get_decl_flags() & (TXD_IMPLICIT | TXD_GENPARAM)))
        return this->get_declaration();
    return nullptr;
}

bool TxType::is_builtin() const {
    return this->get_symbol() && (this->get_declaration()->get_decl_flags() & TXD_BUILTIN);
}

bool TxType::is_concrete() const {
    // A concrete type is not abstract, nor usually generic (references may be concrete while generic).
    if (this->is_abstract())
        return false;
    if (this->is_generic()) {
        // TODO: If all members concrete, then return true (also handle this for Array<Ref<>>)
        return false;
    }
    return true;
}

bool TxType::is_pure_specialization() const {
    return ( this->is_modifiable()
             || ( this->has_base_type() && this->interfaces.empty()
                  && !this->is_builtin()  // this being built-in implies that it is more concrete than base class
                  && typeid(*this) == typeid(*this->baseTypeSpec.type)
                  && !this->is_datatype_extension() ) );
}

bool TxType::is_empty_specialization() const {
    return ( this->has_base_type() && this->interfaces.empty()
             && !this->is_builtin()  // this being built-in implies that it is more concrete than base class
             && typeid(*this) == typeid(*this->baseTypeSpec.type)
             && this->baseTypeSpec.bindings.empty()
             && !this->baseTypeSpec.modifiable
             && !this->is_datatype_extension() );
}

bool TxType::is_virtual_specialization() const {
    return ( this->is_modifiable()
             || ( this->has_base_type()
                  && !this->is_builtin()  // this being built-in implies that it is more concrete than base class
                  && typeid(*this) == typeid(*this->baseTypeSpec.type)
                  && this->baseTypeSpec.bindings.empty()
                  && !this->is_datatype_extension() ) );
}

bool TxType::is_statically_sized() const {
    return this->is_concrete();
}


const TxType* TxType::get_base_type() const {
    if (! has_base_type())
        return nullptr;
    return this->baseTypeSpec.type;
}



//std::vector<std::string>::const_iterator TxType::member_names_cbegin() const {
//    return this->get_nearest_entity()->symbol_names_cbegin();
//}
//
//std::vector<std::string>::const_iterator TxType::member_names_cend()   const {
//    return this->get_nearest_entity()->symbol_names_cend();
//}

/*
TxSymbolScope* TxType::lookup_instance_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) const {
    if (this->entity())
        return this->entity()->lookup_instance_member(path, ident);
        // (the entity will in turn call this type's lookup_inherited_member() if it needs to, so don't call it from here)
    else
        return this->lookup_inherited_instance_member(path, ident);
}

TxSymbolScope* TxType::lookup_inherited_instance_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) const {
    if (this->has_base_type())
        return this->get_base_type()->lookup_instance_member(path, ident);
    // FUTURE: implemented interfaces
    return nullptr;
}

TxSymbolScope* TxType::lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) const {
    if (this->entity())
        return this->entity()->lookup_member(path, ident);
        // (the entity will in turn call this type's lookup_inherited_member() if it needs to, so don't call it from here)
    else
        return this->lookup_inherited_member(path, ident);
}

TxSymbolScope* TxType::lookup_inherited_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) const {
    if (this->has_base_type())
        return this->get_base_type()->lookup_member(path, ident);
    // FUTURE: implemented interfaces
    return nullptr;
}
*/

TxEntitySymbol* TxType::lookup_instance_member(const std::string& name) const {
    for (const TxType* type = this; type; type = type->get_base_type()) {
        if (auto decl = type->get_declaration()) {
            if (auto member = decl->get_symbol()->get_member_symbol(name)) {
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


const TxType* TxType::resolve_param_type(ResolutionContext& resCtx, const std::string& paramName, bool nontransitiveModifiability) const {
    const TxType* type = nullptr;
    if (auto memberEnt = this->lookup_instance_member(paramName)) {
        if (memberEnt->get_type_decl())
            type = memberEnt->get_type_decl()->get_type_definer()->get_type();
    }
    if (! type) {
        // TODO: resolve via symbol table?
        //LOGGER().alert("Resolving type parameter '%s' of type %s", paramName.c_str(), this->to_string().c_str());
        const TxGenericBinding* binding;
        size_t pos = paramName.find_last_of('#');
        if (pos != std::string::npos)
            binding = this->resolve_param_binding(paramName.substr(pos+1));
        else
            binding = this->resolve_param_binding(paramName);
        if (binding && binding->meta_type() == TxTypeParam::MetaType::TXB_TYPE) {
            type = binding->type_definer().resolve_type(resCtx);
            LOGGER().debug("Resolved type parameter '%s' of type that has no entity, to %s", paramName.c_str(), type->to_string().c_str());
        }
        else
            LOGGER().warning("Failed to resolve type parameter '%s' of type that has no entity: %s", paramName.c_str(), this->to_string().c_str());
    }

    if (type) {
        if (! this->is_modifiable() && ! nontransitiveModifiability)
            // non-modifiability transitively applies to TYPE type parameters (NOTE: except for references)
            return type->is_modifiable() ? type->get_base_type() : type;
        else
            return type;
    }
    return nullptr;  // no such type parameter name in type specialization hierarchy
}

const TxExpressionNode* TxType::resolve_param_value(ResolutionContext& resCtx, const std::string& paramName) const {
    // TODO: resolve via symbol table
    if (auto binding = this->resolve_param_binding(paramName)) {
        if (binding->meta_type() == TxTypeParam::MetaType::TXB_VALUE)
            return &binding->value_expr();
    }
    return nullptr;  // no such type parameter name in type specialization hierarchy
}



bool TxType::operator==(const TxType& other) const {
    auto explDecl = this->get_explicit_declaration();
    return explDecl == other.get_explicit_declaration()  // same declaration or both null
           && ( explDecl
                // if unnamed but identical, pure specialization:
                || ( typeid(*this) == typeid(other)
                     && this->baseTypeSpec == other.baseTypeSpec
                     && this->type_params() == other.type_params() ) );
    // (interfaces and members can only apply to a type with an explicit declaration, and an explicit declaration can have only one type instance)
}


bool TxType::is_a(const TxType& other) const {
    //std::cout << *this << "  IS-A\n" << other << std::endl;
    if (*this == other)
        return true;

    // by-pass anonymous, virtual specializations:
    if (! this->get_explicit_declaration()) {
        if (this->is_virtual_specialization())
            return this->get_base_type()->is_a(other);
    }
    if (! other.get_explicit_declaration()) {
        if (other.is_virtual_specialization())
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
    // FUTURE: also check interfaces
    return false;
}

void TxType::prepare_type() {
    if (this->initialized)
        return;
    ASSERT(!this->startedInit, "Recursive call to prepare_type() of " << *this);
    this->startedInit = true;
    LOGGER().debug("Preparing type %s", this->to_string().c_str());

    // resolve all this type's parameters and bindings
    ResolutionContext resCtx;
    for (auto & p : this->typeParams) {
        //if (p.has_base_type_definer())
            p.get_base_type_definer()->resolve_type(resCtx);
    }
    for (auto & b : this->baseTypeSpec.bindings) {
        if (b.meta_type() == TxTypeParam::MetaType::TXB_TYPE) {
            //std::cout << "resolving binding " << b.param_name() << " of " << this->to_string() << std::endl;
            if (! (this->get_type_class() == TXTC_REFERENCE && b.param_name() == "T"))
                b.type_definer().resolve_type(resCtx);
        }
    }

    // copy base type's virtual and instance field tuples (to which we will add and override fields):
    auto baseType = this->get_base_type();
    while (baseType && !baseType->get_symbol())
        baseType = baseType->get_base_type();
    if (baseType) {
        //baseType->ensure_data_laid_out();
        this->virtualFields = baseType->virtualFields;
        this->instanceMethods = baseType->instanceMethods;
        this->instanceFields = baseType->instanceFields;
    }

    if (! this->get_declaration()) {
        this->initialized = true;
        return;
    }

    // for all the member names declared or redeclared in this type:
    auto typeDeclNamespace = this->get_declaration()->get_symbol();
    for (auto symname = typeDeclNamespace->symbol_names_cbegin(); symname != typeDeclNamespace->symbol_names_cend(); symname++) {
        // this drives resolution of all this type's members

        auto entitySym = dynamic_cast<TxEntitySymbol*>(typeDeclNamespace->get_member_symbol(*symname));
        if (! entitySym)
            continue;

        if (entitySym->get_type_decl()) {
            if (*symname != "tx#Ref#T") {  // prevents infinite recursion
                //LOGGER().alert("resolving member type %s", entitySym->get_full_name().to_string().c_str());
                entitySym->get_type_decl()->get_type_definer()->resolve_type(resCtx);
            }
        }

        for (auto fieldDecl = entitySym->fields_cbegin(); fieldDecl != entitySym->fields_cend(); fieldDecl++) {
            auto field = (*fieldDecl)->get_field_definer()->resolve_field(resCtx);

            if (field) {
                // validate:
                if (auto fieldType = field->get_type()) {
                    if (! fieldType->is_concrete()) {
                        CERROR(this, "Can't declare a field of non-concrete type: " << field);
                    }
                    else if (field->get_storage() == TXS_INSTANCE) {
                        //std::cout << "Concrete INSTANCE field " << field << std::endl;
                        if (! fieldType->is_statically_sized()) {
                            CERROR(this, "Instance fields that don't have statically determined size not yet supported: " << field);
                        }
                        else if (! (field->get_decl_flags() & (TXD_GENPARAM | TXD_IMPLICIT))) {
                            if (this->get_type_class() != TXTC_TUPLE)
                                CERROR(this, "Can't declare instance member in non-tuple type: " << field);
                        }
                    }
                    else {  // TXS_STATIC
                        //std::cout << "Concrete STATIC field " << field << std::endl;
                        if (! fieldType->is_statically_sized()) {
                            // since static fields are per generic base type, and not per specialization:
                            CERROR(this, "Static fields must have statically determined size: " << field);
                        }
                    }
                }

                // layout:
                if (field->get_storage() == TXS_INSTANCE) {
                    LOGGER().debug("Laying out instance field %-40s  %s", field->to_string().c_str(),
                                    field->get_type()->to_string(true).c_str());
                    this->instanceFields.add_field(field->get_unique_name(), field);
                    if (field->get_unique_name().find('#') == std::string::npos)
                        // FIXME: make VALUE type params be declared as instance members in generic base type,
                        // so that they are not "extensions" to the specialized subtypes, and remove this '#' test
                        this->extendsParentDatatype = true;
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
                        if (! (field->get_type()->is_a(*this->instanceMethods.get_field(field->get_unique_name())->get_type())))
                            CERROR(this, "Overriding member's type does not derive from overridden member's type: " << field->get_type());
                        this->instanceMethods.override_field(field->get_unique_name(), field);
                    }
                    else {
                        if (field->get_decl_flags() & TXD_OVERRIDE)
                            LOGGER().warning("Field doesn't override but is declared 'override': %s", field->to_string().c_str());
                        this->instanceMethods.add_field(field->get_unique_name(), field);
                        this->extendsParentDatatype = true;
                    }
                }
                else if (field->get_storage() == TXS_VIRTUAL) {
                    if (this->instanceMethods.has_field(field->get_unique_name())) {
                        CERROR(this, "A static field may not override a non-static parent method: " << field);
                    }
                    else if (this->virtualFields.has_field(field->get_unique_name())) {
                        if (! (field->get_decl_flags() & TXD_OVERRIDE))
                            CWARNING(this, "Field overrides but isn't declared 'override': " << field);
                        if (! (field->get_type()->is_a(*this->virtualFields.get_field(field->get_unique_name())->get_type())))
                            CERROR(this, "Overriding member's type does not derive from overridden member's type: " << field->get_type());
                        this->virtualFields.override_field(field->get_unique_name(), field);
                    }
                    else {
                        if (field->get_decl_flags() & TXD_OVERRIDE)
                            LOGGER().warning("Field doesn't override but is declared 'override': %s", field->to_string().c_str());
                        this->virtualFields.add_field(field->get_unique_name(), field);
                    }
                }
                else {
                    ASSERT(field->get_storage() == TXS_STATIC, "Invalid storage class " << field->get_storage() << " for field member " << *field);
                    if (field->get_decl_flags() & TXD_OVERRIDE)
                        LOGGER().warning("Field doesn't override but is declared 'override': %s", field->to_string().c_str());
                    this->staticFields.add_field(field->get_unique_name(), field);
                }
            }
        }
    }

    this->initialized = true;
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

void TxType::self_string(std::stringstream& str, bool brief) const {
    if (this->is_modifiable()) {
        str << "MOD ";
    }
    TxEntitySymbol* symbol = nullptr;
    if (auto explDecl = this->get_explicit_declaration())
        symbol = explDecl->get_symbol();
    if (brief && symbol) {
        str << symbol->get_full_name();
        if (this->is_generic())
            str << this->type_params_string();
    }
    else if (this->has_base_type()) {
        bool separator = false;
        if (symbol) {
            str << symbol->get_full_name();
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
        this->baseTypeSpec.type->self_string(str, true);  // set to false to print entire type chain
    }
    else if (symbol)
        str << symbol->get_full_name();
    else
        str << typeid(*this).name();
}



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

const TxType* TxReferenceType::target_type() const {
    // special resolution implementation for the tx#Ref#T member - this forces resolution of the reference target type
    const std::string targetMemName = "tx#Ref#T";
    for ( const TxType* type = this; type; type = type->get_base_type() ) {
        if (auto decl = type->get_declaration()) {
            if (auto member = decl->get_symbol()->get_member_symbol(targetMemName)) {
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
                    if (auto ttype = typeDecl->get_type_definer()->resolve_type(resCtx)) {
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
        LOGGER().debug("Resolved target type of (unnamed) reference type to %s", ttype->to_string().c_str());
    }
    else
        CERROR(this, "Failed to resolve target type for (unnamed) reference type " << this);
    return ttype;
}
