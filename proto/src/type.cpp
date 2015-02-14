#include "logging.hpp"

#include "type.hpp"
#include "ast.hpp"
#include "entity.hpp"


static Logger* LOGGER() {
    static Logger* LOG; // = Logger::get("PARSER");
    if (! LOG)
        LOG = &Logger::get("PARSER");
    return LOG;
}


bool TxConstantProxy::operator==(const TxConstantProxy& other) const {
    // simple since we so far only support UInt values
    return *this->get_type() == *other.get_type()
           && this->get_value_UInt() == other.get_value_UInt();
}


const TxType* TxNonModTypeProxy::get_type() const {
    auto type = wrappedProxy->get_type();
    return type->is_modifiable() ? type->get_base_type() : type;
}


bool TxTypeBinding::operator==(const TxTypeBinding& other) const {
    return ( this->typeParamName == other.typeParamName
             && this->metaType == other.metaType
             && ( this->metaType == TxTypeParam::MetaType::TXB_TYPE
                      ? ( *(this->typeProxy->get_type()) == *(other.typeProxy->get_type()) )
                      : ( this->valueExpr->get_static_constant_proxy() && other.valueExpr->get_static_constant_proxy()
                          && *this->valueExpr->get_static_constant_proxy() == *other.valueExpr->get_static_constant_proxy() ) ) );
}

std::string TxTypeBinding::to_string() const {
    const TxType* type;
    return this->typeParamName + "=" + ( this->metaType==TxTypeParam::MetaType::TXB_TYPE
                                                ? (type = this->typeProxy->get_type(),
                                                   type ? type->to_string(true) : "")
                                                : this->valueExpr->to_string() );
}


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
    }
    else if (! this->bindings.empty()) {
//        // if at least one param is bound, all need to be bound/redeclared
//        if (this->type->type_params().size() != this->bindings.size())
//            return std::string("Specified " + std::to_string(this->bindings.size()) + " bindings but base type has " + std::to_string(this->type->type_params().size()) + " parameters");
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
                        auto boundType = b.type_proxy().get_type();
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


std::string TxType::validate() const {
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
        if (! this->baseTypeSpec.type->entity())
            // also validate any anonymous base types (otherwise their validate() won't be called)
            return this->baseTypeSpec.type->validate();
    }
    // FUTURE: validate interfaces
    // check that generic interfaces can't be implemented multiple times throughout a type hierarchy,
    // unless their type arguments are exactly the same
    return std::string();
}


TxTypeEntity* TxType::explicit_entity() const {
    if (this->_entity && !(this->_entity->get_decl_flags() & TXD_IMPLICIT))
        return this->_entity;
    return nullptr;
}

bool TxType::is_builtin() const {
    return this->_entity && (this->_entity->get_decl_flags() & TXD_BUILTIN);
}

bool TxType::is_pure_specialization() const {
    return ( this->is_modifiable()
             || ( this->has_base_type() && this->interfaces.empty()
                  && !this->is_builtin()  // this being built-in implies that it is more concrete than base class
                  && typeid(*this) == typeid(*this->baseTypeSpec.type)
                  && !( this->_entity && this->_entity->has_instance_fields() ) ) );
}

bool TxType::is_empty_specialization() const {
    return ( this->has_base_type() && this->interfaces.empty()
             && !this->is_builtin()  // this being built-in implies that it is more concrete than base class
             && typeid(*this) == typeid(*this->baseTypeSpec.type)
             && this->baseTypeSpec.bindings.empty()
             && !this->baseTypeSpec.modifiable
             && !( this->_entity && this->_entity->has_instance_fields() ) );
}

bool TxType::is_virtual_specialization() const {
    return ( this->is_modifiable()
             || ( this->has_base_type()
                  && !this->is_builtin()  // this being built-in implies that it is more concrete than base class
                  && typeid(*this) == typeid(*this->baseTypeSpec.type)
                  && this->baseTypeSpec.bindings.empty()
                  && !( this->_entity && this->_entity->has_instance_fields() ) ) );
}


const TxType* TxType::get_base_type() const {
    if (! has_base_type())
        return nullptr;
    return this->baseTypeSpec.type;
}


TxSymbolScope* TxType::lookup_instance_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) const {
    if (this->_entity)
        return this->_entity->lookup_instance_member(path, ident);
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
    if (this->_entity)
        return this->_entity->lookup_member(path, ident);
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


const TxTypeProxy* TxType::resolve_param_type(const std::string& paramName, bool nontransitiveModifiability) const {
    if (this->entity()) {
        std::vector<TxSymbolScope*> path( { this->entity() } );
        if (auto typeEntity = dynamic_cast<TxTypeEntity*>(this->lookup_member(path, paramName))) {
            if (! this->is_modifiable() && ! nontransitiveModifiability)
                // non-modifiability transitively applies to TYPE type parameters (NOTE: except for references)
                return new TxNonModTypeProxy(typeEntity);  // FUTURE: memoize this (also prevents mem leak)
            else
                return typeEntity;
        }
    }
    else {
        LOGGER()->warning("Attempting to resolve type parameter '%s' of type that has no entity: %s", paramName.c_str(), this->to_string().c_str());
        auto binding = this->resolve_param_binding(paramName);
        if (! binding) {
            size_t pos = paramName.find_last_of('#');
            if (pos != std::string::npos) {
                auto unqualName = paramName.substr(pos+1);
                //LOGGER()->warning("Attempting to resolve type parameter '%s' of type that has no entity: %s", unqualName.c_str(), this->to_string().c_str());
                binding = this->resolve_param_binding(unqualName);
            }
        }
        if (binding && binding->meta_type() == TxTypeParam::MetaType::TXB_TYPE) {
            if (! this->is_modifiable() && ! nontransitiveModifiability)
                // non-modifiability transitively applies to TYPE type parameters (NOTE: except for references)
                return new TxNonModTypeProxy(&binding->type_proxy());  // FUTURE: memoize this (also prevents mem leak)
            else
                return &binding->type_proxy();
        }
    }
    return nullptr;  // no such type parameter name in type specialization hierarchy
}

const TxExpressionNode* TxType::resolve_param_value(const std::string& paramName) const {
    if (auto binding = this->resolve_param_binding(paramName)) {
        if (binding->meta_type() == TxTypeParam::MetaType::TXB_VALUE)
            return &binding->value_expr();
    }
    return nullptr;  // no such type parameter name in type specialization hierarchy
}



bool TxType::is_a(const TxType& other) const {
    //std::cout << *this << "  IS-A\n" << other << std::endl;
    if (this->is_modifiable())
        return this->get_base_type()->is_a(other);
    if (other.is_modifiable())
        return this->is_a(*other.get_base_type());
    if (*this == other)
        return true;
    if (other.is_empty_specialization())
        return this->is_a(*other.get_base_type());
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


static void type_bindings_string(std::stringstream& str, const TxTypeSpecialization& specialization) {
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
    auto entity = this->explicit_entity();
    if (brief && entity) {
        str << entity->get_full_name();
        if (this->is_generic())
            str << this->type_params_string();
    }
    else if (this->has_base_type()) {
        bool separator = false;
        if (entity) {
            str << entity->get_full_name();
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
    else if (entity)
        str << entity->get_full_name();
    else
        str << typeid(*this).name();
}



bool TxArrayType::innerAutoConvertsFrom(const TxType& otherType) const {
    if (const TxArrayType* otherArray = dynamic_cast<const TxArrayType*>(&otherType)) {
        // if other has unbound type params that this does not, other is more generic and can't be auto-converted to this
        if (auto e = this->element_type()) {
            if (auto otherE = otherArray->element_type()) {
                // note: is-a test insufficient for array elements, since same concrete type (same size) required
                if (*e->get_type() != *otherE->get_type())
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
                if (! otherTarget->get_type()->is_a(*target->get_type()))
                    return false;
                else if (target->get_type()->is_modifiable() && !otherTarget->get_type()->is_modifiable())
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
