#pragma once

#include <memory>
#include <map>
#include <vector>
#include <cstdlib>

#include "txassert.hpp"
#include "printable.hpp"

#include "identifier.hpp"

#include "type_visitor.hpp"


class TxType;
class TxSymbolScope;
class TxTypeEntity;

/** Proxy interface that provides a layer of indirection to a type reference.
 * This is needed for two reasons:
 * - Before the symbol table pass has completed, resolving the actual type may not be possible
 * - Resolving the type may be context-dependent (e.g. type parameter resolution depends
 *   on subtype context)
 */
class TxTypeProxy {
public:
    virtual ~TxTypeProxy() = default;

    /** Gets the TxType instance this type proxy represents.
     * The contract is that it shall return the same instance every invocation.
     */
    virtual const TxType* get_type() const = 0;
};

class TxConstantProxy {
public:
    virtual ~TxConstantProxy() = default;

    virtual long get_int_value() const = 0;
};


/** Convenience TxConstantProxy implementation for integers. */
class TxIntConstant : public TxConstantProxy, public Printable {
    const long intValue;
public:
    TxIntConstant(long intValue) : intValue(intValue)  { }
    virtual long get_int_value() const { return intValue; }
    std::string to_string() const { return std::to_string(intValue); }
};


///** Represents a "usage variant" of a type.
// * The same type can be used as modifiable or not (unless it is immutable).
// * This is light-weight and intended to be passed by value.
// * It can simply be wrapped around types:
// * typeRegistry->get_array(TxTypeUsage(charType))
// *
// */
//class TxTypeUsage : public TxTypeProxy, public Printable {
//public:
//    const TxTypeProxy* type_proxy;
//    const bool modifiable;
//
//    TxTypeUsage() : type_proxy(), modifiable() { }
//    TxTypeUsage(const TxTypeProxy* type, bool modifiable=false) : type_proxy(type), modifiable(modifiable)  { }
//    //TxTypeUsage(const TxTypeUsage& other) : type(other.type), modifiable(other.modifiable)  { }
//
//    const TxType* get_type() const { return this->type_proxy->get_type(); }
//
//    inline virtual bool operator==(const TxTypeUsage& other) const {
//        return ( this->modifiable == other.modifiable
//                 && this->type_proxy->get_type() == other.type_proxy->get_type() );
//    }
//    inline virtual bool operator!=(const TxTypeUsage& other) const {
//        return ! this->operator==(other);
//    }
//
//    std::string to_string() const;
//};



/** Represents a type parameter of a generic type.
 * Specializations of the generic type provide a binding to this parameter.
 * The parameter may hold constraints on what definitions are permitted to bind to it.
 * The type parameter of a reference base type's target may specify dataspace constraints.
 */
class TxTypeParam : public Printable {  // FUTURE: add constraints
public:
    enum MetaType { TXB_TYPE, TXB_VALUE };

private:
    MetaType metaType;
    std::string typeParamName;

public:
    TxTypeParam() : metaType(), typeParamName()  { }

    TxTypeParam(MetaType metaType, const std::string& typeParamName)
        : metaType(metaType), typeParamName(typeParamName)  { }

    inline MetaType meta_type() const { return metaType; }
    inline const std::string& param_name() const { return typeParamName; }

    inline virtual bool operator==(const TxTypeParam& other) const {
        return (this->typeParamName == other.typeParamName && this->metaType == other.metaType);
    }
    inline virtual bool operator!=(const TxTypeParam& other) const {
        return ! this->operator==(other);
    }
    inline virtual bool operator<(const TxTypeParam& other) const {
        return this->typeParamName < other.typeParamName;
    }

    std::string to_string() const {
        switch (this->meta_type()) {
        case TXB_TYPE: return "TYPE " + this->param_name();
        case TXB_VALUE: return "VALUE " + this->param_name();
        }
    }
};


/** Represents the binding of a base type's type parameter to the definition used in a type specialization.
 * All type parameters of a base type must have a binding in a specialization of the base type.
 * A binding may however be redeclared - bound to a type parameter of the specialized type -
 * in effect keeping the parameter open for further specialization
 * (with identical or narrowed type parameter constraints).
 *
 * The type parameter of a reference base type's target may specifiy dataspace constraints.
 * The binding of a reference base type's target can specify the dataspace of the target..
 */
class TxTypeBinding : public Printable {
    const std::string typeParamName;
    const TxTypeParam::MetaType metaType;
    const TxTypeProxy* typeProxy;
    const TxConstantProxy* valueProxy;
    const TxTypeParam redeclParam;  // if this type parameter is redeclared as a type parameter of the specialized type

public:
    TxTypeBinding(const std::string& typeParamName, const TxTypeProxy* typeProxy)
        : typeParamName(typeParamName), metaType(TxTypeParam::MetaType::TXB_TYPE),
          typeProxy(typeProxy), valueProxy(), redeclParam()  { }
    TxTypeBinding(const std::string& typeParamName, const TxConstantProxy* valueProxy)
        : typeParamName(typeParamName), metaType(TxTypeParam::MetaType::TXB_VALUE),
          typeProxy(), valueProxy(valueProxy), redeclParam()  { }
    TxTypeBinding(const std::string& typeParamName, const TxTypeParam& redeclParam)
        : typeParamName(typeParamName), metaType(TxTypeParam::MetaType::TXB_VALUE),
          typeProxy(), valueProxy(), redeclParam(redeclParam)  { }

    inline const std::string& param_name()    const { return typeParamName; }

    inline bool is_redeclared() const { return !this->typeProxy && !this->valueProxy; }

    inline const TxTypeParam& rebound_param() const {
        ASSERT(is_redeclared(), "Type parameter binding is not rebound: " << this->to_string());
        return this->redeclParam;
    }

    inline const TxTypeProxy& type_proxy()  const {
        ASSERT(metaType==TxTypeParam::MetaType::TXB_TYPE, "Type parameter binding metatype is VALUE, not TYPE: " << this->to_string());
        ASSERT(!is_redeclared(), "Type parameter binding is rebound: " << this->to_string());
        return *this->typeProxy;
    }

    inline const TxConstantProxy& value_proxy() const {
        ASSERT(metaType==TxTypeParam::MetaType::TXB_VALUE, "Type parameter binding metatype is TYPE, not VALUE: " << this->to_string());
        ASSERT(!is_redeclared(), "Type parameter binding is rebound: " << this->to_string());
        return *this->valueProxy;
    }

    bool operator==(const TxTypeBinding& other) const;
    inline virtual bool operator!=(const TxTypeBinding& other) const {
        return ! this->operator==(other);
    }
    inline virtual bool operator<(const TxTypeBinding& other) const {
        return this->typeParamName < other.typeParamName;
    }

    std::string to_string() const;
};


class TxTypeSpecialization {
public:
    TxType const * const type;
    const bool modifiable;
    const std::vector<TxTypeBinding> bindings;  // each type parameter of type must have binding

    /** Only legal to use by the Any type. */
    TxTypeSpecialization()
            : type(), modifiable(), bindings()  { }

    TxTypeSpecialization(const TxType* baseType, bool modifiable=false)
            : type(baseType), modifiable(modifiable), bindings()  {
        ASSERT(baseType, "NULL baseType");
        //ASSERT(!validate(), "Invalid specialization of " << baseType);
    }

    TxTypeSpecialization(const TxType* baseType, const std::vector<TxTypeBinding>& baseBindings, bool modifiable=false)
            : type(baseType), modifiable(modifiable), bindings(baseBindings)  {
        ASSERT(baseType, "NULL baseType");
        //ASSERT(!validate(), "Invalid specialization of " << baseType);
    }

    bool has_binding(const std::string& typeParamName) const {
        for (auto & b : this->bindings)
            if (b.param_name() == typeParamName)
                return true;
        return false;
    }

    const TxTypeBinding& get_binding(const std::string& typeParamName) const {
        for (auto & b : this->bindings)
            if (b.param_name() == typeParamName)
                return b;
        throw std::out_of_range("No such bound type parameter: " + typeParamName);
    }

    /** Returns empty string if this specialization is valid for the base type. */
    std::string validate() const;

    bool operator==(const TxTypeSpecialization& other) const;
};



/** An instance of this class represents a type definition.
 */
class TxType : public TxTypeProxy, public Printable {
    //std::string _name;
    /** The entity declaration that defined this type. */
    TxTypeEntity const * const _entity;

protected:
    const TxTypeSpecialization baseTypeSpec;  // including bindings for all type parameters of base type
    const std::vector<TxTypeSpecialization> interfaces;  // TODO

    /** Only to be used for Any type. */
    TxType(const TxTypeEntity* entity) : _entity(entity), baseTypeSpec(), typeParams()  { }

    TxType(const TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
           const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>())
            : _entity(entity), baseTypeSpec(baseTypeSpec), typeParams(typeParams) {
        ASSERT(!entity || !this->is_pure_modifiable(), "Can't set entity " << entity << " of pure modifiable type " << this);
        auto res = baseTypeSpec.validate();
        if (! res.empty())
            throw std::logic_error("Invalid specialization for base type " + baseTypeSpec.type->to_string() + ": " + res);
    }

    /** Creates a pure specialization of this type. To be used by the type registry. */
    virtual TxType* make_specialized_type(const TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,  // (contains redundant ref to this obj...)
                                          const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>(),
                                          std::string* errorMsg=nullptr) const = 0;

    /** Returns true if this type can implicitly convert from the provided type.
     * Internal, base-type-specific implementation. */
    virtual bool innerAutoConvertsFrom(const TxType& someType) const { return false; }

    friend class TypeRegistry;  // allows access for registry's type construction

public:
    /** Type parameters of this type (this type is a generic type if this is non-empty). */
    const std::vector<TxTypeParam> typeParams;


    virtual ~TxType() = default;


    /** Returns self. */
    const TxType* get_type() const { return this; }


    const TxTypeSpecialization& get_base_type_spec() const { return this->baseTypeSpec; }


    /*--- type parameter handling ---*/

    bool has_type_param(const std::string& typeParamName) const {
        for (auto & p : this->typeParams)
            if (p.param_name() == typeParamName)
                return true;
        return false;
    }

    const TxTypeParam& get_type_param(const std::string& typeParamName) const {
        for (auto & p : this->typeParams)
            if (p.param_name() == typeParamName)
                return p;
        throw std::out_of_range("No such unbound type parameter in " + this->to_string() + ": " + typeParamName);
    }


    /*--- characteristics ---*/

    inline const TxTypeEntity* entity() const { return this->_entity; }


    /** Returns the size, in bytes, of a direct instance of this type.
     * Illegal to call for abstract types. */
    virtual long size() const = 0;

    /** Returns true if this type is immutable (its instances' contents can never be modified after initialization). */
    virtual bool is_immutable() const { return false; }

    /** Returns true if this type is modifiable (its instances' contents may be modified after initialization).
     * This can only be the case for pure specializations:
     * Declaring a "modifiable" type in source obtains a pure specialization of the sought base type
     * with the modifiable flag set.
     * (The base type must of course be mutable for such a specialization to be legal.)
     * A corollary is that a non-pure-specialization type is never immediately modifiable,
     * it must first be specialized as such.
     */
    bool is_modifiable() const {
        return (this->is_pure_specialization() && this->baseTypeSpec.modifiable);
    }

    /** Returns true if this type cannot be extended. */
    virtual bool is_final() const { return false; }

    /** Returns true if this type is declared abstract. */
    virtual bool is_abstract() const { return true; }

    /** Returns true if this type is concrete (i.e. can be directly instanced).
     * A concrete type is not abstract, nor usually generic (references may be concrete while generic). */
    bool is_concrete() const { return !this->is_abstract() && !this->is_generic(); }

    /** Returns true if this type is generic (i.e. has unbound type parameters). */
    bool is_generic() const { return !this->typeParams.empty(); }

    /** Returns true if this type is a pure specialization of a base type,
     * i.e. does not extend the base type with any definitions, or interfaces,
     * besides type parameter bindings and the modifiable attribute.
     * Such a type is a *direct usage form* of the base type.
     *
     * Note that a pure specialization may still be non-concrete (generic and/or abstract).
     *
     * Technically, pure specialization types are created when only specializing a base type
     * with type parameter bindings and/or the modifiable attribute.
     */
    bool is_pure_specialization() const {
        return (this->baseTypeSpec.type && this->interfaces.empty()
                && typeid(*this) == typeid(*this->baseTypeSpec.type));
        // FUTURE: check if this extends base with new members
    }

    /** Returns true if this type is a pure specialization of a base type that *only*
     * specifies the modifiable attribute. If true, this type is by implication modifiable.
     */
    bool is_pure_modifiable() const {
        return (this->is_pure_specialization() && this->baseTypeSpec.modifiable);
        // FIXME: add check that this specialization does not bind/narrow any base type parameters
    }


    /*--- inherited namespace lookup ---*/

    /** match against this entity's direct instance/static members, and then its inherited members, returning the first found */
    virtual const TxSymbolScope* lookup_instance_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const;

    /** match against this entity's inherited instance/static members (i.e. skipping this type's direct members) */
    virtual const TxSymbolScope* lookup_inherited_instance_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const;

    /** match against this entity's direct (and static) members, and then its inherited members, returning the first found */
    virtual const TxSymbolScope* lookup_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const;

    /** match against this entity's inherited (and static) members (i.e. skipping this type's direct members) */
    virtual const TxSymbolScope* lookup_inherited_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const;


    // TODO: rework this; merge with namespace lookup

    const TxTypeProxy* resolve_param_type(// const TxType* specializationType,  // parent entity context
                                          const std::string& paramName) const {
        if (this->has_type_param(paramName))
            return nullptr;  // type parameter is unbound
        else if (this->baseTypeSpec.type) {
            if (this->baseTypeSpec.has_binding(paramName))
                return &this->baseTypeSpec.get_binding(paramName).type_proxy();
            return this->baseTypeSpec.type->resolve_param_type(paramName);
        }
        else
            return nullptr;  // no such type parameter name in type specialization hierarchy
    }

    const TxConstantProxy* resolve_param_value(// const TxType* specializationType,  // parent entity context
                                               const std::string& paramName) const {
        if (this->has_type_param(paramName))
            return nullptr;  // type parameter is unbound
        else if (this->baseTypeSpec.type) {
            if (this->baseTypeSpec.has_binding(paramName))
                return &this->baseTypeSpec.get_binding(paramName).value_proxy();
            return this->baseTypeSpec.type->resolve_param_value(paramName);
        }
        else
            return nullptr;  // no such type parameter name in type specialization hierarchy
    }



    // FUTURE: checksum?

    // FUTURE: Should we remove the == != operator overloads in favor of more specific comparison methods?
    // Note that named types are non-equal if not same name.
    inline virtual bool operator==(const TxType& other) const {
        return typeid(*this) == typeid(other)
               && this->baseTypeSpec == other.baseTypeSpec
               && this->typeParams == other.typeParams;
            // TODO: same interfaces; same members
    }
    inline bool operator!=(const TxType& other) const  { return ! this->operator==(other); }

    /** Returns true if this type can implicitly convert from the provided type. */
    bool autoConvertsFrom(const TxType& other) const {
        // general logic:
        if (this->is_pure_modifiable())
            return this->baseTypeSpec.type->autoConvertsFrom(other);
        if (other.is_pure_modifiable())
            return this->autoConvertsFrom(*other.baseTypeSpec.type);

        // base-type-specific logic:
        return this->innerAutoConvertsFrom(other);
    }

    /** Returns true if the provided type is the same as this, or a specialization of this.
     * Note that true does not guarantee assignability, for example modifiability is not taken into account.
     */
    virtual bool is_a(const TxType& other) const {
        //std::cout << *this << "  IS-A\n" << other << std::endl;
        if (this->is_pure_modifiable())
            return this->baseTypeSpec.type->is_a(other);
        if (other.is_pure_modifiable())
            return this->is_a(*other.baseTypeSpec.type);

        if (*this == other)
            return true;
        // check whether other is a more generic version of the same type:
        if (this->is_same_generic_base_type(other)) {
            // FiXME: check: are mod and params collapsed into single specialization step???
            for (auto & b : this->baseTypeSpec.bindings) {
                // other's param shall either be redeclared (generic) or *equal* to this (is-a is not sufficient in general case)
                auto other_b = other.get_base_type_spec().get_binding(b.param_name());
                if (! other_b.is_redeclared())
                    if (b != other_b)  // checks whether both bindings resolve to same type/value  // TODO: review
                        return false;
            }
            return true;
        }
        // check whether any parent type that this type specializes is-a of the other type:
        if (this->baseTypeSpec.type)
            if (this->baseTypeSpec.type->is_a(other))
                return true;
        // FUTURE: also check interfaces
        return false;
    }

    bool is_same_generic_base_type(const TxType& other) const {
        if (this->is_pure_specialization())
            return this->baseTypeSpec.type->is_same_generic_base_type(other);
        if (other.is_pure_specialization())
            return this->is_same_generic_base_type(*other.baseTypeSpec.type);
        return *this == other;
    }

    /** Returns true if an instance of this type can be assigned from an instance of the provided type
     * (without performing any kind of conversion). */
    virtual bool is_assignable_from(const TxType& someType) const { return (*this) == someType; }


    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }


    virtual std::string to_string() const {
        return this->to_string(false);
    }
    virtual std::string to_string(bool brief) const {
        std::stringstream str;
        if (this->is_abstract())
            str << "ABSTRACT ";
        if (this->is_immutable())
            str << "IMMUTABLE ";
        this->self_string(str, brief);
        return str.str();
    }


protected:
    void self_string(std::stringstream& str, bool brief=false) const;

    std::string type_params_string() const {
        std::string str = "<";
        for (auto p : this->typeParams) {
            if (str.length() > 1)  str += ",";
            str += p.to_string();
        }
        str += ">";
        return str;
    }

};
