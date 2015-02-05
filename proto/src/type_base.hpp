#pragma once

#include <memory>
#include <map>
#include <vector>
#include <algorithm>
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


/** Type proxy wrapper that provides a non-modifiable 'view' of the underlying type
 * (which may be either modifiable or not).
 */
class TxNonModTypeProxy : public TxTypeProxy {
    TxTypeProxy const * const wrappedProxy;
public:
    TxNonModTypeProxy(TxTypeProxy const * wrappedProxy) : wrappedProxy(wrappedProxy)  { }

    virtual const TxType* get_type() const;
};


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
    TxTypeProxy const * baseTypeDefiner;

public:
    TxTypeParam() : metaType(), typeParamName(), baseTypeDefiner()  { }

    TxTypeParam(MetaType metaType, const std::string& typeParamName, const TxTypeProxy* baseTypeDefiner)
            : metaType(metaType), typeParamName(typeParamName), baseTypeDefiner(baseTypeDefiner)  {
        ASSERT(metaType==TXB_TYPE || baseTypeDefiner, "VALUE type parameter's type is NULL");
    }

    inline MetaType meta_type() const { return metaType; }
    inline const std::string& param_name() const { return typeParamName; }

    inline bool has_base_type_definer() const { return this->baseTypeDefiner; }
    /** Gets the TxType instance that represents the base type constraint (if TYPE) or data type (if VALUE) of this parameter. */
    inline const TxTypeProxy* get_base_type_definer() const {
        ASSERT(this->has_base_type_definer(), "This type parameter '" << this->typeParamName << "' has no base type definer set");
        return this->baseTypeDefiner;
    }

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
        : typeParamName(typeParamName), metaType(redeclParam.meta_type()),
          typeProxy(), valueProxy(), redeclParam(redeclParam)  { }

    inline const std::string& param_name()    const { return typeParamName; }

    inline TxTypeParam::MetaType meta_type()    const { return metaType; }

    inline bool is_redeclared() const { return !this->typeProxy && !this->valueProxy; }

//    inline const TxTypeParam& rebound_param() const {
//        ASSERT(is_redeclared(), "Type parameter binding is not rebound: " << this->to_string());
//        return this->redeclParam;
//    }

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

    inline bool operator!=(const TxTypeBinding& other) const {
        return ! this->operator==(other);
    }

    inline bool operator<(const TxTypeBinding& other) const {
        return this->typeParamName < other.typeParamName;
    }

    std::string to_string() const;
};


/**
 * Use cases:
 * 1. Modifiable - modifiable is true, pass-through bindings
 * 2. Empty extension - modifiable is false, pass-through bindings
 * 3. Type parameter specialization - modifiable is false, one or more bindings are set
 *
 * 2 is used for type aliases, and for derived types that don't bind type parameters.
 * 3 is used for specializing generic types, including derived types (that extend with additional members).
 */
class TxTypeSpecialization : public Printable {
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

    TxTypeSpecialization(const TxType* baseType, const std::vector<TxTypeBinding>& baseBindings)
            : type(baseType), modifiable(false), bindings(baseBindings)  {
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

    std::string to_string() const;
};



/** An instance of this class represents a type definition.
 */
class TxType : public TxTypeProxy, public Printable {
    /** The entity declaration that defined this type. */
    TxTypeEntity const * const _entity;

    /** Type parameters of this type. Should not be accessed directly, use type_params() accessor instead. */
    const std::vector<TxTypeParam> typeParams;

protected:
    const TxTypeSpecialization baseTypeSpec;  // including bindings for all type parameters of base type
    const std::vector<TxTypeSpecialization> interfaces;  // FUTURE

    /** Only to be used for Any type. */
    TxType(const TxTypeEntity* entity) : _entity(entity), typeParams(), baseTypeSpec()  { }

    TxType(const TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
           const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>())
            : _entity(entity), typeParams(typeParams), baseTypeSpec(baseTypeSpec) {
    }

    /** Creates a specialization of this type. To be used by the type registry. */
    virtual TxType* make_specialized_type(const TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,  // (contains redundant ref to this obj...)
                                          const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>(),
                                          std::string* errorMsg=nullptr) const = 0;

    /** Returns true if this type can implicitly convert from the provided type.
     * Internal, base-type-specific implementation. */
    virtual bool innerAutoConvertsFrom(const TxType& someType) const { return false; }

    friend class TypeRegistry;  // allows access for registry's type construction

public:
    virtual ~TxType() = default;


    /** Returns empty string if this type definition is not valid. */
    virtual std::string validate() const;


    /** Returns self. */
    const TxType* get_type() const { return this; }


    const TxTypeSpecialization& get_base_type_spec() const { return this->baseTypeSpec; }


    /*--- type parameter handling ---*/

    /** Gets the type parameters of this type (this type is a generic type if this is non-empty). */
    const std::vector<TxTypeParam>& type_params() const {
        if (this->typeParams.empty()) {
            // if this is an 'empty' or 'modifiable' type usage, pass-through the parameters of the base type
            if (this->baseTypeSpec.type && this->baseTypeSpec.bindings.empty())
                return this->baseTypeSpec.type->type_params();
        }
        return this->typeParams;
    }

    bool has_type_param(const std::string& typeParamName) const {
        for (auto & p : this->type_params())
            if (p.param_name() == typeParamName)
                return true;
        return false;
    }

    const TxTypeParam& get_type_param(const std::string& typeParamName) const {
        for (auto & p : this->type_params())
            if (p.param_name() == typeParamName)
                return p;
        throw std::out_of_range("No such unbound type parameter in " + this->to_string() + ": " + typeParamName);
    }


    /*--- characteristics ---*/

    inline const TxTypeEntity* entity() const { return this->_entity; }

    const TxTypeEntity* explicit_entity() const;


    /** Returns true iff this type is a built-in type. */
    bool is_builtin() const;

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
        return this->baseTypeSpec.modifiable;
    }

    /** Returns true if this type cannot be extended. */
    virtual bool is_final() const { return false; }

    /** Returns true if this type is declared abstract. */
    virtual bool is_abstract() const { return true; }

    /** Returns true if this type is concrete (i.e. can be directly instanced).
     * A concrete type is not abstract, nor usually generic (references may be concrete while generic). */
    bool is_concrete() const { return !this->is_abstract() && !this->is_generic(); }

    /** Returns true if this type is generic (i.e. has unbound type parameters). */
    bool is_generic() const { return !this->type_params().empty(); }

    /** Returns true if this type is a pure specialization of a base type,
     * i.e. does not extend the base type with any definitions, or interfaces,
     * besides type parameter bindings and the modifiable attribute.
     * Such a type is a *direct usage form* of the base type.
     *
     * Note that a pure specialization may still be non-concrete (generic and/or abstract),
     * and that it isn't necessarily the same data type as the base type.
     *
     * Technically, pure specialization types are created when only specializing a base type
     * with type parameter bindings and/or the modifiable attribute.
     *
     * We currently don't regard adding static members, or defining previously abstract methods,
     * as making a specialization impure.
     */
    bool is_pure_specialization() const;

    /** Returns true if this type is an empty specialization of a base type,
     * i.e. does not specialize any type parameters of the base type, nor modifiable,
     * not extends the base type with any definitions, or interfaces.
     * This implies that this type is equivalent to its base type - is the same data type.
     * (An empty specialization is by implication a pure specialization.)
     */
    bool is_empty_specialization() const;

    /** Returns true if this type is a virtual specialization of a base type,
     * i.e. is effectively the same data type as the base type.
     * Modifiability, added interfaces and added static members do not affect the data type;
     * added instance members and bound type parameters do.
     */
    bool is_virtual_specialization() const;



    /*--- inherited namespace lookup ---*/

    /** match against this entity's direct instance/static members, and then its inherited members, returning the first found */
    virtual const TxSymbolScope* lookup_instance_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const;

    /** match against this entity's inherited instance/static members (i.e. skipping this type's direct members) */
    virtual const TxSymbolScope* lookup_inherited_instance_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const;

    /** match against this entity's direct (and static) members, and then its inherited members, returning the first found */
    virtual const TxSymbolScope* lookup_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const;

    /** match against this entity's inherited (and static) members (i.e. skipping this type's direct members) */
    virtual const TxSymbolScope* lookup_inherited_member(std::vector<const TxSymbolScope*>& path, const TxIdentifier& ident) const;


    // TODO: rework this; merge with namespace lookup?
private:
    const TxTypeBinding* resolve_param_binding(// const TxType* specializationType,  // parent entity context
                                               const std::string& paramName) const {
        // note: does not check for transitive modifiability
        if (this->has_type_param(paramName))
            return nullptr;  // type parameter is unbound
        else if (this->baseTypeSpec.type) {
            if (this->baseTypeSpec.has_binding(paramName))
                return &this->baseTypeSpec.get_binding(paramName);
            return this->baseTypeSpec.type->resolve_param_binding(paramName);
        }
        else
            return nullptr;  // no such type parameter name in type specialization hierarchy
    }

public:
    const TxTypeProxy* resolve_param_type(// const TxType* specializationType,  // parent entity context
                                          const std::string& paramName, bool nontransitiveModifiability=false) const {
        if (auto binding = this->resolve_param_binding(paramName)) {
            if (binding->meta_type() == TxTypeParam::MetaType::TXB_TYPE) {
                if (! this->is_modifiable() && ! nontransitiveModifiability)
                    // non-modifiability transitively applies to TYPE type parameters (NOTE: except for references)
                    return new TxNonModTypeProxy(&binding->type_proxy());  // FUTURE: memoize this (also prevents mem leak)
                else
                    return &binding->type_proxy();
            }
        }
        return nullptr;  // no such type parameter name in type specialization hierarchy
    }

    const TxConstantProxy* resolve_param_value(// const TxType* specializationType,  // parent entity context
                                               const std::string& paramName) const {
        if (auto binding = this->resolve_param_binding(paramName)) {
            if (binding->meta_type() == TxTypeParam::MetaType::TXB_VALUE)
                return &binding->value_proxy();
        }
        return nullptr;  // no such type parameter name in type specialization hierarchy
    }



    // FUTURE: checksum?

    // FUTURE: Should we remove the == != operator overloads in favor of more specificly named comparison methods?

    /** Returns true iff the two types are equal in the Tuplex language definition sense.
     * Note that named types are non-equal if not same name. */
    inline virtual bool operator==(const TxType& other) const {
        auto explEnt = this->explicit_entity();
        return explEnt == other.explicit_entity()  // same entity or both null
               && ( explEnt
                    // if unnamed but identical, pure specialization:
                    || ( typeid(*this) == typeid(other)
                         && this->baseTypeSpec == other.baseTypeSpec
                         && this->type_params() == other.type_params() ) );
        // (interfaces and members can only apply to a type with an entity, and an entity can have only one type instance)
    }

    /** Returns true iff the two types are unequal in the Tuplex language definition sense. */
    inline bool operator!=(const TxType& other) const  { return ! this->operator==(other); }


    /** Returns true if this type can implicitly convert from the provided type. */
    bool auto_converts_from(const TxType& other) const {
//        // general logic:
//        if (this->is_modifiable())
//            return this->baseTypeSpec.type->auto_converts_from(other);
//        if (other.is_modifiable())
//            return this->auto_converts_from(*other.baseTypeSpec.type);

        // base-type-specific logic:
        return this->innerAutoConvertsFrom(other);
    }

private:
    bool inner_is_a(const TxType& other) const;

public:
    /** Returns true if the provided type is the same as this, or a specialization of this.
     * Note that true does not guarantee assignability, for example modifiability is not taken into account.
     */
    bool is_a(const TxType& other) const;

    /** Returns the common base type of this and other, if both are pure specializations of it. */
    const TxType* common_generic_base_type(const TxType& other) const {
        if (this->is_pure_specialization())
            return this->baseTypeSpec.type->common_generic_base_type(other);
        if (other.is_pure_specialization())
            return this->common_generic_base_type(*other.baseTypeSpec.type);
        if (*this == other)
            return this;
        return nullptr;
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
        for (auto p : this->type_params()) {
            if (str.length() > 1)  str += ",";
            str += p.to_string();
        }
        str += ">";
        return str;
    }

};
