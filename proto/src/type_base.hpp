#pragma once

#include <typeinfo>
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
class TxExpressionNode;
class ResolutionContext;
class TxTypeDefiner;

/* forward declarations pertaining to LLVM code generation */
class LlvmGenerationContext;
class GenScope;
namespace llvm {
    class Type;
    class StructType;
    class Value;
    class Constant;
    class Function;
}


/** The type classes of Tuplex. Each type class is handled specially by the compiler. */
enum TxTypeClass {
    /** Represents the Any root type. */
    TXTC_ANY,
    /** The built-in, non-aggregate types (e.g. Bool, Scalar). */
    TXTC_ELEMENTARY,
    /** The Ref types. */
    TXTC_REFERENCE,
    /** The Array types. */
    TXTC_ARRAY,
    /** The Tuple types. */
    TXTC_TUPLE,
    /** The Union types. */
    TXTC_UNION,
    /** The function types, including methods and lambdas. */
    TXTC_FUNCTION,
    /** The interface types. */
    TXTC_INTERFACE,
    /** The Range types. */
    TXTC_RANGE,
    /** The Enum types. */
    TXTC_ENUM,
};



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

class TxTypeDefiner : public TxTypeProxy {
public:
    virtual const TxType* resolve_type(ResolutionContext& resCtx) = 0;

    /** Returns true if this type definer "is ready" - has a defined type.
     * If this method returns false, calls to TxTypeProxy::get_type() have undefined results.
     */
    virtual const TxType* attempt_get_type() const = 0;
};

/** Represents a value that can be statically computed (in compile time). */
class TxConstantProxy : public TxTypeProxy {
public:
    virtual ~TxConstantProxy() = default;

    /** Gets the TxType instance representing the type of the constant. */
    virtual const TxType* get_type() const override = 0;

    virtual uint32_t get_value_UInt() const = 0;

    virtual llvm::Constant* code_gen(LlvmGenerationContext& context, GenScope* scope) const = 0;

    virtual bool operator==(const TxConstantProxy& other) const;

    inline virtual bool operator!=(const TxConstantProxy& other) const final {
        return ! this->operator==(other);
    }
};


///** Convenience TxConstantProxy implementation for integers. */
//class TxIntConstant : public TxConstantProxy, public Printable {
//    const long intValue;
//public:
//    TxIntConstant(long intValue) : intValue(intValue)  { }
//    virtual long get_int_value() const { return intValue; }
//    std::string to_string() const { return std::to_string(intValue); }
//};


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
 *
 * The type parameter object belongs to the type object, which may be named (declared)
 * or anonymous. Type declarations must ensure they have named members in their namespace
 * matching the type parameters.
 */
class TxTypeParam : public Printable {  // FUTURE: add constraints
public:
    enum MetaType { TXB_TYPE, TXB_VALUE };

private:
    MetaType metaType;
    std::string typeParamName;
    TxTypeDefiner* baseTypeDefiner;

public:
    TxTypeParam() : metaType(), typeParamName(), baseTypeDefiner()  { }

    TxTypeParam(MetaType metaType, const std::string& typeParamName, TxTypeDefiner* baseTypeDefiner)
            : metaType(metaType), typeParamName(typeParamName), baseTypeDefiner(baseTypeDefiner)  {
        ASSERT(metaType==TXB_TYPE || baseTypeDefiner, "VALUE type parameter's type is NULL");
    }

    inline MetaType meta_type() const { return metaType; }
    inline const std::string& param_name() const { return typeParamName; }

    inline bool has_base_type_definer() const { return this->baseTypeDefiner; }
    /** Gets the TxType instance that represents the base type constraint (if TYPE) or data type (if VALUE) of this parameter. */
    inline TxTypeDefiner* get_base_type_definer() const {
        ASSERT(this->has_base_type_definer(), "This type parameter '" << this->typeParamName << "' has no base type definer set");
        return this->baseTypeDefiner;
    }

    inline virtual bool operator==(const TxTypeParam& other) const {
        return (this->typeParamName == other.typeParamName && this->metaType == other.metaType);
    }
    inline virtual bool operator!=(const TxTypeParam& other) const {
        return ! this->operator==(other);
    }

    std::string to_string() const {
        switch (this->meta_type()) {
        case TXB_TYPE: return "TYPE " + this->param_name();
        case TXB_VALUE: return "VALUE " + this->param_name();
        default: return std::string();
        }
    }
};


/** Describes the binding of a base type's type parameter to the definition used in a type specialization.
 *
 * The type parameter of a reference base type's target may specify dataspace constraints.
 * The binding of a reference base type's target can specify the dataspace of the target.
 */
class TxGenericBinding : public Printable {
    const std::string typeParamName;
    const TxTypeParam::MetaType metaType;
    TxTypeDefiner* typeDefiner;
    TxExpressionNode* valueExpr;

    TxGenericBinding(const std::string& typeParamName, TxTypeParam::MetaType metaType,
                     TxTypeDefiner* typeDefiner, TxExpressionNode* valueExpr)
        : typeParamName(typeParamName), metaType(metaType), typeDefiner(typeDefiner), valueExpr(valueExpr)  { }

public:
    static TxGenericBinding make_type_binding(const std::string& typeParamName, TxTypeDefiner* typeDefiner);
    static TxGenericBinding make_value_binding(const std::string& typeParamName, TxExpressionNode* valueExpr);

//    /** copy constructor */
//    TxGenericBinding(const TxGenericBinding& binding)
//        : TxGenericBinding(typeParamName, metaType, typeDefiner, valueExpr)  { }

    inline const std::string& param_name()    const { return typeParamName; }

    inline TxTypeParam::MetaType meta_type()    const { return metaType; }

    inline TxTypeDefiner& type_definer()  const {
        ASSERT(metaType==TxTypeParam::MetaType::TXB_TYPE, "Type parameter binding metatype is VALUE, not TYPE: " << this->to_string());
        return *this->typeDefiner;
    }

    inline TxExpressionNode& value_expr() const {
        ASSERT(metaType==TxTypeParam::MetaType::TXB_VALUE, "Type parameter binding metatype is TYPE, not VALUE: " << this->to_string());
        return *this->valueExpr;
    }

    std::string to_string() const;
};

bool operator==(const TxGenericBinding& b1, const TxGenericBinding& b2);
inline bool operator!=(const TxGenericBinding& b1, const TxGenericBinding& b2) { return !(b1 == b2); }



/** Describes a specialization of a base type.
 * In a specialization of a generic base type, the base type's type parameters must either have a binding,
 * or be redeclared with the same name and matching constraints in the specialized type
 * (keeping the parameter open for further specialization, with identical or narrowed type parameter
 * constraints).
 *
 * Use cases:
 * 1. Modifiable - modifiable is true, pass-through bindings
 * 2. Empty extension - modifiable is false, pass-through type parameters
 * 3. Type parameter specialization - modifiable is false, one or more bindings are set
 *
 * 2 is used for type aliases, and for derived types that don't bind type parameters.
 * 3 is used for specializing generic types, including derived types (that extend with additional members).
 */
class TxTypeSpecialization : public Printable {
public:
    TxType const * const type;
    const bool modifiable;
    TxIdentifier const * const dataspace;  // only set for reference specializations
    const std::vector<TxGenericBinding> bindings;

    /** Only legal to use by the Any type. */
    TxTypeSpecialization()
            : type(), modifiable(), dataspace(), bindings()  { }

    TxTypeSpecialization(const TxType* baseType, bool modifiable=false)
            : type(baseType), modifiable(modifiable), dataspace(), bindings()  {
        ASSERT(baseType, "NULL baseType");
    }

    TxTypeSpecialization(const TxType* baseType, const std::vector<TxGenericBinding>& baseBindings,
                         const TxIdentifier* dataspace=nullptr)
            : type(baseType), modifiable(false), dataspace(dataspace), bindings(baseBindings)  {
        ASSERT(baseType, "NULL baseType");
    }

    bool has_binding(const std::string& typeParamName) const {
        for (auto & b : this->bindings)
            if (b.param_name() == typeParamName)
                return true;
        return false;
    }

    const TxGenericBinding& get_binding(const std::string& typeParamName) const {
        for (auto & b : this->bindings)
            if (b.param_name() == typeParamName)
                return b;
        throw std::out_of_range("No such bound type parameter: " + typeParamName);
    }

    /** Returns empty string if this specialization is valid for the base type. */
    std::string validate(ResolutionContext& resCtx) const;

    bool operator==(const TxTypeSpecialization& other) const;

    inline bool operator!=(const TxTypeSpecialization& other) const {
        return ! this->operator==(other);
    }

    std::string to_string() const;
};



/** An instance of this class represents a type definition.
 */
class TxType : public TxTypeDefiner, public Printable {
    /** The type id of this type, if it is a statically distinct type (not a pure specialization). */
    uint32_t typeId = UINT32_MAX;

    /** The entity declaration that defined this type. */
    TxTypeEntity * const _entity;

    /** Type parameters of this type. Should not be accessed directly, use type_params() accessor instead. */
    const std::vector<TxTypeParam> typeParams;

    const TxTypeSpecialization baseTypeSpec;  // including bindings for all type parameters of base type
    const std::vector<TxTypeSpecialization> interfaces;  // FUTURE

protected:
    /** Only to be used for Any type. */
    TxType(TxTypeEntity* entity) : _entity(entity), typeParams(), baseTypeSpec()  { }

    TxType(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
           const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>())
            : _entity(entity), typeParams(typeParams), baseTypeSpec(baseTypeSpec) {
    }

    /** Creates a specialization of this type. To be used by the type registry. */
    virtual TxType* make_specialized_type(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,  // (contains redundant ref to this obj...)
                                          const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>(),
                                          std::string* errorMsg=nullptr) const = 0;

    /** Returns true if this type can implicitly convert from the provided type.
     * Internal, base-type-specific implementation. */
    virtual bool innerAutoConvertsFrom(const TxType& someType) const { return false; }

    friend class TypeRegistry;  // allows access for registry's type construction

public:
    virtual ~TxType() = default;


    /** Returns empty string if this type definition is not valid. */
    virtual std::string validate(ResolutionContext& resCtx) const;


    // For now. Implements the TxEntityDefiner interface.
    virtual const TxType* resolve_type(ResolutionContext& resCtx) override { return this; }
    virtual const TxType* attempt_get_type() const override { return this; }
    virtual const TxType* get_type() const override { return this; }



    /*--- characteristics ---*/

    /** Gets the type id of this type. (Pure specializations return their base type's id.) */
    inline uint32_t get_type_id() const { return ( this->typeId == UINT32_MAX ? this->baseTypeSpec.type->get_type_id() : this->typeId ); }

    inline TxTypeEntity* entity() const { return this->_entity; }

    TxTypeEntity* explicit_entity() const;


    /** Returns true if this type has a base type (parent). (Any is the only type that has no base type.) */
    inline bool has_base_type() const { return this->baseTypeSpec.type; }

    /** Returns the type class this type belongs to. */
    virtual TxTypeClass get_type_class() const = 0;

    /** Returns true iff this type is a built-in type. */
    bool is_builtin() const;

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

    /** Returns true if this type extends its base type with additional instance members.
     * (Generic type specialization is regarded as specialization, not extension, and does thus
     * not cause this method to return true.)
     */
    virtual bool is_datatype_extension() const;

    /** Returns true if the size of this type is statically known.
     * May be true for arrays with non-static length initializer or compound types containing such arrays.
     */
    virtual bool is_statically_sized() const;


    /*--- inherited namespace lookup ---*/

    /** match against this entity's direct instance/static members, and then its inherited members, returning the first found */
    virtual TxSymbolScope* lookup_instance_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) const;

    /** match against this entity's inherited instance/static members (i.e. skipping this type's direct members) */
    virtual TxSymbolScope* lookup_inherited_instance_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) const;

    /** match against this entity's direct (and static) members, and then its inherited members, returning the first found */
    virtual TxSymbolScope* lookup_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) const;

    /** match against this entity's inherited (and static) members (i.e. skipping this type's direct members) */
    virtual TxSymbolScope* lookup_inherited_member(std::vector<TxSymbolScope*>& path, const TxIdentifier& ident) const;


    const TxType* get_base_type() const;  // TODO: move to TxTypeSpecialization


    /*--- type parameter handling ---*/

    /** Gets the type parameters of this type (this type is a generic type if this is non-empty). */
    const std::vector<TxTypeParam>& type_params() const {
        if (this->typeParams.empty()) {
            // if this is an 'empty' or 'modifiable' type usage, pass-through the parameters of the base type
            // (shouldn't need generic base type resolution here)
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


    // TODO: rework this; merge with namespace lookup?
private:
    const TxGenericBinding* resolve_param_binding(const std::string& paramName) const {
        // FIXME: handle fully qualified parameter names (or remove these methods in favor of namespace lookup)
        // note: does not check for transitive modifiability
        if (this->has_type_param(paramName))
            return nullptr;  // type parameter is unbound  // FIXME: return constraint base type instead
        else if (auto baseType = this->get_base_type()) {
            if (this->baseTypeSpec.has_binding(paramName))
                return &this->baseTypeSpec.get_binding(paramName);
            return baseType->resolve_param_binding(paramName);
        }
        else
            return nullptr;  // no such type parameter name in type specialization hierarchy
    }

protected:
    const TxType* resolve_param_type(ResolutionContext& resCtx, const std::string& paramName,
                                     bool nontransitiveModifiability=false) const;

    const TxExpressionNode* resolve_param_value(ResolutionContext& resCtx, const std::string& paramName) const;


public:
    // FUTURE: checksum?

    // FUTURE: Should we remove the == != operator overloads in favor of more specifically named comparison methods?

    /** Returns true iff the two types are equal in the Tuplex language definition sense.
     * Note that named types are non-equal if not same name. */
    virtual bool operator==(const TxType& other) const;

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


    /** Returns true if the provided type is the same as this, or a specialization of this.
     * Note that true does not guarantee assignability, for example modifiability is not taken into account.
     */
    bool is_a(const TxType& other) const;

    /** Returns the common base type of this and other, if both are pure specializations of it. */
    const TxType* common_generic_base_type(const TxType& other) const;

//    /** Returns true if an instance of this type can be assigned from an instance of the provided type
//     * (without performing any kind of conversion). */
//    virtual bool is_assignable_from(const TxType& someType) const { return (*this) == someType; }

private:
    bool inner_is_a(const TxType& other) const;


public:
    /*--- LLVM code generation methods ---*/

    virtual llvm::StructType* make_vtable_type(LlvmGenerationContext& context) const;
    virtual llvm::Function* get_type_user_init_func(LlvmGenerationContext& context) const;

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const = 0;
    virtual llvm::Value* gen_size(LlvmGenerationContext& context, GenScope* scope) const;
    virtual llvm::Value* gen_alloca(LlvmGenerationContext& context, GenScope* scope, const std::string &varName="") const;

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }



    /*--- to string methods ---*/

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
