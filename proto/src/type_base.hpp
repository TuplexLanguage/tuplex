#pragma once

#include <typeinfo>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <cstdlib>

#include "assert.hpp"
#include "printable.hpp"

#include "identifier.hpp"

#include "type_visitor.hpp"
#include "generics.hpp"

#include "entity.hpp"


class TxType;
class ResolutionContext;

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



class DataTupleDefinition {
public:
    // we need to be able to look up fields both via index and plain name:
    /** map from plain name to field index (note, contains fewer entries than fields vector when parent field name is hidden) */
    std::unordered_map<std::string, uint32_t> fieldMap;
    /** the fields */
    std::vector<const TxField*> fields;

    /** Adds a field to this tuple. If the field name already exists, it will be shadowed by the new field. */
    void add_field(const std::string& name, const TxField* field) {
        //std::cout << "Adding field " << name << " at index " << this->fields.size() << ": " << field << std::endl;
        this->fieldMap[name] = this->fields.size();  // (inserts new or overwrites existing entry)
        this->fields.push_back(field);
    }

    /** Overrides an existing field name of this tuple with a new field definition. */
    void override_field(const std::string& name, const TxField* field) {
        auto index = this->fieldMap.at(name);
        //std::cout << "Overriding field " << name << " at index " << index << ": " << field << std::endl;
        this->fieldMap[name] = index;  // (overwrites existing entry)
        this->fields[index] = field;
    }

    inline bool has_field(const std::string& name) const { return this->fieldMap.count(name); }

    inline uint32_t get_field_index(const std::string& name) const {
        if (!this->fieldMap.count(name))
            std::cout << "can't find field " << name << std::endl;
        return this->fieldMap.at(name);
    }

    inline const TxField* get_field(const std::string& name) const {
        if (!this->fieldMap.count(name))
            std::cout << "can't find field " << name << std::endl;
        return this->fields.at(this->fieldMap.at(name));
    }

    inline uint32_t get_field_count() const { return this->fields.size(); }
};



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
    std::string validate() const;

    bool operator==(const TxTypeSpecialization& other) const;

    inline bool operator!=(const TxTypeSpecialization& other) const {
        return ! this->operator==(other);
    }

    std::string to_string() const;
};



/** An instance of this class represents a type definition.
 */
class TxType : public TxEntity {
    /** The type class of this type. */
    const TxTypeClass typeClass;

    /** The type id of this type, if it is a statically distinct type (not a pure specialization). */
    uint32_t typeId = UINT32_MAX;

    /** Type parameters of this type. Should not be accessed directly, use type_params() accessor instead. */
    const std::vector<TxTypeParam> typeParams;

    const TxTypeSpecialization baseTypeSpec;  // including bindings for all type parameters of base type
    const std::vector<TxTypeSpecialization> interfaces;  // FUTURE

    // data layout:
    bool extendsParentDatatype = false;
    bool initialized = false;
    bool startedInit = false;
    DataTupleDefinition staticFields;
    DataTupleDefinition virtualFields;
    DataTupleDefinition instanceMethods;
    DataTupleDefinition instanceFields;

    /** prepares / initializes this type, including laying out its data */
    void prepare_type();

    std::string inner_validate() const;

protected:
    /** Only to be used for Any type. */
    TxType(TxTypeClass typeClass, const TxTypeDeclaration* declaration)
            : TxEntity(declaration), typeClass(typeClass), typeParams(), baseTypeSpec()  {
        this->prepare_type();
    }

    TxType(TxTypeClass typeClass, const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
           const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>())
            : TxEntity(declaration), typeClass(typeClass), typeParams(typeParams), baseTypeSpec(baseTypeSpec) {
        this->prepare_type();
    }

    /** Creates a specialization of this type. To be used by the type registry. */
    virtual TxType* make_specialized_type(TxTypeDeclaration* declaration,
                                          const TxTypeSpecialization& baseTypeSpec,  // (contains redundant ref to this obj...)
                                          const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>(),
                                          std::string* errorMsg=nullptr) const = 0;

    /** Returns true if this type can implicitly convert from the provided type.
     * Internal, base-type-specific implementation. */
    virtual bool innerAutoConvertsFrom(const TxType& someType) const { return false; }

    friend class TypeRegistry;  // allows access for registry's type construction

    /** For internal type implementation use; searches the type hierarchy's parameter bindings. */
    const TxGenericBinding* resolve_param_binding(const std::string& paramName) const;

public:
    virtual ~TxType() = default;


    virtual bool validate() const override;


    /** Gets self. Implements the TxTypeProxy interface. */
    virtual const TxType* get_type() const override { return this; }



    /*--- characteristics ---*/

    /** Gets the type id of this type. (Pure specializations return their base type's id.) */
    inline uint32_t get_type_id() const { return ( this->typeId == UINT32_MAX ? this->baseTypeSpec.type->get_type_id() : this->typeId ); }

    virtual inline const TxTypeDeclaration* get_declaration() const override {
        return static_cast<const TxTypeDeclaration*>(TxEntity::get_declaration());
    }

    const TxTypeDeclaration* get_explicit_declaration() const;

    const TxTypeDeclaration* get_nearest_declaration() const {
        if (this->get_declaration())
            return this->get_declaration();
        else
            return this->get_base_type()->get_nearest_declaration();
    }


    /** Returns true if this type has a base type (parent). (Any is the only type that has no base type.) */
    inline bool has_base_type() const { return this->baseTypeSpec.type; }

    /** Returns the type class this type belongs to. */
    inline TxTypeClass get_type_class() const { return this->typeClass; }

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

    /** Returns true if this type is generic (i.e. has unbound type parameters). */
    bool is_generic() const { return !this->type_params().empty(); }

    /** Returns true if this type is concrete (i.e. can be directly instanced).
     * A concrete type is not abstract, nor usually generic (references may be concrete while generic). */
    bool is_concrete() const;

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
    inline bool is_datatype_extension() const { return this->extendsParentDatatype; }

    /** Returns true if the size of this type is statically known.
     * May be true for arrays with non-static length initializer or compound types containing such arrays.
     */
    virtual bool is_statically_sized() const;


    /*--- namespace lookup ---*/

//    virtual TxScopeSymbol* get_member(const std::string& name) const;

// was undefined whether these should return inherited members or not
//    /** Returns a read-only, order-of-declaration iterator that points to the first declared member name. */
//    std::vector<std::string>::const_iterator member_names_cbegin() const;
//    /** Returns a read-only, order-of-declaration iterator that points to one past the last declared symbol name. */
//    std::vector<std::string>::const_iterator member_names_cend()   const;


    /** match against this entity's direct instance/static members, and then its inherited members, returning the first found */
    virtual TxEntitySymbol* lookup_instance_member(const std::string& name) const;
    virtual TxEntitySymbol* lookup_instance_member(TxScopeSymbol* vantageScope, const std::string& name) const;

//    /** match against this entity's inherited instance/static members (i.e. skipping this type's direct members) */
//    virtual const TxEntity* lookup_inherited_instance_member(const std::string& name) const;
//
//    /** match against this entity's inherited (and static) members (i.e. skipping this type's direct members) */
//    virtual const TxEntity* lookup_inherited_member(const std::string& name) const;


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


    // FUTURE: checksum?

    // FUTURE: Should we remove the == != operator overloads in favor of more specifically named comparison methods?

    /** Returns true iff the two types are equal in the Tuplex language definition sense.
     * Note that named types are non-equal if not same name. */
    virtual bool operator==(const TxType& other) const;

    /** Returns true iff the two types are unequal in the Tuplex language definition sense. */
    inline bool operator!=(const TxType& other) const  { return ! this->operator==(other); }


    /** Returns true if this type can implicitly convert from the provided type. */
    bool auto_converts_from(const TxType& other) const {
        // general logic:
        if (*this == other)
            return true;
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
    /*--- data layout ---*/

    bool is_data_laid_out() const { return this->initialized; }

    const DataTupleDefinition& get_instance_fields() const {
        ASSERT(this->initialized, "Data not laid out in " << this);
        return this->instanceFields;
    }

    const DataTupleDefinition& get_instance_methods() const {
        ASSERT(this->initialized, "Data not laid out in " << this);
        return this->instanceMethods;
    }

    const DataTupleDefinition& get_virtual_fields() const {
        ASSERT(this->initialized, "Data not laid out in " << this);
        return this->virtualFields;
    }

    const DataTupleDefinition& get_static_fields() const {
        ASSERT(this->initialized, "Data not laid out in " << this);
        return this->staticFields;
    }



    /*--- LLVM code generation methods ---*/

    virtual llvm::StructType* make_vtable_type(LlvmGenerationContext& context) const;
    virtual llvm::Function* get_type_user_init_func(LlvmGenerationContext& context) const;

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const = 0;
    virtual llvm::Value* gen_size(LlvmGenerationContext& context, GenScope* scope) const;
    virtual llvm::Value* gen_alloca(LlvmGenerationContext& context, GenScope* scope, const std::string &varName="") const;
    virtual llvm::Constant* gen_typeid(LlvmGenerationContext& context, GenScope* scope) const;

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
        for (auto & p : this->type_params()) {
            if (str.length() > 1)  str += ",";
            str += p.to_string();
        }
        str += ">";
        return str;
    }

};



/** Wraps a TxType as a TxTypeDefiner, can be used for const TxType -> non-const TxTypeDefiner conversion. */
class TxTypeDefWrapper : public TxTypeDefiner {
    const TxType* type;
public:
    TxTypeDefWrapper(const TxType* type) : type(type)  { }
    virtual TxDriver* get_driver() const override { return this->type->get_driver(); }
    virtual const yy::location& get_parse_location() const override { return this->type->get_parse_location(); }
    virtual const TxType* resolve_type(ResolutionContext& resCtx) override { return this->type; }
    virtual const TxType* attempt_get_type() const override { return this->type; }
    virtual const TxType* get_type() const override { return this->type; }
};
