#pragma once

#include <typeinfo>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <cstdlib>

#include "util/assert.hpp"
#include "util/printable.hpp"

#include "identifier.hpp"
#include "generics.hpp"

#include "type_visitor.hpp"
#include "entity.hpp"


class TxType;

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
    /** The internal, implicit interface adapter types. */
    TXTC_INTERFACEADAPTER,
};



class DataTupleDefinition {
public:
    // we need to be able to look up fields both via index and plain name:
    /** map from plain name to field index (note, contains fewer entries than fields vector when parent field name is hidden) */
    std::unordered_map<std::string, uint32_t> fieldMap;
    /** the fields */
    std::vector<const TxField*> fields;

    /** Adds a field to this tuple. If the field name already exists, the old will be shadowed by the new field. */
    void add_field(const TxField* field) {
        //std::cerr << "Adding field " << name << " at index " << this->fields.size() << ": " << field << std::endl;
        this->fieldMap[field->get_unique_name()] = this->fields.size();  // (inserts new or overwrites existing entry)
        this->fields.push_back(field);
    }

    /** Overrides an existing field name of this tuple with a new field definition. */
    void override_field(const std::string& name, const TxField* field) {
        auto index = this->fieldMap.at(name);
        //std::cerr << "Overriding field " << name << " at index " << index << ": " << field << std::endl;
        this->fields[index] = field;
    }

    /** Adds the fields from an interface that are not already present to this tuple.
     * (If a field name already exists it will not be added, i.e. neither shadowed or overridden.)
     * @return true if at least one field was added. */
    bool add_interface_fields(const DataTupleDefinition& interfaceFields);

    inline bool has_field(const std::string& name) const { return this->fieldMap.count(name); }

    inline uint32_t get_field_index(const std::string& name) const {
        if (!this->fieldMap.count(name)) {
            // can happen for EXPERR fields when dumping symbol table (their symbol is registered but aren't placed in the field table)
            std::cerr << "Error, can't find field in data tuple definition with field name: " << name << std::endl;
            return UINT32_MAX;
        }
        return this->fieldMap.at(name);
    }

    inline const TxField* get_field(const std::string& name) const {
        ASSERT(this->fieldMap.count(name), "Can't find field name '" << name << "' in data tuple table"); // a more informative err msg than the exception thrown by next line
        return this->fields.at(this->fieldMap.at(name));
    }

    inline const TxField* get_field(uint32_t ix) const {
        return this->fields.at(ix);
    }

    inline uint32_t get_field_count() const { return this->fields.size(); }

    void dump() const;
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
 * 2 is used for explicitly unique'd subtypes, and derived types that don't bind type parameters.
 * 3 is used for specializing generic types, including derived types (that extend with additional members).


type Bar<R> { ... }

type Foo<S> derives Bar<S> { ... }

type Mud Foo<Long>
type Mac Foo<Byte>

Mud : Foo$Long : Bar$Long : Tuple : Any
               \          \
                : Foo<Any> : Bar<Any> : Tuple : Any
               /          /
Mac : Foo$Byte : Bar$Byte : Tuple : Any

Foo<Any> is the "generic base type" for Foo$xyz
Bar$xyz is the     "base data type" for Foo$xyz

It's the base data type that the vtable mechanics use.
For semantic inheritance mechanics, the generic base type is used.
 */
class TxTypeSpecialization : public Printable {
public:
    TxType const * const type;
    const bool modifiable;
    //TxIdentifier const * const dataspace;  // only set for reference specializations

    /** Only legal to use by the Any type. */
    TxTypeSpecialization()
            : type(), modifiable()  { }

    /** Copies another TxTypeSpecialization but with a different base type. */
    TxTypeSpecialization(const TxTypeSpecialization& other, const TxType* baseType)
            : type(baseType), modifiable(other.modifiable)  {
        ASSERT(baseType, "NULL baseType");
    }

    TxTypeSpecialization(const TxType* baseType, bool modifiable=false)
            : type(baseType), modifiable(modifiable)  {
        ASSERT(baseType, "NULL baseType");
    }

    TxTypeSpecialization(const TxType* baseType, const TxIdentifier* dataspace)
            : type(baseType), modifiable(false)  {
        ASSERT(baseType, "NULL baseType");
    }

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
    /** true if this is a built-in type */
    const bool builtin;

    /** The runtime type id of this type, if it is a distinct runtime type (not an equivalent specialization). */
    uint32_t runtimeTypeId = UINT32_MAX;

    /** Type parameters of this type. Should not be accessed directly, use type_params() accessor instead. */
    std::vector<TxEntityDeclaration*> params;

    /** Bindings of the base type's type parameters. Should not be accessed directly, use type_bindings() accessor instead. */
    std::vector<TxEntityDeclaration*> bindings;

    const TxTypeSpecialization baseTypeSpec;  // including bindings for all type parameters of base type
    const std::vector<TxTypeSpecialization> interfaces;

    /** Set for non-generic specializations of a generic base type.
     * This also implies a pure specialization, even if extendsInstanceDatatype technically is true. */
    const TxType* genericBaseType = nullptr;

    /** false unless there are TYPE parameters with other than Ref constraint */
    bool nonRefParameters = false;
    /** false unless there are TYPE bindings for parameters with other than Ref constraint */
    bool nonRefBindings = false;

    /** false until prepare_type_members() has completed (after that this object should be considered immutable) */
    bool prepared = false;

    /** Prepares / initializes this type. Called from constructor. */
    void prepare_type();
    void prepare_type_validation() const;

protected:
    bool extendsInstanceDatatype = false;
    bool modifiesVTable = false;
    bool emptyDerivation = false;
    bool pureDerivation = false;
    // data layout:
    DataTupleDefinition staticFields;
    DataTupleDefinition virtualFields;
    DataTupleDefinition instanceFields;

    /** Only to be used for Any type. */
    TxType(TxTypeClass typeClass, const TxTypeDeclaration* declaration)
            : TxEntity(declaration), typeClass(typeClass), builtin(declaration && (declaration->get_decl_flags() & TXD_BUILTIN)),
              baseTypeSpec(), interfaces()  {
        this->prepare_type();
    }

    TxType(TxTypeClass typeClass, const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
           const std::vector<TxTypeSpecialization>& interfaces=std::vector<TxTypeSpecialization>())
            : TxEntity(declaration), typeClass(typeClass), builtin(declaration && (declaration->get_decl_flags() & TXD_BUILTIN)),
              baseTypeSpec(baseTypeSpec), interfaces(interfaces) {
        this->prepare_type();
    }

    /** Creates a specialization of this type. To be used by the type registry. */
    virtual TxType* make_specialized_type(const TxTypeDeclaration* declaration,
                                          const TxTypeSpecialization& baseTypeSpec,  // (contains redundant ref to this obj...)
                                          const std::vector<TxTypeSpecialization>& interfaces=std::vector<TxTypeSpecialization>()) const = 0;

    friend class TypeRegistry;  // allows access for registry's type construction

    /** Gets the Any root type. */
    inline const TxType* get_root_any_type() const {
        if (! this->has_base_type())
            return this;
        return this->get_semantic_base_type()->get_root_any_type();
    }

public:
    virtual ~TxType() = default;

    virtual const TxLocation& get_parse_location() const override;

    virtual ExpectedErrorClause* exp_err_ctx() const override;

    virtual TxParserContext* get_parser_context() const;

    virtual bool validate() const override { return true; }


    inline bool is_prepared() const { return this->prepared; }

    /** Prepares this type's members, including data layout. Called after object construction has completed. */
    virtual void prepare_type_members();

    /** Gets the runtime type id of this type. (Equivalent specializations return their base type's id.) */
    inline uint32_t get_type_id() const {
        ASSERT(this->prepared, "Can't get runtime type id of unprepared type: " << this);
        return ( this->runtimeTypeId == UINT32_MAX ? this->get_semantic_base_type()->get_type_id() : this->runtimeTypeId );
    }


    virtual inline const TxTypeDeclaration* get_declaration() const override final {
        return static_cast<const TxTypeDeclaration*>(TxEntity::get_declaration());
    }

    const TxTypeDeclaration* get_explicit_declaration() const;

    const TxTypeDeclaration* get_nearest_declaration() const {
        if (this->get_declaration())
            return this->get_declaration();
        else
            return this->get_semantic_base_type()->get_nearest_declaration();
    }

    /** Returns true if this type is explicitly declared and is not a generic parameter nor generic binding. */
    bool is_explicit_nongen_declaration() const;


    /*--- characteristics ---*/

    /** Returns true if this type has a base type (parent). ('Any' is the only type that has no base type.) */
    inline bool has_base_type() const { return this->baseTypeSpec.type; }

    /** Gets the base type (parent) of this type.
     * ('Any' is the only type that has no base type, in which case null is returned.) */
    inline const TxType* get_base_type() const { return this->baseTypeSpec.type; }

    /** Gets the 'semantic' base type (parent) of this type,
     * which is the same as get_base_type() except for generic type specializations
     * in which case the generic base type is returned (instead of the implicitly generated specialization thereof). */
    inline const TxType* get_semantic_base_type() const { return this->genericBaseType ? this->genericBaseType : this->baseTypeSpec.type; }

    /** Gets the "instance base type" of this type, which is either this type, or the closest ancestor type
     * which defines a distinct instance data type.
     * This is used to bypass same-instance-type derivations (e.g. empty/mod. specializations). */
    const TxType* get_instance_base_type() const;


//    /** Returns true if this type is a reinterpreted specialization, i.e. reinterpreted source
//     * with a specialization index greater than zero.
//     */
//    bool is_reinterpreted() const;
//
//    /** Returns true if this type is a reinterpreted specialization, i.e. reinterpreted source
//     * with at least one parameter binding and a specialization index greater than zero.
//     * Note: There are reinterpreted types for which this is false: Types that in themselves are not
//     * specializations of a generic base type, but are a member of another type that is being reinterpreted.
//     */
//    inline bool is_reinterpreted_specialization() const { return this->genericBaseType; }

    /** Returns true if this type is a reinterpreted specialization that is also equivalent to its base type,
     * i.e. no non-ref-constrained type parameters were bound.
     * Note: There are reinterpreted types for which this is false: Types that in themselves are not
     * specializations of a generic base type, but are a member of another type that is being reinterpreted.
     */
    bool is_equivalent_reinterpreted_specialization() const;


    /** Returns the type class this type belongs to. */
    inline TxTypeClass get_type_class() const { return this->typeClass; }

    /** Returns true iff this type is a built-in type. */
    inline bool is_builtin() const { return this->builtin; }

    /** Returns true if this type is modifiable (its instances' contents may be modified after initialization).
     * This can only be the case for pure specializations:
     * Declaring a "modifiable" type in source obtains a pure specialization of the sought base type
     * with the modifiable flag set.
     * (The base type must of course be mutable for such a specialization to be legal.)
     * A corollary is that a non-pure-specialization type is never immediately modifiable,
     * it must first be specialized as such.
     */
    inline bool is_modifiable() const { return this->baseTypeSpec.modifiable; }

    /** Returns true if this type is immutable (its instances' contents can never be modified after initialization). */
    virtual bool is_immutable() const { return false; }

    /** Returns true if this type cannot be extended. */
    virtual bool is_final() const { return false; }

    /** Returns true if this type is declared abstract. */
    virtual bool is_abstract() const;

    /** Returns true if this type is generic (i.e. has unbound type parameters).
     * Note that a non-generic type may still have members that refer to unbound type parameters of an outer scope
     */
    inline bool is_generic() const { return !this->type_params().empty(); }

    /** Returns true if this type is concrete (i.e. can be directly instanced).
     * A concrete type is not abstract, nor usually generic.
     * References are always concrete (even if generic). */
    bool is_concrete() const;

    /** Returns true if this type has the same vtable as its base type. */
    inline bool is_same_vtable_type() const { return this->has_base_type() && !this->modifiesVTable; }

    /** Returns true if this type has the same instance data type as its base type. */
    inline bool is_same_instance_type() const { return this->has_base_type() && !this->extendsInstanceDatatype; }

    /** Returns true if this type is an empty derivation of a base type,
     * i.e. does not specialize any type parameters of the base type, nor modifiable,
     * nor extends the base type with any members, nor derives any additional interfaces.
     * This implies that this type is fully equivalent to its base type.
     * (An empty specialization is by implication a pure specialization.)
     * (Returns false for Any which has no base type.)
     */
    bool is_empty_derivation() const;

    /** Returns true if this type is equivalent to its base type,
     * i.e. is effectively the same *instance data type* as well as same static definition as the base type.
     * If true, this type does not need a distinct vtable nor distinct code generation.
     *
     * Added bound non-ref type parameters, added virtual members, added instance members cause this to return false;
     * interfaces, modifiability, and bound ref-constrained type parameters do not.
     * (Returns false for Any which has no base type.)
     */
    bool is_equivalent_derivation() const;

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
     * (Returns false for Any which has no base type.)
     */
    bool is_pure_specialization() const;

    /** Returns true if this type is a virtual derivation of a base type,
     * i.e. is effectively the same *instance data type* as the base type.
     * Added instance fields and bound non-ref type parameters cause this to return false;
     * modifiability, bound ref-constrained type parameters, added interfaces, added virtual members do not.
     * (Returns false for Any which has no base type.)
     */
    bool is_virtual_derivation() const;

    /** Returns true if the size of this type is statically known.
     * May be true for arrays with non-static length initializer or compound types containing such arrays.
     */
    virtual bool is_statically_sized() const;


    // FUTURE: checksum?

    // FUTURE: Should we remove the == != operator overloads in favor of more specifically named comparison methods?

    /** Returns true iff the two types are equal in the Tuplex language definition sense.
     * Note that named types are non-equal if not same name. */
    virtual bool operator==(const TxType& other) const;

    /** Returns true iff the two types are unequal in the Tuplex language definition sense. */
    inline bool operator!=(const TxType& other) const  { return ! this->operator==(other); }


    /** Returns true if an instance of this type can implicitly convert to an instance of the destination type.
     * Note that this does not test whether destination is modifiable;
     * it only tests type instance compatibility for assignment.
     * (For example, an initializer to an unmodifiable field is still valid if assignable to its type.)
     * This is a less strict test than is_assignable, since some types that are not directly assignable
     * may be so after an implicit conversion (e.g. Byte -> Int). */
    virtual bool auto_converts_to(const TxType& destination) const {
        return this->is_assignable_to(destination);  // default implementation is equal to assignability
    }

    /** Returns true if an instance of this type can be assigned to a field of the provided type
     * (without performing any value conversion).
     * Note that this does not test whether destination is modifiable;
     * it only tests type instance compatibility for assignment.
     * (For example, an initializer to an unmodifiable field is still valid if assignable to its type.)
     * For many type classes this is a more strict test than is-a,
     * however for functions, arrays, references and adapters this test concerns data type equivalence and
     * substitutability rather than is-a relationship. */
    virtual bool is_assignable_to(const TxType& destination) const;

    /** Returns true if the provided type is the same as this, or a specialization of this.
     * Note that true does not guarantee assignability, for example modifiability is not taken into account.
     */
    bool is_a(const TxType& other) const;

private:
    bool inner_equals(const TxType& otherType) const;

    /** Returns the common base type of this and other, if both are pure specializations of it. */
    const TxType* common_generic_base_type(const TxType& other) const;

    bool derives_object(const TxType* objectType) const;
    bool derives_interface(const TxType* interfaceType) const;

public:
    /*--- namespace lookup ---*/

// was undefined whether these should return inherited members or not
//    /** Returns a read-only, order-of-declaration iterator that points to the first declared member name. */
//    std::vector<std::string>::const_iterator member_names_cbegin() const;
//    /** Returns a read-only, order-of-declaration iterator that points to one past the last declared symbol name. */
//    std::vector<std::string>::const_iterator member_names_cend()   const;

    /** match against this entity's direct instance/static members (not its inherited members). */
    virtual TxEntitySymbol* get_instance_member(const std::string& name) const;
    virtual TxEntitySymbol* get_instance_member(TxScopeSymbol* vantageScope, const std::string& name) const;

    /** match against this entity's direct instance/static members, and then its inherited members, returning the first found */
    virtual TxEntitySymbol* lookup_inherited_instance_member(const std::string& name) const;
    virtual TxEntitySymbol* lookup_inherited_instance_member(TxScopeSymbol* vantageScope, const std::string& name) const;

    /** Specialized lookup: searches the type hierarchy's parameter bindings to find the binding for a parameter. */
    const TxEntityDeclaration* lookup_param_binding(const TxEntityDeclaration* paramDecl) const;
    const TxFieldDeclaration* lookup_value_param_binding(const std::string& fullParamName) const;
    const TxTypeDeclaration* lookup_type_param_binding(const std::string& fullParamName) const;


    /*--- type parameter handling ---*/

    /** Gets the (unbound) type parameters of this type (this type is a generic type if this is non-empty). */
    const std::vector<TxEntityDeclaration*>& type_params() const {
        return ( (this->is_empty_derivation() || this->is_modifiable()) ? this->get_base_type()->type_params() : this->params );
    }

    /** Returns true if this type has an (unbound) type parameter with the specified (plain) name. */
    bool has_type_param(const std::string& plainParamName) const {
        for (auto & p : this->type_params())
            if (p->get_unique_name() == plainParamName)
                return true;
        return false;
    }

    /** Gets the declaration of a type parameter of this type. (Note - this does not search ancestors' bound parameters.) */
    const TxEntityDeclaration* get_type_param_decl(const std::string& plainParamName) const {
        for (auto & paramDecl : this->type_params()) {
            if (plainParamName == paramDecl->get_unique_name())
                return paramDecl;
        }
        return nullptr;
    }


//    /** Returns true if there are TYPE parameters with other than Ref constraint. */
//    inline bool has_nonref_parameters() const { return this->nonRefParameters; }
//
//    /** Returns true if there are TYPE bindings for base type's parameters with other than Ref constraint. */
//    inline bool has_nonref_bindings() const { return this->nonRefBindings; }

    inline const std::vector<TxEntityDeclaration*>& get_bindings() const { return this->bindings; }

    /** Gets the declaration of a type parameter of this type, or null if it doesn't exist.
     * (Note - this does not search ancestors' bound parameters.) */
    const TxEntityDeclaration* get_binding(const std::string& plainName) const {
        for (auto b : this->get_bindings())
            if (b->get_unique_name() == plainName
                    || (b->get_unique_name()[0] == '$' && b->get_unique_name().compare(1, std::string::npos, plainName) == 0))
                return b;
        return nullptr;
    }



    /*--- data layout ---*/

    inline const DataTupleDefinition& get_instance_fields() const { return this->instanceFields; }

    inline const DataTupleDefinition& get_virtual_fields() const { return this->virtualFields; }

    inline const DataTupleDefinition& get_static_fields() const { return this->staticFields; }



    /*--- LLVM code generation methods ---*/

    virtual llvm::StructType* make_vtable_type(LlvmGenerationContext& context) const;
    virtual llvm::Function* get_type_user_init_func(LlvmGenerationContext& context) const;

    /** Returns the llvm::Type for an instance of this type (possibly only an opaque struct declaration). */
    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const = 0;
    /** Invoked after make_llvm_type() to augment a possibly forward-declared llvm::Type "header" (named, opaque struct).
     * Default implementation returns the "header" type without modifying it;
     * types that actually predefine an opaque header should override and augment the type or return a new, full type. */
    virtual llvm::Type* make_llvm_type_body(LlvmGenerationContext& context, llvm::Type* header) const { return header; }
    virtual llvm::Value* gen_size(LlvmGenerationContext& context, GenScope* scope) const;
    virtual llvm::Value* gen_alloca(LlvmGenerationContext& context, GenScope* scope, const std::string &varName="") const;
    virtual llvm::Constant* gen_typeid(LlvmGenerationContext& context, GenScope* scope) const;

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }



    /*--- to string methods ---*/

    virtual std::string to_string() const override final { return this->to_string(false); }
    virtual std::string to_string(bool brief, bool skipFirstName=false, bool skipImplicitNames=false) const final;

protected:
    virtual void self_string(std::stringstream& str, bool brief, bool skipFirstName, bool skipImplicitNames) const;
};