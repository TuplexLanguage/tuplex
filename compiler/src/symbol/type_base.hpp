#pragma once

#include <vector>
#include <unordered_map>
#include <algorithm>
#include <cstdlib>

#include "util/assert.hpp"
#include "util/printable.hpp"

#include "identifier.hpp"
#include "tx_lang_defs.hpp"

#include "entity.hpp"
#include "type_class.hpp"
#include "type_class_handler.hpp"


class TxActualType;
class TxFieldDeclNode;

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

class DataTupleDefinition {
public:
    // we need to be able to look up fields both via index and plain name:
    /** map from plain name to field index (note, contains fewer entries than fields vector when parent field name is hidden) */
    std::unordered_map<std::string, uint32_t> fieldMap;
    /** the fields */
    std::vector<const TxField*> fields;

    /** Adds a field to this tuple. If the field name already exists, the old will be shadowed by the new field. */
    void add_field( const TxField* field ) {
        //std::cerr << "Adding field at index " << this->fields.size() << ": " << field << std::endl;
        this->fieldMap[field->get_unique_name()] = this->fields.size();  // (inserts new or overwrites existing entry)
        this->fields.push_back( field );
    }

    /** Overrides an existing field name of this tuple with a new field definition. */
    void override_field( const TxField* field ) {
        auto index = this->fieldMap.at( field->get_unique_name() );
        //std::cerr << "Overriding field " << name << " at index " << index << ": " << field << std::endl;
        this->fields[index] = field;
    }

    /** Adds the fields from an interface that are not already present to this tuple.
     * (If a field name already exists it will not be added, i.e. neither shadowed or overridden.)
     * @return true if at least one field was added. */
    bool add_interface_fields( const DataTupleDefinition& interfaceFields );

    inline bool has_field( const std::string& name ) const {
        return this->fieldMap.count( name );
    }

    inline uint32_t get_field_index( const std::string& name ) const {
        if ( !this->fieldMap.count( name ) ) {
            // can happen for EXPERR fields when dumping symbol table (their symbol is registered but aren't placed in the field table)
            // or for VALUE type parameters of type specializations (the binding of a VALUE parameter doesn't allocate a new field slot)
            //std::cerr << "(Perhaps caused by symbol dump) Note, can't find field in data tuple definition with field name: " << name << std::endl;
            return UINT32_MAX;
        }
        return this->fieldMap.at( name );
    }

    inline const TxField* get_field( const std::string& name ) const {
        ASSERT( this->fieldMap.count( name ), "Can't find field name '" << name << "' in data tuple table" ); // a more informative err msg than the exception thrown by next line
        return this->fields.at( this->fieldMap.at( name ) );
    }

    inline const TxField* get_field( uint32_t ix ) const {
        return this->fields.at( ix );
    }

    inline uint32_t get_field_count() const {
        return this->fields.size();
    }

    void dump() const;
};


bool is_concrete_uinteger_type( const TxActualType* type );

bool is_concrete_sinteger_type( const TxActualType* type );

bool is_concrete_floating_type( const TxActualType* type );


/** An instance of this class represents a type definition.
 *
 * Type instances are not to be constructed directly, they may only by created from the built-in
 * type definitions and from the type registry.
 *
 *
 * In a specialization of a generic base type, the base type's type parameters must either have a binding,
 * or be redeclared with the same name and matching constraints in the specialized type
 * (keeping the parameter open for further specialization, with identical or narrowed type parameter
 * constraints).
 *
 * Use cases:
 * 1. Empty extension - pass-through type parameters
 * 2. Type parameter specialization - one or more bindings are set
 *
 * 1 is used for explicitly unique'd subtypes, and derived types that don't bind type parameters.
 * 2 is used for specializing generic types, including derived types (that extend with additional members).


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
class TxActualType : public TxEntity { //public virtual TxParseOrigin, public Printable {
    static Logger& _LOG;

    friend class TxTypeClassHandler;

    /** The type class of this type. */
    TxTypeClass typeClass = TXTC_ANY;
    const TxTypeClassHandler* typeClassHandler = nullptr;

    /** true if this is a built-in type */
    const bool builtin;

    /** The runtime type id of this type. */
    uint32_t runtimeTypeId = UINT32_MAX;

    /** If true, this type and its members is to skip code-generation. */
    bool suppressCodeGen = false;

    /** If true, this type is mutable, in which case its instances may be declared modifiable. */
    const bool mutableType;

    /** Type parameters of this type. Should not be accessed directly, use type_params() accessor instead. */
    std::vector<const TxEntityDeclaration*> params;

    /** Bindings of the base type's type parameters. Should not be accessed directly, use type_bindings() accessor instead. */
    std::vector<const TxEntityDeclaration*> bindings;

    const TxActualType* baseType;
    std::vector<const TxActualType*> interfaces;

    /** Set for non-generic specializations of a generic base type.
     * This also implies a pure specialization, even if extendsInstanceDatatype technically is true. */
    const TxActualType* genericBaseType = nullptr;

    /** The public constructors of this type (excludes the implicit initializers if explicit constructors provided) */
    std::vector<const TxFieldDeclaration*> constructors;

    /** ASTs for the implicit initializers */
    std::vector<TxFieldDeclNode*> implicitConstructorNodes;

    /** the instance fields of this type to initialize (includes the fields with direct constant initializer expressions) */
    std::vector<const TxFieldDeclaration*> instanceFieldsToInitialize;

//    /** False unless there are either TYPE bindings for parameters with other than Ref constraint, or VALUE bindings.
//     * If this is a Ref type, this is always false (regardless of the Ref's bound target type). */
//    bool nonRefBindings = false;
    bool hasExplicitFieldMembers = false;
    bool hasTypeBindings = false;
    bool hasValueBindings = false;

    /** False unless this type has at least one VALUE binding and no other extensions. */
    bool pureValueSpec = false;

    /** False unless this type is generic and has at least one unbound TYPE type parameter. */
    bool typeGeneric = false;

    /** False unless this type is generic and has at least one unbound VALUE type parameter. */
    bool valueGeneric = false;

    /** true when integrate() has completed */
    bool hasIntegrated = false;

    /** true after prepare_members() has started - guards against recursive data types */
    bool startedPrepare = false;
    /** false until prepare_members() has completed (after that this object should be considered immutable) */
    bool hasPrepared = false;

    /** Examines the members of this type and identifies generic parameters and bindings. Called from constructor. */
    void examine_members();

    /** Initializes this type, determines the kind of derivation. Called when the type class is known. */
    void initialize_type();

    void autogenerate_constructors();

    /** Returns true if is a type with its own constructors (certain derivations reuse parent's constructors). */
    bool is_construction_type() const;

    /** Performs basic validation. */
    void validate_type() const;

    const TxTypeExpressionNode* baseTypeNode;
    const std::vector<const TxTypeExpressionNode*> interfaceNodes;

protected:
    bool modifiesInstanceDatatype = false;
    bool modifiesVTable = false;
    bool emptyDerivation = false;
    bool pureDerivation = false;
    // data layout:
    DataTupleDefinition staticFields;
    DataTupleDefinition virtualFields;
    DataTupleDefinition instanceFields;

    friend class TypeRegistry;  // allows access for registry's type construction
    friend class BuiltinTypes;
    friend class TxBuiltinTypeDefiningNode;

    /** Gets the Any root type. */
    const TxActualType* get_root_any_qtype() const;

    /** Prepares this type's members, including data layout. Called after resolution phase has completed.
     * @return true if a data type recursion has been discovered */
    virtual bool inner_prepare_members();

    /** Construction of type with known type class and super types; invoked from subclasses. */
    TxActualType( const TxTypeClassHandler* typeClassHandler, const TxTypeDeclaration* declaration, bool mutableType,
                  const TxActualType* baseType );

public:
    inline Logger* LOGGER() const {
        return &this->_LOG;
    }

    /** Construction of type without base types, i.e. Any and Void. */
    TxActualType( const TxTypeClassHandler* typeClassHandler, const TxTypeDeclaration* declaration, bool mutableType );

    /** Construction of type-class-root-types. (Although the base type may be already known, the interfaces may not be.) */
    TxActualType( const TxTypeClassHandler* typeClassHandler, const TxTypeDeclaration* declaration, bool mutableType,
                  const TxTypeExpressionNode* baseTypeNode,
                  std::vector<const TxTypeExpressionNode*> interfaceNodes );

    /** Construction of type whose type class and super types are not yet resolved.
     * This is called from type registry.
     * Note that this creation & resolution order is necessary in order to handle constructs such as:
     *   interface Number{N derives Number{N}}
     */
    TxActualType( const TxTypeDeclaration* declaration, bool mutableType,
                  const TxTypeExpressionNode* baseTypeNode,
                  std::vector<const TxTypeExpressionNode*> interfaceNodes );

    /** virtual, default destructor */
    ~TxActualType() override = default;

    const TxNode* get_origin_node() const override;

    inline bool is_initialized() const {
        return this->typeClassHandler;
    }

    inline bool is_integrated() const {
        return this->hasIntegrated;
    }

    inline bool is_prepared() const {
        return this->hasPrepared;
    }

    /** Initializes this type and sets its type class. */
    void initialize_with_type_class( const TxTypeClassHandler* typeClassInstance );

    /** resolve type parameters and bindings */
    void resolve_params( TxPassInfo pass );

    /** Integrates this type with its declaration dependencies - base class, interfaces, generic parameters/bindings.
     * Will also initialize this type with its type class if is isn't already. */
    void integrate();

    /** Prepares this type's members, including data layout. Called after resolution phase has completed.
     * Also checks against recursive data types (a compilation error is expected to be generated by the caller).
     * @return true if a data type recursion has been discovered */
    bool prepare_members();

    const std::vector<const TxFieldDeclaration*>& get_constructors() const {
        return this->constructors;
    }

    const std::vector<TxFieldDeclNode*>& get_implicit_constructors() const {
        return this->implicitConstructorNodes;
    }

    /** Returns the type holding the constructor methods for this type. This is typically this type,
     * but for empty derivations and pure value specializations it may be a parent type. */
    const TxActualType* get_construction_type() const;

    /** the instance fields of this type to initialize (includes the fields with direct constant initializer expressions) */
    const std::vector<const TxFieldDeclaration*>& get_instance_fields_to_initialize() const {
        return this->instanceFieldsToInitialize;
    }

    /** @return true if this type and its members is to skip code-generation */
    bool suppress_code_gen() const {
        return this->suppressCodeGen;
    }

    inline const TxTypeClassHandler* type_class_handler() const {
        ASSERT( this->typeClassHandler, "NULL type class handler in " << this->get_declaration() );
        return this->typeClassHandler;
    }

    inline const TxTypeDeclaration* get_declaration() const override {
        return static_cast<const TxTypeDeclaration*>( TxEntity::get_declaration() );
    }

    /** Gets the runtime type id of this type.
     * For non-built-in types, this is only valid to call after the type preparation phase. */
    inline uint32_t get_runtime_type_id() const {
        ASSERT( this->runtimeTypeId != UINT32_MAX, "Type id not set for " << this->get_declaration() );
        return this->runtimeTypeId;
    }

    /** Returns true if this type instance has a runtime type id assigned.
     * If called before the type preparation phase, only built-in types return true. */
    inline bool has_runtime_type_id() const {
        return ( this->runtimeTypeId != UINT32_MAX );
    }

    /** Gets the runtime type id of the elementary built-in type this type is, or derives from.
     * This is a special helper function to support equivalent handling of the built-in elementary types
     * and user types that derive from those (which will always be empty derivations).
     * For example, given
     * type Foo derives Int;
     * For Foo this function returns the type id of Int.
     * For non-elementary types UINT32_MAX is returned. */
    uint32_t get_elementary_type_id() const;


    /*--- characteristics ---*/

    /** Returns true if this type has a base type (parent). ('Any' is the only type that has no base type.) */
    inline bool has_base_type() const {
        return this->baseType || this->baseTypeNode;
    }

    /** Gets the base type (parent) of this type.
     * ('Any' is the only type that has no base type, in which case null is returned.) */
    inline const TxActualType* get_base_type() const {
        ASSERT( this->hasIntegrated, "Can't get base type of unintegrated type " << this->get_declaration() );
        return this->baseType;
    }

    /** Gets the generic base type (parent) of this type, or null if this type is not a specialization.
     * Note, this is valid to call for types that are not yet initialized. */
    inline const TxActualType* get_generic_base_type() const {
        return this->genericBaseType;
    }

    /** Gets the 'semantic' base type (parent) of this type,
     * which is the same as get_base_type() except for generic type specializations
     * in which case the generic base type is returned (instead of the implicitly generated specialization thereof). */
    inline const TxActualType* get_semantic_base_type() const {
        // (note, genericBaseType is set before initialization and integration, but baseType isn't)
        return this->genericBaseType ? this->genericBaseType : this->get_base_type();
    }

    /** Gets the original source base type (parent) of this type,
     * which is the original generic base type definition, bypassing any partial specializations. */
    inline const TxActualType* get_source_base_type() const {
        // (note, genericBaseType is set before initialization and integration, but baseType isn't)
        auto type = this;
        while ( type->genericBaseType )
            type = type->genericBaseType;
        return type;
    }

    inline const std::vector<const TxActualType*>& get_interfaces() const {
        ASSERT( this->hasIntegrated, "Can't get interfaces of unintegrated type " << this->get_declaration() );
        return this->interfaces;
    }

    /** Returns the type class this type belongs to. */
    inline TxTypeClass get_type_class() const {
        ASSERT( this->typeClassHandler, "Can't get type class of uninitialized type " << this->get_declaration() );
        return this->typeClass;
    }

    /** Returns true if this type is a built-in type. */
    inline bool is_builtin() const {
        return this->builtin;
    }

    /** Returns true if this type is the specified built-in type. */
    inline bool is_builtin( BuiltinTypeId biTypeId ) const {
        return ( this->runtimeTypeId == (uint32_t) biTypeId );  // Note: type ids of built-in types are always set
    }

    /** Returns true if this type is a scalar type. */
    bool is_scalar() const;

    /** Returns true if this type is mutable by declaration.
     * If true its instances can be declared modifiable.
     */
    inline bool is_mutable() const {
        return this->mutableType;
    }

    /** Returns true if this type cannot be derived from. (Final generic types can still be specialized.) */
    inline bool is_final() const {
        return ( this->get_declaration()->get_decl_flags() & TXD_FINAL );
    }

    /** Returns true if this type is declared abstract.
     * Note that there can be types that are neither declared abstract or concrete,
     * if they are dependent on generic type parameters. */
    bool is_abstract() const {
        // (special case lets user skip 'abstract' keyword in interface declarations)
        // TODO: make TXD_ABSTRACT flag be automatically set for all interfaces
        return ( this->get_type_class() == TXTC_INTERFACE
                 || ( this->get_type_class() != TXTC_REFERENCE && ( this->get_declaration()->get_decl_flags() & TXD_ABSTRACT ) ) );
    }

    /** Returns true if this type is concrete.
     * A concrete type is not declared abstract, nor is it dependent on any non-reference-constrained unbound type parameter.
     * This includes both unbound type parameters of this type and of its outer lexical scope, if any.
     * Reference types are always concrete.
     */
    bool is_concrete() const;

    /** Returns true if this type is static, which means it is concrete and non-dynamic.
     * A static type can be directly instanced since its data type and size is fully known at compile time.
     * Reference types are always static.
     */
    bool is_static() const;

    /** Returns true if this type is dynamic, which means it is concrete and dependent on one or more
     * VALUE type parameter bindings with a dynamic value (not known at compile time).
     * The size of a dynamic type may not be known at compile time, but it is instantiable
     * via an expression that computes the dynamic parameter bindings.
     */
    bool is_dynamic() const;

    /** Returns true if this type is generic (i.e. has unbound type parameters).
     * Note that a non-generic type may still have members that refer to unbound type parameters of an outer scope.
     * @see is_generic_dependent()
     */
    inline bool is_generic() const {
        return !this->get_type_params().empty();
    }

    /** Returns true if this type is type-generic (i.e. has unbound TYPE type parameters). */
    inline bool is_type_generic() const {
        return this->typeGeneric;
    }

    /** Returns true if this type is value-generic (i.e. has unbound VALUE type parameters). */
    inline bool is_value_generic() const {
        return this->valueGeneric;
    }

    /** Returns true if this type is dependent on any unbound generic TYPE type parameters.
     * This is true if this type is type-generic (has unbound TYPE parameters),
     * or is defined within an outer scope of a type which is type-generic-dependent,
     * or is a specialization whose bindings in turn are type-generic-dependent.
     * If this is true, this type is not fully specified,
     * and can (typically) not be code-generated.
     */
    bool is_type_generic_dependent() const;

    /** Returns true if this type is dependent on any unbound generic VALUE type parameters.
     * This is true if this type is value-generic (has unbound VALUE parameters),
     * or is defined within an outer scope of a type which is value-generic-dependent,
     * or is a specialization whose bindings in turn are value-generic-dependent.
     * If this is true, this type's size might not be fully specified,
     * but can (typically) still be code-generated.
     */
    bool is_value_generic_dependent() const;

    /** Returns true if this type is a generic type parameter.
     * (Note: Generic type bindings are not distinct type instances, they are aliases.)
     */
    inline bool is_generic_param() const {
        return ( this->get_declaration()->get_decl_flags() & TXD_GENPARAM );
    }

    /** Returns true if this type has the same vtable as its base type. */
    inline bool is_same_vtable_type() const {
        ASSERT( this->is_initialized(), "Can't determine same vtable type of uninitialized type " << this->get_declaration() );
        return this->has_base_type() && !this->modifiesVTable;
    }

    /** Returns true if this type has the same instance data type as its base type. */
    inline bool is_same_instance_type() const {
        ASSERT( this->is_initialized(), "Can't determine same instance type of uninitialized type " << this->get_declaration() );
        return this->has_base_type() && !this->modifiesInstanceDatatype;
    }

    /** Returns true if this type is an empty derivation of a base type,
     * i.e. does not specialize any type parameters of the base type,
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
     * interfaces, and bound ref-constrained type parameters do not.
     * (Returns false for Any which has no base type.)
     */
    bool is_equivalent_derivation() const;

    /** Returns true if this type is a virtual derivation of a base type,
     * i.e. is effectively the same *instance data type* as the base type.
     * Added instance fields and bound non-ref type parameters cause this to return false;
     * Bound ref-constrained type parameters, added interfaces, added virtual members do not.
     * (Returns false for Any which has no base type.)
     */
    bool is_virtual_derivation() const;

    /** Returns true if this type is a pure value specialization of a base type,
     * i.e. is a specialization of a generic base type with only VALUE type parameter bindings and no other extensions.
     * Bound VALUE parameters may affect instance data type size (e.g. arrays),
     * but don't affect other semantics and doesn't require distinct code-generation.
     * (Returns false for Any which has no base type.)
     */
    inline bool is_pure_value_specialization() const {
        return this->pureValueSpec;
    }

    /** Returns true if this type is a specialization of a generic base type. */
    inline bool is_generic_specialization() const { return this->genericBaseType; }

    /** Returns true if this type has no subtypes. */
    bool is_leaf_derivation() const;

    inline bool is_explicit_declaration() const {
        return !( this->get_declaration()->get_decl_flags() & TXD_IMPLICIT );
    }

    /** Returns true if this is a generic type or a direct specialization of a generic type.
     * Note - does not bypass empty derivations. */
    inline bool is_gen_or_spec() const {
        return ( !this->params.empty() || !this->bindings.empty() );
    }


    /*--- relationships ---*/

    // FUTURE: Should we remove the == != operator overloads in favor of more specifically named comparison methods?

    /** Returns true iff the two types are equal in the Tuplex language definition sense.
     * Note that named types are non-equal if not same name. */
    bool operator==( const TxActualType& other ) const;

    /** Returns true iff the two types are unequal in the Tuplex language definition sense. */
    inline bool operator!=( const TxActualType& other ) const {
        return !this->operator==( other );
    }

    /** Returns true if the provided type is the same as this, or a specialization of this.
     * Note that true does not guarantee assignability, for example modifiability is not taken into account.
     */
    bool is_a( const TxActualType& other ) const {
        return this->type_class_handler()->is_a( this, &other );
    }

    /** Returns true if the provided type is the same as this, or a primary-path specialization of this.
     * This condition must be satisfied for vtable substitutability.
     */
    bool is_a_primary_path( const TxActualType& other ) const;

    /** Returns true if an instance of this type can implicitly convert to an instance of the destination type.
     * This may be less strict test than is_assignable, since some types that are not directly assignable
     * may be so after an implicit conversion (e.g. Byte -> Int). */
    bool auto_converts_to( const TxActualType& destination ) const {
        return this->type_class_handler()->auto_converts_to( this, &destination );
    }

    /** Returns true if an instance of this type can be assigned to a field of the provided type
     * (without performing any value conversion).
     * Note that this does not test whether destination is modifiable;
     * it only tests type instance compatibility for assignment.
     * (For example, an initializer to an unmodifiable field is still valid if assignable to its type.)
     * For many type classes this is a more strict test than is-a,
     * however for functions, arrays, references and adapters this test concerns data type equivalence and
     * substitutability rather than is-a relationship.
     * @param returnType if true, special checking for function return type is done (vtable substitutability)
     */
    bool is_assignable_to( const TxActualType& destination, bool returnType=false ) const;


    /*--- type parameters and bindings ---*/

    /** Specialized lookup: searches the type hierarchy's parameter bindings to find the binding for a parameter. */
    const TxEntityDeclaration* lookup_param_binding( const TxEntityDeclaration* paramDecl ) const;
    const TxFieldDeclaration* lookup_value_param_binding( const std::string& fullParamName ) const;
    const TxTypeDeclaration* lookup_type_param_binding( const std::string& fullParamName ) const;

    /** Gets the (unbound) type parameters of this type (this type is a generic type if this is non-empty). */
    inline const std::vector<const TxEntityDeclaration*>& get_type_params() const {
        //return ( this->is_empty_derivation() ? this->get_base_type()->get_type_params() : this->params );
        return this->params;
    }

    /** Returns true if this type has an (unbound) type parameter with the specified (plain) name. */
    bool has_type_param( const std::string& plainParamName ) const {
        for ( auto & p : this->get_type_params() )
            if ( p->get_unique_name() == plainParamName )
                return true;
        return false;
    }

    /** Gets the declaration of an (unbound) type parameter of this type. (Note - this does not search ancestors' bound parameters.) */
    const TxEntityDeclaration* get_type_param_decl( const std::string& plainParamName ) const {
        for ( auto & paramDecl : this->get_type_params() ) {
            if ( plainParamName == paramDecl->get_unique_name() )
                return paramDecl;
        }
        return nullptr;
    }

    inline const std::vector<const TxEntityDeclaration*>& get_bindings() const {
        return this->bindings;
    }

    /** Gets the declaration of a type parameter binding of this type, or null if it doesn't exist.
     * (Note - this does not search ancestors' bound parameters.) */
    const TxEntityDeclaration* get_binding( const std::string& plainName ) const {
        for ( auto b : this->get_bindings() )
            if ( b->get_unique_name() == plainName
                 || ( b->get_unique_name()[0] == '$' && b->get_unique_name().compare( 1, std::string::npos, plainName ) == 0 ) )
                return b;
        return nullptr;
    }


    /*--- ArrayType and ReferenceType accessors ---*/

    /** Returns the element type if bound, or tx.Array.E generic type parameter if unbound.
     * This type must be an array. */
    TxQualType element_type() const;

    /** Returns nullptr if unbound.
     * This type must be an array. */
    const TxExpressionNode* capacity() const;


    /** Returns the target type if bound, or tx.Ref.T generic type parameter if unbound.
     * This type must be a reference. */
    TxQualType target_type() const;


    /*--- FunctionType accessors ---*/

    /** Returns true if functions of this function type may modify their closure. */
    virtual bool modifiable_closure() const;

    /** Helper method for getting the argument types of a function type. */
    virtual const std::vector<const TxActualType*>& argument_types() const;

    /** Gets the return type of this function type. If the function does not return a value, this is Void. */
    virtual const TxActualType* return_type() const;

    /** Returns false if this function type's return type is Void. */
    virtual bool has_return_value() const;

    /** Returns the var-arg element type of this function, or nullptr if this is not a var-arg function, otherwise null. */
    const TxActualType* vararg_elem_type() const;

    /** Returns the array argument type if this function takes a single argument that is an array type with statically known size,
     * otherwise null. */
    const TxActualType* fixed_array_arg_type() const;

    virtual std::string func_signature_str() const;


    /*--- data layout ---*/

    inline const DataTupleDefinition& get_instance_fields() const {
        return this->instanceFields;
    }

    inline const DataTupleDefinition& get_virtual_fields() const {
        return this->virtualFields;
    }

    inline const DataTupleDefinition& get_static_fields() const {
        return this->staticFields;
    }


    /*--- LLVM code generation methods, delegated to the type class handler ---*/

    llvm::StructType* make_vtable_type( LlvmGenerationContext& context ) const {
        return this->type_class_handler()->make_vtable_type( this, context );
    }

    /** Returns the llvm::Type for an instance of this type (possibly only an opaque struct declaration). */
    llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const {
        return this->type_class_handler()->make_llvm_type( this, context );
    }

    /** Returns the llvm::DIType for an instance of this type. */
    llvm::DIType* make_llvm_debug_type( LlvmGenerationContext& context ) const {
        return this->type_class_handler()->make_llvm_debug_type( this, context );
    }

    /** Invoked after make_llvm_type() to augment a possibly forward-declared llvm::Type "header" (named, opaque struct). */
    llvm::Type* make_llvm_type_body( LlvmGenerationContext& context, llvm::Type* header ) const {
        return this->type_class_handler()->make_llvm_type_body( this, context, header );
    }

    /** Invoked after make_llvm_debug_type() to augment a possibly forward-declared llvm::DIType "header" (named, opaque struct). */
    llvm::DIType* make_llvm_debug_type_body( LlvmGenerationContext& context, llvm::DIType* header ) const {
        return this->type_class_handler()->make_llvm_debug_type_body( this, context, header );
    }

    /** Returns the llvm::Type that an instance of this type is converted to/from when passed to/from an extern-c function. */
    llvm::Type* make_llvm_externc_type( LlvmGenerationContext& context ) const {
        return this->type_class_handler()->make_llvm_externc_type( this, context );
    }

    /** Code-generates initialization of bound VALUE parameters for an allocated instance. */
    void initialize_specialized_obj( LlvmGenerationContext& context, GenScope* scope, llvm::Value* objPtrV ) const {
        return this->type_class_handler()->initialize_specialized_obj( this, context, scope, objPtrV );
    }

    /** Generates the formal id of this type. */
    llvm::Constant* gen_typeid( LlvmGenerationContext& context ) const {
        return this->type_class_handler()->gen_typeid( this, context );
    }

    /** Generates the instance/element size of this type. Used by the runtime type info. Only valid for statically concrete types.
     * NOTE: For arrays this returns the instance size of their *element* type. */
    llvm::Constant* gen_static_element_size( LlvmGenerationContext& context ) const {
        return this->type_class_handler()->gen_static_element_size( this, context );
    }

    /** Generates the size of an instance of this type. Only valid for statically concrete types. */
    llvm::Constant* gen_static_size( LlvmGenerationContext& context ) const {
        return this->type_class_handler()->gen_static_size( this, context );
    }

    llvm::Value* gen_alloca( LlvmGenerationContext& context, GenScope* scope, unsigned alignment, const std::string &varName = "" ) const {
        return this->type_class_handler()->gen_alloca( this, context, scope, alignment, varName );
    }
    llvm::Value* gen_alloca( LlvmGenerationContext& context, GenScope* scope, const std::string &varName = "" ) const {
        return this->type_class_handler()->gen_alloca( this, context, scope, varName );
    }
    llvm::Value* gen_malloc( LlvmGenerationContext& context, GenScope* scope, const std::string &varName = "" ) const {
        return this->type_class_handler()->gen_malloc( this, context, scope, varName );
    }


    /*--- to string methods ---*/

    virtual std::string str() const override final;
    virtual std::string str( bool brief ) const final;

protected:
    virtual void self_string( std::stringstream& str, bool brief ) const;
};
