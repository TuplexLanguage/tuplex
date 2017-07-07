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

class TxActualType;
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
    void override_field( const std::string& name, const TxField* field ) {
        auto index = this->fieldMap.at( name );
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

/** An instance of this class represents a type definition.
 *
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
class TxActualType : public virtual TxParseOrigin, public Printable {
    static Logger& _LOG;

    /** The type class of this type. */
    const TxTypeClass typeClass;
    /** true if this is a built-in type */
    const bool builtin;

    /** The vtable id of this type, if it has a distinct vtable type (not an equivalent specialization).
     * Note, abstract types will have a vtable type id although they will not have an actual vtable entry in runtime. */
    uint32_t vtableId = UINT32_MAX;

    /** The formal type id of this type, if it is defined at compile time and a distinct type (not an equivalent specialization). */
    uint32_t formalTypeId = UINT32_MAX;

    /** If true, this type is mutable, in which case its instances may be declared modifiable. */
    const bool mutableType;

    const TxTypeDeclaration* declaration;

    /** Type parameters of this type. Should not be accessed directly, use type_params() accessor instead. */
    std::vector<const TxEntityDeclaration*> params;

    /** Bindings of the base type's type parameters. Should not be accessed directly, use type_bindings() accessor instead. */
    std::vector<const TxEntityDeclaration*> bindings;

    TxActualType const * const baseType;
    const std::vector<const TxActualType*> interfaces;

    /** Set for non-generic specializations of a generic base type.
     * This also implies a pure specialization, even if extendsInstanceDatatype technically is true. */
    const TxActualType* genericBaseType = nullptr;

    /** False unless there are either TYPE bindings for parameters with other than Ref constraint, or VALUE bindings.
     * If this is a Ref type, this is always false (regardless of the Ref's bound target type). */
    bool nonRefBindings = false;

    /** true when initialize_type() has completed its initializations */
    bool hasInitialized = false;

    /** true after prepare_members() has started - guards against recursive data types */
    bool startedPrepare = false;
    /** false until prepare_members() has completed (after that this object should be considered immutable) */
    bool hasPrepared = false;

    /** Initializes this type. Called from constructor. */
    void initialize_type();
    /** Performs basic validation, called from initializer. */
    void validate_type() const;

    inline const TxTypeDeclaration* get_explicit_declaration() const {
        return ( ( this->declaration->get_decl_flags() & TXD_IMPLICIT ) ? nullptr : this->declaration );
    }

    /** Returns the common base type of the types, if both are pure specializations of it. */
    static const TxActualType* common_generic_base_type( const TxActualType* thisType, const TxActualType* otherType );

    static bool inner_is_a( const TxActualType* thisType, const TxActualType* otherType );

    /** Returns true if this is a generic type or a direct specialization of a generic type.
     * Note - does not bypass empty/modifiable derivations. */
    inline bool is_gen_or_spec() const {
        return ( !this->params.empty() || !this->bindings.empty() );
    }

    /** used to prevent infinite recursion when compiler is processing erroneously defined recursive types */
    mutable bool recursionGuard = false;
    friend class ScopedRecursionGuardClause;

    static inline bool determine_builtin( const TxTypeDeclaration* declaration, const TxActualType* baseType) {
        return ( declaration->get_decl_flags() & TXD_BUILTIN );
    }

protected:
    bool extendsInstanceDatatype = false;
    bool modifiesVTable = false;
    bool emptyDerivation = false;
    bool pureDerivation = false;
    // data layout:
    DataTupleDefinition staticFields;
    DataTupleDefinition virtualFields;
    DataTupleDefinition instanceFields;

    /** Only to be used for Any and Void types. */
    TxActualType( TxTypeClass typeClass, const TxTypeDeclaration* declaration, bool mutableType )
            : typeClass( typeClass ), builtin( declaration->get_decl_flags() & TXD_BUILTIN ), mutableType( mutableType ),
              declaration( declaration ), baseType(), interfaces() {
        this->initialize_type();
    }

    TxActualType( TxTypeClass typeClass, const TxTypeDeclaration* declaration, const TxActualType* baseType, bool mutableType,
                  const std::vector<const TxActualType*>& interfaces = std::vector<const TxActualType*>() )
            : typeClass( typeClass ), builtin( determine_builtin( declaration, baseType ) ), mutableType( mutableType ),
              declaration( declaration ), baseType( baseType ), interfaces( interfaces ) {
        this->initialize_type();
    }

    /** Creates a specialization of this type. To be used by the type registry. */
    virtual TxActualType* make_specialized_type( const TxTypeDeclaration* declaration,
                                                 const TxActualType* baseType,  // (contains redundant ref to this obj...)
                                                 bool mutableType = true, const std::vector<const TxActualType*>& interfaces = {} ) const = 0;
    // FUTURE: refactor how TxActualType objects are created and remove this method

    friend class TypeRegistry;  // allows access for registry's type construction
    friend class BuiltinTypes;
    friend class TxBuiltinTypeDefiningNode;

    /** Gets the Any root type. */
    const TxQualType* get_root_any_qtype() const;

    /** Prepares this type's members, including data layout. Called after resolution phase has completed.
     * @return true if a data type recursion has been discovered */
    virtual bool inner_prepare_members();

    /** Overriding this allows a type class to allow structurally equal types to be assignable.
     * The default implementation simply returns false.
     * This method implementation can assume it will only be called with dest types of the same type class. */
    virtual bool inner_is_assignable_to( const TxActualType* dest ) const;

public:
    inline Logger* LOGGER() const {
        return &this->_LOG;
    }

    virtual ~TxActualType() = default;

    virtual const TxNode* get_origin_node() const override;

    inline bool is_prepared() const {
        return this->hasPrepared;
    }

    /** Prepares this type's members, including data layout. Called after resolution phase has completed.
     * Also checks against recursive data types (a compilation error is expected to be generated by the caller).
     * @return true if a data type recursion has been discovered */
    virtual bool prepare_members();

//    inline const TxType* get_type_entity() const {
//        return this->get_declaration()->get_definer()->get_type()->type();
//    }

    inline const TxTypeDeclaration* get_declaration() const {
        return this->declaration;
    }

    /** Gets the vtable id of this type. (Equivalent specializations return their base type's vtable id.)
     * For non-built-in types, this is only valid to call after the type preparation phase. */
    inline uint32_t get_vtable_id() const {
        //ASSERT(this->prepared, "Can't get formal type id of unprepared type: " << this);
        ASSERT( this->vtableId != UINT32_MAX || this->is_equivalent_derivation(), "VTable id not set for " << this );
        return ( this->vtableId == UINT32_MAX ? this->get_semantic_base_type()->get_vtable_id() : this->vtableId );
    }

    /** Returns true if this type instance has a formal type id assigned.
     * If called before the type preparation phase, only built-in types return true. */
    inline bool has_vtable_id() const {
        return ( this->vtableId != UINT32_MAX );
    }

    /** Gets the formal type id of this type. (Equivalent specializations return their base type's id.)
     * For non-built-in types, this is only valid to call after the type preparation phase. */
    inline uint32_t get_formal_type_id() const {
        //ASSERT(this->prepared, "Can't get formal type id of unprepared type: " << this);
        ASSERT( this->formalTypeId != UINT32_MAX || this->is_equivalent_derivation(), "Type id not set for " << this );
        return ( this->formalTypeId == UINT32_MAX ? this->get_semantic_base_type()->get_formal_type_id() : this->formalTypeId );
    }

    /** Returns true if this type instance has a formal type id assigned.
     * If called before the type preparation phase, only built-in types return true. */
    inline bool has_formal_type_id() const {
        return ( this->formalTypeId != UINT32_MAX );
    }

    /*--- characteristics ---*/

    /** Returns true if this type has a base type (parent). ('Any' is the only type that has no base type.) */
    inline bool has_base_type() const {
        return this->baseType;
    }

    /** Gets the base type (parent) of this type.
     * ('Any' is the only type that has no base type, in which case null is returned.) */
    inline const TxActualType* get_base_type() const {
        return this->baseType;
    }

    /** Gets the 'semantic' base type (parent) of this type,
     * which is the same as get_base_type() except for generic type specializations
     * in which case the generic base type is returned (instead of the implicitly generated specialization thereof). */
    inline const TxActualType* get_semantic_base_type() const {
        ASSERT( this->hasInitialized, "Can't get semantic base type of uninitized type " << this );
        return this->genericBaseType ? this->genericBaseType : this->baseType;
    }

    /** Gets the "instance base type" of this type, which is either this type, or the closest ancestor type
     * which defines a distinct instance data type.
     * This is used to bypass same-instance-type derivations (e.g. empty/mod. specializations). */
    const TxActualType* get_instance_base_type() const;

    /** Returns the type class this type belongs to. */
    inline TxTypeClass get_type_class() const {
        return this->typeClass;
    }

    /** Returns true if this type is a built-in type. */
    inline bool is_builtin() const {
        return this->builtin;
    }

    /** Returns true if this type is the specified built-in type. */
    inline bool is_builtin( BuiltinTypeId biTypeId ) const {
        return ( this->formalTypeId == (uint32_t) biTypeId );  // Note: type ids of built-in types are always set
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
    virtual bool is_abstract() const {
        return ( this->get_declaration()->get_decl_flags() & TXD_ABSTRACT );
    }

    /** Returns true if this type is concrete.
     * A concrete type is not declared abstract, nor is it dependent on any non-reference-constrained unbound type parameter.
     * This includes both unbound type parameters of this type and of its outer lexical scope, if any.
     * Reference types are always concrete.
     */
    virtual bool is_concrete() const;

    /** Returns true if this type is static, which means it is concrete and non-dynamic.
     * A static type can be directly instanced since its data type and size is fully known at compile time.
     * Reference types are always static.
     */
    virtual bool is_static() const;

    /** Returns true if this type is dynamic, which means it is concrete and dependent on one or more
     * VALUE type parameter bindings with a dynamic value (not known at compile time).
     * The size of a dynamic type may not be known at compile time, but it is instantiable
     * via an expression that computes the dynamic parameter bindings.
     */
    virtual bool is_dynamic() const;

    /** Returns true if this type is generic (i.e. has unbound type parameters).
     * Note that a non-generic type may still have members that refer to unbound type parameters of an outer scope.
     * @see is_generic_dependent()
     */
    inline bool is_generic() const {
        return !this->get_type_params().empty();
    }

    /** Returns true if this type is dependent on any unbound non-reference generic type parameters;
     * except for references which always yield false whether generic or not.
     * This is true if this type has unbound non-reference type parameters, or is defined within the scope
     * of another generic type (i.e. which has unbound non-reference type parameters).
     */
    bool is_generic_dependent() const;

    /** Returns true if this type is a generic type parameter.
     * (Note: Generic type bindings are not distinct type instances, they are aliases.)
     */
    inline bool is_generic_param() const {
        return ( this->get_declaration()->get_decl_flags() & TXD_GENPARAM );
    }

    /** Returns true if this type has the same vtable as its base type. */
    inline bool is_same_vtable_type() const {
        ASSERT( this->hasInitialized, "Can't determine same vtable type of uninitialized type " << this );
        return this->has_base_type() && !this->modifiesVTable;
    }

    /** Returns true if this type has the same instance data type as its base type. */
    inline bool is_same_instance_type() const {
        ASSERT( this->hasInitialized, "Can't determine same instance type of uninitialized type " << this );
        return this->has_base_type() && !this->extendsInstanceDatatype;
    }

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

    /** Returns true if this type is a virtual derivation of a base type,
     * i.e. is effectively the same *instance data type* as the base type.
     * Added instance fields and bound non-ref type parameters cause this to return false;
     * modifiability, bound ref-constrained type parameters, added interfaces, added virtual members do not.
     * (Returns false for Any which has no base type.)
     */
    bool is_virtual_derivation() const;

    /** Returns true if this type is a specialization of a generic base type. */
    inline bool is_generic_specialization() const { return this->genericBaseType; }


    // FUTURE: Should we remove the == != operator overloads in favor of more specifically named comparison methods?

    /** Returns true iff the two types are equal in the Tuplex language definition sense.
     * Note that named types are non-equal if not same name. */
    virtual bool operator==( const TxActualType& other ) const;

    /** Returns true iff the two types are unequal in the Tuplex language definition sense. */
    inline bool operator!=( const TxActualType& other ) const {
        return !this->operator==( other );
    }

    /** Returns true if an instance of this type can implicitly convert to an instance of the destination type.
     * This may be less strict test than is_assignable, since some types that are not directly assignable
     * may be so after an implicit conversion (e.g. Byte -> Int). */
    virtual bool auto_converts_to( const TxActualType& destination ) const {
        return this->is_assignable_to( destination );  // default implementation is equal to assignability
    }

    /** Returns true if an instance of this type can be assigned to a field of the provided type
     * (without performing any value conversion).
     * Note that this does not test whether destination is modifiable;
     * it only tests type instance compatibility for assignment.
     * (For example, an initializer to an unmodifiable field is still valid if assignable to its type.)
     * For many type classes this is a more strict test than is-a,
     * however for functions, arrays, references and adapters this test concerns data type equivalence and
     * substitutability rather than is-a relationship. */
    bool is_assignable_to( const TxActualType& destination ) const;

    /** Returns true if the provided type is the same as this, or a specialization of this.
     * Note that true does not guarantee assignability, for example modifiability is not taken into account.
     */
    bool is_a( const TxActualType& other ) const;

private:
    inline bool inner_equals( const TxActualType* thatType ) const {
        return this == thatType;
    }

    bool derives_object( const TxActualType* objectType ) const;
    bool derives_interface( const TxActualType* interfaceType ) const;

public:
    /*--- namespace lookup ---*/

    /** match against this entity's direct instance/static members (not its inherited members). */
    virtual TxEntitySymbol* get_instance_member( const std::string& name ) const;
    virtual TxEntitySymbol* get_instance_member( TxScopeSymbol* vantageScope, const std::string& name ) const;

    /** match against this entity's direct instance/static members, and then its inherited members, returning the first found */
    virtual TxEntitySymbol* lookup_inherited_instance_member( const std::string& name ) const;
    virtual TxEntitySymbol* lookup_inherited_instance_member( TxScopeSymbol* vantageScope, const std::string& name ) const;

    /** Specialized lookup: searches the type hierarchy's parameter bindings to find the binding for a parameter. */
    const TxEntityDeclaration* lookup_param_binding( const TxEntityDeclaration* paramDecl ) const;
    const TxFieldDeclaration* lookup_value_param_binding( const std::string& fullParamName ) const;
    const TxTypeDeclaration* lookup_type_param_binding( const std::string& fullParamName ) const;

    /*--- type parameter handling ---*/

    /** Gets the (unbound) type parameters of this type (this type is a generic type if this is non-empty). */
    inline const std::vector<const TxEntityDeclaration*>& get_type_params() const {
        ASSERT( this->hasInitialized, "Can't get type params of uninitized type " << this );
        return ( this->is_empty_derivation() ? this->get_base_type()->get_type_params() : this->params );
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
        ASSERT( this->hasInitialized, "Can't get bindings of uninitized type " << this );
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

    /*--- LLVM code generation methods ---*/

    virtual llvm::StructType* make_vtable_type( LlvmGenerationContext& context ) const;
    virtual llvm::Function* get_type_user_init_func( LlvmGenerationContext& context ) const;

    /** Returns the llvm::Type for an instance of this type (possibly only an opaque struct declaration). */
    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const = 0;

    /** Invoked after make_llvm_type() to augment a possibly forward-declared llvm::Type "header" (named, opaque struct).
     * Default implementation returns the "header" type without modifying it;
     * types that actually predefine an opaque header should override and augment the type or return a new, full type. */
    virtual llvm::Type* make_llvm_type_body( LlvmGenerationContext& context, llvm::Type* header ) const {
        return header;
    }

    /** Returns the llvm::Type that an instance of this type is converted to/from when passed to/from an extern-c function. */
    virtual llvm::Type* make_llvm_externc_type( LlvmGenerationContext& context ) const;


    virtual llvm::Value* gen_size( LlvmGenerationContext& context, GenScope* scope ) const;
    virtual llvm::Value* gen_alloca( LlvmGenerationContext& context, GenScope* scope, const std::string &varName = "" ) const;
    virtual llvm::Value* gen_malloc( LlvmGenerationContext& context, GenScope* scope, const std::string &varName = "" ) const;
    virtual llvm::Constant* gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const;

    /*--- to string methods ---*/

    virtual std::string str() const override final;
    virtual std::string str( bool brief ) const final;

protected:
    virtual void self_string( std::stringstream& str, bool brief ) const;
};
