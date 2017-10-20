#pragma once

#include <functional>

#include "entity.hpp"

#include "tx_logging.hpp"
#include "tx_lang_defs.hpp"
#include "tx_error.hpp"

#include "type.hpp"

class TxQualType;

class TxType;
const TxType* get_type_entity( const TxActualType* actType );

/** A type entity. Represents the types defined in the program.
 * This acts as a proxy for the 'actual' type, whose definition is deferred until its needed in order
 * to facilitate type resolution without causing infinite recursions.
 * Several TxType instances may ultimately refer to the same 'actual' type.
 *
 * Note: Type entities might not have a declaration. Their underlying actual type will always have a declaration though.
 */
class TxType : public TxEntity {
    TxTypeResolvingNode* definer;
    const std::function<const TxActualType*( void )> actualTypeProducer;

    mutable const TxActualType* _type = nullptr;
    mutable bool startedRslv = false;  // guard against recursive resolution
    mutable bool hasResolved = false;  // to prevent multiple identical error messages

    TxQualType define_type( TxPassInfo passInfo ) const;

    // only these may create instances:
    friend class TypeRegistry;
    friend class TxBuiltinTypeDefiningNode;

//    TxType( const TxActualType* actualType );

    TxType( TxTypeResolvingNode* definer, std::function<const TxActualType*( void )> actualTypeProducer );

public:
    TxType( const TxActualType* actualType );

    virtual const TxTypeDeclaration* get_declaration() const override;

    virtual const TxNode* get_origin_node() const override {
        return this->definer;
    }

    std::string str( bool brief ) const {
        if ( this->_type )
            return this->_type->str( brief );
        else if ( auto decl = this->get_declaration() )
            return decl->get_unique_full_name();
        else
            return std::string( "--no declaration--" );
    }

    virtual std::string str() const override {
        return this->str( true );
    }

    inline TxTypeResolvingNode* get_definer() const {
        return this->definer;
    }

    /** Performs as accurate equals as possible without forcing actualization of this type or that type.
     * Since it doesn't force actualization it may produce false negatives; but no false positives.
     */
    bool shallow_equals( const TxType* that ) const {
        if (this == that)
            return true;
        if ( this->_type && that->_type )
            return this->_type == that->_type;
        if ( auto thisDecl = this->TxEntity::get_declaration() ) {
            if ( auto thatDecl = that->TxEntity::get_declaration() ) {
                bool eq = ( thisDecl == thatDecl );
                return eq;
            }
        }
        return false;
    }

    bool is_actualized() const {
        return bool( this->_type );
    }

    // FUTURE: remove explicit usages of this method from AST (declaration & resolution passes)?
    // TODO: make non-const?
    const TxActualType* acttype() const;

    inline const TxActualType* attempt_acttype() const {
        return this->_type;
    }

    inline const TxType* get_base_type() const {
        if ( auto base = this->acttype()->get_base_type() )
            return get_type_entity(base);
        return nullptr;
    }

    inline const TxType* get_semantic_base_type() const {
        if ( auto base = this->acttype()->get_semantic_base_type() )
            return get_type_entity(base);
        return nullptr;
    }

    inline const TxType* get_source_base_type() const {
        if ( auto base = this->acttype()->get_source_base_type() )
            return get_type_entity(base);
        return nullptr;
    }

//    /** Gets the "instance base type" of this type, which is either this type, or the closest ancestor type
//     * which defines a distinct instance data type.
//     * This is used to bypass same-instance-type derivations (e.g. empty/mod. specializations). */
//    const TxType* get_instance_base_type() const {
//        if ( auto base = this->acttype()->get_instance_base_type() )
//            return get_type_entity(base);
//        return nullptr;
//    }

    inline TxTypeClass get_type_class() const {
        return this->acttype()->get_type_class();
    }

    inline uint32_t get_runtime_type_id() const {
        return this->acttype()->get_runtime_type_id();
    }

    /** Returns true if this type is a built-in type. */
    inline bool is_builtin() const {
        return this->acttype()->is_builtin();
    }

    /** Returns true if this type is the specified built-in type. */
    inline bool is_builtin( BuiltinTypeId biTypeId ) const {
        return this->acttype()->is_builtin( biTypeId );
    }

    /** Returns true if this type is a scalar type. */
    inline bool is_scalar() const {
        return this->acttype()->is_scalar();
    }

    /** Returns true if this type is mutable by declaration.
     * If true its instances can be declared modifiable.
     */
    inline bool is_mutable() const {
        return this->acttype()->is_mutable();
    }

    /** Returns true if this type cannot be extended. */
    inline bool is_final() const {
        return this->acttype()->is_final();
    }

    /** Returns true if this type is declared abstract. */
    inline bool is_abstract() const {
        return this->acttype()->is_abstract();
    }
    ;

    /** Returns true if this type is generic (i.e. has unbound type parameters). */
    inline bool is_generic() const {
        return this->acttype()->is_generic();
    }

    /** Returns true if this type is type-generic (i.e. has unbound TYPE type parameters). */
    inline bool is_type_generic() const {
        return this->acttype()->is_type_generic();
    }

    /** Returns true if this type is a generic type parameter. */
    inline bool is_generic_param() const {
        return this->acttype()->is_generic_param();
    }

    /** Returns true if this type is concrete.
     * A concrete type is not declared abstract, nor is it dependent on any non-reference-constrained unbound type parameter.
     * This includes both unbound type parameters of this type and of its outer lexical scope, if any.
     * Reference types are always concrete.
     */
    inline bool is_concrete() const {
        return this->acttype()->is_concrete();
    }

    /** Returns true if this type is static, which means it is concrete and non-dynamic.
     * A static type can be directly instanced since its data type and size is fully known at compile time.
     * Reference types are always static.
     */
    inline bool is_static() const {
        return this->acttype()->is_static();
    }

    /** Returns true if this type is dynamic, which means it is concrete and dependent on one or more
     * VALUE type parameter bindings with a dynamic value (not known at compile time).
     * The size of a dynamic type may not be known at compile time, but it is instantiable
     * via an expression that computes the dynamic parameter bindings.
     */
    inline bool is_dynamic() const {
        return this->acttype()->is_dynamic();
    }

    inline bool is_empty_derivation() const {
        return this->acttype()->is_empty_derivation();
    }

    inline bool is_generic_specialization() const {
        return this->acttype()->is_generic_specialization();
    }

    inline bool is_pure_value_specialization() const {
        return this->acttype()->is_pure_value_specialization();
    }

    inline bool operator==( const TxType& other ) const {
        return this->acttype()->operator==( *other.acttype() );
    }

    inline bool operator!=( const TxType& other ) const {
        return !this->operator==( other );
    }

    /** Returns true if an instance of this type can implicitly convert to an instance of the destination type.
     * Note that this does not test whether destination is modifiable;
     * it only tests type instance compatibility for assignment.
     * (For example, an initializer to an unmodifiable field is still valid if assignable to its type.)
     * This is a less strict test than is_assignable, since some types that are not directly assignable
     * may be so after an implicit conversion (e.g. Byte -> Int). */
    inline bool auto_converts_to( const TxType& destination ) const {
        return this->acttype()->auto_converts_to( *destination.acttype() );
    }

    /** Returns true if an instance of this type can be assigned to a field of the provided type
     * (without performing any value conversion).
     * Note that this does not test whether destination is modifiable;
     * it only tests type instance compatibility for assignment.
     * (For example, an initializer to an unmodifiable field is still valid if assignable to its type.)
     * For many type classes this is a more strict test than is-a,
     * however for functions, arrays, references and adapters this test concerns data type equivalence and
     * substitutability rather than is-a relationship. */
    inline bool is_assignable_to( const TxType& destination ) const {
        return this->acttype()->is_assignable_to( *destination.acttype() );
    }

    /** Returns true if the provided type is the same as this, or a specialization of this.
     * Note that true does not guarantee assignability, for example modifiability is not taken into account.
     */
    inline bool is_a( const TxType& other ) const {
        return this->acttype()->is_a( *other.acttype() );
    }

    /** Returns true if this type has the same vtable as its base type. */
    inline bool is_same_vtable_type() const {
        return this->acttype()->is_same_vtable_type();
    }

    inline const TxEntityDeclaration* lookup_param_binding( const TxEntityDeclaration* paramDecl ) const {
        return this->acttype()->lookup_param_binding( paramDecl );
    }

    /** Special case helper method for getting the target type of a reference type.
     */
    TxQualType target_type() const {
        if ( this->get_type_class() == TXTC_REFERENCE ) {
            auto refType = this->acttype();
            return refType->target_type();
        }
        THROW_LOGIC( "Can't get target_type() of non-reference type: " << this );
    }

    /** Special case helper method for getting the element type of an array type.
     */
    TxQualType element_type() const {
        if ( this->get_type_class() == TXTC_ARRAY ) {
            auto aType = this->acttype();
            return aType->element_type();
        }
        THROW_LOGIC( "Can't get element_type() of non-array type: " << this );
    }

    /** Special case helper method for getting the argument types of a function type.
     */
    std::vector<const TxType*> argument_types() const {
        if ( this->get_type_class() == TXTC_FUNCTION ) {
            std::vector<const TxType*> args;
            auto funcType = static_cast<const TxFunctionType*>( this->acttype() );
            for ( auto a : funcType->argument_types() ) {
                args.push_back( get_type_entity(a) );
            }
            return args;
        }
        THROW_LOGIC( "Can't get argument_types() of non-function type: " << this );
    }

    /** Special case helper method for getting the var-arg element type of a var-arg function type.
     * Returns null if this is not a var-arg function.
     */
    const TxType* vararg_elem_type() const {
        if ( this->get_type_class() == TXTC_FUNCTION ) {
            auto funcType = static_cast<const TxFunctionType*>( this->acttype() );
            if ( auto vaElemType = funcType->vararg_elem_type() )
                return get_type_entity( vaElemType );
            return nullptr;
        }
        THROW_LOGIC( "Can't get vararg_elem_type() of non-function type: " << this );
    }

    /** Special case helper method for getting the array arg type of a fixed array arg function type.
     * Returns null if this is not a fixed array arg function.
     */
    const TxType* fixed_array_arg_type() const {
        if ( this->get_type_class() == TXTC_FUNCTION ) {
            auto funcType = static_cast<const TxFunctionType*>( this->acttype() );
            if ( auto arrayArgType = funcType->fixed_array_arg_type() )
                return get_type_entity( arrayArgType );
            return nullptr;
        }
        THROW_LOGIC( "Can't get fixed_array_arg_type() of non-function type: " << this );
    }

    /** Special case helper method for getting the return type of a function type.
     * Returns null if the return type was not resolved.
     */
    const TxType* return_type() const {
        if ( this->get_type_class() == TXTC_FUNCTION ) {
            auto funcType = static_cast<const TxFunctionType*>( this->acttype() );
            return get_type_entity( funcType->return_type() );
        }
        THROW_LOGIC( "Can't get return_type() of non-function type: " << this );
    }
};
