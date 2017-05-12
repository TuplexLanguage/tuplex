#pragma once

#include <functional>

#include "entity.hpp"

#include "tx_logging.hpp"
#include "tx_lang_defs.hpp"

#include "type.hpp"

/** A type entity. Represents the types defined in the program.
 * This acts as a proxy for the 'actual' type, whose definition is deferred until its needed in order
 * to facilitate type resolution without causing infinite recursions.
 * Several TxType instances may ultimately refer to the same 'actual' type.
 *
 * Note: Type entities might not have a declaration. Their underlying actual type will always have a declaration though.
 */
class TxType : public TxEntity {
    TxTypeDefiningNode* definer;
    const std::function<const TxActualType*( void )> actualTypeProducer;

    mutable const TxActualType* _type = nullptr;
    mutable bool startedRslv = false;  // guard against recursive resolution
    mutable bool hasResolved = false;  // to prevent multiple identical error messages

    const TxActualType* define_type() const;

    // only TypeRegistry may create instances:
    friend class TypeRegistry;

    TxType( const TxActualType* actualType );

    TxType( TxTypeDefiningNode* definer, std::function<const TxActualType*( void )> actualTypeProducer );

public:

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

    inline TxTypeDefiningNode* get_definer() const {
        return this->definer;
    }

    // TODO: remove explicit usages of this method from AST (declaration & resolution passes)
    // TODO: make non-const?
    const TxActualType* type() const;

    inline const TxType* get_base_type() const {
        if ( auto base = this->type()->get_base_type() )
            return base->get_type_entity();
        return nullptr;
    }

    inline const TxType* get_semantic_base_type() const {
        if ( auto base = this->type()->get_semantic_base_type() )
            return base->get_type_entity();
        return nullptr;
    }

    /** Gets the "instance base type" of this type, which is either this type, or the closest ancestor type
     * which defines a distinct instance data type.
     * This is used to bypass same-instance-type derivations (e.g. empty/mod. specializations). */
    const TxType* get_instance_base_type() const {
        if ( auto base = this->type()->get_instance_base_type() )
            return base->get_type_entity();
        return nullptr;
    }

    inline TxTypeClass get_type_class() const {
        return this->type()->get_type_class();
    }

    inline uint32_t get_type_id() const {
        return this->type()->get_type_id();
    }

    /** Returns true if this type is a built-in type. */
    inline bool is_builtin() const {
        return this->type()->is_builtin();
    }

    /** Returns true if this type is the specified built-in type. */
    inline bool is_builtin( BuiltinTypeId biTypeId ) const {
        return this->type()->is_builtin( biTypeId );
    }

    /** Returns true if this type is a scalar type. */
    inline bool is_scalar() const {
        return this->type()->is_scalar();
    }

    /** Returns true if this type is modifiable (its instances' contents may be modified after initialization). */
    inline bool is_modifiable() const {
        return this->type()->is_modifiable();
    }

    /** Returns true if this type is immutable (its instances' contents can never be modified after initialization). */
    inline bool is_immutable() const {
        return this->type()->is_immutable();
    }

    /** Returns true if this type cannot be extended. */
    inline bool is_final() const {
        return this->type()->is_final();
    }

    /** Returns true if this type is declared abstract. */
    inline bool is_abstract() const {
        return this->type()->is_abstract();
    }
    ;

    /** Returns true if this type is generic (i.e. has unbound type parameters). */
    inline bool is_generic() const {
        return this->type()->is_generic();
    }

    /** Returns true if this type is concrete (i.e. can be directly instanced).
     * A concrete type is not abstract, nor usually generic.
     * References are always concrete (even if generic). */
    inline bool is_concrete() const {
        return this->type()->is_concrete();
    }

    inline bool is_empty_derivation() const {
        return this->type()->is_empty_derivation();
    }

    inline bool operator==( const TxType& other ) const {
        return this->type()->operator==( *other.type() );
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
        return this->type()->auto_converts_to( *destination.type() );
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
        return this->type()->is_assignable_to( *destination.type() );
    }

    /** Returns true if the provided type is the same as this, or a specialization of this.
     * Note that true does not guarantee assignability, for example modifiability is not taken into account.
     */
    inline bool is_a( const TxType& other ) const {
        return this->type()->is_a( *other.type() );
    }

    /** Returns true if this type has the same vtable as its base type. */
    inline bool is_same_vtable_type() const {
        return this->type()->is_same_vtable_type();
    }

    /** match against this entity's direct instance/static members (not its inherited members). */
    inline TxEntitySymbol* get_instance_member( const std::string& name ) const {
        return this->type()->get_instance_member( name );
    }

    inline TxEntitySymbol* lookup_inherited_instance_member( const std::string& name ) const {
        return this->type()->lookup_inherited_instance_member( name );
    }
    inline TxEntitySymbol* lookup_inherited_instance_member( TxScopeSymbol* vantageScope, const std::string& name ) const {
        return this->type()->lookup_inherited_instance_member( vantageScope, name );
    }

    inline const TxEntityDeclaration* lookup_param_binding( const TxEntityDeclaration* paramDecl ) const {
        return this->type()->lookup_param_binding( paramDecl );
    }

    /** Special case helper method for getting the target type of a reference type.
     */
    const TxType* target_type() const {
        if ( this->get_type_class() == TXTC_REFERENCE ) {
            auto refType = static_cast<const TxReferenceType*>( this->type() );
            return refType->target_type()->get_type_entity();
        }
        THROW_LOGIC( "Can't get target_type() of non-reference type: " << this );
    }

    /** Special case helper method for getting the element type of an array type.
     */
    const TxType* element_type() const {
        if ( this->get_type_class() == TXTC_ARRAY ) {
            auto aType = static_cast<const TxArrayType*>( this->type() );
            return aType->element_type()->get_type_entity();
        }
        THROW_LOGIC( "Can't get element_type() of non-array type: " << this );
    }

    /** Special case helper method for getting the argument types of a function type.
     */
    std::vector<const TxType*> argument_types() const {
        if ( this->get_type_class() == TXTC_FUNCTION ) {
            std::vector<const TxType*> args;
            auto funcType = static_cast<const TxFunctionType*>( this->type() );
            for ( auto a : funcType->argumentTypes ) {
                args.push_back( a->get_type_entity() );
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
            auto funcType = static_cast<const TxFunctionType*>( this->type() );
            if ( auto vaElemType = funcType->vararg_elem_type() )
                return vaElemType->get_type_entity();
            return nullptr;
        }
        THROW_LOGIC( "Can't get vararg_elem_type() of non-function type: " << this );
    }

    /** Special case helper method for getting the array arg type of a fixed array arg function type.
     * Returns null if this is not a fixed array arg function.
     */
    const TxType* fixed_array_arg_type() const {
        if ( this->get_type_class() == TXTC_FUNCTION ) {
            auto funcType = static_cast<const TxFunctionType*>( this->type() );
            if ( auto arrayArgType = funcType->fixed_array_arg_type() )
                return arrayArgType->get_type_entity();
            return nullptr;
        }
        THROW_LOGIC( "Can't get fixed_array_arg_type() of non-function type: " << this );
    }

    /** Special case helper method for getting the return type of a function type.
     * Returns null if the return type was not resolved.
     */
    const TxType* return_type() const {
        if ( this->get_type_class() == TXTC_FUNCTION ) {
            auto funcType = static_cast<const TxFunctionType*>( this->type() );
            return funcType->returnType->get_type_entity();
        }
        THROW_LOGIC( "Can't get return_type() of non-function type: " << this );
    }
};
