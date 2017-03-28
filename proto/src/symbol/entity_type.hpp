#pragma once

#include <functional>

#include "entity.hpp"

#include "tx_logging.hpp"
#include "tx_lang_defs.hpp"

#include "type.hpp"



// Make TxType the type all components outside the type registry interact with.
// It encapsulates instancing of actual type, and rewiring while unifying equivalent specializations.


class TxType : public TxEntity {
    const TxTypeDefiningNode* definer;
    const std::function<const TxActualType*(void)> actualTypeProducer;

    mutable const TxActualType* _type = nullptr;
    mutable bool startedRslv = false;  // guard against recursive resolution
    mutable bool hasResolved = false;  // to prevent multiple identical error messages

    const TxActualType* define_type() const;

public:
    TxType( const TxActualType* actualType );

    TxType( const TxTypeDefiningNode* definer, std::function<const TxActualType*(void)> actualTypeProducer );


    virtual const TxTypeDeclaration* get_declaration() const override {
        return static_cast<const TxTypeDeclaration*>(TxEntity::get_declaration());
    }

    virtual const TxLocation& get_parse_location() const override {
        return ( this->definer ? this->definer->get_parse_location() : this->_type->get_parse_location() );
    }

    virtual ExpectedErrorClause* exp_err_ctx() const override {
        return ( this->definer ? this->definer->exp_err_ctx() : this->_type->exp_err_ctx() );
    }

    std::string str( bool brief ) const {
        if (this->_type)
            return this->_type->str( brief );
        else if (auto decl = this->get_declaration())
            return decl->get_unique_full_name();
        else
            return std::string("--no declaration--");
    }

    virtual std::string str() const override {
        return this->str( false );
    }

    const TxActualType* type() const;


    const TxTypeDeclaration* get_explicit_declaration() const { return this->type()->get_explicit_declaration(); }

    bool is_explicit_nongen_declaration() const { return this->type()->is_explicit_nongen_declaration(); }


    inline const TxType* get_base_type() const {
        if (auto base = this->type()->get_base_type())
            return base->get_type_entity();
        return nullptr;
    }

    inline const TxType* get_semantic_base_type() const {
        if (auto base = this->type()->get_semantic_base_type())
            return base->get_type_entity();
        return nullptr;
    }

    /** Gets the "instance base type" of this type, which is either this type, or the closest ancestor type
     * which defines a distinct instance data type.
     * This is used to bypass same-instance-type derivations (e.g. empty/mod. specializations). */
    const TxType* get_instance_base_type() const {
        if (auto base = this->type()->get_instance_base_type())
            return base->get_type_entity();
        return nullptr;
    }



    inline TxTypeClass get_type_class() const { return this->type()->get_type_class(); }

    inline uint32_t get_type_id() const { return this->type()->get_type_id(); }

    /** Returns true if this type is a built-in type. */
    inline bool is_builtin() const { return this->type()->is_builtin(); }

    /** Returns true if this type is the specified built-in type. */
    inline bool is_builtin( BuiltinTypeId biTypeId ) const { return this->type()->is_builtin( biTypeId ); }

    /** Returns true if this type is a scalar type. */
    inline bool is_scalar() const { return this->type()->is_scalar(); }


    /** Returns true if this type is modifiable (its instances' contents may be modified after initialization). */
    inline bool is_modifiable() const { return this->type()->is_modifiable(); }

    /** Returns true if this type is immutable (its instances' contents can never be modified after initialization). */
    virtual bool is_immutable() const { return this->type()->is_immutable(); }

    /** Returns true if this type cannot be extended. */
    virtual bool is_final() const { return this->type()->is_final(); }

    /** Returns true if this type is declared abstract. */
    virtual bool is_abstract() const { return this->type()->is_abstract(); };

    /** Returns true if this type is generic (i.e. has unbound type parameters). */
    inline bool is_generic() const { return this->type()->is_generic(); }

    /** Returns true if this type is concrete (i.e. can be directly instanced).
     * A concrete type is not abstract, nor usually generic.
     * References are always concrete (even if generic). */
    bool is_concrete() const { return this->type()->is_concrete(); }


    bool is_empty_derivation() const  { return this->type()->is_empty_derivation(); }



    virtual bool operator==(const TxType& other) const { return this->type()->operator==( *other.type() ); }

    inline bool operator!=(const TxType& other) const  { return ! this->operator==(other); }


    /** Returns true if an instance of this type can implicitly convert to an instance of the destination type.
     * Note that this does not test whether destination is modifiable;
     * it only tests type instance compatibility for assignment.
     * (For example, an initializer to an unmodifiable field is still valid if assignable to its type.)
     * This is a less strict test than is_assignable, since some types that are not directly assignable
     * may be so after an implicit conversion (e.g. Byte -> Int). */
    virtual bool auto_converts_to(const TxType& destination) const { return this->type()->auto_converts_to( *destination.type() ); }

    /** Returns true if an instance of this type can be assigned to a field of the provided type
     * (without performing any value conversion).
     * Note that this does not test whether destination is modifiable;
     * it only tests type instance compatibility for assignment.
     * (For example, an initializer to an unmodifiable field is still valid if assignable to its type.)
     * For many type classes this is a more strict test than is-a,
     * however for functions, arrays, references and adapters this test concerns data type equivalence and
     * substitutability rather than is-a relationship. */
    virtual bool is_assignable_to(const TxType& destination) const { return this->type()->is_assignable_to( *destination.type() ); }

    /** Returns true if the provided type is the same as this, or a specialization of this.
     * Note that true does not guarantee assignability, for example modifiability is not taken into account.
     */
    bool is_a(const TxType& other) const { return this->type()->is_a( *other.type() ); }


    /** Returns true if this type has the same vtable as its base type. */
    inline bool is_same_vtable_type() const { return this->type()->is_same_vtable_type(); }



    /** match against this entity's direct instance/static members (not its inherited members). */
    virtual TxEntitySymbol* get_instance_member(const std::string& name) const {
        return this->type()->get_instance_member( name );
    }

    virtual TxEntitySymbol* lookup_inherited_instance_member(const std::string& name) const {
        return this->type()->lookup_inherited_instance_member( name );
    }
    virtual TxEntitySymbol* lookup_inherited_instance_member( TxScopeSymbol* vantageScope, const std::string& name ) const {
        return this->type()->lookup_inherited_instance_member( vantageScope, name );
    }

    const TxEntityDeclaration* lookup_param_binding(const TxEntityDeclaration* paramDecl) const {
        return this->type()->lookup_param_binding( paramDecl );
    }



    /** Special case helper method for getting the target type of a reference type.
     * Returns null if this is not a reference type or if the target type was not resolved.
     */
    const TxType* target_type() const {
        if (this->get_type_class() == TXTC_REFERENCE)
            if (auto refType = static_cast<const TxReferenceType*>( this->type() ) )
                return refType->target_type()->get_type_entity();
        return nullptr;
    }


    /** Special case helper method for getting the element type of an array type.
     * Returns null if this is not an array type or if the element type was not resolved.
     */
    const TxType* element_type() const {
        if (this->get_type_class() == TXTC_ARRAY)
            if (auto aType = static_cast<const TxArrayType*>( this->type() ) )
                return aType->element_type()->get_type_entity();
        return nullptr;
    }


    /** Special case helper method for getting the argument types of a function type.
     * Returns an empty vector if this is not a function type or if an argument type was not resolved.
     */
    std::vector<const TxType*> argument_types() const {
        std::vector<const TxType*> args;
        if (this->get_type_class() == TXTC_FUNCTION) {
            if (auto funcType = static_cast<const TxFunctionType*>( this->type() ) ) {
                for ( auto a : funcType->argumentTypes ) {
                    args.push_back( a->get_type_entity() );
                }
            }
        }
        return args;
    }

    /** Special case helper method for getting the return type of a function type.
     * Returns null if this is not a function type or if the return type was not resolved.
     */
    const TxType* return_type() const {
        if (this->get_type_class() == TXTC_FUNCTION)
            if (auto funcType = static_cast<const TxFunctionType*>( this->type() ) )
                if (funcType->returnType)
                    return funcType->returnType->get_type_entity();
        return nullptr;
    }
};
