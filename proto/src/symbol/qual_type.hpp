#pragma once

#include "type_class.hpp"

#include "util/printable.hpp"
#include "util/assert.hpp"

class TxActualType;

/** An instance of this class represents a type usage form, which adds qualifiers to the type.
 * The use of and implementation of this class is inspired by QualType in the clang source.
 * It mashes the qualifiers as bits with the type pointer for efficiency.
 *
 * In contrast with QualType, the type pointer may never be null, and TxQualType instances
 * are never modified after construction except via whole instance assignment.
 */
class TxQualType final : public Printable {
//    TxActualType const * const _type;
//    const bool _modifiable;
    uintptr_t _mashed;

    static uintptr_t mashptr( const TxActualType* type, bool modifiable ) {
        return ( reinterpret_cast<uintptr_t>( type ) | uintptr_t( modifiable ) );
    }

    inline const TxActualType* inner_get_type() const {
        return reinterpret_cast<const TxActualType *>(_mashed & (~(uintptr_t(1))));
    }

    inline bool inner_get_mod() const {
        return bool(_mashed & (uintptr_t(1)));
    }

public:
    TxQualType() : _mashed( 0 )  { }

    TxQualType( const TxActualType* type, bool modifiable = false )
            : _mashed( mashptr( type, modifiable ) ) {
        ASSERT( type, "NULL type" );
    }

//    TxQualType( const TxQualType* tu )
//            :  _type( tu->_type ), _modifiable( tu->_modifiable ) {
//    }

//    TxQualType& operator=( TxQualType&& other ) {
//        this->_mashed = other._mashed;
//        return *this;
//    }

    const TxActualType &operator*() const {
        return *this->inner_get_type();
    }
    const TxActualType *operator->() const {
        return this->inner_get_type();
    }

    operator bool() const { return bool(_mashed); }

    /** Returns true if this type usage form is _modifiable
     * (i.e. its instances' contents may be modified after initialization).
     * (The base type must be mutable for a _modifiable usage form to be legal.)
     */
    inline bool is_modifiable() const {
        return this->inner_get_mod();
    }

    inline const TxActualType* type() const {
        return this->inner_get_type();
    }

//    inline TxTypeClass get_type_class() const {
//        return this->_type->get_type_class();
//    }
//
//    inline uint32_t get_type_id() const {
//        return this->_type->get_runtime_type_id();
//    }

//    /** Performs as accurate equals as possible without forcing actualization of this type or that type.
//     * Since it doesn't force actualization it may produce false negatives; but no false positives.
//     */
//    bool shallow_equals( const TxQualType* that ) const;

    inline bool operator==( const TxQualType& other ) const {
        return ( this->_mashed == other._mashed );
    }

    inline bool operator!=( const TxQualType& other ) const {
        return !this->operator==( other );
    }

    std::string str( bool brief ) const;

    virtual std::string str() const override;
};
