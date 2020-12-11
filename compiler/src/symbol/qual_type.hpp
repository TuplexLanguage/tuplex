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
    /** Initializes a null-valued (not set) instance. */
    TxQualType() : _mashed( 0 )  { }

    /** Initializes a properly set instance. */
    TxQualType( const TxActualType* type, bool modifiable = false )
            : _mashed( mashptr( type, modifiable ) ) {
        ASSERT( type, "NULL type" );
    }

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

    /** Returns true if this qual-type is set, false if not (then it is considered null-valued). */
    operator bool() const { return bool(_mashed); }

    /** Returns true if this type usage form is modifiable
     * (i.e. its instances' contents may be modified after initialization).
     * (The base type must be mutable for a modifiable usage form to be legal.)
     */
    inline bool is_modifiable() const {
        return this->inner_get_mod();
    }

    /** Returns the TxActualType of this TxQualType. */
    inline const TxActualType* type() const {
        return this->inner_get_type();
    }

    inline bool operator==( const TxQualType& other ) const {
        return ( this->_mashed == other._mashed );
    }

    inline bool operator!=( const TxQualType& other ) const {
        return !this->operator==( other );
    }

    std::string str( bool brief ) const;

    virtual std::string str() const override;
};
