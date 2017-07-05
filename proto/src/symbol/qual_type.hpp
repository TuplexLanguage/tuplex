#pragma once

#include "type_class.hpp"
#include "entity_type.hpp"

/** An instance of this class represents a type usage form, which adds qualifiers to the type.
 */
class TxQualType : public Printable {
    TxType const * const _type;
    const bool modifiable;

public:
    TxQualType( const TxType* type, bool modifiable = false )
            : _type( type ), modifiable( modifiable ) {
    }

    TxQualType( const TxQualType* tu ) : TxQualType( tu->_type, tu->modifiable ) { }

    /** Returns true if this type usage form is modifiable
     * (i.e. its instances' contents may be modified after initialization).
     * (The base type must be mutable for a modifiable usage form to be legal.)
     */
    inline bool is_modifiable() const {
        return this->modifiable;
    }

    inline const TxType* type() const {
        return this->_type;
    }

    inline TxTypeClass get_type_class() const {
        return this->type()->get_type_class();
    }

    inline uint32_t get_type_id() const {
        return this->type()->get_formal_type_id();
    }

    /** Performs as accurate equals as possible without forcing actualization of this type or that type.
     * Since it doesn't force actualization it may produce false negatives; but no false positives.
     */
    bool shallow_equals( const TxQualType* that ) const;

    bool operator==( const TxQualType& other ) const;

    inline bool operator!=( const TxQualType& other ) const {
        return !this->operator==( other );
    }

    std::string str( bool brief ) const;

    virtual std::string str() const override;
};
