#include "qual_type.hpp"


bool TxQualType::shallow_equals( const TxQualType* that ) const {
    if (this == that)
        return true;
    return ( this->modifiable == that->modifiable
             && this->_type->shallow_equals( that->_type ) );
}

bool TxQualType::operator==( const TxQualType& other ) const {
    return this->modifiable == other.modifiable && *this->_type == *other._type;
}

std::string TxQualType::str( bool brief ) const {
    if (this->modifiable)
        return "~" + this->_type->str( brief );
    else
        return this->_type->str( brief );
}

std::string TxQualType::str() const {
    if (this->modifiable)
        return "~" + this->_type->str();
    else
        return this->_type->str();
}
