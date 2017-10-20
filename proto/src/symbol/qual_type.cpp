#include "qual_type.hpp"

#include "type_base.hpp"


//bool TxQualType::shallow_equals( const TxQualType* that ) const {
//    if (this == that)
//        return true;
//    return ( this->_modifiable == that->_modifiable
//             && this->_type->shallow_equals( that->_type ) );
//}

//bool TxQualType::operator==( const TxQualType& other ) const {
//    return this->inner_get_ == other._modifiable && *this->_type == *other._type;
//}

std::string TxQualType::str( bool brief ) const {
    if ( !this->_mashed )
        return "NULL";
    else if (this->is_modifiable())
        return "~" + this->type()->str( brief );
    else
        return this->type()->str( brief );
}

std::string TxQualType::str() const {
    return this->str( true );
}
