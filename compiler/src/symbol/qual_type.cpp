#include "qual_type.hpp"

#include "type_base.hpp"


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
