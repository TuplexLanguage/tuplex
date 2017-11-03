#include "package.hpp"

#include "entity.hpp"
#include "type_registry.hpp"
#include "builtin/builtin_types.hpp"

TxPackage::TxPackage( TxDriver& driver, const TxParseOrigin& rootOrigin )
        : TxModule( nullptr, "", rootOrigin, false ), _driver( driver ), mainFunc() {
    this->typeRegistry = new TypeRegistry( *this );
}

void TxPackage::registerMainFunc( const TxFieldDeclaration* mainFunc ) {
    if ( !this->mainFunc ) {
        this->mainFunc = mainFunc;
        LOG_DEBUG( this->LOGGER(), "Set user main function: " << mainFunc );
    }
    else
        CINFO( mainFunc->get_definer(), "Multiple main() functions, will use first one encountered: " << this->mainFunc->get_unique_full_name()
               << " at " << this->mainFunc->get_definer()->parse_loc_string() );
}

const TxFieldDeclaration* TxPackage::getMainFunc() const {
    return this->mainFunc;
}
