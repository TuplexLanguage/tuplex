#include "package.hpp"

#include "entity.hpp"
#include "type_registry.hpp"
#include "builtin/builtin_types.hpp"


TxPackage::TxPackage( TxDriver& driver, const TxParseOrigin& rootOrigin )
        : TxModule(nullptr, "", rootOrigin, false), _driver(driver), mainFunc() {
    this->typeRegistry = new TypeRegistry( *this );
    this->builtinTypes = new BuiltinTypes( *this->typeRegistry );
}


void TxPackage::registerMainFunc(const TxEntitySymbol* mainFunc) {
    if (! this->mainFunc) {
        this->mainFunc = mainFunc;
        this->LOGGER().debug("Set user main function: %s", mainFunc->str().c_str());
    }
    else
        this->LOGGER().debug("User main function already set, skipping %s", mainFunc->str().c_str());
}

const TxFieldDeclaration* TxPackage::getMainFunc() const {
    if (this->mainFunc) {
        if (this->mainFunc->field_count() == 1)
            return this->mainFunc->get_first_field_decl();
        else if (this->mainFunc->is_overloaded())
            CWARNING(this->mainFunc->get_first_field_decl()->get_definer(),
                     "main() function symbol is overloaded: " << this->mainFunc);
    }
    return nullptr;
}
