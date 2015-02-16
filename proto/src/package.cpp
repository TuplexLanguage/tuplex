#include "package.hpp"
#include "entity.hpp"

void TxPackage::registerMainFunc(const TxFieldEntity* mainFunc) {
    if (! this->mainFunc) {
        this->mainFunc = mainFunc;
        this->LOGGER().debug("Set user main function: %s", mainFunc->to_string().c_str());
    }
    else
        this->LOGGER().debug("User main function already set, skipping %s", mainFunc->to_string().c_str());
}
