#pragma once

#include "driver.hpp"
#include "logging.hpp"
#include "type_registry.hpp"
#include "module.hpp"




/** A Tuplex package represents a Tuplex compilation unit.
 * A compilation unit can consist of multiple parsing units (source files).
 * As derived from TxModule, the package constitutes the root "module" (the namespace root).
 */
class TxPackage : public TxModule {
    TxDriver& _driver;
    TypeRegistry* typeRegistry;
    const TxFieldEntity* mainFunc;

public:
    TxPackage(TxDriver& driver)
            : TxModule(nullptr, "", false), _driver(driver), mainFunc() {
        this->typeRegistry = new TypeRegistry(*this);
    }


    inline TxDriver& driver() const {
        return this->_driver;
    }


    inline const TypeRegistry& types() const { return *this->typeRegistry; }
    inline TypeRegistry& types() {
        return const_cast<TypeRegistry&>(static_cast<const TxPackage *>(this)->types());
    }

    void registerMainFunc(const TxFieldEntity* mainFunc);

    const TxFieldEntity* getMainFunc() const {
        return this->mainFunc;
    }


    virtual std::string to_string() const {
        return "<package>";
    }
};
