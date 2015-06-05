#pragma once

#include "driver.hpp"
#include "logging.hpp"
#include "tx_error.hpp"
#include "type_registry.hpp"
#include "module.hpp"




/** A Tuplex package represents a Tuplex compilation unit.
 * A compilation unit can consist of multiple parsing units (source files).
 * As derived from TxModule, the package constitutes the root "module" (the namespace root).
 */
class TxPackage : public TxModule {
    TxDriver& _driver;
    TypeRegistry* typeRegistry;
    const TxEntitySymbol* mainFunc;

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

    void registerMainFunc(const TxEntitySymbol* mainFunc);

    const TxFieldDeclaration* getMainFunc() const {
        if (this->mainFunc) {
            if (this->mainFunc->field_count() == 1)
                return this->mainFunc->get_first_field_decl();
            else if (this->mainFunc->is_overloaded())
                CWARNING(this->mainFunc->get_first_field_decl()->get_definer(),
                         "main() function symbol is overloaded: " << this->mainFunc);
        }
        return nullptr;
    }


    virtual std::string symbol_class_string() const override {
        return "<package>";
    }
    virtual std::string to_string() const override {
        return "(namespace root)";
    }
};
