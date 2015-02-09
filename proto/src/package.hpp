#pragma once

#include <map>

#include "logging.hpp"
#include "type_registry.hpp"
#include "module.hpp"
#include "parser_driver.hpp"




/** A Tuplex package represents a Tuplex compilation unit.
 * A compilation unit can consist of multiple parsing units (source files).
 * As derived from TxModule, the package constitutes the root "module" (the namespace root).
 */
class TxPackage : public TxModule {
    TxDriver& _driver;
    TypeRegistry* typeRegistry;
    TxIdentifier mainFuncQualIdent;

public:
    TxPackage(TxDriver& driver)
            : TxModule(nullptr, "", false), _driver(driver) {
        this->typeRegistry = new TypeRegistry(*this);
    }


    inline TxDriver& driver() const {
        return this->_driver;
    }


    inline const TypeRegistry& types() const { return *this->typeRegistry; }
    inline TypeRegistry& types() {
        return const_cast<TypeRegistry&>(static_cast<const TxPackage *>(this)->types());
    }

    void registerMain(const TxIdentifier& mainFuncQualIdent) {
        if (this->mainFuncQualIdent.is_empty()) {
            this->mainFuncQualIdent = mainFuncQualIdent;
            this->LOGGER().debug("Set user main function: %s", mainFuncQualIdent.to_string().c_str());
        }
        else
            this->LOGGER().debug("User main function already set, skipping %s", mainFuncQualIdent.to_string().c_str());
    }
    const TxIdentifier& getMainFuncIdent() const {
        return this->mainFuncQualIdent;
    }


    virtual std::string to_string() const {
        return "<package>";
    }
};
