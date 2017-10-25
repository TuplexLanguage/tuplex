#pragma once

#include "util/logging.hpp"

#include "tx_error.hpp"

#include "module.hpp"

class TypeRegistry;
class BuiltinTypes;
class TxDriver;

/** A Tuplex package represents a Tuplex compilation unit.
 * A compilation unit can consist of multiple parsing units (source files).
 * As derived from TxModule, the package constitutes the root "module" (the namespace root).
 */
class TxPackage : public TxModule {
    TxDriver& _driver;
    TypeRegistry* typeRegistry;
    BuiltinTypes* builtinTypes;
    const TxFieldDeclaration* mainFunc;

public:
    TxPackage( TxDriver& driver, const TxParseOrigin& rootOrigin );

    inline TxDriver& driver() const {
        return this->_driver;
    }

    inline TypeRegistry& registry() const {
        return *this->typeRegistry;
    }

    inline BuiltinTypes& builtins() const {
        return *this->builtinTypes;
    }

    /** Gets the "root parse origin", corresponding to the package scope (the namespace root). */
    inline const TxParseOrigin& root_origin() const {
        return this->origin;
    }

    void registerMainFunc( const TxFieldDeclaration* mainFunc );

    const TxFieldDeclaration* getMainFunc() const;

    virtual std::string description_string() const override {
        return "package";
    }

    virtual std::string str() const override {
        return "package";
    }
};
