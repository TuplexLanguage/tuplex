#pragma once

#include "util/logging.hpp"

#include "tx_error.hpp"

#include "module.hpp"

class TypeRegistry;
class TxDriver;

/** A Tuplex package represents a Tuplex compilation unit.
 * A compilation unit can consist of multiple parsing units (source files).
 * As derived from TxModule, the package constitutes the root "module" (the namespace root).
 */
class TxPackage : public TxModule {
    TxDriver& _driver;
    TypeRegistry* typeRegistry;
    std::vector<const TxFieldDeclaration*> mainFuncCandidates;
    const TxFieldDeclaration* mainFunc;

public:
    TxPackage( TxDriver& driver, const TxParseOrigin& rootOrigin );

    inline TxDriver& driver() const {
        return this->_driver;
    }

    inline TypeRegistry& registry() const {
        return *this->typeRegistry;
    }

    /** Gets the "root parse origin", corresponding to the package scope (the namespace root). */
    inline const TxParseOrigin& root_origin() const {
        return this->origin;
    }

    /** Gets the declarations with external linkage (included in binary even if unused).
     * Typically for main() and API functions. */
    const std::vector<const TxFieldDeclaration*>& get_extlink_declarations();

    /** register candidate main function declaration */
    void register_main_func( const TxFieldDeclaration* mainFuncCand );

    /** Verify main function candidates' signatures, and determine which one to use */
    void determine_main_func();

    /** get main function declaration */
    const TxFieldDeclaration* get_main_func() const;

    std::string description_string() const override {
        return "package";
    }

    virtual std::string str() const override {
        return "package";
    }
};
