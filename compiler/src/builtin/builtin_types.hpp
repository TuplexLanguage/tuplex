#pragma once

#include "parser/location.hpp"
#include "tx_lang_defs.hpp"

#include "symbol/type_registry.hpp"


/** Returns the builtins' source file name (the C++ source file name). Used for builtins' debug info. */
const std::string& get_builtins_file_name();

class TxParsingUnitNode;
class TxModuleNode;
class TxTypeDeclNode;

class BuiltinTypes {
    /** Parser context representing the built-in internally coded constructs (without tx source code). */
    TxParserContext* builtinParserContext;

    /** the currently compiled tuplex package */
    TxPackage* package;

    /** parse location used for built-in constructs without actual source code */
    const TxLocation& builtinLocation;

    TxTypeDeclNode* builtinTypes[BuiltinTypeId_COUNT];

    TxModuleNode* create_tx_c_module();

public:
    BuiltinTypes( TxParserContext* builtinParserContext );

    /** Creates the package, i.e. the root namespace module (""). */
    TxPackage* get_root_package() const {
        return this->package;
    }

    TxParsingUnitNode* createTxModuleAST();

    void resolveBuiltinSymbols();

    /** parse location used for built-in constructs without actual source code */
    const TxLocation& get_builtin_location() const {
        return this->builtinLocation;
    }

    const TxActualType* get_builtin_type( const BuiltinTypeId id ) const;
};

class TxDerivedTypeNode;
void merge_builtin_type_definers( TxDerivedTypeNode* sourcecodeDefiner, TxTypeResolvingNode* builtinDefiner );
