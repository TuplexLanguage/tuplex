#pragma once

#include "parser/location.hpp"
#include "tx_lang_defs.hpp"

#include "symbol/type_registry.hpp"

/** Creates the package, i.e. the root namespace module (""). */
TxPackage* make_root_package( TxParserContext* parserContext );

class TxParsingUnitNode;
class TxModuleNode;
class TypeRegistry;
class TxTypeDeclNode;

class BuiltinTypes {
    TypeRegistry& registry;

    /** parse location used for built-in constructs without actual source code */
    const TxLocation& builtinLocation;

    TxTypeDeclNode* builtinTypes[BuiltinTypeId_COUNT];

    TxModuleNode* create_tx_c_module();

    void declare_tx_functions();

public:
    BuiltinTypes( TypeRegistry& registry );

    TxParsingUnitNode* createTxModuleAST();

    void resolveBuiltinSymbols();  // TODO: remove

    const TxLocation& get_builtin_location() const {
        return this->builtinLocation;
    }

    const TxType* get_builtin_type( const BuiltinTypeId id ) const;
};

class TxDerivedTypeNode;
void merge_builtin_type_definers( TxDerivedTypeNode* sourcecodeDefiner, TxTypeDefiningNode* builtinDefiner );
