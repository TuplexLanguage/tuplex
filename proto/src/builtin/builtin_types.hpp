#pragma once

#include "location.hpp"
#include "tx_lang_defs.hpp"

#include "symbol/type_registry.hpp"

/** Creates the package, i.e. the root namespace module (""). */
TxPackage* make_root_package( TxParserContext* parserContext );

class TypeRegistry;
class TxModule;
class TxTypeDeclNode;
class TxInterfaceAdapterType;

class BuiltinTypes {
    TypeRegistry& registry;

    /** parse location used for built-in constructs without actual source code */
    const TxLocation& builtinLocation;

    TxTypeDeclNode* builtinTypes[BuiltinTypeId_COUNT];

    void declare_tx_functions();

public:
    BuiltinTypes( TypeRegistry& registry );

    TxParsingUnitNode* createTxModuleAST();

    void initializeBuiltinSymbols();  // TODO: remove

    const TxLocation& get_builtin_location() const {
        return this->builtinLocation;
    }

    const TxType* get_builtin_type( const BuiltinTypeId id ) const;
};

void merge_builtin_type_definers( TxTypeExpressionNode* definer, TxTypeDefiningNode* builtinDefiner,
                                  TxTypeExpressionNode* baseType,
                                  const std::vector<TxTypeExpressionNode*>& interfaces, bool _mutable );
