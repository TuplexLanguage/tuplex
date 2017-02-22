#pragma once

#include "location.hpp"
#include "tx_lang_defs.hpp"

#include "symbol/type_registry.hpp"


class TypeRegistry;
class TxModule;
class TxBuiltinTypeDeclNode;
class TxInterfaceAdapterType;


class BuiltinTypes {
    TypeRegistry& registry;

    /** parse location used for built-in constructs without actual source code */
    const TxLocation& builtinLocation;

    TxBuiltinTypeDeclNode* builtinTypes[BuiltinTypeId_COUNT];
    const TxType* builtinModTypes[BuiltinTypeId_COUNT];

    void declare_default_constructor(LexicalContext& ctx, BuiltinTypeId typeId, TxExpressionNode* initValueExpr);
    void declare_conversion_constructor(BuiltinTypeId fromTypeId, BuiltinTypeId toTypeId);
    void declare_tx_functions(TxModule* module);

    void add_builtin_abstract(TxModule* module, TxTypeClass typeClass, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId);
    void add_builtin_integer(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size, bool sign);
    void add_builtin_floating(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size);

public:
    BuiltinTypes( TypeRegistry& registry );

    const TxLocation& get_builtin_location() const { return this->builtinLocation; }

    /** to be invoked immediately after object construction */
    void initializeBuiltinSymbols();

    const TxType* get_builtin_type(const BuiltinTypeId id, bool mod=false) const;

    /** Gets a concrete "adapter type" that specializes the interface type and redirects to adaptedType. */
    // TODO: move to TypeRegistry?
    const TxInterfaceAdapterType* inner_get_interface_adapter( const TxType* interfaceType, const TxType* adaptedType );
};
