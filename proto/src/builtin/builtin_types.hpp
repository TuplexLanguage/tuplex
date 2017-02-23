#pragma once

#include "location.hpp"
#include "tx_lang_defs.hpp"

#include "symbol/type_registry.hpp"


class TypeRegistry;
class TxModule;
class TxTypeDeclNode;
class TxInterfaceAdapterType;


class BuiltinTypes {
    TypeRegistry& registry;

    /** parse location used for built-in constructs without actual source code */
    const TxLocation& builtinLocation;

    TxTypeDeclNode* builtinTypes[BuiltinTypeId_COUNT];

    void declare_default_constructor(LexicalContext& ctx, BuiltinTypeId typeId, TxExpressionNode* initValueExpr);
    void declare_conversion_constructor(BuiltinTypeId fromTypeId, BuiltinTypeId toTypeId);
    void declare_tx_functions(TxModule* module);

    void add_builtin_abstract(TxModule* module, TxTypeClass typeClass, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId);
    void add_builtin_integer(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size, bool sign);
    void add_builtin_floating(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size);

    TxTypeDeclNode* make_builtin_abstract( TxTypeClass typeClass, BuiltinTypeId id, std::string plainName, std::string parentName ) const;
    TxTypeDeclNode* make_builtin_integer ( BuiltinTypeId id, std::string plainName, std::string parentName, int size, bool sign ) const;
    TxTypeDeclNode* make_builtin_floating( BuiltinTypeId id, std::string plainName, std::string parentName, int size ) const;

    TxFieldDeclNode* make_default_constructor( const std::string& toTypeName, TxExpressionNode* initValueExpr ) const;

public:
    BuiltinTypes( TypeRegistry& registry );


    TxParsingUnitNode* createTxModuleAST();

    void initializeBuiltinSymbols();  // FIXME: remove


    const TxLocation& get_builtin_location() const { return this->builtinLocation; }

    const TxType* get_builtin_type( const BuiltinTypeId id ) const;

    /** Gets a concrete "adapter type" that specializes the interface type and redirects to adaptedType. */
    // TODO: move to TypeRegistry?
    const TxInterfaceAdapterType* inner_get_interface_adapter( const TxType* interfaceType, const TxType* adaptedType );
};
