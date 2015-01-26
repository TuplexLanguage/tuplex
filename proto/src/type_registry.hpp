#pragma once

#include "tx_lang_defs.hpp"
#include "type.hpp"


enum BuiltinTypeId : int {
    ANY,
    ELEMENTARY,
    SCALAR,
    INTEGER,
    SIGNED,
    BYTE,
    SHORT,
    INT,
    LONG,
    UNSIGNED,
    UBYTE,
    USHORT,
    UINT,
    ULONG,
    FLOATINGPOINT,
    HALF,
    FLOAT,
    DOUBLE,
    BOOLEAN,
    REFERENCE,
    ARRAY,
    FUNCTION,
    TUPLE,
    BuiltinTypeId_COUNT
};


class TxPackage;
class TxModule;
class BuiltinTypeRecord;


class TypeRegistry {
    TxPackage& package;
    const BuiltinTypeRecord* builtinTypes[BuiltinTypeId_COUNT];
    const TxType* builtinModTypes[BuiltinTypeId_COUNT];

    void initializeBuiltinSymbols();
    //void add_builtin(TxModule* module, BuiltinTypeRecord* record);
    void add_builtin_abstract(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId);
    void add_builtin_integer(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size, bool sign);
    void add_builtin_floating(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size);

public:
    TypeRegistry(TxPackage& package) : package(package) {
        this->initializeBuiltinSymbols();
    }

    /** The root type. All variables / fields / interfaces / objects are instances of this type. */
    static TxType const * const Any;


    const TxType* get_builtin_type(const BuiltinTypeId id, bool mod=false) const;


    /*--- retrievers for derived types ---*/

    /** Gets a pure specialization of a base type.
     * Any unbound type parameters of the base type will be redeclared in the specialized type.
     */
    const TxType* get_type_specialization(const TxTypeEntity* newEntity, const TxTypeSpecialization& specialization, std::string* errorMsg=nullptr);


    const TxReferenceType* get_reference_type(const TxTypeEntity* newEntity, const TxTypeProxy* targetType, bool mod=false, std::string* errorMsg=nullptr);

    const TxArrayType* get_array_type(const TxTypeEntity* newEntity, const TxTypeProxy* elemType, const TxConstantProxy* length, bool mod=false, std::string* errorMsg=nullptr);
    const TxArrayType* get_array_type(const TxTypeEntity* newEntity, const TxTypeProxy* elemType, bool mod=false, std::string* errorMsg=nullptr);

    // "mod" of function refers to whether functions of this type may modify its closure when run.
    // Note: "mod" of args not part of the function type (though concrete function may mod-ify its stack arg copies).
    const TxFunctionType* get_function_type(const TxTypeEntity* newEntity, const std::vector<const TxType*>& argumentTypes, const TxType* returnType, bool mod=false, std::string* errorMsg=nullptr);
    const TxFunctionType* get_function_type(const TxTypeEntity* newEntity, const std::vector<const TxType*>& argumentTypes, bool mod=false, std::string* errorMsg=nullptr);

    // "mod" of tuple means it is not immutable, i.e. its instances may be declared modifiable.
    const TxTupleType* get_tuple_type(const TxTypeEntity* newEntity, bool mod=false, std::string* errorMsg=nullptr);
//    const TxTupleType* get_tuple(const TxTypeEntity* newEntity, const TxTypeSpecialization* baseType,
//                                 const std::vector<const TxTypeSpecialization*>& interfaces,
//                                 static members, instance members, bool mod=false);

};
