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
    BuiltinTypeRecord* builtinTypes[BuiltinTypeId_COUNT];
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

    /** Gets a modifiable 'usage' of a base type.
     * The type parameters of the base type will pass-through (appear redeclared) in the modifiable type.
     */
    const TxType* get_modifiable_type(TxTypeEntity* newEntity, const TxType* type, std::string* errorMsg=nullptr);

    /** Gets a specialization of a base type.
     * Any type parameters of the base type that aren't bound in the provided specialization
     * will automatically be redeclared in the specialized type.
     */
    const TxType* get_type_specialization(TxTypeEntity* newEntity, const TxTypeSpecialization& specialization,
                                          bool _mutable, const std::vector<TxTypeParam>* typeParams=nullptr,
                                          std::string* errorMsg=nullptr);


    const TxReferenceType* get_reference_type(TxTypeEntity* newEntity, TxGenericBinding targetTypeBinding, std::string* errorMsg=nullptr);
    //const TxReferenceType* get_reference_type(TxTypeEntity* newEntity, TxEntityDefiner* targetType, std::string* errorMsg=nullptr);

    const TxArrayType* get_array_type(TxTypeEntity* newEntity, TxGenericBinding elemTypeBinding, TxGenericBinding lengthBinding, std::string* errorMsg=nullptr);
    const TxArrayType* get_array_type(TxTypeEntity* newEntity, TxGenericBinding elemTypeBinding, std::string* errorMsg=nullptr);

    // "mod" of function refers to whether functions of this type may modify its closure when run.
    // Note: "mod" of args not part of the function type (though concrete function may mod-ify its stack arg copies).
    const TxFunctionType* get_function_type(TxTypeEntity* newEntity, const std::vector<const TxType*>& argumentTypes, const TxType* returnType, bool mod=false, std::string* errorMsg=nullptr);
    const TxFunctionType* get_function_type(TxTypeEntity* newEntity, const std::vector<const TxType*>& argumentTypes, bool mod=false, std::string* errorMsg=nullptr);

//    /** Creates a new tuple type that does not extend another data type or interface.
//     * "mut" of tuple means it is not immutable, i.e. its instances may be declared modifiable. */
//    const TxTupleType* get_tuple_type(TxTypeEntity* newEntity, bool mut=false, std::string* errorMsg=nullptr);
//
//    /** Creates a new tuple type that is an extension to other types.
//     * "mut" of tuple means it is not immutable, i.e. its instances may be declared modifiable. */
//    const TxTupleType* get_tuple_type(TxTypeEntity* newEntity, const TxTypeSpecialization& baseType,
//                                      bool mut=false, std::string* errorMsg=nullptr);

};
