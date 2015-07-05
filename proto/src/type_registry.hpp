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
    BOOL,
    REFERENCE,
    ARRAY,
    FUNCTION,
    TUPLE,
    INTERFACE,
    BuiltinTypeId_COUNT
};


class TxPackage;
class TxModule;
class BuiltinTypeRecord;


class TypeRegistry {
    TxPackage& package;
    BuiltinTypeRecord* builtinTypes[BuiltinTypeId_COUNT];
    const TxType* builtinModTypes[BuiltinTypeId_COUNT];

    /** all the static types, i.e. all types with distinct static data type (and thus distinct TypeId) */
    std::vector<const TxType*> allStaticTypes;

    void register_type(TxType* type);

    void initializeBuiltinSymbols();
    void declare_conversion_constructor(BuiltinTypeId fromTypeId, BuiltinTypeId toTypeId);

    void add_builtin_abstract(TxModule* module, TxTypeClass typeClass, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId);
    void add_builtin_integer(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size, bool sign);
    void add_builtin_floating(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size);

public:
    TypeRegistry(TxPackage& package) : package(package) {
        this->initializeBuiltinSymbols();
    }

    /** The root type. All variables / fields / interfaces / objects are instances of this type. */
    static TxType const * const Any;


    const TxTypeDefiner* get_builtin_type_def(const BuiltinTypeId id) const;

    const TxType* get_builtin_type(const BuiltinTypeId id, bool mod=false) const;


    /*--- retrievers for derived types ---*/

    /** Gets a modifiable 'usage' of a base type.
     * The type parameters of the base type will pass-through (appear redeclared) in the modifiable type.
     */
    const TxType* get_modifiable_type(const TxTypeDeclaration* declaration, const TxType* type);

    /** Gets a specialization of a base type.
     * Any type parameters of the base type that aren't bound in the provided specialization
     * will automatically be redeclared in the specialized type.
     */
    const TxType* get_type_specialization(const TxTypeDeclaration* declaration, const TxTypeSpecialization& specialization,
                                          const std::vector<TxTypeSpecialization>& interfaces=std::vector<TxTypeSpecialization>(),
                                          const std::vector<TxTypeParam>* typeParams=nullptr, bool _mutable=false);

    /** Gets a concrete "adapter type" that specializes the interface type and redirects to adaptedType. */
    const TxType* get_interface_adapter(const TxType* interfaceType, const TxType* adaptedType);

    const TxReferenceType* get_reference_type(const TxTypeDeclaration* declaration, TxGenericBinding targetTypeBinding,
                                              const TxIdentifier* dataspace=nullptr);
    //const TxReferenceType* get_reference_type(const TxTypeDeclaration* declaration, TxEntityDefiner* targetType);

    const TxArrayType* get_array_type(const TxTypeDeclaration* declaration, TxGenericBinding elemTypeBinding, TxGenericBinding lengthBinding);
    const TxArrayType* get_array_type(const TxTypeDeclaration* declaration, TxGenericBinding elemTypeBinding);

    // "mod" of function refers to whether functions of this type may modify its closure when run.
    // Note: "mod" of args not part of the function type (though concrete function may mod-ify its stack arg copies).
    const TxFunctionType* get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, const TxType* returnType, bool mod=false);
    const TxFunctionType* get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, bool mod=false);
    const TxConstructorType* get_constructor_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, TxTypeDeclaration* constructedObjTypeDecl);

//    /** Creates a new tuple type that does not extend another data type or interface.
//     * "mut" of tuple means it is not immutable, i.e. its instances may be declared modifiable. */
//    const TxTupleType* get_tuple_type(const TxTypeDeclaration* declaration, bool mut=false, std::string* errorMsg=nullptr);
//
//    /** Creates a new tuple type that is an extension to other types.
//     * "mut" of tuple means it is not immutable, i.e. its instances may be declared modifiable. */
//    const TxTupleType* get_tuple_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseType,
//                                      bool mut=false, std::string* errorMsg=nullptr);


    /** Returns a read-only iterator that points to the first type. */
    inline std::vector<const TxType*>::const_iterator types_cbegin() const { return this->allStaticTypes.cbegin(); }
    /** Returns a read-only iterator that points to one past the last type. */
    inline std::vector<const TxType*>::const_iterator types_cend()   const { return this->allStaticTypes.cend(); }

    inline uint32_t get_type_count() const { return this->allStaticTypes.size(); }
};
