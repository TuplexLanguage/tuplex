#pragma once

#include <vector>

#include "location.hpp"
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
class TxBuiltinTypeDeclNode;
class TxTypeExpressionNode;


class TypeRegistry {
    TxPackage& package;
    TxBuiltinTypeDeclNode* builtinTypes[BuiltinTypeId_COUNT];
    const TxType* builtinModTypes[BuiltinTypeId_COUNT];

    /** parse location used for built-in constructs without actual source code */
    const TxLocation builtinLocation;

    std::vector<TxTypeExpressionNode*> enqueuedSpecializations;

    /** all the types created */
    std::vector<TxType*>* createdTypes;
    /** all the static types, i.e. all types with distinct static data type (and thus distinct TypeId) */
    std::vector<const TxType*> staticTypes;

    void add_type(TxType* type);

    void declare_default_constructor(LexicalContext& ctx, BuiltinTypeId typeId, TxExpressionNode* initValueExpr);
    void declare_conversion_constructor(BuiltinTypeId fromTypeId, BuiltinTypeId toTypeId);
    void declare_tx_functions(TxModule* module);

    void add_builtin_abstract(TxModule* module, TxTypeClass typeClass, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId);
    void add_builtin_integer(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size, bool sign);
    void add_builtin_floating(TxModule* module, BuiltinTypeId id, std::string plainName, BuiltinTypeId parentId, int size);

    /** Makes a new type specialization and registers it with this registry. */
    TxType* make_specialized_type(const TxTypeDeclaration* declaration,
                                  const TxTypeSpecialization& specialization,
                                  const std::vector<TxTypeSpecialization>& interfaces=std::vector<TxTypeSpecialization>());
    friend class TxImplicitTypeDefiningNode;  // may access make_specialized_type()

    /** Gets a concrete "adapter type" that specializes the interface type and redirects to adaptedType. */
    const TxInterfaceAdapterType* inner_get_interface_adapter(const TxType* interfaceType, const TxType* adaptedType);

public:
    /** The root type. All variables / fields / interfaces / objects are instances of this type. */
    static TxType const * const Any;

    TypeRegistry(TxPackage& package);

    TxDriver* get_driver() const;

    inline const TxLocation& get_builtin_location() const { return this->builtinLocation; }

    /** to be invoked immediately after object construction */
    void initializeBuiltinSymbols();

    /** to be invoked after the resolution pass has been run on package's source, and before type registration */
    void enqueued_resolution_pass();

    const std::vector<TxTypeExpressionNode*>& get_enqueued_specializations() const { return this->enqueuedSpecializations; }

    /** to be invoked after the whole package's source has been processed, before code generation */
    void register_types();


    const TxType* get_builtin_type(const BuiltinTypeId id, bool mod=false) const;


    /*--- retrievers for derived types ---*/

    /** Gets a modifiable 'usage' of a base type.
     * The type parameters of the base type will pass-through (appear redeclared) in the modifiable type.
     */
    const TxType* get_modifiable_type(const TxTypeDeclaration* declaration, const TxType* type);

    const TxType* get_empty_specialization(const TxTypeDeclaration* declaration, const TxType* type);

    /** Gets a specialization of a base type.
     * Note: Added / overridden members will not be initialized, the caller must invoke prepare_type_members() to do that.
     */
    TxType* get_type_specialization(const TxTypeDeclaration* declaration, const TxType* baseType,
                                    const std::vector<TxTypeSpecialization>& interfaces=std::vector<TxTypeSpecialization>(),
                                    const std::vector<TxGenericBinding>* bindings=nullptr, bool _mutable=false);

    /** Gets a concrete "adapter type" that specializes the interface type and redirects to adaptedType. */
    const TxType* get_interface_adapter(const TxType* interfaceType, const TxType* adaptedType);

    const TxReferenceType* get_reference_type(const TxTypeDeclaration* declaration, TxGenericBinding targetTypeBinding,
                                              const TxIdentifier* dataspace=nullptr);

    const TxArrayType* get_array_type(const TxTypeDeclaration* declaration, TxGenericBinding elemTypeBinding, TxGenericBinding lengthBinding);
    const TxArrayType* get_array_type(const TxTypeDeclaration* declaration, TxGenericBinding elemTypeBinding);

    // "mod" of function refers to whether functions of this type may modify its closure when run.
    // Note: "mod" of args not part of the function type (though concrete function may mod-ify its stack arg copies).
    const TxFunctionType* get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, const TxType* returnType, bool mod=false);
    const TxFunctionType* get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, bool mod=false);
    const TxConstructorType* get_constructor_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, TxTypeDeclaration* constructedObjTypeDecl);


    /** Returns a read-only iterator that points to the first type. */
    inline std::vector<const TxType*>::const_iterator types_cbegin() const { return this->staticTypes.cbegin(); }
    /** Returns a read-only iterator that points to one past the last type. */
    inline std::vector<const TxType*>::const_iterator types_cend()   const { return this->staticTypes.cend(); }

    inline uint32_t get_type_count() const { return this->staticTypes.size(); }
};
