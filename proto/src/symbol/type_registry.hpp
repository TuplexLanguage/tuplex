#pragma once

#include <vector>

#include "location.hpp"
#include "tx_lang_defs.hpp"
#include "type.hpp"


class TxPackage;
class TxModule;
class TxBuiltinTypeDeclNode;
class TxTypeExpressionNode;
class TxTypeDeclNode;
class TxTypeArgumentNode;
class TxTypeTypeArgumentNode;
class TxValueTypeArgumentNode;


extern std::string encode_type_name( const TxTypeDeclaration* typeDecl );


class TypeRegistry {
    TxPackage& package;

//    /** parse location used for built-in constructs without actual source code */
//    const TxLocation& builtinLocation;
    const TxLocation& get_builtin_location() const;

    /** all the type usages */
    std::vector<TxType*> usedTypes;

    std::vector<TxTypeDeclNode*> enqueuedSpecializations;

    /** set to true when the type preparation phase starts */
    bool startedPreparingTypes = false;

    /** all the types created */
    std::vector<TxActualType*>* createdTypes;

    /** all the static types, i.e. all types with distinct static data type (and thus distinct TypeId) */
    std::vector<const TxActualType*> staticTypes;

    void add_type_usage(TxType* type);

    void add_type(TxActualType* type);

    /** Makes a new type specialization and registers it with this registry. */
    TxActualType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                         const std::vector<const TxType*>& interfaces={},
                                         bool modifiable=false );
    friend class TxImplicitTypeDefiningNode;  // may access make_specialized_type()
    friend class TxBuiltinTypeDefiningNode;  // may access make_specialized_type()
    friend class BuiltinTypes;  // TODO: temporary

//    /** Gets a concrete "adapter type" that specializes the interface type and redirects to adaptedType. */
//    const TxInterfaceAdapterType* inner_get_interface_adapter(const TxType* interfaceType, const TxType* adaptedType);

    const TxActualType* get_actual_empty_specialization( const TxTypeDeclaration* declaration, const TxActualType* type );

    const TxActualType* get_actual_type_derivation( const TxTypeExpressionNode* definer, const TxActualType* baseType,
                                                    const std::vector<const TxType*>& interfaces, bool _mutable );


    const TxActualType* get_actual_type_specialization( const TxTypeDefiningNode* definer, const TxActualType* baseType,
                                                        const std::vector<const TxTypeArgumentNode*>* bindings );

    const TxActualType* get_inner_type_specialization( const TxTypeDefiningNode* definer, const TxActualType* baseType,
                                                       const std::vector<const TxTypeArgumentNode*>* bindings,
                                                       bool isExpErrType );

    const TxActualType* make_type_specialization( const TxTypeDefiningNode* definer, const TxActualType* baseType,
                                                  const std::vector<const TxTypeArgumentNode*>* bindings,
                                                  bool isExpErrType, const std::string& newBaseTypeNameStr );


    void resolve_deferred_types();

//    /** to be invoked after the resolution pass has been run on package's source, and before type preparation */
//    void enqueued_resolution_pass();

    /** to be invoked after the whole package's source has been processed, before code generation.
     * This will perform some type validation, perform data layout, and assign runtime type ids. */
    void prepare_types();

    friend TxType;

public:
    TypeRegistry(TxPackage& package);

    TxPackage& get_package() const { return this->package; }


    /** to be invoked after the resolution pass has been run on package's source, and before type registration */
    void deferred_type_resolution_pass();

    /** Gets the enqueued specialization ASTs (e.g. for running code generation on them) */
    const std::vector<TxTypeDeclNode*>& get_enqueued_specializations() const { return this->enqueuedSpecializations; }


    const TxType* get_builtin_type(const BuiltinTypeId id, bool mod=false);


    /*--- retrievers for derived types ---*/

    /** Gets a modifiable 'usage' of a base type.
     * The type parameters of the base type will pass-through (appear redeclared) in the modifiable type.
     */
    const TxType* get_modifiable_type(const TxTypeDeclaration* declaration, const TxType* type);

    const TxType* get_empty_specialization(const TxTypeDeclaration* declaration, const TxType* type);

    /** Gets/makes a specialization of a generic base type. */
    const TxType* get_type_specialization( const TxTypeDefiningNode* definer, const TxType* baseType,
                                           const std::vector<const TxTypeArgumentNode*>& bindings );

    /** Makes a new derivation of a base type and a set of interfaces. */
    const TxType* get_type_derivation( const TxTypeExpressionNode* definer, const TxType* baseType,
                                       const std::vector<const TxType*>& interfaces, bool _mutable );

    /** Gets a concrete "adapter type" that specializes the interface type and redirects to adaptedType. */
    const TxType* get_interface_adapter(const TxType* interfaceType, const TxType* adaptedType);

    const TxType* get_reference_type( const TxTypeDefiningNode* definer, const TxTypeTypeArgumentNode* targetTypeBinding,
                                      const TxIdentifier* dataspace );

    const TxType* get_array_type( const TxTypeDefiningNode* definer, const TxTypeTypeArgumentNode* elemTypeBinding,
                                  const TxValueTypeArgumentNode* lengthBinding );
    const TxType* get_array_type( const TxTypeDefiningNode* definer, const TxTypeTypeArgumentNode* elemTypeBinding );

    // "mod" of function refers to whether functions of this type may modify its closure when run.
    // Note: "mod" of args not part of the function type (though concrete function may mod-ify its stack arg copies).
    const TxType* get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, const TxType* returnType, bool mod=false);
    const TxType* get_function_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, bool mod=false);
    const TxType* get_constructor_type(const TxTypeDeclaration* declaration, const std::vector<const TxType*>& argumentTypes, const TxTypeDeclaration* constructedObjTypeDecl);


    /** Returns a read-only iterator that points to the first static type (with unique compile-time id). */
    inline std::vector<const TxActualType*>::const_iterator static_types_cbegin() const { return this->staticTypes.cbegin(); }
    /** Returns a read-only iterator that points to one past the last static type. */
    inline std::vector<const TxActualType*>::const_iterator static_types_cend()   const { return this->staticTypes.cend(); }

    inline uint32_t get_static_type_count() const { return this->staticTypes.size(); }
};
