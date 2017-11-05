#pragma once

#include <algorithm>

#include "util/logging.hpp"

#include "ast/lexcontext.hpp"

#include "type_base.hpp"


llvm::Value* gen_compute_array_size( LlvmGenerationContext& context, GenScope* scope,
                                     llvm::Value* elemSizeV, llvm::Value* arrayCapi64V );

/** Initializes an array object in memory by storing the specified capacity and length values to it. */
void initialize_array_obj( LlvmGenerationContext& context, GenScope* scope,
                           llvm::Value* arrayObjPtrV, llvm::Value* arrayCap, llvm::Value* arrayLen );


class TxArrayTypeClassHandler final : public TxTypeClassHandler {
protected:
    virtual bool inner_is_assignable_to( const TxActualType* type, const TxActualType* other ) const override;

public:
    TxArrayTypeClassHandler()
            : TxTypeClassHandler( TXTC_ARRAY ) {
    }

    virtual llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
    virtual llvm::Type* make_llvm_externc_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
    virtual llvm::DIType* make_llvm_debug_type( const TxActualType* type, LlvmGenerationContext& context ) const override;

    virtual void initialize_specialized_obj( const TxActualType* type, LlvmGenerationContext& context, GenScope* scope, llvm::Value* objPtrV ) const override;

    virtual llvm::Constant* gen_static_element_size( const TxActualType* type, LlvmGenerationContext& context ) const override;
    virtual llvm::Constant* gen_static_size( const TxActualType* type, LlvmGenerationContext& context ) const override;

    virtual llvm::Value* gen_alloca( const TxActualType* type, LlvmGenerationContext& context, GenScope* scope, const std::string &varName = "" ) const override;
    virtual llvm::Value* gen_malloc( const TxActualType* type, LlvmGenerationContext& context, GenScope* scope, const std::string &varName = "" ) const override;
};


/** Note, all reference specializations are mutable. */
class TxReferenceTypeClassHandler final : public TxTypeClassHandler {
protected:
    virtual bool inner_is_assignable_to( const TxActualType* type, const TxActualType* other ) const override;

public:
    TxReferenceTypeClassHandler()
            : TxTypeClassHandler( TXTC_REFERENCE ) {
    }

    virtual llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
    virtual llvm::Type* make_llvm_externc_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
    virtual llvm::DIType* make_llvm_debug_type( const TxActualType* type, LlvmGenerationContext& context ) const override;

    static llvm::Type* make_ref_llvm_type( LlvmGenerationContext& context, llvm::Type* targetType );
    static llvm::Type* make_ref_llvm_type( LlvmGenerationContext& context, llvm::Type* targetType, const std::string& name );
};


class TxTupleTypeClassHandler final : public TxTypeClassHandler {
public:
    TxTupleTypeClassHandler() : TxTypeClassHandler( TXTC_TUPLE ) { }

    virtual void initialize_specialized_obj( const TxActualType* type, LlvmGenerationContext& context, GenScope* scope, llvm::Value* objPtrV ) const override;
    virtual llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
    virtual llvm::Type* make_llvm_type_body( const TxActualType* type, LlvmGenerationContext& context, llvm::Type* header ) const override;
    virtual llvm::DIType* make_llvm_debug_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
};


/** Interfaces are intrinsically abstract types. */
class TxInterfaceTypeClassHandler final : public TxTypeClassHandler {
public:
    TxInterfaceTypeClassHandler() : TxTypeClassHandler( TXTC_INTERFACE )  { }

    // TODO: allow interfaces with proper is-a relationship to auto-convert (via adapter)

    virtual llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
};



class TxInterfaceAdapterTypeClassHandler final : public TxTypeClassHandler {
public:
    TxInterfaceAdapterTypeClassHandler() : TxTypeClassHandler( TXTC_INTERFACEADAPTER )  { }

    virtual llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
};

/** Constitutes a type adapter from the adapted type to the interface type.
 * The interface type is the direct base type of the adapter type.
 * Technically, the type adapter is as concrete/abstract/generic-dependent as its adapted type.
 * However no instances of it are ever created - it is to be used as a reference target type. */
class TxInterfaceAdapterType final : public TxActualType {
    // as adapters are intrinsically abstract, no instances are created and can't be any more assignable than interfaces
    const TxActualType* adaptedType;

protected:
    virtual bool inner_prepare_members() override;

public:
    TxInterfaceAdapterType( const TxTypeDeclaration* declaration, const TxActualType* interfaceType, const TxActualType* adaptedType )
            : TxActualType( get_type_class_handler( TXTC_INTERFACEADAPTER ), declaration, adaptedType->is_mutable(), interfaceType ),
                            adaptedType( adaptedType ) {
    }

    inline const TxActualType* adapted_type() const {
        return this->adaptedType;
    }
};
