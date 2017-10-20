#pragma once

#include <algorithm>

#include "util/logging.hpp"

#include "ast/lexcontext.hpp"

#include "type_base.hpp"

class TxArrayTypeClassHandler final : public TxTypeClassHandler {
protected:
//    virtual TxArrayType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
//                                                bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
//        if ( !dynamic_cast<const TxArrayType*>( baseType ) )
//            throw std::logic_error( "Specified a base type for TxArrayType that was not a TxArrayType: " + baseType->str() );
//        return new TxArrayType( declaration, baseType, mutableType, interfaces );
//    }

    virtual bool auto_converts_to( const TxActualType* type, const TxActualType* dest ) const override;

    virtual bool inner_is_assignable_to( const TxActualType* type, const TxActualType* other ) const override;

public:
    TxArrayTypeClassHandler()
            : TxTypeClassHandler( TXTC_ARRAY ) {
    }

//    /** Returns the element type if bound, or tx.Array.E generic type parameter if unbound. */
//    const TxQualType* element_type() const;
//
//    /** Returns nullptr if unbound. */
//    const TxExpressionNode* capacity() const;

    virtual llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
    virtual llvm::Type* make_llvm_externc_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
    virtual void initialize_specialized_obj( const TxActualType* type, LlvmGenerationContext& context, GenScope* scope, llvm::Value* objPtrV ) const override;

    virtual llvm::Constant* gen_static_element_size( const TxActualType* type, LlvmGenerationContext& context ) const override;
    virtual llvm::Constant* gen_static_size( const TxActualType* type, LlvmGenerationContext& context ) const override;

    virtual llvm::Value* gen_alloca( const TxActualType* type, LlvmGenerationContext& context, GenScope* scope, const std::string &varName = "" ) const override;
    virtual llvm::Value* gen_malloc( const TxActualType* type, LlvmGenerationContext& context, GenScope* scope, const std::string &varName = "" ) const override;
};

/** Note, all reference specializations are mutable. */
class TxReferenceTypeClassHandler final : public TxTypeClassHandler {
protected:
//    virtual TxReferenceType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
//                                                    bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
//        if ( !dynamic_cast<const TxReferenceType*>( baseType ) )
//            throw std::logic_error( "Specified a base type for TxReferenceType that was not a TxReferenceType: " + baseType->str() );
//        return new TxReferenceType( declaration, baseType, interfaces );
//    }

    virtual bool inner_is_assignable_to( const TxActualType* type, const TxActualType* other ) const override;

public:
    TxReferenceTypeClassHandler()
            : TxTypeClassHandler( TXTC_REFERENCE ) {
    }

//    /** Returns the target type if bound, or tx.Ref.T generic type parameter if unbound. */
//    const TxQualType* target_type() const;

//    /** References are never abstract. */
//    virtual bool is_abstract() const override {
//        return false;
//    }
//
//    /** Returns true if this type is concrete. References are always concrete, also when generic. */
//    virtual bool is_concrete() const override {
//        return true;
//    }
//
//    /** Returns true if this type is static (i.e. can be directly instanced). References are always static, also when generic. */
//    virtual bool is_static() const override {
//        return true;
//    }

    virtual llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
    virtual llvm::Type* make_llvm_externc_type( const TxActualType* type, LlvmGenerationContext& context ) const override;

    static llvm::Type* make_ref_llvm_type( LlvmGenerationContext& context, llvm::Type* targetType );
    static llvm::Type* make_ref_llvm_type( LlvmGenerationContext& context, llvm::Type* targetType, const std::string& name );
};


class TxTupleTypeClassHandler : public TxTypeClassHandler {
//protected:
//    virtual TxTupleType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
//                                                bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
//        if ( dynamic_cast<const TxTupleType*>( baseType ) ) {
//            return new TxTupleType( declaration, baseType, interfaces, mutableType );
//        }
//        throw std::logic_error( "Specified a base type for TxTupleType that was not a TxTupleType: " + baseType->str() );
//    }

public:
    TxTupleTypeClassHandler() : TxTypeClassHandler( TXTC_TUPLE ) { }

    virtual void initialize_specialized_obj( const TxActualType* type, LlvmGenerationContext& context, GenScope* scope, llvm::Value* objPtrV ) const override;
    virtual llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
    virtual llvm::Type* make_llvm_type_body( const TxActualType* type, LlvmGenerationContext& context, llvm::Type* header ) const override;
};


/** Interfaces are intrinsically abstract types. */
class TxInterfaceTypeClassHandler : public TxTypeClassHandler {
//protected:
//    virtual TxInterfaceType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
//                                                    bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
//        if ( dynamic_cast<const TxInterfaceType*>( baseType ) )
//            return new TxInterfaceType( declaration, baseType, interfaces );
//        throw std::logic_error( "Specified a base type for TxInterfaceType that was not a TxInterfaceType: " + baseType->str() );
//    }

public:
    TxInterfaceTypeClassHandler() : TxTypeClassHandler( TXTC_INTERFACE )  { }

    // TODO: allow interfaces with proper is-a relationship to auto-convert (via adapter)

    virtual llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
};



class TxInterfaceAdapterTypeClassHandler : public TxTypeClassHandler {
public:
    TxInterfaceAdapterTypeClassHandler() : TxTypeClassHandler( TXTC_INTERFACEADAPTER )  { }

    virtual llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
};

/** Constitutes a type adapter from the adapted type to the interface type.
 * The interface type is the direct base type of the adapter type.
 * Technically, the type adapter is as concrete/abstract/generic-dependent as its adapted type.
 * However no instances of it are ever created - it is to be used as a reference target type. */
class TxInterfaceAdapterType : public TxActualType {
    // as adapters are intrinsically abstract, no instances are created and can't be any more assignable than interfaces
    const TxActualType* adaptedType;

//    TxInterfaceAdapterType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
//                            const std::vector<const TxActualType*>& interfaces, const TxActualType* adaptedType )
//            : TxActualType( TXTC_INTERFACEADAPTER, declaration, baseType, adaptedType->is_mutable(), interfaces ), adaptedType( adaptedType ) {
//    }

protected:
//    virtual TxInterfaceAdapterType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
//                                                           bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
//        // Note: Only for equivalent derivations including modifiable.
//        if ( dynamic_cast<const TxInterfaceAdapterType*>( baseType ) )
//            return new TxInterfaceAdapterType( declaration, baseType, interfaces, this->adaptedType );
//        throw std::logic_error(
//                "Specified a base type for TxInterfaceAdapterType that was not a TxInterfaceAdapterType: " + baseType->str() );
//    }

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
