#pragma once

#include "type_base.hpp"

class TxBoolType : public TxActualType {
protected:
    virtual TxBoolType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                               bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
        if ( dynamic_cast<const TxBoolType*>( baseType ) )
            return new TxBoolType( declaration, baseType, interfaces );
        throw std::logic_error( "Specified a base type for TxBoolType that was not a TxBoolType: " + baseType->str() );
    }
    ;

public:
    TxBoolType( const TxTypeDeclaration* declaration, const TxActualType* baseType, const std::vector<const TxActualType*>& interfaces )
            : TxActualType( TXTC_ELEMENTARY, declaration, baseType, true, interfaces ) {
    }

    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override final;
    virtual llvm::Type* make_llvm_externc_type( LlvmGenerationContext& context ) const override final;
};

class TxScalarType : public TxActualType {
protected:
    const uint64_t _size;

    TxScalarType( const TxTypeDeclaration* declaration, const TxActualType* baseType, const std::vector<const TxActualType*>& interfaces,
                  uint64_t size )
            : TxActualType( TXTC_ELEMENTARY, declaration, baseType, true, interfaces ), _size( size ) {
    }

public:
    virtual uint64_t size() const {
        return this->_size;
    }

    /** This is legal to invoke during analysis passes. It is used for constant expression evaluation. */
    virtual llvm::Type* get_scalar_llvm_type( LlvmGenerationContext& context ) const = 0;

    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override final;
    virtual llvm::Type* make_llvm_externc_type( LlvmGenerationContext& context ) const override final;
};

class TxIntegerType final : public TxScalarType {
protected:
    virtual TxIntegerType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                                  bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
        if ( const TxIntegerType* intType = dynamic_cast<const TxIntegerType*>( baseType ) )
            return new TxIntegerType( declaration, baseType, interfaces, intType->size(), intType->sign );
        throw std::logic_error( "Specified a base type for TxIntegerType that was not a TxIntegerType: " + baseType->str() );
    }
    ;

public:
    const bool sign;

    TxIntegerType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                   const std::vector<const TxActualType*>& interfaces,
                   int size,
                   bool sign )
            : TxScalarType( declaration, baseType, interfaces, size ), sign( sign ) {
    }

    inline bool is_signed() const {
        return this->sign;
    }

    virtual bool auto_converts_to( const TxActualType& destination ) const override {
        if ( const TxIntegerType* destInt = dynamic_cast<const TxIntegerType*>( &destination ) ) {
            if ( this->sign == destInt->sign )
                return this->_size <= destInt->_size;
            else
                return destInt->sign && this->_size < destInt->_size;
        }
        return false;
    }

    virtual llvm::Type* get_scalar_llvm_type( LlvmGenerationContext& context ) const override;
};

class TxFloatingType final : public TxScalarType {
protected:
    virtual TxFloatingType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                                   bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
        if ( const TxFloatingType* floatType = dynamic_cast<const TxFloatingType*>( baseType ) )
            return new TxFloatingType( declaration, baseType, interfaces, floatType->size() );
        throw std::logic_error( "Specified a base type for TxFloatingType that was not a TxFloatingType: " + baseType->str() );
    }
    ;

public:
    TxFloatingType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                    const std::vector<const TxActualType*>& interfaces, int size )
            : TxScalarType( declaration, baseType, interfaces, size ) {
    }

    virtual bool auto_converts_to( const TxActualType& destination ) const override {
        if ( const TxFloatingType* destFloat = dynamic_cast<const TxFloatingType*>( &destination ) )
            return this->_size <= destFloat->_size;
        return false;
    }

    virtual llvm::Type* get_scalar_llvm_type( LlvmGenerationContext& context ) const override;
};
