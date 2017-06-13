#pragma once

#include "type_base.hpp"

class TxBoolType : public TxActualType {
protected:
    virtual TxBoolType* make_specialized_type( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                               bool mutableType, const std::vector<TxTypeSpecialization>& interfaces ) const override {
        if ( dynamic_cast<const TxBoolType*>( baseTypeSpec.type ) )
            return new TxBoolType( declaration, baseTypeSpec, interfaces );
        throw std::logic_error( "Specified a base type for TxBoolType that was not a TxBoolType: " + baseTypeSpec.type->str() );
    }
    ;

public:
    TxBoolType( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec, const std::vector<TxTypeSpecialization>& interfaces )
            : TxActualType( TXTC_ELEMENTARY, declaration, baseTypeSpec, true, interfaces ) {
    }

//    inline virtual bool operator==( const TxActualType& other ) const override {
//        return ( this == &other );
//    }

    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override;
};

class TxScalarType : public TxActualType {
protected:
    const uint64_t _size;

    TxScalarType( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec, const std::vector<TxTypeSpecialization>& interfaces,
                  uint64_t size )
            : TxActualType( TXTC_ELEMENTARY, declaration, baseTypeSpec, true, interfaces ), _size( size ) {
    }

public:
    virtual uint64_t size() const {
        return this->_size;
    }

//    inline virtual bool operator==( const TxActualType& other ) const override final {
//        return ( this == &other );
//    }

    /** This is legal to invoke during analysis passes. It is used for constant expression evaluation. */
    virtual llvm::Type* get_scalar_llvm_type( LlvmGenerationContext& context ) const = 0;

    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override final;
};

class TxIntegerType final : public TxScalarType {
protected:
    virtual TxIntegerType* make_specialized_type( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                  bool mutableType, const std::vector<TxTypeSpecialization>& interfaces ) const override {
        if ( const TxIntegerType* intType = dynamic_cast<const TxIntegerType*>( baseTypeSpec.type ) )
            return new TxIntegerType( declaration, baseTypeSpec, interfaces, intType->size(), intType->sign );
        throw std::logic_error( "Specified a base type for TxIntegerType that was not a TxIntegerType: " + baseTypeSpec.type->str() );
    }
    ;

public:
    const bool sign;

    TxIntegerType( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                   const std::vector<TxTypeSpecialization>& interfaces,
                   int size,
                   bool sign )
            : TxScalarType( declaration, baseTypeSpec, interfaces, size ), sign( sign ) {
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
    virtual TxFloatingType* make_specialized_type( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                   bool mutableType, const std::vector<TxTypeSpecialization>& interfaces ) const override {
        if ( const TxFloatingType* floatType = dynamic_cast<const TxFloatingType*>( baseTypeSpec.type ) )
            return new TxFloatingType( declaration, baseTypeSpec, interfaces, floatType->size() );
        throw std::logic_error( "Specified a base type for TxFloatingType that was not a TxFloatingType: " + baseTypeSpec.type->str() );
    }
    ;

public:
    TxFloatingType( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                    const std::vector<TxTypeSpecialization>& interfaces, int size )
            : TxScalarType( declaration, baseTypeSpec, interfaces, size ) {
    }

    virtual bool auto_converts_to( const TxActualType& destination ) const override {
        if ( const TxFloatingType* destFloat = dynamic_cast<const TxFloatingType*>( &destination ) )
            return this->_size <= destFloat->_size;
        return false;
    }

    virtual llvm::Type* get_scalar_llvm_type( LlvmGenerationContext& context ) const override;
};
