#pragma once

#include "type_base.hpp"


class TxBoolType : public TxType {
protected:
    virtual TxBoolType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                              const std::vector<TxTypeSpecialization>& interfaces) const override {
        if (dynamic_cast<const TxBoolType*>(baseTypeSpec.type))
            return new TxBoolType(declaration, baseTypeSpec);
        throw std::logic_error("Specified a base type for TxBoolType that was not a TxBoolType: " + baseTypeSpec.type->to_string());
    };

public:
    TxBoolType(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec)
        : TxType(TXTC_ELEMENTARY, declaration, baseTypeSpec) { }

    virtual bool is_final() const override { return true; }
    //virtual bool is_abstract() const override { return false; }

    inline virtual bool operator==(const TxType& other) const override {
        return ( typeid(*this) == typeid(other) );
    }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};



class TxScalarType : public TxType {
protected:
    const uint64_t _size;

    TxScalarType(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec, uint64_t size)
        : TxType(TXTC_ELEMENTARY, declaration, baseTypeSpec), _size(size) { }

public:
    virtual uint64_t size() const { return this->_size; }
    virtual bool is_final() const override { return true; }
    //virtual bool is_abstract() const override { return false; }

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};

class TxIntegerType final : public TxScalarType {
protected:
    virtual TxIntegerType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                 const std::vector<TxTypeSpecialization>& interfaces) const override {
        if (const TxIntegerType* intType = dynamic_cast<const TxIntegerType*>(baseTypeSpec.type))
            return new TxIntegerType(declaration, baseTypeSpec, intType->size(), intType->sign);
        throw std::logic_error("Specified a base type for TxIntegerType that was not a TxIntegerType: " + baseTypeSpec.type->to_string());
    };

    TxIntegerType(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec, int size, bool sign)
        : TxScalarType(declaration, baseTypeSpec, size), sign(sign) { }

public:
    const bool sign;

    TxIntegerType(const TxTypeDeclaration* declaration, const TxType* baseType, int size, bool sign)
        : TxScalarType(declaration, TxTypeSpecialization(baseType), size), sign(sign) { }


    inline bool is_signed() const { return this->sign; }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;

    inline virtual bool operator==(const TxType& other) const override {
        return (typeid(*this) == typeid(other)
                && this->sign == ((TxIntegerType&)other).sign
                && this->_size == ((TxIntegerType&)other)._size);
    }

    virtual bool auto_converts_to(const TxType& destination) const override {
        if (const TxIntegerType* destInt = dynamic_cast<const TxIntegerType*>(&destination)) {
            if (this->sign == destInt->sign)
                return this->_size <= destInt->_size;
            else
                return destInt->sign && this->_size < destInt->_size;
        }
        return false;
    }
};

class TxFloatingType final : public TxScalarType {
protected:
    virtual TxFloatingType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                  const std::vector<TxTypeSpecialization>& interfaces) const override {
        if (const TxFloatingType* floatType = dynamic_cast<const TxFloatingType*>(baseTypeSpec.type))
            return new TxFloatingType(declaration, baseTypeSpec, floatType->size());
        throw std::logic_error("Specified a base type for TxFloatingType that was not a TxFloatingType: " + baseTypeSpec.type->to_string());
    };

    TxFloatingType(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec, int size)
        : TxScalarType(declaration, baseTypeSpec, size) { }

public:
    TxFloatingType(const TxTypeDeclaration* declaration, const TxType* baseType, int size)
        : TxScalarType(declaration, TxTypeSpecialization(baseType), size) { }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;

    virtual bool operator==(const TxType& other) const override {
        return (typeid(*this) == typeid(other)
                && this->_size == ((TxFloatingType&)other)._size);
    }

    virtual bool auto_converts_to(const TxType& destination) const override {
        if (const TxFloatingType* destFloat = dynamic_cast<const TxFloatingType*>(&destination))
            return this->_size <= destFloat->_size;
        return false;
    }
};
