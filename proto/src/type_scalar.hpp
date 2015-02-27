#pragma once

#include "type_base.hpp"


class TxBoolType : public TxType {
protected:
    virtual TxBoolType* make_specialized_type(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                                 const std::vector<TxTypeParam>& typeParams,
                                                 std::string* errorMsg=nullptr) const override {
        ASSERT(typeParams.empty(), "can't specify type parameters for " << this);
        if (dynamic_cast<const TxBoolType*>(baseTypeSpec.type))
            return new TxBoolType(entity, baseTypeSpec);
        throw std::logic_error("Specified a base type for TxBoolType that was not a TxBoolType: " + baseTypeSpec.type->to_string());
    };

public:
    TxBoolType(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec)
        : TxType(entity, baseTypeSpec) { }

    virtual TxTypeClass get_type_class() const override { return TXTC_ELEMENTARY; }
    virtual bool is_immutable() const override { return false; }
    virtual bool is_final() const override { return true; }
    virtual bool is_abstract() const override { return false; }

    inline virtual bool operator==(const TxType& other) const override {
        return ( typeid(*this) == typeid(other) );
    }

    virtual bool innerAutoConvertsFrom(const TxType& other) const override {
        // should we auto-convert from Null (false)?
        // should we auto-convert from integers (0 => false)?
        return ( typeid(*this) == typeid(other) );
    }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};



class TxScalarType : public TxType {
protected:
    const uint64_t _size;

    virtual TxScalarType* make_specialized_type(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                                const std::vector<TxTypeParam>& typeParams,
                                                std::string* errorMsg=nullptr) const override {
        throw std::logic_error("Can't specialize type " + this->to_string());
    };

    TxScalarType(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec, uint64_t size)
        : TxType(entity, baseTypeSpec), _size(size) { }

public:
    virtual uint64_t size() const { return this->_size; }
    virtual TxTypeClass get_type_class() const override { return TXTC_ELEMENTARY; }
    virtual bool is_immutable() const override { return false; }
    virtual bool is_final() const override { return true; }
    virtual bool is_abstract() const override { return false; }

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};

class TxIntegerType final : public TxScalarType {
protected:
    virtual TxIntegerType* make_specialized_type(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                                 const std::vector<TxTypeParam>& typeParams,
                                                 std::string* errorMsg=nullptr) const override {
        ASSERT(typeParams.empty(), "can't specify type parameters for " << this);
        if (const TxIntegerType* intType = dynamic_cast<const TxIntegerType*>(baseTypeSpec.type))
            return new TxIntegerType(entity, baseTypeSpec, intType->size(), intType->sign);
        throw std::logic_error("Specified a base type for TxIntegerType that was not a TxIntegerType: " + baseTypeSpec.type->to_string());
    };

    TxIntegerType(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec, int size, bool sign)
        : TxScalarType(entity, baseTypeSpec, size), sign(sign) { }

public:
    const bool sign;

    TxIntegerType(TxTypeEntity* entity, const TxType* baseType, int size, bool sign)
        : TxScalarType(entity, TxTypeSpecialization(baseType), size), sign(sign) { }


    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;

    inline virtual bool operator==(const TxType& other) const override {
        return (typeid(*this) == typeid(other)
                && this->sign == ((TxIntegerType&)other).sign
                && this->_size == ((TxIntegerType&)other)._size);
    }

    virtual bool innerAutoConvertsFrom(const TxType& otherType) const override {
        if (const TxIntegerType* intType = dynamic_cast<const TxIntegerType*>(&otherType)) {
            if (this->sign == intType->sign)
                return this->_size >= intType->_size;
            else
                return this->sign && this->_size > intType->_size;
        }
        return false;
    }
};

class TxFloatingType final : public TxScalarType {
protected:
    virtual TxFloatingType* make_specialized_type(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                                  const std::vector<TxTypeParam>& typeParams,
                                                  std::string* errorMsg=nullptr) const override {
        ASSERT(typeParams.empty(), "can't specify type parameters for " << this);
        if (const TxFloatingType* floatType = dynamic_cast<const TxFloatingType*>(baseTypeSpec.type))
            return new TxFloatingType(entity, baseTypeSpec, floatType->size());
        throw std::logic_error("Specified a base type for TxFloatingType that was not a TxFloatingType: " + baseTypeSpec.type->to_string());
    };

    TxFloatingType(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec, int size)
        : TxScalarType(entity, baseTypeSpec, size) { }

public:
    TxFloatingType(TxTypeEntity* entity, const TxType* baseType, int size)
        : TxScalarType(entity, TxTypeSpecialization(baseType), size) { }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;

    virtual bool operator==(const TxType& other) const override {
        return (typeid(*this) == typeid(other)
                && this->_size == ((TxFloatingType&)other)._size);
    }

    virtual bool innerAutoConvertsFrom(const TxType& otherType) const override {
        if (const TxFloatingType* floatType = dynamic_cast<const TxFloatingType*>(&otherType))
            return this->_size >= floatType->_size;
        return false;
    }
};
