#pragma once

#include "type_base.hpp"


class TxScalarType : public TxType {
protected:
    const int _size;

    virtual TxScalarType* make_specialized_type(const TxTypeSpecialization& baseTypeSpec,
                                                const std::vector<TxTypeParam>& typeParams,
                                                std::string* errorMsg=nullptr) const override {
        throw std::logic_error("Can't specialize type " + this->to_string());
    };

    TxScalarType(const TxTypeSpecialization& baseTypeSpec, int size) : TxType(baseTypeSpec), _size(size) { }

public:
    virtual long size() const { return this->_size; }
    virtual bool is_immutable() const { return false; }
    virtual bool is_final() const { return true; }
    virtual bool is_abstract() const { return false; }

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }

//    virtual std::string to_string() const {
//        return std::string(this->is_modifiable() ? "MOD " : "") + typeid(*this).name() + "_" + std::to_string(this->_size*8) + "b";
//    }
};

class TxIntegerType : public TxScalarType {
protected:
    virtual TxIntegerType* make_specialized_type(const TxTypeSpecialization& baseTypeSpec,
            const std::vector<TxTypeParam>& typeParams,
            std::string* errorMsg=nullptr) const override {
        ASSERT(typeParams.empty(), "can't specify type parameters for " << this);
        ASSERT(baseTypeSpec.modifiable, "should only be used to create a modifiable type usage");
        if (const TxIntegerType* intType = dynamic_cast<const TxIntegerType*>(baseTypeSpec.type))
            return new TxIntegerType(baseTypeSpec, intType->size(), intType->sign);
        throw std::logic_error("Specified a base type for TxIntegerType that was not a TxIntegerType: " + baseTypeSpec.type->to_string());
    };

    TxIntegerType(const TxTypeSpecialization& baseTypeSpec, int size, bool sign)
        : TxScalarType(baseTypeSpec, size), sign(sign) { }

public:
    const bool sign;

    TxIntegerType(const TxType* baseType, int size, bool sign)
        : TxScalarType(TxTypeSpecialization(baseType), size), sign(sign) { }


    inline virtual bool operator==(const TxType& other) const {
        return (typeid(*this) == typeid(other)
                && this->sign == ((TxIntegerType&)other).sign
                && this->_size == ((TxIntegerType&)other)._size);
    }

    virtual bool innerAutoConvertsFrom(const TxType& otherType) const {
        if (const TxIntegerType* intType = dynamic_cast<const TxIntegerType*>(&otherType)) {
            if (this->sign == intType->sign)
                return this->_size >= intType->_size;
            else
                return this->sign && this->_size > intType->_size;
        }
        return false;
    }
};

class TxFloatingType : public TxScalarType {
protected:
    virtual TxFloatingType* make_specialized_type(const TxTypeSpecialization& baseTypeSpec,
            const std::vector<TxTypeParam>& typeParams,
            std::string* errorMsg=nullptr) const override {
        ASSERT(typeParams.empty(), "can't specify type parameters for " << this);
        ASSERT(baseTypeSpec.modifiable, "should only be used to create a modifiable type usage");
        if (const TxFloatingType* floatType = dynamic_cast<const TxFloatingType*>(baseTypeSpec.type))
            return new TxFloatingType(baseTypeSpec, floatType->size());
        throw std::logic_error("Specified a base type for TxFloatingType that was not a TxFloatingType: " + baseTypeSpec.type->to_string());
    };

    TxFloatingType(const TxTypeSpecialization& baseTypeSpec, int size) : TxScalarType(baseTypeSpec, size) { }

public:
    TxFloatingType(const TxType* baseType, int size)
        : TxScalarType(TxTypeSpecialization(baseType), size) { }

    virtual bool operator==(const TxType& other) const {
        return (typeid(*this) == typeid(other)
                && this->_size == ((TxFloatingType&)other)._size);
    }

    virtual bool innerAutoConvertsFrom(const TxType& otherType) const {
        if (const TxFloatingType* floatType = dynamic_cast<const TxFloatingType*>(&otherType))
            return this->_size >= floatType->_size;
        return false;
    }
};
