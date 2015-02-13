#pragma once

#include "logging.hpp"

#include "type_base.hpp"


//class TxInterfaceType : public TxType {
//public:
//    TxInterfaceType() : TxType() { }
//};

//class TxUnionType : public TxObjectType {
//};

//class TxTupleType : public TxObjectType {
//public:
//  TxTupleType() : TxObjectType()  { }
//};


class TxArrayType : public TxType {
    TxArrayType(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>())
            : TxType(entity, baseTypeSpec, typeParams)  { }

protected:
    virtual TxArrayType* make_specialized_type(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                               const std::vector<TxTypeParam>& typeParams,
                                               std::string* errorMsg=nullptr) const override {
        if (! dynamic_cast<const TxArrayType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxArrayType that was not a TxArrayType: " + baseTypeSpec.type->to_string());
        return new TxArrayType(entity, baseTypeSpec, typeParams);
    };

public:
    /** Creates the Array base type (no element type nor length specified). Only one such instance should exist. */
    TxArrayType(TxTypeEntity* entity, const TxType* anyType, const TxType* uintType)
            : TxType(entity, TxTypeSpecialization(anyType),
                     std::vector<TxTypeParam>( { TxTypeParam(TxTypeParam::TXB_TYPE,  "E", anyType),
                                                 TxTypeParam(TxTypeParam::TXB_VALUE, "L", uintType) } ) ) { }


    inline const TxTypeProxy* element_type() const {
        const TxTypeProxy* etype = this->resolve_param_type("tx#Array#E", true);
        if (! etype)
            Logger::get("PARSER").warning("NULL element type for array %s", this->to_string().c_str());
        //ASSERT(etype, "NULL element type for array " << this);
        return etype;
    }
    inline const TxExpressionNode* length() const {
        //const TxConstantProxy* len = this->resolve_param_value("tx#Array#L");
        const TxExpressionNode* len = this->resolve_param_value("L");
        if (! len)
            Logger::get("PARSER").warning("NULL length proxy for array %s", this->to_string().c_str());
        // ASSERT(len, "NULL length proxy for array " << this);
        return len;
    }


    virtual bool is_abstract() const { return false; }

    virtual bool innerAutoConvertsFrom(const TxType& otherType) const override;

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};



class TxReferenceType : public TxType {
    TxReferenceType(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                    const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>())
            : TxType(entity, baseTypeSpec, typeParams)  { }

protected:
    virtual TxReferenceType* make_specialized_type(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                                   const std::vector<TxTypeParam>& typeParams,
                                                   std::string* errorMsg=nullptr) const override {
        if (! dynamic_cast<const TxReferenceType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxReferenceType that was not a TxReferenceType: " + baseTypeSpec.type->to_string());
        return new TxReferenceType(entity, baseTypeSpec, typeParams);
    };

public:
    /** Creates the Reference base type (no target type specified). Only one such instance should exist. */
    TxReferenceType(TxTypeEntity* entity, const TxType* anyType)
            : TxType(entity, TxTypeSpecialization(anyType),
                     std::vector<TxTypeParam>( { TxTypeParam(TxTypeParam::TXB_TYPE, "T", anyType) } ) ) { }


    /** Returns proxy representing the target type of this reference type, or nullptr if this reference type is generic. */
    inline const TxTypeProxy* target_type() const {
        const TxTypeProxy* ttype = this->resolve_param_type("tx#Ref#T", true);
        if (! ttype)
            Logger::get("PARSER").warning("NULL target type for reference %s", this->to_string().c_str());
        //ASSERT(ttype, "NULL target type for reference " << this);
        return ttype;
    }

    virtual bool is_final() const { return true; }
    virtual bool is_abstract() const { return false; }

    /** Returns true if this type is concrete (i.e. can be directly instanced).
     * A concrete type is not abstract, nor usually generic (references may be concrete while generic). */
    virtual bool is_concrete() const { return true; }
    // FUTURE: might be abstract when unknown whether independent object or member ref?

    virtual bool innerAutoConvertsFrom(const TxType& otherType) const override;

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};



class TxFunctionType : public TxType {
    /** Indicates whether functions of this type may modify its closure when run. */
    const bool modifiableClosure;

    TxFunctionType(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec, const std::vector<TxTypeParam>& typeParams,
                   const std::vector<const TxType*>& argumentTypes, const TxType* returnType=nullptr,
                   bool modifiableClosure=false)
            : TxType(entity, baseTypeSpec, typeParams), modifiableClosure(modifiableClosure),
              argumentTypes(argumentTypes), returnType(returnType)  { }

protected:
    virtual TxFunctionType* make_specialized_type(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                                  const std::vector<TxTypeParam>& typeParams,
                                                  std::string* errorMsg=nullptr) const override {
        if (auto funcBaseType = dynamic_cast<const TxFunctionType*>(baseTypeSpec.type))
            return new TxFunctionType(entity, baseTypeSpec, typeParams, funcBaseType->argumentTypes,
                                      funcBaseType->returnType, funcBaseType->modifiableClosure);
        throw std::logic_error("Specified a base type for TxFunctionType that was not a TxFunctionType: " + baseTypeSpec.type->to_string());
    };

public:
    const std::vector<const TxType*> argumentTypes;
    TxType const * const returnType;

    TxFunctionType(TxTypeEntity* entity, const TxType* baseType, const std::vector<const TxType*>& argumentTypes,
                   const TxType* returnType=nullptr, bool modifiableClosure=false)
        : TxType(entity, TxTypeSpecialization(baseType)),
          modifiableClosure(modifiableClosure), argumentTypes(argumentTypes), returnType(returnType)  { }

    bool hasReturnValue() const  { return this->returnType != nullptr; }

    virtual bool is_abstract() const { return false; }

    virtual bool is_immutable() const { return !this->modifiableClosure; }

    inline virtual bool operator==(const TxType& other) const {
        return (typeid(*this) == typeid(other) &&
                *this->returnType == *((TxFunctionType&)other).returnType &&
                this->argumentTypes == ((TxFunctionType&)other).argumentTypes);
    }

    virtual bool innerAutoConvertsFrom(const TxType& someType) const {
        return (*this) == someType;  // FUTURE: allow polymorphic compatibility
    }

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};

class TxBuiltinFunctionType : public TxFunctionType {
public:
    TxBuiltinFunctionType(TxTypeEntity* entity, const TxType* baseType, const std::vector<const TxType*> argumentTypes, const TxType* returnType)
        : TxFunctionType(entity, baseType, argumentTypes, returnType) { }
};

class TxBuiltinConversionFunctionType : public TxBuiltinFunctionType {
public:
    TxBuiltinConversionFunctionType(TxTypeEntity* entity, const TxType* baseType, const TxType* argumentType, const TxType* returnType)
        : TxBuiltinFunctionType(entity, baseType, std::vector<const TxType*>{ argumentType }, returnType) { }
};



/** Special, internal type that represents a group of differing function types.
 * Used to resolve function name overloading.
 */
class TxFunctionGroupType : public TxType {
    std::vector<const TxFunctionType*> functionTypes;

    TxFunctionGroupType* make_specialized_type(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                               const std::vector<TxTypeParam>& typeParams,
                                               std::string* errorMsg=nullptr) const override {
        throw std::logic_error("Can't specialize type " + this->to_string());
    };

public:
    TxFunctionGroupType(TxTypeEntity* entity) : TxType(entity) { }
    TxFunctionGroupType(TxTypeEntity* entity, const TxFunctionType* funcType) : TxType(entity) {
        this->functionTypes.push_back(funcType);
    }

    inline void add(const TxFunctionType* funcType) { this->functionTypes.push_back(funcType); }

    inline std::vector<const TxFunctionType*>::const_iterator cbegin() const noexcept { return this->functionTypes.cbegin(); }
    inline std::vector<const TxFunctionType*>::const_iterator cend() const noexcept { return this->functionTypes.cend(); }
};



class TxTupleType : public TxType {
    // Indicates if this type is *not* immutable, in which case its instances may be declared modifiable.
    const bool _mutable;
    const bool abstract = false;

    TxTupleType(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec, const std::vector<TxTypeParam>& typeParams, bool _mutable=false)
            : TxType(entity, baseTypeSpec, typeParams), _mutable(_mutable)  { }

protected:
    virtual TxTupleType* make_specialized_type(TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                               const std::vector<TxTypeParam>& typeParams,
                                               std::string* errorMsg=nullptr) const override {
        if (auto tupleBaseType = dynamic_cast<const TxTupleType*>(baseTypeSpec.type))
            return new TxTupleType(entity, baseTypeSpec, typeParams, tupleBaseType->_mutable);
        throw std::logic_error("Specified a base type for TxTupleType that was not a TxTupleType: " + baseTypeSpec.type->to_string());
    };

public:
    TxTupleType(TxTypeEntity* entity, const TxType* baseType, bool _mutable=false)
            : TxType(entity, TxTypeSpecialization(baseType)), _mutable(_mutable)  {
        ASSERT(entity, "NULL entity");
    }

    virtual bool is_abstract() const { return this->abstract; }

    virtual bool is_immutable() const { return !this->_mutable; }

    virtual bool innerAutoConvertsFrom(const TxType& someType) const {
        return (*this) == someType;
    }

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};
