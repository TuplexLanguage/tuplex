#pragma once

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
    TxArrayType(const TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>())
            : TxType(entity, baseTypeSpec, typeParams)  { }

protected:
    virtual TxArrayType* make_specialized_type(const TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                               const std::vector<TxTypeParam>& typeParams,
                                               std::string* errorMsg=nullptr) const override {
        if (! dynamic_cast<const TxArrayType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxArrayType that was not a TxArrayType: " + baseTypeSpec.type->to_string());
        return new TxArrayType(entity, baseTypeSpec, typeParams);
    };

public:
    /** Creates the Array base type (no element type nor length specified). Only one such instance should exist. */
    TxArrayType(const TxTypeEntity* entity, const TxType* anyType, const TxType* uintType)
            : TxType(entity, TxTypeSpecialization(anyType),
                     std::vector<TxTypeParam>( { TxTypeParam(TxTypeParam::TXB_TYPE,  "E", anyType),
                                                 TxTypeParam(TxTypeParam::TXB_VALUE, "L", uintType) } ) ) { }


    const TxTypeProxy* element_type() const {
        return this->resolve_param_type("E");
    }
    const TxConstantProxy* length() const {
        return this->resolve_param_value("L");
    }


    bool is_builtin() const { return typeid(*this) != typeid(*this->baseTypeSpec.type); }

    long size() const {
        if (! this->is_concrete())
            throw std::logic_error("Can't get size of abstract or generic type: " + this->to_string());
        return this->element_type()->get_type()->size() * this->length()->get_int_value();
    }

    virtual bool is_abstract() const { return false; }

    virtual bool innerAutoConvertsFrom(const TxType& otherType) const {
        if (const TxArrayType* otherArray = dynamic_cast<const TxArrayType*>(&otherType)) {
            // if other has unbound type params that this does not, other is more generic and can't be auto-converted to this
            if (auto e = this->resolve_param_type("E")) {
                if (auto otherE = otherArray->resolve_param_type("E")) {
                    // note: is-a test insufficient for array elements, since same concrete type (same size) required
                    if (*e->get_type() != *otherE->get_type())
                        return false;
                }
                else
                    return false;  // other has not bound E
            }
            if (auto len = this->resolve_param_value("L")) {
                if (auto otherLen = otherArray->resolve_param_value("L")) {
                    if (len->get_int_value() != otherLen->get_int_value())
                        return false;
                }
                else
                    return false;  // other has not bound L
            }
            return true;
        }
        return false;
    }

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};



class TxReferenceType : public TxType {
    TxReferenceType(const TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                    const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>())
            : TxType(entity, baseTypeSpec, typeParams)  { }

protected:
    virtual TxReferenceType* make_specialized_type(const TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                                   const std::vector<TxTypeParam>& typeParams,
                                                   std::string* errorMsg=nullptr) const override {
        if (! dynamic_cast<const TxReferenceType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxReferenceType that was not a TxReferenceType: " + baseTypeSpec.type->to_string());
        return new TxReferenceType(entity, baseTypeSpec, typeParams);
    };

public:
    /** Creates the Reference base type (no target type specified). Only one such instance should exist. */
    TxReferenceType(const TxTypeEntity* entity, const TxType* anyType)
            : TxType(entity, TxTypeSpecialization(anyType),
                     std::vector<TxTypeParam>( { TxTypeParam(TxTypeParam::TXB_TYPE, "T", anyType) } ) ) { }


    /** Returns proxy representing the target type of this reference type, or nullptr if this reference type is generic. */
    const TxTypeProxy* target_type() const {
        return this->resolve_param_type("T", true);
    }

    bool is_builtin() const { return typeid(*this) != typeid(*this->baseTypeSpec.type); }

    long size() const { return 8; }

    virtual bool is_final() const { return true; }
    virtual bool is_abstract() const { return false; }

    /** Returns true if this type is concrete (i.e. can be directly instanced).
     * A concrete type is not abstract, nor usually generic (references may be concrete while generic). */
    virtual bool is_concrete() const { return true; }
    // FUTURE: might be abstract when unknown whether independent object or member ref?

    virtual bool innerAutoConvertsFrom(const TxType& otherType) const {
        if (const TxReferenceType* otherRef = dynamic_cast<const TxReferenceType*>(&otherType)) {
            // if other has unbound type params that this does not, other is more generic and can't be auto-converted to this
            if (auto target = this->target_type()) {
                if (auto otherTarget = otherRef->target_type()) {
                    // is-a test sufficient for reference targets (it isn't for arrays, which require same concrete type)
                    //std::cout << "CHECKING AUTOCONV FROM\n" << *otherTarget->get_type() << "\nTO\n" << *target->get_type() << std::endl;
                    if (! otherTarget->get_type()->is_a(*target->get_type()))
                        return false;
                    else if (target->get_type()->is_modifiable() && !otherTarget->get_type()->is_modifiable())
                        return false;  // can't lose modifiable attribute of target
                    else
                        return true;
                }
                else
                    return false;  // other has not bound T
            }
            else
                return true;
        }
        else
            return false;
    }

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};



class TxFunctionType : public TxType {
    /** Indicates whether functions of this type may modify its closure when run. */
    const bool modifiableClosure;

    TxFunctionType(const TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec, const std::vector<TxTypeParam>& typeParams,
                   const std::vector<const TxType*>& argumentTypes, const TxType* returnType=nullptr,
                   bool modifiableClosure=false)
            : TxType(entity, baseTypeSpec, typeParams), modifiableClosure(modifiableClosure),
              argumentTypes(argumentTypes), returnType(returnType)  { }

protected:
    virtual TxFunctionType* make_specialized_type(const TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
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

    TxFunctionType(const TxTypeEntity* entity, const TxType* baseType, const std::vector<const TxType*>& argumentTypes,
                   const TxType* returnType=nullptr, bool modifiableClosure=false)
        : TxType(entity, TxTypeSpecialization(baseType)),
          modifiableClosure(modifiableClosure), argumentTypes(argumentTypes), returnType(returnType)  { }

    bool hasReturnValue() const  { return this->returnType != nullptr; }

    bool is_builtin() const { return typeid(*this) != typeid(*this->baseTypeSpec.type); }

    long size() const { return 8; }

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
    TxBuiltinFunctionType(const TxTypeEntity* entity, const TxType* baseType, const std::vector<const TxType*> argumentTypes, const TxType* returnType)
        : TxFunctionType(entity, baseType, argumentTypes, returnType) { }

    bool is_builtin() const { return true; }
};

class TxBuiltinConversionFunctionType : public TxBuiltinFunctionType {
public:
    TxBuiltinConversionFunctionType(const TxTypeEntity* entity, const TxType* baseType, const TxType* argumentType, const TxType* returnType)
        : TxBuiltinFunctionType(entity, baseType, std::vector<const TxType*>{ argumentType }, returnType) { }
};



/** Special, internal type that represents a group of differing function types.
 * Used to resolve function name overloading.
 */
class TxFunctionGroupType : public TxType {
    std::vector<const TxFunctionType*> functionTypes;

    TxFunctionGroupType* make_specialized_type(const TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                               const std::vector<TxTypeParam>& typeParams,
                                               std::string* errorMsg=nullptr) const override {
        throw std::logic_error("Can't specialize type " + this->to_string());
    };

public:
    TxFunctionGroupType(const TxTypeEntity* entity) : TxType(entity) { }
    TxFunctionGroupType(const TxTypeEntity* entity, const TxFunctionType* funcType) : TxType(entity) {
        this->functionTypes.push_back(funcType);
    }

    long size() const { throw std::logic_error("Can't get size of abstract type " + this->to_string()); }

    inline void add(const TxFunctionType* funcType) { this->functionTypes.push_back(funcType); }

    inline std::vector<const TxFunctionType*>::const_iterator cbegin() const noexcept { return this->functionTypes.cbegin(); }
    inline std::vector<const TxFunctionType*>::const_iterator cend() const noexcept { return this->functionTypes.cend(); }
};



class TxTupleType : public TxType {
    // Indicates if this type is *not* immutable, in which case its instances may be declared modifiable.
    const bool _mutable;
    const bool abstract = false;

    TxTupleType(const TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec, const std::vector<TxTypeParam>& typeParams, bool _mutable=false)
            : TxType(entity, baseTypeSpec, typeParams), _mutable(_mutable)  { }

protected:
    virtual TxTupleType* make_specialized_type(const TxTypeEntity* entity, const TxTypeSpecialization& baseTypeSpec,
                                               const std::vector<TxTypeParam>& typeParams,
                                               std::string* errorMsg=nullptr) const override {
        if (auto tupleBaseType = dynamic_cast<const TxTupleType*>(baseTypeSpec.type))
            return new TxTupleType(entity, baseTypeSpec, typeParams, tupleBaseType->_mutable);
        throw std::logic_error("Specified a base type for TxTupleType that was not a TxTupleType: " + baseTypeSpec.type->to_string());
    };

public:
    TxTupleType(const TxTypeEntity* entity, const TxType* baseType, bool _mutable=false)
            : TxType(entity, TxTypeSpecialization(baseType)), _mutable(_mutable)  {
        ASSERT(entity, "NULL entity");
    }

    bool is_builtin() const { return typeid(*this) != typeid(*this->baseTypeSpec.type); }

    long size() const { return 8; }  // FIXME

    virtual bool is_abstract() const { return this->abstract; }

    virtual bool is_immutable() const { return !this->_mutable; }

    virtual bool innerAutoConvertsFrom(const TxType& someType) const {
        return (*this) == someType;
    }

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};
