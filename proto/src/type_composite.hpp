#pragma once

#include <algorithm>

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
    TxArrayType(TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>())
            : TxType(TXTC_ARRAY, declaration, baseTypeSpec, typeParams)  { }

protected:
    virtual TxArrayType* make_specialized_type(TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                               const std::vector<TxTypeParam>& typeParams,
                                               std::string* errorMsg=nullptr) const override {
        if (! dynamic_cast<const TxArrayType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxArrayType that was not a TxArrayType: " + baseTypeSpec.type->to_string());
        return new TxArrayType(declaration, baseTypeSpec, typeParams);
    };

public:
    /** Creates the Array base type (no element type nor length specified). Only one such instance should exist. */
    TxArrayType(TxTypeDeclaration* declaration, const TxType* anyType, TxTypeDefiner* anyTypeDefiner, TxTypeDefiner* uintTypeDefiner)
            : TxType(TXTC_ARRAY, declaration, TxTypeSpecialization(anyType),
                     std::vector<TxTypeParam>( { TxTypeParam(TxTypeParam::TXB_TYPE,  "E", anyTypeDefiner),
                                                 TxTypeParam(TxTypeParam::TXB_VALUE, "L", uintTypeDefiner) } ) ) { }


    /** Returns nullptr if unbound. */
    inline const TxType* element_type() const {
        ResolutionContext resCtx;
        const TxType* etype = this->resolve_param_type(resCtx, "tx#Array#E", true);
        if (! etype)
            Logger::get("PARSER").warning("NULL element type for array %s", this->to_string().c_str());
        //ASSERT(etype, "NULL element type for array " << this);
        return etype;
    }

    /** Returns nullptr if unbound. */
    inline const TxExpressionNode* length() const {
        ResolutionContext resCtx;
        //const TxConstantProxy* len = this->resolve_param_value("tx#Array#L");
        const TxExpressionNode* len = this->resolve_param_value(resCtx, "L");
        return len;
    }


    virtual bool is_abstract() const { return false; }

    virtual bool is_statically_sized() const override;

    virtual bool innerAutoConvertsFrom(const TxType& otherType) const override;

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;
    virtual llvm::Value* gen_size(LlvmGenerationContext& context, GenScope* scope) const override;
    virtual llvm::Value* gen_alloca(LlvmGenerationContext& context, GenScope* scope, const std::string &varName="") const override;

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }

private:
    llvm::Value* inner_code_gen_size(LlvmGenerationContext& context, GenScope* scope, llvm::Value* elemSize, llvm::Value* arrayLen) const;
};



class TxReferenceType : public TxType {
    TxReferenceType(TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                    const std::vector<TxTypeParam>& typeParams=std::vector<TxTypeParam>())
            : TxType(TXTC_REFERENCE, declaration, baseTypeSpec, typeParams)  { }

protected:
    virtual TxReferenceType* make_specialized_type(TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                   const std::vector<TxTypeParam>& typeParams,
                                                   std::string* errorMsg=nullptr) const override {
        if (! dynamic_cast<const TxReferenceType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxReferenceType that was not a TxReferenceType: " + baseTypeSpec.type->to_string());
        return new TxReferenceType(declaration, baseTypeSpec, typeParams);
    };

public:
    /** Creates the Reference base type (no target type specified). Only one such instance should exist. */
    TxReferenceType(TxTypeDeclaration* declaration, const TxType* anyType, TxTypeDefiner* anyTypeDefiner)
            : TxType(TXTC_REFERENCE, declaration, TxTypeSpecialization(anyType),
                     std::vector<TxTypeParam>( { TxTypeParam(TxTypeParam::TXB_TYPE, "T", anyTypeDefiner) } ) ) { }


    /** Returns the target type of this reference type, or nullptr if it failed to resolve. */
    const TxType* target_type() const;

    virtual bool is_final() const { return true; }
    virtual bool is_abstract() const { return false; }

    /** Returns true if this type is concrete (i.e. can be directly instanced).
     * A concrete type is not abstract, nor usually generic (references may be concrete while generic). */
    virtual bool is_concrete() const { return true; }
    // FUTURE: might be abstract when unknown whether independent object or member ref?

    virtual bool innerAutoConvertsFrom(const TxType& otherType) const override;

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;

    static llvm::Type* make_ref_llvm_type(LlvmGenerationContext& context, llvm::Type* targetType);

    /** Casts a reference value from one type to another */
    static llvm::Value* gen_ref_conversion(LlvmGenerationContext& context, GenScope* scope,
                                           llvm::Value* origRefV, llvm::Type* targetRefT);

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};



class TxFunctionType : public TxType {
    /** Indicates whether functions of this type may modify its closure when run. */
    const bool modifiableClosure;

    TxFunctionType(TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec, const std::vector<TxTypeParam>& typeParams,
                   const std::vector<const TxType*>& argumentTypes, const TxType* returnType=nullptr,
                   bool modifiableClosure=false)
            : TxType(TXTC_FUNCTION, declaration, baseTypeSpec, typeParams), modifiableClosure(modifiableClosure),
              argumentTypes(argumentTypes), returnType(returnType)  { }

protected:
    virtual TxFunctionType* make_specialized_type(TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                  const std::vector<TxTypeParam>& typeParams,
                                                  std::string* errorMsg=nullptr) const override {
        if (auto funcBaseType = dynamic_cast<const TxFunctionType*>(baseTypeSpec.type))
            return new TxFunctionType(declaration, baseTypeSpec, typeParams, funcBaseType->argumentTypes,
                                      funcBaseType->returnType, funcBaseType->modifiableClosure);
        throw std::logic_error("Specified a base type for TxFunctionType that was not a TxFunctionType: " + baseTypeSpec.type->to_string());
    };

public:
    const std::vector<const TxType*> argumentTypes;
    TxType const * const returnType;

    TxFunctionType(TxTypeDeclaration* declaration, const TxType* baseType, const std::vector<const TxType*>& argumentTypes,
                   const TxType* returnType=nullptr, bool modifiableClosure=false)
        : TxType(TXTC_FUNCTION, declaration, TxTypeSpecialization(baseType)),
          modifiableClosure(modifiableClosure), argumentTypes(argumentTypes), returnType(returnType)  { }

    bool hasReturnValue() const  { return this->returnType != nullptr; }

    virtual bool is_abstract() const { return false; }

    virtual bool is_immutable() const { return !this->modifiableClosure; }

    inline virtual bool operator==(const TxType& other) const {
        if (auto otherF = dynamic_cast<const TxFunctionType*>(&other))
            return ( ( this->returnType == otherF->returnType
                       || ( this->returnType != nullptr && otherF->returnType != nullptr
                            && *this->returnType == *otherF->returnType ) )
                     && this->argumentTypes.size() == otherF->argumentTypes.size()
                     && std::equal(this->argumentTypes.cbegin(), this->argumentTypes.cend(),
                                   otherF->argumentTypes.cbegin(),
                                   [](const TxType* t1, const TxType* t2) { return *t1 == *t2; } ) );
        return false;
    }

    virtual bool innerAutoConvertsFrom(const TxType& someType) const {
        return (*this) == someType;  // FUTURE: allow polymorphic compatibility
    }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }

    virtual std::string to_string() const override {
        std::stringstream str;
        str << "func(";
        if (! this->argumentTypes.empty()) {
            auto ai = this->argumentTypes.cbegin();
            str << (*ai)->to_string(true);
            for (ai++; ai != this->argumentTypes.cend(); ai++)
                str << ", " << (*ai)->to_string(true);
        }
        str << ")";
        if (this->returnType)
            str << " -> " << this->returnType->to_string(true);
        return str.str();
    }
};

class TxConstructorType : public TxFunctionType {
    TxTypeDeclaration* objTypeDeclaration;
public:
    TxConstructorType(TxTypeDeclaration* declaration, const TxType* baseType, const std::vector<const TxType*> argumentTypes, TxTypeDeclaration* objTypeDeclaration)
        : TxFunctionType(declaration, baseType, argumentTypes, nullptr, true), objTypeDeclaration(objTypeDeclaration) { }

    TxTypeDeclaration* get_constructed_type_decl() const {
        return this->objTypeDeclaration;
    }
};

class TxBuiltinFunctionType : public TxFunctionType {
public:
    TxBuiltinFunctionType(TxTypeDeclaration* declaration, const TxType* baseType, const std::vector<const TxType*> argumentTypes, const TxType* returnType)
        : TxFunctionType(declaration, baseType, argumentTypes, returnType) { }
};

class TxBuiltinConversionFunctionType : public TxBuiltinFunctionType {
public:
    TxBuiltinConversionFunctionType(TxTypeDeclaration* declaration, const TxType* baseType, const TxType* argumentType, const TxType* returnType)
        : TxBuiltinFunctionType(declaration, baseType, std::vector<const TxType*>{ argumentType }, returnType) { }
};

/*
class TxStackConstructorType : public TxFunctionType {
public:
    TxStackConstructorType(TxTypeDeclaration* declaration, const TxType* baseType, const std::vector<const TxType*>& argumentTypes, const TxType* returnType)
        : TxFunctionType(declaration, baseType, argumentTypes, returnType) { }

//    // TO DO: make common "inlinable function" superclass that contains this method:
//    virtual llvm::Value* gen_inline_expr(LlvmGenerationContext& context, GenScope* scope) const;
};
*/


// no longer used
/* * Special, internal type that represents a group of differing function types.
 * Used to resolve function name overloading.
 * /
class TxFunctionGroupType : public TxType {
    std::vector<const TxFunctionType*> functionTypes;

    TxFunctionGroupType* make_specialized_type(TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                               const std::vector<TxTypeParam>& typeParams,
                                               std::string* errorMsg=nullptr) const override {
        throw std::logic_error("Can't specialize type " + this->to_string());
    };

public:
    TxFunctionGroupType(TxTypeDeclaration* declaration) : TxType(TXTC_FUNCTION, declaration) { }
    TxFunctionGroupType(TxTypeDeclaration* declaration, const TxFunctionType* funcType) : TxType(declaration) {
        this->functionTypes.push_back(funcType);
    }

    inline void add(const TxFunctionType* funcType) { this->functionTypes.push_back(funcType); }

    inline std::vector<const TxFunctionType*>::const_iterator cbegin() const noexcept { return this->functionTypes.cbegin(); }
    inline std::vector<const TxFunctionType*>::const_iterator cend() const noexcept { return this->functionTypes.cend(); }
};
*/



class TxTupleType : public TxType {
    // Indicates if this type is *not* immutable, in which case its instances may be declared modifiable.
    const bool _mutable;
    const bool abstract = false;

    TxTupleType(TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec, const std::vector<TxTypeParam>& typeParams, bool _mutable=false)
            : TxType(TXTC_TUPLE, declaration, baseTypeSpec, typeParams), _mutable(_mutable)  { }

protected:
    virtual TxTupleType* make_specialized_type(TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                               const std::vector<TxTypeParam>& typeParams,
                                               std::string* errorMsg=nullptr) const override {
        if (auto tupleBaseType = dynamic_cast<const TxTupleType*>(baseTypeSpec.type))
            return new TxTupleType(declaration, baseTypeSpec, typeParams, tupleBaseType->_mutable);
        throw std::logic_error("Specified a base type for TxTupleType that was not a TxTupleType: " + baseTypeSpec.type->to_string());
    };

public:
    TxTupleType(TxTypeDeclaration* declaration, const TxType* baseType, bool _mutable=false)
            : TxType(TXTC_TUPLE, declaration, TxTypeSpecialization(baseType)), _mutable(_mutable)  {
        ASSERT(declaration, "NULL declaration");
    }

    virtual bool is_abstract() const override { return this->abstract; }

    virtual bool is_immutable() const override { return !this->_mutable; }

    // FUTURE: override is_statically_sized() and return false if any instance member is not statically sized
    // (currently such members are not supported so this can't be false for valid tuple types)
    //bool TxArrayType::is_statically_sized() const override;

    virtual bool innerAutoConvertsFrom(const TxType& someType) const override {
        return (*this) == someType;
    }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};
