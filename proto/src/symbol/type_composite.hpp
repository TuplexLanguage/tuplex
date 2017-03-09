#pragma once

#include <algorithm>

#include "util/logging.hpp"

#include "context.hpp"

#include "type_base.hpp"


class TxArrayType : public TxType {
    TxArrayType(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                const std::vector<TxTypeSpecialization>& interfaces)
            : TxType(TXTC_ARRAY, declaration, baseTypeSpec, interfaces)  { }

protected:
    virtual TxArrayType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                               const std::vector<TxTypeSpecialization>& interfaces) const override {
        if (! dynamic_cast<const TxArrayType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxArrayType that was not a TxArrayType: " + baseTypeSpec.type->str());
        return new TxArrayType(declaration, baseTypeSpec, interfaces);
    };

public:
    /** Creates the Array base type. Only one such instance should exist. */
    TxArrayType(const TxTypeDeclaration* declaration, const TxType* anyType)
            : TxType(TXTC_ARRAY, declaration, TxTypeSpecialization(anyType)) { }


    /** Returns nullptr if unbound. */
    const TxType* element_type() const;

    /** Returns nullptr if unbound. */
    const TxExpressionNode* length() const;


    //virtual bool is_abstract() const { return false; }

    virtual bool is_statically_sized() const override;

    virtual bool is_assignable_to(const TxType& other) const override;

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;
    virtual llvm::Value* gen_size(LlvmGenerationContext& context, GenScope* scope) const override;
    virtual llvm::Value* gen_alloca(LlvmGenerationContext& context, GenScope* scope, const std::string &varName="") const override;

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }

private:
    llvm::Value* inner_code_gen_size(LlvmGenerationContext& context, GenScope* scope, llvm::Value* elemSize, llvm::Value* arrayLen) const;
};



class TxReferenceType : public TxType {
    TxReferenceType(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                    const std::vector<TxTypeSpecialization>& interfaces)
            : TxType(TXTC_REFERENCE, declaration, baseTypeSpec, interfaces)  { }

protected:
    virtual TxReferenceType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                   const std::vector<TxTypeSpecialization>& interfaces) const override {
        if (! dynamic_cast<const TxReferenceType*>(baseTypeSpec.type))
            throw std::logic_error("Specified a base type for TxReferenceType that was not a TxReferenceType: " + baseTypeSpec.type->str());
        return new TxReferenceType(declaration, baseTypeSpec, interfaces);
    };

public:
    /** Creates the Reference base type. Only one such instance should exist. */
    TxReferenceType(const TxTypeDeclaration* declaration, const TxType* anyType)
            : TxType(TXTC_REFERENCE, declaration, TxTypeSpecialization(anyType)) { }


    /** Returns the target type of this reference type, or nullptr if it failed to resolve. */
    const TxType* target_type() const;

    virtual bool is_final() const { return true; }
    //virtual bool is_abstract() const { return false; }

    /** Returns true if this type is concrete (i.e. can be directly instanced).
     * A concrete type is not abstract, nor usually generic (references may be concrete while generic). */
    virtual bool is_concrete() const { return true; }

    virtual bool is_assignable_to(const TxType& other) const override;

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;

    static llvm::Type* make_ref_llvm_type(LlvmGenerationContext& context, llvm::Type* targetType);

    /** Casts a reference value from one type to another. If targetTypeId is specified, it will replace the original type id. */
    static llvm::Value* gen_ref_conversion(LlvmGenerationContext& context, GenScope* scope, llvm::Value* origRefV,
                                           llvm::Type* targetRefT, uint32_t targetTypeId=UINT32_MAX);

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};


/** A function type describes the user signature of a function.
 * It does not include the implicit "receiver" / "closure" reference argument.
 * This means that free functions, methods and lambda expressions can have the same function type,
 * although their closures work differently.
 */
class TxFunctionType : public TxType {
    /** Indicates whether functions of this type may modify its closure when run. */
    const bool modifiableClosure;

    TxFunctionType(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                   const std::vector<const TxType*>& argumentTypes, const TxType* returnType=nullptr,
                   bool modifiableClosure=false)
            : TxType(TXTC_FUNCTION, declaration, baseTypeSpec, std::vector<TxTypeSpecialization>()),
              modifiableClosure(modifiableClosure), argumentTypes(argumentTypes), returnType(returnType)  { }

protected:
    // TODO: review, is this used?
    virtual TxFunctionType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                  const std::vector<TxTypeSpecialization>& interfaces) const override {
        if (auto funcBaseType = dynamic_cast<const TxFunctionType*>(baseTypeSpec.type))
            return new TxFunctionType(declaration, baseTypeSpec, funcBaseType->argumentTypes,
                                      funcBaseType->returnType, funcBaseType->modifiableClosure);
        throw std::logic_error("Specified a base type for TxFunctionType that was not a TxFunctionType: " + baseTypeSpec.type->str());
    };

public:
    const std::vector<const TxType*> argumentTypes;
    TxType const * const returnType;

    TxFunctionType(const TxTypeDeclaration* declaration, const TxType* baseType, const std::vector<const TxType*>& argumentTypes,
                   const TxType* returnType=nullptr, bool modifiableClosure=false)
        : TxType(TXTC_FUNCTION, declaration, TxTypeSpecialization(baseType)),
          modifiableClosure(modifiableClosure), argumentTypes(argumentTypes), returnType(returnType)  {
        ASSERT(argumentTypes.size() == 0 || argumentTypes.at(0), "NULL arg type");
    }

    /** Returns false. Functions types are never 'abstract' (except the abstract base type for all functions). */
    virtual bool is_abstract() const override { return false; }

    bool hasReturnValue() const  { return this->returnType != nullptr; }

    virtual bool modifiable_closure() const { return this->modifiableClosure; }

    //virtual bool is_abstract() const override { return false; }

    inline virtual bool operator==(const TxType& other) const override {
        if (auto otherF = dynamic_cast<const TxFunctionType*>(&other)) {
            //std::cerr << "EQUAL RETURN TYPES?\n\t" << this->returnType << "\n\t" << otherF->returnType << std::endl;
            return ( ( this->returnType == otherF->returnType
                       || ( this->returnType != nullptr && otherF->returnType != nullptr
                            && *this->returnType == *otherF->returnType ) )
                     && this->argumentTypes.size() == otherF->argumentTypes.size()
                     && std::equal(this->argumentTypes.cbegin(), this->argumentTypes.cend(),
                                   otherF->argumentTypes.cbegin(),
                                   [](const TxType* ta, const TxType* oa) { return *ta == *oa; } ) );
        }
        return false;
    }

    virtual bool is_assignable_to(const TxType& other) const override {
        if (auto otherF = dynamic_cast<const TxFunctionType*>(&other)) {
            //std::cerr << "ASSIGNABLE RETURN TYPES?\n\t" << this->returnType << "\n\t" << otherF->returnType << std::endl;
            return ( ( this->returnType == otherF->returnType
                       || ( this->returnType != nullptr && otherF->returnType != nullptr
                            && this->returnType->is_assignable_to( *otherF->returnType ) ) )
                     && this->argumentTypes.size() == otherF->argumentTypes.size()
                     && std::equal(this->argumentTypes.cbegin(), this->argumentTypes.cend(),
                                   otherF->argumentTypes.cbegin(),
                                   [](const TxType* ta, const TxType* oa) { return oa->is_assignable_to( *ta ); } ) );
        }
        return false;
    }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;

    virtual void accept(TxTypeVisitor& visitor) const override { visitor.visit(*this); }

protected:
    virtual void self_string(std::stringstream& str, bool brief, bool skipFirstName, bool skipImplicitNames) const override {
        str << "func(";
        if (! this->argumentTypes.empty()) {
            auto ai = this->argumentTypes.cbegin();
            str << (*ai)->str(true);
            for (ai++; ai != this->argumentTypes.cend(); ai++)
                str << ", " << (*ai)->str(true);
        }
        str << ")";
        if (this->returnType)
            str << " -> " << this->returnType->str(true);
    }
};


class TxConstructorType : public TxFunctionType {
    TxTypeDeclaration* objTypeDeclaration;
public:
    TxConstructorType(const TxTypeDeclaration* declaration, const TxType* baseType, const std::vector<const TxType*> argumentTypes,
                      TxTypeDeclaration* objTypeDeclaration)
        : TxFunctionType(declaration, baseType, argumentTypes, nullptr, true), objTypeDeclaration(objTypeDeclaration) { }

    TxTypeDeclaration* get_constructed_type_decl() const {
        return this->objTypeDeclaration;
    }
};


class TxMaybeConversionNode;

class TxInlineFunctionType : public TxFunctionType {
public:
    TxInlineFunctionType(const TxTypeDeclaration* declaration, const TxType* baseType, const std::vector<const TxType*> argumentTypes,
                          const TxType* returnType)
        : TxFunctionType(declaration, baseType, argumentTypes, returnType) { }

    /** Factory method that produces the TxExpressionNode to replace the function call with.
     * Note that the argument expressions will have run the resolution pass before this call, but not calleeExpr. */
    virtual TxExpressionNode* make_inline_expr( TxExpressionNode* calleeExpr, std::vector<TxMaybeConversionNode*>* argsExprList ) const = 0;
};

class TxBuiltinDefaultConstructorType : public TxInlineFunctionType {
    TxExpressionNode* initValueExpr;
public:
    TxBuiltinDefaultConstructorType(const TxTypeDeclaration* declaration, const TxType* baseType,
                                    const TxType* returnType, TxExpressionNode* initValueExpr)
        : TxInlineFunctionType(declaration, baseType, std::vector<const TxType*>{ }, returnType),
          initValueExpr(initValueExpr)  { }

    virtual TxExpressionNode* make_inline_expr( TxExpressionNode* calleeExpr, std::vector<TxMaybeConversionNode*>* argsExprList ) const override {
        return initValueExpr;
    }
};

class TxBuiltinConversionFunctionType : public TxInlineFunctionType {
public:
    TxBuiltinConversionFunctionType(const TxTypeDeclaration* declaration, const TxType* baseType, const TxType* argumentType,
                                    const TxType* returnType)
        : TxInlineFunctionType(declaration, baseType, std::vector<const TxType*>{ argumentType }, returnType) { }

    virtual TxExpressionNode* make_inline_expr( TxExpressionNode* calleeExpr, std::vector<TxMaybeConversionNode*>* argsExprList ) const override;
};



class TxTupleType : public TxType {
    // Indicates if this type is *not* immutable, in which case its instances may be declared modifiable.
    const bool _mutable;

    TxTupleType(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                const std::vector<TxTypeSpecialization>& interfaces, bool _mutable=false)
            : TxType(TXTC_TUPLE, declaration, baseTypeSpec, interfaces), _mutable(_mutable)  { }

protected:
    virtual TxTupleType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                               const std::vector<TxTypeSpecialization>& interfaces) const override {
        if (auto tupleBaseType = dynamic_cast<const TxTupleType*>(baseTypeSpec.type))
            return new TxTupleType(declaration, baseTypeSpec, interfaces, tupleBaseType->_mutable);
        throw std::logic_error("Specified a base type for TxTupleType that was not a TxTupleType: " + baseTypeSpec.type->str());
    };

public:
    TxTupleType(const TxTypeDeclaration* declaration, const TxType* baseType, bool _mutable=false)
            : TxType(TXTC_TUPLE, declaration, TxTypeSpecialization(baseType)), _mutable(_mutable)  {
        ASSERT(declaration, "NULL declaration");
    }

    virtual bool is_immutable() const override { return !this->_mutable; }

    // FUTURE: override is_statically_sized() and return false if any instance member is not statically sized
    // (currently such members are not supported so this can't be false for valid tuple types)
    //bool TxArrayType::is_statically_sized() const override;

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;
    virtual llvm::Type* make_llvm_type_body(LlvmGenerationContext& context, llvm::Type* header) const override;

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};


/** Interfaces are intrinsically abstract types. */
class TxInterfaceType : public TxType {
    TxInterfaceType(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                    const std::vector<TxTypeSpecialization>& interfaces)
            : TxType(TXTC_INTERFACE, declaration, baseTypeSpec, interfaces)  { }

protected:
    virtual TxInterfaceType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                   const std::vector<TxTypeSpecialization>& interfaces) const override {
        if (dynamic_cast<const TxInterfaceType*>(baseTypeSpec.type))
            return new TxInterfaceType(declaration, baseTypeSpec, interfaces);
        throw std::logic_error("Specified a base type for TxInterfaceType that was not a TxInterfaceType: " + baseTypeSpec.type->str());
    };

public:
    TxInterfaceType(const TxTypeDeclaration* declaration, const TxType* baseType)
            : TxType(TXTC_INTERFACE, declaration, TxTypeSpecialization(baseType)) {
        ASSERT(declaration, "NULL declaration");
    }

    // special case (lets user skip 'abstract' keyword in interface declarations)
    // TODO: make TXD_ABSTRACT flag be automatically set for all interfaces?
    virtual bool is_abstract() const override { return true; }

    // TODO: allow interfaces with proper is-a relationship to auto-convert (via adapter)

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};


/** Constitutes a type adapter from the adapted type to the interface type.
 * The interface type is the direct base type of the adapter type.
 * The type adapter is abstract - no instances of it are created - it is to be used as a reference target type. */
class TxInterfaceAdapterType : public TxType {
    const TxType* adaptedType;

    TxInterfaceAdapterType(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                           const std::vector<TxTypeSpecialization>& interfaces,
                           const TxType* adaptedType)
            : TxType(TXTC_INTERFACEADAPTER, declaration, baseTypeSpec, interfaces), adaptedType(adaptedType)  { }

protected:
    virtual TxInterfaceAdapterType* make_specialized_type(const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                          const std::vector<TxTypeSpecialization>& interfaces) const override {
        // Note: Only 'modifiable' and perhaps reference target binding is allowed.
        if (dynamic_cast<const TxInterfaceAdapterType*>(baseTypeSpec.type))
            return new TxInterfaceAdapterType(declaration, baseTypeSpec, interfaces, this->adaptedType);
        throw std::logic_error("Specified a base type for TxInterfaceAdapterType that was not a TxInterfaceAdapterType: " + baseTypeSpec.type->str());
    }

public:
    TxInterfaceAdapterType(const TxTypeDeclaration* declaration, const TxType* interfaceType, const TxType* adaptedType)
            : TxType(TXTC_INTERFACEADAPTER, declaration, TxTypeSpecialization(interfaceType)), adaptedType(adaptedType)  { }

    virtual void prepare_members() override;

//    virtual bool is_abstract() const override { return false; }

// as adapters are intrinsically abstract, no instances are created and can't be any more assignable than interfaces
//    // TO DO: allow adapters with proper is-a relationship to auto-convert
//    virtual bool is_assignable_to(const TxType& someType) const override {
//        if (auto otherAdapter = dynamic_cast<const TxInterfaceAdapterType*>(&someType))
//            return (*this == *otherAdapter && this->adaptedType == otherAdapter->adaptedType);
//        else
//            return false;
//    }

    inline const TxType* adapted_type() const { return this->adaptedType; }

    virtual llvm::Type* make_llvm_type(LlvmGenerationContext& context) const override;

    virtual void accept(TxTypeVisitor& visitor) const { visitor.visit(*this); }
};
