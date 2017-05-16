#pragma once

#include <algorithm>

#include "util/logging.hpp"

#include "context.hpp"

#include "type_base.hpp"

class TxArrayType : public TxActualType {
protected:
    virtual TxArrayType* make_specialized_type( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                const std::vector<TxTypeSpecialization>& interfaces, bool mutableType ) const override {
        if ( !dynamic_cast<const TxArrayType*>( baseTypeSpec.type ) )
            throw std::logic_error( "Specified a base type for TxArrayType that was not a TxArrayType: " + baseTypeSpec.type->str() );
        return new TxArrayType( declaration, baseTypeSpec, interfaces );
    }
    ;

    //virtual void self_string( std::stringstream& str, bool brief ) const override;

public:
    TxArrayType( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                 const std::vector<TxTypeSpecialization>& interfaces )
            : TxActualType( TXTC_ARRAY, declaration, baseTypeSpec, interfaces ) {
    }

    /** Returns the element type if bound, or tx.Array.E generic type parameter if unbound. */
    const TxActualType* element_type() const;

    /** Returns nullptr if unbound. */
    const TxExpressionNode* length() const;

    /** Returns true if this type is concrete (i.e. can be directly instanced). */
    virtual bool is_concrete() const override;

    virtual bool is_assignable_to( const TxActualType& other ) const override;

    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* gen_size( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* gen_alloca( LlvmGenerationContext& context, GenScope* scope, const std::string &varName = "" ) const override;

private:
    llvm::Value* inner_code_gen_size( LlvmGenerationContext& context, GenScope* scope, llvm::Value* elemSize, llvm::Value* arrayLen ) const;
};

class TxReferenceType : public TxActualType {
protected:
    virtual TxReferenceType* make_specialized_type( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                    const std::vector<TxTypeSpecialization>& interfaces, bool mutableType ) const override {
        if ( !dynamic_cast<const TxReferenceType*>( baseTypeSpec.type ) )
            throw std::logic_error( "Specified a base type for TxReferenceType that was not a TxReferenceType: " + baseTypeSpec.type->str() );
        return new TxReferenceType( declaration, baseTypeSpec, interfaces );
    }
    ;

    //virtual void self_string( std::stringstream& str, bool brief ) const override;

public:
    TxReferenceType( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                     const std::vector<TxTypeSpecialization>& interfaces )
            : TxActualType( TXTC_REFERENCE, declaration, baseTypeSpec, interfaces ) {
    }

    /** Returns the target type if bound, or tx.Ref.T generic type parameter if unbound. */
    const TxActualType* target_type() const;

    virtual bool is_abstract() const override {
        return false;
    }

    /** Returns true if this type is concrete (i.e. can be directly instanced). References are always concrete, also when generic. */
    virtual bool is_concrete() const override {
        return true;
    }

    virtual bool is_assignable_to( const TxActualType& other ) const override;

    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override;

    static llvm::Type* make_ref_llvm_type( LlvmGenerationContext& context, llvm::Type* targetType );

    /** Casts a reference value from one type to another. If targetTypeId is specified, it will replace the original type id. */
    static llvm::Value* gen_ref_conversion( LlvmGenerationContext& context, GenScope* scope, llvm::Value* origRefV,
                                            llvm::Type* targetRefT,
                                            uint32_t targetTypeId = UINT32_MAX );
};

/** A function type describes the user signature of a function.
 * It does not include the implicit "receiver" / "closure" reference argument.
 * This means that free functions, methods and lambda expressions can have the same function type,
 * although their closures work differently.
 */
class TxFunctionType : public TxActualType {
    /** Indicates whether functions of this type may modify its closure when run. */
    const bool modifiableClosure;

    TxFunctionType( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                    const std::vector<const TxActualType*>& argumentTypes,
                    const TxActualType* returnType,
                    bool modifiableClosure = false )
            : TxActualType( TXTC_FUNCTION, declaration, baseTypeSpec, std::vector<TxTypeSpecialization>() ),
              modifiableClosure( modifiableClosure ),
              argumentTypes( argumentTypes ), returnType( returnType ) {
    }

protected:
    // TODO: review, is this used?
    virtual TxFunctionType* make_specialized_type( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                   const std::vector<TxTypeSpecialization>& interfaces, bool mutableType ) const override {
        if ( auto funcBaseType = dynamic_cast<const TxFunctionType*>( baseTypeSpec.type ) )
            return new TxFunctionType( declaration, baseTypeSpec, funcBaseType->argumentTypes,
                                       funcBaseType->returnType,
                                       funcBaseType->modifiableClosure );
        throw std::logic_error( "Specified a base type for TxFunctionType that was not a TxFunctionType: " + baseTypeSpec.type->str() );
    }
    ;

public:
    const std::vector<const TxActualType*> argumentTypes;
    TxActualType const * const returnType;

    /** Creates a function type with no value (Void) return type. */
    TxFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType, const std::vector<const TxActualType*>& argumentTypes,
                    bool modifiableClosure = false );

    /** Creates a function type with a return type. */
    TxFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType, const std::vector<const TxActualType*>& argumentTypes,
                    const TxActualType* returnType,
                    bool modifiableClosure = false )
            : TxActualType( TXTC_FUNCTION, declaration, TxTypeSpecialization( baseType ) ),
              modifiableClosure( modifiableClosure ),
              argumentTypes( argumentTypes ), returnType( returnType ) {
        ASSERT( argumentTypes.empty() || argumentTypes.front(), "NULL arg type" );
        ASSERT( returnType, "NULL return type (must be proper type or Void" );
    }

//    /** Returns false. Functions types are never 'abstract' (except the abstract base type for all functions). */
//    virtual bool is_abstract() const override { return false; }

    bool has_return_value() const {
        return this->returnType->get_type_class() != TXTC_VOID;
    }

    virtual bool modifiable_closure() const {
        return this->modifiableClosure;
    }

    /** Returns the var-arg element type of this function, or nullptr if this is not a var-arg function. */
    const TxActualType* vararg_elem_type() const;

    /** Returns the array argument type if this function takes a single argument that is an array type with statically known size. */
    const TxArrayType* fixed_array_arg_type() const;

    inline virtual bool operator==( const TxActualType& other ) const override {
        if ( auto otherF = dynamic_cast<const TxFunctionType*>( &other ) ) {
            //std::cerr << "EQUAL RETURN TYPES?\n\t" << this->returnType << "\n\t" << otherF->returnType << std::endl;
            return ( ( this->returnType == otherF->returnType
                       || ( *this->returnType == *otherF->returnType ) )
                     && this->argumentTypes.size() == otherF->argumentTypes.size()
                     && std::equal( this->argumentTypes.cbegin(), this->argumentTypes.cend(),
                                    otherF->argumentTypes.cbegin(),
                                    [](const TxActualType* ta, const TxActualType* oa) {return *ta == *oa;} ) );
        }
        return false;
    }

    virtual bool is_assignable_to( const TxActualType& other ) const override {
        if ( auto otherF = dynamic_cast<const TxFunctionType*>( &other ) ) {
            //std::cerr << "ASSIGNABLE RETURN TYPES?\n\t" << this->returnType << "\n\t" << otherF->returnType << std::endl;
            return ( ( this->returnType == otherF->returnType
                       || ( this->returnType->is_assignable_to( *otherF->returnType ) ) )
                     && this->argumentTypes.size() == otherF->argumentTypes.size()
                     && std::equal( this->argumentTypes.cbegin(), this->argumentTypes.cend(),
                                    otherF->argumentTypes.cbegin(),
                                    [](const TxActualType* ta, const TxActualType* oa) {return oa->is_assignable_to( *ta );} ) );
        }
        return false;
    }

    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override;

protected:
    virtual void self_string( std::stringstream& str, bool brief ) const override {
        str << this->get_declaration()->get_unique_full_name() << " : func(";
        str << join( this->argumentTypes, ", " );
        str << ")";
        if ( this->modifiable_closure() )
            str << " ~";
        if ( this->has_return_value() )
            str << " -> " << this->returnType->str( true );
    }
};

class TxConstructorType : public TxFunctionType {
    const TxTypeDeclaration* objTypeDeclaration;
    public:
    TxConstructorType( const TxTypeDeclaration* declaration, const TxActualType* baseType, const std::vector<const TxActualType*> argumentTypes,
                       const TxTypeDeclaration* objTypeDeclaration )
            : TxFunctionType( declaration, baseType, argumentTypes, true ), objTypeDeclaration( objTypeDeclaration ) {
    }

    const TxTypeDeclaration* get_constructed_type_decl() const {
        return this->objTypeDeclaration;
    }
};

class TxMaybeConversionNode;

class TxInlineFunctionType : public TxFunctionType {
public:
    TxInlineFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType, const std::vector<const TxActualType*> argumentTypes,
                          const TxActualType* returnType )
            : TxFunctionType( declaration, baseType, argumentTypes, returnType ) {
    }

    /** Factory method that produces the TxExpressionNode to replace the function call with.
     * Note that the argument expressions will have run the resolution pass before this call, but not calleeExpr.
     * The returned expression will have run the declaration pass. */
    virtual TxExpressionNode* make_inline_expr( TxExpressionNode* calleeExpr, std::vector<TxMaybeConversionNode*>* argsExprList ) const = 0;
};

class TxBuiltinDefaultConstructorType : public TxInlineFunctionType {
    TxExpressionNode* initValueExpr;
    public:
    TxBuiltinDefaultConstructorType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                     const TxActualType* returnType,
                                     TxExpressionNode* initValueExpr )
            : TxInlineFunctionType( declaration, baseType, std::vector<const TxActualType*> { }, returnType ),
              initValueExpr( initValueExpr ) {
    }

    virtual TxExpressionNode* make_inline_expr( TxExpressionNode* calleeExpr, std::vector<TxMaybeConversionNode*>* argsExprList ) const override {
        return initValueExpr;
    }
};

class TxBuiltinConversionFunctionType : public TxInlineFunctionType {
public:
    TxBuiltinConversionFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType, const TxActualType* argumentType,
                                     const TxActualType* returnType )
            : TxInlineFunctionType( declaration, baseType, std::vector<const TxActualType*> { argumentType }, returnType ) {
    }

    virtual TxExpressionNode* make_inline_expr( TxExpressionNode* calleeExpr, std::vector<TxMaybeConversionNode*>* argsExprList ) const override;
};

class TxBuiltinArrayInitializerType : public TxInlineFunctionType {
public:
    TxBuiltinArrayInitializerType( const TxTypeDeclaration* declaration, const TxActualType* baseType, const TxActualType* argumentType,
                                   const TxActualType* returnType )
            : TxInlineFunctionType( declaration, baseType, std::vector<const TxActualType*> { argumentType }, returnType ) {
    }

    virtual TxExpressionNode* make_inline_expr( TxExpressionNode* calleeExpr, std::vector<TxMaybeConversionNode*>* argsExprList ) const override;
};

class TxTupleType : public TxActualType {
    // Indicates if this type is *not* immutable, in which case its instances may be declared modifiable.
    const bool mutableType;

protected:
    virtual TxTupleType* make_specialized_type( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                const std::vector<TxTypeSpecialization>& interfaces, bool mutableType ) const override {
        if ( auto tupleBaseType = dynamic_cast<const TxTupleType*>( baseTypeSpec.type ) ) {
            bool actuallyMutable = tupleBaseType->mutableType && mutableType;
            return new TxTupleType( declaration, baseTypeSpec, interfaces, actuallyMutable );
        }
        throw std::logic_error( "Specified a base type for TxTupleType that was not a TxTupleType: " + baseTypeSpec.type->str() );
    }
    ;

public:
    TxTupleType( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                 const std::vector<TxTypeSpecialization>& interfaces, bool mutableType )
            : TxActualType( TXTC_TUPLE, declaration, baseTypeSpec, interfaces ), mutableType( mutableType ) {
        ASSERT( declaration, "NULL declaration" );
    }

    virtual bool is_immutable() const override {
        return !this->mutableType;
    }

    // FUTURE: override is_statically_sized() and return false if any instance member is not statically sized
    // (currently such members are not supported so this can't be false for valid tuple types)
    //bool TxArrayType::is_statically_sized() const override;

    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override;
    virtual llvm::Type* make_llvm_type_body( LlvmGenerationContext& context, llvm::Type* header ) const override;
};

/** Interfaces are intrinsically abstract types. */
class TxInterfaceType : public TxActualType {
protected:
    virtual TxInterfaceType* make_specialized_type( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                    const std::vector<TxTypeSpecialization>& interfaces, bool mutableType ) const override {
        if ( dynamic_cast<const TxInterfaceType*>( baseTypeSpec.type ) )
            return new TxInterfaceType( declaration, baseTypeSpec, interfaces );
        throw std::logic_error( "Specified a base type for TxInterfaceType that was not a TxInterfaceType: " + baseTypeSpec.type->str() );
    }
    ;

public:
    TxInterfaceType( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                     const std::vector<TxTypeSpecialization>& interfaces )
            : TxActualType( TXTC_INTERFACE, declaration, baseTypeSpec, interfaces ) {
        ASSERT( declaration, "NULL declaration" );
    }

    // special case (lets user skip 'abstract' keyword in interface declarations)
    // TODO: make TXD_ABSTRACT flag be automatically set for all interfaces
    virtual bool is_abstract() const override {
        return true;
    }

    // TODO: allow interfaces with proper is-a relationship to auto-convert (via adapter)

    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override;
};

/** Constitutes a type adapter from the adapted type to the interface type.
 * The interface type is the direct base type of the adapter type.
 * The type adapter is abstract - no instances of it are created - it is to be used as a reference target type. */
class TxInterfaceAdapterType : public TxActualType {
    const TxActualType* adaptedType;

    TxInterfaceAdapterType( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                            const std::vector<TxTypeSpecialization>& interfaces,
                            const TxActualType* adaptedType )
            : TxActualType( TXTC_INTERFACEADAPTER, declaration, baseTypeSpec, interfaces ), adaptedType( adaptedType ) {
    }

protected:
    virtual TxInterfaceAdapterType* make_specialized_type( const TxTypeDeclaration* declaration, const TxTypeSpecialization& baseTypeSpec,
                                                           const std::vector<TxTypeSpecialization>& interfaces, bool mutableType ) const override {
        // Note: Only 'modifiable' and perhaps reference target binding is allowed.
        if ( dynamic_cast<const TxInterfaceAdapterType*>( baseTypeSpec.type ) )
            return new TxInterfaceAdapterType( declaration, baseTypeSpec, interfaces, this->adaptedType );
        throw std::logic_error(
                "Specified a base type for TxInterfaceAdapterType that was not a TxInterfaceAdapterType: " + baseTypeSpec.type->str() );
    }

    virtual bool inner_prepare_members() override;

public:
    TxInterfaceAdapterType( const TxTypeDeclaration* declaration, const TxActualType* interfaceType, const TxActualType* adaptedType )
            : TxActualType( TXTC_INTERFACEADAPTER, declaration, TxTypeSpecialization( interfaceType ) ), adaptedType( adaptedType ) {
    }

//    virtual bool is_abstract() const override { return false; }

// as adapters are intrinsically abstract, no instances are created and can't be any more assignable than interfaces
//    // TO DO: allow adapters with proper is-a relationship to auto-convert
//    virtual bool is_assignable_to(const TxActualType& someType) const override {
//        if (auto otherAdapter = dynamic_cast<const TxInterfaceAdapterType*>(&someType))
//            return (*this == *otherAdapter && this->adaptedType == otherAdapter->adaptedType);
//        else
//            return false;
//    }

    inline const TxActualType* adapted_type() const {
        return this->adaptedType;
    }

    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override;
};
