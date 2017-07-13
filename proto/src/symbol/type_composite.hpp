#pragma once

#include <algorithm>

#include "util/logging.hpp"

#include "ast/lexcontext.hpp"

#include "type_base.hpp"

class TxArrayType final : public TxActualType {
protected:
    virtual TxArrayType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                                bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
        if ( !dynamic_cast<const TxArrayType*>( baseType ) )
            throw std::logic_error( "Specified a base type for TxArrayType that was not a TxArrayType: " + baseType->str() );
        return new TxArrayType( declaration, baseType, mutableType, interfaces );
    }

    virtual bool inner_is_assignable_to( const TxActualType* other ) const override;

    //virtual void self_string( std::stringstream& str, bool brief ) const override;

public:
    TxArrayType( const TxTypeDeclaration* declaration, const TxActualType* baseType, bool mutableType,
                 const std::vector<const TxActualType*>& interfaces )
            : TxActualType( TXTC_ARRAY, declaration, baseType, mutableType, interfaces ) {
    }

    /** Returns the element type if bound, or tx.Array.E generic type parameter if unbound. */
    const TxQualType* element_type() const;

    /** Returns nullptr if unbound. */
    const TxExpressionNode* capacity() const;

    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override;
    virtual llvm::Type* make_llvm_externc_type( LlvmGenerationContext& context ) const override;
    virtual void initialize_specialized_obj( LlvmGenerationContext& context, GenScope* scope, llvm::Value* objPtrV ) const override;
    virtual llvm::Value* gen_size( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* gen_alloca( LlvmGenerationContext& context, GenScope* scope, const std::string &varName = "" ) const override;
    virtual llvm::Value* gen_malloc( LlvmGenerationContext& context, GenScope* scope, const std::string &varName = "" ) const override;

private:
    llvm::Value* inner_code_gen_size( LlvmGenerationContext& context, GenScope* scope, llvm::Value* elemSize, llvm::Value* arrayLen ) const;
};

/** Note, all reference specializations are mutable. */
class TxReferenceType final : public TxActualType {
protected:
    virtual TxReferenceType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                                    bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
        if ( !dynamic_cast<const TxReferenceType*>( baseType ) )
            throw std::logic_error( "Specified a base type for TxReferenceType that was not a TxReferenceType: " + baseType->str() );
        return new TxReferenceType( declaration, baseType, interfaces );
    }

    virtual bool inner_is_assignable_to( const TxActualType* other ) const override;

    //virtual void self_string( std::stringstream& str, bool brief ) const override;

public:
    TxReferenceType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                     const std::vector<const TxActualType*>& interfaces )
            : TxActualType( TXTC_REFERENCE, declaration, baseType, true, interfaces ) {
    }

    /** Returns the target type if bound, or tx.Ref.T generic type parameter if unbound. */
    const TxQualType* target_type() const;

    /** References are never abstract. */
    virtual bool is_abstract() const override {
        return false;
    }

    /** Returns true if this type is concrete. References are always concrete, also when generic. */
    virtual bool is_concrete() const override {
        return true;
    }

    /** Returns true if this type is static (i.e. can be directly instanced). References are always static, also when generic. */
    virtual bool is_static() const override {
        return true;
    }

    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override;
    virtual llvm::Type* make_llvm_externc_type( LlvmGenerationContext& context ) const override;

    static llvm::Type* make_ref_llvm_type( LlvmGenerationContext& context, llvm::Type* targetType );
};

/** A function type describes the user signature of a function.
 * This signature does not include the implicit "receiver" / "closure" reference argument.
 * This means that free functions, methods, external C functions, and lambda expressions can have the same function type,
 * although their closures work differently.
 *
 * Note that function objects in a Tuplex program are lambda objects, containing a function pointer and a closure pointer
 * (regardless of whether the latter is actually used by the callee in question).
 */
class TxFunctionType : public TxActualType {
    /** Indicates whether functions of this type may modify its closure when run. */
    const bool modifiableClosure;
    // TODO: review if modifiableClosure can be merged with mutableType for FunctionType

protected:
    virtual TxFunctionType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                                   bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
        // Note: Only for equivalent derivations - e.g. empty, 'modifiable', and GENPARAM constraints.
        if ( auto funcBaseType = dynamic_cast<const TxFunctionType*>( baseType ) )
            return new TxFunctionType( declaration, baseType, funcBaseType->argumentTypes,
                                       funcBaseType->returnType, funcBaseType->modifiableClosure );
        throw std::logic_error( "Specified a base type for TxFunctionType that was not a TxFunctionType: " + baseType->str() );
    }

    virtual bool inner_is_assignable_to( const TxActualType* other ) const override;

public:
    const std::vector<const TxActualType*> argumentTypes;
    TxActualType const * const returnType;

    /** Creates a function type with no value (Void) return type. */
    TxFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType, const std::vector<const TxActualType*>& argumentTypes,
                    bool modifiableClosure = false );

    /** Creates a function type with a return type. */
    TxFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType, const std::vector<const TxActualType*>& argumentTypes,
                    const TxActualType* returnType, bool modifiableClosure = false )
            : TxActualType( TXTC_FUNCTION, declaration, baseType, true ),
              modifiableClosure( modifiableClosure ), argumentTypes( argumentTypes ), returnType( returnType ) {
        ASSERT( argumentTypes.empty() || argumentTypes.front(), "NULL arg type" );
        ASSERT( returnType, "NULL return type (must be proper type or Void" );
    }

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
            return ( typeid(*this) == typeid(*otherF)
                     && ( this->returnType == otherF->returnType
                          || ( *this->returnType == *otherF->returnType ) )
                     && this->argumentTypes.size() == otherF->argumentTypes.size()
                     && std::equal( this->argumentTypes.cbegin(), this->argumentTypes.cend(),
                                    otherF->argumentTypes.cbegin(),
                                    [](const TxActualType* ta, const TxActualType* oa) {return *ta == *oa;} ) );
        }
        return false;
    }

    /** Makes the LLVM type of this function as seen from calling code, i.e. as a lambda object. */
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

class TxExternCFunctionType : public TxFunctionType {
public:
    TxExternCFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                           const std::vector<const TxActualType*>& argumentTypes, const TxActualType* returnType )
            : TxFunctionType( declaration, baseType, argumentTypes, returnType ) {
    }

    /** Makes the LLVM type of this function as seen from calling code, i.e. as a lambda object. */
    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override;
    /** Makes the LLVM type of this function as declared in code generation. */
    virtual llvm::Type* make_llvm_externc_type( LlvmGenerationContext& context ) const override;
};


class TxMaybeConversionNode;

class TxInlineFunctionType : public TxFunctionType {
public:
    TxInlineFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                          const std::vector<const TxActualType*> argumentTypes, const TxActualType* returnType )
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
protected:
    virtual TxTupleType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                                bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
        if ( dynamic_cast<const TxTupleType*>( baseType ) ) {
            return new TxTupleType( declaration, baseType, interfaces, mutableType );
        }
        throw std::logic_error( "Specified a base type for TxTupleType that was not a TxTupleType: " + baseType->str() );
    }

public:
    TxTupleType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                 const std::vector<const TxActualType*>& interfaces, bool mutableType )
            : TxActualType( TXTC_TUPLE, declaration, baseType, mutableType, interfaces ) {
        ASSERT( declaration, "NULL declaration" );
    }

    virtual void initialize_specialized_obj( LlvmGenerationContext& context, GenScope* scope, llvm::Value* objPtrV ) const override;
    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override;
    virtual llvm::Type* make_llvm_type_body( LlvmGenerationContext& context, llvm::Type* header ) const override;
};

/** Interfaces are intrinsically abstract types. */
class TxInterfaceType : public TxActualType {
protected:
    virtual TxInterfaceType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                                    bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
        if ( dynamic_cast<const TxInterfaceType*>( baseType ) )
            return new TxInterfaceType( declaration, baseType, interfaces );
        throw std::logic_error( "Specified a base type for TxInterfaceType that was not a TxInterfaceType: " + baseType->str() );
    }

public:
    TxInterfaceType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                     const std::vector<const TxActualType*>& interfaces )
            : TxActualType( TXTC_INTERFACE, declaration, baseType, true, interfaces ) {
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
    // as adapters are intrinsically abstract, no instances are created and can't be any more assignable than interfaces
    const TxActualType* adaptedType;

    TxInterfaceAdapterType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                            const std::vector<const TxActualType*>& interfaces, const TxActualType* adaptedType )
            : TxActualType( TXTC_INTERFACEADAPTER, declaration, baseType, adaptedType->is_mutable(), interfaces ), adaptedType( adaptedType ) {
    }

protected:
    virtual TxInterfaceAdapterType* make_specialized_type( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                                           bool mutableType, const std::vector<const TxActualType*>& interfaces ) const override {
        // Note: Only for equivalent derivations including modifiable.
        if ( dynamic_cast<const TxInterfaceAdapterType*>( baseType ) )
            return new TxInterfaceAdapterType( declaration, baseType, interfaces, this->adaptedType );
        throw std::logic_error(
                "Specified a base type for TxInterfaceAdapterType that was not a TxInterfaceAdapterType: " + baseType->str() );
    }

    virtual bool inner_prepare_members() override;

public:
    TxInterfaceAdapterType( const TxTypeDeclaration* declaration, const TxActualType* interfaceType, const TxActualType* adaptedType )
            : TxActualType( TXTC_INTERFACEADAPTER, declaration, interfaceType, adaptedType->is_mutable() ),
                            adaptedType( adaptedType ) {
    }

    inline const TxActualType* adapted_type() const {
        return this->adaptedType;
    }

    virtual llvm::Type* make_llvm_type( LlvmGenerationContext& context ) const override;
};
