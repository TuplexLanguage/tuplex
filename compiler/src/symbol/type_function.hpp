#pragma once


#include "type_base.hpp"

namespace llvm {
    class DISubroutineType;
}

class TxFunctionTypeClassHandler : public TxTypeClassHandler {
protected:
    bool is_a( const TxActualType* type, const TxActualType* other ) const override;

    bool inner_is_assignable_to( const TxActualType* type, const TxActualType* dest, bool returnType ) const override;

    bool inner_equals( const TxActualType* type, const TxActualType* other ) const override;

public:
    TxFunctionTypeClassHandler()
            : TxTypeClassHandler( TXTC_FUNCTION ) {
    }

    /** Makes the LLVM type of this function as seen from calling code, i.e. as a lambda object. */
    llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;

    /** Makes the llvm::DIType for an instance of this type. */
    llvm::DIType* make_llvm_debug_type( const TxActualType* type, LlvmGenerationContext& context ) const override;

    llvm::DISubroutineType* make_llvm_suboutine_debug_type( const TxActualType* type, LlvmGenerationContext& context ) const;
};


/** A function type describes the user signature of a function.
 * This signature does not include the implicit "receiver" / "closure" reference argument.
 * This means that free functions, methods, external C functions, and lambda expressions can have the same function type,
 * although their closures work differently.
 *
 * Note that function objects in a Tuplex program are lambda objects, containing a function pointer and a closure pointer
 * (regardless of whether the latter is actually used by the callee in question).
 *
 * All function types are mutable, meaning that fields of function types may be declared modifiable
 * (thus reassignable to a different lambda object).
 * Function types also have an attribute specifying whether they may modify their closure or not when run.
 */
class TxFunctionType : public TxActualType {
    /** Indicates whether functions of this type may modify its closure when run. */
    const bool modifiableClosure;

    const std::vector<const TxActualType*> argumentTypes;
    TxActualType const* const returnType;

protected:
    TxFunctionType( const TxTypeClassHandler* typeClassHandler, const TxTypeDeclaration* declaration, const TxActualType* baseType,
                    const std::vector<const TxActualType*>& argumentTypes,
                    const TxActualType* returnType,
                    bool modifiableClosure )
            : TxActualType( typeClassHandler, declaration, true, baseType ),
              modifiableClosure( modifiableClosure ),
              argumentTypes( argumentTypes ), returnType( returnType ) {
        ASSERT( argumentTypes.empty() || argumentTypes.front(), "NULL arg type" );
        ASSERT( returnType, "NULL return type (must be proper type or Void" );
    }

//    virtual bool inner_is_assignable_to( const TxActualType* other ) const override;

public:
    /** Creates a function type with no value (Void) return type. */
    TxFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType, const std::vector<const TxActualType*>& argumentTypes,
                    bool modifiableClosure = false );

    /** Creates a function type with a return type. */
    TxFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType, const std::vector<const TxActualType*>& argumentTypes,
                    const TxActualType* returnType, bool modifiableClosure = false )
            : TxFunctionType( get_type_class_handler( TXTC_FUNCTION ), declaration, baseType, argumentTypes, returnType, modifiableClosure ) {
    }

    /** Returns true if functions of this function type may modify their closure. */
    bool modifiable_closure() const override {
        return this->modifiableClosure;
    }

    /** Helper method for getting the argument types of a function type. */
    const std::vector<const TxActualType*>& argument_types() const override {
        return this->argumentTypes;
    }

    /** Gets the return type of this function type. If the function does not return a value, this is Void. */
    const TxActualType* return_type() const override {
        return this->returnType;
    }

    /** Returns false if this function type's return type is Void. */
    bool has_return_value() const override {
        return !this->returnType->is_builtin( TXBT_VOID );
    }

    std::string func_signature_str() const override {
        std::stringstream str;
        this->sig_string( str );
        return str.str();
    }

protected:
    void sig_string( std::stringstream& str ) const {
        if ( this->argumentTypes.empty())
            str << "()";
        else
            str << "( " << join( this->argumentTypes, ", " ) << " )";
        if ( this->modifiable_closure())
            str << " ~";
        if ( this->has_return_value())
            str << " -> " << this->returnType->str( true );
    }

    void self_string( std::stringstream& str, bool brief ) const override {
        str << this->get_declaration()->get_unique_full_name();
        this->sig_string( str );
    }
};


class TxExternCFunctionTypeClassHandler final : public TxFunctionTypeClassHandler {
public:
    TxExternCFunctionTypeClassHandler() : TxFunctionTypeClassHandler() {}

    /** Makes the LLVM type of this function as seen from calling code, i.e. as a lambda object. */
    llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;

    /** Makes the LLVM type of this function as declared in code generation. */
    llvm::Type* make_llvm_externc_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
};

class TxExternCFunctionType final : public TxFunctionType {
public:
    static const TxExternCFunctionTypeClassHandler externcfuncTypeClassHandler;

    TxExternCFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                           const std::vector<const TxActualType*>& argumentTypes, const TxActualType* returnType )
            : TxFunctionType( &externcfuncTypeClassHandler, declaration, baseType, argumentTypes, returnType, false ) {
    }
};


class TxConstructorType final : public TxFunctionType {
    const TxTypeDeclaration* objTypeDeclaration;
public:
    TxConstructorType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                       const std::vector<const TxActualType*>& argumentTypes,
                       const TxTypeDeclaration* objTypeDeclaration )
            : TxFunctionType( declaration, baseType, argumentTypes, true ), objTypeDeclaration( objTypeDeclaration ) {
    }

    [[maybe_unused]] const TxTypeDeclaration* get_constructed_type_decl() const {
        return this->objTypeDeclaration;
    }
};


class TxMaybeConversionNode;

class TxInlineFunctionType : public TxFunctionType {
public:
    TxInlineFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                          const std::vector<const TxActualType*>& argumentTypes, const TxActualType* returnType )
            : TxFunctionType( declaration, baseType, argumentTypes, returnType ) {
    }

    /** Factory method that produces the TxExpressionNode to replace the function call with.
     * Note that the argument expressions will have run the resolution pass before this call, but not calleeExpr.
     * The returned expression will have run the declaration pass. */
    virtual TxExpressionNode* make_inline_expr( TxExpressionNode* calleeExpr, std::vector<TxMaybeConversionNode*>* argsExprList ) const = 0;
};

class TxBuiltinDefaultConstructorType final : public TxInlineFunctionType {
    TxExpressionNode* initValueExpr;  // note, does not own this expression node
public:
    TxBuiltinDefaultConstructorType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                     const TxActualType* returnType,
                                     TxExpressionNode* initValueExpr )
            : TxInlineFunctionType( declaration, baseType, std::vector<const TxActualType*> {}, returnType ),
              initValueExpr( initValueExpr ) {
    }

    TxExpressionNode* make_inline_expr( TxExpressionNode* calleeExpr, std::vector<TxMaybeConversionNode*>* argsExprList ) const override;
};

class TxBuiltinConversionFunctionType final : public TxInlineFunctionType {
public:
    TxBuiltinConversionFunctionType( const TxTypeDeclaration* declaration, const TxActualType* baseType, const TxActualType* argumentType,
                                     const TxActualType* returnType )
            : TxInlineFunctionType( declaration, baseType, std::vector<const TxActualType*> { argumentType }, returnType ) {
    }

    TxExpressionNode* make_inline_expr( TxExpressionNode* calleeExpr, std::vector<TxMaybeConversionNode*>* argsExprList ) const override;
};

class TxBuiltinAssignInitializerType final : public TxInlineFunctionType {
public:
    TxBuiltinAssignInitializerType( const TxTypeDeclaration* declaration, const TxActualType* baseType,
                                    const TxActualType* argAndReturnType )
            : TxInlineFunctionType( declaration, baseType,
                                    std::vector<const TxActualType*> { argAndReturnType },
                                    argAndReturnType ) {
    }

    TxExpressionNode* make_inline_expr( TxExpressionNode* calleeExpr, std::vector<TxMaybeConversionNode*>* argsExprList ) const override;
};
