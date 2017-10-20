#pragma once

#include "type_base.hpp"



class TxBoolTypeClassHandler : public TxTypeClassHandler {
public:
    TxBoolTypeClassHandler() : TxTypeClassHandler( TXTC_ELEMENTARY )  { }

    virtual llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override final;
    virtual llvm::Type* make_llvm_externc_type( const TxActualType* type, LlvmGenerationContext& context ) const override final;
};

class TxBoolType : public TxActualType {
public:
    static const TxBoolTypeClassHandler boolTypeClassHandler;

    TxBoolType( const TxTypeDeclaration* declaration, const TxTypeExpressionNode* baseTypeNode,
                const std::vector<const TxTypeExpressionNode*>& interfaceNodes )
            : TxActualType( &boolTypeClassHandler, declaration, true, baseTypeNode, interfaceNodes ) {
    }
};


class TxConcreteScalarType : public TxActualType {
protected:
    TxConcreteScalarType( const TxTypeClassHandler* typeClassHandler, const TxTypeDeclaration* declaration, const TxTypeExpressionNode* baseTypeNode,
                          const std::vector<const TxTypeExpressionNode*>& interfaceNodes )
            : TxActualType( typeClassHandler, declaration, true, baseTypeNode, interfaceNodes ) {
    }
};


class TxScalarTypeClassHandler : public TxTypeClassHandler {
    const uint32_t _size;

public:
    TxScalarTypeClassHandler( uint32_t size ) : TxTypeClassHandler( TXTC_ELEMENTARY ), _size( size )  { }

    inline uint32_t size() const {
        return this->_size;
    }

    /** This is legal to invoke during analysis passes. It is used for constant expression evaluation. */
    virtual llvm::Type* get_scalar_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const = 0;

    virtual llvm::Type* make_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override final;
    virtual llvm::Type* make_llvm_externc_type( const TxActualType* type, LlvmGenerationContext& context ) const override final;
};


class TxIntegerTypeClassHandler : public TxScalarTypeClassHandler {
    const bool _sign;
protected:
    virtual bool auto_converts_to( const TxActualType* type, const TxActualType* dest ) const override {
        if ( auto destHandler = dynamic_cast<const TxIntegerTypeClassHandler*>( dest->type_class_handler() ) ) {
            if ( this->_sign == destHandler->_sign )
                return this->size() <= destHandler->size();
            else
                return destHandler->_sign && this->size() < destHandler->size();
        }
        return false;
    }

public:
    TxIntegerTypeClassHandler( uint32_t size, bool sign ) : TxScalarTypeClassHandler( size ), _sign( sign )  { }

    inline bool is_signed() const {
        return this->_sign;
    }

    virtual llvm::Type* get_scalar_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
};

class TxIntegerType final : public TxConcreteScalarType {
public:
    TxIntegerType( const TxTypeDeclaration* declaration,
                   const TxTypeExpressionNode* baseTypeNode,
                   const std::vector<const TxTypeExpressionNode*>& interfaceNodes,
                   int size, bool sign )
            : TxConcreteScalarType( new TxIntegerTypeClassHandler( size, sign ), declaration, baseTypeNode, interfaceNodes) {
    }


//    virtual bool auto_converts_to( const TxActualType& destination ) const override {
//        if ( const TxIntegerType* destInt = dynamic_cast<const TxIntegerType*>( &destination ) ) {
//            if ( this->sign == destInt->sign )
//                return this->_size <= destInt->_size;
//            else
//                return destInt->sign && this->_size < destInt->_size;
//        }
//        return false;
//    }
};


class TxFloatingTypeClassHandler : public TxScalarTypeClassHandler {
protected:
    virtual bool auto_converts_to( const TxActualType* type, const TxActualType* dest ) const override {
        if ( auto destHandler = dynamic_cast<const TxFloatingTypeClassHandler*>( dest->type_class_handler() ) )
            return this->size() <= destHandler->size();
        return false;
    }

public:
    TxFloatingTypeClassHandler( uint32_t size ) : TxScalarTypeClassHandler( size )  { }

    virtual llvm::Type* get_scalar_llvm_type( const TxActualType* type, LlvmGenerationContext& context ) const override;
};

class TxFloatingType final : public TxConcreteScalarType {
public:
    TxFloatingType( const TxTypeDeclaration* declaration,
                    const TxTypeExpressionNode* baseTypeNode,
                    const std::vector<const TxTypeExpressionNode*>& interfaceNodes,
                    int size )
            : TxConcreteScalarType( new TxFloatingTypeClassHandler( size ), declaration, baseTypeNode, interfaceNodes ) {
    }

//    virtual bool auto_converts_to( const TxActualType& destination ) const override {
//        if ( const TxFloatingType* destFloat = dynamic_cast<const TxFloatingType*>( &destination ) )
//            return this->_size <= destFloat->_size;
//        return false;
//    }
};
