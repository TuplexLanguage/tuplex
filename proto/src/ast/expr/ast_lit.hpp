#pragma once

#include "ast/ast_entitydecls.hpp"

class TxLiteralElementaryValueNode : public TxExpressionNode {
public:
    TxLiteralElementaryValueNode( const TxLocation& ploc )
            : TxExpressionNode( ploc ) {
    }

    virtual TxLiteralElementaryValueNode* make_ast_copy() const override = 0;

    virtual bool is_statically_constant() const override final {
        return true;
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override = 0;

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override final;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override final {
    }
};

class TxIntegerLitNode : public TxLiteralElementaryValueNode {
    class IntConstant {
        void init_unsigned( uint64_t u64value, BuiltinTypeId typeId );

    public:
        BuiltinTypeId typeId = (BuiltinTypeId) 0;
        unsigned long radix = 10;
        bool _signed = false;
        bool outOfRange = false;
        union {
            int64_t i64 = 0;
            uint64_t u64;
        } value;

        IntConstant() { }

        void initialize( const std::string& valueLiteral, bool hasRadix, bool negative, BuiltinTypeId typeId = (BuiltinTypeId) 0 );

        void initialize( int64_t i64value, BuiltinTypeId typeId, bool _signed );
    };

    IntConstant constValue;

    const std::string sourceLiteral;
    bool hasRadix;

    int64_t i64value;
    bool _signed;
    BuiltinTypeId typeId;

    bool negative = false;

    TxIntegerLitNode( const TxIntegerLitNode& orig )
            : TxLiteralElementaryValueNode( orig.ploc ), sourceLiteral( orig.sourceLiteral ), hasRadix( orig.hasRadix ),
              i64value( orig.i64value ), _signed( orig._signed ), typeId( orig.typeId ) {
    }

protected:
    virtual void declaration_pass() override;

    virtual const TxQualType* define_type() override {
        return new TxQualType( this->registry().get_builtin_type( this->constValue.typeId ) );
    }

public:
    TxIntegerLitNode( const TxLocation& ploc, const std::string& sourceLiteral, bool hasRadix )
            : TxLiteralElementaryValueNode( ploc ), sourceLiteral( sourceLiteral ), hasRadix( hasRadix ),
              i64value(), _signed(), typeId() {
    }

    TxIntegerLitNode( const TxLocation& ploc, int64_t i64value, bool _signed, BuiltinTypeId typeId = (BuiltinTypeId) 0 )
            : TxLiteralElementaryValueNode( ploc ), sourceLiteral(), hasRadix(),
              i64value( i64value ), _signed( _signed ), typeId( typeId ) {
        ASSERT( typeId == 0 || is_concrete_sinteger_type( typeId ) || is_concrete_uinteger_type( typeId ),
                "Type id not a concrete integer type: " << typeId << " in " << this );
    }

//    TxIntegerLitNode(const TxLocation& ploc, uint64_t u64value, BuiltinTypeId typeId=(BuiltinTypeId)0)
//            : TxLiteralValueNode(ploc), intValue(u64value, typeId), sourceLiteral("")  { }

    /** sets this integer literal to be negative, as if preceded by a unary minus. */
    void set_negative() {
        this->negative = true;
    }

    virtual TxIntegerLitNode* make_ast_copy() const override {
        return new TxIntegerLitNode( *this );
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual const std::string& get_descriptor() const override {
        return this->sourceLiteral;
    }
};


class TxFloatingLitNode : public TxLiteralElementaryValueNode {
    class FloatConstant {
    public:
        BuiltinTypeId typeId;
        double value;
        bool outOfRange;

        FloatConstant( const std::string& literal );

        FloatConstant( double value, BuiltinTypeId typeId );
    };

    const std::string literal;
    FloatConstant constValue;

protected:
    virtual const TxQualType* define_type() override {
        return new TxQualType( this->registry().get_builtin_type( this->constValue.typeId ) );
    }

public:
    TxFloatingLitNode( const TxLocation& ploc, const std::string& literal )
            : TxLiteralElementaryValueNode( ploc ), literal( literal ), constValue( literal ) {
    }

    /** Creates a floating point literal value of zero. */
    TxFloatingLitNode( const TxLocation& ploc, BuiltinTypeId typeId = TXBT_FLOAT )
            : TxLiteralElementaryValueNode( ploc ), literal( "0" ), constValue( 0.0, typeId ) {
        ASSERT( is_concrete_floating_type( typeId ), "Type id not a concrete floating point type: " << typeId << " in " << this );
    }

    virtual TxFloatingLitNode* make_ast_copy() const override {
        return new TxFloatingLitNode( this->ploc, this->literal );
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual const std::string& get_descriptor() const override {
        return this->literal;
    }
};


class TxBoolLitNode : public TxLiteralElementaryValueNode {
    static const std::string TRUE;
    static const std::string FALSE;

protected:
    virtual const TxQualType* define_type() override {
        return new TxQualType( this->registry().get_builtin_type( TXBT_BOOL ) );
    }

public:
    const bool value;

    TxBoolLitNode( const TxLocation& ploc, bool value )
            : TxLiteralElementaryValueNode( ploc ), value( value ) {
    }

    virtual TxBoolLitNode* make_ast_copy() const override {
        return new TxBoolLitNode( this->ploc, this->value );
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual const std::string& get_descriptor() const override {
        return ( this->value ? TRUE : FALSE );
    }
};

class TxCharacterLitNode : public TxLiteralElementaryValueNode {
protected:
    virtual const TxQualType* define_type() override {
        return new TxQualType( this->registry().get_builtin_type( TXBT_UBYTE ) );
    }

public:
    const std::string literal;
    const char value;  // TODO: unicode support

    TxCharacterLitNode( const TxLocation& ploc, const std::string& literal )
            : TxLiteralElementaryValueNode( ploc ), literal( literal ), value( literal.at( 1 ) ) {
    }
    // TODO: properly parse char literal

    virtual TxCharacterLitNode* make_ast_copy() const override {
        return new TxCharacterLitNode( this->ploc, this->literal );
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual const std::string& get_descriptor() const override {
        return this->literal;
    }
};
