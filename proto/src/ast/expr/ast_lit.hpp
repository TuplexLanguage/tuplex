#pragma once

#include "ast/ast_entitydecls.hpp"

class TxLiteralElementaryValueNode : public TxExpressionNode {
public:
    TxLiteralElementaryValueNode( const TxLocation& parseLocation )
            : TxExpressionNode( parseLocation ) {
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

        llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const;
    };

    IntConstant constValue;

    const std::string sourceLiteral;
    bool hasRadix;

    int64_t i64value;
    bool _signed;
    BuiltinTypeId typeId;

    bool negative = false;

    TxIntegerLitNode( const TxIntegerLitNode& orig )
            : TxLiteralElementaryValueNode( orig.parseLocation ), sourceLiteral( orig.sourceLiteral ), hasRadix( orig.hasRadix ),
              i64value( orig.i64value ), _signed( orig._signed ), typeId( orig.typeId ) {
    }

protected:
    virtual void declaration_pass() override;

    virtual const TxType* define_type() override {
        return this->registry().get_builtin_type( this->constValue.typeId );
    }

public:
    TxIntegerLitNode( const TxLocation& parseLocation, const std::string& sourceLiteral, bool hasRadix )
            : TxLiteralElementaryValueNode( parseLocation ), sourceLiteral( sourceLiteral ), hasRadix( hasRadix ),
              i64value(), _signed(), typeId() {
    }

    TxIntegerLitNode( const TxLocation& parseLocation, int64_t i64value, bool _signed, BuiltinTypeId typeId = (BuiltinTypeId) 0 )
            : TxLiteralElementaryValueNode( parseLocation ), sourceLiteral(), hasRadix(),
              i64value( i64value ), _signed( _signed ), typeId( typeId ) {
    }

//    TxIntegerLitNode(const TxLocation& parseLocation, uint64_t u64value, BuiltinTypeId typeId=(BuiltinTypeId)0)
//            : TxLiteralValueNode(parseLocation), intValue(u64value, typeId), sourceLiteral("")  { }

    /** sets this integer literal to be negative, as if preceded by a unary minus. */
    void set_negative() {
        this->negative = true;
    }

    virtual TxIntegerLitNode* make_ast_copy() const override {
        return new TxIntegerLitNode( *this );
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual std::string get_identifier() const override {
        return this->sourceLiteral;
    }
};

class TxFloatingLitNode : public TxLiteralElementaryValueNode {
    BuiltinTypeId typeId;
    const std::string literal;
    const double value;

protected:
    virtual const TxType* define_type() override {
        return this->registry().get_builtin_type( this->typeId );
    }

public:
    TxFloatingLitNode( const TxLocation& parseLocation, const std::string& literal )
            : TxLiteralElementaryValueNode( parseLocation ), typeId( TXBT_FLOAT ), literal( literal ), value( atof( literal.c_str() ) ) {
        // TODO: produce different Floating types
    }

    /** Creates a floating point literal value of zero. */
    TxFloatingLitNode( const TxLocation& parseLocation, BuiltinTypeId typeId = TXBT_FLOAT )
            : TxLiteralElementaryValueNode( parseLocation ), typeId( typeId ), literal( "0" ), value( 0 ) {
    }

    virtual TxFloatingLitNode* make_ast_copy() const override {
        return new TxFloatingLitNode( this->parseLocation, this->literal );
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual std::string get_identifier() const override {
        return this->literal;
    }
};

class TxBoolLitNode : public TxLiteralElementaryValueNode {
protected:
    virtual const TxType* define_type() override {
        return this->registry().get_builtin_type( TXBT_BOOL );
    }

public:
    const bool value;

    TxBoolLitNode( const TxLocation& parseLocation, bool value )
            : TxLiteralElementaryValueNode( parseLocation ), value( value ) {
    }

    virtual TxBoolLitNode* make_ast_copy() const override {
        return new TxBoolLitNode( this->parseLocation, this->value );
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual std::string get_identifier() const override {
        return ( this->value ? "TRUE" : "FALSE" );
    }
};

class TxCharacterLitNode : public TxLiteralElementaryValueNode {
protected:
    virtual const TxType* define_type() override {
        return this->registry().get_builtin_type( TXBT_UBYTE );
    }

public:
    const std::string literal;
    const char value;  // TODO: unicode support

    TxCharacterLitNode( const TxLocation& parseLocation, const std::string& literal )
            : TxLiteralElementaryValueNode( parseLocation ), literal( literal ), value( literal.at( 1 ) ) {
    }
    // TODO: properly parse char literal

    virtual TxCharacterLitNode* make_ast_copy() const override {
        return new TxCharacterLitNode( this->parseLocation, this->literal );
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;

    virtual std::string get_identifier() const override {
        return this->literal;
    }
};