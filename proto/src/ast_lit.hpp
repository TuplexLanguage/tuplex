#pragma once

#include "ast_declbase.hpp"

class IntConstant {
    TxTypeDefiningNode* node;
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

    IntConstant( TxTypeDefiningNode* node, const std::string& valueLiteral, bool hasRadix, BuiltinTypeId typeId = (BuiltinTypeId) 0 );

    IntConstant( TxTypeDefiningNode* node, int64_t i64value, BuiltinTypeId typeId, bool _signed );

//    IntConstant( uint64_t u64value, BuiltinTypeId typeId ) {
//        this->init_unsigned( u64value, typeId );
//    }

    TxTypeDefiningNode* get_node() const {
        return this->node;
    }

    llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const;
};

class TxLiteralElementaryValueNode : public TxExpressionNode {
public:
    TxLiteralElementaryValueNode( const TxLocation& parseLocation )
            : TxExpressionNode( parseLocation ) {
    }

    virtual TxLiteralElementaryValueNode* make_ast_copy() const override = 0;

    virtual bool is_statically_constant() const override final {
        return true;
    }

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override = 0;

    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override final;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override final {
    }
};

class TxIntegerLitNode : public TxLiteralElementaryValueNode {
    const IntConstant constValue;
    const std::string sourceLiteral;

    TxIntegerLitNode( const TxIntegerLitNode& orig )
            : TxLiteralElementaryValueNode( orig.parseLocation ), constValue( orig.constValue ), sourceLiteral( orig.sourceLiteral ) {
    }

protected:
    virtual void declaration_pass() override;

    virtual const TxType* define_type() override {
        return this->registry().get_builtin_type( this->constValue.typeId );
    }

public:
    TxIntegerLitNode( const TxLocation& parseLocation, const std::string& sourceLiteral, bool hasRadix )
            : TxLiteralElementaryValueNode( parseLocation ), constValue( this, sourceLiteral, hasRadix ), sourceLiteral( sourceLiteral ) {
    }

    TxIntegerLitNode( const TxLocation& parseLocation, int64_t i64value, bool _signed, BuiltinTypeId typeId = (BuiltinTypeId) 0 )
            : TxLiteralElementaryValueNode( parseLocation ), constValue( this, i64value, typeId, _signed ),sourceLiteral( "" ) {
    }

//    TxIntegerLitNode(const TxLocation& parseLocation, uint64_t u64value, BuiltinTypeId typeId=(BuiltinTypeId)0)
//            : TxLiteralValueNode(parseLocation), intValue(u64value, typeId), sourceLiteral("")  { }

    virtual TxIntegerLitNode* make_ast_copy() const override {
        return new TxIntegerLitNode( *this );
    }

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
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

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
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

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
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

    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;
};

class TxCStringLitNode : public TxExpressionNode {
    const size_t arrayCapacity;  // note: array capacity includes the null terminator
    TxTypeExpressionNode* cstringTypeNode;  // implicit type definer

    static TxTypeExpressionNode* make_cstring_type_expr( const TxLocation& parseLocation, const std::string& literal );

protected:
    virtual const TxType* define_type() override {
        return this->cstringTypeNode->resolve_type();
    }

public:
    const std::string literal;
    const std::string value;

    TxCStringLitNode( const TxLocation& parseLocation, const std::string& literal )
            : TxExpressionNode( parseLocation ), arrayCapacity( literal.length() - 2 ),
              cstringTypeNode( make_cstring_type_expr( parseLocation, literal ) ),
              literal( literal ), value( literal, 2, literal.length() - 3 ) {
    }
    // TODO: properly parse string literal

    virtual TxCStringLitNode* make_ast_copy() const override {
        return new TxCStringLitNode( this->parseLocation, this->literal );
    }

    virtual bool is_statically_constant() const override final {
        return true;
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Constant* code_gen_constant( LlvmGenerationContext& context ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->cstringTypeNode->visit_ast( visitor, thisCursor, "cstrtype", context );
    }
};
