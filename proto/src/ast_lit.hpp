#pragma once

#include "ast_declbase.hpp"


//bool is_signed_out_of_range(const int64_t i64, const BuiltinTypeId typeId);
//
//bool is_unsigned_out_of_range(const uint64_t u64, const BuiltinTypeId typeId);



class TxLiteralValueNode : public TxExpressionNode {
public:
    TxLiteralValueNode(const TxLocation& parseLocation)
        : TxExpressionNode(parseLocation) { }

    virtual TxLiteralValueNode* make_ast_copy() const override = 0;

    virtual bool has_predefined_type() const override final { return true; }

    virtual bool is_statically_constant() const override final { return true; }
};


class IntValue {
    void init_unsigned(uint64_t u64value, BuiltinTypeId typeId);

public:
    BuiltinTypeId typeId = (BuiltinTypeId)0;
    unsigned long radix = 10;
    bool _signed = false;
    bool outOfRange = false;
    union {
        int64_t  i64 = 0;
        uint64_t u64;
    } value;

    IntValue(const std::string& valueLiteral, bool hasRadix, BuiltinTypeId typeId = (BuiltinTypeId)0);

    IntValue(int64_t i64value, BuiltinTypeId typeId, bool _signed);

//    IntValue(uint64_t u64value, BuiltinTypeId typeId) {
//        this->init_unsigned(u64value, typeId);
//    }

    uint32_t get_value_UInt() const;
};


class TxIntegerLitNode : public TxLiteralValueNode {
    class IntConstantProxy : public TxConstantProxy {
        TxIntegerLitNode* intNode;
    public:
        IntConstantProxy(TxIntegerLitNode* intNode) : intNode(intNode) { }
        virtual const TxType* get_type() const override { return intNode->get_type(); }
        virtual uint32_t get_value_UInt() const override { return intNode->intValue.get_value_UInt(); }
//        virtual bool operator==(const TxConstantProxy& other) const override {
//            if (auto otherInt = dynamic_cast<const IntConstantProxy*>(&other)) {
//                return ( intNode->intValue._signed == otherInt->intNode->intValue._signed
//                         && intNode->intValue.value.u64 == otherInt->intNode->intValue.value.u64 );
//            }
//            else return false;
//        }
        virtual llvm::Constant* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
    } intConstProxy;

    IntValue intValue;
    const std::string sourceLiteral;

    TxIntegerLitNode(const TxIntegerLitNode& orig)
        : TxLiteralValueNode(orig.parseLocation), intConstProxy(this), intValue(orig.intValue), sourceLiteral(orig.sourceLiteral) { }

protected:
    virtual const TxType* define_type() override {
        return this->types().get_builtin_type(this->intValue.typeId);
    }

public:
    TxIntegerLitNode(const TxLocation& parseLocation, const std::string& sourceLiteral, bool hasRadix)
            : TxLiteralValueNode(parseLocation), intConstProxy(this),
              intValue(sourceLiteral, hasRadix), sourceLiteral(sourceLiteral)  { }

    TxIntegerLitNode(const TxLocation& parseLocation, int64_t i64value, bool _signed, BuiltinTypeId typeId=(BuiltinTypeId)0)
            : TxLiteralValueNode(parseLocation), intConstProxy(this),
              intValue(i64value, typeId, _signed), sourceLiteral("")  { }

//    TxIntegerLitNode(const TxLocation& parseLocation, uint64_t u64value, BuiltinTypeId typeId=(BuiltinTypeId)0)
//            : TxLiteralValueNode(parseLocation), intConstProxy(this),
//              intValue(u64value, typeId), sourceLiteral("")  { }

    virtual TxIntegerLitNode* make_ast_copy() const override {
        return new TxIntegerLitNode( *this );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        if (this->intValue.radix < 2 || this->intValue.radix > 36)
            CERROR(this, "Radix outside valid range [2,36]: " << this->intValue.radix);
        else if (this->intValue.outOfRange)
            CERROR(this, "Integer literal '" << sourceLiteral << "' badly formatted or outside value range of type " << this->types().get_builtin_type(this->intValue.typeId));
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override { return &this->intConstProxy; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxFloatingLitNode : public TxLiteralValueNode {
    BuiltinTypeId typeId;
    const std::string literal;
    const double value;

protected:
    virtual const TxType* define_type() override {
        return this->types().get_builtin_type(this->typeId);
    }

public:
    TxFloatingLitNode(const TxLocation& parseLocation, const std::string& literal)
        : TxLiteralValueNode(parseLocation), typeId(FLOAT), literal(literal), value(atof(literal.c_str())) {
        // TODO: produce different Floating types
    }

    TxFloatingLitNode(const TxLocation& parseLocation, BuiltinTypeId typeId=FLOAT)
        : TxLiteralValueNode(parseLocation), typeId(typeId), literal("0"), value(0) { }

    virtual TxFloatingLitNode* make_ast_copy() const override {
        return new TxFloatingLitNode( *this );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxCharacterLitNode : public TxLiteralValueNode {
protected:
    virtual const TxType* define_type() override {
        return this->types().get_builtin_type(UBYTE);
    }

public:
    const std::string literal;
    const char value;  // TODO: unicode support

    TxCharacterLitNode(const TxLocation& parseLocation, const std::string& literal)
        : TxLiteralValueNode(parseLocation), literal(literal), value(literal.at(1)) { }
    // TODO: properly parse char literal

    virtual TxCharacterLitNode* make_ast_copy() const override {
        return new TxCharacterLitNode( *this );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxCStringLitNode : public TxLiteralValueNode {
    const size_t arrayLength;  // note: array length includes the null terminator
    TxTypeDeclNode* cstringTypeNode;  // implicit type definer  // FIXME: refactor

protected:
    virtual const TxType* define_type() override {
//        const TxType* charType = this->types().get_builtin_type(UBYTE);
//        return this->types().get_array_type(nullptr, charType, &this->arrayLength);
        return this->cstringTypeNode->typeExpression->resolve_type();
    }

public:
    const std::string literal;
    const std::string value;

    TxCStringLitNode(const TxLocation& parseLocation, const std::string& literal)
        : TxLiteralValueNode(parseLocation), arrayLength(literal.length()-2), cstringTypeNode(),
          literal(literal), value(literal, 2, literal.length()-3) { }
    // TODO: properly parse string literal

    virtual TxCStringLitNode* make_ast_copy() const override {
        return new TxCStringLitNode( *this );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override;

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxBoolLitNode : public TxLiteralValueNode {
protected:
    virtual const TxType* define_type() override {
        return this->types().get_builtin_type(BOOL);
    }

public:
    const bool value;

    TxBoolLitNode(const TxLocation& parseLocation, bool value)
        : TxLiteralValueNode(parseLocation), value(value)  { }

    virtual TxBoolLitNode* make_ast_copy() const override {
        return new TxBoolLitNode( *this );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};
