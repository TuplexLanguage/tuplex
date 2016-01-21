#pragma once

#include "ast_base.hpp"


bool is_signed_out_of_range(const int64_t i64, const BuiltinTypeId typeId);

bool is_unsigned_out_of_range(const uint64_t u64, const BuiltinTypeId typeId);



class TxLiteralValueNode : public TxExpressionNode {
public:
    TxLiteralValueNode(const yy::location& parseLocation)
        : TxExpressionNode(parseLocation) { }

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

    IntValue(uint64_t u64value, BuiltinTypeId typeId) {
        this->init_unsigned(u64value, typeId);
    }

    uint32_t get_value_UInt() const;
};


class TxIntegerLitNode : public TxLiteralValueNode {
    class IntConstantProxy : public TxConstantProxy {
        TxIntegerLitNode* intNode;
    public:
        IntConstantProxy(TxIntegerLitNode* intNode) : intNode(intNode) { }
        virtual const TxType* get_type() const override { return intNode->get_type(0); }
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

protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        return this->types(six).get_builtin_type(this->intValue.typeId);
    }

public:
    TxIntegerLitNode(const yy::location& parseLocation, const std::string& sourceLiteral, bool hasRadix)
            : TxLiteralValueNode(parseLocation), intConstProxy(this),
              intValue(sourceLiteral, hasRadix), sourceLiteral(sourceLiteral)  { }

    TxIntegerLitNode(const yy::location& parseLocation, int64_t i64value, BuiltinTypeId typeId=(BuiltinTypeId)0)
            : TxLiteralValueNode(parseLocation), intConstProxy(this),
              intValue(i64value, typeId), sourceLiteral("")  { }

    TxIntegerLitNode(const yy::location& parseLocation, uint64_t u64value, BuiltinTypeId typeId=(BuiltinTypeId)0)
            : TxLiteralValueNode(parseLocation), intConstProxy(this),
              intValue(u64value, typeId), sourceLiteral("")  { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        if (this->intValue.radix < 2 || this->intValue.radix > 36)
            CERROR(this, "Radix outside valid range [2,36]: " << this->intValue.radix);
        else if (this->intValue.outOfRange)
            CERROR(this, "Integer literal '" << sourceLiteral << "' badly formatted or outside value range of type " << this->types(six).get_builtin_type(this->intValue.typeId));
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override { return &this->intConstProxy; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxFloatingLitNode : public TxLiteralValueNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        // TODO: produce different Floating types
        return this->types(six).get_builtin_type(FLOAT);
    }

public:
    const std::string literal;
    const double value;
    TxFloatingLitNode(const yy::location& parseLocation, const std::string& literal)
        : TxLiteralValueNode(parseLocation), literal(literal), value(atof(literal.c_str())) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxCharacterLitNode : public TxLiteralValueNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        return this->types(six).get_builtin_type(UBYTE);
    }

public:
    const std::string literal;
    const char value;  // TODO: unicode support
    TxCharacterLitNode(const yy::location& parseLocation, const std::string& literal)
        : TxLiteralValueNode(parseLocation), literal(literal), value(literal.at(1)) { }
    // TODO: properly parse char literal

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxCStringLitNode : public TxLiteralValueNode {
    const size_t arrayLength;  // note: array length includes the null terminator
    TxTypeDeclNode* cstringTypeNode;  // implicit type definer

protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
//        const TxType* charType = this->types(six).get_builtin_type(UBYTE);
//        return this->types(six).get_array_type(nullptr, charType, &this->arrayLength);
        return this->cstringTypeNode->typeExpression->resolve_type(six);
    }

public:
    const std::string literal;
    const std::string value;

    TxCStringLitNode(const yy::location& parseLocation, const std::string& literal)
        : TxLiteralValueNode(parseLocation), arrayLength(literal.length()-2), cstringTypeNode(),
          literal(literal), value(literal, 2, literal.length()-3) { }
    // TODO: properly parse string literal

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override;

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxBoolLitNode : public TxLiteralValueNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        return this->types(six).get_builtin_type(BOOL);
    }

public:
    const bool value;

    TxBoolLitNode(const yy::location& parseLocation, bool value)
        : TxLiteralValueNode(parseLocation), value(value)  { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};
