#pragma once

#include "ast_base.hpp"
#include "ast_fields.hpp"


/*=== conversion/casting ===*/

class TxConversionNode : public TxExpressionNode {
public:
    TxExpressionNode* expr;
    TxType const * const targetType;
    TxConversionNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxType* targetType)
        : TxExpressionNode(parseLocation), expr(expr), targetType(targetType) { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override { return this->targetType; }
    virtual bool is_statically_constant() const { return this->expr->is_statically_constant(); }
    virtual void semantic_pass() { }
};

class TxScalarCastNode : public TxConversionNode {
public:
    TxScalarCastNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxType* targetType)
        : TxConversionNode(parseLocation, expr, targetType) { }
    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};

class TxToPointerCastNode : public TxConversionNode {  // internal conversions for references
public:
    TxToPointerCastNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxType* targetType)
        : TxConversionNode(parseLocation, expr, targetType) { }
    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};

/** Converts between object specializations (across type parameters and inheritance). */
class TxObjSpecCastNode : public TxConversionNode {
public:
    TxObjSpecCastNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxType* targetType)
        : TxConversionNode(parseLocation, expr, targetType) { }
    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};



/*=== expressions ===*/

class TxIntegerLitNode : public TxExpressionNode {
public:
    const std::string literal;
    const long long value;
    TxIntegerLitNode(const yy::location& parseLocation, const std::string& literal)
        : TxExpressionNode(parseLocation), literal(literal), value(atol(literal.c_str())) { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        // TODO: produce different Integer types
        return this->types().get_builtin_type(INT);
    }

    virtual long get_int_value() const { return value; }
    virtual bool is_statically_constant() const { return true; }
    virtual void semantic_pass() { }
    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};

class TxFloatingLitNode : public TxExpressionNode {
public:
    const std::string literal;
    const double value;
    TxFloatingLitNode(const yy::location& parseLocation, const std::string& literal)
        : TxExpressionNode(parseLocation), literal(literal), value(atof(literal.c_str())) { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        // TODO: produce different Floating types
        return this->types().get_builtin_type(FLOAT);
    }

    virtual bool is_statically_constant() const { return true; }
    virtual void semantic_pass() { }
    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};

class TxCharacterLitNode : public TxExpressionNode {
public:
    const std::string literal;
    const char value;  // TODO: unicode support
    TxCharacterLitNode(const yy::location& parseLocation, const std::string& literal)
        : TxExpressionNode(parseLocation), literal(literal), value(literal.at(1)) { }
    // TODO: properly parse char literal

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        return this->types().get_builtin_type(UBYTE);
    }

    virtual bool is_statically_constant() const { return true; }
    virtual void semantic_pass() { }
    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};

class TxCStringLitNode : public TxExpressionNode {
    const TxIntConstant arrayLength;  // note: array length includes the null terminator
public:
    const std::string literal;
    const std::string value;
    TxCStringLitNode(const yy::location& parseLocation, const std::string& literal)
        : TxExpressionNode(parseLocation), arrayLength(literal.length()-2),
          literal(literal), value(literal, 2, literal.length()-3) { }
    // TODO: properly parse string literal

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        const TxType* charType = this->types().get_builtin_type(UBYTE);
        return this->types().get_array_type(charType, &this->arrayLength);
    }

    virtual bool is_statically_constant() const { return true; }
    virtual void semantic_pass() { }
    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};



class TxReferenceToNode : public TxExpressionNode {
public:
    TxFieldValueNode* target;
    TxReferenceToNode(const yy::location& parseLocation, TxFieldValueNode* target)
        : TxExpressionNode(parseLocation), target(target) { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        target->symbol_table_pass(lexContext);
    }

    const TxFieldEntity* get_target_entity() const {
        return this->target->get_entity();
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        return this->types().get_reference_type(this->get_target_entity()->get_type());
    }

    virtual bool is_statically_constant() const {
        return get_target_entity()->is_statically_constant();  // stat. const field will also have stat. const address, right??
    }

    virtual void semantic_pass() {
        target->semantic_pass();
        if (this->get_target_entity()->get_storage() == TXS_NOSTORAGE)
            parser_error(this->parseLocation, "Can't construct reference to non-addressable expression.");
    }

    virtual void setAppliedFuncArgTypes(std::vector<const TxType*>* appliedTypeParameters) {
        this->target->setAppliedFuncArgTypes(appliedTypeParameters);
    }

    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxReferenceDerefNode : public TxExpressionNode {
public:
    TxExpressionNode* reference;
    TxReferenceDerefNode(const yy::location& parseLocation, TxExpressionNode* operand)
        : TxExpressionNode(parseLocation), reference(operand) { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        reference->symbol_table_pass(lexContext);
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        auto opType = this->reference->get_type();
        if (auto refType = dynamic_cast<const TxReferenceType*>(opType)) {
            if (refType->is_generic())
                // FUTURE: return constraint type if present
                return this->types().get_builtin_type(ANY);
            return refType->target_type().get_type();
        }
        parser_error(this->parseLocation, "Operand is not a reference and can't be dereferenced: %s", opType->to_string().c_str());
        return nullptr;
    }

    virtual bool is_statically_constant() const {
        return false;  // can we ever know if target is statically constant?
    }

    virtual void semantic_pass() {
        reference->semantic_pass();
        if (! dynamic_cast<const TxReferenceType*>(this->reference->get_type()))
            parser_error(this->parseLocation, "Can't de-reference non-reference expression.");
    }

    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxElemDerefNode : public TxExpressionNode {
public:
    TxExpressionNode* array;
    TxExpressionNode* subscript;
    TxElemDerefNode(const yy::location& parseLocation, TxExpressionNode* operand, TxExpressionNode* subscript)
        : TxExpressionNode(parseLocation), array(operand), subscript(subscript)  { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        array->symbol_table_pass(lexContext);
        subscript->symbol_table_pass(lexContext);
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        auto opType = this->array->get_type();
        if (auto arrayType = dynamic_cast<const TxArrayType*>(opType)) {
            if (auto e = arrayType->resolve_param_type("E"))
                return e->get_type();
            else
                // FUTURE: return constraint type if present
                return this->types().get_builtin_type(ANY);
        }
        parser_error(this->parseLocation, "Operand is not an array and can't be subscripted: %s", opType->to_string().c_str());
        return nullptr;
    }

    virtual bool is_statically_constant() const {
        return false;  // can we ever know if target is statically constant?
    }

    virtual void semantic_pass() {
        array->semantic_pass();
        subscript->semantic_pass();
        if (! dynamic_cast<const TxArrayType*>(this->array->get_type()))
            parser_error(this->parseLocation, "Can't subscript non-array expression.");
        subscript = wrapConversion(this->context().scope(), subscript, this->types().get_builtin_type(LONG));
    }

    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxBinaryOperatorNode : public TxExpressionNode {
public:
    const TxOperation op;
    TxExpressionNode* lhs;
    TxExpressionNode* rhs;
    TxBinaryOperatorNode(const yy::location& parseLocation, TxExpressionNode* lhs, const TxOperation op, TxExpressionNode* rhs)
        : TxExpressionNode(parseLocation), op(op), lhs(lhs), rhs(rhs) {
        ASSERT(is_valid(op), "Invalid operator value: " << (int)op);
    }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        lhs->symbol_table_pass(lexContext);
        rhs->symbol_table_pass(lexContext);
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        if (get_op_class(this->op) == TXOC_COMPARISON)
            return this->types().get_builtin_type(BOOLEAN);
        else {  // TXOC_ARITHMETIC
            // Note: After semantic pass the lhs will hold the proper resulting type.
            return this->lhs->get_type();
        }
    }

    virtual bool is_statically_constant() const {
        return this->lhs->is_statically_constant() && this->rhs->is_statically_constant();
    }

    virtual void semantic_pass() {
        lhs->semantic_pass();
        rhs->semantic_pass();
        auto ltype = lhs->get_type();
        auto rtype = rhs->get_type();

        if (auto scalar_ltype = dynamic_cast<const TxScalarType*>(ltype)) {
            if (auto scalar_rtype = dynamic_cast<const TxScalarType*>(rtype)) {
                if (scalar_ltype != scalar_rtype) {
                    if (scalar_ltype->autoConvertsFrom(*scalar_rtype)) {
                        // wrap rhs with cast instruction node
                        this->rhs = new TxScalarCastNode(this->rhs->parseLocation, this->rhs, scalar_ltype);
                        return;
                    }
                    else if (scalar_rtype->autoConvertsFrom(*scalar_ltype)) {
                        // wrap lhs with cast instruction node
                        this->lhs = new TxScalarCastNode(this->lhs->parseLocation, this->lhs, scalar_rtype);
                        return;
                    }
                }
                else
                    return;  // same type, no additional action necessary
            }
        }
        else if (dynamic_cast<const TxReferenceType*>(ltype)) {
            if (dynamic_cast<const TxReferenceType*>(rtype)) {
                if (! (this->op == TXOP_EQ || this->op == TXOP_NE))
                    parser_error(this->parseLocation, "Invalid operator for reference operands: %s", to_cstring(this->op));
                return;
            }
        }
        if (ltype && rtype)
            parser_error(this->parseLocation, "Mismatching operand types for binary operator %s: %s, %s", to_cstring(this->op), ltype->to_string().c_str(), rtype->to_string().c_str());
    }

    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};

class TxUnaryMinusNode : public TxExpressionNode {
public:
    TxExpressionNode* operand;
    TxUnaryMinusNode(const yy::location& parseLocation, TxExpressionNode* operand)
        : TxExpressionNode(parseLocation), operand(operand) { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        operand->symbol_table_pass(lexContext);
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        // TODO: promote unsigned integers upon negation
        return this->operand->get_type();
    }

    virtual bool is_statically_constant() const {
        return this->operand->is_statically_constant();
    }

    virtual void semantic_pass() {
        operand->semantic_pass();
        auto type = operand->get_type();
        // assume arithmetic, scalar negation:
        if (dynamic_cast<const TxScalarType*>(type)) {
            // TODO: handle unsigned integers
        }
        else
            parser_error(this->parseLocation, "Operand of unary '-' is not of scalar type: %s", type->to_string().c_str());
    }

    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};


class TxFunctionCallNode : public TxExpressionNode {
    /** resolve possible function overloading by registering actual function signature with callee node */
    void register_callee_signature() const {
        if (callee->hasAppliedFuncArgTypes())
            return;
        std::vector<const TxType*>* appliedArgTypes = new std::vector<const TxType*>();
        for (auto argExpr : *this->argsExprList) {
            argExpr->semantic_pass();
            if (auto argType = argExpr->get_type())
                appliedArgTypes->push_back(argType);
            else {
                delete appliedArgTypes;
                appliedArgTypes = nullptr;
                break;
            }
        }
        if (appliedArgTypes)
            callee->setAppliedFuncArgTypes(appliedArgTypes);
    }

public:
    TxExpressionNode* callee;
    std::vector<TxExpressionNode*>* argsExprList;
    TxExpressionNode* inlinedExpression = nullptr;  // substitutes the function call if non-null

    TxFunctionCallNode(const yy::location& parseLocation, TxExpressionNode* callee, std::vector<TxExpressionNode*>* argsExprList)
        : TxExpressionNode(parseLocation), callee(callee), argsExprList(argsExprList) { }

    virtual void symbol_table_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        this->callee->symbol_table_pass(lexContext);
        for (auto argExpr : *this->argsExprList)
            argExpr->symbol_table_pass(lexContext);
    }

    virtual const TxType* define_type(std::string* errorMsg=nullptr) const override {
        this->register_callee_signature();
        auto calleeType = this->callee->get_type();
        if (auto funcType = dynamic_cast<const TxFunctionType*>(calleeType))
            return funcType->returnType;
        return nullptr;
    }

    virtual void semantic_pass() {
        this->register_callee_signature();
        callee->semantic_pass();
        auto calleeType = callee->get_type();
        if (! calleeType) {
            parser_error(this->parseLocation, "Failed to resolve type of callee in function call expression");
            return;
        }

        // verify matching function signature:
        auto funcType = dynamic_cast<const TxFunctionType*>(calleeType);
        if (! funcType) {
            parser_error(this->parseLocation, "Callee of function call expression is not of function type: %s", calleeType->to_string().c_str());
            return;
        }
        if (funcType->argumentTypes.size() != this->argsExprList->size()) {
            parser_error(this->parseLocation, "Callee of function call expression has mismatching argument count: %s", calleeType->to_string().c_str());
            return;
        }
        if (auto inlineFunc = dynamic_cast<const TxBuiltinConversionFunctionType*>(calleeType)) {
            // "inline" function call by replacing with conversion expression
            this->inlinedExpression = wrapConversion(this->context().scope(), this->argsExprList->front(), inlineFunc->returnType, true);
        }
        else {
            // regular function call
            auto argExprI = this->argsExprList->begin();
            for (auto argDef : funcType->argumentTypes) {
                // note: similar rules to assignment
                // if arg is a reference:
                // TODO: check that no modifiable attribute is lost
                // TODO: check dataspace rules
                *argExprI = wrapConversion(this->context().scope(), *argExprI, argDef);
                argExprI++;
            }
        }
    }

    virtual bool is_statically_constant() const {
        return this->inlinedExpression && this->inlinedExpression->is_statically_constant();
    }

    virtual llvm::Value* codeGen(LlvmGenerationContext& context, GenScope* scope) const;
};
