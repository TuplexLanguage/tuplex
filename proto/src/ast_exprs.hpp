#pragma once

#include "ast_base.hpp"
#include "ast_fields.hpp"
#include "ast_types.hpp"
#include "ast_conv.hpp"


extern llvm::Value* gen_get_struct_member(LlvmGenerationContext& context, GenScope* scope, llvm::Value* structV, unsigned ix);

extern llvm::Value* gen_get_ref_pointer(LlvmGenerationContext& context, GenScope* scope, llvm::Value* refV);
extern llvm::Value* gen_get_ref_typeid(LlvmGenerationContext& context, GenScope* scope, llvm::Value* refV);
extern llvm::Value* gen_ref(LlvmGenerationContext& context, GenScope* scope, llvm::Type* refT, llvm::Value* ptrV, llvm::Value* tidV);

extern llvm::Value* gen_lambda(LlvmGenerationContext& context, GenScope* scope, llvm::Type* lambdaT, llvm::Value* funcV, llvm::Value* closureRefV);



class TxReferenceDerefNode : public TxExpressionNode {
    /** internal "cache" to prevent multiple code generations */
    mutable llvm::Value* refExprValue = nullptr;

protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        auto opType = this->reference->resolve_type(six);
        if (auto refType = dynamic_cast<const TxReferenceType*>(opType)) {
            if (refType->is_generic())
                // FUTURE: return constraint type if present
                return this->types(six).get_builtin_type(ANY);
            return refType->target_type();
        }
        CERROR(this, "Operand is not a reference and can't be dereferenced: " << opType);
        return nullptr;
    }

public:
    TxExpressionNode* reference;
    TxReferenceDerefNode(const yy::location& parseLocation, TxExpressionNode* operand)
        : TxExpressionNode(parseLocation), reference(operand) { }

    virtual bool has_predefined_type() const override { return this->reference->has_predefined_type(); }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) {
        this->set_context(six, lexContext);
        reference->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxExpressionNode::symbol_resolution_pass(six);
        this->reference->symbol_resolution_pass(six);

        if (! dynamic_cast<const TxReferenceType*>(this->reference->get_type(six)))
            CERROR(this, "Can't de-reference non-reference expression.");
    }

    virtual bool is_statically_constant() const {
        return false;  // can we ever know if target is statically constant?
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
    virtual llvm::Value* code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxElemDerefNode : public TxExpressionNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        auto opType = this->array->resolve_type(six);
        if (auto arrayType = dynamic_cast<const TxArrayType*>(opType)) {
            if (auto elemType = arrayType->element_type())
                return elemType;
            else
                // FUTURE: return constraint type if present
                return this->types(six).get_builtin_type(ANY);
        }
        if (opType)
            CERROR(this, "Operand is not an array and can't be subscripted: " << opType);
        return nullptr;
    }

public:
    TxExpressionNode* array;
    TxExpressionNode* subscript;
    TxElemDerefNode(const yy::location& parseLocation, TxExpressionNode* operand, TxExpressionNode* subscript)
        : TxExpressionNode(parseLocation), array(operand), subscript(make_generic_conversion_node(subscript))  { }

    virtual bool has_predefined_type() const override { return this->array->has_predefined_type(); }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        this->array->symbol_declaration_pass(six, lexContext);
        this->subscript->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxExpressionNode::symbol_resolution_pass(six);
        this->array->symbol_resolution_pass(six);
        insert_conversion(this->subscript, six, this->types(six).get_builtin_type(LONG));
        this->subscript->symbol_resolution_pass(six);
    }

    virtual bool is_statically_constant() const {
        return this->array->is_statically_constant() && this->subscript->is_statically_constant();
    }

    virtual llvm::Value* code_gen_address(LlvmGenerationContext& context, GenScope* scope) const;
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxReferenceToNode : public TxExpressionNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        auto implTypeName = this->context(six).scope()->make_unique_name("$type");
        auto typeDecl = this->context(six).scope()->declare_type(implTypeName, this->get_type_definer(six), TXD_PUBLIC | TXD_IMPLICIT);
        return this->types(six).get_reference_type(typeDecl, TxGenericBinding::make_type_binding("T", this->target->get_type_definer(six)));
    }

public:
    TxExpressionNode* target;

    TxReferenceToNode(const yy::location& parseLocation, TxExpressionNode* target)
        : TxExpressionNode(parseLocation), target(target) { }

    virtual bool has_predefined_type() const override { return false; }  // (this expr constructs new type)

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) {
        this->set_context(six, lexContext);
        target->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxExpressionNode::symbol_resolution_pass(six);
        this->target->symbol_resolution_pass(six);

        if (dynamic_cast<TxFieldValueNode*>(this->target)) {
        }
        else if (dynamic_cast<TxElemDerefNode*>(this->target)) {
        }
        else if (this->target->is_statically_constant()) {
        }
        else
            CERROR(this, "Can't construct reference to non-addressable expression / rvalue.");
    }

    virtual bool is_statically_constant() const override {
        // apparently static const field will not be recognized to have statically const address by llvm
        //return false;
        return this->target->is_statically_constant();  // trying again
    }

    virtual void set_applied_func_arg_types(TxSpecializationIndex six, std::vector<const TxType*>* appliedTypeParameters) override {
        this->target->set_applied_func_arg_types(six, appliedTypeParameters);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxOperatorValueNode : public TxExpressionNode {
public:
    TxOperatorValueNode(const yy::location& parseLocation)
        : TxExpressionNode(parseLocation) { }

    virtual bool has_predefined_type() const override { return true; }
};

class TxBinaryOperatorNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        auto ltype = lhs->resolve_type(six);
        auto rtype = rhs->resolve_type(six);

        const TxType* arithResultType = nullptr;
        if (auto scalar_ltype = dynamic_cast<const TxScalarType*>(ltype)) {
            if (auto scalar_rtype = dynamic_cast<const TxScalarType*>(rtype)) {
                if (scalar_ltype != scalar_rtype) {
                    if (scalar_rtype->auto_converts_to(*scalar_ltype)) {
                        // wrap rhs with cast instruction node
                        this->rhs = new TxScalarConvNode(this->rhs->parseLocation, this->rhs, scalar_ltype);
                        this->rhs->symbol_declaration_pass(six, this->context(six));
                        //this->rhs->symbol_resolution_pass(six);
                        arithResultType = scalar_ltype;
                    }
                    else if (scalar_ltype->auto_converts_to(*scalar_rtype)) {
                        // wrap lhs with cast instruction node
                        this->lhs = new TxScalarConvNode(this->lhs->parseLocation, this->lhs, scalar_rtype);
                        this->lhs->symbol_declaration_pass(six, this->context(six));
                        //this->lhs->symbol_resolution_pass(six);
                        arithResultType = scalar_rtype;
                    }
                }
                else
                    // same type, no additional action necessary
                    arithResultType = scalar_ltype;
            }
            if (arithResultType) {
                if (op_class == TXOC_BOOLEAN)
                    CERROR(this, "Can't perform boolean operation on operands of scalar type: " << ltype);
            }
            else if (rtype)
                CERROR(this, "Mismatching scalar operand types for binary operator " << this->op << ": " << ltype << ", " << rtype);
            else
                return nullptr;
        }
        else if (dynamic_cast<const TxBoolType*>(ltype)) {
            if (dynamic_cast<const TxBoolType*>(rtype)) {
                if (op_class == TXOC_ARITHMETIC)
                    CERROR(this, "Can't perform arithmetic operation on operands of boolean type: " << this->op);
            }
            else
                CERROR(this, "Mismatching operand types for binary operator " << this->op << ": " << ltype << ", " << rtype);
        }
        else if (dynamic_cast<const TxReferenceType*>(ltype)) {
            if (dynamic_cast<const TxReferenceType*>(rtype)) {
                if (op_class != TXOC_EQUALITY)
                    CERROR(this, "Invalid operator for reference operands: " << this->op);
            }
            else
                CERROR(this, "Mismatching operand types for binary operator " << this->op << ": " << ltype << ", " << rtype);
        }
        else if (ltype && rtype) {
            CERROR(this, "Unsupported operand types for binary operator " << this->op << ": " << ltype << ", " << rtype);
        }
        else
            return nullptr;

        if (this->op_class == TXOC_ARITHMETIC) {
            // Note: After analyzing conversions, the lhs will hold the proper resulting type.
            return arithResultType;
        }
        else {  // TXOC_EQUALITY, TXOC_COMPARISON, TXOC_BOOLEAN
            return this->types(six).get_builtin_type(BOOL);
        }
    }

public:
    const TxOperation op;
    TxExpressionNode* lhs;
    TxExpressionNode* rhs;
    const int op_class;

    TxBinaryOperatorNode(const yy::location& parseLocation, TxExpressionNode* lhs, const TxOperation op, TxExpressionNode* rhs)
            : TxOperatorValueNode(parseLocation), op(op), lhs(lhs), rhs(rhs), op_class(get_op_class(op))  {
        ASSERT(is_valid(op), "Invalid operator value: " << (int)op);
    }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        lhs->symbol_declaration_pass(six, lexContext);
        rhs->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxExpressionNode::symbol_resolution_pass(six);
        lhs->symbol_resolution_pass(six);
        rhs->symbol_resolution_pass(six);
    }

    virtual bool is_statically_constant() const override {
        return this->lhs->is_statically_constant() && this->rhs->is_statically_constant();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxUnaryMinusNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        auto type = this->operand->resolve_type(six);
        if (! dynamic_cast<const TxScalarType*>(type))
            CERROR(this, "Invalid operand type for unary '-', not of scalar type: " << (type ? type->to_string().c_str() : "NULL"));
        else if (auto intType = dynamic_cast<const TxIntegerType*>(type))
            if (! intType->is_signed()) {
                // promote unsigned integers upon negation
                // TODO: if operand is an integer literal (or statically constant) and small enough, convert to signed of same width
                bool mod = intType->is_modifiable();
                switch (intType->get_type_id()) {
                case UBYTE:
                    type = this->types(six).get_builtin_type(SHORT, mod);
                    break;
                case USHORT:
                    type = this->types(six).get_builtin_type(INT, mod);
                    break;
                case UINT:
                    type = this->types(six).get_builtin_type(LONG, mod);
                    break;
                case ULONG:
                    CERROR(this, "Invalid operand type for unary '-': " << (type ? type->to_string().c_str() : "NULL"));
                    break;
                default:
                    ASSERT(false, "Unknown unsigned integer type id=" << intType->get_type_id() << ": " << intType);
                }
                this->operand = new TxScalarConvNode(this->operand->parseLocation, this->operand, static_cast<const TxScalarType*>(type));
                this->operand->symbol_declaration_pass(six, this->context(six));
                this->operand->symbol_resolution_pass(six);
            }
        return type;
    }

public:
    TxExpressionNode* operand;
    TxUnaryMinusNode(const yy::location& parseLocation, TxExpressionNode* operand)
        : TxOperatorValueNode(parseLocation), operand(operand) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        operand->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxExpressionNode::symbol_resolution_pass(six);
        operand->symbol_resolution_pass(six);
    }

    virtual bool is_statically_constant() const override {
        return this->operand->is_statically_constant();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxUnaryLogicalNotNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        return this->types(six).get_builtin_type(BOOL);
    }

public:
    TxExpressionNode* operand;
    TxUnaryLogicalNotNode(const yy::location& parseLocation, TxExpressionNode* operand)
        : TxOperatorValueNode(parseLocation), operand(operand) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        operand->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxExpressionNode::symbol_resolution_pass(six);
        operand->symbol_resolution_pass(six);
        auto type = operand->get_type(six);
        // assume arithmetic, scalar negation:
        if (! dynamic_cast<const TxBoolType*>(type))
            // should we support any auto-conversion to Bool?
            CERROR(this, "Operand of unary '!' is not of Bool type: " << (type ? type->to_string().c_str() : "NULL"));
    }

    virtual bool is_statically_constant() const override {
        return this->operand->is_statically_constant();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxFunctionCallNode : public TxExpressionNode {
    /** resolve possible function overloading by registering actual function signature with callee node */
    void register_callee_signature(TxSpecializationIndex six) const {
        ASSERT (!callee->get_applied_func_arg_types(six), "callee already has applied func arg types: " << callee);
        std::vector<const TxType*>* appliedArgTypes = new std::vector<const TxType*>();
        for (auto argExpr : *this->argsExprList) {
            if (auto argType = argExpr->resolve_type(six))
                appliedArgTypes->push_back(argType);
            else {
                delete appliedArgTypes;
                appliedArgTypes = nullptr;
                break;
            }
        }
        if (appliedArgTypes)
            callee->set_applied_func_arg_types(six, appliedArgTypes);
    }

protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override;

public:
    TxExpressionNode* callee;
    std::vector<TxExpressionNode*>* argsExprList;

    TxFunctionCallNode(const yy::location& parseLocation, TxExpressionNode* callee, std::vector<TxExpressionNode*>* argsExprList)
        : TxExpressionNode(parseLocation), callee(callee), argsExprList(argsExprList) {
        for (auto argExprI = this->argsExprList->begin(); argExprI != this->argsExprList->end(); argExprI++)
            *argExprI = make_generic_conversion_node(*argExprI);
    }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        this->callee->symbol_declaration_pass(six, lexContext);
        for (auto argExpr : *this->argsExprList) {
            if (argExpr->is_context_set(six))
                break;  // can happen if wrapped, e.g. for stack construction calls
            argExpr->symbol_declaration_pass(six, lexContext);
        }
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxExpressionNode::symbol_resolution_pass(six);
        callee->symbol_resolution_pass(six);

        if (auto funcType = dynamic_cast<const TxFunctionType*>(this->callee->resolve_type(six))) {
            // verify matching function signature:
            if (funcType->argumentTypes.size() != this->argsExprList->size()) {
                CERROR(this, "Callee of function call expression has mismatching argument count: " << funcType);
            }
            else if (auto inlineConverter = dynamic_cast<const TxBuiltinConversionFunctionType*>(funcType)) {
                // "inline" function call by replacing with conversion expression to function result type (not required arg type)
                insert_conversion(this->argsExprList->front(), six, inlineConverter->returnType, true);
            }
            else {
                auto argExprI = this->argsExprList->begin();
                for (auto argDefType : funcType->argumentTypes) {
                    // note: similar rules to assignment
                    if (! argDefType->is_concrete())  // move this to lambda expression?
                        // TODO: dynamic concrete type resolution (recognize actual type in runtime when dereferencing a generic pointer)
                        CERROR(*argExprI, "Function argument is not a concrete type (size potentially unknown): " << argDefType);
                    // if function arg is a reference:
                    // TODO: check dataspace rules

                    insert_conversion(*argExprI, six, argDefType);
                    //*argExprI = validate_wrap_assignment(six, *argExprI, argDefType);
                    argExprI++;
                }
            }
        }

        for (auto argExpr : *this->argsExprList)
            argExpr->symbol_resolution_pass(six);

        if (auto inlinedExpression = this->get_spec(six)->inlinedExpression)
            inlinedExpression->symbol_resolution_pass(six);
    }

    virtual bool is_statically_constant() const override {
        if (this->get_spec(0)->inlinedExpression)
            return this->get_spec(0)->inlinedExpression->is_statically_constant();
        return false;
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        if (this->get_spec(0)->inlinedExpression)
            return this->get_spec(0)->inlinedExpression->get_static_constant_proxy();
        return nullptr;
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    llvm::Value* gen_call(LlvmGenerationContext& context, GenScope* scope) const;
    llvm::Value* gen_call(LlvmGenerationContext& context, GenScope* scope, llvm::Value* functionPtrV, llvm::Value* closureRefV) const;
};



/** Special callee expression node for calling constructors. */
class TxConstructorCalleeExprNode : public TxExpressionNode {
    mutable llvm::Value* objectPtrV = nullptr;

    /** @return a function pointer (not a lambda value) */
    virtual llvm::Value* gen_func_ptr(LlvmGenerationContext& context, GenScope* scope) const;

protected:
    TxExpressionNode* objectExpr;

    virtual const TxType* define_type(TxSpecializationIndex six) override;

public:
    TxConstructorCalleeExprNode(const yy::location& parseLocation, TxExpressionNode* objectExpr)
            : TxExpressionNode(parseLocation), objectExpr(objectExpr) { }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        this->objectExpr->symbol_declaration_pass(six, lexContext);
    }
    virtual void symbol_resolution_pass(TxSpecializationIndex six) override { this->objectExpr->symbol_resolution_pass(six); }

    /** @return a lambda value */
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    /** @return an object pointer (not a lambda value) */
    virtual llvm::Value* gen_obj_ptr(LlvmGenerationContext& context, GenScope* scope) const;
};



/** Abstract superclass for memory allocation expressions, for heap and stack allocators. */
class TxMemAllocNode : public TxExpressionNode {
    TxTypeExpressionNode* objTypeExpr;

protected:
    //const TxType* get_object_type() const { return this->objTypeDefiner->get_type(); }
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        return this->objTypeExpr->resolve_type(six);
    }

    TxMemAllocNode(const yy::location& parseLocation, TxTypeExpressionNode* objTypeExpr)
            : TxExpressionNode(parseLocation), objTypeExpr(objTypeExpr) { }

public:
    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override { this->set_context(six, lexContext); }
    virtual void symbol_resolution_pass(TxSpecializationIndex six) override { }
    virtual bool has_predefined_type() const override { return false; }  // for now
};

class TxHeapAllocNode : public TxMemAllocNode {
public:
    TxHeapAllocNode(const yy::location& parseLocation, TxTypeExpressionNode* objTypeExpr)
            : TxMemAllocNode(parseLocation, objTypeExpr) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxStackAllocNode : public TxMemAllocNode {
public:
    TxStackAllocNode(const yy::location& parseLocation, TxTypeExpressionNode* objTypeExpr)
            : TxMemAllocNode(parseLocation, objTypeExpr) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



/** Abstract common superclass for new expression and local init expression */
class TxMakeObjectNode : public TxExpressionNode {
protected:
    TxTypeExpressionNode* typeExpr;
    TxConstructorCalleeExprNode* constructor = nullptr;
    TxFunctionCallNode* constructorCall = nullptr;

    /** Gets the type of the allocated object. Should not be called before resolution. */
    virtual const TxType* get_object_type(TxSpecializationIndex six) const = 0;

    TxMakeObjectNode(const yy::location& parseLocation, TxTypeExpressionNode* typeExpr)
            : TxExpressionNode(parseLocation), typeExpr(typeExpr) { }

public:
    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        auto typeDeclFlags = TXD_PUBLIC | TXD_IMPLICIT;
        // unless the type expression is a directly named type, declare implicit type:
        auto implTypeName = ( this->typeExpr->has_predefined_type() ? "" : lexContext.scope()->make_unique_name("$type") );
        this->typeExpr->symbol_declaration_pass(six, lexContext, lexContext, typeDeclFlags, implTypeName, nullptr);
        this->constructorCall->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxExpressionNode::symbol_resolution_pass(six);
        this->typeExpr->symbol_resolution_pass(six);

        this->constructorCall->symbol_resolution_pass(six);

        if (dynamic_cast<const TxBuiltinConversionFunctionType*>(this->constructor->get_type(six))) {
            // "inline" initialization by replacing with conversion expression
            // note: conversion already inserted by function call node
            this->get_spec(six)->inlinedExpression = this->constructorCall->argsExprList->front();
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** Makes a new object in newly allocated heap memory and returns it by reference. */
class TxNewExprNode : public TxMakeObjectNode {
protected:
    virtual const TxType* get_object_type(TxSpecializationIndex six) const override { return this->typeExpr->get_type(six); }

    virtual const TxType* define_type(TxSpecializationIndex six) override {
        // new constructor returns the constructed object by reference
        auto implTypeName = this->context(six).scope()->make_unique_name("$type");
        auto typeDecl = this->context(six).scope()->declare_type(implTypeName, this->get_type_definer(six), TXD_PUBLIC | TXD_IMPLICIT);
        return this->types(six).get_reference_type(typeDecl, TxGenericBinding::make_type_binding("T", this->typeExpr->get_type_definer(six)));
    }

public:
    TxNewExprNode(const yy::location& parseLocation, TxTypeExpressionNode* typeExpr, std::vector<TxExpressionNode*>* argsExprList)
            : TxMakeObjectNode(parseLocation, typeExpr) {
        this->constructor = new TxConstructorCalleeExprNode(parseLocation, new TxHeapAllocNode(parseLocation, typeExpr));
        this->constructorCall = new TxFunctionCallNode(parseLocation, this->constructor, argsExprList);
    }

    virtual bool has_predefined_type() const override { return this->typeExpr->has_predefined_type(); }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** Makes a new object in newly allocated stack memory and returns it by value. */
class TxStackConstructorNode : public TxMakeObjectNode {
protected:
    virtual const TxType* get_object_type(TxSpecializationIndex six) const override { return this->get_type(six); }

    virtual const TxType* define_type(TxSpecializationIndex six) override {
        // stack constructor returns the constructed object by value, not by reference
        return this->typeExpr->resolve_type(six);
    }

public:
    /** produced by the expression syntax: <...type-expr...>(...constructor-args...) */
    TxStackConstructorNode(const yy::location& parseLocation, TxTypeExpressionNode* typeExpr,
                           std::vector<TxExpressionNode*>* argsExprList)
            : TxMakeObjectNode(parseLocation, typeExpr) {
        this->constructor = new TxConstructorCalleeExprNode(parseLocation, new TxStackAllocNode(parseLocation, typeExpr));
        this->constructorCall = new TxFunctionCallNode(parseLocation, this->constructor, argsExprList);
    }

    TxStackConstructorNode(TxFunctionCallNode* originalCall, TxTypeDefiner* typeDefiner)
            : TxStackConstructorNode(originalCall->parseLocation, new TxTypeExprWrapperNode(originalCall->parseLocation, typeDefiner),
                                     originalCall->argsExprList) {
    }

    virtual bool has_predefined_type() const override {
        return this->typeExpr->has_predefined_type();
    }

    // virtual bool is_statically_constant() const override { return true; }  // TODO: review

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



/*=== assignee expressions ===*/

class TxFieldAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        return this->field->resolve_type(six);
    }

public:
    TxFieldValueNode* field;
    TxFieldAssigneeNode(const yy::location& parseLocation, TxFieldValueNode* field)
        : TxAssigneeNode(parseLocation), field(field) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        field->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxAssigneeNode::symbol_resolution_pass(six);
        field->symbol_resolution_pass(six);

        auto fieldDecl = field->get_field_declaration(six);
        if (fieldDecl && fieldDecl->get_storage() == TXS_NOSTORAGE)
            CERROR(this, "Assignee '" << field->symbolName << "' is not an L-value / has no storage.");
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxDerefAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        auto opType = this->operand->resolve_type(six);
        if (auto refType = dynamic_cast<const TxReferenceType*>(opType)) {
            if (refType->is_generic())
                // FUTURE: return constraint type if present
                return this->types(six).get_builtin_type(ANY);
            return refType->target_type();
        }
        CERROR(this, "Operand is not a reference and can't be dereferenced: " << opType);
        return nullptr;
    }

public:
    TxExpressionNode* operand;
    TxDerefAssigneeNode(const yy::location& parseLocation, TxExpressionNode* operand)
        : TxAssigneeNode(parseLocation), operand(operand) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        operand->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxAssigneeNode::symbol_resolution_pass(six);
        operand->symbol_resolution_pass(six);

        if (! dynamic_cast<const TxReferenceType*>(this->operand->get_type(six)))
            CERROR(this, "Can't de-reference non-reference expression.");
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxElemAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six) override {
        auto opType = this->array->resolve_type(six);
        if (auto arrayType = dynamic_cast<const TxArrayType*>(opType)) {
            if (auto elemType = arrayType->element_type())
                return elemType;
            else
                // FUTURE: return constraint type if present
                return this->types(six).get_builtin_type(ANY);  // (not modifiable)
        }
        // operand type is unknown / not an array and can't be subscripted
        CERROR(this, "Can't subscript non-array expression.");
        return nullptr;
    }

public:
    TxExpressionNode* array;
    TxExpressionNode* subscript;
    TxElemAssigneeNode(const yy::location& parseLocation, TxExpressionNode* array, TxExpressionNode* subscript)
        : TxAssigneeNode(parseLocation), array(array), subscript(make_generic_conversion_node(subscript))  { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        array->symbol_declaration_pass(six, lexContext);
        subscript->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six) override {
        TxAssigneeNode::symbol_resolution_pass(six);
        array->symbol_resolution_pass(six);
        insert_conversion(this->subscript, six, this->types(six).get_builtin_type(LONG));
        subscript->symbol_resolution_pass(six);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};
