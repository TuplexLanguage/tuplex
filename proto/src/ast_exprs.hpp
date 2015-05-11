#pragma once

#include "ast_base.hpp"
#include "ast_fields.hpp"


extern llvm::Value* gen_get_struct_member(LlvmGenerationContext& context, GenScope* scope, llvm::Value* structV, unsigned ix);

extern llvm::Value* gen_get_ref_pointer(LlvmGenerationContext& context, GenScope* scope, llvm::Value* refV);
extern llvm::Value* gen_get_ref_typeid(LlvmGenerationContext& context, GenScope* scope, llvm::Value* refV);
extern llvm::Value* gen_ref(LlvmGenerationContext& context, GenScope* scope, llvm::Type* refT, llvm::Value* ptrV, llvm::Value* tidV);

extern llvm::Value* gen_lambda(LlvmGenerationContext& context, GenScope* scope, llvm::Type* lambdaT, llvm::Value* funcV, llvm::Value* closureRefV);


/*=== conversion/casting ===*/

class TxConversionNode : public TxExpressionNode {
protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        // FIXME: type equality logic
        //auto type = expr->resolve_type(resCtx);
        //ASSERT(type && (*type) == (*this->targetType), "Mismatching types in " << this << ": \n" << type << " != \n" << this->targetType);
        return this->targetType;
    }
public:
    TxExpressionNode* expr;
    TxType const * const targetType;

    TxConversionNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxType* targetType)
            : TxExpressionNode(parseLocation), expr(expr), targetType(targetType) {
        ASSERT(targetType, "NULL targetType");
    }

    virtual bool has_predefined_type() const override { return this->targetType->entity(); }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        this->expr->symbol_resolution_pass(resCtx);
    }

    virtual bool is_statically_constant() const override { return this->expr->is_statically_constant(); }

    virtual void semantic_pass() override { ASSERT(this->get_type(), "symbol resolution pass not run for " << this); }
};

class TxScalarConvNode : public TxConversionNode {
    class ScalarConvConstantProxy : public TxConstantProxy {
        const TxScalarConvNode* convNode;
        const TxConstantProxy* originalConstant;

    public:
        ScalarConvConstantProxy() : convNode(), originalConstant()  { }
        ScalarConvConstantProxy(const TxScalarConvNode* convNode, const TxConstantProxy* originalConstant)
            : convNode(convNode), originalConstant(originalConstant)  { }

        void init(const TxScalarConvNode* convNode, const TxConstantProxy* originalConstant) {
            this->convNode = convNode;
            this->originalConstant = originalConstant;
        }

        inline const TxConstantProxy* original_constant() const { return this->originalConstant; }
        virtual const TxType* get_type() const override { return this->convNode->targetType; }
        virtual uint32_t get_value_UInt() const override { return this->originalConstant->get_value_UInt(); }
        virtual llvm::Constant* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
    };

    ScalarConvConstantProxy constProxy;
public:
    TxScalarConvNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxScalarType* targetType)
        : TxConversionNode(parseLocation, expr, targetType), constProxy()  { }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxConversionNode::symbol_resolution_pass(resCtx);
        if (auto originalConstant = this->expr->get_static_constant_proxy())
            this->constProxy.init(this, originalConstant);
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        return (this->constProxy.original_constant() ? &this->constProxy : nullptr);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxBoolConvNode : public TxConversionNode {
public:
    TxBoolConvNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxBoolType* targetType)
        : TxConversionNode(parseLocation, expr, targetType) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxReferenceConvNode : public TxConversionNode {
public:
    TxReferenceConvNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxReferenceType* targetType)
        : TxConversionNode(parseLocation, expr, targetType) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** Casts (not converts) between object specializations (across type parameters and inheritance). */
class TxObjSpecCastNode : public TxConversionNode {
public:
    TxObjSpecCastNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxType* targetType)
        : TxConversionNode(parseLocation, expr, targetType) { }
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



/*=== expressions ===*/

class TxReferenceDerefNode : public TxExpressionNode {
    /** internal "cache" to prevent multiple code generations */
    mutable llvm::Value* refExprValue = nullptr;

protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        auto opType = this->reference->resolve_type(resCtx);
        if (auto refType = dynamic_cast<const TxReferenceType*>(opType)) {
            if (refType->is_generic())
                // FUTURE: return constraint type if present
                return this->types().get_builtin_type(ANY);
            return refType->target_type(resCtx);
        }
        cerror("Operand is not a reference and can't be dereferenced: %s", opType->to_string().c_str());
        return nullptr;
    }

public:
    TxExpressionNode* reference;
    TxReferenceDerefNode(const yy::location& parseLocation, TxExpressionNode* operand)
        : TxExpressionNode(parseLocation), reference(operand) { }

    virtual bool has_predefined_type() const override { return this->reference->has_predefined_type(); }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        reference->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        this->reference->symbol_resolution_pass(resCtx);
    }

    virtual bool is_statically_constant() const {
        return false;  // can we ever know if target is statically constant?
    }

    virtual void semantic_pass() {
        reference->semantic_pass();
        if (! dynamic_cast<const TxReferenceType*>(this->reference->get_type()))
            cerror("Can't de-reference non-reference expression.");
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
    virtual llvm::Value* code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxElemDerefNode : public TxExpressionNode {
protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        auto opType = this->array->resolve_type(resCtx);
        if (auto arrayType = dynamic_cast<const TxArrayType*>(opType)) {
            if (auto elemType = arrayType->element_type(resCtx))
                return elemType;
            else
                // FUTURE: return constraint type if present
                return this->types().get_builtin_type(ANY);
        }
        if (opType)
            cerror("Operand is not an array and can't be subscripted: %s", opType->to_string().c_str());
        return nullptr;
    }

public:
    TxExpressionNode* array;
    TxExpressionNode* subscript;
    TxElemDerefNode(const yy::location& parseLocation, TxExpressionNode* operand, TxExpressionNode* subscript)
        : TxExpressionNode(parseLocation), array(operand), subscript(subscript)  { }

    virtual bool has_predefined_type() const override { return this->array->has_predefined_type(); }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        this->array->symbol_declaration_pass(lexContext);
        this->subscript->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        this->array->symbol_resolution_pass(resCtx);
        this->subscript->symbol_resolution_pass(resCtx);
        this->subscript = validate_wrap_convert(resCtx, this->subscript, this->types().get_builtin_type(LONG));
    }

    virtual bool is_statically_constant() const {
        return this->array->is_statically_constant() && this->subscript->is_statically_constant();
    }

    virtual void semantic_pass() override {
        array->semantic_pass();
        subscript->semantic_pass();
    }

    virtual llvm::Value* code_gen_address(LlvmGenerationContext& context, GenScope* scope) const;
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxReferenceToNode : public TxExpressionNode {
protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        return this->types().get_reference_type(nullptr, TxGenericBinding::make_type_binding("T", this->target));
    }

public:
    TxExpressionNode* target;

    TxReferenceToNode(const yy::location& parseLocation, TxExpressionNode* target)
        : TxExpressionNode(parseLocation), target(target) { }

    virtual bool has_predefined_type() const override { return false; }  // (this expr constructs new type)

    virtual void symbol_declaration_pass(LexicalContext& lexContext) {
        this->set_context(lexContext);
        target->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        this->target->symbol_resolution_pass(resCtx);
    }

    virtual bool is_statically_constant() const override {
        // apparently static const field will not be recognized to have statically const address by llvm
        //return false;
        return this->target->is_statically_constant();  // trying again
    }

    virtual void semantic_pass() override {
        this->target->semantic_pass();
        if (dynamic_cast<TxFieldValueNode*>(this->target)) {
        }
        else if (dynamic_cast<TxElemDerefNode*>(this->target)) {
        }
        else if (this->target->is_statically_constant()) {
        }
        else
            cerror("Can't construct reference to non-addressable expression / rvalue.");
        //if (this->get_target_entity()->get_storage() == TXS_NOSTORAGE)
        //    parser_error(this->parseLocation, "Can't construct reference to non-addressable expression.");
    }

    virtual void set_applied_func_arg_types(std::vector<const TxType*>* appliedTypeParameters) override {
        this->target->set_applied_func_arg_types(appliedTypeParameters);
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
    bool reference_operands = false;
protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        auto ltype = lhs->resolve_type(resCtx);
        auto rtype = rhs->resolve_type(resCtx);

        const TxType* arithResultType = nullptr;
        if (auto scalar_ltype = dynamic_cast<const TxScalarType*>(ltype)) {
            if (auto scalar_rtype = dynamic_cast<const TxScalarType*>(rtype)) {
                if (scalar_ltype != scalar_rtype) {
                    if (scalar_ltype->auto_converts_from(*scalar_rtype)) {
                        // wrap rhs with cast instruction node
                        this->rhs = new TxScalarConvNode(this->rhs->parseLocation, this->rhs, scalar_ltype);
                        this->rhs->symbol_declaration_pass(this->context());
                        this->rhs->symbol_resolution_pass(resCtx);
                        arithResultType = scalar_ltype;
                    }
                    else if (scalar_rtype->auto_converts_from(*scalar_ltype)) {
                        // wrap lhs with cast instruction node
                        this->lhs = new TxScalarConvNode(this->lhs->parseLocation, this->lhs, scalar_rtype);
                        this->lhs->symbol_declaration_pass(this->context());
                        this->lhs->symbol_resolution_pass(resCtx);
                        arithResultType = scalar_rtype;
                    }
                }
                else
                    // same type, no additional action necessary
                    arithResultType = scalar_ltype;
            }
            if (arithResultType) {
                if (op_class == TXOC_BOOLEAN)
                    this->cerror("Can't perform boolean operation on operands of scalar type: %s", ltype->to_string().c_str());
            }
            else if (rtype)
                cerror("Mismatching scalar operand types for binary operator %s: %s, %s", to_cstring(this->op), ltype->to_string().c_str(), rtype->to_string().c_str());
            else
                return nullptr;
        }
        else if (dynamic_cast<const TxBoolType*>(ltype)) {
            if (dynamic_cast<const TxBoolType*>(rtype)) {
                if (op_class == TXOC_ARITHMETIC)
                    this->cerror("Can't perform arithmetic operation on operands of boolean type: %s", to_cstring(this->op));
            }
            else
                cerror("Mismatching operand types for binary operator %s: %s, %s", to_cstring(this->op), ltype->to_string().c_str(), rtype->to_string().c_str());
        }
        else if (dynamic_cast<const TxReferenceType*>(ltype)) {
            if (dynamic_cast<const TxReferenceType*>(rtype)) {
                if (op_class == TXOC_EQUALITY)
                    this->reference_operands = true;
                else
                    cerror("Invalid operator for reference operands: %s", to_cstring(this->op));
            }
            else
                cerror("Mismatching operand types for binary operator %s: %s, %s", to_cstring(this->op), ltype->to_string().c_str(), rtype->to_string().c_str());
        }
        else if (ltype && rtype) {
            cerror("Unsupported operand types for binary operator %s: %s, %s", to_cstring(this->op), ltype->to_string().c_str(), rtype->to_string().c_str());
        }
        else
            return nullptr;

        if (this->op_class == TXOC_ARITHMETIC) {
            // Note: After analyzing conversions, the lhs will hold the proper resulting type.
            return arithResultType;
        }
        else {  // TXOC_EQUALITY, TXOC_COMPARISON, TXOC_BOOLEAN
            return this->types().get_builtin_type(BOOL);
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

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        lhs->symbol_declaration_pass(lexContext);
        rhs->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        lhs->symbol_resolution_pass(resCtx);
        rhs->symbol_resolution_pass(resCtx);
    }

    virtual bool is_statically_constant() const override {
        return this->lhs->is_statically_constant() && this->rhs->is_statically_constant();
    }

    virtual void semantic_pass() override {
        lhs->semantic_pass();
        rhs->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxUnaryMinusNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        // TODO: promote unsigned integers upon negation
        auto type = this->operand->resolve_type(resCtx);
        if (! dynamic_cast<const TxScalarType*>(type))
            cerror("Operand of unary '-' is not of scalar type: %s", (type ? type->to_string().c_str() : "NULL"));
        return type;
    }

public:
    TxExpressionNode* operand;
    TxUnaryMinusNode(const yy::location& parseLocation, TxExpressionNode* operand)
        : TxOperatorValueNode(parseLocation), operand(operand) { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        operand->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        operand->symbol_resolution_pass(resCtx);
    }

    virtual bool is_statically_constant() const override {
        return this->operand->is_statically_constant();
    }

    virtual void semantic_pass() override {
        operand->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxUnaryLogicalNotNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        return this->types().get_builtin_type(BOOL);
    }

public:
    TxExpressionNode* operand;
    TxUnaryLogicalNotNode(const yy::location& parseLocation, TxExpressionNode* operand)
        : TxOperatorValueNode(parseLocation), operand(operand) { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        operand->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        operand->symbol_resolution_pass(resCtx);
        auto type = operand->get_type();
        // assume arithmetic, scalar negation:
        if (! dynamic_cast<const TxBoolType*>(type))
            // should we support any auto-conversion to Bool?
            cerror("Operand of unary '!' is not of Bool type: %s", (type ? type->to_string().c_str() : "NULL"));
    }

    virtual bool is_statically_constant() const override {
        return this->operand->is_statically_constant();
    }

    virtual void semantic_pass() override {
        operand->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



class TxFunctionCallNode : public TxExpressionNode {
    TxExpressionNode* inlinedExpression = nullptr;  // substitutes the function call if non-null

    /** resolve possible function overloading by registering actual function signature with callee node */
    void register_callee_signature(ResolutionContext& resCtx) const {
        ASSERT (!callee->hasAppliedFuncArgTypes(), "callee already has applied func arg types: " << callee);
        std::vector<const TxType*>* appliedArgTypes = new std::vector<const TxType*>();
        for (auto argExpr : *this->argsExprList) {
            if (auto argType = argExpr->resolve_type(resCtx))
                appliedArgTypes->push_back(argType);
            else {
                delete appliedArgTypes;
                appliedArgTypes = nullptr;
                break;
            }
        }
        if (appliedArgTypes)
            callee->set_applied_func_arg_types(appliedArgTypes);
    }

protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override;

public:
    TxExpressionNode* callee;
    std::vector<TxExpressionNode*>* argsExprList;

    TxFunctionCallNode(const yy::location& parseLocation, TxExpressionNode* callee, std::vector<TxExpressionNode*>* argsExprList)
        : TxExpressionNode(parseLocation), callee(callee), argsExprList(argsExprList) { }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        this->callee->symbol_declaration_pass(lexContext);
        for (auto argExpr : *this->argsExprList)
            argExpr->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        callee->symbol_resolution_pass(resCtx);
        for (auto argExpr : *this->argsExprList)
            argExpr->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        if (this->inlinedExpression)
            this->inlinedExpression->semantic_pass();
        else
            this->callee->semantic_pass();
        for (auto argExpr : *this->argsExprList)
            argExpr->semantic_pass();
    }

    virtual bool is_statically_constant() const override {
        if (this->inlinedExpression)
            return this->inlinedExpression->is_statically_constant();
        return false;
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        if (this->inlinedExpression)
            return this->inlinedExpression->get_static_constant_proxy();
        return nullptr;
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    llvm::Value* gen_call(LlvmGenerationContext& context, GenScope* scope) const;
};


/** Callee expression node for calling constructors. */
class TxConstructorCalleeExprNode : public TxExpressionNode {
    const TxTypeDefiner* objTypeDefiner;
    TxFieldEntity* constructorEntity = nullptr;

    mutable llvm::Value* objectPtrV = nullptr;
    friend class TxNewExprNode;
    friend class TxStackConstructorNode;

    const TxType* get_object_type() const { return this->objTypeDefiner->get_type(); }

protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override;

public:
    TxConstructorCalleeExprNode(const yy::location& parseLocation, const TxTypeDefiner* objTypeDefiner)
            : TxExpressionNode(parseLocation), objTypeDefiner(objTypeDefiner) { }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override { }

    virtual void semantic_pass() override { }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxNewExprNode : public TxExpressionNode {
    TxConstructorCalleeExprNode* constructor;
    TxFunctionCallNode* constructorCall;

protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        return this->types().get_reference_type(nullptr, TxGenericBinding::make_type_binding("T", this->typeExpr));
//        // currently we require the new expr type to be modifiable  TODO: solve assignment from new expr differently
//        if (auto type = this->typeExpr->resolve_type(resCtx)) {
//            if (type->is_modifiable())
//                return type;
//            else if (! type->is_immutable())
//                return this->types().get_modifiable_type(nullptr, type);
//            else
//                cerror("Not yet supported to assign immutable type in 'new' expression");
//        }
//        return nullptr;
    }

public:
    TxTypeExpressionNode* typeExpr;

    TxNewExprNode(const yy::location& parseLocation, TxTypeExpressionNode* typeExpr, std::vector<TxExpressionNode*>* argsExprList)
            : TxExpressionNode(parseLocation), typeExpr(typeExpr) {
        this->constructor = new TxConstructorCalleeExprNode(parseLocation, this->typeExpr);
        this->constructorCall = new TxFunctionCallNode(parseLocation, this->constructor, argsExprList);
    }

    virtual bool has_predefined_type() const override { return this->typeExpr->has_predefined_type(); }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        // (similar to TxFieldDefNode::symbol_declaration_pass())
        this->set_context(lexContext);
        auto typeDeclFlags = TXD_PUBLIC | TXD_IMPLICIT;
        // unless the type expression is a directly named type, declare implicit type entity for this field's type:
        if (this->typeExpr->has_predefined_type())
            this->typeExpr->symbol_declaration_pass(lexContext, lexContext, typeDeclFlags);
        else {
            auto implTypeName = lexContext.scope()->get_unique_name("$type");
            this->typeExpr->symbol_declaration_pass(lexContext, lexContext, typeDeclFlags, implTypeName);
        }

        this->constructorCall->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        this->typeExpr->symbol_resolution_pass(resCtx);
        this->constructorCall->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        this->typeExpr->semantic_pass();
        this->constructorCall->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxStackConstructorNode : public TxExpressionNode {
    class TxTypeDefWrapper : public TxTypeDefiner {
        const TxType* txType;
    public:
        TxTypeDefWrapper(const TxType* txType) : txType(txType) {}
        virtual const TxType* resolve_type(ResolutionContext& resCtx) { return this->txType; }
        virtual const TxType* attempt_get_type() const { return this->txType; }
        virtual const TxType* get_type() const  { return this->txType; }
    };

    TxTypeDefWrapper typeDefWrapper;
    TxConstructorCalleeExprNode* constructor;
    TxFunctionCallNode* constructorCall;

    const TxType* get_object_type() const { return this->typeDefWrapper.get_type(); }

protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        return this->types().get_reference_type(nullptr, TxGenericBinding::make_type_binding("T", &this->typeDefWrapper));
    }

public:
    TxStackConstructorNode(TxFunctionCallNode* originalCall, const TxType* objectType)
            : TxExpressionNode(originalCall->parseLocation), typeDefWrapper(objectType) {
        this->constructor = new TxConstructorCalleeExprNode(parseLocation, &this->typeDefWrapper);
        this->constructorCall = new TxFunctionCallNode(parseLocation, this->constructor, originalCall->argsExprList);
    }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        // does not invoke proper symbol_declaration_pass() since assumes that originalCall has already done that
        // on its arguments:
        // this->constructorCall->symbol_declaration_pass(lexContext);
        this->constructorCall->set_context(this);
        this->constructor->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(resCtx);
        this->constructorCall->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        this->constructorCall->semantic_pass();
    }

    virtual bool is_statically_constant() const { return true; }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



/*=== assignee expressions ===*/

class TxFieldAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        return this->field->resolve_type(resCtx);
    }

public:
    TxFieldValueNode* field;
    TxFieldAssigneeNode(const yy::location& parseLocation, TxFieldValueNode* field)
        : TxAssigneeNode(parseLocation), field(field) { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        field->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxAssigneeNode::symbol_resolution_pass(resCtx);
        field->symbol_resolution_pass(resCtx);

        auto entity = field->get_field_entity();
        if (entity && entity->get_storage() == TXS_NOSTORAGE)
            cerror("Assignee %s is not an L-value / has no storage.", field->memberName.c_str());
    }

    virtual void semantic_pass() override {
        field->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxDerefAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        auto opType = this->operand->resolve_type(resCtx);
        if (auto refType = dynamic_cast<const TxReferenceType*>(opType)) {
            if (refType->is_generic())
                // FUTURE: return constraint type if present
                return this->types().get_builtin_type(ANY);
            return refType->target_type(resCtx);
        }
        cerror("Operand is not a reference and can't be dereferenced: %s", opType->to_string().c_str());
        return nullptr;
    }

public:
    TxExpressionNode* operand;
    TxDerefAssigneeNode(const yy::location& parseLocation, TxExpressionNode* operand)
        : TxAssigneeNode(parseLocation), operand(operand) { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        operand->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxAssigneeNode::symbol_resolution_pass(resCtx);
        operand->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        operand->semantic_pass();
        if (! dynamic_cast<const TxReferenceType*>(this->operand->get_type()))
            cerror("Can't de-reference non-reference expression.");
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxElemAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type(ResolutionContext& resCtx) override {
        auto opType = this->array->resolve_type(resCtx);
        subscript = validate_wrap_convert(resCtx, subscript, this->types().get_builtin_type(LONG));
        if (auto arrayType = dynamic_cast<const TxArrayType*>(opType)) {
            if (auto elemType = arrayType->element_type(resCtx))
                return elemType;
            else
                // FUTURE: return constraint type if present
                return this->types().get_builtin_type(ANY);  // (not modifiable)
        }
        // operand type is unknown / not an array and can't be subscripted
        cerror("Can't subscript non-array expression.");
        return nullptr;
    }

public:
    TxExpressionNode* array;
    TxExpressionNode* subscript;
    TxElemAssigneeNode(const yy::location& parseLocation, TxExpressionNode* array, TxExpressionNode* subscript)
        : TxAssigneeNode(parseLocation), array(array), subscript(subscript)  { }

    virtual void symbol_declaration_pass(LexicalContext& lexContext) override {
        this->set_context(lexContext);
        array->symbol_declaration_pass(lexContext);
        subscript->symbol_declaration_pass(lexContext);
    }

    virtual void symbol_resolution_pass(ResolutionContext& resCtx) override {
        TxAssigneeNode::symbol_resolution_pass(resCtx);
        array->symbol_resolution_pass(resCtx);
        subscript->symbol_resolution_pass(resCtx);
    }

    virtual void semantic_pass() override {
        array->semantic_pass();
        subscript->semantic_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};
