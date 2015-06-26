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
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        // FIXME: type equality logic
        //auto type = expr->resolve_type(six, resCtx);
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

    virtual bool has_predefined_type() const override { return this->targetType->get_symbol(); }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        if (! this->expr->is_context_set(six))
            this->expr->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(six, resCtx);
        this->expr->symbol_resolution_pass(six, resCtx);
    }

    virtual bool is_statically_constant() const override { return this->expr->is_statically_constant(); }
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
        virtual llvm::Constant* code_gen(LlvmGenerationContext& context, GenScope* scope) const;
    };

    ScalarConvConstantProxy constProxy;
public:
    TxScalarConvNode(const yy::location& parseLocation, TxExpressionNode* expr, const TxScalarType* targetType)
        : TxConversionNode(parseLocation, expr, targetType), constProxy()  { }

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxConversionNode::symbol_resolution_pass(six, resCtx);
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
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        auto opType = this->reference->resolve_type(six, resCtx);
        if (auto refType = dynamic_cast<const TxReferenceType*>(opType)) {
            if (refType->is_generic())
                // FUTURE: return constraint type if present
                return this->types().get_builtin_type(ANY);
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

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(six, resCtx);
        this->reference->symbol_resolution_pass(six, resCtx);

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
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        auto opType = this->array->resolve_type(six, resCtx);
        if (auto arrayType = dynamic_cast<const TxArrayType*>(opType)) {
            if (auto elemType = arrayType->element_type())
                return elemType;
            else
                // FUTURE: return constraint type if present
                return this->types().get_builtin_type(ANY);
        }
        if (opType)
            CERROR(this, "Operand is not an array and can't be subscripted: " << opType);
        return nullptr;
    }

public:
    TxExpressionNode* array;
    TxExpressionNode* subscript;
    TxElemDerefNode(const yy::location& parseLocation, TxExpressionNode* operand, TxExpressionNode* subscript)
        : TxExpressionNode(parseLocation), array(operand), subscript(subscript)  { }

    virtual bool has_predefined_type() const override { return this->array->has_predefined_type(); }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        this->array->symbol_declaration_pass(six, lexContext);
        this->subscript->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(six, resCtx);
        this->array->symbol_resolution_pass(six, resCtx);
        this->subscript->symbol_resolution_pass(six, resCtx);
        this->subscript = validate_wrap_convert(six, resCtx, this->subscript, this->types().get_builtin_type(LONG));
    }

    virtual bool is_statically_constant() const {
        return this->array->is_statically_constant() && this->subscript->is_statically_constant();
    }

    virtual llvm::Value* code_gen_address(LlvmGenerationContext& context, GenScope* scope) const;
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};


class TxReferenceToNode : public TxExpressionNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        auto implTypeName = this->context(six).scope()->make_unique_name("$type");
        auto typeDecl = this->context(six).scope()->declare_type(implTypeName, this->get_type_definer(six), TXD_PUBLIC | TXD_IMPLICIT);
        return this->types().get_reference_type(typeDecl, TxGenericBinding::make_type_binding("T", this->target->get_type_definer(six)));
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

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(six, resCtx);
        this->target->symbol_resolution_pass(six, resCtx);

        if (dynamic_cast<TxFieldValueNode*>(this->target)) {
        }
        else if (dynamic_cast<TxElemDerefNode*>(this->target)) {
        }
        else if (this->target->is_statically_constant()) {
        }
        else
            CERROR(this, "Can't construct reference to non-addressable expression / rvalue.");
        //if (this->get_target_entity()->get_storage() == TXS_NOSTORAGE)
        //    parser_error(this->parseLocation, "Can't construct reference to non-addressable expression.");
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
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        auto ltype = lhs->resolve_type(six, resCtx);
        auto rtype = rhs->resolve_type(six, resCtx);

        const TxType* arithResultType = nullptr;
        if (auto scalar_ltype = dynamic_cast<const TxScalarType*>(ltype)) {
            if (auto scalar_rtype = dynamic_cast<const TxScalarType*>(rtype)) {
                if (scalar_ltype != scalar_rtype) {
                    if (scalar_ltype->auto_converts_from(*scalar_rtype)) {
                        // wrap rhs with cast instruction node
                        this->rhs = new TxScalarConvNode(this->rhs->parseLocation, this->rhs, scalar_ltype);
                        this->rhs->symbol_declaration_pass(six, this->context(six));
                        this->rhs->symbol_resolution_pass(six, resCtx);
                        arithResultType = scalar_ltype;
                    }
                    else if (scalar_rtype->auto_converts_from(*scalar_ltype)) {
                        // wrap lhs with cast instruction node
                        this->lhs = new TxScalarConvNode(this->lhs->parseLocation, this->lhs, scalar_rtype);
                        this->lhs->symbol_declaration_pass(six, this->context(six));
                        this->lhs->symbol_resolution_pass(six, resCtx);
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

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        lhs->symbol_declaration_pass(six, lexContext);
        rhs->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(six, resCtx);
        lhs->symbol_resolution_pass(six, resCtx);
        rhs->symbol_resolution_pass(six, resCtx);
    }

    virtual bool is_statically_constant() const override {
        return this->lhs->is_statically_constant() && this->rhs->is_statically_constant();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxUnaryMinusNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        // TODO: promote unsigned integers upon negation
        auto type = this->operand->resolve_type(six, resCtx);
        if (! dynamic_cast<const TxScalarType*>(type))
            CERROR(this, "Operand of unary '-' is not of scalar type: " << (type ? type->to_string().c_str() : "NULL"));
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

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(six, resCtx);
        operand->symbol_resolution_pass(six, resCtx);
    }

    virtual bool is_statically_constant() const override {
        return this->operand->is_statically_constant();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxUnaryLogicalNotNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        return this->types().get_builtin_type(BOOL);
    }

public:
    TxExpressionNode* operand;
    TxUnaryLogicalNotNode(const yy::location& parseLocation, TxExpressionNode* operand)
        : TxOperatorValueNode(parseLocation), operand(operand) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        operand->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(six, resCtx);
        operand->symbol_resolution_pass(six, resCtx);
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
//    /** substitutes the function call if non-null */
//    TxExpressionNode* inlinedExpression = nullptr;

    /** resolve possible function overloading by registering actual function signature with callee node */
    void register_callee_signature(TxSpecializationIndex six, ResolutionContext& resCtx) const {
        ASSERT (!callee->get_applied_func_arg_types(six), "callee already has applied func arg types: " << callee);
        std::vector<const TxType*>* appliedArgTypes = new std::vector<const TxType*>();
        for (auto argExpr : *this->argsExprList) {
            if (auto argType = argExpr->resolve_type(six, resCtx))
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
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override;

public:
    TxExpressionNode* callee;
    std::vector<TxExpressionNode*>* argsExprList;

    TxFunctionCallNode(const yy::location& parseLocation, TxExpressionNode* callee, std::vector<TxExpressionNode*>* argsExprList)
        : TxExpressionNode(parseLocation), callee(callee), argsExprList(argsExprList) { }

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

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(six, resCtx);
        callee->symbol_resolution_pass(six, resCtx);
        for (auto argExpr : *this->argsExprList)
            argExpr->symbol_resolution_pass(six, resCtx);
    }

    virtual bool is_statically_constant() const override {
        if (this->get_spec(0).inlinedExpression)
            return this->get_spec(0).inlinedExpression->is_statically_constant();
        return false;
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const override {
        if (this->get_spec(0).inlinedExpression)
            return this->get_spec(0).inlinedExpression->get_static_constant_proxy();
        return nullptr;
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    llvm::Value* gen_call(LlvmGenerationContext& context, GenScope* scope) const;
    llvm::Value* gen_call(LlvmGenerationContext& context, GenScope* scope, llvm::Value* functionPtrV, llvm::Value* closureRefV) const;
};



/** Special callee expression node for calling constructors. */
class TxConstructorCalleeExprNode : public TxExpressionNode {
//    /** The constructor method's declaration */
//    const TxEntityDeclaration* declaration = nullptr;

    mutable llvm::Value* objectPtrV = nullptr;

    /** @return a function pointer (not a lambda value) */
    virtual llvm::Value* gen_func_ptr(LlvmGenerationContext& context, GenScope* scope) const;

protected:
    TxExpressionNode* objectExpr;

    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override;

public:
    TxConstructorCalleeExprNode(const yy::location& parseLocation, TxExpressionNode* objectExpr)
            : TxExpressionNode(parseLocation), objectExpr(objectExpr) { }

    virtual bool has_predefined_type() const override { return true; }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        this->objectExpr->symbol_declaration_pass(six, lexContext);
    }
    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override { this->objectExpr->symbol_resolution_pass(six, resCtx); }

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
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        return this->objTypeExpr->resolve_type(six, resCtx);
    }

    TxMemAllocNode(const yy::location& parseLocation, TxTypeExpressionNode* objTypeExpr)
            : TxExpressionNode(parseLocation), objTypeExpr(objTypeExpr) { }

public:
    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override { this->set_context(six, lexContext); }
    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override { }
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

//    /** substitutes the constructor call if non-null */
//    TxExpressionNode* inlinedInitializer = nullptr;

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

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxExpressionNode::symbol_resolution_pass(six, resCtx);
        this->typeExpr->symbol_resolution_pass(six, resCtx);

        this->constructorCall->symbol_resolution_pass(six, resCtx);

        if (auto inlineFunc = dynamic_cast<const TxBuiltinConversionFunctionType*>(this->constructor->get_type(six))) {
            // "inline" initialization by replacing with conversion expression
            this->get_spec(six).inlinedExpression = validate_wrap_convert(six, resCtx, this->constructorCall->argsExprList->front(),
                                                                          inlineFunc->returnType, true);
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** Makes a new object in newly allocated heap memory and returns it by reference. */
class TxNewExprNode : public TxMakeObjectNode {
protected:
    virtual const TxType* get_object_type(TxSpecializationIndex six) const override { return this->typeExpr->get_type(six); }

    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        // new constructor returns the constructed object by reference
        auto implTypeName = this->context(six).scope()->make_unique_name("$type");
        auto typeDecl = this->context(six).scope()->declare_type(implTypeName, this->get_type_definer(six), TXD_PUBLIC | TXD_IMPLICIT);
        return this->types().get_reference_type(typeDecl, TxGenericBinding::make_type_binding("T", this->typeExpr->get_type_definer(six)));
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

    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        // stack constructor returns the constructed object by value, not by reference
        return this->typeExpr->resolve_type(six, resCtx);
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
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        return this->field->resolve_type(six, resCtx);
    }

public:
    TxFieldValueNode* field;
    TxFieldAssigneeNode(const yy::location& parseLocation, TxFieldValueNode* field)
        : TxAssigneeNode(parseLocation), field(field) { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        field->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxAssigneeNode::symbol_resolution_pass(six, resCtx);
        field->symbol_resolution_pass(six, resCtx);

        auto fieldDecl = field->get_field_declaration(six);
        if (fieldDecl && fieldDecl->get_storage() == TXS_NOSTORAGE)
            CERROR(this, "Assignee '" << field->memberName << "' is not an L-value / has no storage.");
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxDerefAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        auto opType = this->operand->resolve_type(six, resCtx);
        if (auto refType = dynamic_cast<const TxReferenceType*>(opType)) {
            if (refType->is_generic())
                // FUTURE: return constraint type if present
                return this->types().get_builtin_type(ANY);
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

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxAssigneeNode::symbol_resolution_pass(six, resCtx);
        operand->symbol_resolution_pass(six, resCtx);

        if (! dynamic_cast<const TxReferenceType*>(this->operand->get_type(six)))
            CERROR(this, "Can't de-reference non-reference expression.");
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxElemAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        auto opType = this->array->resolve_type(six, resCtx);
        subscript = validate_wrap_convert(six, resCtx, subscript, this->types().get_builtin_type(LONG));
        if (auto arrayType = dynamic_cast<const TxArrayType*>(opType)) {
            if (auto elemType = arrayType->element_type())
                return elemType;
            else
                // FUTURE: return constraint type if present
                return this->types().get_builtin_type(ANY);  // (not modifiable)
        }
        // operand type is unknown / not an array and can't be subscripted
        CERROR(this, "Can't subscript non-array expression.");
        return nullptr;
    }

public:
    TxExpressionNode* array;
    TxExpressionNode* subscript;
    TxElemAssigneeNode(const yy::location& parseLocation, TxExpressionNode* array, TxExpressionNode* subscript)
        : TxAssigneeNode(parseLocation), array(array), subscript(subscript)  { }

    virtual void symbol_declaration_pass(TxSpecializationIndex six, LexicalContext& lexContext) override {
        this->set_context(six, lexContext);
        array->symbol_declaration_pass(six, lexContext);
        subscript->symbol_declaration_pass(six, lexContext);
    }

    virtual void symbol_resolution_pass(TxSpecializationIndex six, ResolutionContext& resCtx) override {
        TxAssigneeNode::symbol_resolution_pass(six, resCtx);
        array->symbol_resolution_pass(six, resCtx);
        subscript->symbol_resolution_pass(six, resCtx);
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};
