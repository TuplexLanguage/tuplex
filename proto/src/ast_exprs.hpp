#pragma once

#include "ast_declbase.hpp"
#include "ast_wrappers.hpp"
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
    virtual const TxType* define_type() override {
        auto opType = this->reference->resolve_type();
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
    TxReferenceDerefNode(const TxLocation& parseLocation, TxExpressionNode* operand)
        : TxExpressionNode(parseLocation), reference(operand) { }

    virtual TxReferenceDerefNode* make_ast_copy() const override {
        return new TxReferenceDerefNode( this->parseLocation, this->reference->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) {
        this->set_context( lexContext);
        reference->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->reference->symbol_resolution_pass();

        if (! dynamic_cast<const TxReferenceType*>(this->reference->get_type()))
            CERROR(this, "Can't de-reference non-reference expression.");
    }

    virtual bool is_statically_constant() const {
        return false;  // can we ever know if target is statically constant?
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
    virtual llvm::Value* code_gen_typeid(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->reference->visit_ast( visitor, thisAsParent, "ref", context );
    }
};


class TxElemDerefNode : public TxExpressionNode {
protected:
    virtual const TxType* define_type() override {
        auto opType = this->array->resolve_type();
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
    TxMaybeConversionNode* subscript;

    TxElemDerefNode(const TxLocation& parseLocation, TxExpressionNode* operand, TxExpressionNode* subscript)
        : TxExpressionNode(parseLocation), array(operand), subscript(new TxMaybeConversionNode(subscript))  { }

    virtual TxElemDerefNode* make_ast_copy() const override {
        return new TxElemDerefNode( this->parseLocation, this->array->make_ast_copy(),
                                    this->subscript->originalExpr->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        this->array->symbol_declaration_pass( lexContext);
        this->subscript->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->array->symbol_resolution_pass();
        this->subscript->insert_conversion( this->types().get_builtin_type(LONG) );
        this->subscript->symbol_resolution_pass();
    }

    virtual bool is_statically_constant() const {
        return this->array->is_statically_constant() && this->subscript->is_statically_constant();
    }

    virtual llvm::Value* code_gen_address(LlvmGenerationContext& context, GenScope* scope) const;
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->array->visit_ast( visitor, thisAsParent, "array", context );
        this->subscript->visit_ast( visitor, thisAsParent, "subscript", context );
    }
};


class TxReferenceToNode : public TxExpressionNode {
protected:
    virtual const TxType* define_type() override {
//        auto implTypeName = this->context().scope()->make_unique_name("$type");
//        TxDeclarationFlags tmpFlags = TXD_PUBLIC | TXD_IMPLICIT;
//        auto typeDecl = this->context().scope()->declare_type(implTypeName, this, tmpFlags);
        return this->types().get_reference_type( this, TxGenericBinding::make_type_binding("T", this->target), nullptr );
    }

public:
    TxExpressionNode* target;

    TxReferenceToNode(const TxLocation& parseLocation, TxExpressionNode* target)
        : TxExpressionNode(parseLocation), target(target) { }

    virtual TxReferenceToNode* make_ast_copy() const override {
        return new TxReferenceToNode( this->parseLocation, this->target->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) {
        this->set_context( lexContext);
        target->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->target->symbol_resolution_pass();

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

    virtual const std::vector<TxMaybeConversionNode*>* get_applied_func_args() override {
        return this->target->get_applied_func_args();
    }
    virtual void set_applied_func_args( std::vector<TxMaybeConversionNode*>* appliedTypeParameters ) override {
        this->target->set_applied_func_args( appliedTypeParameters );
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->target->visit_ast( visitor, thisAsParent, "target", context );
    }
};



class TxOperatorValueNode : public TxExpressionNode {
public:
    TxOperatorValueNode(const TxLocation& parseLocation)
        : TxExpressionNode(parseLocation) { }
};

class TxBinaryOperatorNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type() override {
        auto ltype = lhs->resolve_type();
        auto rtype = rhs->resolve_type();

        const TxType* arithResultType = nullptr;
        if (auto scalar_ltype = dynamic_cast<const TxScalarType*>(ltype)) {
            if (auto scalar_rtype = dynamic_cast<const TxScalarType*>(rtype)) {
                if (scalar_ltype != scalar_rtype) {
                    if (scalar_rtype->auto_converts_to(*scalar_ltype)) {
                        // wrap rhs with cast instruction node
                        this->rhs = new TxScalarConvNode( this->rhs, scalar_ltype );
                        this->rhs->symbol_declaration_pass( this->context());
                        //this->rhs->symbol_resolution_pass();
                        arithResultType = scalar_ltype;
                    }
                    else if (scalar_ltype->auto_converts_to(*scalar_rtype)) {
                        // wrap lhs with cast instruction node
                        this->lhs = new TxScalarConvNode( this->lhs, scalar_rtype );
                        this->lhs->symbol_declaration_pass( this->context());
                        //this->lhs->symbol_resolution_pass();
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

    TxBinaryOperatorNode(const TxLocation& parseLocation, TxExpressionNode* lhs, const TxOperation op, TxExpressionNode* rhs)
            : TxOperatorValueNode(parseLocation), op(op), lhs(lhs), rhs(rhs), op_class(get_op_class(op))  {
        ASSERT(is_valid(op), "Invalid operator value: " << (int)op);
    }

    virtual TxBinaryOperatorNode* make_ast_copy() const override {
        // FIXME: operands may be TxConversionNodes
        return new TxBinaryOperatorNode( this->parseLocation, this->lhs->make_ast_copy(), this->op, this->rhs->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        lhs->symbol_declaration_pass( lexContext);
        rhs->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        lhs->symbol_resolution_pass();
        rhs->symbol_resolution_pass();
    }

    virtual bool is_statically_constant() const override {
        return this->lhs->is_statically_constant() && this->rhs->is_statically_constant();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->lhs->visit_ast( visitor, thisAsParent, "lhs", context );
        this->rhs->visit_ast( visitor, thisAsParent, "rhs", context );
    }
};

class TxUnaryMinusNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type() override {
        auto type = this->operand->resolve_type();
        if (! dynamic_cast<const TxScalarType*>(type))
            CERROR(this, "Invalid operand type for unary '-', not of scalar type: " << (type ? type->str().c_str() : "NULL"));
        else if (auto intType = dynamic_cast<const TxIntegerType*>(type))
            if (! intType->is_signed()) {
                // promote unsigned integers upon negation
                // TODO: if operand is an integer literal (or statically constant) and small enough, convert to signed of same width
                bool mod = intType->is_modifiable();
                switch (intType->get_type_id()) {
                case UBYTE:
                    type = this->types().get_builtin_type(SHORT, mod);
                    break;
                case USHORT:
                    type = this->types().get_builtin_type(INT, mod);
                    break;
                case UINT:
                    type = this->types().get_builtin_type(LONG, mod);
                    break;
                case ULONG:
                    CERROR(this, "Invalid operand type for unary '-': " << (type ? type->str().c_str() : "NULL"));
                    break;
                default:
                    ASSERT(false, "Unknown unsigned integer type id=" << intType->get_type_id() << ": " << intType);
                }
                this->operand = new TxScalarConvNode( this->operand, static_cast<const TxScalarType*>(type) );
                this->operand->symbol_declaration_pass( this->context());
                this->operand->symbol_resolution_pass();
            }
        return type;
    }

public:
    TxExpressionNode* operand;
    TxUnaryMinusNode(const TxLocation& parseLocation, TxExpressionNode* operand)
        : TxOperatorValueNode(parseLocation), operand(operand) { }

    virtual TxUnaryMinusNode* make_ast_copy() const override {
        // FIXME: operands may be TxConversionNodes
        return new TxUnaryMinusNode( this->parseLocation, this->operand->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        operand->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        operand->symbol_resolution_pass();
    }

    virtual bool is_statically_constant() const override {
        return this->operand->is_statically_constant();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->operand->visit_ast( visitor, thisAsParent, "operand", context );
    }
};

class TxUnaryLogicalNotNode : public TxOperatorValueNode {
protected:
    virtual const TxType* define_type() override {
        return this->types().get_builtin_type(BOOL);
    }

public:
    TxExpressionNode* operand;
    TxUnaryLogicalNotNode(const TxLocation& parseLocation, TxExpressionNode* operand)
        : TxOperatorValueNode(parseLocation), operand(operand) { }

    virtual TxUnaryLogicalNotNode* make_ast_copy() const override {
        return new TxUnaryLogicalNotNode( this->parseLocation, this->operand->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        operand->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        operand->symbol_resolution_pass();
        auto type = operand->get_type();
        // assume arithmetic, scalar negation:
        if (! dynamic_cast<const TxBoolType*>(type))
            // should we support any auto-conversion to Bool?
            CERROR(this, "Operand of unary '!' is not of Bool type: " << (type ? type->str().c_str() : "NULL"));
    }

    virtual bool is_statically_constant() const override {
        return this->operand->is_statically_constant();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->operand->visit_ast( visitor, thisAsParent, "operand", context );
    }
};



class TxFunctionCallNode : public TxExpressionNode {
    const TxFunctionType* funcType = nullptr;
    bool isSelfSuperConstructorInvocation = false;
    TxExpressionNode* inlinedExpression = nullptr;  // substitutes the function/constructor call if non-null

    static std::vector<TxMaybeConversionNode*>* make_args_vec( const std::vector<TxExpressionNode*>* argsExprList ) {
        std::vector<TxMaybeConversionNode*>* copyVec = new std::vector<TxMaybeConversionNode*>( argsExprList->size() );
        std::transform( argsExprList->cbegin(), argsExprList->cend(), copyVec->begin(),
                        []( TxExpressionNode* n ) -> TxMaybeConversionNode*  {  return new TxMaybeConversionNode( n );  } );
        return copyVec;
    }

    void prepare_self_super_invocations();

protected:
    virtual const TxType* define_type() override;

public:
    TxExpressionNode* callee;
    std::vector<TxExpressionNode*> const * const origArgsExprList;
    std::vector<TxMaybeConversionNode*>* argsExprList;

    TxFunctionCallNode(const TxLocation& parseLocation, TxExpressionNode* callee, const std::vector<TxExpressionNode*>* argsExprList)
            : TxExpressionNode(parseLocation), callee(callee), origArgsExprList(argsExprList), argsExprList( make_args_vec( argsExprList ) ) {
        this->prepare_self_super_invocations();
    }

    virtual TxFunctionCallNode* make_ast_copy() const override {
        return new TxFunctionCallNode( this->parseLocation, this->callee->make_ast_copy(), make_node_vec_copy( this->origArgsExprList ) );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        this->callee->symbol_declaration_pass( lexContext);
        for (auto argExpr : *this->argsExprList) {
            if (argExpr->is_context_set())
                break;  // can happen if wrapped, e.g. for stack construction calls
            argExpr->symbol_declaration_pass( lexContext);
        }
    }

    virtual void symbol_resolution_pass() override;

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
    llvm::Value* gen_call(LlvmGenerationContext& context, GenScope* scope, llvm::Value* functionPtrV, llvm::Value* closureRefV) const;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->callee->visit_ast( visitor, thisAsParent, "callee", context );
        for (auto arg : *this->argsExprList)
            arg->visit_ast( visitor, thisAsParent, "arg", context );
    }
};



/** Special callee expression node for calling constructors. */
class TxConstructorCalleeExprNode : public TxExpressionNode {
    const TxFieldDeclaration* declaration = nullptr;
    mutable llvm::Value* objectPtrV = nullptr;

    /** @return a function pointer (not a lambda value) */
    virtual llvm::Value* gen_func_ptr(LlvmGenerationContext& context, GenScope* scope) const;

protected:
    TxExpressionNode* objectExpr;

    virtual const TxType* define_type() override;

public:
    TxConstructorCalleeExprNode(const TxLocation& parseLocation, TxExpressionNode* objectExpr)
            : TxExpressionNode(parseLocation), objectExpr(objectExpr) { }

    virtual TxConstructorCalleeExprNode* make_ast_copy() const override {
        return new TxConstructorCalleeExprNode( this->parseLocation, this->objectExpr->make_ast_copy() );
    }


    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        this->objectExpr->symbol_declaration_pass( lexContext);
    }
    virtual void symbol_resolution_pass() override { this->objectExpr->symbol_resolution_pass(); }

    /** @return a lambda value */
    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    /** @return an object pointer (not a lambda value) */
    virtual llvm::Value* gen_obj_ptr(LlvmGenerationContext& context, GenScope* scope) const;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->objectExpr->visit_ast( visitor, thisAsParent, "objectexpr", context );
    }
};



/** Abstract superclass for memory allocation expressions, for heap and stack allocators. */
class TxMemAllocNode : public TxExpressionNode {
protected:
    TxTypeExpressionNode* objTypeExpr;

    //const TxType* get_object_type() const { return this->objTypeDefiner->get_type(); }
    virtual const TxType* define_type() override {
        return this->objTypeExpr->resolve_type();
    }

    TxMemAllocNode(const TxLocation& parseLocation, TxTypeExpressionNode* objTypeExpr)
            : TxExpressionNode(parseLocation), objTypeExpr(objTypeExpr) { }

public:
    virtual void symbol_declaration_pass( LexicalContext& lexContext) override { this->set_context( lexContext); }
    virtual void symbol_resolution_pass() override { }

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->objTypeExpr->visit_ast( visitor, thisAsParent, "type", context );
    }
};

class TxHeapAllocNode : public TxMemAllocNode {
public:
    TxHeapAllocNode(const TxLocation& parseLocation, TxTypeExpressionNode* objTypeExpr)
            : TxMemAllocNode(parseLocation, objTypeExpr) { }

    virtual TxHeapAllocNode* make_ast_copy() const override {
        return new TxHeapAllocNode( this->parseLocation, this->objTypeExpr->make_ast_copy() );
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

class TxStackAllocNode : public TxMemAllocNode {
public:
    TxStackAllocNode(const TxLocation& parseLocation, TxTypeExpressionNode* objTypeExpr)
            : TxMemAllocNode(parseLocation, objTypeExpr) { }

    virtual TxStackAllocNode* make_ast_copy() const override {
        return new TxStackAllocNode( this->parseLocation, this->objTypeExpr->make_ast_copy() );
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



/** Abstract common superclass for new expression and local init expression */
class TxMakeObjectNode : public TxExpressionNode {

protected:
    TxTypeExpressionNode* typeExpr;
    TxFunctionCallNode* constructorCall;
    TxExpressionNode* inlinedExpression = nullptr;  // substitutes the function/constructor call if non-null

    /** Gets the type of the allocated object. Should not be called before resolution. */
    virtual const TxType* get_object_type() const = 0;

    TxMakeObjectNode( const TxLocation& parseLocation, TxTypeExpressionNode* typeExpr, TxFunctionCallNode* constructorCall )
            : TxExpressionNode(parseLocation), typeExpr(typeExpr), constructorCall(constructorCall) { }

public:
    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        this->typeExpr->symbol_declaration_pass( lexContext, lexContext, nullptr );
        this->constructorCall->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->typeExpr->symbol_resolution_pass();

        this->constructorCall->symbol_resolution_pass();

        if (dynamic_cast<const TxBuiltinConversionFunctionType*>(this->constructorCall->callee->get_type())) {
            // "inline" initialization by replacing with conversion expression
            // note: conversion already inserted by function call node
            this->inlinedExpression = this->constructorCall->argsExprList->front();
        }
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->typeExpr->visit_ast( visitor, thisAsParent, "type", context );
        if (this->inlinedExpression)
            this->inlinedExpression->visit_ast( visitor, thisAsParent, "inlinedexpr", context );
        else
            this->constructorCall->visit_ast( visitor, thisAsParent, "call", context );
    }
};

/** Makes a new object in newly allocated heap memory and returns it by reference. */
class TxNewExprNode : public TxMakeObjectNode {
protected:
    virtual const TxType* get_object_type() const override { return this->typeExpr->get_type(); }

    virtual const TxType* define_type() override {
        // new constructor returns the constructed object by reference
        return this->types().get_reference_type( this, TxGenericBinding::make_type_binding("T", this->typeExpr), nullptr );
    }

public:
    TxNewExprNode(const TxLocation& parseLocation, TxTypeExpressionNode* typeExpr, std::vector<TxExpressionNode*>* argsExprList)
            : TxMakeObjectNode( parseLocation, typeExpr,
                                new TxFunctionCallNode(parseLocation,
                                                       new TxConstructorCalleeExprNode(parseLocation, new TxHeapAllocNode(parseLocation, typeExpr)),
                                                       argsExprList ) ) {
    }

    virtual TxNewExprNode* make_ast_copy() const override {
        return new TxNewExprNode( this->parseLocation, this->typeExpr->make_ast_copy(),
                                  make_node_vec_copy( this->constructorCall->origArgsExprList ) );
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};

/** Makes a new object in newly allocated stack memory and returns it by value. */
class TxStackConstructorNode : public TxMakeObjectNode {
protected:
    virtual const TxType* get_object_type() const override { return this->get_type(); }

    virtual const TxType* define_type() override {
        // stack constructor returns the constructed object by value, not by reference
        return this->typeExpr->resolve_type();
    }

public:
    /** produced by the expression syntax: <...type-expr...>(...constructor-args...) */
    TxStackConstructorNode(const TxLocation& parseLocation, TxTypeExpressionNode* typeExpr,
                           const std::vector<TxExpressionNode*>* argsExprList)
            : TxMakeObjectNode(parseLocation, typeExpr,
                    new TxFunctionCallNode(parseLocation,
                                           new TxConstructorCalleeExprNode(parseLocation, new TxStackAllocNode(parseLocation, typeExpr)),
                                           argsExprList ) ) {
    }

    TxStackConstructorNode( TxFunctionCallNode* originalCall, TxTypeDeclaration* typeDecl )
            : TxStackConstructorNode( originalCall->parseLocation, new TxTypeDeclWrapperNode( originalCall->parseLocation, typeDecl ),
                                      originalCall->origArgsExprList ) {
    }

    virtual TxStackConstructorNode* make_ast_copy() const override {
        return new TxStackConstructorNode( this->parseLocation, this->typeExpr->make_ast_copy(),
                                           make_node_vec_copy( this->constructorCall->origArgsExprList ) );
    }

    virtual bool is_stack_allocation_expression() const override {
        return !this->inlinedExpression;  // performs stack allocation unless this is an inlined value expression
    }

    // virtual bool is_statically_constant() const override { return true; }  // TODO: review

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;
};



/*=== assignee expressions ===*/

class TxFieldAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type() override {
        return this->field->resolve_type();
    }

public:
    TxFieldValueNode* field;
    TxFieldAssigneeNode(const TxLocation& parseLocation, TxFieldValueNode* field)
        : TxAssigneeNode(parseLocation), field(field) { }

    virtual TxFieldAssigneeNode* make_ast_copy() const override {
        return new TxFieldAssigneeNode( this->parseLocation, this->field->make_ast_copy() );
    }

    virtual std::string get_declared_name() const override {
        return field->get_declared_name();
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        field->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxAssigneeNode::symbol_resolution_pass();
        field->symbol_resolution_pass();

        auto fieldDecl = field->get_field_declaration();
        if (fieldDecl && fieldDecl->get_storage() == TXS_NOSTORAGE)
            CERROR(this, "Assignee '" << field->symbolName << "' is not an L-value / has no storage.");
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->field->visit_ast( visitor, thisAsParent, "field", context );
    }
};

class TxDerefAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type() override {
        auto opType = this->operand->resolve_type();
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
    TxDerefAssigneeNode(const TxLocation& parseLocation, TxExpressionNode* operand)
        : TxAssigneeNode(parseLocation), operand(operand) { }

    virtual TxDerefAssigneeNode* make_ast_copy() const override {
        return new TxDerefAssigneeNode( this->parseLocation, this->operand->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        operand->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxAssigneeNode::symbol_resolution_pass();
        operand->symbol_resolution_pass();

        if (! dynamic_cast<const TxReferenceType*>(this->operand->get_type()))
            CERROR(this, "Can't de-reference non-reference expression.");
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->operand->visit_ast( visitor, thisAsParent, "operand", context );
    }
};

class TxElemAssigneeNode : public TxAssigneeNode {
protected:
    virtual const TxType* define_type() override {
        auto opType = this->array->resolve_type();
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
    TxMaybeConversionNode* subscript;

    TxElemAssigneeNode(const TxLocation& parseLocation, TxExpressionNode* array, TxExpressionNode* subscript)
        : TxAssigneeNode(parseLocation), array(array), subscript(new TxMaybeConversionNode(subscript))  { }

    virtual TxElemAssigneeNode* make_ast_copy() const override {
        return new TxElemAssigneeNode( this->parseLocation, this->array->make_ast_copy(), this->subscript->originalExpr->make_ast_copy() );
    }

    virtual void symbol_declaration_pass( LexicalContext& lexContext) override {
        this->set_context( lexContext);
        array->symbol_declaration_pass( lexContext);
        subscript->symbol_declaration_pass( lexContext);
    }

    virtual void symbol_resolution_pass() override {
        TxAssigneeNode::symbol_resolution_pass();
        array->symbol_resolution_pass();
        this->subscript->insert_conversion( this->types().get_builtin_type(LONG) );
        subscript->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen(LlvmGenerationContext& context, GenScope* scope) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstParent& thisAsParent, const std::string& role, void* context ) const override {
        this->array->visit_ast( visitor, thisAsParent, "array", context );
        this->subscript->visit_ast( visitor, thisAsParent, "subscript", context );
    }
};
