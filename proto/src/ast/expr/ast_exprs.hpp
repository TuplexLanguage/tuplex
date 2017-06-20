#pragma once

#include "../ast_entitydecls.hpp"
#include "ast_expr_node.hpp"
#include "ast_maybe_conv_node.hpp"

#include "ast/ast_wrappers.hpp"
#include "ast/type/ast_typearg_node.hpp"


class TxFunctionCallNode : public TxExpressionNode {
    bool doesNotReturn;
    const TxType* calleeType = nullptr;
    bool isSelfSuperConstructorInvocation = false;
    TxExpressionNode* inlinedExpression = nullptr;  // substitutes the function/constructor call if non-null

    static std::vector<TxMaybeConversionNode*>* make_args_vec( const std::vector<TxExpressionNode*>* argsExprList ) {
        std::vector<TxMaybeConversionNode*>* copyVec = new std::vector<TxMaybeConversionNode*>( argsExprList->size() );
        std::transform( argsExprList->cbegin(), argsExprList->cend(), copyVec->begin(),
                        []( TxExpressionNode* n ) -> TxMaybeConversionNode* {return new TxMaybeConversionNode( n );} );
        return copyVec;
    }

protected:
    virtual void declaration_pass() override;

    virtual const TxType* define_type() override;

public:
    TxExpressionNode* callee;
    std::vector<TxExpressionNode*> const * const origArgsExprList;
    std::vector<TxMaybeConversionNode*>* argsExprList;

    TxFunctionCallNode( const TxLocation& ploc, TxExpressionNode* callee, const std::vector<TxExpressionNode*>* argsExprList,
                        bool doesNotReturn = false );

    virtual TxFunctionCallNode* make_ast_copy() const override {
        return new TxFunctionCallNode( this->ploc, this->callee->make_ast_copy(), make_node_vec_copy( this->origArgsExprList ) );
    }

    virtual void symbol_resolution_pass() override;

    virtual bool is_stack_allocation_expression() const override {
        if ( this->inlinedExpression )
            return this->inlinedExpression->is_stack_allocation_expression();
        return false;
    }

    virtual TxFieldStorage get_storage() const override {
        if ( this->inlinedExpression )
            return this->inlinedExpression->get_storage();
        return TXS_NOSTORAGE;
    }

    virtual bool is_statically_constant() const override {
        if ( this->inlinedExpression )
            return this->inlinedExpression->is_statically_constant();
        return false;
    }

    virtual llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        if ( this->inlinedExpression )
            this->inlinedExpression->visit_ast( visitor, thisCursor, "inlinedexpr", context );
        else {
            this->callee->visit_ast( visitor, thisCursor, "callee", context );
            for ( auto arg : *this->argsExprList )
                arg->visit_ast( visitor, thisCursor, "arg", context );
        }
    }
};

/** Special callee expression node for calling constructors. */
class TxConstructorCalleeExprNode : public TxExpressionNode {
    const TxFieldDeclaration* declaration = nullptr;
    mutable llvm::Value* objectPtrV = nullptr;

    /** @return a function pointer (not a lambda value) */
    virtual llvm::Value* gen_func_ptr( LlvmGenerationContext& context, GenScope* scope ) const;

protected:
    /** Produces the object - either an allocation, or a self/super reference */
    TxExpressionNode* objectExpr;

    virtual const TxType* define_type() override;

public:
    TxConstructorCalleeExprNode( const TxLocation& ploc, TxExpressionNode* objectExpr )
            : TxExpressionNode( ploc ), objectExpr( objectExpr ) {
    }

    virtual TxConstructorCalleeExprNode* make_ast_copy() const override {
        return new TxConstructorCalleeExprNode( this->ploc, this->objectExpr->make_ast_copy() );
    }

    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->objectExpr->symbol_resolution_pass();
    }

    /** Returns the constructed type. */
    inline const TxType* get_constructed_type() const {
        return this->objectExpr->resolve_type();
    }

    /** @return a lambda value */
    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    /** @return an object pointer (not a lambda value) */
    virtual llvm::Value* gen_obj_ptr( LlvmGenerationContext& context, GenScope* scope ) const;

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->objectExpr->visit_ast( visitor, thisCursor, "objectexpr", context );
    }
};

/** Abstract superclass for memory allocation expressions, for heap and stack allocators. */
class TxMemAllocNode : public TxExpressionNode {
protected:
    TxTypeExpressionNode* objTypeExpr;

    virtual const TxType* define_type() override {
        return this->objTypeExpr->resolve_type();
    }

    TxMemAllocNode( const TxLocation& ploc, TxTypeExpressionNode* objTypeExpr )
            : TxExpressionNode( ploc ), objTypeExpr( objTypeExpr ) {
    }

public:
    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->objTypeExpr->symbol_resolution_pass();
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->objTypeExpr->visit_ast( visitor, thisCursor, "type", context );
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override {
        THROW_LOGIC( "Unsupported: code_gen() for node type " << this );
    }

    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override = 0;
};

class TxHeapAllocNode : public TxMemAllocNode {
public:
    TxHeapAllocNode( const TxLocation& ploc, TxTypeExpressionNode* objTypeExpr )
            : TxMemAllocNode( ploc, objTypeExpr ) {
    }

    virtual TxHeapAllocNode* make_ast_copy() const override {
        return new TxHeapAllocNode( this->ploc, this->objTypeExpr->make_ast_copy() );
    }

    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
};

class TxStackAllocNode : public TxMemAllocNode {
public:
    TxStackAllocNode( const TxLocation& ploc, TxTypeExpressionNode* objTypeExpr )
            : TxMemAllocNode( ploc, objTypeExpr ) {
    }

    virtual TxStackAllocNode* make_ast_copy() const override {
        return new TxStackAllocNode( this->ploc, this->objTypeExpr->make_ast_copy() );
    }

    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
};

/** Abstract common superclass for new expression and local init expression */
class TxMakeObjectNode : public TxExpressionNode {
    friend class TxERangeLitNode;
protected:
    /** the type of the object to make/allocate */
    TxTypeExpressionNode* typeExpr;
    TxFunctionCallNode* constructorCall;
    TxExpressionNode* initializationExpression = nullptr;  // substitutes the function/constructor call if non-null

    /** Gets the type of the allocated object. Should not be called before resolution. */
    virtual const TxType* get_object_type() const = 0;

    TxMakeObjectNode( const TxLocation& ploc, TxTypeExpressionNode* typeExpr, TxFunctionCallNode* constructorCall )
            : TxExpressionNode( ploc ), typeExpr( typeExpr ), constructorCall( constructorCall ) {
    }

public:
    virtual void symbol_resolution_pass() override {
        TxExpressionNode::symbol_resolution_pass();
        this->typeExpr->symbol_resolution_pass();
        if ( !this->typeExpr->get_type()->is_concrete() ) {
            if ( !this->context().is_generic() )
                CERROR( this->typeExpr, "Object to allocate is not concrete: " << this->typeExpr->get_type() );
            else
                LOG_DEBUG( this->LOGGER(), "(Not error since generic context) Object to allocate is not concrete: "
                           << this->typeExpr->get_type() );
        }

        this->constructorCall->symbol_resolution_pass();

        if ( auto calleeType = this->constructorCall->callee->get_type() ) {
            if ( auto inlineCalleeType = dynamic_cast<const TxInlineFunctionType*>( calleeType->type() ) ) {
                // This constructor is an inlineable function that returns the initializer value
                // (as opposed to a constructor whose code assigns value to the object's members).
                // We replace the constructor call with the initialization expression:
                this->initializationExpression = inlineCalleeType->make_inline_expr( this->constructorCall->callee,
                                                                                     this->constructorCall->argsExprList );
            }
        }
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->typeExpr->visit_ast( visitor, thisCursor, "type", context );
        if ( this->initializationExpression )
            this->initializationExpression->visit_ast( visitor, thisCursor, "initexpr", context );
        else
            this->constructorCall->visit_ast( visitor, thisCursor, "call", context );
    }
};

/** Makes a new object in newly allocated heap memory and returns it by reference. */
class TxNewConstructionNode : public TxMakeObjectNode {
    TxTypeTypeArgumentNode* targetTypeNode;

protected:
    virtual void declaration_pass() override {
        targetTypeNode->node_declaration_pass( this );  // special case instead of wrapping typeExpr and overriding visit_descendants()
    }

    virtual const TxType* get_object_type() const override {
        return this->typeExpr->get_type();
    }

    virtual const TxType* define_type() override {
        // new constructor returns the constructed object by reference
        return this->registry().get_reference_type( this, this->targetTypeNode, nullptr );
    }

public:
    TxNewConstructionNode( const TxLocation& ploc, TxTypeExpressionNode* typeExpr, std::vector<TxExpressionNode*>* argsExprList )
            : TxMakeObjectNode( ploc, typeExpr,
                                new TxFunctionCallNode(
                                        ploc,
                                        new TxConstructorCalleeExprNode(
                                                ploc, new TxHeapAllocNode( ploc, new TxTypeExprWrapperNode( typeExpr ) ) ),
                                        argsExprList ) ) {
        targetTypeNode = new TxTypeTypeArgumentNode( this->typeExpr );
    }

    virtual TxNewConstructionNode* make_ast_copy() const override {
        return new TxNewConstructionNode( this->ploc, this->typeExpr->make_ast_copy(),
                                          make_node_vec_copy( this->constructorCall->origArgsExprList ) );
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
};

/** Makes a new object in newly allocated stack memory and returns it by value/address. */
class TxStackConstructionNode : public TxMakeObjectNode {
protected:
    virtual const TxType* get_object_type() const override {
        return this->get_type();
    }

    virtual const TxType* define_type() override {
        // stack constructor returns the constructed object by value, not by reference
        return this->typeExpr->resolve_type();
    }

public:
    /** produced by the expression syntax: <...type-expr...>(...constructor-args...) */
    TxStackConstructionNode( const TxLocation& ploc, TxTypeExpressionNode* typeExpr,
                             const std::vector<TxExpressionNode*>* argsExprList )
            : TxMakeObjectNode( ploc, typeExpr,
                                new TxFunctionCallNode(
                                        ploc,
                                        new TxConstructorCalleeExprNode(
                                                ploc, new TxStackAllocNode( ploc, new TxTypeExprWrapperNode( typeExpr ) ) ),
                                        argsExprList ) ) {
    }

    virtual TxStackConstructionNode* make_ast_copy() const override {
        return new TxStackConstructionNode( this->ploc, this->typeExpr->make_ast_copy(),
                                            make_node_vec_copy( this->constructorCall->origArgsExprList ) );
    }

    virtual bool is_stack_allocation_expression() const override {
        return !this->initializationExpression;  // performs stack allocation unless this is an inlined value expression
    }

    virtual llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    virtual llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
};
