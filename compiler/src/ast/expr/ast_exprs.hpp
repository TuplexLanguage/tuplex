#pragma once

#include "ast_expr_node.hpp"
#include "ast_maybe_conv_node.hpp"
#include "ast/ast_entitydecls.hpp"
#include "ast/ast_wrappers.hpp"
#include "ast/ast_declpass.hpp"
#include "ast/type/ast_typearg_node.hpp"
#include "ast/type/ast_types.hpp"

namespace llvm {
    class FunctionType;
}


/** Generates code for a call to a lambda.
 * Note, the passed args vector shall contain only the user-passed args (not the closure).
 */
llvm::Value* gen_lambda_call( LlvmGenerationContext& context, GenScope* scope, llvm::FunctionType *fnTy, llvm::Value* lambdaV,
                              std::vector<llvm::Value*>& passedArgs, const std::string& exprLabel, bool doesNotReturn );


class TxFunctionCallNode : public TxExpressionNode {
    bool doesNotReturn;
    const TxActualType* calleeType = nullptr;
    TxExpressionNode* inlinedExpression = nullptr;  // substitutes the function/constructor call if non-null
    std::vector<TxMaybeConversionNode*>* varargsList = nullptr;  // since FilledArrayNode isn't currently able to "own" them

    /** Wraps the expression nodes with TxMaybeConversionNode. */
    static std::vector<TxMaybeConversionNode*>* make_args_vec( const std::vector<TxExpressionNode*>* argsExprList ) {
        auto* copyVec = new std::vector<TxMaybeConversionNode*>( argsExprList->size() );
        std::transform( argsExprList->cbegin(), argsExprList->cend(), copyVec->begin(),
                        []( TxExpressionNode* n ) -> TxMaybeConversionNode* {return new TxMaybeConversionNode( n );} );
        return copyVec;
    }

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

public:
    TxExpressionNode* callee;
    std::vector<TxExpressionNode*> const * const origArgsExprList;
    std::vector<TxMaybeConversionNode*>* argsExprList;

    TxFunctionCallNode( const TxLocation& ploc, TxExpressionNode* callee, const std::vector<TxExpressionNode*>* argsExprList,
                        bool doesNotReturn = false );

    TxFunctionCallNode* make_ast_copy() const override {
        return new TxFunctionCallNode( this->ploc, this->callee->make_ast_copy(), make_node_vec_copy( this->origArgsExprList ) );
    }

    bool is_inlined() const {
        return this->inlinedExpression;
    }

    TxFieldStorage get_storage() const override {
        if ( this->inlinedExpression )
            return this->inlinedExpression->get_storage();
        return TXS_NOSTORAGE;
    }

    bool is_statically_constant() const override {
        if ( this->inlinedExpression )
            return this->inlinedExpression->is_statically_constant();
        return false;
    }

    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override;
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;
    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        if ( this->inlinedExpression )
            this->inlinedExpression->visit_ast( visitor, cursor, "inlinedexpr", aux );
        else {
            this->callee->visit_ast( visitor, cursor, "callee", aux );
            for ( auto arg : *this->argsExprList )
                arg->visit_ast( visitor, cursor, "arg", aux );
            if ( this->varargsList )
                for ( auto arg : *this->varargsList )
                    arg->visit_ast( visitor, cursor, "vararg", aux );
        }
    }
};


/** Requires / ensures the contained expression to produce a value of modifiable type. */
class TxModifiableValueNode : public TxExpressionNode {
    TxGenSpecTypeNode* mutTypeDefNode = nullptr;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

    void verification_pass() const override;

public:
    TxExpressionNode* exprNode;

    TxModifiableValueNode( const TxLocation& ploc, TxExpressionNode* exprNode )
            : TxExpressionNode( ploc ), exprNode( exprNode ) {
    }

    TxModifiableValueNode* make_ast_copy() const override {
        return new TxModifiableValueNode( this->ploc, exprNode->make_ast_copy() );
    }

    const std::vector<TxExpressionNode*>* get_applied_func_args() const override {
        return this->exprNode->get_applied_func_args();
    }
    void set_applied_func_args( const std::vector<TxExpressionNode*>* appliedTypeParameters ) override {
        this->exprNode->set_applied_func_args( appliedTypeParameters );
    }

    const TxActualType* get_constructed_type( TxTypeResLevel typeResLevel ) const override {
        return this->exprNode->get_constructed_type( typeResLevel );
    }


    bool is_value() const override {
        return this->exprNode->is_value();
    }
    const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->exprNode->get_data_graph_origin_expr();
    }
    TxFieldStorage get_storage() const override {
        return this->exprNode->get_storage();
    }
    bool is_statically_constant() const override {
        return this->exprNode->is_statically_constant();
    }

    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override {
        return this->exprNode->code_gen_const_value( context );
    }
    llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override {
        return this->exprNode->code_gen_const_address( context );
    }
    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->exprNode->code_gen_dyn_address( context, scope );
    }
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->exprNode->code_gen_dyn_value( context, scope );
    }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->exprNode->visit_ast( visitor, cursor, "expr", aux );
        if ( this->mutTypeDefNode )
            this->mutTypeDefNode->visit_ast( visitor, cursor, "mut-type", aux );
    }

    const std::string& get_descriptor() const override {
        return this->exprNode->get_descriptor();
    }
};


/** Abstract superclass for memory providing expressions, used in conjunction with object construction / initialization. */
class TxMemProviderNode : public TxExpressionNode {
protected:
    explicit TxMemProviderNode( const TxLocation& ploc ) : TxExpressionNode( ploc )  { }

public:
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override {
        THROW_LOGIC( "Unsupported: code_gen() for node type " << this );
    }

    TxMemProviderNode* make_ast_copy() const override = 0;

    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override = 0;
};


/** Special callee expression node for calling constructors. */
class TxConstructorCalleeExprNode : public TxExpressionNode {
    const TxFieldDeclaration* declaration = nullptr;
    mutable llvm::Value* objectPtrV = nullptr;

    /** @return a function pointer (not a lambda value) */
    virtual llvm::Value* gen_func_ptr( LlvmGenerationContext& context, GenScope* scope ) const;

protected:
    /** Produces the object - either an allocation, or a self/super reference */
    TxMemProviderNode* objectExpr;

    TxQualType define_type( TxTypeResLevel typeResLevel ) override;

public:
    TxConstructorCalleeExprNode( const TxLocation& ploc, TxMemProviderNode* objectExpr )
            : TxExpressionNode( ploc ), objectExpr( objectExpr ) {
    }

    TxConstructorCalleeExprNode* make_ast_copy() const override {
        return new TxConstructorCalleeExprNode( this->ploc, this->objectExpr->make_ast_copy() );
    }

    const TxActualType* get_constructed_type( TxTypeResLevel typeResLevel ) const override {
        return this->objectExpr->resolve_type( typeResLevel ).type();
    }

    /** @return a lambda value */
    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    /** @return an object pointer (not a lambda value) */
    virtual llvm::Value* gen_obj_ptr( LlvmGenerationContext& context, GenScope* scope ) const;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->objectExpr->visit_ast( visitor, cursor, "objectexpr", aux );
    }
};


/** Does not allocate memory, instead passes memory previously allocated */
class TxInPlaceAllocNode : public TxMemProviderNode {
    TxExpressionNode* objExpr;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return this->objExpr->resolve_type( typeResLevel );
    }

public:
    TxInPlaceAllocNode( const TxLocation& ploc, TxExpressionNode* objExpr )
            : TxMemProviderNode( ploc ), objExpr( objExpr ) {
    }

    TxInPlaceAllocNode* make_ast_copy() const override {
        return new TxInPlaceAllocNode( this->ploc, this->objExpr->make_ast_copy() );
    }

    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->objExpr->visit_ast( visitor, cursor, "obj", aux );
    }
};


/** Abstract superclass for memory allocation expressions, for heap and stack allocators. */
class TxMemAllocNode : public TxMemProviderNode {
protected:
    TxTypeExpressionNode* objTypeExpr;

    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return this->objTypeExpr->resolve_type( typeResLevel );
    }

    TxMemAllocNode( const TxLocation& ploc, TxTypeExpressionNode* objTypeExpr )
            : TxMemProviderNode( ploc ), objTypeExpr( objTypeExpr ) {
    }

public:
    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->objTypeExpr->visit_ast( visitor, cursor, "type", aux );
    }
};

class TxHeapAllocNode : public TxMemAllocNode {
public:
    TxHeapAllocNode( const TxLocation& ploc, TxTypeExpressionNode* objTypeExpr )
            : TxMemAllocNode( ploc, objTypeExpr ) {
    }

    TxHeapAllocNode* make_ast_copy() const override {
        return new TxHeapAllocNode( this->ploc, this->objTypeExpr->make_ast_copy() );
    }

    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
};

class TxStackAllocNode : public TxMemAllocNode {
public:
    TxStackAllocNode( const TxLocation& ploc, TxTypeExpressionNode* objTypeExpr )
            : TxMemAllocNode( ploc, objTypeExpr ) {
    }

    TxStackAllocNode* make_ast_copy() const override {
        return new TxStackAllocNode( this->ploc, this->objTypeExpr->make_ast_copy() );
    }

    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
};


/** Abstract common superclass for new expression and local init expression */
class TxMakeObjectNode : public TxExpressionNode {
    friend class TxERangeLitNode;
protected:
    /** the type of the object to make/allocate */
    TxQualTypeExprNode* typeExpr;
    TxFunctionCallNode* constructorCall;
    TxExpressionNode* initializationExpression = nullptr;  // substitutes the function/constructor call if non-null

    /** Gets the type of the allocated object. Should not be called before resolution. */
    TxQualType get_object_type() const {
        return this->typeExpr->qtype();
    }

    TxMakeObjectNode( const TxLocation& ploc, TxQualTypeExprNode* typeExpr, TxFunctionCallNode* constructorCall )
            : TxExpressionNode( ploc ), typeExpr( typeExpr ), constructorCall( constructorCall ) {
    }

    void resolution_pass() override {
        TxExpressionNode::resolution_pass();
        if ( auto calleeType = this->constructorCall->callee->resolve_type( TXR_FULL_RESOLUTION ) ) {
            if ( auto inlineCalleeType = dynamic_cast<const TxInlineFunctionType*>( calleeType.type() ) ) {
                // This constructor is an inlineable function that returns the initializer value
                // (as opposed to a constructor whose code assigns value to the object's members).
                // We replace the constructor call with the initialization expression:
                this->initializationExpression = inlineCalleeType->make_inline_expr( this->constructorCall->callee,
                                                                                     this->constructorCall->argsExprList );
                inserted_node( this->initializationExpression, this, "inlinedconstr" );
            }
        }
    }

    void verification_pass() const override {
        if ( auto qtype = this->typeExpr->attempt_qtype() ) {
            if ( is_not_properly_concrete( this, qtype ) )
                CERROR( this->typeExpr, "Object to allocate is not concrete: " << qtype );
        }
    }

public:
    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->typeExpr->visit_ast( visitor, cursor, "type", aux );
        if ( this->initializationExpression )
            this->initializationExpression->visit_ast( visitor, cursor, "initexpr", aux );
        else
            this->constructorCall->visit_ast( visitor, cursor, "call", aux );
    }
};

/** Makes a new object in newly allocated heap memory and returns it by reference. */
class TxNewConstructionNode : public TxMakeObjectNode {
    TxTypeExpressionNode* resultTypeNode;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        // new constructor returns the constructed object by reference
        return this->resultTypeNode->resolve_type( typeResLevel );
    }

public:
    TxNewConstructionNode( const TxLocation& ploc, TxQualTypeExprNode* typeExpr, std::vector<TxExpressionNode*>* argsExprList )
            : TxMakeObjectNode( ploc, typeExpr,
                                new TxFunctionCallNode(
                                        ploc,
                                        new TxConstructorCalleeExprNode(
                                                ploc, new TxHeapAllocNode( ploc, new TxTypeExprWrapperNode( typeExpr->get_type_expr()))),
                                        argsExprList )),
              resultTypeNode( new TxReferenceTypeNode( ploc, nullptr, this->typeExpr )) {
    }

    TxNewConstructionNode* make_ast_copy() const override {
        return new TxNewConstructionNode( this->ploc, this->typeExpr->make_ast_copy(),
                                          make_node_vec_copy( this->constructorCall->origArgsExprList ) );
    }

    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->resultTypeNode->visit_ast( visitor, cursor, "ref-type", aux );
        if ( this->initializationExpression )
            this->initializationExpression->visit_ast( visitor, cursor, "initexpr", aux );
        else
            this->constructorCall->visit_ast( visitor, cursor, "call", aux );
    }
};

/** Makes a new object in newly allocated stack memory and returns it by value/address. */
class TxStackConstructionNode : public TxMakeObjectNode {
protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        // stack constructor returns the constructed object by value, not by reference
        return this->typeExpr->resolve_type( typeResLevel );
    }

public:
    /** produced by the expression syntax: <...type-expr...>(...constructor-args...) */
    TxStackConstructionNode( const TxLocation& ploc, TxQualTypeExprNode* typeExpr,
                             const std::vector<TxExpressionNode*>* argsExprList )
            : TxMakeObjectNode( ploc, typeExpr,
                                new TxFunctionCallNode(
                                        ploc,
                                        new TxConstructorCalleeExprNode(
                                                ploc, new TxStackAllocNode( ploc, new TxTypeExprWrapperNode( typeExpr->get_type_expr() ) ) ),
                                        argsExprList ) ) {
    }

    TxStackConstructionNode* make_ast_copy() const override {
        return new TxStackConstructionNode( this->ploc, this->typeExpr->make_ast_copy(),
                                            make_node_vec_copy( this->constructorCall->origArgsExprList ) );
    }

    TxFieldStorage get_storage() const override {
        // performs stack allocation unless this is an inlined value expression
        return ( this->initializationExpression ? TXS_NOSTORAGE : TXS_UNBOUND_STACK );
    }

    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override;

    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override;
};
