#pragma once

#include "ast_declbase.hpp"

#include "symbol/type_registry.hpp"

/** Wraps a TxExpressionNode. The declaration and resolution pass calls won't be forwarded,
 * allowing the wrapped node to be added as a TxExpressionNode child to additional parent nodes.
 * Code generation however is performed on the wrapped node.
 */
class TxExprWrapperNode : public TxExpressionNode {
protected:
    virtual const TxType* define_type() override {
        return this->exprNode->resolve_type();
    }

public:
    TxExpressionNode* const exprNode;

    TxExprWrapperNode( TxExpressionNode* exprNode )
            : TxExpressionNode( exprNode->parseLocation ), exprNode( exprNode ) {
    }

    virtual TxExprWrapperNode* make_ast_copy() const override {
        // since declaration and resolution passes aren't forwarded, the wrapped type definition doesn't need copying
        return new TxExprWrapperNode( this->exprNode );
    }

    virtual bool is_stack_allocation_expression() const {
        return this->exprNode->is_stack_allocation_expression();
    }

    virtual bool is_statically_constant() const override {
        return this->exprNode->is_statically_constant();
    }

    virtual const TxConstantProxy* get_static_constant_proxy() const {
        return this->exprNode->get_static_constant_proxy();
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->exprNode->code_gen( context, scope );
    }

    virtual llvm::Value* code_gen_address( LlvmGenerationContext& context, GenScope* scope ) const {
        return this->exprNode->code_gen_address( context, scope );
    }

    virtual llvm::Value* code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const {
        return this->exprNode->code_gen_typeid( context, scope );
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        // Traversal does not proceed to the wrapped node from here since it is visited via its original AST location.
    }
};

/** Wraps a TxTypeDefiningNode as an TxTypeExpressionNode.
 * The declaration and resolution pass calls won't be forwarded,
 * allowing the wrapped node to be added as a TxTypeExpressionNode child to additional parent nodes.
 * Code generation however is performed on the wrapped node.
 */
class TxTypeExprWrapperNode : public TxTypeExpressionNode {
protected:
    virtual const TxType* define_type() override {
        return this->typeDefNode->resolve_type();
    }

public:
    TxTypeDefiningNode* const typeDefNode;

    TxTypeExprWrapperNode( TxTypeDefiningNode* typeExprNode )
            : TxTypeExpressionNode( typeExprNode->parseLocation ), typeDefNode( typeExprNode ) {
    }

    virtual TxTypeExprWrapperNode* make_ast_copy() const override {
        // since declaration and resolution passes aren't forwarded, the wrapped type definition doesn't need copying
        return new TxTypeExprWrapperNode( this->typeDefNode );
    }

    virtual std::string get_auto_type_name() const override {
        if ( auto typeExpr = dynamic_cast<TxTypeExpressionNode*>( this->typeDefNode ) )
            return typeExpr->get_auto_type_name();
        else {
            LOG( this->LOGGER(), WARN, "couldn't determine an auto-name for node: " << this->typeDefNode );
            return "?";
        }
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->typeDefNode->code_gen( context, scope );
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        // Traversal does not proceed to the wrapped node from here since it is visited via its original AST location.
    }
};

/** Helper function that wraps a vector of TxExpressionNode. The returned vector contains TxExprWrapperNode instances. */
template<class N>
std::vector<TxExpressionNode*>* make_expr_wrapper_vec( const std::vector<N*>* nodeVec ) {
    if ( !nodeVec )
        return nullptr;
    std::vector<TxExpressionNode*>* wrapperVec = new std::vector<TxExpressionNode*>( nodeVec->size() );
    std::transform( nodeVec->cbegin(), nodeVec->cend(), wrapperVec->begin(),
                    []( N* n ) -> TxExpressionNode* {return new TxExprWrapperNode ( n );} );
    return wrapperVec;
}

/** Helper function that wraps a vector of TxTypeDefiningNode. The returned vector contains TxTypeExprWrapperNode instances. */
template<class N>
std::vector<TxTypeExpressionNode*>* make_type_wrapper_vec( const std::vector<N*>* nodeVec ) {
    if ( !nodeVec )
        return nullptr;
    std::vector<TxTypeExpressionNode*>* wrapperVec = new std::vector<TxTypeExpressionNode*>( nodeVec->size() );
    std::transform( nodeVec->cbegin(), nodeVec->cend(), wrapperVec->begin(),
                    []( N* n ) -> TxTypeExpressionNode* {return new TxTypeExprWrapperNode ( n );} );
    return wrapperVec;
}

/** Wraps a TxEntityDeclaration as a TxTypeExpressionNode.
 * If this wrapper is used to declare a type name, that name will effectively be a type alias. */
class TxTypeDeclWrapperNode : public TxTypeExpressionNode {
    TxEntityDeclaration const * const typeDecl;

protected:
    virtual const TxType* define_type() override {
        return this->typeDecl->get_definer()->resolve_type();
    }

public:
    TxTypeDeclWrapperNode( const TxLocation& parseLocation, const TxEntityDeclaration* typeDecl )
            : TxTypeExpressionNode( parseLocation ), typeDecl( typeDecl ) {
    }

    virtual TxTypeDeclWrapperNode* make_ast_copy() const override {
        return new TxTypeDeclWrapperNode( this->parseLocation, this->typeDecl );
    }

    virtual std::string get_auto_type_name() const override {
        return this->typeDecl->get_unique_full_name();
    }

    const TxEntityDeclaration* get_wrapped_declaration() const {
        return this->typeDecl;
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override {
        return nullptr;
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
    }
};

/** An internal AST root node for processing a compiler-generated node. */
class TxInternalRootNode : public TxNode {
    TxNode* innerNode;

public:
    TxInternalRootNode( const TxLocation& parseLocation, TxNode* node, const LexicalContext& context )
            : TxNode( parseLocation ), innerNode( node ) {
        this->lexContext = context;
    }

    virtual TxNode* make_ast_copy() const override {
        ASSERT( false, "Can't make AST copy of " << this ); return nullptr;
    }

    virtual void symbol_resolution_pass() override {
        this->innerNode->symbol_resolution_pass();
    }

    virtual llvm::Value* code_gen( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->innerNode->code_gen( context, scope );
    }

    virtual void visit_descendants( AstVisitor visitor, const AstCursor& thisCursor, const std::string& role, void* context ) override {
        this->innerNode->visit_ast( visitor, thisCursor, "node", context );
    }
};
