#pragma once

#include "ast_entitydecls.hpp"

/** Wraps a TxExpressionNode. The declaration and resolution pass calls won't be forwarded,
 * allowing the wrapped node to be added as a TxExpressionNode child to additional parent nodes.
 * Code generation however is performed on the wrapped node.
 */
class TxExprWrapperNode : public TxExpressionNode {
protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return this->exprNode->resolve_type( typeResLevel );
    }

public:
    TxExpressionNode* const exprNode;

    explicit TxExprWrapperNode( TxExpressionNode* exprNode )
            : TxExpressionNode( exprNode->ploc ), exprNode( exprNode ) {
    }

    TxExprWrapperNode* make_ast_copy() const override {
        // since declaration and resolution passes aren't forwarded, the wrapped type definition doesn't need copying
        return new TxExprWrapperNode( this->exprNode );
    }

    const TxExpressionNode* get_data_graph_origin_expr() const override {
        return this->exprNode;
    }

    TxFieldStorage get_storage() const override {
        return this->exprNode->get_storage();
    }

    bool is_statically_constant() const override {
        return this->exprNode->is_statically_constant();
    }

    llvm::Value* code_gen_dyn_value( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->exprNode->code_gen_dyn_value( context, scope );
    }

    llvm::Constant* code_gen_const_value( LlvmGenerationContext& context ) const override {
        return this->exprNode->code_gen_const_value( context );
    }

    llvm::Value* code_gen_dyn_address( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->exprNode->code_gen_dyn_address( context, scope );
    }

    llvm::Constant* code_gen_const_address( LlvmGenerationContext& context ) const override {
        return this->exprNode->code_gen_const_address( context );
    }

    llvm::Value* code_gen_typeid( LlvmGenerationContext& context, GenScope* scope ) const override {
        return this->exprNode->code_gen_typeid( context, scope );
    }

    llvm::Constant* code_gen_typeid( LlvmGenerationContext& context ) const override {
        return this->exprNode->code_gen_typeid( context );
    }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        // Traversal does not proceed to the wrapped node from here since it is visited via its original AST location.
    }
};

/** Wraps a TxTypeResolvingNode as an TxTypeExpressionNode.
 * The declaration and resolution pass calls won't be forwarded,
 * allowing the wrapped node to be added as a TxTypeExpressionNode child to additional parent nodes.
 */
class TxTypeExprWrapperNode : public TxTypeExpressionNode {
protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return this->typeResNode->resolve_type( typeResLevel );
    }

public:
    TxEntityResolvingNode* const typeResNode;

    explicit TxTypeExprWrapperNode( TxEntityResolvingNode* typeExprNode )
            : TxTypeExpressionNode( typeExprNode->ploc ), typeResNode( typeExprNode ) {
    }

    TxTypeExprWrapperNode* make_ast_copy() const override {
        // since declaration and resolution passes aren't forwarded, the wrapped type definition doesn't need copying
        return new TxTypeExprWrapperNode( this->typeResNode );
    }

    void code_gen_type( LlvmGenerationContext& context ) const override { }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        // Traversal does not proceed to the wrapped node from here since it is visited via its original AST location.
    }
};

/** Helper function that wraps a vector of TxExpressionNode. The returned vector contains TxExprWrapperNode instances. */
template<class N>
std::vector<TxExpressionNode*>* make_expr_wrapper_vec( const std::vector<N*>* nodeVec ) {
    if ( !nodeVec )
        return nullptr;
    auto* wrapperVec = new std::vector<TxExpressionNode*>( nodeVec->size() );
    std::transform( nodeVec->cbegin(), nodeVec->cend(), wrapperVec->begin(),
                    []( N* n ) -> TxExpressionNode* {return new TxExprWrapperNode ( n );} );
    return wrapperVec;
}

/** Helper function that wraps a vector of TxTypeDefiningNode. The returned vector contains TxTypeExprWrapperNode instances. */
template<class N>
[[maybe_unused]] std::vector<TxTypeExpressionNode*>* make_type_wrapper_vec( const std::vector<N*>* nodeVec ) {
    if ( !nodeVec )
        return nullptr;
    auto* wrapperVec = new std::vector<TxTypeExpressionNode*>( nodeVec->size() );
    std::transform( nodeVec->cbegin(), nodeVec->cend(), wrapperVec->begin(),
                    []( N* n ) -> TxTypeExpressionNode* {return new TxTypeExprWrapperNode ( n );} );
    return wrapperVec;
}

/** Wraps a TxEntityDeclaration as a TxTypeExpressionNode.
 * If this wrapper is used to declare a type name, that name will effectively be a type alias. */
class TxTypeDeclWrapperNode : public TxTypeExpressionNode {
    TxEntityDeclaration const * const typeDecl;

protected:
    TxQualType define_type( TxTypeResLevel typeResLevel ) override {
        return this->typeDecl->get_definer()->resolve_type( typeResLevel );
    }

public:
    TxTypeDeclWrapperNode( const TxLocation& ploc, const TxEntityDeclaration* typeDecl )
            : TxTypeExpressionNode( ploc ), typeDecl( typeDecl ) {
        ASSERT( typeDecl, "NULL typeDecl; ploc: " << ploc );
    }

    TxTypeDeclWrapperNode* make_ast_copy() const override {
        return new TxTypeDeclWrapperNode( this->ploc, this->typeDecl );
    }

    [[maybe_unused]] const TxEntityDeclaration* get_wrapped_declaration() const {
        return this->typeDecl;
    }

    void code_gen_type( LlvmGenerationContext& context ) const override { }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
    }
};

/** An internal AST root node for processing a compiler-generated node. */
class TxInternalRootNode : public TxNode {
    TxNode* innerNode;

public:
    TxInternalRootNode( TxNode* childNode, const LexicalContext& context )
            : TxNode( childNode->ploc ), innerNode( childNode ) {
        this->lexContext = context;
    }

    TxNode* make_ast_copy() const override {
        ASSERT( false, "Can't make AST copy of " << this );
        return nullptr;
    }

    void visit_descendants( const AstVisitor& visitor, const AstCursor& cursor, const std::string& role, void* aux ) override {
        this->innerNode->visit_ast( visitor, cursor, "node", aux );
    }
};
