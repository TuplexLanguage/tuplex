#include "ast_declpass.hpp"

#include "ast_wrappers.hpp"


AstVisitorFunc declPassVisitorFunc = []( TxNode* node, const AstCursor& parent, const std::string& role, void* parserCtx ) {
    node->node_declaration_pass( parent.node );
};

const AstVisitor declPassVisitor = { declPassVisitorFunc, nullptr };

void run_declaration_pass( TxNode* node, const TxNode* parentNode, const std::string& role) {
    ASSERT(parentNode, "NULL parentNode");
    const AstCursor cursor( parentNode );
    node->visit_ast( declPassVisitor, cursor, role, nullptr );
}

void run_declaration_pass( TxNode* node, const LexicalContext& lexContext ) {
    ASSERT( lexContext.scope(), "uninitialized lex-context" );
    auto internalRoot = new TxInternalRootNode( node->ploc, node, lexContext );
    run_declaration_pass( node, internalRoot, "node" );
}



AstVisitorFunc typePassVisitorFunc = []( TxNode* node, const AstCursor& parent, const std::string& role, void* parserCtx ) {
    node->node_type_pass();
};

const AstVisitor typePassVisitor = { typePassVisitorFunc, nullptr };

void run_type_pass( TxNode* node, const std::string& role) {
    const AstCursor cursor( node->parent() );
    node->visit_ast( typePassVisitor, cursor, role, nullptr );
}



AstVisitorFunc resolutionPassVisitorFunc = []( TxNode* node, const AstCursor& parent, const std::string& role, void* parserCtx ) {
    node->node_resolution_pass();
};

const AstVisitor resolutionPassVisitor = { nullptr, resolutionPassVisitorFunc };

void run_resolution_pass( TxNode* node, const std::string& role) {
    const AstCursor cursor( node->parent() );
    node->visit_ast( resolutionPassVisitor, cursor, role, nullptr );
}



AstVisitorFunc verificationPassVisitorFunc = []( TxNode* node, const AstCursor& parent, const std::string& role, void* parserCtx ) {
    node->node_verification_pass();
};

const AstVisitor verificationPassVisitor = { verificationPassVisitorFunc, nullptr };

void run_verification_pass( const TxNode* node, const std::string& role) {
    const AstCursor cursor( node->parent() );
    // Note - does not modify the nodes, however the visitor functions are non-const
    const_cast<TxNode*>( node )->visit_ast( verificationPassVisitor, cursor, role, nullptr );
}
