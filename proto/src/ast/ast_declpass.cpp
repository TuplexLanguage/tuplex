#include "ast_declpass.hpp"

#include "ast_wrappers.hpp"


AstVisitor declPassVisitor = []( TxNode* node, const AstCursor& parent, const std::string& role, void* parserCtx ) {
    node->node_declaration_pass( parent.node );
};

void run_declaration_pass( TxNode* node, const TxNode* parentNode, const std::string& role) {
    ASSERT(parentNode, "NULL parentNode");
    const AstCursor parent( parentNode );
    node->visit_ast( declPassVisitor, parent, role, nullptr );
}

void run_declaration_pass( TxNode* node, const LexicalContext& lexContext ) {
    ASSERT( lexContext.scope(), "uninitialized lex-context" );
    auto internalRoot = new TxInternalRootNode( node->parseLocation, node, lexContext );
    run_declaration_pass( node, internalRoot, "node" );
}
