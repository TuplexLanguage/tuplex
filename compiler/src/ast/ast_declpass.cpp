#include "ast_declpass.hpp"


AstVisitorFunc declPassVisitorFunc = []( TxNode* node, const AstCursor& cursor, const std::string& role, void* aux ) {
    node->node_declaration_pass( cursor.parentNode );
};

const AstVisitor declPassVisitor = { declPassVisitorFunc, nullptr, TXP_DECLARATION };

void run_declaration_pass( TxNode* node, const TxNode* parentNode, const std::string& role) {
    ASSERT( parentNode, "NULL parentNode" );
    const AstCursor cursor( parentNode );
    node->visit_ast( declPassVisitor, cursor, role, nullptr );
}



AstVisitorFunc typePassVisitorFunc = []( TxNode* node, const AstCursor& cursor, const std::string& role, void* aux ) {
    node->node_type_pass();
};

const AstVisitor typePassVisitor = { typePassVisitorFunc, nullptr, TXP_TYPE_CREATION };

void run_type_pass( TxNode* node, const std::string& role) {
    const AstCursor cursor( node->parent() );
    node->visit_ast( typePassVisitor, cursor, role, nullptr );
}



AstVisitorFunc resolutionPassVisitorFunc = []( TxNode* node, const AstCursor& cursor, const std::string& role, void* aux ) {
    node->node_resolution_pass();
};

const AstVisitor resolutionPassVisitor = { nullptr, resolutionPassVisitorFunc, TXP_RESOLUTION };

void run_resolution_pass( TxNode* node, const std::string& role) {
    const AstCursor cursor( node->parent() );
    node->visit_ast( resolutionPassVisitor, cursor, role, nullptr );
}



AstVisitorFunc verificationPassVisitorFunc = []( TxNode* node, const AstCursor& cursor, const std::string& role, void* aux ) {
    node->node_verification_pass();
};

const AstVisitor verificationPassVisitor = { verificationPassVisitorFunc, nullptr, TXP_VERIFICATION };

void run_verification_pass( const TxNode* node, const std::string& role) {
    const AstCursor cursor( node->parent() );
    // Note - does not modify the nodes, however the visitor functions are non-const
    const_cast<TxNode*>( node )->visit_ast( verificationPassVisitor, cursor, role, nullptr );
}



void inserted_node( TxNode* node, const TxNode* parentNode, const std::string& role ) {
    ASSERT( parentNode, "NULL parentNode" );
    const AstCursor cursor( parentNode );
    if ( node->get_last_pass() < TXP_DECLARATION && parentNode->get_last_pass() >= TXP_DECLARATION ) {
        const AstVisitor visitor = { declPassVisitorFunc, nullptr, TXP_DECLARATION, true };
        node->visit_ast( visitor, cursor, role, nullptr );
    }
    if ( node->get_last_pass() < TXP_TYPE_CREATION && parentNode->get_last_pass() >= TXP_TYPE_CREATION ) {
        const AstVisitor visitor = { typePassVisitorFunc, nullptr, TXP_TYPE_CREATION, true };
        node->visit_ast( visitor, cursor, role, nullptr );
    }
    if ( node->get_last_pass() < TXP_RESOLUTION && parentNode->get_last_pass() >= TXP_RESOLUTION ) {
        const AstVisitor visitor = { nullptr, resolutionPassVisitorFunc, TXP_RESOLUTION, true };
        node->visit_ast( visitor, cursor, role, nullptr );
    }
    if ( node->get_last_pass() < TXP_VERIFICATION && parentNode->get_last_pass() >= TXP_VERIFICATION ) {
        const AstVisitor visitor = { verificationPassVisitorFunc, nullptr, TXP_VERIFICATION, true };
        node->visit_ast( visitor, cursor, role, nullptr );
    }
    ASSERT( node->get_last_pass() == parentNode->get_last_pass(),
            "Mismatching passes between child " << node << "  and parent " << parentNode );
}
