#include "ast_util.hpp"

static unsigned inner_print_root_path( const TxNode* toNode ) {
    if ( !toNode )
        return 0;
    unsigned level = inner_print_root_path( toNode->parent() );
    std::cerr << std::string( level * 2, ' ' ) << toNode << std::endl;
    return level + 1;
}

void print_node_root_path( const TxNode* toNode ) {
    inner_print_root_path( toNode );
}
