#pragma once

#include <vector>
#include <algorithm>

class TxNode;
class TxType;

void print_node_root_path( const TxNode* toNode );

/** Helper function that makes a deep-copy of a vector of nodes. */
template<class N>
std::vector<N*>* make_node_vec_copy( const std::vector<N*>* nodeVec ) {
    if ( !nodeVec )
        return nullptr;
    std::vector<N*>* copyVec = new std::vector<N*>( nodeVec->size() );
    std::transform( nodeVec->cbegin(), nodeVec->cend(), copyVec->begin(),
                    []( N* n ) -> N* {return n->make_ast_copy();} );
    return copyVec;
}

/** Helper function that makes a deep-copy of a vector of nodes to an initializer list. */
template<class N>
std::vector<N*> make_node_vec_copy( const std::vector<N*>& nodeVec ) {
    std::vector<N*> copyVec( nodeVec.size() );
    std::transform( nodeVec.cbegin(), nodeVec.cend(), copyVec.begin(),
                    []( N* n ) -> N* {return n->make_ast_copy();} );
    return copyVec;
}

template<typename Node>
std::vector<const TxType*> attempt_typevec( const std::vector<Node*>* nodevec ) {
    std::vector<const TxType*> types = std::vector<const TxType*>( nodevec->size() );
    std::transform( nodevec->cbegin(), nodevec->cend(), types.begin(), []( Node* node ) -> const TxType* {return node->attempt_get_type();} );
    return types;
}

template<typename Node>
std::vector<const TxType*> resolve_typevec( const std::vector<Node*>* nodevec ) {
    std::vector<const TxType*> types = std::vector<const TxType*>( nodevec->size() );
    std::transform( nodevec->cbegin(), nodevec->cend(), types.begin(), []( Node* node ) -> const TxType* {
        node->resolve_type()->type(); return node->attempt_get_type(); } );
    return types;
}

//template<class N>
//const N* enclosing_node(const TxNode* node) const {
//    for ( const TxNode* node = node->parent(); node; node = node->parent() ) {
//        if ( auto stmtNode = dynamic_cast<const N*>( node ) )
//            return stmtNode;
//    }
//    return nullptr;
//}
