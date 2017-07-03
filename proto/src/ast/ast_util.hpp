#pragma once

#include <vector>
#include <algorithm>

class TxNode;
class TxQualType;

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
std::vector<const TxQualType*> attempt_typevec( const std::vector<Node*>* nodevec ) {
    std::vector<const TxQualType*> types = std::vector<const TxQualType*>( nodevec->size() );
    std::transform( nodevec->cbegin(), nodevec->cend(), types.begin(), []( Node* node ) -> const TxQualType* {return node->attempt_qualtype();} );
    return types;
}

template<typename Node>
std::vector<const TxQualType*> resolve_typevec( const std::vector<Node*>* nodevec ) {
    std::vector<const TxQualType*> types = std::vector<const TxQualType*>( nodevec->size() );
    std::transform( nodevec->cbegin(), nodevec->cend(), types.begin(), []( Node* node ) -> const TxQualType* {
        node->resolve_type()->type(); return node->attempt_qualtype(); } );
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
