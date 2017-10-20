#pragma once

#include <vector>
#include <algorithm>

#include "symbol/qual_type.hpp"

class TxNode;

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
std::vector<TxQualType> attempt_typevec( const std::vector<Node*>* nodevec ) {
    std::vector<TxQualType> types = std::vector<TxQualType>( nodevec->size() );
    std::transform( nodevec->cbegin(), nodevec->cend(), types.begin(),
                    []( Node* node ) -> TxQualType { return node->attempt_qtype(); } );
    return types;
}

//class TxActualType;
//template<typename Node>
//std::vector<const TxActualType*> resolve_typevec( const std::vector<Node*>* nodevec ) {
//    std::vector<const TxActualType*> types = std::vector<const TxActualType*>( nodevec->size() );
//    std::transform( nodevec->cbegin(), nodevec->cend(), types.begin(),
//                    []( Node* node ) -> const TxActualType* { return node->resolve_type( passInfo ).type(); } );
//    return types;
//}

//template<class N>
//const N* enclosing_node(const TxNode* node) const {
//    for ( const TxNode* node = node->parent(); node; node = node->parent() ) {
//        if ( auto stmtNode = dynamic_cast<const N*>( node ) )
//            return stmtNode;
//    }
//    return nullptr;
//}
