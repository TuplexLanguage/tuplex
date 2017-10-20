#pragma once

#include "ast_node.hpp"


/** Runs the declaration pass on a node and its subtree.
 * @parentNode the parent of node; must not be null */
void run_declaration_pass( TxNode* node, const TxNode* parentNode, const std::string& role="");

/** Runs the declaration pass on a node and its subtree. */
void run_declaration_pass( TxNode* node, const LexicalContext& lexContext );


/** Runs the type pass on a node and its subtree. */
void run_type_pass( TxNode* node, const std::string& role="");


/** Runs the resolution pass on a node and its subtree. */
void run_resolution_pass( TxNode* node, const std::string& role="");


/** Runs the verification pass on a node and its subtree. */
void run_verification_pass( const TxNode* node, const std::string& role="");
