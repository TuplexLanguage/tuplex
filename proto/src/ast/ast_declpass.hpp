#pragma once

#include "ast_node.hpp"

/** Runs the declaration pass on a node and its subtree.
 * @parentNode the parent of node; must not be null */
extern void run_declaration_pass( TxNode* node, const TxNode* parentNode, const std::string& role="");

/** Runs the declaration pass on a node and its subtree. */
extern void run_declaration_pass( TxNode* node, const LexicalContext& lexContext );
