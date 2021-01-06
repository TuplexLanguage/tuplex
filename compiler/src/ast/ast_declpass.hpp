#pragma once

#include "ast_node.hpp"

/**
On Passes

 Parsing Pass
 Parsing is bottom-up, the remaining passes are top-down.

 Declaration Pass
 Run for all the source code (irrespective of reachability; we don't know the reachable graph yet).
 Two purposes:
 1. Complete the AST set up, by setting the parent node reference and the lexical (scope) context.
    (These can't be set in the parse pass since parsing is bottom-up.)
 2. Register all declarations with the symbol table. This includes modules, types, fields, and scopes.

 Resolution Passes - repeated until the resolution queue / reachable graph has been completely processed:

     Type Creation Pass
     Run across the reachable AST graph.
     1. Create all the types (the TxActualType objects). Only do shallow resolution, i.e. do not resolve dependent types.
     2. Register all referenced entities (fields and types) in the reachable graph.

     Type Integration Pass
     Run for all the created types (i.e. doesn't traverse the AST as such).
     1. Full, recursive resolve of the types' dependencies - base types, interfaces, generic parameters and bindings.
     2. Complete the type's initialization according to their type class.
     3. Auto-generate constructors as necessary.
     4. Perform basic validation of the type (error checking).

     Resolution Pass
     Run across the reachable AST graph.
     1. Resolve all the types fully. FIXME: follow up - are any unintegrated types encountered here?
     2. Perform implicit and syntactic sugar behavior - transforming the AST as necessary

 Verification Pass
 Run across the reachable AST graph.
 1. Check for AST compilation errors that can't be checked before this pass.

 Type Preparation Pass
 Run for all the created types (i.e. doesn't traverse the AST as such).
 1. Traverse all the types' members, perform applicable type compilation error checking (e.g. field inheritance rules)
 2. Perform vtable and instance data layout

 (Dumps - symbol table, types, ast - are performed at this point if requested)

 Code Generation Pass
 Run across the reachable AST graph.
 1. Generate the LLVM code.

 */

/** Runs the declaration pass on a node and its subtree.
 * @param parentNode the parent of node; must not be null */
void run_declaration_pass( TxNode* node, const TxNode* parentNode, const std::string& role="");


/** Runs the type pass on a node and its subtree. */
void run_type_pass( TxNode* node, const std::string& role="");


/** Runs the resolution pass on a node and its subtree. */
void run_resolution_pass( TxNode* node, const std::string& role="");


/** Runs the verification pass on a node and its subtree. */
void run_verification_pass( const TxNode* node, const std::string& role="");


void inserted_node( TxNode* node, const TxNode* parentNode, const std::string& role="");
