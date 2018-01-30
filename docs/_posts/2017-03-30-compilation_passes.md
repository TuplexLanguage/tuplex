---
layout: default
title: The Compilation Passes
---
{{ page.date | date: "%Y-%m-%d" }}
## {{ page.title }}

Labels: Bison, Compiler, Declaration, Flex, Resolution

How many parser / compiler passes should be considered "many"?

The Tuplex compiler has four. Or five. Or seven... What do we count? You could count the lexical and syntax parsing as two separate passes. Or just as two parts of a single one since they combine into a single linear pass over the source text.

The semantic analysis has four distinct steps. But they don't all traverse the full program so do they count as separate passes?

### Lexical Analysis

The lexical analysis scans the source text, removes comments and white space, and transforms it into a sequence of tokens, e.g. '+' or 'variable_name'.

The lexical scanner of Tuplex is written using <a href="https://en.wikipedia.org/wiki/Flex_(lexical_analyser_generator)">Flex</a>.

### Syntax Analysis

The syntax analysis processes the token sequence and matches it to the grammar rules. Each matched grammar rule typically produces a node in the Abstract Syntax Tree (AST).

The syntax parser is written using <a href="https://www.gnu.org/software/bison/">Bison</a>.

The grammar of Tuplex is a context-free, deterministic LR grammar.

### Semantic Analysis

The semantic analysis is where it gets really interesting! This is where the meaning of the program is processed. The design and internal model of the semantic analyzer is unique for each language, although it will have conceptual similarities with compilers of other high-level languages in the same genre.

An important part of the vision for Tuplex is that all the core constructs be first-class citizens, general, and orthogonal. This puts a lot of complexity on the semantic analyzer. It's been remodeled and refactored a number of times, mostly driven by of the demands of the type resolution system.

Since Tuplex supports declaring types and fields in any order, the semantic analysis needs to be divided into two separate passes.

### 1 - The Declaration Pass

The declaration pass traverses the entire AST from start to finish and builds the namespaces and their symbol tables. It essentially maps each and every AST node to a scope in the namespace hierarchy, along with registering all declaration attributes and and flags together with the declaration names.

### 2 - The Resolution Pass

The resolution pass traverses the entire AST a second time. Since the declaration pass has completed before this, all the names are known and can be cross-referenced (resolved). The compiler can now create the internal representation of all the program entities - types, fields and functions.

This is where three major semantic operations are performed:

* AST transformations that depend on knowledge of the types / fields being used - e.g. inlining and special semantics for certain constructs like references and constructors.

* Type derivations - setting up the internal representation of all the types defined in the program.

* Type specializations - reinterpreting (copying and re-analyzing) of the AST sub-trees of the generic (parameterized) types that are being specialized with concrete type arguments.

### 2b - Deferred Type Resolutions

Certain parameterized type definitions are semantically recursive, even while being legal and well defined. Consider the definition of the Enumerable interface:

    interface Enumerable{ E derives Enumerable{E} }

The constraint on the E parameter refers to the type being declared by this statement, and in addition the constraint refers to the parameter itself. To resolve such definitions the compiler works with a deferred ("lazy") resolution model. At the point where the type is defined a proxy is created for it, and put on the type resolution queue to be fully instantiated and laid out later.

### 2c - Data Type Layout

When all types are resolved they are mapped to static data types. The static data types are the distinct data types that will actually be used in runtime. They get assigned a distinct type id, a v-table entry, and tuples get their instance data laid out.


### Code Generation

This pass invokes the LLVM API to generate LLVM IR for the program.


Theoretically the compiler could probably be condensed into two start-to-end passes over the source, with some lazy evaluation in the mix. However this would be very difficult to work with, even if the language was completely defined to begin with! These passes have evolved over the compiler development process and do make for effective separations of concern for different parts of the compilation.
