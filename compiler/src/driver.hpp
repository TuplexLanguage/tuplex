#pragma once

#include <string>
#include <vector>
#include <deque>
#include <unordered_map>
#include <unordered_set>

#include "util/logging.hpp"
#include "identifier.hpp"
#include "tx_options.hpp"

namespace llvm {
class LLVMContext;
}

class TxPackage;
class TxNode;
class TxDeclarationNode;
class TxParsingUnitNode;
class TxParserContext;
class BuiltinTypes;
class LlvmGenerationContext;

std::string get_version_string();


/** Represents a Tuplex package compilation job.
 * Conducts the whole scanning, parsing, symbol table and semantic passes, and code generation.
 * A TxDriver instance is not reentrant in the sense that it only performs a single compilation.
 */
class TxDriver {
    Logger& _LOG;

    const TxOptions options;

    /** The name of the first user source file of this compilation package.
     * Will often contain the 'main' function but this is not guaranteed. */
    std::string firstSourceFilename;

    /** Parser context representing the built-in internally coded constructs (without actual source code). */
    TxParserContext* builtinParserContext;

    /** the currently compiled tuplex package */
    TxPackage* package = nullptr;

    /** creates the built-in types' AST */
    BuiltinTypes* builtinTypes = nullptr;

    /** global LLVMContext */
    llvm::LLVMContext* llvmContext;

    /** Note, may only be used for constant evaluation before code generation pass. */
    LlvmGenerationContext* genContext = nullptr;

    /** number of compilation errors */
    int error_count = 0;
    /** number of compilation warnings */
    int warning_count = 0;

    /** The queue of source files to parse in this compilation. */
    std::deque<std::pair<TxIdentifier, std::string> > sourceFileQueue;

    /** The parsing units for the source files already parsed, in parse order. */
    std::vector<TxParserContext*> parsedASTs;

    /** The source files already parsed.
     * The value is the top level root node of the AST. */
    std::unordered_map<std::string, TxParsingUnitNode*> parsedSourceFiles;

    /** The ASTs for the reachable non-local declarations/entities. */
    std::vector<TxDeclarationNode*> reachableASTsQueue;
    std::unordered_set<unsigned long> reachableASTs;

    /** Performs resolution and verification passes by traversing *all* parsed entities in lexical order */
    void compile_lexical();
    /** Performs resolution and verification passes by only traversing reachable entities */
    void compile_reachable();

    /** Generate LLVM IR and/or bytecode. */
    int llvm_compile( const std::string& outputFilename );

    /** Add all .tx source files directly under the specified directory to the currently compiling package. */
    int add_all_in_dir( const TxIdentifier& moduleName, const std::string &dirPath, bool recurseSubDirs );

    /** Add a source file to the currently compiling package.
     * @param moduleName the module expected to be found in the source file
     * @param filePath the path to the source file
     */
    void add_source_file( const TxIdentifier& moduleName, const std::string &filePath );

    /** Add a module to the currently compiling package.
     * The Tuplex source path will be searched for the module's source.
     * @return true if the module's source was found (does not indicate whether parse and compilation succeeded)
     */
    bool add_import( const TxIdentifier& moduleName );

    friend class TxParserContext;

public:
    /** Constructs a TxDriver instance with the specified run-time options. */
    explicit TxDriver( const TxOptions& options );

    virtual ~TxDriver();

    inline const std::string& get_first_source_filename() const {
        return this->firstSourceFilename;
    }

    inline const TxOptions& get_options() const {
        return this->options;
    }

    inline BuiltinTypes& builtins() const {
        return *this->builtinTypes;
    }

    inline TxParserContext* builtin_parser_context() const {
        return this->builtinParserContext;
    }

    inline LlvmGenerationContext* get_llvm_gen_context() const {
        ASSERT( this->genContext, "Can't get LlvmGenerationContext before code generation pass" );
        return this->genContext;
    }

    /** Compile this Tuplex package. May only be called once.
     * Return values:
     * 0 upon success
     * 1 if compilation could not start (e.g. bad command arguments or no source)
     * 2 if compilation failed (source code errors)
     * 3 if code generation failed
     * @return 0 on success
     */
    int compile( const std::vector<std::string>& startSourceFiles, const std::string& outputFileName );

    /** Adds a node to the reachable graph. Typically called from a resolving AST node that references the node. */
    void add_reachable( TxNode* node );
};
