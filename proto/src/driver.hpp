#pragma once

#include <string>
#include <vector>
#include <deque>
#include <unordered_map>

#include "util/logging.hpp"
#include "identifier.hpp"

namespace llvm {
class LLVMContext;
}

class TxPackage;
class TxParsingUnitNode;
class TxParserContext;
class BuiltinTypes;
class LlvmGenerationContext;

std::string get_version_string();

/** Represents Tuplex compilation run-time options. */
class TxOptions {
public:
    bool only_parse = false;
    bool debug_lexer = false;
    bool debug_parser = false;
    bool dump_ast = false;
    bool dump_symbol_table = false;
    bool dump_tx_symbols = false;
    bool dump_types = false;
    bool dump_ir = false;
    bool run_verifier = false;
    bool run_jit = false;
    bool no_bc_output = false;
    bool suppress_asserts = false;
    bool allow_tx = false;
    std::string txPath;
    std::vector<std::string> sourceSearchPaths;
    int jit_argc = 0;
    const char** jit_argv = nullptr;
};

/** Represents a Tuplex package compilation job.
 * Conducts the whole scanning, parsing, symbol table and semantic passes, and code generation.
 * A TxDriver instance is not reentrant in the sense that it only performs a single compilation.
 */
class TxDriver {
    Logger& _LOG;

    const TxOptions options;

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

    // Handling the scanner.
    int scan_begin( const std::string &filePath );
    void scan_end();

    /** Parse a file, including it in the currently compiling package. */
    int parse( TxParserContext& parserContext );

    /** Generate LLVM IR and/or bytecode. */
    int llvm_compile( const std::string& outputFileName );

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
    TxDriver( const TxOptions& options );

    virtual ~TxDriver();

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
};
