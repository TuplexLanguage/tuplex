#pragma once

#include <string>
#include <vector>
#include <deque>
#include <unordered_map>

#include "logging.hpp"

#include "identifier.hpp"

namespace yy {
    class location;
}
class TxPackage;
class TxParsingUnitNode;


/** Represents Tuplex compilation run-time options. */
class TxOptions {
public:
    bool only_parse = false;
    bool debug_lexer = false;
    bool debug_parser = false;
    bool dump_symbol_table = false;
    bool dump_ir = false;
    bool run_jit = true;
    bool no_bc_output = false;
    bool suppress_asserts = false;
    std::vector<std::string> sourceSearchPaths;
    std::vector<std::string> startSourceFiles;
    std::string outputFileName;
};


/** Represents a Tuplex package compilation job.
 * Conducts the whole scanning, parsing, symbol table and semantic passes, and code generation.
 */
class TxDriver {
    Logger& LOG;

    const TxOptions options;
    TxPackage * const package;

    /** true if within an EXPERR block */
    bool exp_err = false;
    /** number or errors encountered within an EXPERR block */
    int exp_err_count = 0;

    /** number of compilation errors */
    int error_count = 0;
    /** number of compilation warnings */
    int warning_count = 0;

    /** used for parse error messages */
    std::string* currentInputFilename = nullptr;

    /** The queue of source files to parse in this compilation. */
    std::deque< std::pair<TxIdentifier,std::string> > sourceFileQueue;
    /** The ASTs of the source files already parsed, in parse order.
     * The value is the top level root node of the AST. */
    std::vector<TxParsingUnitNode*> parsedASTs;
    /** The source files already parsed.
     * The value is the top level root node of the AST. */
    std::unordered_map<std::string, TxParsingUnitNode*> parsedSourceFiles;

    // Handling the scanner.
    int scan_begin(const std::string &filePath);
    void scan_end();

    /** Parse a file, including it in the currently compiling package. */
    int parse(const std::string &filePath);

    /** Generate LLVM IR and/or bytecode. */
    int llvm_compile();


    /** Add all .tx source files directly under the specified directory to the currently compiling package. */
    int add_all_in_dir(const TxIdentifier& moduleName, const std::string &dirPath);

    /** Add a source file to the currently compiling package.
     * @param moduleName the module expected to be found in the source file
     * @param filePath the path to the source file
     */
    void add_source_file(const TxIdentifier& moduleName, const std::string &filePath);

    void emit_comp_error(char const *msg);
    void emit_comp_warning(char const *msg);

public:
    /** currently set directly by parser */
    TxParsingUnitNode* parsingUnit = nullptr;

    /** Constructs a TxDriver instance with the specified run-time options. */
    TxDriver(const TxOptions& options);

    virtual ~TxDriver();

    inline const TxOptions& get_options() const { return this->options; }


    /** Compile this Tuplex package.
     * @return 0 on success
     */
    int compile();

    bool validate_module_name(const TxIdentifier& moduleName);

    /** Add a module to the currently compiling package.
     * The Tuplex source path will be searched for the module's source.
     * @return true if the module's source was found (does not indicate whether parse and compilation succeeded)
     */
    bool add_import(const TxIdentifier& moduleName);

    /** The path of the file currently being parsed.
     * Used later to pass the file path to the location tracker. */
    std::string* current_input_filepath();


    // Compilation error handling.
    void begin_exp_err(const yy::location& loc);
    int    end_exp_err(const yy::location& loc);
    bool    is_exp_err();

    int  get_error_count();
    int  get_warning_count();

    void cerror(const yy::location& loc, char const *fmt, ...);
    void cerror(const yy::location& loc, const std::string& msg);
    void cerror(const std::string& msg);
    void cwarning(const yy::location& loc, char const *fmt, ...);
    void cwarning(const yy::location& loc, const std::string& msg);
    void cwarning(const std::string& msg);

    // fall-back for when driver instance is not reachable */
    static void emit_comp_error(const yy::location& loc, const std::string& msg);
    static void emit_comp_warning(const yy::location& loc, const std::string& msg);
};
