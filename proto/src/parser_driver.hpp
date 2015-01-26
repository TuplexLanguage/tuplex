#pragma once

#include <string>
#include <vector>
#include <deque>
#include <map>

#include "logging.hpp"

#include "identifier.hpp"

namespace yy {
    class location;
}
class TxPackage;
class TxParsingUnitNode;


// until we have injected error tracking into the driver instance:
extern int error_count;
extern void parser_error(const yy::location& parseLocation, char const *fmt, ...);


/** Represents Tuplex compilation run-time options. */
class TxOptions {
public:
    bool only_parse = false;
    bool debug_lexer = false;
    bool debug_parser = false;
    bool dump_symbol_table = false;
    bool dump_ir = false;
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

    /** The queue of source files to parse in this compilation. */
    std::deque< std::pair<TxIdentifier,std::string> > sourceFileQueue;
    /** The source files already parsed.
     * The value is the top level root node of the AST. */
    std::map<std::string, TxParsingUnitNode*> parsedSourceFiles;

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

public:
    /** currently set directly by parser */
    TxParsingUnitNode* parsingUnit = nullptr;

    /** Constructs a TxDriver instance with the specified run-time options. */
    TxDriver(const TxOptions& options);

    virtual ~TxDriver();

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
    std::string& current_input_filepath() { return this->sourceFileQueue.front().second; }


    // Error handling.
    void error(const yy::location& l, const std::string& m);
    void error(const std::string& m);
};


// Tell Flex the lexer's prototype ...
//# define YY_DECL \
//        yy::TxParser::symbol_type yylex(TxDriver& driver)
// declare yylex for the parser's sake
//YY_DECL;
