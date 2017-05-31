#pragma once

#include <string>
#include <vector>
#include <deque>
#include <stack>
#include <unordered_map>

#include "util/logging.hpp"

#include "location.hpp"
#include "identifier.hpp"
#include "tx_error.hpp"

namespace llvm {
class LLVMContext;
}

namespace yy {
class location;
}
class TxPackage;
class TxNode;
class TxParsingUnitNode;
class TxFieldDeclNode;
class TxParserContext;
class LlvmGenerationContext;

/** Represents Tuplex compilation run-time options. */
class TxOptions {
public:
    bool only_parse = false;
    bool debug_lexer = false;
    bool debug_parser = false;
    bool dump_ast = false;
    bool dump_symbol_table = false;
    bool dump_tx_symbols = false;
    bool dump_ir = false;
    bool run_verifier = false;
    bool run_jit = false;
    bool no_bc_output = false;
    bool suppress_asserts = false;
    bool allow_tx = false;
    std::string txPath;
    std::vector<std::string> sourceSearchPaths;
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

    /*--- these members reflect the current compilation state ---*/
    /** the currently compiled tuplex package */
    TxPackage* package = nullptr;

    // Note, may only be used for constant evaluation before code generation pass:
    llvm::LLVMContext* llvmContext = nullptr;

    // not set before code generation pass:
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

/** Represents the processing of a parsing unit.
 * When a driver compiles a package it consists of one or more parsing units.
 * Also acts as a proxy towards TxDriver, in future this may be used
 * to enable parallel compilation of parsing units.
 */
class TxParserContext : public Printable {
    TxDriver& _driver;
    TxIdentifier _moduleName;  // note, may be empty
    /** used for parse error messages */
    std::string* _currentInputFilename = nullptr;

    /** non-empty if currently processing within an EXPERR block */
    std::stack<ExpectedErrorClause*> expErrorStack;
    //    ExpectedErrorClause* expError = nullptr;

    void emit_comp_error( const std::string& msg, ExpectedErrorClause* expErrorContext );
    void emit_comp_warning( const std::string& msg );
    void emit_comp_info( const std::string& msg );

    /** the expected-error nodes having been parsed */
    std::vector<TxNode*> expErrorNodes;

public:
    /** set directly by parser */
    TxParsingUnitNode* parsingUnit = nullptr;

    /** used by lexer to track nested comments */
    unsigned commentNestLevel = 0;

    TxParserContext( TxDriver& driver, TxIdentifier moduleName, const std::string &filePath )
            : _driver( driver ), _moduleName( moduleName ) {
        // FUTURE: make parser not save *pointer* to filename, necessitating this leaky snippet:
        this->_currentInputFilename = new std::string( filePath );
    }

    virtual ~TxParserContext() = default;

    TxDriver& driver() const {
        return this->_driver;
    }

    /** Returns the LLVMContext for this parser context. Can be used in constant expression evaluation during analysis. */
    inline LlvmGenerationContext* get_llvm_gen_context() const {
        // Note: LLVMContext are to be unique per thread, so if we in future use one ParserContext per thread each will have its own.
        return this->_driver.get_llvm_gen_context();
    }

    /** Returns true if this parser context is for the internally coded built-in constructs (lacking actual source code). */
    bool is_internal_builtin();

    /** The path of the file currently being parsed.
     * Used later to pass the file path to the location tracker. */
    std::string* current_input_filepath() const {
        return this->_currentInputFilename;
    }

    /** Checks that the module name is valid in relation to the currently parsed source file and its file name/path. */
    bool validate_module_name( const TxParseOrigin* origin, const TxIdentifier* moduleName );

    /** Add a module to the currently compiling package.
     * The Tuplex source path will be searched for the module's source.
     * @return true if the module's source was found (does not indicate whether parse and compilation succeeded)
     */
    bool add_import( const TxIdentifier& moduleName ) {
        return this->_driver.add_import( moduleName );
    }

    /** Registers an expected-error node that is being parsed within this context. */
    void register_exp_err_node( TxNode* expErrNode );

    /** Checks all expected-error nodes of this parser context if the expected errors have occurred. */
    void finalize_expected_error_clauses();

    // Compilation error handling.
    void cerror( const TxParseOrigin* origin, const std::string& msg );
    void cwarning( const TxParseOrigin* origin, const std::string& msg );
    void cinfo( const TxLocation& loc, const std::string& msg );

    /** should only be used when ParseOrigin is not available */
    void cerror( const TxLocation& loc, const std::string& msg );

    // Compilation error handling.
    void begin_exp_err( const TxParseOrigin* origin );
    void begin_exp_err( const TxLocation& loc, ExpectedErrorClause* expError );
    ExpectedErrorClause* end_exp_err( const TxLocation& loc );
    bool in_exp_err() const;

    virtual std::string str() const override;
};
