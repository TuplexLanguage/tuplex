#pragma once

#include <stack>
#include <utility>

#include "util/printable.hpp"

#include "identifier.hpp"
#include "tx_error.hpp"

#include "parser_if.hpp"

#include "llvm/IR/DebugInfoMetadata.h"

namespace yy {
class location;
}

class TxDriver;
class TxSourceScan;
class TxNode;
class TxParsingUnitNode;
class LlvmGenerationContext;

/** Creates a formatted string of a parse location. */
std::string format_location( const TxLocation& ploc );

/** Represents the processing of a parsing unit.
 * When a driver compiles a package it consists of one or more parsing units.
 * Also acts as a proxy towards TxDriver, in future this may be used
 * to enable parallel compilation of parsing units.
 */
class TxParserContext : public Printable {
    TxDriver& _driver;
    const TxIdentifier _moduleName;  // note, may be empty
    /** used for parse error messages */
    const std::string _inputFilename;

    /** non-empty if currently processing within an EXPERR block */
    std::stack<ExpectedErrorClause*> expErrorStack;

    llvm::DIFile *_debugFile = nullptr;

    void emit_comp_error( const std::string& msg, ExpectedErrorClause* expErrorContext );
    void emit_comp_warning( const std::string& msg );
    static void emit_comp_info( const std::string& msg );

    /** the expected-error nodes having been parsed */
    std::vector<TxNode*> expErrorNodes;

public:
    /** Represents the source scan of this parsing unit. */
    TxSourceScan* const scanCtx;

    /** set directly by parser */
    TxParsingUnitNode* parsingUnit = nullptr;

    enum ParseInputSourceSet { BUILTINS, TX_SOURCES, FIRST_USER_SOURCE, REST_USER_SOURCES };
    const ParseInputSourceSet parseInputSourceSet;

    TxParserContext( TxDriver& driver, TxIdentifier moduleName, const std::string& filePath,
                     TxSourceBuffer sourceBuffer, ParseInputSourceSet parseInputSourceSet );

    ~TxParserContext() override = default;

    inline TxDriver& driver() const {
        return this->_driver;
    }

    void init_debug();

    llvm::DIFile* debug_file() const;

    /** Returns the LLVMContext for this parser context. Can be used in constant expression evaluation during analysis. */
    LlvmGenerationContext* get_llvm_gen_context() const;

    /** Returns true if this parser context is for the internally coded built-in constructs (lacking actual source code). */
    bool is_internal_builtin();

    /** Returns true if this parser context is processing user source code. */
    bool is_user_source() const {
        return ( this->parseInputSourceSet == FIRST_USER_SOURCE || this->parseInputSourceSet == REST_USER_SOURCES );
    }

    /** The path of the file currently being parsed.
     * Used later to pass the file path to the location tracker. */
    const std::string* source_filepath() const {
        return &this->_inputFilename;
    }

    /** Checks that the module name is valid in relation to the currently parsed source file and its file name/path. */
    bool validate_module_name( const TxParseOrigin* origin, const TxIdentifier* moduleName );

    /** Add a module to the currently compiling package.
     * The Tuplex source path will be searched for the module's source.
     * @return true if the module's source was found (does not indicate whether parse and compilation succeeded)
     */
    bool add_import( const TxIdentifier& moduleName );

    /** Registers an expected-error node that is being parsed within this context. */
    void register_exp_err_node( TxNode* expErrNode );

    /** Checks all expected-error nodes of this parser context if the expected errors have occurred. */
    void finalize_expected_error_clauses();

    const std::vector<TxNode*>& get_exp_error_nodes() const;

    // Compilation error handling.
    void cerror( const TxParseOrigin* origin, const std::string& msg );
    void cwarning( const TxParseOrigin* origin, const std::string& msg );
    void cinfo( const TxLocation& loc, const std::string& msg );

    /** should only be used when ParseOrigin is not available */
    void cerror( const TxLocation& loc, const std::string& msg );
    void cwarning( const TxLocation& loc, const std::string& msg );

    // Compilation error handling.
    void begin_exp_err( const TxParseOrigin* origin );
    void begin_exp_err( const TxLocation& loc, ExpectedErrorClause* expError );
    ExpectedErrorClause* end_exp_err( const TxLocation& loc );

    std::string str() const override;
};
