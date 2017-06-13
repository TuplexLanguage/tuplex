#pragma once

#include <stack>

#include "util/printable.hpp"

#include "identifier.hpp"
#include "tx_error.hpp"

namespace yy {
class location;
}

class TxDriver;
class TxNode;
class TxParsingUnitNode;
class LlvmGenerationContext;

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

    enum ParseInputSourceSet { BUILTINS, TX_SOURCES, FIRST_USER_SOURCE, REST_USER_SOURCES };
    const ParseInputSourceSet parseInputSourceSet;

    TxParserContext( TxDriver& driver, TxIdentifier moduleName, const std::string &filePath, ParseInputSourceSet parseInputSourceSet )
            : _driver( driver ), _moduleName( moduleName ), parseInputSourceSet( parseInputSourceSet ) {
        // FUTURE: make parser not save *pointer* to filename, necessitating this leaky snippet:
        this->_currentInputFilename = new std::string( filePath );
    }

    virtual ~TxParserContext() = default;

    inline TxDriver& driver() const {
        return this->_driver;
    }

    /** Returns the LLVMContext for this parser context. Can be used in constant expression evaluation during analysis. */
    LlvmGenerationContext* get_llvm_gen_context() const;

    /** Returns true if this parser context is for the internally coded built-in constructs (lacking actual source code). */
    bool is_internal_builtin();

    /** Returns true if this parser context is processing user source code. */
    bool is_user_source() {
        return ( this->parseInputSourceSet == FIRST_USER_SOURCE || this->parseInputSourceSet == REST_USER_SOURCES );
    }

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
    bool add_import( const TxIdentifier& moduleName );

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
