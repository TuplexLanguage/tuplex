#include "parsercontext.hpp"

#include "ast/ast_node.hpp"
#include "ast/ast_entitydefs.hpp"
#include "driver.hpp"
#include "tx_lang_defs.hpp"

static std::string format_location( const TxLocation& ploc ) {
    const size_t bufSize = 256;
    char buf[bufSize];
    auto filename = ploc.begin.filename ? ploc.begin.filename->c_str() : "";
    if ( ploc.begin.line == ploc.end.line ) {
        int lcol = ( ploc.end.column > ploc.begin.column ) ? ploc.end.column : ploc.end.column;
        snprintf( buf, bufSize, "%s %2d.%2d-%2d", filename,
                  ploc.begin.line,
                  ploc.begin.column, lcol );
    }
    else
        snprintf( buf, bufSize, "%s %2d.%2d-%2d.%2d", filename,
                  ploc.begin.line,
                  ploc.begin.column,
                  ploc.end.line,
                  ploc.end.column );
    return std::string( buf );
}

static std::string format_location_message( const TxLocation& ploc, char const *msg ) {
    return format_location(ploc) + " " + msg;
}

static std::string format_location_message( const TxParseOrigin* origin, char const *msg ) {
    auto message = format_location_message( origin->get_parse_location(), msg );
    auto reintOrigin = origin->get_origin_node();
    std::string prefix = "\n\t";
    do {
        if ( reintOrigin->is_context_set() ) {
            if ( auto reinterpretingNode = reintOrigin->context().reinterpretation_definer() ) {
                // display both reinterpretation site location and error site location
                message += prefix + "-- In type specialized from here: " + format_location( reinterpretingNode->get_parse_location() );
                reintOrigin = reinterpretingNode;
                prefix += '\t';
                continue;
            }
        }
        break;
    } while (true);
    return message;
}


static Logger& CLOG = Logger::get( "COMPILER" );


LlvmGenerationContext* TxParserContext::get_llvm_gen_context() const {
    // Note: LLVMContext are to be unique per thread, so if we in future use one ParserContext per thread each will have its own.
    return this->_driver.get_llvm_gen_context();
}

bool TxParserContext::is_internal_builtin() {
    return this == this->_driver.builtinParserContext;
}

bool TxParserContext::validate_module_name( const TxParseOrigin* origin, const TxIdentifier* moduleName ) {
    if ( moduleName->begins_with( BUILTIN_NS ) ) {
        if ( this->parseInputSourceSet != BUILTINS && this->parseInputSourceSet != TX_SOURCES) {
            if ( ! this->_driver.get_options().allow_tx ) {
                this->cerror( origin, "Can't declare or extend built-in namespace from user code: " + std::string( BUILTIN_NS ) );
                return false;
            }
        }
    }
    else if ( moduleName->str() == LOCAL_NS ) {
        if ( this->parseInputSourceSet != FIRST_USER_SOURCE ) {
            this->cerror( origin, "Only the first source file may have unspecified module name (implicit module " + std::string( LOCAL_NS ) + ")" );
            return false;
        }
    }
    auto res = moduleName->begins_with( this->_moduleName );
    if ( !res )
        this->cerror( origin, "Source contains module '" + moduleName->str() + "', not '"
                              + this->_moduleName.str()
                              + "' as expected." );
    return res;
}

bool TxParserContext::add_import( const TxIdentifier& moduleName ) {
    return this->_driver.add_import( moduleName );
}

void TxParserContext::emit_comp_error( const std::string& msg, ExpectedErrorClause* expErrorContext ) {
    if (this->in_exp_err()) {
        if ( expErrorContext )
            expErrorContext->encountered_error_count++;
        CLOG.info( "EXPECTED CERROR: %s", msg.c_str() );
    }
    else {
        this->_driver.error_count++;
        CLOG.error( "%s", msg.c_str() );
    }
}

void TxParserContext::emit_comp_warning( const std::string& msg ) {
    this->_driver.warning_count++;
    CLOG.warning( "%s", msg.c_str() );
}

void TxParserContext::emit_comp_info( const std::string& msg ) {
    CLOG.info( "%s", msg.c_str() );
}

void TxParserContext::begin_exp_err( const TxParseOrigin* origin ) {
    this->begin_exp_err( origin->get_parse_location(), origin->exp_err_ctx() );
}

void TxParserContext::begin_exp_err( const TxLocation& loc, ExpectedErrorClause* expError ) {
    this->expErrorStack.push( expError );
    //std::cerr << "EXPERR {  " << loc << std::endl;
}

ExpectedErrorClause* TxParserContext::end_exp_err( const TxLocation& loc ) {
    ASSERT( !this->expErrorStack.empty(), "EXPECTED ERROR block end doesn't match a corresponding begin, loc: " << loc );
    ExpectedErrorClause* currentExpErr = this->expErrorStack.top();
    this->expErrorStack.pop();
    //std::cerr << "} EXPERR  " << loc << std::endl;
    return currentExpErr;
}

bool TxParserContext::in_exp_err() const {
    return !this->expErrorStack.empty();
}

void TxParserContext::register_exp_err_node( TxNode* expErrNode ) {
    this->expErrorNodes.push_back( expErrNode );
}

void TxParserContext::finalize_expected_error_clauses() {
    ASSERT( !this->in_exp_err(), "Can't finalize expected error clauses with one still open" );
    for ( auto expErrNode : this->expErrorNodes ) {
        finalize_expected_error_clause( expErrNode );
    }
}

void TxParserContext::cerror( const TxParseOrigin* origin, const std::string& msg ) {
    auto str = format_location_message( origin, msg.c_str() );
    this->emit_comp_error( str, origin->exp_err_ctx() );
}

void TxParserContext::cerror( const TxLocation& loc, const std::string& msg ) {
    auto str = format_location_message( loc, msg.c_str() );
    this->emit_comp_error( str, ( this->expErrorStack.empty() ? nullptr : this->expErrorStack.top() ) );
}

void TxParserContext::cwarning( const TxParseOrigin* origin, const std::string& msg ) {
    auto str = format_location_message( origin, msg.c_str() );
    this->emit_comp_warning( str );
}

void TxParserContext::cinfo( const TxLocation& loc, const std::string& msg ) {
    auto str = format_location_message( loc, msg.c_str() );
    this->emit_comp_info( str );
}

std::string TxParserContext::str() const {
    return std::string( "ParserContext file '" ) + *this->current_input_filepath() + "'";
}
