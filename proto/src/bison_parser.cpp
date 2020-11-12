//
// Created by christer on 11.11.20.
//

#include "parser_if.hpp"

#include "tx_options.hpp"

// bison's generated header file:
#include "parser.hpp"

extern FILE * yyin;
extern void yyrestart ( FILE *input_file );
extern int yy_flex_debug;
typedef struct yy_buffer_state *YY_BUFFER_STATE;
YY_BUFFER_STATE yy_scan_string( const char *yy_str );
void yy_delete_buffer (YY_BUFFER_STATE b );


static Logger& _LOG = Logger::get( "BARSER" );


int parse( TxParserContext* parserContext, FILE* file, const TxOptions& options ) {
    yyin = file;
    yyrestart( yyin );
    yy_flex_debug = options.debug_lexer;

    //if ( options.only_scan )  // currently unsupported
    //    return test_scanner();

    yy::TxParser parser( parserContext );
    parser.set_debug_level( options.debug_parser );
    int ret = parser.parse();
    return ret;
}

int parse( TxParserContext* parserContext, const char* source_buffer, const TxOptions& options ) {
    // TODO: make the line numbers match
    auto memBuffer = yy_scan_string( source_buffer );
    yy_flex_debug = options.debug_lexer;
    yy::TxParser parser( parserContext );
    int ret = parser.parse();
    yy_delete_buffer( memBuffer );
    return ret;
}
