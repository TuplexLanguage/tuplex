//
// Created by christer on 11.11.20.
//

#include "parser_if.hpp"

#include "tx_options.hpp"
#include <txparser/scanner.hpp>

// bison's generated header file:
#include "bison_parser.hpp"


static Logger& _LOG = Logger::get( "BARSER" );


int parse(TxParserContext* parserContext, const char* buffer, const TxOptions& options ) {
    TxSourceBuffer srcBuffer( { buffer } );
    auto scanState = new TxScanState( srcBuffer );
    parserContext->scanState = scanState;

    yy::TxParser parser( parserContext );
    parser.set_debug_level( options.debug_parser );
    int ret = parser.parse();
    return ret;
}

int parse( TxParserContext* parserContext, FILE* file, const TxOptions& options ) {
    // FUTURE: use input stream of unicode characters instead?

    // obtain file size:
    fseek( file, 0, SEEK_END );
    auto lSize = ftell( file );
    if ( lSize < 0 ) {
        _LOG.error( "File seek error" );
        return 1;
    }
    rewind( file );

    // allocate memory to contain the whole file:
    auto buffer = (char*) malloc( sizeof( char ) * lSize );
    if ( ! buffer ) {
        _LOG.error( "memory error" );
        return 2;
    }

    // copy the file into the buffer:
    long result = fread( buffer, 1, lSize, file );
    if ( result != lSize ) {
        _LOG.error( "File reading error" );
        free( buffer );
        return 3;
    }

    int ret = parse( parserContext, buffer, options );
    free( buffer );
    return ret;
}
