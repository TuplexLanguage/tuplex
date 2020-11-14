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
    // TODO: remove options from parameters, it is accessible via parserCtx->driver().get_options()
    TxSourceBuffer srcBuffer( { buffer } );
    auto scanState = new TxSourceScan( srcBuffer );
    parserContext->scanState = scanState;

    yy::TxParser parser( parserContext );
    parser.set_debug_level( options.debug_parser );
    int ret = parser.parse();
    return ret;
}

int parse( TxParserContext* parserContext, FILE* file, const TxOptions& options ) {
    // FUTURE: use input stream of unicode characters instead?

    // obtain file size:
    int ret = fseek( file, 0, SEEK_END );
    if ( ret ) {
        _LOG.error( "File seek error - is it a proper, readable file? ret=%d", ret );
        return 1;
    }
    const long fSize = ftell( file );
    if ( fSize < 0 ) {
        _LOG.error( "File seek error, fSize=%ld", fSize );
        return 1;
    }
    rewind( file );

    // allocate memory to contain the whole file:
    auto buffer = (char*) malloc( sizeof( char ) * ( fSize + 1 ) );  // add space for null-terminator
    if ( ! buffer ) {
        _LOG.error( "memory error, file size %ld", fSize );
        return 2;
    }

    // copy the file into the buffer:
    long result = fread( buffer, 1, fSize, file );
    if ( result != fSize ) {
        _LOG.error( "File reading error" );
        free( buffer );
        return 3;
    }
    buffer[fSize] = '\0';  // append null terminator

    ret = parse( parserContext, buffer, options );
    // we retain the buffer and scan state in memory for now
    // free( buffer );
    return ret;
}
