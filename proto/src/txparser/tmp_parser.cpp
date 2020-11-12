#include "parser_if.hpp"

#include "tx_options.hpp"
#include "util/logging.hpp"

#include "scanner.hpp"


static Logger& _LOG = Logger::get( "TARSER" );

using namespace std;

static int tx_scan(TxParserContext* parserContext, const char* buffer, const TxOptions& options ) {
    cout << "Attempting scan of \n\"" << buffer << "\"" << endl;
    TxSourceBuffer srcBuffer( { buffer } );
    TxScanState state( srcBuffer );

    do {
        auto token = state.next_token();
        token.print( 0 );
        cout << endl;
        if ( token.id == TxTokenId::ERROR ) {
            cout << "Unrecognized character '" << srcBuffer.source[token.begin.index] << "'" << endl;
        } else if ( token.id == TxTokenId::END )
            break;
    } while ( true );

    return 0;
}

int tx_parse(TxParserContext* parserContext, const char* buffer, const TxOptions& options ) {
    return tx_scan( parserContext, buffer, options );
}

int tx_parse( TxParserContext* parserContext, FILE* file, const TxOptions& options ) {
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

    int ret = tx_parse( parserContext, buffer, options );
    free( buffer );
    return ret;
}
