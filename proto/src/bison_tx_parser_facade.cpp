//
// Created by christer on 11.11.20.
//

#include "parser_if.hpp"

#include "driver.hpp"
#include "txparser/scanner.hpp"
#include "util/files_env.hpp"

// bison's generated header file:
#include "bison_parser.hpp"


static Logger& _LOG = Logger::get( "B-PARSER" );


const size_t chunk_size = 4000;  // FiXME: test this with small chunk size


/// consolidates chunks into a contiguous buffer
static char* consolidate( std::vector<char*>& bufChunks, char* lastChunk,
                          size_t fileSize, size_t lastReadLen) {
    auto buffer = (char*) malloc( sizeof( char ) * ( fileSize + 1 ));
    if ( !buffer ) {
        _LOG.fatal( "memory allocation failed" );
        for ( auto ch : bufChunks )
            free( ch );
        free( lastChunk );
        return nullptr;
    }
    auto dest = buffer;
    for ( auto ch : bufChunks ) {
        memcpy( dest, ch, chunk_size );
        free( ch );
        dest += chunk_size;
    }
    memcpy( dest, lastChunk, lastReadLen );
    free( lastChunk );
    return buffer;
}

static TxSourceBuffer load_file( FILE* file ) {
    char* buffer = nullptr;
    std::vector<char*> bufChunks;
    size_t fileSize = 0;
    do {
        auto chunk = (char*) malloc( chunk_size + 1 );
        if ( !chunk ) {
            _LOG.fatal( "memory allocation failed" );
            for ( auto ch : bufChunks )
                free( ch );
            return TxSourceBuffer( { nullptr } );
        }
        // read the next chunk of the file:
        size_t readLen = fread( chunk, 1, chunk_size, file );
        fileSize += readLen;
        if ( feof( file )) {
            if ( bufChunks.empty()) {
                //_LOG.debug( "Loaded source file in single chunk");
                buffer = chunk;
            }
            else {
                _LOG.note( "Loaded source file in %d chunks", bufChunks.size() + 1);
                buffer = consolidate( bufChunks, chunk, fileSize, readLen );
                if ( !buffer )
                    return TxSourceBuffer( { nullptr } );
            }
            buffer[fileSize] = '\0';  // append null terminator
            break;
        }
        else if ( readLen != chunk_size ) {
            _LOG.fatal( "File reading error, read %ld bytes, errno=%d", readLen, errno);
            for ( auto ch : bufChunks )
                free( ch );
            free( chunk );
            return TxSourceBuffer( { nullptr } );
        }
        bufChunks.push_back( chunk );
    } while ( true );

    return TxSourceBuffer( { buffer } );
}

static TxSourceBuffer load_file( const std::string& filePath ) {
    TxSourceBuffer srcBuffer{};
    if ( filePath.empty() || filePath == "-" ) {
        srcBuffer = load_file( stdin );
    }
    else {
        if ( file_status( filePath ) != 1 ) {
            _LOG.fatal( "Source file name '%s' is not found or not a file.", filePath.c_str());
            return TxSourceBuffer( { nullptr } );
        }
        FILE* file = fopen( filePath.c_str(), "rb" );  // open in 'binary' i.e. without translations
        if ( !file ) {
            _LOG.fatal( "Could not open source file '%s': %s", filePath.c_str(), strerror(errno));
            return TxSourceBuffer( { nullptr } );
        }
        srcBuffer = load_file( file );
        fclose( file );
    }
    return srcBuffer;
}


int parse( TxParserContext* parserContext, TxSourceBuffer srcBuffer ) {
    auto scanState = new TxSourceScan( srcBuffer );
    parserContext->scanState = scanState;

    yy::TxParser parser( parserContext );
    parser.set_debug_level( parserContext->driver().get_options().debug_parser );
    int ret = parser.parse();
    return ret;
}

int parse( TxParserContext* parserContext, const char* buffer ) {
    TxSourceBuffer srcBuffer( { buffer } );
    return parse( parserContext, srcBuffer );
}

int parse( TxParserContext* parserContext, const std::string& filePath ) {
    TxSourceBuffer srcBuffer = load_file( filePath );
    if ( srcBuffer.source != nullptr ) {
        _LOG.info( "+ Loaded source file '%s'", filePath.c_str());
        return parse( parserContext, srcBuffer );
    }
    else
        return -1;
}
