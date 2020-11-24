#pragma once

#include <string>

class TxParserContext;

/** Contains a handle or reference to the source buffer / stream / file */
struct TxSourceBuffer {
    explicit TxSourceBuffer( const char* source ) : source( source ) { }
    // FUTURE: process input stream of UTF-8 characters instead (they have variable length)
    const char* source;
};

int parse( TxParserContext* parserContext );
