#pragma once

#include <iostream>
#include <vector>
#include <stack>

#include "tx_tokens.hpp"


/** Contains a handle or reference to the source buffer / stream / file */
struct TxSourceBuffer {
    const char* source;
};


struct TxLineIndex {
    /** The source buffer index of each line. */
    std::vector<uint32_t> line_index;
};


/** A specific character position within a buffer. (The buffer is not referenced by this object.)
 * Line numbering starts at 1. Column numbering starts at 1. */
struct TxSourcePosition {
    TxSourcePosition() : TxSourcePosition( 0, 1, 1 ) {}

    TxSourcePosition( const TxSourcePosition& ) = default;

    TxSourcePosition( uint32_t index, uint32_t line, uint32_t column )
            : index( index ), line( line ), column( column ) {}

    uint32_t index;
    uint32_t line;
    uint32_t column;
};


class TxBasicNode {  // TODO: do we want a common superclass for tokens and ast nodes?
public:
    const TxSourceBuffer& buffer;  // TODO: should it refer buffer, or parser context, or nothing?
    const TxSourcePosition begin;
    const TxSourcePosition end;

    TxBasicNode( const TxSourceBuffer& buffer, const TxSourcePosition& begin, const TxSourcePosition& end )
            : buffer( buffer ), begin( begin ), end( end ) {}

    std::string_view getSourceText() const {
        return std::string_view( &( buffer.source[begin.index] ), ( end.index - begin.index ));
    }

    virtual void print( int indent ) const = 0;
};


/** (A token can be seen as a simple case of node.) */
class TxToken : public TxBasicNode {
public:
    TxTokenId id;

    TxToken( const TxToken& token ) = default;

    TxToken( const TxSourceBuffer& buffer, const TxSourcePosition& begin, const TxSourcePosition& end, TxTokenId id )
            : TxBasicNode( buffer, begin, end ), id( id ) {}

    void print( int indent ) const override;
};


class TxScanner;


class TxSourceScan {
    void advance_head( size_t length );

    // input
    const TxSourceBuffer buffer;

    // current state
    TxSourcePosition cursor;
    size_t nextToken;

    // outputs
    TxLineIndex lineIndex;
    std::vector<TxToken> tokens;

public:
    // external interface
    explicit TxSourceScan( const TxSourceBuffer& buffer );

    const TxToken& next_token();

    inline const TxSourcePosition& current_cursor() const {
        return this->cursor;
    }


    // internal interface; encapsulate?
    std::stack<const TxScanner*> scannerStack;
    std::stack<uint32_t> indentStack;

    inline const char* input_buffer() const {
        return &buffer.source[cursor.index];
    }

    void add_token( TxTokenId id, uint32_t len );
};
