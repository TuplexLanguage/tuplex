#pragma once

#include <iostream>
#include <vector>
#include <stack>


struct TxSourceBuffer {
    const char* source;
};


struct TxLineIndex {
    /** The source buffer index of each line. */
    std::vector<uint32_t> line_index;
};


enum class TxTokenId : u_int32_t {
    END = 0,
    ERROR,
    NEWLINE,
    WHITESPACE,
    INDENT,
    DEDENT,
    COMMENT,
    NAME,
    LBRACE,
    RBRACE,
    LPAREN,
    RPAREN,
    SEMICOLON,
    PACKAGE,
};

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<<( std::basic_ostream<charT, traits> &lhs, TxTokenId const &rhs ) {
    return lhs << rhs;
}


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


class TxBasicNode {
public:
    const TxSourceBuffer& buffer;
    const TxSourcePosition begin;
    const TxSourcePosition end;

    TxBasicNode( const TxSourceBuffer& buffer, const TxSourcePosition& begin, const TxSourcePosition& end )
            : buffer( buffer ), begin( begin ), end( end ) {}

    std::string_view getSourceText() const {
        return std::string_view( &( buffer.source[begin.index] ), ( end.index - begin.index ));
    }

    virtual void print( int indent ) = 0;
};


/** (A token can be seen as a simple case of node.) */
class TxToken : public TxBasicNode {
public:
    TxTokenId id;

    TxToken( const TxToken& token ) = default;

    TxToken( const TxSourceBuffer& buffer, const TxSourcePosition& begin, const TxSourcePosition& end, TxTokenId id )
            : TxBasicNode( buffer, begin, end ), id( id ) {}

    void print( int indent ) override;
};


class TxScanState {
    const TxSourceBuffer& buffer;
    TxLineIndex lineIndex;

    TxSourcePosition cursor;

    std::stack<size_t> indentStack;

    std::vector<TxToken> tokens;
    size_t nextToken;

    void advance_head( size_t length );

public:
    explicit TxScanState( const TxSourceBuffer& buffer )
            : buffer( buffer ), cursor(), nextToken( 0 ) {}

    const TxToken& next_token();
};
