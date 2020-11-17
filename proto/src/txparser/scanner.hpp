#pragma once

#include <iostream>
#include <vector>
#include <stack>

#include "parser_if.hpp"
#include "tx_tokens.hpp"


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


class TxBasicNode : public Printable {  // TODO: do we want a common superclass for tokens and ast nodes?
public:
    const TxSourceBuffer& buffer;  // TODO: should it refer buffer, or parser context, or nothing?
    const TxSourcePosition begin;
    const TxSourcePosition end;

    TxBasicNode( const TxSourceBuffer& buffer, const TxSourcePosition& begin, const TxSourcePosition& end )
            : buffer( buffer ), begin( begin ), end( end ) {}

    std::string_view getSourceText() const {
        return std::string_view( &( buffer.source[begin.index] ), ( end.index - begin.index ));
    }
};


/** (A token can be seen as a simple case of node.) */
class TxToken : public TxBasicNode {
public:
    TxTokenId id;

    TxToken( const TxToken& token ) = default;

    TxToken( const TxSourceBuffer& buffer, const TxSourcePosition& begin, const TxSourcePosition& end, TxTokenId id )
            : TxBasicNode( buffer, begin, end ), id( id ) {}

    std::string str() const override;
};


class TxScanner;

struct TxScopeBlock {
    explicit TxScopeBlock( TxTokenId closingToken ) : closingToken( closingToken ) { }
    std::stack<uint32_t> indentStack;
    TxTokenId closingToken;
};

/** Represents the scanning of a parsing unit. */
class TxSourceScan {
    TxParserContext& parserContext;

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
    explicit TxSourceScan( TxParserContext& parserContext, const TxSourceBuffer& buffer );

    const TxToken& next_token();

    TxParserContext& parser_context() const {
        return parserContext;
    }

    // internal interface; encapsulate?
    std::stack<const TxScanner*> scannerStack;
    std::deque<TxScopeBlock> scopeStacks;

    inline const TxSourcePosition& current_cursor() const {
        return this->cursor;
    }

    inline const char* input_buffer() const {
        return &buffer.source[cursor.index];
    }

    void add_token( TxTokenId id, uint32_t len );

    std::string indent_str() const;
};
