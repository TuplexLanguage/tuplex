#pragma clang diagnostic push
#pragma ide diagnostic ignored "bugprone-branch-clone"

#include "scanner.hpp"

#include <vector>
#include <string>
#include <cstring>


class TxTokenMatcher {
public:
    const std::string pattern;
    const bool exact;

    explicit TxTokenMatcher( const char* pattern, bool exact = true ) : pattern( pattern ), exact( exact ) {}

    virtual ~TxTokenMatcher() = default;

    virtual size_t match( const char* source ) const = 0;
};

class TxExactMatcher : public TxTokenMatcher {
public:
    explicit TxExactMatcher( const char* pattern ) : TxTokenMatcher( pattern ) {}

    size_t match( const char* source ) const override {
        for ( size_t s = 0; s < pattern.length(); s++ ) {
            if ( pattern[s] != source[s] )
                return 0;
        }
        return pattern.length();
    }
};

class TxCommentMatcher : public TxTokenMatcher {
public:
    const std::string endPattern;

    TxCommentMatcher( const char* startPattern, const char* endPattern ) : TxTokenMatcher( startPattern ),
                                                                           endPattern( endPattern ) {}

    size_t match( const char* source ) const override {
        for ( size_t s = 0; s < pattern.length(); s++ ) {
            if ( pattern[s] != source[s] )
                return 0;
        }
        // matched comment start
        for ( size_t s = pattern.length(); true; s++ ) {
            if ( !source[s] ) {
                // end of buffer = end of comment
                return s;
            } else if ( source[s] == endPattern[0] ) {
                for ( size_t e = 1; e < endPattern.length(); e++ ) {
                    if ( endPattern[e] != source[s + e] ) {
                        goto CONTINUE_COMMENT;
                    }
                }
                // end of comment
                return s + endPattern.length();
            }
            CONTINUE_COMMENT:;
        }
        // TODO: support nested comments
    }
};

class TxLineCommentMatcher : public TxTokenMatcher {
public:  // ( this hardcodes the matched characters - makes macro handling easier)
    TxLineCommentMatcher() : TxTokenMatcher( "##" ) {}

    size_t match( const char* source ) const override {
        for ( size_t s = 0; s < pattern.length(); s++ ) {
            if ( pattern[s] != source[s] )
                return 0;
        }
        // matched comment start
        for ( size_t s = pattern.length(); true; s++ ) {
            if ( !source[s] ) {
                // end of buffer = end of comment
                return s;
            } else if ( source[s] == '\n' || source[s] == '\r' ) {
                // end of comment (the newline character is not included in the matched string)
                return s;
            }
        }
        // TODO: support nested comments
    }
};

class TxIndentationMatcher : public TxTokenMatcher {
public:
    explicit TxIndentationMatcher( const char* pattern ) : TxTokenMatcher( pattern ) {}

    size_t match( const char* source ) const override {
        // only matches if there are non-whitespace characters on the same line
        for ( size_t s = 0; true; s++ ) {
            if ( !source[s] ) {  // end of buffer
                return 0;
            } else if ( pattern.find( source[s] ) == std::string::npos ) {
                if ( source[s] == '\n' || source[s] == '\r' )
                    return 0;  // empty line - treat as insignificant whitespace
                else if ( source[s] == '#' && source[s + 1] == '#' )
                    return 0;  // rest of line is a comment - treat as insignificant whitespace
                else if ( source[s] == '/' && source[s + 1] == '*' )
                    return 0;  // start of comment - treat as insignificant whitespace (FUTURE: difficult to define consistent behavior for this case)
                else
                    return s;
            }
        }
    }
};

class TxWhiteSpaceMatcher : public TxTokenMatcher {
public:
    explicit TxWhiteSpaceMatcher( const char* pattern ) : TxTokenMatcher( pattern ) {}

    size_t match( const char* source ) const override {
        for ( size_t s = 0; true; s++ ) {
            if ( !source[s] ) {  // end of buffer
                return s;
            } else if ( pattern.find( source[s] ) == std::string::npos ) {
                return s;
            }
        }
    }
};

class TxNameMatcher : public TxTokenMatcher {
public:
    TxNameMatcher() : TxTokenMatcher( "", false ) {}

    size_t match( const char* source ) const override {
        for ( size_t s = 0; true; s++ ) {
            if ( !source[s] ) {  // end of buffer
                return s;
            } else if ( !( std::isalpha( source[s] ) || ( s > 0 && std::isdigit( source[s] )))) {
                return s;
            }
        }
    }
};

//class TxIndentMatcher : public TxTokenMatcher {
//public:
//    TxIndentMatcher() : TxTokenMatcher( "", false ) { }
//    size_t match( const char* source ) const override {
//        for ( size_t s = 0; true; s++ ) {
//            if ( !source[s] ) {  // end of buffer
//                return s;
//            }
//            else if ( !( source[s] == ' ' || source[s] == '\t' ) ) {
//                return s;
//            }
//        }
//    }
//};

/** Dummy matcher that never matches input. */
class TxNeverMatcher : public TxTokenMatcher {
public:
    TxNeverMatcher() : TxTokenMatcher( "" ) {}

    size_t match( const char* source ) const override {
        return 0;
    }
};


struct TxTokenDef {
    TxTokenId id;
    char const* const label;
    TxTokenMatcher const* const matcher;
};

#define TOKDEF( token, matcher )  { TxTokenId:: token, #token, matcher }

static TxTokenMatcher const* const indentation_matcher = new TxIndentationMatcher( " \t" );

const std::vector<TxTokenDef> tokenDefinitions = {
        TOKDEF( WHITESPACE, new TxWhiteSpaceMatcher( " \t" )),
        TOKDEF( NEWLINE, new TxWhiteSpaceMatcher( "\n\r" )),

        TOKDEF( LBRACE, new TxExactMatcher( "{" )),
        TOKDEF( RBRACE, new TxExactMatcher( "}" )),
        TOKDEF( LPAREN, new TxExactMatcher( "(" )),
        TOKDEF( RPAREN, new TxExactMatcher( ")" )),
        TOKDEF( SEMICOLON, new TxExactMatcher( ";" )),
        TOKDEF( PACKAGE, new TxExactMatcher( "package" )),

        TOKDEF( COMMENT, new TxCommentMatcher( "/*", "*/" )),
        TOKDEF( COMMENT, new TxLineCommentMatcher()),

        TOKDEF( NAME, new TxNameMatcher()),

//		TOKDEF( INTEGER,	new TxIntegerMatcher( "/*" ) ),
//		TOKDEF( FLOAT, 		new TxFloattMatcher( "*/" ) ),

        // for symbol lookup completeness:
        TOKDEF( INDENT, new TxNeverMatcher()),
        TOKDEF( DEDENT, new TxNeverMatcher()),
        TOKDEF( ERROR, new TxNeverMatcher()),
        TOKDEF( END, new TxNeverMatcher()),
};


/** moves the head cursor forwards, updating the line index as proper */
void TxScanState::advance_head( size_t length ) {
    for ( size_t i = 0; i < length; i++ ) {
        auto nextChar = buffer.source[cursor.index];
        cursor.index++;
        if ( nextChar == '\n' ) {
            lineIndex.line_index.push_back( cursor.index );
            cursor.line++;
            cursor.column = 1;
        } else {
            cursor.column++;
        }
    }
}

const TxToken& TxScanState::next_token() {
    // first return any queued tokens
    if ( tokens.size() > nextToken ) {
        return tokens.at( nextToken++ );
    }

    // check end of buffer
    if ( ! buffer.source[cursor.index] ) {
        // end of buffer
        tokens.emplace_back( buffer, cursor, cursor, TxTokenId::END );
        return tokens.at( nextToken++ );
    }

    // only match for indentation when at start of line
    if ( cursor.column == 1 ) {
        // TODO: warn if mixing tabs and spaces
        const size_t matchLen = indentation_matcher->match( &( buffer.source[cursor.index] ));
        if ( matchLen > 0 ) {
            TxTokenId tokenId;
            size_t prevIndent = ( indentStack.empty() ? 0 : indentStack.top());
            if ( matchLen < prevIndent ) {
                TxSourcePosition begin = cursor;
                advance_head( matchLen );  // only done for the first DEDENT at the same line
                do {
                    tokens.emplace_back( buffer, begin, cursor, TxTokenId::DEDENT );
                    indentStack.pop();
                    std::cout << "DEDENT at line=" << cursor.line << ",col=" << cursor.column << ", "
                              << matchLen << " < " << prevIndent << std::endl;

                    prevIndent = ( indentStack.empty() ? 0 : indentStack.top());
                    if ( matchLen < prevIndent ) {
                        begin = cursor;
                    } else {
                        // check consistency with previous indentation levels (it should match one of the previous lengths exactly):
                        if ( matchLen > prevIndent ) {
                            std::cout << "Inconsistent indentation at line=" << cursor.line << ",col="
                                      << cursor.column << ", " << matchLen << " > " << prevIndent << std::endl;
                        }
                        break;
                    }
                } while ( true );
            } else {
                if ( matchLen > prevIndent ) {
                    indentStack.push( matchLen );
                    tokenId = TxTokenId::INDENT;
                    std::cout << "INDENT at line=" << cursor.line << ",col=" << cursor.column << ", "
                              << matchLen << " > " << prevIndent << std::endl;
                } else {  // matchLen == prevIndent
                    // same indentation as previous, treat as simple whitespace
                    tokenId = TxTokenId::WHITESPACE;
                }
                TxSourcePosition begin = cursor;
                advance_head( matchLen );
                tokens.emplace_back( buffer, begin, cursor, tokenId );
            }
            return tokens.at( nextToken++ );
        }
    }

    // match token:
    const TxTokenDef* matchedTokenDef = nullptr;
    size_t candidateMatchLen = 0;
    for ( auto& tokDef : tokenDefinitions ) {
        size_t matchLen = tokDef.matcher->match( &( buffer.source[cursor.index] ));
        if ( matchLen > 0 ) {
            //std::cout << "candidate match: " << tokDef.label << " matchLen=" << matchLen << std::endl;
            if ( tokDef.matcher->exact ) {
                candidateMatchLen = matchLen;
                matchedTokenDef = &tokDef;
                break;
            } else if ( matchLen > candidateMatchLen ) {
                candidateMatchLen = matchLen;
                matchedTokenDef = &tokDef;
            }
        }
    }

    if ( candidateMatchLen ) {
        // a token matched
        TxSourcePosition begin = cursor;
        advance_head( candidateMatchLen );
        tokens.emplace_back( buffer, begin, cursor, matchedTokenDef->id );
    } else {
        // no token matched
        TxSourcePosition begin = cursor;
        advance_head( 1 );
        tokens.emplace_back( buffer, begin, cursor, TxTokenId::ERROR );
    }
    return tokens.at( nextToken++ );
}


using namespace std;

int scanner_main() {
    const char* srcText = ";() /* * / */\n;##foo\n    bar\n     etc\n    tmp\n bad";
    cout << "Attempting scan of \n\"" << srcText << "\"" << endl;
    TxSourceBuffer srcBuffer( { srcText } );
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

void TxToken::print( int indent ) {
    for ( int i = 0; i < indent; i++ ) {
        std::cout << ' ';
    }
    for ( auto& tokDef : tokenDefinitions ) {
        if ( tokDef.id == id ) {
            std::cout << "line=" << begin.line << ",col=" << begin.column << " " << tokDef.label << " \""
                      << getSourceText() << '"';
            return;
        }
    }
    std::cout << "line=" << begin.line << ",col=" << begin.column << " id=" << id << " \"" << getSourceText() << '"';
}

#pragma clang diagnostic pop
