#pragma clang diagnostic push
#pragma ide diagnostic ignored "bugprone-branch-clone"
#pragma ide diagnostic ignored "cert-err58-cpp"

#include "scanner.hpp"

#include <vector>
#include <string>
#include <cstring>


static inline bool char_in_string( char c, const char* str ) {
    for ( unsigned i = 0; str[i]; i++ ) {
        if ( c == str[i] )
            return true;
    }
    return false;
}

class TxTokenMatcher {
public:
    const std::string pattern;
    const bool definitive;

    explicit TxTokenMatcher( char character, bool definitive = false )
            : pattern( 1, character ), definitive( definitive ) {}

    explicit TxTokenMatcher( const char* pattern, bool definitive = false )
            : pattern( pattern ), definitive( definitive ) {}

    virtual ~TxTokenMatcher() = default;

    /** Returns the possible first byte values for this matcher. */
    virtual std::vector<int8_t> first_bytes() const = 0;

    virtual uint32_t match( const char* source ) const = 0;
};

/** Matches a fixed string. */
class TxFixedMatcher : public TxTokenMatcher {
public:
    explicit TxFixedMatcher( const char* pattern, bool definitive = false )
            : TxTokenMatcher( pattern, definitive ) {}

    std::vector<int8_t> first_bytes() const override {
        return std::vector<int8_t>( { pattern[0] } );
    }

    uint32_t match( const char* source ) const override {
        for ( size_t s = 0; s < pattern.length(); s++ ) {
            if ( pattern[s] != source[s] )
                return 0;
        }
        return pattern.length();
    }
};

/** Matches a string containing one or more of the specified characters, in any order. */
class TxCharSetMatcher : public TxTokenMatcher {
public:
    explicit TxCharSetMatcher( const char* pattern, bool definitive = false )
            : TxTokenMatcher( pattern, definitive ) {}

    std::vector<int8_t> first_bytes() const override {
        return std::vector<int8_t>( pattern.cbegin(), pattern.cend());
    }

    uint32_t match( const char* source ) const override {
        size_t s = 0;
        for ( ; char_in_string( source[s], pattern.c_str()); s++ ) {
        }
        return s;
    }
};

/** Matches a string containing a specific count of of the specified characters, in any order. */
class TxCharSetCountMatcher : public TxTokenMatcher {
    const unsigned minLength, maxLength;
public:
    explicit TxCharSetCountMatcher( const char* pattern, unsigned minLength, unsigned maxLength,
                                    bool definitive = false )
            : TxTokenMatcher( pattern, definitive ), minLength( minLength ), maxLength( maxLength ) {}

    std::vector<int8_t> first_bytes() const override {
        return std::vector<int8_t>( pattern.cbegin(), pattern.cend());
    }

    uint32_t match( const char* source ) const override {
        size_t s = 0;
        for ( ; char_in_string( source[s], pattern.c_str()); s++ ) {
        }
        return ( s >= minLength && s <= maxLength ) ? s : 0;
    }
};

class TxCommentMatcher : public TxTokenMatcher {
public:
    TxCommentMatcher() : TxTokenMatcher( "/*", true ) {}

    std::vector<int8_t> first_bytes() const override {
        return std::vector<int8_t>( { pattern[0] } );
    }

    uint32_t match( const char* source ) const override {
        if ( !( source[0] == '/' && source[1] == '*' ))
            return 0;
        unsigned nestLevel = 1;
        for ( size_t s = 2; true; s++ ) {
            if ( !source[s] ) {
                // end of buffer = end of comment
                return s;
            }
            if ( source[s] == '*' && source[s + 1] == '/' ) {
                // end of a comment level
                if ( --nestLevel == 0 ) {
                    return s + 2;
                }
            }
            if ( source[s] == '/' && source[s + 1] == '*' ) {
                // begin of a comment level
                ++nestLevel;
            }
        }
    }
};

class TxLineCommentMatcher : public TxTokenMatcher {
public:
    TxLineCommentMatcher() : TxTokenMatcher( "##", true ) {}

    std::vector<int8_t> first_bytes() const override {
        return std::vector<int8_t>( { pattern[0] } );
    }

    uint32_t match( const char* source ) const override {
        if ( !( source[0] == '#' && source[1] == '#' ))
            return 0;
        // matched comment start
        for ( size_t s = 2; true; s++ ) {
            if ( !source[s] ) {
                // end of buffer = end of comment
                return s;
            }
            else if ( source[s] == '\n' || source[s] == '\r' ) {
                // end of comment (the newline character is not included in the matched string)
                return s;
            }
        }
    }
};

class TxNameMatcher : public TxTokenMatcher {
public:
    TxNameMatcher() : TxTokenMatcher( "" ) {}

    std::vector<int8_t> first_bytes() const override {
        const std::string chars( "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" );
        return std::vector<int8_t>( chars.cbegin(), chars.cend());
    }

    uint32_t match( const char* source ) const override {
        if ( !( std::isalpha( source[0] ) || source[0] == '_' ))
            return 0;
        for ( size_t s = 1; true; s++ ) {
            if ( !source[s] )  // end of buffer
                return s;
            if ( !( std::isalnum( source[s] )
                    || source[s] == '_'
                    || source[s] == '#' )) {
                return s;
            }
        }
    }
};

class TxIntegerMatcher : public TxTokenMatcher {
public:
    TxIntegerMatcher() : TxTokenMatcher( "BSIL" ) {}

    std::vector<int8_t> first_bytes() const override {
        const std::string chars( "0123456789" );
        return std::vector<int8_t>( chars.cbegin(), chars.cend());
    }

    uint32_t match( const char* source ) const override {
        size_t s = 0;
        if ( !std::isdigit( source[s] ))
            return 0;
        for ( s++; std::isdigit( source[s] ) || source[s] == '_'; s++ ) {
        }
        if ( source[s] == 'U' )
            s++;
        if ( char_in_string( source[s], pattern.c_str()))
            s++;
        return s;
    }
};

class TxRadixIntegerMatcher : public TxTokenMatcher {
public:
    TxRadixIntegerMatcher() : TxTokenMatcher( "BSIL" ) {}

    std::vector<int8_t> first_bytes() const override {
        const std::string chars( "0123456789" );
        return std::vector<int8_t>( chars.cbegin(), chars.cend());
    }

    uint32_t match( const char* source ) const override {
        size_t s;

        if ( source[0] == '0' ) {
            // radix integer literal only starts with 0 if also followed by x
            if ( !( source[1] == 'x' || source[1] == 'X' ))
                return 0;
            // 0x hexadecimal integer
            s = 2;
            if ( !std::isxdigit( source[s] ))
                return 0;
            for ( s++; std::isxdigit( source[s] ) || source[s] == '_'; s++ ) {
            }
        }
        else {
            for ( s = 0; std::isdigit( source[s] ); s++ ) {
            }
            if ( s == 0 )
                return 0;
            if ( source[s] != '#' )
                return 0;
            s++;

            // optional whitespaces and an optional minus sign:
            for ( s++; source[s] == ' ' || source[s] == '\t'; s++ ) {
            }
            if ( source[s] == '-' ) {
                for ( s++; source[s] == ' ' || source[s] == '\t'; s++ ) {
                }
            }

            if ( !std::isalnum( source[s] ))
                return 0;
            for ( s++; std::isalnum( source[s] ) || source[s] == '_'; s++ ) {
            }
        }

        if ( source[s] != '#' )
            return s;
        // type suffix
        s++;
        if ( source[s] == 'U' )
            s++;
        if ( char_in_string( source[s], pattern.c_str()))
            s++;
        return s;
    }
};

class TxFloatMatcher : public TxTokenMatcher {
public:
    TxFloatMatcher() : TxTokenMatcher( "HFD" ) {}

    std::vector<int8_t> first_bytes() const override {
        const std::string chars( "0123456789." );
        return std::vector<int8_t>( chars.cbegin(), chars.cend());
    }

    uint32_t match( const char* source ) const override {
        size_t s = 0;
        if ( std::isdigit( source[s] )) {
            for ( s++; std::isdigit( source[s] ) || source[s] == '_'; s++ ) {
            }
        }
        if ( source[s] != '.' )
            return 0;
        s++;
        if ( std::isdigit( source[s] )) {
            for ( s++; std::isdigit( source[s] ) || source[s] == '_'; s++ ) {
            }
        }
        else if ( s == 1 )
            return 0;  // only '.' without any digits
        else if ( source[s] == '.' )
            return 0;  // prevent conflation with '..' token

        if ( char_in_string( source[s], pattern.c_str()))
            s++;
        return s;
    }
};

class TxExpFloatMatcher : public TxTokenMatcher {
public:
    TxExpFloatMatcher() : TxTokenMatcher( "HFD" ) {}

    std::vector<int8_t> first_bytes() const override {
        const std::string chars( "0123456789." );
        return std::vector<int8_t>( chars.cbegin(), chars.cend());
    }

    uint32_t match( const char* source ) const override {
        size_t s = 0;
        if ( std::isdigit( source[s] )) {
            for ( s++; std::isdigit( source[s] ) || source[s] == '_'; s++ ) {
            }
        }
        if ( source[s] == '.' ) {
            s++;
            if ( std::isdigit( source[s] )) {
                for ( s++; std::isdigit( source[s] ) || source[s] == '_'; s++ ) {
                }
            }
            else if ( s == 1 )
                return 0;  // only '.' without any digits
        }
        else if ( s == 0 )
            return 0;

        if ( source[s] == 'e' || source[s] == 'E' )
            s++;
        else
            return 0;
        if ( source[s] == '+' || source[s] == '-' )
            s++;
        if ( std::isdigit( source[s] )) {
            for ( s++; std::isdigit( source[s] ) || source[s] == '_'; s++ ) {
            }
        }
        else
            return 0;

        if ( char_in_string( source[s], pattern.c_str()))
            s++;
        return s;
    }
};

class TxHexExpFloatMatcher : public TxTokenMatcher {
public:
    TxHexExpFloatMatcher() : TxTokenMatcher( "HFD" ) {}

    std::vector<int8_t> first_bytes() const override {
        const std::string chars( "0123456789." );
        return std::vector<int8_t>( chars.cbegin(), chars.cend());
    }

    uint32_t match( const char* source ) const override {
        if ( !( source[0] == '0' && ( source[1] == 'x' || source[1] == 'X' )))
            return 0;
        size_t s = 2;
        if ( std::isxdigit( source[s] )) {
            for ( s++; std::isxdigit( source[s] ) || source[s] == '_'; s++ ) {
            }
        }
        bool hasDot = false;
        if ( source[s] == '.' ) {
            hasDot = true;
            s++;
            if ( std::isxdigit( source[s] )) {
                for ( s++; std::isxdigit( source[s] ) || source[s] == '_'; s++ ) {
                }
            }
            else if ( s == 1 )
                return 0;  // only '.' without any digits
            else if ( source[s] == '.' )
                return 0;  // prevent conflation with '..' token
        }
        else if ( s == 2 )
            return 0;

        if ( source[s] == 'p' || source[s] == 'P' ) {
            s++;
            if ( source[s] == '+' || source[s] == '-' )
                s++;
            if ( std::isxdigit( source[s] )) {
                for ( s++; std::isxdigit( source[s] ) || source[s] == '_'; s++ ) {
                }
            }
            else
                return 0;
        }
        else if ( !hasDot )  // must have at least one of . and p-expression
            return 0;

        if ( source[s] == '#' ) {
            s++;
            if ( char_in_string( source[s], pattern.c_str()))
                s++;
            else
                return 0;
        }
        return s;
    }
};

class TxStringMatcher : public TxTokenMatcher {
public:
    TxStringMatcher() : TxTokenMatcher( "" ) {}

    explicit TxStringMatcher( char prefix ) : TxTokenMatcher( prefix ) {}

    std::vector<int8_t> first_bytes() const override {
        return std::vector<int8_t>( { '"', 'c' } );
    }

    uint32_t match( const char* source ) const override {
        size_t s = 0;
        if ( !pattern.empty()) {
            if ( source[s] != pattern[0] )  // c-string literal prefix
                return 0;
            s++;
        }
        if ( source[s] != '"' )
            return 0;
        for ( s++;; s++ ) {
            switch ( source[s] ) {
                case 0:
                    return 0;
                case '"':
                    return s + 1;
                case '\\':
                    s++;
                    if ( source[s] == 0 )
                        return 0;
                    break;
                default:
                    break;
            }
        }
    }
};

class TxCharacterMatcher : public TxTokenMatcher {
public:
    TxCharacterMatcher() : TxTokenMatcher( "" ) {}

    std::vector<int8_t> first_bytes() const override {
        return std::vector<int8_t>( { '\'' } );
    }

    uint32_t match( const char* source ) const override {
        if ( source[0] != '\'' )
            return 0;
        else if ( source[1] == '\\' ) {
            if ( source[2] == 0 || source[3] != '\'' )
                return 0;
            switch ( source[2] ) {
                case 'n':
                case 'r':
                case 't':
                    break;
                default:
                    break;
            }
            return 4;
        }
        else {
            if ( source[1] == 0 || source[2] != '\'' )
                return 0;
            return 3;
        }
    }
};

class TxSfWidthMatcher : public TxTokenMatcher {
public:
    TxSfWidthMatcher() : TxTokenMatcher( "" ) {}

    std::vector<int8_t> first_bytes() const override {
        const std::string chars( "123456789" );
        return std::vector<int8_t>( chars.cbegin(), chars.cend());
    }

    uint32_t match( const char* source ) const override {
        if ( source[0] == '*' )
            return 1;
        if ( source[0] == '0' || !std::isdigit( source[0] ))
            return 0;
        size_t s = 1;
        for ( ; std::isdigit( source[s] ); s++ ) {
        }
        return s;
    }
};

class TxSfPrecMatcher : public TxTokenMatcher {
public:
    TxSfPrecMatcher() : TxTokenMatcher( "" ) {}

    std::vector<int8_t> first_bytes() const override {
        return std::vector<int8_t>( { '.' } );
    }

    uint32_t match( const char* source ) const override {
        if ( source[0] != '.' )
            return 0;
        if ( source[1] == '*' )
            return 2;
        if ( !std::isdigit( source[1] ))
            return 0;
        size_t s = 2;
        for ( ; std::isdigit( source[s] ); s++ ) {
        }
        return s;
    }
};


/** Dummy matcher that never matches input. */
class TxNeverMatcher : public TxTokenMatcher {
public:
    TxNeverMatcher() : TxTokenMatcher( "" ) {}

    std::vector<int8_t> first_bytes() const override {
        return std::vector<int8_t>();
    }

    uint32_t match( const char* source ) const override {
        return 0;
    }
};


/** Represents a token definition. */
struct TxTokenDef {
    TxTokenId id;
    TxTokenMatcher const* const matcher;
};

/** Represents a token match. */
struct TxTokenMatch {
    TxTokenId id;
    uint32_t length;
};


/** One scanner container for each context-dependent "scan state".
 * Note that the scanner container object is constant, and can in theory occur multiple times in the scanner  stack.
 */
class TxScanner {
    const std::vector<TxTokenDef>& tokenDefinitions;

    std::vector<const TxTokenDef*> definitionsTable[256];

    inline const std::vector<const TxTokenDef*>* lookup( uint8_t first ) const {
        return &definitionsTable[first];
    }

protected:
    TxTokenMatch match_token( const TxSourceScan& scanState ) const;

public:
    explicit TxScanner( const std::vector<TxTokenDef>& tokenDefinitions ) noexcept
            : tokenDefinitions( tokenDefinitions ) {
        for ( auto& tokDef : this->tokenDefinitions ) {
            for ( uint8_t first : tokDef.matcher->first_bytes()) {
                definitionsTable[first].push_back( &tokDef );
            }
        }
    }

    /** Scans another token. Will add one or more tokens to the tokens vector. (TxTokenID::ERROR if no match) */
    virtual void scan_token( TxSourceScan& scanState ) const = 0;
};

TxTokenMatch TxScanner::match_token( const TxSourceScan& scanState ) const {
    TxTokenMatch match = { TxTokenId::END, 0 };
    for ( const TxTokenDef* tokDef : *lookup( *scanState.input_buffer())) {
        auto newMatchLen = tokDef->matcher->match( scanState.input_buffer());
        if ( newMatchLen > 0 ) {
            if ( tokDef->matcher->definitive ) {
                match.length = newMatchLen;
                match.id = tokDef->id;
                break;
            }
            else if ( newMatchLen > match.length ) {
                match.length = newMatchLen;
                match.id = tokDef->id;
            }
        }
    }
    return match;
}


class TxStringFormatScanner : public TxScanner {
    static const std::vector<TxTokenDef> tokenDefinitions;

public:
    TxStringFormatScanner() : TxScanner( tokenDefinitions ) {}

    void scan_token( TxSourceScan& scanState ) const override;
};

void TxStringFormatScanner::scan_token( TxSourceScan& scanState ) const {
    // match token:
    TxTokenMatch match = match_token( scanState );

    if ( match.length ) {
        // a token matched
        scanState.add_token( match.id, match.length );
        if ( match.id != TxTokenId::SF_TYPE ) {
            return;
        }
    }
    scanState.scannerStack.pop();
}


class TxTopScanner : public TxScanner {
//    static const TxIndentationMatcher indentation_matcher;
    static const std::vector<TxTokenDef> topTokenDefinitions;
    TxStringFormatScanner stringFormatScanner;

public:
    TxTopScanner() : TxScanner( topTokenDefinitions ) {}

    void scan_token( TxSourceScan& scanState ) const override;
};

void TxTopScanner::scan_token( TxSourceScan& scanState ) const {
    // Special logic for indentation:
    // Only match for indentation when at start of line,
    // and if not in a brace / bracket / paren block,
    // and last non-empty line did not end with a COMMA.
    if ( scanState.current_cursor().column == 1
         && ( scanState.scopeStack.back().closingToken == TxTokenId::END
              || scanState.scopeStack.back().closingToken == TxTokenId::DEDENT )
         && scanState.last_non_empty_token_id() != TxTokenId::COMMA ) {
        uint32_t indentLen;
        bool emptyLine;
        {
            auto source = scanState.input_buffer();
            bool mixedIndentChars = false;
            size_t s = 0;
            for ( ; ( source[s] == ' ' || source[s] == '\t' ); s++ ) {
                // check if source buffer uses more than one kind of indentation characters:
                if ( source[s] != scanState.firstUsedIndentChar ) {
                    if ( scanState.firstUsedIndentChar == 0 )
                        scanState.firstUsedIndentChar = source[s];
                    else
                        mixedIndentChars = true;
                }
            }
            // only matches if there are non-whitespace, non-closing characters on the same line:
            // if empty line - treat as insignificant whitespace
            // if rest of line is a comment - treat as insignificant whitespace
            // if start of multi-line comment - treat as insignificant whitespace
            // (FUTURE: difficult to define consistent behavior for multi-line comment case)
            if ( source[s] == 0
                 || source[s] == '\n' || source[s] == '\r'
                 || ( source[s] == '#' && source[s + 1] == '#' )
                 || ( source[s] == '/' && source[s + 1] == '*' )) {
                emptyLine = true;
            }
            else {
                indentLen = s;
                emptyLine = false;
                if ( mixedIndentChars ) {
                    auto& cursor = scanState.current_cursor();
                    TxLocation loc( scanState.parser_context().current_input_filepath(),
                                    cursor.line, cursor.column, &scanState.parser_context());
                    std::ostringstream ostr;
                    ostr << "Mixed spaces and tabs in indentations";
                    scanState.parser_context().cerror( loc, ostr.str());
                }
            }
        }

        if ( !emptyLine ) {
            // when outer block is not an indentation block, do not generate further DEDENTs
            auto prevIndent = scanState.scopeStack.back().indentLength;
            if ( indentLen < prevIndent ) {
                if ( scanState.scopeStack.back().closingToken == TxTokenId::DEDENT ) {
                    auto tokenLen = indentLen;
                    while ( true ) {
                        scanState.add_token( TxTokenId::DEDENT, tokenLen );
                        scanState.scopeStack.pop_back();
//                        std::cerr << "<---  poping scope stack at line " << scanState.current_cursor().line
//                                  << " to depth " << scanState.scopeStack.size()
//                                  << " with indentLen " << indentLen << std::endl;

                        prevIndent = scanState.scopeStack.back().indentLength;
                        if ( indentLen >= prevIndent ) {
                            // check consistency with previous indentation levels (it should match one of the previous lengths exactly):
                            if ( indentLen > prevIndent ) {
                                auto& cursor = scanState.current_cursor();
                                TxLocation loc( scanState.parser_context().current_input_filepath(),
                                                cursor.line, cursor.column, &scanState.parser_context());
                                std::ostringstream ostr;
                                ostr << "Inconsistent indentation at line=" << cursor.line << ",col="
                                     << cursor.column << ", " << indentLen << " > " << prevIndent;
                                scanState.parser_context().cerror( loc, ostr.str());
                            }
                            break;
                        }
                        tokenLen = 0;
                    }
                    return;
                }
            }
            else if ( indentLen > prevIndent ) {
                scanState.scopeStack.emplace_back( indentLen, TxTokenId::DEDENT );
//                std::cerr << "---> pushing scope stack at line " << scanState.current_cursor().line
//                          << " to depth " << scanState.scopeStack.size()
//                          << " with indentLen " << indentLen << std::endl;
                scanState.add_token( TxTokenId::INDENT, indentLen );
                return;
            }
            else if ( indentLen > 0 ) {
                // same indentation as previous, treat as simple whitespace
                scanState.add_token( TxTokenId::WHITESPACE, indentLen );
                return;
            }
        }
    }

    // match token:
    TxTokenMatch match = match_token( scanState );

    if ( match.length ) {  // a token matched
        switch ( match.id ) {
            case TxTokenId::PERCENT:
                scanState.scannerStack.push( &stringFormatScanner );
                break;

            case TxTokenId::LBRACE:
                scanState.scopeStack.emplace_back( 0, TxTokenId::RBRACE );
                break;
            case TxTokenId::LBRACKET:
                scanState.scopeStack.emplace_back( 0, TxTokenId::RBRACKET );
                break;
            case TxTokenId::LPAREN:
                scanState.scopeStack.emplace_back( 0, TxTokenId::RPAREN );
                break;

            case TxTokenId::RBRACE:
            case TxTokenId::RBRACKET:
            case TxTokenId::RPAREN:
                if ( match.id != scanState.scopeStack.back().closingToken ) {
                    TxLocation loc( scanState.parser_context().current_input_filepath(),
                                    scanState.current_cursor().line, scanState.current_cursor().column,
                                    &scanState.parser_context());
                    std::ostringstream ostr;
                    ostr << "Unexpected closing brace / paren / bracket, expected "
                         << scanState.scopeStack.back().closingToken << " but was " << match.id;
                    scanState.parser_context().cerror( loc, ostr.str());
                    break;
                }
                scanState.scopeStack.pop_back();
                break;

            default:
                break;
        }
        scanState.add_token( match.id, match.length );
        return;
    }

    // no token matched
    scanState.add_token( TxTokenId::ERROR, 1 );
}


#define TOKDEF( token, matcher )  { token, matcher }

const std::vector<TxTokenDef> TxTopScanner::topTokenDefinitions =
        {
                TOKDEF( TxTokenId::WHITESPACE, new TxCharSetMatcher( " \t", true )),
                TOKDEF( TxTokenId::COMMENT, new TxCommentMatcher()),
                TOKDEF( TxTokenId::COMMENT, new TxLineCommentMatcher()),

                /* statement separators */
                TOKDEF( TxTokenId::NEWLINE, new TxCharSetMatcher( "\n\r", true )),
                TOKDEF( TxTokenId::SEMICOLON, new TxFixedMatcher( ";", true )),
                TOKDEF( TxTokenId::LBRACE, new TxFixedMatcher( "{", true )),
                TOKDEF( TxTokenId::RBRACE, new TxFixedMatcher( "}", true )),

                /* operators */
                TOKDEF( TxTokenId::LPAREN, new TxFixedMatcher( "(", true )),
                TOKDEF( TxTokenId::RPAREN, new TxFixedMatcher( ")", true )),
                TOKDEF( TxTokenId::LBRACKET, new TxFixedMatcher( "[", true )),
                TOKDEF( TxTokenId::RBRACKET, new TxFixedMatcher( "]", true )),
                TOKDEF( TxTokenId::COMMA, new TxFixedMatcher( ",", true )),
                TOKDEF( TxTokenId::COLON, new TxFixedMatcher( ":" )),
                TOKDEF( TxTokenId::DOT, new TxFixedMatcher( "." )),
                TOKDEF( TxTokenId::DOTDOT, new TxFixedMatcher( ".." )),
                TOKDEF( TxTokenId::ELLIPSIS, new TxFixedMatcher( "..." )),
                TOKDEF( TxTokenId::ASTERISK, new TxFixedMatcher( "*" )),
                TOKDEF( TxTokenId::PLUS, new TxFixedMatcher( "+" )),
                TOKDEF( TxTokenId::MINUS, new TxFixedMatcher( "-" )),
                TOKDEF( TxTokenId::FSLASH, new TxFixedMatcher( "/" )),
                TOKDEF( TxTokenId::BSLASH, new TxFixedMatcher( "\\" )),
                TOKDEF( TxTokenId::AAND, new TxFixedMatcher( "&" )),
                TOKDEF( TxTokenId::PIPE, new TxFixedMatcher( "|" )),
                TOKDEF( TxTokenId::CARET, new TxFixedMatcher( "^" )),
                TOKDEF( TxTokenId::TILDE, new TxFixedMatcher( "~" )),
                TOKDEF( TxTokenId::PERCENT, new TxFixedMatcher( "%" )),
                TOKDEF( TxTokenId::PERCENTPERCENT, new TxFixedMatcher( "%%" )),
                TOKDEF( TxTokenId::DOLLAR, new TxFixedMatcher( "$" )),
                TOKDEF( TxTokenId::QMARK, new TxFixedMatcher( "?" )),
                TOKDEF( TxTokenId::EMARK, new TxFixedMatcher( "!" )),
                TOKDEF( TxTokenId::DASHGT, new TxFixedMatcher( "->" )),
                TOKDEF( TxTokenId::LTCOLON, new TxFixedMatcher( "<:" )),
                TOKDEF( TxTokenId::EQUAL, new TxFixedMatcher( "=" )),
                TOKDEF( TxTokenId::EEQUAL, new TxFixedMatcher( "==" )),
                TOKDEF( TxTokenId::NEQUAL, new TxFixedMatcher( "!=" )),
                TOKDEF( TxTokenId::EEEQUAL, new TxFixedMatcher( "===" )),
                TOKDEF( TxTokenId::NEEQUAL, new TxFixedMatcher( "!==" )),
                TOKDEF( TxTokenId::LT, new TxFixedMatcher( "<" )),
                TOKDEF( TxTokenId::LTLT, new TxFixedMatcher( "<<" )),
                TOKDEF( TxTokenId::GT, new TxFixedMatcher( ">" )),
                // unsure why bison grammar doesn't handle these properly
                //TOKDEF( TxTokenId::GTGT, new TxFixedMatcher( ">>" ) ),
                //TOKDEF( TxTokenId::GTGTGT, new TxFixedMatcher( ">>>" ) ),
                TOKDEF( TxTokenId::LEQUAL, new TxFixedMatcher( "<=" )),
                TOKDEF( TxTokenId::GEQUAL, new TxFixedMatcher( ">=" )),
                TOKDEF( TxTokenId::COLEQUAL, new TxFixedMatcher( ":=" )),
                TOKDEF( TxTokenId::PLUSEQUAL, new TxFixedMatcher( "+=" )),
                TOKDEF( TxTokenId::MINUSEQUAL, new TxFixedMatcher( "-=" )),
                TOKDEF( TxTokenId::ASTERISKEQUAL, new TxFixedMatcher( "*=" )),
                TOKDEF( TxTokenId::FSLASHEQUAL, new TxFixedMatcher( "/=" )),

                /* keywords */
                TOKDEF( TxTokenId::KW_MODULE, new TxFixedMatcher( "module" )),
                TOKDEF( TxTokenId::KW_IMPORT, new TxFixedMatcher( "import" )),
                TOKDEF( TxTokenId::KW_TYPE, new TxFixedMatcher( "type" )),
                TOKDEF( TxTokenId::KW_INTERFACE, new TxFixedMatcher( "interface" )),
                TOKDEF( TxTokenId::KW_BUILTIN, new TxFixedMatcher( "builtin" )),
                TOKDEF( TxTokenId::KW_VIRTUAL, new TxFixedMatcher( "virtual" )),
                TOKDEF( TxTokenId::KW_ABSTRACT, new TxFixedMatcher( "abstract" )),
                TOKDEF( TxTokenId::KW_FINAL, new TxFixedMatcher( "final" )),
                TOKDEF( TxTokenId::KW_OVERRIDE, new TxFixedMatcher( "override" )),
                TOKDEF( TxTokenId::KW_EXTERNC, new TxFixedMatcher( "externc" )),
                TOKDEF( TxTokenId::KW_MUTABLE, new TxFixedMatcher( "mutable" )),
                TOKDEF( TxTokenId::KW_REFERENCE, new TxFixedMatcher( "reference" )),
                TOKDEF( TxTokenId::KW_DERIVES, new TxFixedMatcher( "derives" )),
                TOKDEF( TxTokenId::KW_WHILE, new TxFixedMatcher( "while" )),
                TOKDEF( TxTokenId::KW_FOR, new TxFixedMatcher( "for" )),
                TOKDEF( TxTokenId::KW_IF, new TxFixedMatcher( "if" )),
                TOKDEF( TxTokenId::KW_ELSE, new TxFixedMatcher( "else" )),
                TOKDEF( TxTokenId::KW_IN, new TxFixedMatcher( "in" )),
                TOKDEF( TxTokenId::KW_IS, new TxFixedMatcher( "is" )),
                TOKDEF( TxTokenId::KW_RETURN, new TxFixedMatcher( "return" )),
                TOKDEF( TxTokenId::KW_BREAK, new TxFixedMatcher( "break" )),
                TOKDEF( TxTokenId::KW_CONTINUE, new TxFixedMatcher( "continue" )),
                TOKDEF( TxTokenId::KW_NEW, new TxFixedMatcher( "new" )),
                TOKDEF( TxTokenId::KW_DELETE, new TxFixedMatcher( "delete" )),
                TOKDEF( TxTokenId::KW_XOR, new TxFixedMatcher( "xor" )),
                TOKDEF( TxTokenId::KW_PANIC, new TxFixedMatcher( "panic" )),
                TOKDEF( TxTokenId::KW_ASSERT, new TxFixedMatcher( "assert" )),
                TOKDEF( TxTokenId::KW_EXPERR, new TxFixedMatcher( "#experr" )),

                TOKDEF( TxTokenId::KW__ADDRESS, new TxFixedMatcher( "_address" )),
                TOKDEF( TxTokenId::KW__TYPEID, new TxFixedMatcher( "_typeid" )),
                TOKDEF( TxTokenId::KW__SIZEOF, new TxFixedMatcher( "_sizeof" )),
                TOKDEF( TxTokenId::KW__SUPERTYPES, new TxFixedMatcher( "_supertypes" )),

                /* reserved but not currently used: */
                TOKDEF( TxTokenId::AT, new TxFixedMatcher( "@" )),
                TOKDEF( TxTokenId::EURO, new TxFixedMatcher( "€" )),
                TOKDEF( TxTokenId::COLONGT, new TxFixedMatcher( ":>" )),
                TOKDEF( TxTokenId::KW_PUBLIC, new TxFixedMatcher( "public" )),
                TOKDEF( TxTokenId::KW_PROTECTED, new TxFixedMatcher( "protected" )),
                TOKDEF( TxTokenId::KW_STATIC, new TxFixedMatcher( "static" )),
                TOKDEF( TxTokenId::KW_CONST, new TxFixedMatcher( "const" )),
                TOKDEF( TxTokenId::KW_EXTENDS, new TxFixedMatcher( "extends" )),
                TOKDEF( TxTokenId::KW_IMPLEMENTS, new TxFixedMatcher( "implements" )),
                TOKDEF( TxTokenId::KW_DO, new TxFixedMatcher( "do" )),
                TOKDEF( TxTokenId::KW_SCOPE, new TxFixedMatcher( "scope" )),
                TOKDEF( TxTokenId::KW_SWITCH, new TxFixedMatcher( "switch" )),
                TOKDEF( TxTokenId::KW_CASE, new TxFixedMatcher( "case" )),
                TOKDEF( TxTokenId::KW_WITH, new TxFixedMatcher( "with" )),
                TOKDEF( TxTokenId::KW_AS, new TxFixedMatcher( "as" )),
                TOKDEF( TxTokenId::KW_AND, new TxFixedMatcher( "and" )),
                TOKDEF( TxTokenId::KW_OR, new TxFixedMatcher( "or" )),
                TOKDEF( TxTokenId::KW_NOT, new TxFixedMatcher( "not" )),
                TOKDEF( TxTokenId::KW_TRY, new TxFixedMatcher( "try" )),
                TOKDEF( TxTokenId::KW_EXCEPT, new TxFixedMatcher( "except" )),
                TOKDEF( TxTokenId::KW_FINALLY, new TxFixedMatcher( "finally" )),
                TOKDEF( TxTokenId::KW_RAISE, new TxFixedMatcher( "raise" )),
                TOKDEF( TxTokenId::KW_RAISES, new TxFixedMatcher( "rasises" )),

                /* literals */
                TOKDEF( TxTokenId::LIT_DEC_INT, new TxIntegerMatcher()),
                TOKDEF( TxTokenId::LIT_RADIX_INT, new TxRadixIntegerMatcher()),
                TOKDEF( TxTokenId::LIT_FLOATING, new TxFloatMatcher()),
                TOKDEF( TxTokenId::LIT_FLOATING, new TxExpFloatMatcher()),
                TOKDEF( TxTokenId::LIT_FLOATING, new TxHexExpFloatMatcher()),
                TOKDEF( TxTokenId::LIT_CHARACTER, new TxCharacterMatcher()),
                TOKDEF( TxTokenId::LIT_CSTRING, new TxStringMatcher( 'c' )),
                TOKDEF( TxTokenId::LIT_STRING, new TxStringMatcher()),
                TOKDEF( TxTokenId::ARRAY_LIT, new TxNeverMatcher()),  // TODO

                /* TRUE and FALSE are parsed as keywords until they can be implemented using proper Enum facility */
                TOKDEF( TxTokenId::KW_TRUE, new TxFixedMatcher( "TRUE" )),
                TOKDEF( TxTokenId::KW_FALSE, new TxFixedMatcher( "FALSE" )),

                /* identifiers */
                TOKDEF( TxTokenId::NAME, new TxNameMatcher()),
                TOKDEF( TxTokenId::NAME, new TxFixedMatcher( "#" )),
                TOKDEF( TxTokenId::HASHINIT, new TxFixedMatcher( "#init" )),
                TOKDEF( TxTokenId::HASHSELF, new TxFixedMatcher( "#self" )),
        };

const std::vector<TxTokenDef> TxStringFormatScanner::tokenDefinitions =
        {
                TOKDEF( TxTokenId::SF_PARAM, new TxNeverMatcher()),  // FUTURE
                TOKDEF( TxTokenId::SF_FLAGS, new TxNeverMatcher()),  // FUTURE
                TOKDEF( TxTokenId::SF_MINUS, new TxFixedMatcher( "-" )),
                TOKDEF( TxTokenId::SF_PLUS, new TxFixedMatcher( "+" )),
                TOKDEF( TxTokenId::SF_SPACE, new TxFixedMatcher( " " )),
                TOKDEF( TxTokenId::SF_ZERO, new TxFixedMatcher( "0" )),
                TOKDEF( TxTokenId::SF_HASH, new TxFixedMatcher( "#" )),
                TOKDEF( TxTokenId::SF_WIDTH, new TxSfWidthMatcher()),
                TOKDEF( TxTokenId::SF_PREC, new TxSfPrecMatcher()),
                TOKDEF( TxTokenId::SF_TYPE, new TxCharSetCountMatcher( "xXoObBdiufFeEgGscaA", 1, 1 )),
        };


static TxTopScanner topLevelScanner;


TxSourceScan::TxSourceScan( TxParserContext& parserContext, const TxSourceBuffer& buffer )
        : parserContext( parserContext ), buffer( buffer ), nextToken( 0 ),
          scannerStack( { &topLevelScanner } ), scopeStack( { TxScopeLevel( 0, TxTokenId::END ) } ) {
}

/** moves the head cursor forwards, updating the line index as proper */
void TxSourceScan::advance_head( size_t length ) {
    for ( size_t i = 0; i < length; i++ ) {
        auto nextChar = buffer.source[cursor.index];
        cursor.index++;
        if ( nextChar == '\n' ) {
            lineIndex.line_index.push_back( cursor.index );
            cursor.line++;
            cursor.column = 1;
        }
        else {
            cursor.column++;
        }
    }
}

TxTokenId TxSourceScan::last_non_empty_token_id() const {
    for ( auto it = tokens.crbegin(); it != tokens.crend(); it++ ) {
        switch ( it->id ) {
            case TxTokenId::NEWLINE:
            case TxTokenId::WHITESPACE:
            case TxTokenId::COMMENT:
                break;
            default:
                return it->id;
        }
    }
    return TxTokenId::ERROR;
}

void TxSourceScan::add_token( TxTokenId id, uint32_t len ) {
    // (len can be 0 when multiple tokens are generated at the same position)
    TxSourcePosition begin = cursor;
    advance_head( len );
    tokens.emplace_back( buffer, begin, cursor, id );
}

std::string TxSourceScan::indent_str() const {
    std::ostringstream ostr;
    for ( auto it = this->scopeStack.cbegin(); it != this->scopeStack.cend(); ++it ) {
        if ( it != this->scopeStack.cbegin())
            ostr << "|";
        for ( unsigned i = 0; i < it->indentLength; i++ ) {
            ostr << " ";
        }
    }
    return ostr.str();
}


const TxToken& TxSourceScan::next_token() {
    // first return any queued tokens
    if ( tokens.size() > nextToken ) {
        return tokens.at( nextToken++ );
    }

    // check end of buffer
    if ( !buffer.source[cursor.index] ) {
        // end of buffer
        for ( auto it = scopeStack.crbegin(); it != scopeStack.crend() - 1; it++ ) {
            // produce implicit DEDENTs
            if ( it->closingToken != TxTokenId::DEDENT ) {
                TxLocation loc( parser_context().current_input_filepath(),
                                current_cursor().line, current_cursor().column,
                                &parser_context());
                std::ostringstream ostr;
                ostr << "Missing closing brace / paren / bracket: " << it->closingToken;
                parser_context().cerror( loc, ostr.str());
            }
            tokens.emplace_back( buffer, cursor, cursor, TxTokenId::DEDENT );
        }
        tokens.emplace_back( buffer, cursor, cursor, TxTokenId::END );
        return tokens.at( nextToken++ );
    }

    scannerStack.top()->scan_token( *this );
    auto& token = this->tokens.at( this->nextToken++ );
    return token;
}


std::string TxToken::str() const {
    std::ostringstream ostr;
    ostr << "line=" << begin.line << ",col=" << begin.column << " " << id
         << " \"" << getSourceText() << '"';
    return ostr.str();
}


#pragma clang diagnostic pop
