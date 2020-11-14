#pragma clang diagnostic push
#pragma ide diagnostic ignored "bugprone-branch-clone"
#pragma ide diagnostic ignored "cert-err58-cpp"

#include "scanner.hpp"

#include <vector>
#include <string>
#include <cstring>


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
        for ( size_t s = 0; true; s++ ) {
            if ( !source[s] ) {  // end of buffer
                return s;
            }
            else if ( pattern.find( source[s] ) == std::string::npos ) {
                return s;
            }
        }
    }
};

class TxCommentMatcher : public TxTokenMatcher {
public:
    const std::string endPattern;

    TxCommentMatcher( const char* startPattern, const char* endPattern )
            : TxTokenMatcher( startPattern, true ), endPattern( endPattern ) {
        ASSERT( this->pattern.length() == 2, "implementation requires a prefix length of 2" );
        ASSERT( this->endPattern.length() == 2, "implementation requires a suffix length of 2" );
    }

    std::vector<int8_t> first_bytes() const override {
        return std::vector<int8_t>( { pattern[0] } );
    }

    uint32_t match( const char* source ) const override {
        if ( !( source[0] == pattern[0] && source[1] == pattern[1] ))
            return 0;
        unsigned nestLevel = 1;
        for ( size_t s = 2; true; s++ ) {
            if ( !source[s] ) {
                // end of buffer = end of comment
                return s;
            }
            if ( source[s] == endPattern[0] && source[s + 1] == endPattern[1] ) {
                // end of a comment level
                if ( --nestLevel == 0 ) {
                    return s + endPattern.length();
                }
            }
            if ( source[s] == pattern[0] && source[s + 1] == pattern[1] ) {
                // begin of a comment level
                ++nestLevel;
            }
        }
    }
};

class TxLineCommentMatcher : public TxTokenMatcher {
public:  // ( this hardcodes the matched characters - makes macro handling easier)
    TxLineCommentMatcher() : TxTokenMatcher( "##" ) {
        ASSERT( this->pattern.length() == 2, "implementation requires a prefix length of 2" );
    }

    std::vector<int8_t> first_bytes() const override {
        return std::vector<int8_t>( { pattern[0] } );
    }

    uint32_t match( const char* source ) const override {
        if ( !( source[0] == pattern[0] && source[1] == pattern[1] ))
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
        // TODO: support nested comments
    }
};

class TxIndentationMatcher : public TxTokenMatcher {
public:
    explicit TxIndentationMatcher( const char* pattern ) noexcept
            : TxTokenMatcher( pattern, true ) {}

    std::vector<int8_t> first_bytes() const override {
        return std::vector<int8_t>( pattern.cbegin(), pattern.cend());
    }

    uint32_t match( const char* source ) const override {
        // only matches if there are non-whitespace characters on the same line
        for ( size_t s = 0; true; s++ ) {
            if ( !source[s] ) {  // end of buffer
                return 0;
            }
            else if ( pattern.find( source[s] ) == std::string::npos ) {
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

class TxNameMatcher : public TxTokenMatcher {
public:
    TxNameMatcher() : TxTokenMatcher( "" ) {}

    std::vector<int8_t> first_bytes() const override {
        const std::string chars( "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" );
        return std::vector<int8_t>( chars.cbegin(), chars.cend());
    }

    uint32_t match( const char* source ) const override {
        for ( size_t s = 0; true; s++ ) {
            if ( !source[s] ) {  // end of buffer
                return s;
            }
            else if ( !( std::isalpha( source[s] )
                         || source[s] == '_'
                         || ( s > 0 && std::isdigit( source[s] )))) {
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
        if ( ! std::isdigit( source[s] ) )
            return 0;
        for ( s++; std::isdigit( source[s] ) || source[s] == '_'; s++ ) {
        }
        if ( source[s] == 'U' )
            s++;
        if ( pattern.find( source[s] ) != std::string::npos )
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
        if ( pattern.find( source[s] ) != std::string::npos )
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
        if ( std::isdigit( source[s] ) ) {
            for ( s++; std::isdigit( source[s] ) || source[s] == '_'; s++ ) {
            }
        }
        if ( source[s] != '.' )
            return 0;
        s++;
        if ( std::isdigit( source[s] ) ) {
            for ( s++; std::isdigit( source[s] ) || source[s] == '_'; s++ ) {
            }
        }
        if ( s == 1 )
            return 0;  // only '.' without any digits
        if ( pattern.find( source[s] ) != std::string::npos )
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
        if ( std::isdigit( source[s] ) ) {
            for ( s++; std::isdigit( source[s] ) || source[s] == '_'; s++ ) {
            }
        }
        if ( source[s] == '.' ) {
            s++;
            if ( std::isdigit( source[s] ) ) {
                for ( s++; std::isdigit( source[s] ) || source[s] == '_'; s++ ) {
                }
            }
            if ( s == 1 )
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
        if ( std::isdigit( source[s] ) ) {
            for ( s++; std::isdigit( source[s] ) || source[s] == '_'; s++ ) {
            }
        }
        else
            return 0;

        if ( pattern.find( source[s] ) != std::string::npos )
            s++;
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

    std::vector<int8_t> first_bytes() const override {  // FIXME
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
    std::cerr << "Exiting scanner stringFormatScanner" << std::endl;
    scanState.scannerStack.pop();
}


class TxTopScanner : public TxScanner {
    static const TxIndentationMatcher indentation_matcher; //( " \t" );
    static const std::vector<TxTokenDef> topTokenDefinitions;
    TxStringFormatScanner stringFormatScanner;

public:
    TxTopScanner() : TxScanner( topTokenDefinitions ) {}

    void scan_token( TxSourceScan& scanState ) const override;
};

void TxTopScanner::scan_token( TxSourceScan& scanState ) const {
    // Special logic for indentation. Only match for indentation when at start of line.
    if ( scanState.current_cursor().column == 1 ) {
        // TODO: warn if mixing tabs and spaces
        const auto indentLen = indentation_matcher.match( scanState.input_buffer());
        if ( indentLen > 0 ) {
            TxTokenId tokenId;
            size_t prevIndent = ( scanState.indentStack.empty() ? 0 : scanState.indentStack.top());
            if ( indentLen < prevIndent ) {
                scanState.add_token( TxTokenId::DEDENT, indentLen );
                do {
                    scanState.indentStack.pop();

                    prevIndent = ( scanState.indentStack.empty() ? 0 : scanState.indentStack.top());
                    if ( indentLen >= prevIndent ) {
                        // check consistency with previous indentation levels (it should match one of the previous lengths exactly):
                        if ( indentLen > prevIndent ) {
                            auto& cursor = scanState.current_cursor();
                            std::cerr << "Inconsistent indentation at line=" << cursor.line << ",col="
                                      << cursor.column << ", " << indentLen << " > " << prevIndent << std::endl;
                        }
                        break;
                    }
                    scanState.add_token( TxTokenId::DEDENT, 0 );
                } while ( true );
            }
            else {
                if ( indentLen > prevIndent ) {
                    scanState.indentStack.push( indentLen );
                    tokenId = TxTokenId::INDENT;
                }
                else {  // matchLen == prevIndent
                    // same indentation as previous, treat as simple whitespace
                    tokenId = TxTokenId::WHITESPACE;
                }
                scanState.add_token( tokenId, indentLen );
            }
            return;
        }
    }

    // match token:
    TxTokenMatch match = match_token( scanState );

    if ( match.length ) {
        // a token matched
        scanState.add_token( match.id, match.length );
        if ( match.id == TxTokenId::PERCENT ) {
            std::cerr << "Entering scanner stringFormatScanner" << std::endl;
            scanState.scannerStack.push( &stringFormatScanner );
        }
        return;
    }

    // no token matched
    scanState.add_token( TxTokenId::ERROR, 1 );
}


const TxIndentationMatcher TxTopScanner::indentation_matcher( " \t" );

#define TOKDEF( token, matcher )  { token, matcher }

const std::vector<TxTokenDef> TxTopScanner::topTokenDefinitions =
        {
                TOKDEF( TxTokenId::WHITESPACE, new TxCharSetMatcher( " \t", true )),
                TOKDEF( TxTokenId::COMMENT, new TxCommentMatcher( "/*", "*/" )),
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

//                /* precedence operators, not actually lexically produced */
//                TOKDEF( TxTokenId::STMT, new TxNeverMatcher()),
//                TOKDEF( TxTokenId::TYPE, new TxNeverMatcher()),
//                TOKDEF( TxTokenId::EXPR, new TxNeverMatcher()),
//                TOKDEF( TxTokenId::NOT, new TxNeverMatcher()),
//                TOKDEF( TxTokenId::NEG, new TxNeverMatcher()),
//                TOKDEF( TxTokenId::ADDR, new TxNeverMatcher()),
//
//                /* for symbol lookup completeness, matched separately */
//                TOKDEF( TxTokenId::INDENT, new TxNeverMatcher()),
//                TOKDEF( TxTokenId::DEDENT, new TxNeverMatcher()),
//
//                /* for symbol lookup completeness, not actually lexically produced */
//                TOKDEF( TxTokenId::ERROR, new TxNeverMatcher()),
//                TOKDEF( TxTokenId::END, new TxNeverMatcher()),
        };

const std::vector<TxTokenDef> TxStringFormatScanner::tokenDefinitions =
        {
                TOKDEF( TxTokenId::SF_PARAM, new TxNeverMatcher()),  // FUTURE
                TOKDEF( TxTokenId::SF_FLAGS, new TxNeverMatcher()),  // FUTURE
                TOKDEF( TxTokenId::SF_PREC, new TxNeverMatcher()),  // TODO
                TOKDEF( TxTokenId::SF_TYPE, new TxCharSetMatcher( "xXoObBdiufFeEgGscaA" )),  // TODO: only match single char
                TOKDEF( TxTokenId::SF_WIDTH, new TxCharSetMatcher( "0123456789" )),  // TODO: not match initial 0
                TOKDEF( TxTokenId::SF_WIDTH, new TxFixedMatcher( "*" )),
                TOKDEF( TxTokenId::SF_MINUS, new TxFixedMatcher( "-" )),
                TOKDEF( TxTokenId::SF_PLUS, new TxFixedMatcher( "+" )),
                TOKDEF( TxTokenId::SF_SPACE, new TxFixedMatcher( " " )),
                TOKDEF( TxTokenId::SF_ZERO, new TxFixedMatcher( "0" )),
                TOKDEF( TxTokenId::SF_HASH, new TxFixedMatcher( "#" )),
        };


static TxTopScanner topLevelScanner;


TxSourceScan::TxSourceScan( const TxSourceBuffer& buffer )
        : buffer( buffer ), nextToken( 0 ), scannerStack( { &topLevelScanner } ) {
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

void TxSourceScan::add_token( TxTokenId id, uint32_t len ) {
    // (len can be 0 when multiple tokens are generated at the same position)
    TxSourcePosition begin = cursor;
    advance_head( len );
    tokens.emplace_back( buffer, begin, cursor, id );
}


const TxToken& TxSourceScan::next_token() {
    // first return any queued tokens
    if ( tokens.size() > nextToken ) {
        return tokens.at( nextToken++ );
    }

    // check end of buffer
    if ( !buffer.source[cursor.index] ) {
        // end of buffer
        tokens.emplace_back( buffer, cursor, cursor, TxTokenId::END );
        return tokens.at( nextToken++ );
    }

    scannerStack.top()->scan_token( *this );
    auto& token = this->tokens.at( this->nextToken++ );
    return token;
}


void TxToken::print( int indent ) const {
    for ( int i = 0; i < indent; i++ ) {
        std::cerr << ' ';
    }
    std::cerr << "line=" << begin.line << ",col=" << begin.column << " " << id
              << " \"" << getSourceText() << '"';
}


#pragma clang diagnostic pop
