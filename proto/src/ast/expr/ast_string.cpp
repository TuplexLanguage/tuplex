#include "ast_string.hpp"

#include "ast_lit.hpp"
#include "ast/type/ast_types.hpp"

static std::string parse_string( const std::string source ) {
    std::string result;
    // note: skips leading and trailing " characters
    for ( auto it = next( source.cbegin() ); it != prev( source.cend() ); ++it ) {
        if ( *it == '\\' ) {
            ++it;
            switch ( *it ) {
            case '\n':
                case '\r':
                // skip EOL preceded by escape character
                break;
            case 'n':
                result.push_back( '\n' );
                break;
            case 'r':
                result.push_back( '\r' );
                break;
            case 't':
                result.push_back( '\t' );
                break;
            default:
                // all other characters are included as-is
                //case '\\':
                //case '"':
                result.push_back( *it );
                break;
            }
        }
        else {
            result.push_back( *it );
        }
    }
    return result;
}

static unsigned iso_8859_1_to_utf8( const std::string& input, uint8_t *output ) {
    unsigned i = 0;
    for ( auto chr : input ) {
        if ( chr < 0x80 ) {
            output[i++] = chr;
        }
        else {
            output[i++] = 0xc0 | ( chr >> 6 );
            output[i++] = 0x80 | ( chr & 0x3f );
        }
    }
    return i;
}

static std::vector<uint8_t> iso_8859_1_to_utf8( const std::string& input ) {
    uint8_t byteArray[input.size() * 2];
    size_t len = iso_8859_1_to_utf8( input, byteArray );
    return std::vector<uint8_t>( byteArray, byteArray + len );
}

TxStringLitNode::TxStringLitNode( const TxLocation& ploc, const std::string& literal )
        : TxExpressionNode( ploc ), utf8data( iso_8859_1_to_utf8( parse_string( literal ) ) ),
          arrayTypeNode( new TxArrayTypeNode( ploc, new TxNamedTypeNode( ploc, "tx.UByte" ),
                                              new TxIntegerLitNode( ploc, utf8data.size(), false, TXBT_UINT ) ) ),
          literal( literal ) {
}


TxConcatenateStringsNode::TxConcatenateStringsNode( const TxLocation& ploc, const std::vector<TxExpressionNode*>& stringNodes )
        : TxExpressionNode( ploc ), stringNodes( stringNodes ) {
}



TxTypeExpressionNode* TxCStringLitNode::make_cstring_type_expr( const TxLocation& ploc, const std::string& literal ) {
    // (for now) Create AST to declare the implicit type of this c-string literal:
    TxTypeExpressionNode* elemTypeExpr = new TxNamedTypeNode( ploc, "tx.UByte" );
    TxExpressionNode* capExpr = new TxIntegerLitNode( ploc, literal.length() - 2, false, TXBT_UINT );
    TxTypeExpressionNode* typeExpr = new TxArrayTypeNode( ploc, elemTypeExpr, capExpr );
    return typeExpr;
}
