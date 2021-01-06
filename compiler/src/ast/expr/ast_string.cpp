#include "ast_string.hpp"

#include "ast_lit.hpp"
#include "ast/type/ast_types.hpp"

std::string parse_string_literal( const std::string& source, unsigned startOffset, unsigned endOffset ) {
    std::string result;
    for ( auto it = source.cbegin()+startOffset; it != source.cend()-endOffset; ++it ) {
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
            case '0':
                result.push_back( '\0' );
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
        if ( chr <= 0x7F ) {
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
        : TxExpressionNode( ploc ),
          utf8data( iso_8859_1_to_utf8( parse_string_literal( literal, 1, 1 ))),
          stringTypeNode( new TxNamedTypeNode( ploc, "tx.String" )),
          arrayTypeNode( new TxArrayTypeNode( ploc, new TxNamedTypeNode( ploc, "tx.UByte" ),
                                              new TxIntegerLitNode( ploc, utf8data.size(), false, TXBT_UINT ))),
          literal( literal ) {
}


TxConcatenateStringsNode::TxConcatenateStringsNode( const TxLocation& ploc, const std::vector<TxExpressionNode*>& stringNodes )
        : TxExpressionNode( ploc ), stringNodes( stringNodes ) {
}



static TxTypeExpressionNode* make_cstring_type_expr( const TxLocation& ploc, unsigned arrayCap ) {
    // (for now) Create AST to declare the implicit type of this c-string literal:
    TxTypeExpressionNode* elemTypeExpr = new TxNamedTypeNode( ploc, "tx.UByte" );
    TxExpressionNode* capExpr = new TxIntegerLitNode( ploc, arrayCap, false, TXBT_UINT );
    TxTypeExpressionNode* typeExpr = new TxArrayTypeNode( ploc, elemTypeExpr, capExpr );
    return typeExpr;
}

TxCStringLitNode::TxCStringLitNode( const TxLocation& ploc, const std::string& literal )
        : TxExpressionNode( ploc ),
          literal( literal ),
          value( parse_string_literal( literal, 2, 1 ) ),
          cstringTypeNode( make_cstring_type_expr( ploc, this->value.length() + 1 ) ) {
}
