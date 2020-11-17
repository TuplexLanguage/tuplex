// an adapter between Bison parser and Tx scanner

#include "tx_options.hpp"

#include "bison_parser.hpp"
#include "driver.hpp"
#include "txparser/scanner.hpp"

class TxParserContext;

yy::TxParser::token_type yylex( yy::TxParser::semantic_type* yylval, yy::TxParser::location_type* yylloc, TxParserContext* parserCtx ) {
    do {
        yylloc->step();
        ASSERT( yylloc->begin.line == parserCtx->scanState->current_cursor().line,
                "Mismatching lines: " << yylloc->begin.line << " != " << parserCtx->scanState->current_cursor().line );
        ASSERT( yylloc->begin.column == parserCtx->scanState->current_cursor().column,
                "Mismatching columns: " << yylloc->begin.column << " != " << parserCtx->scanState->current_cursor().column );
        auto& token = parserCtx->scanState->next_token();
        switch ( token.id ) {
            // translate / filter certain tokens
            case TxTokenId::WHITESPACE:
                yylloc->columns( token.getSourceText().length());
                break;
            case TxTokenId::NEWLINE:
                yylloc->lines( token.getSourceText().length());
                break;
            case TxTokenId::COMMENT:
                if ( token.end.line > token.begin.line ) {
                    yylloc->lines( token.end.line - token.begin.line );
                    if ( token.end.column > 1 )
                        yylloc->columns( token.end.column - 1 );
                }
                else
                    yylloc->columns( token.getSourceText().length());
                break;

            default:
                if ( parserCtx->driver().get_options().debug_scanner /* && parserCtx->is_user_source() */ ) {
                    std::cerr << "Returned token: " << token.str().c_str() << std::endl;
                    switch ( token.id ) {
                        case TxTokenId::LBRACE:
                        case TxTokenId::LBRACKET:
                        case TxTokenId::LPAREN:
                        case TxTokenId::RBRACE:
                        case TxTokenId::RBRACKET:
                        case TxTokenId::RPAREN:
                            std::cerr << parserCtx->scanState->indent_str() << token.getSourceText() << std::endl;
                            break;
                        case TxTokenId::INDENT:
                        case TxTokenId::DEDENT:
                            std::cerr << parserCtx->scanState->indent_str() << "|" << std::endl;
                            break;
                        default:
                            break;
                    }
                }

                yylloc->columns( token.getSourceText().length());
                yylval->emplace<std::string>( token.getSourceText());
                return token.id;
        }
    } while ( true );
}
