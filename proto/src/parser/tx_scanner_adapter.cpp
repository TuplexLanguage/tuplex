// an adapter between Bison parser and Tx scanner

#include "txparser/scanner.hpp"
#include "tx_options.hpp"

#include "bison_parser.hpp"
#include "driver.hpp"

class TxParserContext;


yy::TxParser::token_type yylex( yy::TxParser::semantic_type* yylval, yy::TxParser::location_type* yylloc, TxParserContext* parserCtx ) {
    do {
        yylloc->step();
        ASSERT( yylloc->begin.line == parserCtx->scanState->current_cursor().line,
                "Mismatching lines: " << yylloc->begin.line << " != " << parserCtx->scanState->current_cursor().line );
        ASSERT( yylloc->begin.column == parserCtx->scanState->current_cursor().column,
                "Mismatching columns: " << yylloc->begin.column << " != " << parserCtx->scanState->current_cursor().column );
        auto& token = parserCtx->scanState->next_token();
        switch( token.id ) {
            // translate / filter certain tokens
            case TxTokenId::WHITESPACE:
                yylloc->columns( token.getSourceText().length() );
                break;
            case TxTokenId::NEWLINE:
                yylloc->lines( token.getSourceText().length() );
                break;
            case TxTokenId::COMMENT:
                if ( token.end.line > token.begin.line ) {
                    yylloc->lines( token.end.line - token.begin.line );
                    if ( token.end.column > 1 )
                        yylloc->columns( token.end.column - 1 );
                }
                else
                    yylloc->columns( token.getSourceText().length() );
                break;
            case TxTokenId::INDENT:
            case TxTokenId::DEDENT:
                yylloc->columns( token.getSourceText().length() );
                break;
//            case TxTokenId::INDENT:
//                yylloc->columns( token.getSourceText().length() );
//                yylval->emplace<std::string>( token.getSourceText() );
//                return TxTokenId::LBRACE;
//            case TxTokenId::DEDENT:
//                yylloc->columns( token.getSourceText().length() );
//                yylval->emplace<std::string>( token.getSourceText() );
//                return TxTokenId::RBRACE;

            default:
                if ( parserCtx->driver().get_options().debug_scanner ) {
                    std::cerr << "Returned token: ";
                    token.print( 0 );
                    std::cerr << std::endl;
                }
                yylloc->columns( token.getSourceText().length());
                yylval->emplace<std::string>( token.getSourceText());
                return token.id;
        }
    } while ( true );
}
