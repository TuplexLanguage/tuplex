// an adapter between Bison parser and Tx scanner

#include "tx_options.hpp"

#include "bison_parser.hpp"
#include "driver.hpp"
#include "txparser/scanner.hpp"


static TxToken inner_yylex( yy::TxParser::semantic_type* yylval,
                              yy::TxParser::location_type* yylloc,
                              TxParserContext* parserCtx ) {
    // TODO: Make reentrant
    static TxTokenId lastTokenId = TxTokenId::END;
    static uint32_t lastTokenLine = 0;
    do {
        yylloc->step();
        ASSERT( yylloc->begin.line == parserCtx->scanCtx->current_cursor().line,
                "Mismatching lines: " << yylloc->begin.line << " != " << parserCtx->scanCtx->current_cursor().line );
        ASSERT( yylloc->begin.column == parserCtx->scanCtx->current_cursor().column,
                "Mismatching columns: " << yylloc->begin.column << " != " << parserCtx->scanCtx->current_cursor().column );
        auto& token = parserCtx->scanCtx->next_token();
        switch ( token.id ) {
            // translate / filter certain tokens
            case TxTokenId::WHITESPACE:
                yylloc->columns( token.getSourceText().length());
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

            case TxTokenId::NEWLINE:
                yylloc->lines( token.getSourceText().length());
                // Any sequence of WHITESPACE, COMMENT mixed with at least one NEWLINE shall return a single NEWLINE.
                // These NEWLINE tokens are then filtered based on context and only passed on if:
                //   not in a bracket / paren block
                //   not on the same line as certain other non-whitespace tokens
                if ( parserCtx->scanCtx->scopeStacks.back().closingToken != TxTokenId::RPAREN
                     && parserCtx->scanCtx->scopeStacks.back().closingToken != TxTokenId::RBRACKET ) {
                    if ( lastTokenLine == token.begin.line ) {
                        switch ( lastTokenId ) {
                            case TxTokenId::LBRACE:
                            case TxTokenId::LBRACKET:
                            case TxTokenId::LPAREN:
                            case TxTokenId::COLON:
                            case TxTokenId::COMMA:
                            case TxTokenId::SEMICOLON:
                            case TxTokenId::WHITESPACE:
                            case TxTokenId::KW_ELSE:  // since colon after else is optional
                                break;
                            case TxTokenId::RBRACE:
                                if ( parserCtx->scanCtx->nl_after_rbrace() )
                                    parserCtx->scanCtx->nl_after_rbrace( false );
                                else
                                    break;
                            default:
                                lastTokenId = token.id;  // NEWLINE
                                return token;
                        }
                    }
                }
                break;

            default:
                yylloc->columns( token.getSourceText().length());
                yylval->emplace<std::string>( token.getSourceText());
                lastTokenId = token.id;
                lastTokenLine = token.end.line;
                return token;
        }
    } while ( true );
}

yy::TxParser::token_type yylex( yy::TxParser::semantic_type* yylval, yy::TxParser::location_type* yylloc, TxParserContext* parserCtx ) {
    auto token = inner_yylex( yylval, yylloc, parserCtx );
    if ( parserCtx->driver().get_options().debug_scanner /* && parserCtx->is_user_source() */ ) {
        std::cerr << "Returned token: " << token.str().c_str() << std::endl;
        switch ( token.id ) {
            case TxTokenId::LBRACE:
            case TxTokenId::LBRACKET:
            case TxTokenId::LPAREN:
            case TxTokenId::RBRACE:
            case TxTokenId::RBRACKET:
            case TxTokenId::RPAREN:
                std::cerr << parserCtx->scanCtx->indent_str() << token.getSourceText() << std::endl;
                break;
            case TxTokenId::INDENT:
            case TxTokenId::DEDENT:
                std::cerr << parserCtx->scanCtx->indent_str() << "|" << std::endl;
                break;
            default:
                break;
        }
    }
    return token.id;
}
