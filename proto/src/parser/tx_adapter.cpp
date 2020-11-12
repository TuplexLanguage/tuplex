// an adapter between Bison parser and Tx scanner

#include "txparser/scanner.hpp"
#include "tx_options.hpp"

#include "bison_parser.hpp"

class TxParserContext;


yy::TxParser::token_type yylex ( yy::TxParser::semantic_type* yylval, yy::TxParser::location_type* yylloc, TxParserContext* parserCtx ) {
    // FIXME: implement!
    auto& token = parserCtx->scanState->next_token();
    yylval->emplace<std::string>( token.getSourceText() );
    return token.id;
}

int tx_scan(TxParserContext* parserContext, const char* buffer, const TxOptions& options ) {
    std::cout << "Attempting scan of \n\"" << buffer << "\"" << std::endl;
    TxSourceBuffer srcBuffer( { buffer } );
    TxScanState state( srcBuffer );

    do {
        auto token = state.next_token();
        token.print( 0 );
        std::cout << std::endl;
        if ( token.id == TxTokenId::ERROR ) {
            std::cout << "Unrecognized character '" << srcBuffer.source[token.begin.index] << "'" << std::endl;
        } else if ( token.id == TxTokenId::END )
            break;
    } while ( true );

    return 0;
}
