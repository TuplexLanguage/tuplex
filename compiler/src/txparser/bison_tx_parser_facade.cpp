//
// Created by christer on 11.11.20.
//

#include "parser_if.hpp"

#include "driver.hpp"

// bison's generated header file:
#include "bison_parser.hpp"


static Logger& _LOG = Logger::get( "B-PARSER" );


int parse( TxParserContext* parserContext ) {
    yy::TxParser parser( parserContext );
    parser.set_debug_level( parserContext->driver().get_options().debug_parser );
    int ret = parser.parse();
    _LOG.debug( "Completed grammar parse of source buffer for '%s'",
                parserContext->source_filepath()->c_str() );
    return ret;
}
