#pragma once

class TxOptions;
class TxParserContext;

int parse( TxParserContext* parserContext, const TxOptions& options );

int parse_mem_buffer( TxParserContext* parserContext, const char* source_buffer, const TxOptions& options );
