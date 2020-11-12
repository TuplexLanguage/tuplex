#pragma once

#include <cstdio>

class TxOptions;
class TxParserContext;

int parse( TxParserContext* parserContext, FILE* file, const TxOptions& options );

int parse(TxParserContext* parserContext, const char* buffer, const TxOptions& options );
