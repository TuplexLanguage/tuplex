#pragma once

#include <cstdio>
#include <string>

class TxOptions;
class TxParserContext;

int parse( TxParserContext* parserContext, const std::string& filePath );

int parse(TxParserContext* parserContext, const char* buffer );
