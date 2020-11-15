#pragma once

#include <string>

class TxParserContext;

int parse( TxParserContext* parserContext, const std::string& filePath );

int parse(TxParserContext* parserContext, const char* buffer );
