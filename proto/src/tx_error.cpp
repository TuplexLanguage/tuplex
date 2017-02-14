#include "tx_error.hpp"
#include "driver.hpp"



void cerror(const TxParseOrigin* origin, const std::string& msg) {
    origin->get_parse_location().parserCtx->cerror(origin->get_parse_location(), msg);
}

void cwarning(const TxParseOrigin* origin, const std::string& msg) {
    origin->get_parse_location().parserCtx->cwarning(origin->get_parse_location(), msg);
}

void cerror(const TxLocation& location, const std::string& msg) {
    location.parserCtx->cerror(location, msg);
}
void cwarning(const TxLocation& location, const std::string& msg) {
    location.parserCtx->cwarning(location, msg);
}
