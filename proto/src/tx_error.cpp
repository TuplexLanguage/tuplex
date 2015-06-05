#include "tx_error.hpp"
#include "driver.hpp"


const yy::location NULL_LOC = yy::location(nullptr, 0, 0);


void cerror(const TxParseOrigin* origin, const std::string& msg) {
    if (origin->get_driver())
        origin->get_driver()->cerror(origin->get_parse_location(), msg);
    else
        TxDriver::emit_comp_error(origin->get_parse_location(), msg);
}

void cwarning(const TxParseOrigin* origin, const std::string& msg) {
    if (origin->get_driver())
        origin->get_driver()->cwarning(origin->get_parse_location(), msg);
    else
        TxDriver::emit_comp_warning(origin->get_parse_location(), msg);
}

void cerror(TxDriver* driver, const std::string& msg) {
    if (driver)
        driver->cwarning(NULL_LOC, msg);
    else
        TxDriver::emit_comp_warning(NULL_LOC, msg);
}
void cwarning(TxDriver* driver, const std::string& msg) {
    if (driver)
        driver->cwarning(NULL_LOC, msg);
    else
        TxDriver::emit_comp_warning(NULL_LOC, msg);
}
