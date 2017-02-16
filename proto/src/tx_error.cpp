#include "tx_error.hpp"
#include "driver.hpp"
#include "context.hpp"



void cerror( const TxParseOrigin* origin, const std::string& msg ) {
    origin->get_parse_location().parserCtx->cerror( origin, msg );
}

void cwarning( const TxParseOrigin* origin, const std::string& msg ) {
    origin->get_parse_location().parserCtx->cwarning( origin, msg );
}

void cerror(const TxParseOrigin& origin, const std::string& msg) {
    cerror( &origin, msg );
}

void cwarning(const TxParseOrigin& origin, const std::string& msg) {
    cwarning( &origin, msg );
}


void finalize_expected_error_clause( const TxParseOrigin* origin ) {
    auto expError = origin->exp_err_ctx();
    if ( origin->exp_err_ctx()->expected_error_count <  0 ) {
        if ( expError->encountered_error_count == 0 )
            CERROR(origin, "COMPILER TEST FAIL: Expected one or more compilation errors but encountered " << expError->encountered_error_count);
    }
    else if ( expError->expected_error_count != expError->encountered_error_count )
        CERROR(origin, "COMPILER TEST FAIL: Expected " << expError->expected_error_count
                       << " compilation errors but encountered " << expError->encountered_error_count);
}
