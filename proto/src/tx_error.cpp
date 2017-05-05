#include "tx_error.hpp"
#include "driver.hpp"
#include "context.hpp"

ScopedExpErrClause::ScopedExpErrClause( TxParseOrigin* origin, bool enabled )
        : origin( origin ), enabled( enabled ) {
    if ( this->enabled )
        origin->get_parse_location().parserCtx->begin_exp_err( origin );
}

ScopedExpErrClause::~ScopedExpErrClause() {
    if ( this->enabled )
        origin->get_parse_location().parserCtx->end_exp_err( origin->get_parse_location() );
}

void cerror( const TxParseOrigin* origin, const std::string& msg ) {
    origin->get_parse_location().parserCtx->cerror( origin, msg );
}

void cwarning( const TxParseOrigin* origin, const std::string& msg ) {
    origin->get_parse_location().parserCtx->cwarning( origin, msg );
}

void cerror( const TxParseOrigin& origin, const std::string& msg ) {
    cerror( &origin, msg );
}

void cwarning( const TxParseOrigin& origin, const std::string& msg ) {
    cwarning( &origin, msg );
}

void finalize_expected_error_clause( const TxParseOrigin* origin ) {
    auto expError = origin->exp_err_ctx();
    // Note: Can't use origin directly in these error messages since origin holds an ExpectedErrorContext.
    if ( expError->expected_error_count < 0 ) {
        if ( expError->encountered_error_count == 0 ) {
            std::stringstream msg;
            msg << "COMPILER TEST FAIL: Expected one or more compilation errors but encountered " << expError->encountered_error_count;
            origin->get_parse_location().parserCtx->cerror( origin->get_parse_location(), msg.str() );
        }
    }
    else if ( expError->expected_error_count != expError->encountered_error_count ) {
        std::stringstream msg;
        msg << "COMPILER TEST FAIL: Expected " << expError->expected_error_count << " compilation errors but encountered "
            << expError->encountered_error_count;
        origin->get_parse_location().parserCtx->cerror( origin->get_parse_location(), msg.str() );
    }
}
