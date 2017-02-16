#pragma once

#include <iostream>
#include <sstream>

#include "location.hpp"


struct ExpectedErrorClause {
    const int expected_error_count;
    const int prev_encountered_errors;
    int encountered_error_count;

    ExpectedErrorClause( int expected_error_count )
       : expected_error_count( expected_error_count ),
         prev_encountered_errors(0), encountered_error_count(0) { }

    ExpectedErrorClause( int expected_error_count, int prev_encountered_errors, int encountered_error_count )
       : expected_error_count(expected_error_count),
         prev_encountered_errors(prev_encountered_errors),
         encountered_error_count(encountered_error_count) { }
};


class TxParseOrigin {
public:
    virtual ~TxParseOrigin() = default;

    /** Returns the parse location of this parse origin. */
    virtual const TxLocation& get_parse_location() const = 0;

    /** Returns the expected-error clause of this parse origin, or null if none. */
    virtual ExpectedErrorClause* exp_err_ctx() const = 0;
};


/** Emits a compiler error message including the source code origin where it likely occurred. */
void cerror(const TxParseOrigin* origin, const std::string& msg);

/** Emits a compiler warning message including the source code origin where it likely occurred. */
void cwarning(const TxParseOrigin* origin, const std::string& msg);

/** Emits a compiler error message including the source code origin where it likely occurred. */
void cerror(const TxParseOrigin& origin, const std::string& msg);

/** Emits a compiler warning message including the source code origin where it likely occurred. */
void cwarning(const TxParseOrigin& origin, const std::string& msg);


void finalize_expected_error_clause( const TxParseOrigin* origin );


#ifndef NO_CERRORS
#   define CERROR(origin, message) \
    do { \
        std::stringstream msg;  msg << message; \
        cerror(origin, msg.str()); \
    } while (false)
#   define CWARNING(origin, message) \
    do { \
        std::stringstream msg;  msg << message; \
        cwarning(origin, msg.str()); \
    } while (false)
#else
#   define CERROR(origin, message) do { } while (false)
#   define CWARNING(origin, message) do { } while (false)
#endif
