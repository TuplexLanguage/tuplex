#pragma once

#include <iostream>
#include <sstream>

#include "parser/location.hpp"
#include "tx_except.hpp"

struct ExpectedErrorClause {
    const int expected_error_count;
    const int prev_encountered_errors;
    int encountered_error_count;

    ExpectedErrorClause( int expected_error_count )
            : expected_error_count( expected_error_count ),
              prev_encountered_errors( 0 ),
              encountered_error_count( 0 ) {
    }

    ExpectedErrorClause( int expected_error_count, int prev_encountered_errors, int encountered_error_count )
            : expected_error_count( expected_error_count ),
              prev_encountered_errors( prev_encountered_errors ),
              encountered_error_count( encountered_error_count ) {
    }
};

class TxNode;

class TxParseOrigin {
public:
    virtual ~TxParseOrigin() = default;

    TxParserContext* get_parser_context() const {
        return this->get_parse_location().parserCtx;
    }

    /** Returns the node that is this parse origin. */
    virtual const TxNode* get_origin_node() const = 0;

    /** Returns the parse location of this parse origin. */
    virtual const TxLocation& get_parse_location() const;

    /** Returns the expected-error clause of this parse origin, or null if none. */
    virtual ExpectedErrorClause* exp_err_ctx() const;
};

/** Used to ensure proper closing of an exp-err-clause (RAII style). */
class ScopedExpErrClause {
    TxParseOrigin* const origin;
    const bool enabled;
    public:
    ScopedExpErrClause( TxParseOrigin* origin, bool enabled = true );
    ~ScopedExpErrClause();
};

/** Emits a compiler error message including the source code origin where it likely occurred. */
void cerror( const TxParseOrigin* origin, const std::string& msg );

/** Emits a compiler warning message including the source code origin where it likely occurred. */
void cwarning( const TxParseOrigin* origin, const std::string& msg );

/** Emits a compiler error message including the source code origin where it likely occurred. */
void cerror( const TxParseOrigin& origin, const std::string& msg );

/** Emits a compiler warning message including the source code origin where it likely occurred. */
void cwarning( const TxParseOrigin& origin, const std::string& msg );

void finalize_expected_error_clause( const TxParseOrigin* origin );

#ifndef NO_CERRORS
#   define CERR_THROWRES(origin, message) \
    do { \
        std::stringstream msg;  msg << message; \
        cerror(origin, msg.str()); \
        throw resolution_error( origin, msg.str() ); \
    } while (false)
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
#   define CERR_THROWRES(origin, message) \
    do { \
        std::stringstream msg;  msg << message; \
        throw resolution_error( origin, msg.str() ); \
    } while (false)
#   define CERROR(origin, message) do { } while (false)
#   define CWARNING(origin, message) do { } while (false)
#endif
