#pragma once

#include <iostream>
#include <sstream>

#include "location.hh"

class TxDriver;

class TxParseOrigin {
public:
    virtual ~TxParseOrigin() = default;

    virtual TxDriver* get_driver() const = 0;
    virtual const yy::location& get_parse_location() const = 0;
};

/** Used when there is no source location information available. */
extern const yy::location NULL_LOC;

/** Emits a compiler error message including the source code origin where it likely occurred. */
void cerror(const TxParseOrigin* origin, const std::string& msg);

/** Emits a compiler warning message including the source code origin where it likely occurred. */
void cwarning(const TxParseOrigin* origin, const std::string& msg);

/** Emits a compiler error message (without source code origin information). */
void cerror(TxDriver* driver, const std::string& msg);

/** Emits a compiler warning message (without source code origin information). */
void cwarning(TxDriver* driver, const std::string& msg);


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
