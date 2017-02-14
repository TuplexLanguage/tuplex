#pragma once

#include <iostream>
#include <sstream>

#include "location.hpp"

class TxDriver;

class TxParseOrigin {
public:
    virtual ~TxParseOrigin() = default;

    virtual const TxLocation& get_parse_location() const = 0;
};


/** Emits a compiler error message including the source code origin where it likely occurred. */
void cerror(const TxParseOrigin* origin, const std::string& msg);

/** Emits a compiler warning message including the source code origin where it likely occurred. */
void cwarning(const TxParseOrigin* origin, const std::string& msg);

/** Emits a compiler error message. */
void cerror(const TxLocation& location, const std::string& msg);

/** Emits a compiler warning message. */
void cwarning(const TxLocation& location, const std::string& msg);


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
