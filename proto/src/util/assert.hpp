#pragma once

#include <iostream>
#include <sstream>
#include <stdexcept>

class assertion_error : public std::logic_error {
public:
    assertion_error( const std::string& errMessage )
            : std::logic_error( errMessage ) {
    }
};

#ifdef DEVMODE
#   define ASSERT(condition, message) \
    do { \
        if (! (condition)) { \
            std::stringstream msg;  msg << message; \
            std::cerr << "Assertion `" #condition "` failed in " << __FILE__ \
                      << " line " << __LINE__ << ": " << msg.str() << std::endl; \
            throw assertion_error(msg.str()); \
            /*std::exit(EXIT_FAILURE);*/ \
        } \
    } while (false)
#else
#   define ASSERT(condition, message) do { } while (false)
#endif
