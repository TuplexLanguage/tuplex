#pragma once

#include <iostream>
#include <sstream>
#include <stdexcept>

#ifndef NDEBUG
#   define ASSERT(condition, message) \
    do { \
        if (! (condition)) { \
            std::stringstream msg;  msg << message; \
            std::cerr << "Assertion `" #condition "` failed in " << __FILE__ \
                      << " line " << __LINE__ << ": " << msg.str() << std::endl; \
            throw std::logic_error(msg.str()); \
            /*std::exit(EXIT_FAILURE);*/ \
        } \
    } while (false)
#else
#   define ASSERT(condition, message) do { } while (false)
#endif
