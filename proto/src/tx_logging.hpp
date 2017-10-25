#pragma once

#include "util/logging.hpp"

#define LOG(logger, level, message) \
    do { \
        if ( level <= Logger::globalThreshold ) { \
            std::stringstream msg;  msg << message; \
            (logger)->log( level, "%s", msg.str().c_str() ); \
        } \
    } while (false)

#define LOG_INFO(logger, message) \
    do { \
        if ( INFO <= Logger::globalThreshold ) { \
            std::stringstream msg;  msg << message; \
            (logger)->info( "%s", msg.str().c_str() ); \
        } \
    } while (false)

#define LOG_NOTE(logger, message) \
    do { \
        if ( NOTE <= Logger::globalThreshold ) { \
            std::stringstream msg;  msg << message; \
            (logger)->note( "%s", msg.str().c_str() ); \
        } \
    } while (false)

#define LOG_DEBUG(logger, message) \
    do { \
        if ( DEBUG <= Logger::globalThreshold ) { \
            std::stringstream msg;  msg << message; \
            (logger)->debug( "%s", msg.str().c_str() ); \
        } \
    } while (false)

#ifdef DEVMODE
#   define LOG_TRACE(logger, message) \
    do { \
        if ( TRACE <= Logger::globalThreshold ) { \
            std::stringstream msg;  msg << message; \
            (logger)->trace( "%s", msg.str().c_str() ); \
        } \
    } while (false)
#else
#   define LOG_TRACE(logger, message) do { } while (false)
#endif

#define GET_MACRO(_1,_2,_3,NAME,...) NAME
#define TRACE_CODEGEN(...) GET_MACRO(__VA_ARGS__, TRACE_CODEGEN3, TRACE_CODEGEN2)(__VA_ARGS__)

#ifdef DEBUG_CODEGEN
#   define TRACE_CODEGEN2(node, llvmctx) \
    do { \
        if ( TRACE <= Logger::globalThreshold ) { \
            std::stringstream msg;  msg << node << " code_gen"; \
            llvmctx.LOGGER()->trace( "%s", msg.str().c_str() ); \
        } \
    } while (false)
#   define TRACE_CODEGEN3(node, llvmctx, message) \
    do { \
        if ( TRACE <= Logger::globalThreshold ) { \
            std::stringstream msg;  msg << node << " code_gen " << message; \
            llvmctx.LOGGER()->trace( "%s", msg.str().c_str() ); \
        } \
    } while (false)
#else
#   define TRACE_CODEGEN2(node, llvmctx) do { } while (false)
#   define TRACE_CODEGEN3(node, llvmctx, message) do { } while (false)
#endif
