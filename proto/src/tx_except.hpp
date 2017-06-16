#pragma once

#include <stdexcept>

class TxParseOrigin;

/** Represents a failure to resolve a type / field.
 * (This is used instead of passing NULL values up the call stack, requiring many NULL pointer checks.)
 * It is expected that the original throwing site will have generated an appropriate compiler error or log message.
 */
class resolution_error : public std::runtime_error {
    TxParseOrigin const * const origin;
    public:
    resolution_error( const TxParseOrigin* origin, const std::string& errMessage )
            : std::runtime_error( errMessage ), origin( origin ) {
    }
};

/** Represents a failed check during code generation (e.g. out-of-bounds check).
 * (This is used instead of passing NULL values up the call stack, requiring many NULL pointer checks.)
 * It is expected that the original throwing site will have generated an appropriate compiler error or log message.
 */
class codecheck_error : public std::runtime_error {
    TxParseOrigin const * const origin;
    public:
    codecheck_error( const TxParseOrigin* origin, const std::string& errMessage )
            : std::runtime_error( errMessage ), origin( origin ) {
    }
};

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<<( std::basic_ostream<charT, traits> &lhs, const resolution_error& rhs ) {
    return lhs << rhs.what();
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<<( std::basic_ostream<charT, traits> &lhs, const codecheck_error& rhs ) {
    return lhs << rhs.what();
}

#define THROW_LOGIC(message) \
do { \
    std::stringstream msg;  msg << message; \
    throw std::logic_error( msg.str() ); \
} while (false)
