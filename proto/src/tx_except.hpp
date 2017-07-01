#pragma once

#include <stdexcept>

class TxParseOrigin;

/** Represents a compilation error (Tuplex source code error).
 * (This is used instead of passing NULL values up the call stack, requiring many NULL pointer checks.)
 * It is expected that the original throwing site will have generated an appropriate compiler error or log message.
 */
class compilation_error : public std::runtime_error {
    TxParseOrigin const * const origin;
public:
    compilation_error( const TxParseOrigin* origin, const std::string& errMessage )
            : std::runtime_error( errMessage ), origin( origin ) {
    }
};

/** Represents a failure to declare a name.
 * (This is used instead of passing NULL values up the call stack, requiring many NULL pointer checks.)
 * It is expected that the original throwing site will have generated an appropriate compiler error or log message.
 */
class declaration_error : public compilation_error {
public:
    declaration_error( const TxParseOrigin* origin, const std::string& errMessage )
            : compilation_error( origin, errMessage ) { }

    declaration_error( const TxParseOrigin& origin, const std::string& errMessage )
            : declaration_error( &origin, errMessage ) {
    }
};

/** Represents a failure to resolve a type / field.
 * (This is used instead of passing NULL values up the call stack, requiring many NULL pointer checks.)
 * It is expected that the original throwing site will have generated an appropriate compiler error or log message.
 */
class resolution_error : public compilation_error {
public:
    resolution_error( const TxParseOrigin* origin, const std::string& errMessage )
            : compilation_error( origin, errMessage ) { }
};

/** Represents a failed check during code generation (e.g. out-of-bounds check).
 * (This is used instead of passing NULL values up the call stack, requiring many NULL pointer checks.)
 * It is expected that the original throwing site will have generated an appropriate compiler error or log message.
 */
class codecheck_error : public compilation_error {
public:
    codecheck_error( const TxParseOrigin* origin, const std::string& errMessage )
            : compilation_error( origin, errMessage ) { }
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
