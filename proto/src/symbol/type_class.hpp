#pragma once

#include <string>

/** The type classes of Tuplex. Each type class is handled specially by the compiler. */
enum TxTypeClass {
    /** Represents the Any root type. */
    TXTC_ANY,
    /** The built-in, non-aggregate types (e.g. Bool, Scalar). */
    TXTC_ELEMENTARY,
    /** The Ref types. */
    TXTC_REFERENCE,
    /** The Array types. */
    TXTC_ARRAY,
    /** The Tuple types. */
    TXTC_TUPLE,
    /** The Union types. */
    TXTC_UNION,
    /** The function types, including methods and lambdas. */
    TXTC_FUNCTION,
    /** The interface types. */
    TXTC_INTERFACE,
    /** The internal, implicit interface adapter types. */
    TXTC_INTERFACEADAPTER,
    /** The internal Void type (represents the "return type" of functions that do not return a value). */
    TXTC_VOID,
};

inline std::string to_string( TxTypeClass tc ) {
    switch( tc ) {
    case TXTC_ANY:
        return "ANY";
    case TXTC_ELEMENTARY:
        return "ELEMENTARY";
    case TXTC_REFERENCE:
        return "REFERENCE";
    case TXTC_ARRAY:
        return "ARRAY";
    case TXTC_TUPLE:
        return "TUPLE";
    case TXTC_UNION:
        return "UNION";
    case TXTC_FUNCTION:
        return "FUNCTION";
    case TXTC_INTERFACE:
        return "INTERFACE";
    case TXTC_INTERFACEADAPTER:
        return "INFADAPTER";
    case TXTC_VOID:
        return "VOID";
    }
    return "-unknown TxTypeClass value " + std::to_string((int)tc) + "-";
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<<( std::basic_ostream<charT, traits> &lhs, TxTypeClass const rhs ) {
    return lhs << to_string( rhs );
}
