#pragma once

#include <string>

enum TxOperationClass {
    TXOC_NONE,
    TXOC_ARITHMETIC,
    TXOC_EQUALITY,
    TXOC_COMPARISON,
    TXOC_LOGICAL,
    TXOC_SHIFT,
};

enum TxOperation {
    TXOP_NONE,
    // TXOC_ARITHMETIC: scalar operand and result types:
    TXOP_PLUS,
    TXOP_MINUS,
    TXOP_MUL,
    TXOP_DIV,
    // TXOC_EQUALITY: any operand types; boolean result type:
    TXOP_EQ,
    TXOP_NE,
    // TXOC_COMPARISON: scalar operand types; boolean result type:
    TXOP_GT,
    TXOP_GE,
    TXOP_LT,
    TXOP_LE,
    // TXOC_LOGICAL: integer/boolean operand and result types:
    TXOP_AND,
    TXOP_OR,
    TXOP_XOR,
    // TXOC_SHIFT: integer operand and result types:
    TXOP_LSHIFT,
    TXOP_RSHIFT,
    TXOP_ARSHIFT,  // arithmetic (signed) right shift
    TXOP_END
};

inline bool is_valid( TxOperation op ) {
    return op >= 0 && op < TXOP_END;
}

extern TxOperationClass get_op_class( TxOperation op );

extern const char* to_cstring( TxOperation op );

extern std::string to_string( TxOperation op );

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<<( std::basic_ostream<charT, traits> &lhs, TxOperation const rhs ) {
    return lhs << to_string( rhs );
}
