#include "tx_operations.hpp"

typedef struct {
    TxOperation op;
    TxOperationClass opclass;
    const char* name;
} OpName;

static const OpName OP_NAMES[] = {
                                   { TXOP_NONE, TXOC_NONE, "<none>" },
                                   { TXOP_PLUS, TXOC_ARITHMETIC, "PLUS" },
                                   { TXOP_MINUS, TXOC_ARITHMETIC, "MINUS" },
                                   { TXOP_MUL, TXOC_ARITHMETIC, "MUL" },
                                   { TXOP_DIV, TXOC_ARITHMETIC, "DIV" },
                                   { TXOP_EQ, TXOC_EQUALITY, "EQ" },
                                   { TXOP_NE, TXOC_EQUALITY, "NE" },
                                   { TXOP_GT, TXOC_COMPARISON, "GT" },
                                   { TXOP_GE, TXOC_COMPARISON, "GE" },
                                   { TXOP_LT, TXOC_COMPARISON, "LT" },
                                   { TXOP_LE, TXOC_COMPARISON, "LE" },
                                   { TXOP_AND, TXOC_BOOLEAN, "AND" },
                                   { TXOP_OR, TXOC_BOOLEAN, "OR" },
};

TxOperationClass get_op_class( TxOperation op ) {
    return OP_NAMES[op].opclass;
}

const char* to_cstring( TxOperation op ) {
    return OP_NAMES[op].name;
}

std::string to_string( TxOperation op ) {
    return std::string( to_cstring( op ) );
}
