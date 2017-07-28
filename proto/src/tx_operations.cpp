#include "tx_operations.hpp"

typedef struct {
    TxOperation op;
    TxOperationClass opclass;
    const std::string name;
} OpName;

static const OpName OP_NAMES[] = {
                                   { TXOP_NONE, TXOC_NONE, "<none>" },
                                   { TXOP_PLUS, TXOC_ARITHMETIC, "PLUS" },
                                   { TXOP_MINUS, TXOC_ARITHMETIC, "MINUS" },
                                   { TXOP_MUL, TXOC_ARITHMETIC, "MUL" },
                                   { TXOP_DIV, TXOC_ARITHMETIC, "DIV" },
//                                   { TXOP_EQ, TXOC_EQUALITY, "EQ" },
//                                   { TXOP_NE, TXOC_EQUALITY, "NE" },
                                   { TXOP_GT, TXOC_COMPARISON, "GT" },
                                   { TXOP_GE, TXOC_COMPARISON, "GE" },
                                   { TXOP_LT, TXOC_COMPARISON, "LT" },
                                   { TXOP_LE, TXOC_COMPARISON, "LE" },
                                   { TXOP_AND, TXOC_LOGICAL, "AND" },
                                   { TXOP_OR, TXOC_LOGICAL, "OR" },
                                   { TXOP_XOR, TXOC_LOGICAL, "XOR" },
                                   { TXOP_LSHIFT, TXOC_SHIFT, "LSH" },
                                   { TXOP_RSHIFT, TXOC_SHIFT, "RSH" },
                                   { TXOP_ARSHIFT, TXOC_SHIFT, "ARSH" },
};

TxOperationClass get_op_class( TxOperation op ) {
    return OP_NAMES[op].opclass;
}

const std::string& to_string( TxOperation op ) {
    return OP_NAMES[op].name;
}
