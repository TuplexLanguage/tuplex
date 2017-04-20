#pragma once

/** namespace for the built-in language entities */
#define BUILTIN_NS "tx"

/** implicit identifier for the local namespace */
#define LOCAL_NS "$local"

/** internal identifier for constructors */
#define CONSTR_IDENT "$constr"



#include <stdint.h>

enum BuiltinTypeId : uint32_t {
    TXBT_ANY,
    TXBT_VOID,
    TXBT_ELEMENTARY,
    TXBT_SCALAR,
    TXBT_INTEGER,
    TXBT_SIGNED,
    TXBT_BYTE,
    TXBT_SHORT,
    TXBT_INT,
    TXBT_LONG,
    TXBT_UNSIGNED,
    TXBT_UBYTE,
    TXBT_USHORT,
    TXBT_UINT,
    TXBT_ULONG,
    TXBT_FLOATINGPOINT,
    TXBT_HALF,
    TXBT_FLOAT,
    TXBT_DOUBLE,
    TXBT_BOOL,
    TXBT_REFERENCE,
    TXBT_ARRAY,
    TXBT_FUNCTION,
    TXBT_TUPLE,
    TXBT_INTERFACE,
    BuiltinTypeId_COUNT,
    TXBT_NOTSET = UINT32_MAX
};
