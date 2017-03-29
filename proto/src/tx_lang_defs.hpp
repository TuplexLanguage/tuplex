#pragma once

/** namespace for the built-in language entities */
#define BUILTIN_NS "tx"

/** implicit identifier for the local namespace */
#define LOCAL_NS "$local"

/** internal identifier for constructors */
#define CONSTR_IDENT "$constr"

///** internal identifier for initializers */
//#define INIT_IDENT "$init"


#include <stdint.h>

enum BuiltinTypeId : uint32_t {
    ANY,
    VOID,
    ELEMENTARY,
    SCALAR,
    INTEGER,
    SIGNED,
    BYTE,
    SHORT,
    INT,
    LONG,
    UNSIGNED,
    UBYTE,
    USHORT,
    UINT,
    ULONG,
    FLOATINGPOINT,
    HALF,
    FLOAT,
    DOUBLE,
    BOOL,
    REFERENCE,
    ARRAY,
    FUNCTION,
    TUPLE,
    INTERFACE,
    BuiltinTypeId_COUNT,
    TXBTID_NOTSET = UINT32_MAX
};
