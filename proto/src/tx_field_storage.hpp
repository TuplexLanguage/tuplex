#pragma once

/** Specifies the storage type for a field entity.
 * GLOBAL are globally declared fields, i.e. outside of any type definition.
 * STATIC and VIRTUAL are statically allocated fields within a type definition.
 * VIRTUAL fields are like STATIC but subject to polymorphic lookup.
 * INSTANCEMETHOD is a special case, where the function pointer is static/virtual and the 'self' ref is provided in runtime
 * INSTANCE fields are members of type instances (i.e. object members).
 * STACK fields are regular "auto" variables, including function arguments.
 * GLOBAL, STATIC, VIRTUAL are compile-time-allocated.
 */
enum TxFieldStorage
    : int {TXS_NOSTORAGE, TXS_GLOBAL, TXS_STATIC, TXS_VIRTUAL, TXS_INSTANCEMETHOD, TXS_INSTANCE, TXS_STACK
};
