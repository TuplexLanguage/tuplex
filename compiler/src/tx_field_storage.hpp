#pragma once

#include <string>

/** Specifies the storage type for a field entity / value.
 * GLOBAL are globally declared fields, i.e. outside of any type definition.
 * STATIC and VIRTUAL are statically allocated fields within a type definition.
 * VIRTUAL fields are subject to polymorphic lookup.
 * STATIC fields are an "optimization" of virtual that will never need polymorphic lookup (e.g. constructors; private)
 * INSTANCEMETHOD is a special case, where the function pointer is static/virtual and the 'self' ref is provided in runtime
 * INSTANCE fields are members of type instances (i.e. object members).
 * STACK fields are regular "auto" variables, including function arguments, bound to a field declaration.
 * UNBOUND_STACK indicates a value that is allocated on the stack, but not (yet) bound to a field declaration.
 * NOSTORAGE indicates an unallocated / temporary / register value, also known as rvalue.
 */
enum TxFieldStorage : int {
    TXS_NOSTORAGE, TXS_GLOBAL, TXS_STATIC, TXS_VIRTUAL, TXS_INSTANCEMETHOD, TXS_INSTANCE, TXS_STACK, TXS_UNBOUND_STACK,
};

inline std::string to_string( TxFieldStorage storage ) {
    switch( storage ) {
    case TXS_NOSTORAGE:
        return "TXS_NOSTORAGE";
    case TXS_GLOBAL:
        return "TXS_GLOBAL";
    case TXS_STATIC:
        return "TXS_STATIC";
    case TXS_VIRTUAL:
        return "TXS_VIRTUAL";
    case TXS_INSTANCEMETHOD:
        return "TXS_INSTANCEMETHOD";
    case TXS_INSTANCE:
        return "TXS_INSTANCE";
    case TXS_STACK:
        return "TXS_STACK";
    case TXS_UNBOUND_STACK:
        return "TXS_UNBOUND_STACK";
    }
    return "-unknown TxFieldStorage value " + std::to_string((int)storage) + "-";
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<<( std::basic_ostream<charT, traits> &lhs, TxFieldStorage const rhs ) {
    return lhs << to_string( rhs );
}
