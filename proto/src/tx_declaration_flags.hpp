#pragma once

#include <string>

enum TxDeclarationFlags {
    TXD_NONE = 0,
    // can be declared in source:
    TXD_PUBLIC = 1 << 0,
    TXD_PROTECTED = 1 << 1,
    TXD_STATIC = 1 << 2,
    TXD_EXTERN = 1 << 3,
    TXD_ABSTRACT = 1 << 4,
    TXD_FINAL = 1 << 5,
    TXD_OVERRIDE = 1 << 6,
    // cannot be declared in source:
    TXD_BUILTIN = 1 << 7,
    TXD_IMPLICIT = 1 << 8,
    TXD_GENPARAM = 1 << 9,
    TXD_GENBINDING = 1 << 10,
    TXD_CONSTRUCTOR = 1 << 11,
    TXD_INITIALIZER = 1 << 12,
    TXD_EXPERRBLOCK = 1 << 13,
};
inline TxDeclarationFlags operator|( TxDeclarationFlags a, TxDeclarationFlags b ) {
    return static_cast<TxDeclarationFlags>( static_cast<int>( a ) | static_cast<int>( b ) );
}
inline TxDeclarationFlags operator&( TxDeclarationFlags a, TxDeclarationFlags b ) {
    return static_cast<TxDeclarationFlags>( static_cast<int>( a ) & static_cast<int>( b ) );
}
inline TxDeclarationFlags operator^( TxDeclarationFlags a, TxDeclarationFlags b ) {
    return static_cast<TxDeclarationFlags>( static_cast<int>( a ) ^ static_cast<int>( b ) );
}

inline std::string to_string( TxDeclarationFlags flags ) {
    char buf[16];
    sprintf( buf, "%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c",
             ( flags & TXD_PUBLIC ) ? 'P' : '-',
             ( flags & TXD_PROTECTED ) ? 'R' : '-',
             ( flags & TXD_STATIC ) ? 'S' : '-',
             ( flags & TXD_EXTERN ) ? 'X' : '-',
             ( flags & TXD_ABSTRACT ) ? 'A' : '-',
             ( flags & TXD_FINAL ) ? 'F' : '-',
             ( flags & TXD_OVERRIDE ) ? 'O' : '-',
             ' ',
             ( flags & TXD_BUILTIN ) ? 'b' : '-',
             ( flags & TXD_IMPLICIT ) ? 'I' : '-',
             ( flags & TXD_GENPARAM ) ? 'G' : '-',
             ( flags & TXD_GENBINDING ) ? 'B' : '-',
             ( flags & TXD_CONSTRUCTOR ) ? 'C' : '-',
             ( flags & TXD_INITIALIZER ) ? 'i' : '-',
             ( flags & TXD_EXPERRBLOCK ) ? 'E' : '-' );
    return std::string( buf );
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<<( std::basic_ostream<charT, traits> &lhs, TxDeclarationFlags const rhs ) {
    return lhs << to_string( rhs );
}
