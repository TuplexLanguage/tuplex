#pragma once

#include <string>


enum TxDeclarationFlags {
	TXD_NONE       = 0,
	// can be declared in source:
	TXD_STATIC     = 1 << 0,
    TXD_PUBLIC     = 1 << 1,
    TXD_PROTECTED  = 1 << 2,
	TXD_ABSTRACT   = 1 << 3,
    TXD_FINAL      = 1 << 4,
    TXD_OVERRIDE   = 1 << 5,
    // cannot be declared in source:
    TXD_BUILTIN    = 1 << 6,
    TXD_IMPLICIT   = 1 << 7,
    TXD_GENPARAM   = 1 << 8,
    TXD_GENBINDING = 1 << 9,
    TXD_CONSTRUCTOR= 1 << 10,
    TXD_EXPERRBLOCK= 1 << 11,
};
inline TxDeclarationFlags operator|(TxDeclarationFlags a, TxDeclarationFlags b) {
	return static_cast<TxDeclarationFlags>(static_cast<int>(a) | static_cast<int>(b));
}
inline TxDeclarationFlags operator&(TxDeclarationFlags a, TxDeclarationFlags b) {
	return static_cast<TxDeclarationFlags>(static_cast<int>(a) & static_cast<int>(b));
}
inline TxDeclarationFlags operator^(TxDeclarationFlags a, TxDeclarationFlags b) {
    return static_cast<TxDeclarationFlags>(static_cast<int>(a) ^ static_cast<int>(b));
}

inline std::string to_string(TxDeclarationFlags flags) {
    char buf[16];
    sprintf(buf, "%c%c%c%c%c%c%c%c%c%c%c%c%c%c",
            (flags & TXD_STATIC)    ? 'S' : '-',
            (flags & TXD_PUBLIC)    ? 'P' : '-',
            (flags & TXD_PROTECTED) ? 'R' : '-',
            (flags & TXD_ABSTRACT)  ? 'A' : '-',
            (flags & TXD_FINAL)     ? 'F' : '-',
            (flags & TXD_OVERRIDE)  ? 'O' : '-',
            ' ',
            (flags & TXD_BUILTIN)   ? 'b' : '-',
            (flags & TXD_IMPLICIT)  ? 'I' : '-',
            (flags & TXD_GENPARAM)  ? 'G' : '-',
            (flags & TXD_GENBINDING)? 'B' : '-',
            (flags & TXD_CONSTRUCTOR)?'C' : '-',
            (flags & TXD_EXPERRBLOCK)?'E' : '-',
            ' ');  // available char slot placeholder
    return std::string(buf);
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<< (std::basic_ostream<charT, traits> &lhs, TxDeclarationFlags const rhs) {
    return lhs << to_string(rhs);
}
