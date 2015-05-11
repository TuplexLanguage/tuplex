#pragma once

#include <string>


enum TxDeclarationFlags {
	TXD_NONE       = 0,
	TXD_STATIC     = 1 << 0,
	TXD_ABSTRACT   = 1 << 1,
    TXD_FINAL      = 1 << 2,
    TXD_OVERRIDE   = 1 << 3,
	TXD_PUBLIC 	   = 1 << 4,
    TXD_PROTECTED  = 1 << 5,
    TXD_BUILTIN    = 1 << 6,
    TXD_IMPLICIT   = 1 << 7,
    TXD_GENPARAM   = 1 << 8,
    TXD_ALIAS      = 1 << 9,
    TXD_CONSTRUCTOR= 1 << 10,
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
    sprintf(buf, "%c%c%c%c%c%c%c%c%c%c%c",
            (flags & TXD_STATIC)    ? 'S' : '-',
            (flags & TXD_ABSTRACT)  ? 'A' : '-',
            (flags & TXD_FINAL)     ? 'F' : '-',
            (flags & TXD_OVERRIDE)  ? 'O' : '-',
            (flags & TXD_PUBLIC)    ? 'P' : '-',
            (flags & TXD_PROTECTED) ? 'R' : '-',
            (flags & TXD_BUILTIN)   ? 'B' : '-',
            (flags & TXD_IMPLICIT)  ? 'I' : '-',
            (flags & TXD_GENPARAM)  ? 'G' : '-',
            (flags & TXD_ALIAS)     ? 'A' : '-',
            (flags & TXD_CONSTRUCTOR)?'C' : '-');
    return std::string(buf);
}

template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<< (std::basic_ostream<charT, traits> &lhs, TxDeclarationFlags const rhs) {
    return lhs << to_string(rhs);
}
