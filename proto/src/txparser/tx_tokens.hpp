#pragma once

#include "bison_parser.hpp"

typedef yy::TxParser::token_type TxTokenId;
/*
enum class TxTokenId : u_int32_t {
    END = 0,
    ERROR,
    NEWLINE,
    WHITESPACE,
    INDENT,
    DEDENT,
    COMMENT,
    NAME,
    LBRACE,
    RBRACE,
    LPAREN,
    RPAREN,
    SEMICOLON,
    KW_MODULE,
};
*/


template<typename charT, typename traits>
std::basic_ostream<charT, traits> &
operator<<( std::basic_ostream<charT, traits> &lhs, TxTokenId const &rhs ) {
    return lhs << rhs;
}
