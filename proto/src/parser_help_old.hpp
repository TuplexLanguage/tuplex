#pragma once

#include "location.hh"

extern int error_count;

//extern void yyerror (char const *s);

extern void cerror(const yy::location& parseLocation, char const *fmt, ...);

//extern char* txstrndup(const char *s, size_t n);
