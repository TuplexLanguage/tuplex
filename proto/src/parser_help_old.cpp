#include <stdio.h>

#include "logging.hpp"
#include "ast.hpp"
#include "parser.hpp"

#include "parser_help.hpp"


static Logger* LOG; // = Logger::get("PARSER");

int error_count = 0;

/* Called by yyparse on error.
void yyerror(char const *msg) {
    parser_error(yylloc, "%s", msg);
}
*/

void parser_error(const yy::location& parseLocation, char const *fmt, ...) {
    error_count++;
    if (! LOG)
        LOG = &Logger::get("PARSER");

    va_list ap;
    va_start(ap, fmt);
    char buf[256];
    vsnprintf(buf, 256, fmt, ap);
    va_end(ap);

    // (if the error is unexpected token and it is SEP, lineno may be 1 too high)
    if (parseLocation.begin.line == parseLocation.end.line) {
        int lcol = (parseLocation.end.column > parseLocation.begin.column) ? parseLocation.end.column-1 : parseLocation.end.column;
        LOG->error("At line %d.%d-%d: %s", parseLocation.begin.line, parseLocation.begin.column, lcol, buf);
    }
    else
        LOG->error("At lines %d.%d-%d.%d: %s",
                parseLocation.begin.line, parseLocation.begin.column, parseLocation.end.line, parseLocation.end.column-1, buf);
}


char* txstrndup(const char *s, size_t n) {
    // TODO: efficient alloc, and GC
    return strndup(s, n);
}
