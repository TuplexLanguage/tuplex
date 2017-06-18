D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			[U]?[HFDQ]
IS			[U]?[BSIL]
WS          [ \t]

%{ /* -*- C++ -*- */
#include <cerrno>
#include <climits>
#include <cstdlib>
#include <string>

#include "parser.hpp"

typedef yy::TxParser::token token;

/* By default yylex returns int, we use token_type.
   Unfortunately yyterminate by default returns 0, which is not of token_type.  */
#define yyterminate() return token::END; /* yy::TxParser::make_END(loc) */

// Code run each time a pattern is matched.
#define YY_USER_ACTION  yylloc->columns (yyleng);

/* The scanner calls this function on end of file, so you can point it to another file
   and continue scanning its contents, unless %option noyywrap is declared.
extern "C" int yywrap() { return 0; }
*/

%}

/* %option yylineno */
%option nounput
%option noinput
%option noyywrap
%option batch
%option debug

%s IN_COMMENT STRINGFORMAT

%%

%{
  // Code run each time yylex is called.
  yylloc->step ();
%}


 /* append extra newline at end-of-file */
 /* <<EOF>>     { static int extra_eof_nl = 0; return (extra_eof_nl = !extra_eof_nl) ? token::NL : token::END; } */


 /* multi-line comment */
<INITIAL>{
  "/*"      yylloc->step(); parserCtx->commentNestLevel++; BEGIN(IN_COMMENT);
}
<IN_COMMENT>{
  "*/"       yylloc->step(); parserCtx->commentNestLevel--; if (parserCtx->commentNestLevel == 0) BEGIN(INITIAL);
  "/*"       yylloc->step(); parserCtx->commentNestLevel++;
  [^/\*\n]+  yylloc->step(); // eat comment in chunks
  [/\*]      yylloc->step(); // eat lone stars and slashes
  [\n]+      yylloc->lines(yyleng); yylloc->step();
  <<EOF>>    BEGIN(INITIAL); return token::END;  // ensures lexer isn't still in comment state upon parsing subsequent file (future: refactor)
}

 /* line comment */
"##".*$     yylloc->step();

 /* white space */
[ \t\r]+    yylloc->step();
[\n]+       { yylloc->lines(yyleng); yylloc->step(); }

 /* statement separators */
";"         { return token::SEMICOLON; }

"{"     { return token::LBRACE; }
"}"     { return token::RBRACE; }


 /* operators */
"."		{ return token::DOT; }
":"		{ return token::COLON; }
","		{ return token::COMMA; }
".."    { return token::DOTDOT; }
"..."   { return token::ELLIPSIS; }
"*"		{ return token::ASTERISK; }
"+"		{ return token::PLUS; }
"-"		{ return token::MINUS; }
"/"		{ return token::FSLASH; }
"\\"	{ return token::BSLASH; }
"&"		{ return token::AAND; }
"|"		{ return token::PIPE; }
"^"		{ return token::CARET; }
"~"		{ return token::TILDE; }
"@"		{ return token::AT; }
"$"		{ return token::DOLLAR; }
""	{ return token::EURO; }
"("		{ return token::LPAREN; }
")"		{ return token::RPAREN; }
"["		{ return token::LBRACKET; }
"]"		{ return token::RBRACKET; }
"?"		{ return token::QMARK; }
"!"		{ return token::EMARK; }
"="		{ return token::EQUAL; }
"<"     { return token::LT; }
">"     { return token::GT; }
"<<"    { return token::LTLT; }
 /*
">>"    { return token::GTGT; }
">>>"   { return token::GTGTGT; }
 */
"<="    { return token::LEQUAL; }
">="    { return token::GEQUAL; }
"=="		{ return token::EEQUAL; }
"!="		{ return token::NEQUAL; }
"==="		{ return token::EEEQUAL; }
"!=="		{ return token::NEEQUAL; }
":="		{ return token::COLEQUAL; }
"+="		{ return token::PLUSEQUAL; }
"-="		{ return token::MINUSEQUAL; }
"*="		{ return token::ASTERISKEQUAL; }
"/="		{ return token::FSLASHEQUAL; }
"->"        { return token::DASHGT; }

 /* keywords */
"module"	{ return token::KW_MODULE; }
"import"	{ return token::KW_IMPORT; }
"public"	{ return token::KW_PUBLIC; }
"protected"	{ return token::KW_PROTECTED; }
"static"	{ return token::KW_STATIC; }
"extern"    { return token::KW_EXTERN; }
"type"		{ return token::KW_TYPE; }
"class"		{ return token::KW_CLASS; }
"interface"	{ return token::KW_INTERFACE; }
"abstract"	{ return token::KW_ABSTRACT; }
"final"		{ return token::KW_FINAL; }
"override"  { return token::KW_OVERRIDE; }
"mod"		{ return token::KW_MODIFIABLE; }
"ref"		{ return token::KW_REFERENCE; }
"derives"   { return token::KW_DERIVES; }

"assert"    { return token::KW_ASSERT; }
"#experr"   { return token::KW_EXPERR; }

"while"		{ return token::KW_WHILE; }
"for"		{ return token::KW_FOR; }
"in"		{ return token::KW_IN; }
"if"		{ return token::KW_IF; }
"is"		{ return token::KW_IS; }
"else"		{ return token::KW_ELSE; }
"switch"	{ return token::KW_SWITCH; }
"with"		{ return token::KW_WITH; }
"as"		{ return token::KW_AS; }
"case"		{ return token::KW_CASE; }
"break"		{ return token::KW_BREAK; }
"continue"	{ return token::KW_CONTINUE; }
"return"	{ return token::KW_RETURN; }
"new"		{ return token::KW_NEW; }
"delete"    { return token::KW_DELETE; }
"from"		{ return token::KW_FROM; }
"or"		{ return token::KW_OR; }

 /* reserved but not currently used: */
"and"		{ return token::KW_AND; }
"xor"		{ return token::KW_XOR; }
"not"		{ return token::KW_NOT; }
"builtin"	{ return token::KW_BUILTIN; }
"extends"   { return token::KW_EXTENDS; }
"implements" { return token::KW_IMPLEMENTS; }
"func"      { return token::KW_FUNC; }
"lambda"    { return token::KW_LAMBDA; }
"tuple"     { return token::KW_TUPLE; }
"union"     { return token::KW_UNION; }
"enum"      { return token::KW_ENUM; }
"try"       { return token::KW_TRY; }
"except"    { return token::KW_EXCEPT; }
"finally"   { return token::KW_FINALLY; }
"raise"     { return token::KW_RAISE; }
"raises"    { return token::KW_RAISES; }

 /* string format operators */
 /* %[parameter][flags][width][.precision][length]type */
 /* %($[0-9]+)?[-+ 0#]*([1-9][0-9]*|\*)?(\.([0-9]+|\*))?[diufFeEgGxXoscaA] */
<INITIAL>{
"%"                 { BEGIN(STRINGFORMAT); return token::PERCENT; }
}

<STRINGFORMAT>{
($[0-9]+)           { yylval->build(std::string (yytext)); return token::SF_PARAM; }
[\-\+ 0#]*          { yylval->build(std::string (yytext)); return token::SF_FLAGS; }
[1-9][0-9]*|\*      { yylval->build(std::string (yytext)); return token::SF_WIDTH; }
\.([0-9]+|\*)       { yylval->build(std::string (yytext)); return token::SF_PREC; }
[diufFeEgGxXoscaA]  { yylval->build(std::string (yytext)); return token::SF_TYPE; }
"%"                 { BEGIN(INITIAL); return token::PERCENT; }
}


 /* literals */
"NULL"                  { return token::KW_NULL; }
"TRUE"                  { return token::KW_TRUE; }
"FALSE"                 { return token::KW_FALSE; }

{L}({L}|{D}|"#")*       { yylval->build(std::string (yytext)); return token::NAME; }
                        /* return yy::TxParser::make_NAME(std::string(yytext), loc); */

[1-9]{D}*"#"{WS}*("-"{WS}*)?({L}|{D})+("#"{IS})?  { yylval->build(std::string (yytext)); return token::LIT_RADIX_INT; }
{D}({D}|"_")*{IS}?	    { yylval->build(std::string (yytext)); return token::LIT_DEC_INT; }

{D}+{E}{FS}?		    { yylval->build(std::string (yytext)); return token::LIT_FLOATING; }
{D}*"."{D}+({E})?{FS}?	{ yylval->build(std::string (yytext)); return token::LIT_FLOATING; }
 /* {D}+"."{D}*({E})?{FS}?	{ yylval->build(std::string (yytext)); return token::LIT_FLOATING; } */

'(\\.|[^\\'])'	        { yylval->build(std::string (yytext)); return token::LIT_CHARACTER; }

\"(\\.|[^\\"])*\"	    { yylval->build(std::string (yytext)); return token::LIT_STRING; }
c\"(\\.|[^\\"])*\"      { yylval->build(std::string (yytext)); return token::LIT_CSTRING; }


.                       { parserCtx->cerror (*yylloc, "invalid character");
                          yyterminate(); }

%%