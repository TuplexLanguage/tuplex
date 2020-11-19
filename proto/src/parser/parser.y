%require "3.2"  // initially authored with Bison 3.0.2
%language "C++"
%skeleton "lalr1.cc"

%defines
%define api.parser.class {TxParser}
//%define api.token.constructor
%define api.value.type variant
//%define parse.assert

// Pass the parsing context to yylex() and yyparse()
%param { TxParserContext* parserCtx }

//%locations  // populates YYLTYPE
%define api.location.type {TxLocation}


// what YYSTYPE and YYLTYPE require:
%code requires
{
#include <string>

#include "parser/location.hpp"

#include "parsercontext.hpp"

#include "ast/ast_modbase.hpp"
#include "ast/expr/ast_array.hpp"
#include "ast/expr/ast_ref.hpp"
#include "ast/expr/ast_op_exprs.hpp"
#include "ast/expr/ast_exprs.hpp"
#include "ast/expr/ast_field.hpp"
#include "ast/expr/ast_lambda_node.hpp"
#include "ast/expr/ast_lit.hpp"
#include "ast/expr/ast_string.hpp"
#include "ast/expr/ast_range.hpp"
#include "ast/expr/ast_intrinsics.hpp"
#include "ast/type/ast_typecreating_node.hpp"
#include "ast/type/ast_types.hpp"
#include "ast/type/ast_qualtypes.hpp"
#include "ast/stmt/ast_flow.hpp"
#include "ast/stmt/ast_deletestmt_node.hpp"
#include "ast/stmt/ast_assertstmt_node.hpp"
#include "ast/stmt/ast_panicstmt_node.hpp"

struct TxModuleMembers {
    std::vector<TxDeclarationNode*> declarations;
    std::vector<TxModuleNode*> modules;
};
}

// Tell Flex the lexer's prototype:
%code provides
{
  yy::TxParser::token_type                         \
  yylex (yy::TxParser::semantic_type* yylval,      \
         yy::TxParser::location_type* yylloc,      \
         TxParserContext* parserCtx);
/*
//# define YY_DECL                    \
  yy::TxParser::token_type                         \
  yylex (yy::TxParser::semantic_type* yylval,      \
         yy::TxParser::location_type* yylloc,      \
         TxParserContext* parserCtx)
 declare yylex for the parser's sake
YY_DECL;
*/
}

%{
#include "tx_lang_defs.hpp"
#include "tx_operations.hpp"
#include "tx_error.hpp"
#include "txparser/scanner.hpp"

#define BEGIN_TXEXPERR(loc, expError) parserCtx->begin_exp_err(loc, expError)
#define END_TXEXPERR(loc)             parserCtx->end_exp_err(loc)
#define TX_SYNTAX_ERROR     { /* yyerrok (sets error flag yyerrstatus_ to 0) */ ; }

// modified YYLLOC_DEFAULT to ensure full location state copying:
#define YYLLOC_DEFAULT(Current, Rhs, N)                               \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).begin  = YYRHSLOC (Rhs, 1).begin;                   \
          (Current).end    = YYRHSLOC (Rhs, N).end;                     \
          (Current).parserCtx = YYRHSLOC (Rhs, 1).parserCtx;            \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).begin = (Current).end = YYRHSLOC (Rhs, 0).end;      \
          (Current).parserCtx = YYRHSLOC (Rhs, 0).parserCtx;            \
        }                                                               \
    while (/*CONSTCOND*/ false)
%}


%initial-action
{
    // Initialize the initial location.
    // Afterward new locations are computed relatively to the previous locations: the file name will be propagated.
    @$.begin.filename = @$.end.filename = parserCtx->current_input_filepath();
    @$.parserCtx = parserCtx;
};


// include parser instrumentation for tracing; displayed if variable yydebug / parser.set_debug_level() is set to non-zero value
%define parse.trace true  // (previously %debug)
%define parse.error verbose

// no longer used in C++ parser:
//%define parse.lac full  // look-ahead correction upon error ("experimental" feature of bison)
//%define api.pure full // define as pure, reentrant parser (also requires modification to lexer invocation)

// enable debug printout of symbol content
%printer { yyoutput << $$; } <std::string>;
%printer { yyoutput << "declflag " << $$; } <TxDeclarationFlags>;
%printer { yyoutput << $$; } <bool>;
//%printer { yyoutput << $$; } <*>;


/* Define our terminal symbols (tokens). This should match our .l lex file.
 We also define the node type they represent.
 */

/* operators: */
%token END 0  // end of scan
%token ERROR  // indicates lexcial error
%token WHITESPACE COMMENT  // needed for ids, will not be produced
%token INDENT DEDENT
%token NEWLINE SEMICOLON // statement separator
%token DOT COLON COMMA DOTDOT ELLIPSIS ASTERISK PLUS MINUS FSLASH BSLASH AAND
%token PIPE CARET TILDE AT PERCENT DOLLAR EURO LPAREN RPAREN LBRACE
%token RBRACE LBRACKET RBRACKET QMARK EMARK DASHGT LTCOLON COLONGT
%token EQUAL EEQUAL NEQUAL EEEQUAL NEEQUAL LT GT LEQUAL GEQUAL
%token LTLT GTGT GTGTGT
%token COLEQUAL PLUSEQUAL MINUSEQUAL ASTERISKEQUAL FSLASHEQUAL
%token SF_MINUS SF_PLUS SF_SPACE SF_ZERO SF_HASH  // used in string formats

/* keywords: */
%token KW_MODULE KW_IMPORT KW_TYPE KW_INTERFACE
%token KW_BUILTIN KW_VIRTUAL KW_ABSTRACT KW_FINAL KW_OVERRIDE KW_EXTERNC
%token KW_MUTABLE KW_REFERENCE KW_DERIVES
%token KW_WHILE KW_FOR KW_IF KW_ELSE KW_IN KW_IS
%token KW_RETURN KW_BREAK KW_CONTINUE KW_NEW KW_DELETE
%token KW_XOR
%token KW_TRUE KW_FALSE
%token KW_PANIC KW_ASSERT KW_EXPERR
%token KW__ADDRESS KW__TYPEID KW__SIZEOF KW__SUPERTYPES

/* keywords reserved but not currently used */
%token KW_PUBLIC KW_PROTECTED
%token KW_STATIC KW_CONST KW_EXTENDS KW_IMPLEMENTS
%token KW_SWITCH KW_CASE KW_WITH KW_AS
%token KW_AND KW_OR KW_NOT
%token KW_RAISES KW_TRY KW_EXCEPT KW_FINALLY KW_RAISE

 /* literals: */
%token <std::string> NAME LIT_DEC_INT LIT_RADIX_INT LIT_FLOATING LIT_CHARACTER LIT_CSTRING LIT_STRING
%token <std::string> SF_PARAM SF_FLAGS SF_WIDTH SF_PREC SF_TYPE
%token <std::string> HASHINIT HASHSELF

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above.
 */
%type <TxIdentifier*> module_identifier import_identifier
%type <TxIdentifier*> opt_module_decl
%type <TxIdentifierNode*> identifier dataspace

%type <TxDeclarationFlags> declaration_flags opt_visibility opt_externc opt_virtual opt_abstract opt_override opt_final opt_builtin
%type <bool> opt_mutable type_or_if
%type <TxParsingUnitNode*> parsing_unit
%type <TxModuleNode*> sub_module

%type <std::vector<TxImportNode*> *> import_statements opt_import_stmts
%type <TxImportNode *> import_statement

%type <TxModuleMembers*> module_members opt_module_members
%type <TxDeclarationNode *> member_declaration experr_decl type_param
%type <std::vector<TxDeclarationNode*> *> type_body member_list type_param_list
%type <TxTypeDeclNode*> type_declaration

%type <TxFieldValueNode*> named_symbol

%type <std::vector<TxTypeExpressionNode*> *> type_expr_list

%type <TxTypeArgumentNode *> type_arg
%type <std::vector<TxTypeArgumentNode*> *> type_arg_list

%type <TxNonLocalFieldDefNode*> field_def field_assignment_def method_def

%type <TxArgTypeDefNode*> func_arg_def
%type <std::vector<TxArgTypeDefNode*> *> func_args func_args_list

%type <TxTypeCreatingNode*> type_derivation
%type <TxQualTypeExprNode*> qual_type_expr
%type <TxTypeExpressionNode*> type_expression spec_type_expr type_production val_type_prod
%type <TxTypeExpressionNode*> reference_type array_type

%type <TxFunctionTypeNode*> function_signature
%type <TxExpressionNode*> val_expr gen_val_expr lambda_expr value_literal array_dimensions intrinsics_expr make_expr
%type <TxExpressionNode*> array_literal //tuple_literal
%type <TxFunctionCallNode*> call_expr
%type <std::vector<TxExpressionNode*> *> expression_list min2_expr_list call_params
%type <std::vector<TxStatementNode*> *> statement_list
%type <TxSuiteNode*> suite
%type <TxStatementNode*> statement single_statement assignment_stmt return_stmt break_stmt continue_stmt type_decl_stmt
%type <TxStatementNode*> flow_stmt simple_stmt elementary_stmt terminal_stmt flow_else_stmt
%type <TxStatementNode*> init_stmt delete_stmt assert_stmt panic_stmt experr_stmt
%type <TxElseClauseNode*> else_clause
%type <TxFlowHeaderNode*> cond_clause is_clause in_clause for_header
%type <std::vector<TxFlowHeaderNode*> *> in_clause_list
%type <TxAssigneeNode*> assignee_expr
%type <TxLocalFieldDefNode*> local_field_def
%type <TxMemberInitNode*> member_init_expr
%type <std::vector<TxMemberInitNode*> *> member_init_list

%type <TxExpressionNode*> string_format_expr
%type <TxStringFormatNode*> string_format
%type <StringFormatFlags> opt_sf_flags sf_flag_list sf_flag
%type <std::string>       opt_sf_width opt_sf_prec opt_sf_type


/* Operator precedence for expression operators (higher line no = higher precedence) */
%precedence STMT /* used to specify statement / member rule precedence, to be lower than e.g. separator  */
%precedence TYPE  // type expression
%precedence EXPR  // value expression
%precedence ELLIPSIS
%left COMMA COLON
%right EQUAL
%left PERCENTPERCENT PERCENT // string concatenation and formatting
%precedence STRFORMAT // unary string formatting
%right TILDE
%left PIPE        // boolean and bitwise operator
%left KW_XOR      // boolean and bitwise operator
%left AAND        // boolean and bitwise operator
%left EEQUAL NEQUAL
%left LT GT LEQUAL GEQUAL
%left EEEQUAL NEEQUAL
%right DOTDOT           // range has lower priority than arithmetic but higher than boolen operators
%left LTLT GTGT GTGTGT  // bit-shift har lower priority than arithmetic but higher than range and boolen operators
%left PLUS MINUS
%left ASTERISK FSLASH
%right NOT        /* unary logical not */
%right NEG        /* negation--unary minus */
%right ADDR       /* unary prefix address-of */
%precedence LPAREN RPAREN  LBRACE RBRACE  INDENT DEDENT
%precedence CARET /* unary postfix de-reference */
%precedence LBRACKET RBRACKET
%right DOT
%precedence ARRAY_LIT
%right KW_MODULE KW_IMPORT  /* high token shift precedence */
%right KW_ELSE
%right SEMICOLON NEWLINE    /* semantic statement separator, always/greedily shift */

%start parsing_unit

%%

parsing_unit : opt_module_decl
               opt_import_stmts
               opt_module_members
                   { auto module = new TxModuleNode( @$, $1, $2, &$3->declarations, &$3->modules );
                     $$ = new TxParsingUnitNode( @$, module );
                     parserCtx->validate_module_name( module, $1 );
                     parserCtx->parsingUnit = $$;
                   }
    ;

opt_module_members : %empty { $$ = new TxModuleMembers(); }
                   | module_members { $$ = $1; } ;

module_members : member_declaration
                    { $$ = new TxModuleMembers();
                      if ($1)  $$->declarations.push_back($1); }
               | sub_module
                    { $$ = new TxModuleMembers();
                      $$->modules.push_back($1); }
               | module_members member_declaration
                    { $$ = $1;
                      if ($2)  $$->declarations.push_back($2); }
               | module_members sub_module
                    { $$ = $1;
                      $$->modules.push_back($2); }
;

block_begin : INDENT | LBRACE
            ;
block_end   : DEDENT | RBRACE
            ;

sub_module : KW_MODULE module_identifier
               LBRACE  opt_import_stmts  opt_module_members  RBRACE
                 { $$ = new TxModuleNode( @$, $2, $4, &$5->declarations, &$5->modules );
                   parserCtx->validate_module_name( $$, $2 );
                 }
    ;

identifier          : NAME   { $$ = new TxIdentifierNode(@1, $1); } ;

module_identifier   : NAME                         { $$ = new TxIdentifier($1); }
                    | module_identifier DOT NAME   { $$ = $1; $$->append($3); }
                    ;

import_identifier   : NAME                              { $$ = new TxIdentifier($1); }
                    | import_identifier DOT NAME        { $$ = $1; $$->append($3); }
                    | import_identifier DOT ASTERISK    { $$ = $1; $$->append("*"); }
                    ;


eos        : NEWLINE | SEMICOLON ;
//opt_eos    : %empty | eos ;
opt_comma  : %empty | COMMA ;


opt_module_decl    : %empty %prec STMT { $$ = new TxIdentifier( LOCAL_NS ); }
                   | KW_MODULE module_identifier eos { $$ = $2; } ;

opt_import_stmts   : %empty { $$ = new std::vector<TxImportNode*>(); }
                   | import_statements { $$ = $1; } ;

import_statements  : import_statement
                     { $$ = new std::vector<TxImportNode*>(); if ($1) $$->push_back($1); }
                   | import_statements import_statement
                     { $$ = $1; if ($2) $$->push_back($2); }
                   ;

import_statement   : KW_IMPORT import_identifier eos  { $$ = new TxImportNode(@$, $2); }
                   | KW_IMPORT error eos              { $$ = NULL; }
                   ;


member_declaration
    // field
    : declaration_flags field_def eos
            { $$ = new TxFieldDeclNode(@$, $1, $2); }

    // type
    | type_declaration     { $$ = $1; }

    // function / method
    |   declaration_flags method_def
            { $$ = ( $2 ? new TxFieldDeclNode(@$, $1, $2, true) : NULL ); }

    | NEWLINE  { $$ = NULL; }

    // error recovery
    |   error eos  { $$ = NULL; }

    |   experr_decl      { $$ = $1; }
    ;

experr_decl : KW_EXPERR COLON              { BEGIN_TXEXPERR(@1, new ExpectedErrorClause()); }
              member_declaration           { $$ = new TxExpErrDeclNode(@$, END_TXEXPERR(@4), $4); }
            | KW_EXPERR LIT_DEC_INT COLON  { BEGIN_TXEXPERR(@1, new ExpectedErrorClause(std::stoi($2))); }
              member_declaration           { $$ = new TxExpErrDeclNode(@$, END_TXEXPERR(@5), $5); }
            ;


declaration_flags : opt_visibility opt_builtin opt_externc opt_virtual opt_abstract opt_override opt_final
                        { $$ = ($1 | $2 | $3 | $4 | $5 | $6 | $7); } ;

opt_visibility : %empty        { $$ = TXD_NONE; }
               | KW_PUBLIC     { $$ = TXD_PUBLIC; }
               | KW_PROTECTED  { $$ = TXD_PROTECTED; }
               ;
opt_builtin    : %empty { $$ = TXD_NONE; } | KW_BUILTIN  { $$ = TXD_BUILTIN;  } ;
opt_externc    : %empty { $$ = TXD_NONE; } | KW_EXTERNC  { $$ = TXD_EXTERNC;  } ;
opt_virtual    : %empty { $$ = TXD_NONE; } | KW_VIRTUAL  { $$ = TXD_VIRTUAL;  } ;
opt_abstract   : %empty { $$ = TXD_NONE; } | KW_ABSTRACT { $$ = TXD_ABSTRACT; } ;
opt_override   : %empty { $$ = TXD_NONE; } | KW_OVERRIDE { $$ = TXD_OVERRIDE; } ;
opt_final      : %empty { $$ = TXD_NONE; } | KW_FINAL    { $$ = TXD_FINAL;    } ;


type_or_if : KW_TYPE        { $$ = false; }
           | KW_INTERFACE   { $$ = true;  }
           ;

field_def : identifier COLON qual_type_expr  { $$ = new TxNonLocalFieldDefNode(@$, $1, $3, nullptr); }
          | field_assignment_def             { $$ = $1; }
          ;

field_assignment_def : identifier COLON qual_type_expr EQUAL gen_val_expr
                           { $$ = new TxNonLocalFieldDefNode(@$, $1, $3, $5); }
                     | identifier COLEQUAL gen_val_expr
                           { $$ = new TxNonLocalFieldDefNode(@$, $1, $3, false); }
;


//// types:

derives_token    : KW_DERIVES    | LTCOLON ;
ref_token        : KW_REFERENCE  | AAND ;
mut_token        : KW_MUTABLE    | TILDE ;

opt_mutable   : %empty { $$ = false; } | mut_token { $$ = true; } ;

type_body
    :   block_begin block_end                   { $$ = new std::vector<TxDeclarationNode*>(); }
    |   block_begin member_list block_end       { $$ = $2; }
    |   block_begin error block_end             { $$ = new std::vector<TxDeclarationNode*>(); TX_SYNTAX_ERROR; }
    |   block_begin member_list error block_end { $$ = $2;                                    TX_SYNTAX_ERROR; }
    ;

member_list : member_declaration
                     { $$ = new std::vector<TxDeclarationNode*>();
                       if ($1 != NULL)
                           $$->push_back($1); }
            | member_list member_declaration
                     { $$ = $1;
                       if ($2 != NULL)
                           $$->push_back($2); }
            ;

type_param_list : type_param  { $$ = new std::vector<TxDeclarationNode*>(); $$->push_back($1); }
                | type_param_list COMMA type_param  { $$ = $1; $$->push_back($3); }
                ;
type_param      : identifier  { $$ = new TxTypeDeclNode( @$, TXD_PUBLIC | TXD_GENPARAM, $1, NULL,
                                                   new TxGenParamTypeNode( @$, new TxNamedTypeNode(@$, "tx.Any") ) ); }
                | identifier derives_token type_expression
                        { $$ = new TxTypeDeclNode( @$, TXD_PUBLIC | TXD_GENPARAM, $1, NULL,
                                                   new TxGenParamTypeNode( @$, $3 ) ); }
                | identifier COLON qual_type_expr
                        { $$ = new TxFieldDeclNode( @$, TXD_PUBLIC | TXD_GENPARAM, new TxNonLocalFieldDefNode( @$, $1, $3, nullptr ) ); }
                ;

type_declaration : declaration_flags type_or_if opt_mutable identifier type_derivation
                        { $$ = new TxTypeDeclNode(@$, $1, $4, NULL, $5, $2, $3); }
                 | declaration_flags type_or_if opt_mutable identifier LBRACE type_param_list RBRACE type_derivation
                        { $$ = new TxTypeDeclNode(@$, $1, $4, $6,   $8, $2, $3); }

                 // error recovery, handles when an error occurs before a type body's LBRACE:
                 | error type_body  { $$ = NULL; }
                 ;

type_derivation : derives_token type_expression eos              { $$ = new TxDerivedTypeNode(@$, $2); }
                | derives_token type_expression COLON type_body  { $$ = new TxDerivedTypeNode(@$, $2, $4); }
                | derives_token type_expression COMMA type_expr_list COLON type_body
                                                                 { $$ = new TxDerivedTypeNode(@$, $2, $4, $6); }
                | derives_token type_expression COMMA type_expr_list eos
                                                                 { $$ = new TxDerivedTypeNode(@$, $2, $4); }
                | COLON type_body                                { $$ = new TxDerivedTypeNode(@$, $2); }

                | derives_token error eos  { $$ = new TxDerivedTypeNode(@$, (TxTypeExpressionNode*)nullptr); }
                | derives_token error COLON type_body  { $$ = new TxDerivedTypeNode(@$, $4); }
                ;

type_expr_list  : type_expression  { $$ = new std::vector<TxTypeExpressionNode*>();  $$->push_back($1); }
                | type_expr_list COMMA type_expression  { $$ = $1;  $$->push_back($3); }
                ;


/// function type and function declarations:

lambda_expr : function_signature COLON statement  { $$ = new TxLambdaExprNode(@$, $1, $3); } ;

function_signature : func_args opt_mutable DASHGT type_expression
                        { $$ = new TxFunctionTypeNode(@$, $2, $1, $4); }
                   | func_args opt_mutable
                        { $$ = new TxFunctionTypeNode(@$, $2, $1, NULL); }
                   ;

func_args : LPAREN func_args_list RPAREN  { $$ = $2; }
          | LPAREN RPAREN  { $$ = new std::vector<TxArgTypeDefNode*>(); }
          ;

func_args_list : func_arg_def
                      { $$ = new std::vector<TxArgTypeDefNode*>();
                        $$->push_back($1); }
               | func_args_list COMMA func_arg_def
                      { $$ = $1;
                        $$->push_back($3); }
               | error  { $$ = new std::vector<TxArgTypeDefNode*>(); }
               ;

func_arg_def   : identifier COLON type_expression
                        { $$ = new TxArgTypeDefNode(@$, $1, $3); }
               | identifier COLON type_expression ELLIPSIS
                        { $$ = new TxArgTypeDefNode(@$, $1,
                                    new TxReferenceTypeNode(@3, nullptr,
                                            new TxConstTypeNode( @3, new TxArrayTypeNode(@3, new TxMaybeModTypeNode(@3, $3))))); }  // TODO: remove const and mod nodes?
               | identifier COLON mut_token type_expression ELLIPSIS
                        { $$ = new TxArgTypeDefNode(@$, $1,
                                    new TxReferenceTypeNode(@3, nullptr,
                                            new TxConstTypeNode( @3, new TxArrayTypeNode(@3, new TxModifiableTypeNode(@3, $4))))); }  // TODO: remove const and mod nodes?
               ;


method_def  : identifier function_signature COLON statement
                { $$ = new TxNonLocalFieldDefNode(@$, $1, new TxLambdaExprNode(@$, $2, $4, true), false); }
            | identifier function_signature eos  // abstract method (KW_ABSTRACT should be specified)
                { $$ = new TxNonLocalFieldDefNode(@$, $1, $2, nullptr); }
            | identifier error eos  { $$ = nullptr; }
            | identifier error suite  { $$ = nullptr; }
            ;



///////////////////
//// expressions:

named_symbol    : identifier                       { $$ = new TxFieldValueNode(@$, NULL, $1); }
                | named_symbol    DOT identifier   { $$ = new TxFieldValueNode(@$, $1,   $3); }
                | spec_type_expr  DOT identifier   { $$ = new TxFieldValueNode(@$, $1,   $3); }
                ;

// a qualified type expression - a type expression with possible qualifiers (e.g. mutable):
qual_type_expr  : opt_mutable type_expression
                         { $$ = ( $1 ? new TxModifiableTypeNode(@$, $2)
                                     : new TxQualTypeExprNode(@2, $2) ); }
                ;

// can identify or construct new type but can't extend (specify interfaces or add members to) one:
type_expression : named_symbol        %prec TYPE  { $$ = new TxNamedTypeNode(@$, $1); }
                | type_production                 { $$ = $1; }
                ;

type_production : reference_type             { $$ = $1; }
                | val_type_prod              { $$ = $1; }
                ;

val_type_prod   : spec_type_expr             { $$ = $1; }
                | array_type                 { $$ = $1; }
                | function_signature  %prec TYPE  { $$ = $1; }  // Note: has lower precedence than lambda val_expr (COLON)
                //| LBRACE qual_type_expr RBRACE { $$ = $2; }
                ;
//    |  data_tuple_type  - same syntactical construct as function args?
//    |  union_type
//    |  enum_type
//    |  shared_obj_type

spec_type_expr  : named_symbol LBRACE type_arg_list
                  { parserCtx->scanCtx->nl_after_rbrace( true ); }  RBRACE
                  { $$ = new TxGenSpecTypeNode(@$, new TxNamedTypeNode(@1, $1), $3); }
                ;


type_arg_list   : type_arg  { $$ = new std::vector<TxTypeArgumentNode*>();  $$->push_back($1); }
                | type_arg_list COMMA type_arg  { $$ = $1;  $$->push_back($3); }
                ;

type_arg        : type_production              { $$ = new TxTypeArgumentNode($1); }
                | mut_token type_production    { $$ = new TxTypeArgumentNode(new TxModifiableTypeNode(@$, $2)); }
                | mut_token val_expr
                    {
                        // special handling to strip TxNamedFieldNode (this way avoids reduce-reduce grammar conflict)
                        if ( auto namedField = dynamic_cast<TxNamedFieldNode*>( $2 ) )
                            $$ = new TxTypeArgumentNode(new TxModifiableTypeNode(@$, new TxNamedTypeNode(@2, namedField->exprNode)));
                        else
                            $$ = new TxTypeArgumentNode(new TxModifiableTypeNode(@$, new TxNamedTypeNode(@2, $2)));
                    }
                | val_expr                     // including named type
                    {
                        // special handling to strip TxNamedFieldNode (this way avoids reduce-reduce grammar conflict)
                        if ( auto namedField = dynamic_cast<TxNamedFieldNode*>( $1 ) )
                            $$ = new TxTypeArgumentNode(namedField->exprNode);
                        else
                            $$ = new TxTypeArgumentNode($1);
                    }
                ;


reference_type : ref_token qual_type_expr            { $$ = new TxReferenceTypeNode(@$, nullptr, $2); }
               | dataspace ref_token qual_type_expr  { $$ = new TxReferenceTypeNode(@$, $1, $3); }
               ;

dataspace      :  QMARK { $$ = NULL; }  |  identifier EMARK { $$ = $1; } ;


/* (custom ast node needed to provide syntactic sugar for modifiable decl) */
array_type       : array_dimensions type_expression  { $$ = new TxArrayTypeNode(@$, new TxMaybeModTypeNode(@2, $2), $1); }
                 | array_dimensions mut_token type_expression
                        { $$ = new TxModifiableTypeNode(@$, new TxArrayTypeNode(@$, new TxModifiableTypeNode(@2, $3), $1)); }
                 ;

array_dimensions : LBRACKET val_expr RBRACKET  { $$ = $2; }
                 //| LBRACKET conv_type_expr RBRACKET  // type must be an enum
                 | LBRACKET RBRACKET  { $$ = NULL; }
                 ;


//// (value) expressions:

gen_val_expr
    :   val_expr                           { $$ = $1; }
    |   AAND gen_val_expr   %prec ADDR     { $$ = new TxReferenceToNode(@$, $2); }

    |   gen_val_expr EEQUAL gen_val_expr   { $$ = new TxEqualityOperatorNode(@$, $1, $3); }
    |   gen_val_expr NEQUAL gen_val_expr   { $$ = new TxUnaryLogicalNotNode(@$, new TxEqualityOperatorNode(@$, $1, $3)); }
    |   gen_val_expr EEEQUAL gen_val_expr  { $$ = new TxRefEqualityOperatorNode(@$, $1, $3); }
    |   gen_val_expr NEEQUAL gen_val_expr  { $$ = new TxUnaryLogicalNotNode(@$, new TxRefEqualityOperatorNode(@$, $1, $3)); }

    |   gen_val_expr AAND gen_val_expr     { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_AND, $3); }
    |   gen_val_expr PIPE gen_val_expr     { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_OR,  $3); }
    |   gen_val_expr KW_XOR gen_val_expr   { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_XOR,  $3); }

    |   mut_token gen_val_expr %prec TILDE { $$ = new TxModifiableValueNode(@$, $2); }
    ;

val_expr
    :   LPAREN gen_val_expr RPAREN   { $$ = $2; }
    |   value_literal                { $$ = $1; }
//    |   tuple_literal                { $$ = $1; }
    |   array_literal                { $$ = $1; }
    |   lambda_expr                  { $$ = $1; }
    |   intrinsics_expr              { $$ = $1; }

    |   named_symbol     %prec EXPR  { $$ = new TxNamedFieldNode(@$, $1); }
    |   val_expr DOT identifier      { $$ = new TxNamedFieldNode(@$, new TxFieldValueNode(@$, $1, $3)); }
    |   val_expr LBRACKET val_expr RBRACKET  { $$ = new TxElemDerefNode(@$, $1, $3); }
    |   val_expr CARET               { $$ = new TxReferenceDerefNode(@$, $1); }

    |   call_expr                    { $$ = $1; }
    |   make_expr                    { $$ = $1; }

    |   val_expr DOTDOT val_expr     { $$ = TxERangeLitNode::make_range_node(@$, $1, $3); }

    |   MINUS val_expr  %prec NEG    { $$ = new TxUnaryMinusNode(@$, $2); }  // unary minus
    |   val_expr PLUS val_expr       { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_PLUS, $3); }
    |   val_expr MINUS val_expr      { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_MINUS, $3); }
    |   val_expr ASTERISK val_expr   { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_MUL, $3); }
    |   val_expr FSLASH val_expr     { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_DIV, $3); }
    |   val_expr LT val_expr         { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_LT, $3); }
    |   val_expr GT val_expr         { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_GT, $3); }
    |   val_expr LEQUAL val_expr     { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_LE, $3); }
    |   val_expr GEQUAL val_expr     { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_GE, $3); }

    |   EMARK val_expr  %prec NOT    { $$ = new TxUnaryLogicalNotNode(@$, $2); }  // unary not
    |   val_expr LTLT val_expr      %prec LTLT  { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_LSHIFT, $3); }
    |   val_expr GT GT val_expr     %prec LTLT  { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_RSHIFT, $4); }
    |   val_expr GT GT GT val_expr  %prec LTLT  { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_ARSHIFT, $5); }

    |   string_format_expr           { $$ = $1; }
    ;

value_literal
        :       LIT_DEC_INT   { $$ = new TxIntegerLitNode(@1, $1, false); }
        |       LIT_RADIX_INT { $$ = new TxIntegerLitNode(@1, $1, true); }
        |       LIT_FLOATING  { $$ = new TxFloatingLitNode(@1, $1); }
        |       LIT_CHARACTER { $$ = new TxCharacterLitNode(@1, $1); }
        |       LIT_CSTRING   { $$ = new TxCStringLitNode(@1, $1); }
        |       LIT_STRING    { $$ = new TxStringLitNode(@1, $1); }
        |       KW_FALSE      { $$ = new TxBoolLitNode(@1, false); }
        |       KW_TRUE       { $$ = new TxBoolLitNode(@1, true); }
    ;

/*
tuple_literal :        LPAREN min2_expr_list opt_comma RPAREN  { $$ = new TxTupleLitNode(@$, $2); }
              | BSLASH LPAREN min2_expr_list opt_comma RPAREN  { $$ = new TxTupleLitNode(@$, $3); }
              |        LPAREN gen_val_expr       COMMA RPAREN  { $$ = new TxTupleLitNode(@$, new std::vector<TxExpressionNode*>({$2})); }
              | BSLASH LPAREN gen_val_expr             RPAREN  { $$ = new TxTupleLitNode(@$, new std::vector<TxExpressionNode*>({$3})); }
              | BSLASH LPAREN gen_val_expr       COMMA RPAREN  { $$ = new TxTupleLitNode(@$, new std::vector<TxExpressionNode*>({$3})); }
              | BSLASH LPAREN                          RPAREN  { $$ = new TxTupleLitNode(@$, new std::vector<TxExpressionNode*>()); }
              ;
*/
array_literal :        LBRACKET min2_expr_list opt_comma RBRACKET  { $$ = new TxFilledArrayLitNode(@$, $2); }
              | BSLASH LBRACKET min2_expr_list opt_comma RBRACKET  { $$ = new TxFilledArrayLitNode(@$, $3); }
              |        LBRACKET gen_val_expr       COMMA RBRACKET  { $$ = new TxFilledArrayLitNode(@$, new std::vector<TxExpressionNode*>({$2})); }
              | BSLASH LBRACKET gen_val_expr             RBRACKET  { $$ = new TxFilledArrayLitNode(@$, new std::vector<TxExpressionNode*>({$3})); }
              | BSLASH LBRACKET gen_val_expr       COMMA RBRACKET  { $$ = new TxFilledArrayLitNode(@$, new std::vector<TxExpressionNode*>({$3})); }
              // BSLASH LBRACKET RBRACKET - empty, unqualified array literal "[]" illegal since element type can't be determined
              ;

// a list of two or more comma-separated value expressions
min2_expr_list : gen_val_expr COMMA gen_val_expr   { $$ = new std::vector<TxExpressionNode*>( { $1, $3 } ); }
               | min2_expr_list COMMA gen_val_expr  { $$ = $1;  $$->push_back($3); }
               ;


make_expr : KW_NEW qual_type_expr call_params  { $$ = new TxNewConstructionNode(@$, $2, $3); }
          |         val_type_prod call_params  { $$ = new TxStackConstructionNode(@$, new TxQualTypeExprNode(@1, $1), $2); }
;

call_expr : val_expr call_params  { $$ = new TxFunctionCallNode(@$, $1, $2); }
;

call_params : LPAREN expression_list RPAREN  { $$ = $2; }
            | LPAREN RPAREN  { $$ = new std::vector<TxExpressionNode*>(); }
;

expression_list : gen_val_expr  { $$ = new std::vector<TxExpressionNode*>({$1}); }
                | expression_list COMMA gen_val_expr  { $$ = $1;  $$->push_back($3); }
                ;

intrinsics_expr : KW__ADDRESS LPAREN gen_val_expr RPAREN     { $$ = new TxRefAddressNode(@$, $3); }
                | KW__TYPEID  LPAREN gen_val_expr RPAREN     { $$ = new TxRefTypeIdNode(@$, $3); }
                | KW__TYPEID  LBRACE qual_type_expr
                  { parserCtx->scanCtx->nl_after_rbrace( true ); }  RBRACE  { $$ = new TxTypeExprTypeIdNode(@$, $3); }
                | KW__SIZEOF  LPAREN gen_val_expr RPAREN     { $$ = new TxSizeofExprNode(@$, $3); }
                | KW__SUPERTYPES LPAREN gen_val_expr RPAREN  { $$ = new TxSupertypesExprNode(@$, $3); }
                ;


string_format_expr : string_format val_expr           %prec STRFORMAT
                        { $$ = new TxStackConstructionNode( @$, new TxConstTypeNode( @$, new TxNamedTypeNode( @$, "tx.FormattedStringer" ) ),
                                                            new std::vector<TxExpressionNode*>( { $1, $2 } ) ); }
                   | val_expr string_format val_expr  %prec PERCENT
                        { $$ = TxConcatenateStringsNode::make_strcat_node( @$, $1,
                                    new TxStackConstructionNode( @2, new TxConstTypeNode( @2, new TxNamedTypeNode( @2, "tx.FormattedStringer" ) ),
                                                                        new std::vector<TxExpressionNode*>( { $2, $3 } ) ) ); }
                   | val_expr PERCENTPERCENT val_expr
                        { $$ = TxConcatenateStringsNode::make_strcat_node( @$, $1, $3 ); }
                   ;

string_format   : PERCENT opt_sf_flags opt_sf_width opt_sf_prec opt_sf_type
                        { $$ = new TxStringFormatNode( @$, $2, $3, $4, $5.front() ); }
                ;

opt_sf_flags    : %empty { $$ = SF_NONE; } | sf_flag_list { $$ = $1; } ;
opt_sf_width    : %empty { $$ = ""; } | SF_WIDTH { $$ = $1; } ;
opt_sf_prec     : %empty { $$ = ""; } | SF_PREC { $$ = $1; } ;
opt_sf_type     : %empty { $$ = ""; } | SF_TYPE { $$ = $1; } ;

sf_flag_list    : sf_flag              { $$ = $1; }
                | sf_flag_list sf_flag { $$ = (StringFormatFlags)($1 | $2); }
                ;

sf_flag         : SF_MINUS { $$ = SF_MINUS; }
                | SF_PLUS  { $$ = SF_PLUS; }
                | SF_SPACE { $$ = SF_SPACE; }
                | SF_ZERO  { $$ = SF_ZERO; }
                | SF_HASH  { $$ = SF_HASH; }
                ;


//// statements

suite
    :   block_begin block_end                      { $$ = new TxSuiteNode(@$); }
    |   block_begin statement_list block_end       { $$ = new TxSuiteNode(@$, $2); }
    |   block_begin error block_end                { $$ = new TxSuiteNode(@$);     TX_SYNTAX_ERROR; }
    |   block_begin statement_list error block_end { $$ = new TxSuiteNode(@$, $2); TX_SYNTAX_ERROR; }
    ;

statement_list : statement  { $$ = new std::vector<TxStatementNode*>();
                              if ($1) $$->push_back($1); }
               | statement_list statement  { $$ = $1;  if ($2) $$->push_back($2); }
               ;

// statement is a syntactically terminated program statement, either with a separator token,
// or in the case of a suite with a }.
// Conditional statements can be seen as statements prefixed with a condition clause
// (which in itself is not syntactically terminated).
statement
    :   single_statement                         { $$ = $1; }
    |   suite                                    { $$ = $1; }
    ;

single_statement
    :   flow_stmt                  %prec STMT    { $$ = $1; }
    |   simple_stmt                %prec STMT    { $$ = $1; }
    ;

simple_stmt
    :   type_decl_stmt          %prec STMT    { $$ = $1; }
    |   elementary_stmt eos     %prec STMT    { $$ = $1; }
    |   terminal_stmt   eos     %prec STMT    { $$ = $1; }
    |   init_stmt       eos     %prec STMT    { $$ = $1; }
    |   flow_else_stmt          %prec KW_ELSE { $$ = $1; }
    |   experr_stmt             %prec STMT    { $$ = $1; }
    |   error eos               %prec STMT    { $$ = new TxNoOpStmtNode(@$); TX_SYNTAX_ERROR; }
    |   eos                     %prec STMT    { $$ = new TxNoOpStmtNode(@$); }
    ;

elementary_stmt
    :   local_field_def { $$ = new TxFieldStmtNode(@1, $1); }
    |   call_expr       { $$ = new TxCallStmtNode(@1, $1); } // function call without return value assignment
    |   assignment_stmt { $$ = $1; }
    |   delete_stmt     { $$ = $1; }
    |   assert_stmt     { $$ = $1; }
    |   panic_stmt      { $$ = $1; }
    ;

terminal_stmt
    :   return_stmt     { $$ = $1; }
    |   break_stmt      { $$ = $1; }
    |   continue_stmt   { $$ = $1; }
    ;

experr_stmt : KW_EXPERR COLON              { BEGIN_TXEXPERR(@1, new ExpectedErrorClause()); }
              statement                    { $$ = new TxExpErrStmtNode(@$, END_TXEXPERR(@4), static_cast<TxStatementNode*>($4)); }
            | KW_EXPERR LIT_DEC_INT COLON  { BEGIN_TXEXPERR(@1, new ExpectedErrorClause(std::stoi($2))); }
              statement                    { $$ = new TxExpErrStmtNode(@$, END_TXEXPERR(@5), $5); }
            ;


flow_stmt        : KW_IF    cond_clause    COLON single_statement  %prec STMT  { $$ = new TxIfStmtNode (@$, $2, $4); }
                 | KW_IF    cond_clause    COLON suite             %prec STMT  { $$ = new TxIfStmtNode (@$, $2, $4); }
                 | KW_IF    is_clause      COLON single_statement  %prec STMT  { $$ = new TxIfStmtNode (@$, $2, $4); }
                 | KW_IF    is_clause      COLON suite             %prec STMT  { $$ = new TxIfStmtNode (@$, $2, $4); }
                 | KW_WHILE cond_clause    COLON single_statement  %prec STMT  { $$ = new TxForStmtNode(@$, $2, $4); }
                 | KW_WHILE cond_clause    COLON suite             %prec STMT  { $$ = new TxForStmtNode(@$, $2, $4); }
                 | KW_FOR   in_clause_list COLON single_statement  %prec STMT  { $$ = new TxForStmtNode(@$, $2, $4); }
                 | KW_FOR   in_clause_list COLON suite             %prec STMT  { $$ = new TxForStmtNode(@$, $2, $4); }
                 | KW_FOR   for_header     COLON single_statement  %prec STMT  { $$ = new TxForStmtNode(@$, $2, $4); }
                 | KW_FOR   for_header     COLON suite             %prec STMT  { $$ = new TxForStmtNode(@$, $2, $4); }
                 ;

flow_else_stmt   : KW_IF    cond_clause    COLON simple_stmt else_clause  { $$ = new TxIfStmtNode (@$, $2, $4, $5); }
                 | KW_IF    cond_clause    COLON suite       else_clause  { $$ = new TxIfStmtNode (@$, $2, $4, $5); }
                 | KW_IF    is_clause      COLON simple_stmt else_clause  { $$ = new TxIfStmtNode (@$, $2, $4, $5); }
                 | KW_IF    is_clause      COLON suite       else_clause  { $$ = new TxIfStmtNode (@$, $2, $4, $5); }
                 | KW_WHILE cond_clause    COLON simple_stmt else_clause  { $$ = new TxForStmtNode(@$, $2, $4, $5); }
                 | KW_WHILE cond_clause    COLON suite       else_clause  { $$ = new TxForStmtNode(@$, $2, $4, $5); }
                 | KW_FOR   in_clause_list COLON simple_stmt else_clause  { $$ = new TxForStmtNode(@$, $2, $4, $5); }
                 | KW_FOR   in_clause_list COLON suite       else_clause  { $$ = new TxForStmtNode(@$, $2, $4, $5); }
                 | KW_FOR   for_header     COLON simple_stmt else_clause  { $$ = new TxForStmtNode(@$, $2, $4, $5); }
                 | KW_FOR   for_header     COLON suite       else_clause  { $$ = new TxForStmtNode(@$, $2, $4, $5); }
                 ;

// colon is currently optional since unambiguous
else_clause      : KW_ELSE COLON statement    { $$ = new TxElseClauseNode(@$, $3); }
                 | KW_ELSE statement          { $$ = new TxElseClauseNode(@$, $2); }
                 ;


cond_clause      : gen_val_expr %prec STMT    { $$ = new TxCondClauseNode( @1, $1 ); } ;

is_clause        : gen_val_expr KW_IS identifier COLON qual_type_expr  { $$ = new TxIsClauseNode(@$, $1, $3, $5); } ;

in_clause_list   : in_clause                        { $$ = new std::vector<TxFlowHeaderNode*>( { $1 } ); }
                 | in_clause_list COMMA in_clause   { $$ = $1; $$->push_back($3); }
                 | error                            { $$ = new std::vector<TxFlowHeaderNode*>(); }
                 ;

in_clause        : identifier KW_IN gen_val_expr                   { $$ = new TxInClauseNode( @$, $1, $3 ); }
                 | identifier COMMA identifier KW_IN gen_val_expr  { $$ = new TxInClauseNode( @$, $1, $3, $5 ); }
                 ;

for_header       : elementary_stmt eos gen_val_expr eos elementary_stmt  { $$ = new TxForHeaderNode( @$, $1, $3, $5 ); }
                 ;


local_field_def  : identifier COLON qual_type_expr                      { $$ = new TxLocalFieldDefNode(@$, $1, $3, nullptr); }
                 | identifier COLON qual_type_expr EQUAL gen_val_expr   { $$ = new TxLocalFieldDefNode(@$, $1, $3, $5); }
                 | identifier COLEQUAL gen_val_expr                     { $$ = new TxLocalFieldDefNode(@$, $1, $3, false); }
;


// TODO: support declaration flags abstract, final, and maybe static
type_decl_stmt   : type_or_if opt_mutable identifier type_derivation
                     { $$ = new TxTypeStmtNode(@$, $3, NULL, $4, $1, $2); }
                 | type_or_if opt_mutable identifier LBRACE type_param_list RBRACE type_derivation
                     { $$ = new TxTypeStmtNode(@$, $3, $5,   $7, $1, $2); }

                 // error recovery, handles when an error occurs before a type body's LBRACE:
                 | error type_body  { $$ = new TxNoOpStmtNode(@$); TX_SYNTAX_ERROR; }
                 ;


member_init_expr : identifier call_params  { $$ = new TxMemberInitNode(@$, $1, $2); }
                 ;

member_init_list : member_init_expr                         { $$ = new std::vector<TxMemberInitNode*>( { $1 } ); }
                 | member_init_list COMMA member_init_expr  { $$ = $1; $$->push_back($3); }
                 | error                                    { $$ = new std::vector<TxMemberInitNode*>(); }
                 ;

// special #init ... ; / #self() function call from within constructor
init_stmt   : HASHINIT COLON member_init_list   { $$ = new TxInitStmtNode(@$, $3); }
            | HASHINIT                          { $$ = new TxInitStmtNode(@$); }
            | HASHSELF call_params              { $$ = new TxInitStmtNode(@$, $2); }
            ;

return_stmt : KW_RETURN gen_val_expr  { $$ = new TxReturnStmtNode(@$, $2); }
            | KW_RETURN       { $$ = new TxReturnStmtNode(@$); }
            ;

break_stmt     : KW_BREAK     { $$ = new TxBreakStmtNode(@$); }  ;
continue_stmt  : KW_CONTINUE  { $$ = new TxContinueStmtNode(@$); }  ;

assert_stmt : KW_ASSERT gen_val_expr  { $$ = new TxAssertStmtNode(@$, $2); }
            // | KW_ASSERT gen_val_expr COMMA gen_val_expr
            ;

panic_stmt : KW_PANIC gen_val_expr  { $$ = new TxPanicStmtNode(@$, $2); }
           ;

delete_stmt : KW_DELETE gen_val_expr  { $$ = new TxDeleteStmtNode(@$, $2); }
            ;


assignment_stmt : assignee_expr EQUAL gen_val_expr  { $$ = new TxAssignStmtNode(@$, $1, $3); }
//                | assignee_expr PLUSEQUAL val_expr
//                | assignee_expr MINUSEQUAL val_expr
//                | assignee_expr ASTERISKEQUAL val_expr
//                | assignee_expr FSLASHEQUAL val_expr
                ;

//assignment_stmt  : assignee_pattern EQUAL gen_val_expr ;
//assignee_pattern : nested_assignee (',' nested_assignee)* ;
//nested_assignee  : assignee_expr | '{' assignee_pattern '}' ;

assignee_expr  // expressions capable of (but not guaranteed) to produce an lvalue
//    :   identifier                          { $$ = new TxFieldAssigneeNode(@$, new TxFieldValueNode(@1, NULL, $1)); }
//    |   val_expr DOT identifier             { $$ = new TxFieldAssigneeNode(@$, new TxFieldValueNode(@3, $1,   $3)); }
    :   named_symbol                        { $$ = new TxFieldAssigneeNode(@$, $1); }
    |   val_expr CARET                      { $$ = new TxDerefAssigneeNode(@$, $1); }  // unary value-of suffix
    |   val_expr LBRACKET val_expr RBRACKET { $$ = new TxElemAssigneeNode(@$, $1, $3); }
    ;

%%

void yy::TxParser::error (const location_type& l, const std::string& m) {
    parserCtx->cerror (l, m);
}
