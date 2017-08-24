%require "3.0"  // initially authored with Bison 3.0.2
%language "C++"
%skeleton "lalr1.cc"

%defines
%define parser_class_name {TxParser}
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
#include "ast/type/ast_types.hpp"
#include "ast/stmt/ast_flow.hpp"
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
# define YY_DECL                    \
  yy::TxParser::token_type                         \
  yylex (yy::TxParser::semantic_type* yylval,      \
         yy::TxParser::location_type* yylloc,      \
         TxParserContext* parserCtx)
// declare yylex for the parser's sake
YY_DECL;
}

%{
#include "tx_lang_defs.hpp"
#include "tx_operations.hpp"
#include "tx_error.hpp"

#define BEGIN_TXEXPERR(loc, expError) parserCtx->begin_exp_err(loc, expError)
#define END_TXEXPERR(loc)             parserCtx->end_exp_err(loc)
#define TX_SYNTAX_ERROR     { yyerrok; }

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
%token NL SEMICOLON // statement separator
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
%token KW_WHILE KW_FOR KW_IF KW_ELSE KW_IN
%token KW_RETURN KW_BREAK KW_CONTINUE KW_NEW KW_DELETE
%token KW_XOR
%token KW_NULL KW_TRUE KW_FALSE
%token KW_PANIC KW_ASSERT KW_EXPERR
%token KW__ADDRESS KW__TYPEID KW__SIZEOF

/* keywords reserved but not currently used */
%token KW_PUBLIC KW_PROTECTED
%token KW_STATIC KW_CONST KW_EXTENDS KW_IMPLEMENTS
%token KW_SWITCH KW_CASE KW_WITH KW_AS KW_IS
%token KW_AND KW_OR KW_NOT
%token KW_RAISES KW_TRY KW_EXCEPT KW_FINALLY KW_RAISE

 /* literals: */
%token <std::string> NAME LIT_DEC_INT LIT_RADIX_INT LIT_FLOATING LIT_CHARACTER LIT_CSTRING LIT_STRING
%token <std::string> STR_FORMAT SF_PARAM SF_FLAGS SF_WIDTH SF_PREC SF_TYPE

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above.
 */
%type <TxIdentifier*> compound_identifier import_identifier 
%type <TxIdentifier*> opt_module_decl opt_dataspace

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

%type <TxIdentifiedSymbolNode*> named_symbol

%type <std::vector<TxTypeExpressionNode*> *> conv_type_list

%type <TxTypeArgumentNode *> type_arg
%type <std::vector<TxTypeArgumentNode*> *> type_arg_list

%type <TxNonLocalFieldDefNode*> field_def field_assignment_def method_def

%type <TxArgTypeDefNode*> func_arg_def
%type <std::vector<TxArgTypeDefNode*> *> func_args func_args_list

%type <TxTypeExpressionNode*> qual_type_expr
%type <TxTypeExpressionNode*> type_expression type_derivation conv_type_expr produced_type
%type <TxTypeExpressionNode*> named_type specialized_type reference_type array_type

%type <TxFunctionTypeNode*> function_signature
%type <TxExpressionNode*> expr make_expr lambda_expr value_literal array_literal array_dimensions cond_expr intrinsics_expr
%type <TxFunctionCallNode*> call_expr
%type <std::vector<TxExpressionNode*> *> expression_list call_params array_lit_expr_list
%type <std::vector<TxStatementNode*> *> statement_list
%type <TxSuiteNode*> suite
%type <TxStatementNode*> statement single_statement assignment_stmt return_stmt break_stmt continue_stmt type_decl_stmt
%type <TxStatementNode*> flow_stmt simple_stmt elementary_stmt terminal_stmt flow_else_stmt
%type <TxStatementNode*> assert_stmt panic_stmt experr_stmt
%type <TxElseClauseNode*> else_clause
%type <TxInClauseNode*> in_clause
%type <TxForHeaderNode*> for_header
%type <std::vector<TxLoopHeaderNode*> *> in_clause_list
%type <TxAssigneeNode*> assignee_expr
%type <TxLocalFieldDefNode*> local_field_def

%type <TxExpressionNode*> string_format_expr
%type <TxStringFormatNode*> string_format
%type <StringFormatFlags> opt_sf_flags sf_flag_list sf_flag
%type <std::string>       opt_sf_width opt_sf_prec opt_sf_type


/* Operator precedence for expression operators (higher line no = higher precedence) */
%precedence STMT /* used to specify statement / member rule precedence, to be lower than e.g. separator  */
%precedence EXPR
%precedence ELLIPSIS
%left COMMA COLON
%right EQUAL
%left PERCENTPERCENT PERCENT // string concatenation and formatting
%precedence STRFORMAT // unary string formatting
%left PIPE        // boolean and bitwise operator
%left KW_XOR      // boolean and bitwise operator
%left AAND        // boolean and bitwise operator
%precedence NOT   /* unary logical not */
%left EEQUAL NEQUAL
%left LT GT LEQUAL GEQUAL
%left EEEQUAL NEEQUAL
%right DOTDOT           // range has lower priority than arithmetic but higher than boolen operators
%left LTLT GTGT GTGTGT  // bit-shift har lower priority than arithmetic but higher than range and boolen operators
%left PLUS MINUS
%left ASTERISK FSLASH
%precedence NEG   /* negation--unary minus */
%precedence LPAREN RPAREN  LBRACE RBRACE
%precedence ADDR  /* unary prefix address-of */
%precedence CARET /* unary postfix de-reference */
%precedence LBRACKET RBRACKET
%right DOT
%precedence ARRAY_LIT
%right KW_MODULE KW_IMPORT  /* high token shift precedence */
%right KW_ELSE
%right SEMICOLON     /* semantic statement separator, always/greedily shift */

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

sub_module : KW_MODULE compound_identifier
               LBRACE  opt_import_stmts  opt_module_members  RBRACE
                 { $$ = new TxModuleNode( @$, $2, $4, &$5->declarations, &$5->modules );
                   parserCtx->validate_module_name( $$, $2 );
                 }
    ;


compound_identifier : NAME                              { $$ = new TxIdentifier($1); }
                    | compound_identifier DOT NAME      { $$ = $1; $$->append($3); }
                    ;

import_identifier   : compound_identifier               { $$ = $1; }
                    | compound_identifier DOT ASTERISK  { $$ = $1; $$->append("*"); }
                    ;


opt_sc : %empty | SEMICOLON ;

opt_module_decl    : %empty %prec STMT { $$ = new TxIdentifier( LOCAL_NS ); }
                   | KW_MODULE compound_identifier opt_sc { $$ = $2; } ;

opt_import_stmts   : %empty { $$ = new std::vector<TxImportNode*>(); }
                   | import_statements { $$ = $1; } ;

import_statements  : import_statement
                     { $$ = new std::vector<TxImportNode*>(); if ($1) $$->push_back($1); }
                   | import_statements import_statement
                     { $$ = $1; if ($2) $$->push_back($2); }
                   ;

import_statement   : KW_IMPORT import_identifier opt_sc  { $$ = new TxImportNode(@$, $2); }
                   | KW_IMPORT error opt_sc                { $$ = NULL; }
                   ;


member_declaration
    // field
    : declaration_flags field_def SEMICOLON
            { $$ = new TxFieldDeclNode(@$, $1, $2); }

    // type
    | type_declaration     { $$ = $1; }

    // function / method
    |   declaration_flags method_def
            { $$ = new TxFieldDeclNode(@$, $1, $2, true); }

    // error recovery
    |   error SEMICOLON  { $$ = NULL; }

    |   experr_decl      { $$ = $1; }
    ;

experr_decl : KW_EXPERR COLON              { BEGIN_TXEXPERR(@1, new ExpectedErrorClause(-1)); }
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

field_def : NAME COLON qual_type_expr  { $$ = new TxNonLocalFieldDefNode(@$, $1, $3, nullptr); }
          | field_assignment_def       { $$ = $1; }
          ;

field_assignment_def : NAME COLON qual_type_expr EQUAL expr
                           { $$ = new TxNonLocalFieldDefNode(@$, $1, $3,      $5); }
                     | NAME COLEQUAL expr
                           { $$ = new TxNonLocalFieldDefNode(@$, $1, nullptr, $3); }
                     | NAME COLEQUAL mut_token expr
                           { $$ = new TxNonLocalFieldDefNode(@$, $1, nullptr, $4, true); }
;


//// types:

derives_token    : KW_DERIVES    | LTCOLON ;
ref_token        : KW_REFERENCE  | AAND ;
mut_token        : KW_MUTABLE    | TILDE ;

opt_mutable   : %empty { $$ = false; } | mut_token { $$ = true; } ;

type_body
    :   LBRACE RBRACE                   { $$ = new std::vector<TxDeclarationNode*>(); }
    |   LBRACE member_list RBRACE       { $$ = $2; }
    |   LBRACE error RBRACE             { $$ = new std::vector<TxDeclarationNode*>(); TX_SYNTAX_ERROR; }
    |   LBRACE member_list error RBRACE { $$ = $2;                                    TX_SYNTAX_ERROR; }
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
type_param      : NAME  { $$ = new TxTypeDeclNode(@$, TXD_PUBLIC | TXD_GENPARAM, $1, NULL, new TxNamedTypeNode(@$, "tx.Any")); }
                | NAME derives_token conv_type_expr { $$ = new TxTypeDeclNode (@$, TXD_PUBLIC | TXD_GENPARAM, $1, NULL, $3); }
                | NAME COLON type_expression        { $$ = new TxFieldDeclNode(@$, TXD_PUBLIC | TXD_GENPARAM, new TxNonLocalFieldDefNode(@$, $1, $3, nullptr)); }
                ;

type_declaration : declaration_flags type_or_if opt_mutable NAME type_derivation  
                        { $$ = new TxTypeDeclNode(@$, $1, $4, NULL, $5, $2, $3); }
                 | declaration_flags type_or_if opt_mutable NAME LT type_param_list GT type_derivation
                        { $$ = new TxTypeDeclNode(@$, $1, $4, $6,   $8, $2, $3); }

                 // error recovery, handles when an error occurs before a type body's LBRACE:
                 | error type_body  { $$ = NULL; }
                 ;

type_derivation : derives_token type_expression SEMICOLON  { $$ = $2; }

                | derives_token type_expression type_body  { $$ = new TxDerivedTypeNode(@$, $2, $3); }
    
                | derives_token type_expression COMMA conv_type_list type_body
                    { $$ = new TxDerivedTypeNode(@$, $2, $4, $5); }

                | type_body         { $$ = new TxDerivedTypeNode(@$, $1); }
                ;

conv_type_list  : conv_type_expr  { $$ = new std::vector<TxTypeExpressionNode*>();  $$->push_back($1); }
                | conv_type_list COMMA conv_type_expr  { $$ = $1;  $$->push_back($3); }
                ;

// conventional type expressions are named types (and possibly specialized) (i.e. not using not syntatic sugar to describe the type)
conv_type_expr  : named_symbol             %prec DOT  { $$ = new TxNamedTypeNode(@$, $1); }
                | conv_type_expr DOT NAME             { $$ = new TxMemberTypeNode(@$, $1, $3); }
                | conv_type_expr LT type_arg_list GT  { $$ = new TxGenSpecTypeNode(@$, $1, $3); }
                ;


named_symbol    : NAME                   { $$ = new TxIdentifiedSymbolNode(@$, NULL, $1); }
                | named_symbol DOT NAME  { $$ = new TxIdentifiedSymbolNode(@$, $1, $3); }
                ;


named_type      : named_symbol            %prec EXPR { $$ = new TxNamedTypeNode(@$, $1); }
                | produced_type DOT NAME  %prec DOT  { $$ = new TxMemberTypeNode(@$, $1, $3); }
                ;

specialized_type  : named_type LT type_arg_list GT  { $$ = new TxGenSpecTypeNode(@$, $1, $3); }
                  ;

type_arg_list   : type_arg  { $$ = new std::vector<TxTypeArgumentNode*>();  $$->push_back($1); }
                | type_arg_list COMMA type_arg  { $$ = $1;  $$->push_back($3); }
                ;

type_arg        : value_literal       { $$ = new TxValueTypeArgumentNode($1); }  // unambiguous value expr
                | LPAREN expr RPAREN  { $$ = new TxValueTypeArgumentNode($2); }  // parens prevent conflation with type expr
                | opt_mutable conv_type_expr   { $$ = new TxTypeTypeArgumentNode(( $1 ? new TxModifiableTypeNode(@$, $2)
                                                                                         : new TxMaybeModTypeNode(@2, $2) )); }
                // TODO: support reference and array types (possibly chained):
                // | qual_type_expr     { $$ = new TxTypeTypeArgumentNode($1); }
                // | opt_mutable AAND conv_type_expr
                // | opt_mutable LBRACKET RBRACKET conv_type_expr
                ;

// a qualified type expression, or "type usage variant", is a type expression with possible qualifiers (e.g. mutable):
qual_type_expr  : opt_mutable type_expression  %prec EXPR
                         { $$ = ( $1 ? new TxModifiableTypeNode(@$, $2)
                                     : new TxMaybeModTypeNode(@2, $2) ); }
                ;

// can identify or construct new type but can't extend (specify interfaces or add members to) one:
type_expression : named_type     %prec DOT  { $$ = $1; }
                | produced_type  %prec EXPR { $$ = $1; }
                ;

produced_type   :  specialized_type   { $$ = $1; }
                |  reference_type     { $$ = $1; }
                |  array_type         { $$ = $1; }
                |  function_signature { $$ = $1; }
                ;
//    |  data_tuple_type
//    |  union_type
//    |  enum_type
//    |  range_type
//    |  shared_obj_type

reference_type : opt_dataspace ref_token qual_type_expr
                    { /* (custom ast node needed to handle dataspaces) */
                      $$ = new TxReferenceTypeNode(@$, $1, $3);
                    } ;
opt_dataspace : %empty { $$ = NULL; } | QMARK { $$ = NULL; } | NAME { $$ = new TxIdentifier($1); } ;

array_type : array_dimensions qual_type_expr
                    { /* (custom ast node needed to provide syntactic sugar for modifiable decl) */
                      $$ = new TxArrayTypeNode(@$, $2, $1);
                    } ;
array_dimensions : LBRACKET expr RBRACKET  { $$ = $2; }
                 //| LBRACKET conv_type_expr RBRACKET  // type must be an enum
                 | LBRACKET RBRACKET  { $$ = NULL; }
                 ;


/// function type and function declarations:

lambda_expr : function_signature suite  { $$ = new TxLambdaExprNode(@$, $1, $2); } ;

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

func_arg_def   : NAME COLON type_expression
                        { $$ = new TxArgTypeDefNode(@$, $1, $3); }
               | NAME COLON type_expression ELLIPSIS
                        { $$ = new TxArgTypeDefNode(@$, $1,
                                    new TxReferenceTypeNode(@3, nullptr, new TxArrayTypeNode(@3, new TxMaybeModTypeNode(@3, $3)))); }
               | NAME COLON mut_token type_expression ELLIPSIS
                        { $$ = new TxArgTypeDefNode(@$, $1,
                                    new TxReferenceTypeNode(@3, nullptr, new TxArrayTypeNode(@3, new TxModifiableTypeNode(@3, $4)))); }
               ;


method_def  : NAME function_signature suite
                { $$ = new TxNonLocalFieldDefNode(@$, $1, nullptr, new TxLambdaExprNode(@$, $2, $3, true)); }
            | NAME function_signature SEMICOLON  // abstract method (KW_ABSTRACT should be specified)
                { $$ = new TxNonLocalFieldDefNode(@$, $1, $2,      nullptr); }
            ;



//// (value) expressions:

expr
    :   LPAREN expr RPAREN           { $$ = $2; }
    |   value_literal                { $$ = $1; }
    |   array_literal                { $$ = $1; }
    |   lambda_expr                  { $$ = $1; }
    |   call_expr                    { $$ = $1; }
    |   make_expr                    { $$ = $1; }
    |   intrinsics_expr              { $$ = $1; }

    |   NAME                         { $$ = new TxFieldValueNode(@$, NULL, $1); }
    |   expr DOT NAME                { $$ = new TxFieldValueNode(@$, $1,   $3); }
    |   expr LBRACKET expr RBRACKET  { $$ = new TxElemDerefNode(@$, $1, $3); }
    |   expr CARET                   { $$ = new TxReferenceDerefNode(@$, $1); }
    |   AAND expr   %prec ADDR       { $$ = new TxReferenceToNode(@$, $2); }

    |   expr DOTDOT expr             { $$ = TxERangeLitNode::make_range_node(@$, $1, $3); }

    |   expr EEQUAL expr             { $$ = new TxEqualityOperatorNode(@$, $1, $3); }
    |   expr NEQUAL expr             { $$ = new TxUnaryLogicalNotNode(@$, new TxEqualityOperatorNode(@$, $1, $3)); }

    |   expr EEEQUAL expr            { $$ = new TxRefEqualityOperatorNode(@$, $1, $3); }
    |   expr NEEQUAL expr            { $$ = new TxUnaryLogicalNotNode(@$, new TxRefEqualityOperatorNode(@$, $1, $3)); }

    |   MINUS expr  %prec NEG        { $$ = new TxUnaryMinusNode(@$, $2); }  // unary minus
    |   expr PLUS expr               { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_PLUS, $3); }
    |   expr MINUS expr              { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_MINUS, $3); }
    |   expr ASTERISK expr           { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_MUL, $3); }
    |   expr FSLASH expr             { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_DIV, $3); }
    |   expr LT expr                 { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_LT, $3); }
    |   expr GT expr                 { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_GT, $3); }
    |   expr LEQUAL expr             { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_LE, $3); }
    |   expr GEQUAL expr             { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_GE, $3); }

    |   EMARK expr  %prec NOT        { $$ = new TxUnaryLogicalNotNode(@$, $2); }  // unary not
    |   expr AAND expr               { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_AND, $3); }
    |   expr PIPE expr               { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_OR,  $3); }
    |   expr KW_XOR expr             { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_XOR,  $3); }
    |   expr LTLT expr      %prec LTLT          { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_LSHIFT, $3); }
    |   expr GT GT expr     %prec LTLT          { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_RSHIFT, $4); }
    |   expr GT GT GT expr  %prec LTLT          { $$ = new TxBinaryElemOperatorNode(@$, $1, TXOP_ARSHIFT, $5); }

    |   string_format_expr           { $$ = $1; }
    ;

value_literal
        :       LIT_DEC_INT   { $$ = new TxIntegerLitNode(@1, $1, false); }
        |       LIT_RADIX_INT { $$ = new TxIntegerLitNode(@1, $1, true); }
        |       LIT_FLOATING  { $$ = new TxFloatingLitNode(@1, $1); }
        |       LIT_CHARACTER { $$ = new TxCharacterLitNode(@1, $1); }
        |       LIT_CSTRING   { $$ = new TxCStringLitNode(@1, $1); }
        |       LIT_STRING    { $$ = new TxStringLitNode(@1, $1); }
        |       KW_NULL       { $$ = new TxBoolLitNode(@1, false); }  // TODO: proper Null type
        |       KW_FALSE      { $$ = new TxBoolLitNode(@1, false); }
        |       KW_TRUE       { $$ = new TxBoolLitNode(@1, true); }
    ;

array_literal : LBRACKET expr COMMA array_lit_expr_list RBRACKET  { (*$4)[0] = $2;  $$ = new TxFilledArrayLitNode(@$, $4); }
              | LBRACKET expr RBRACKET  { $$ = new TxFilledArrayLitNode(@$, new std::vector<TxExpressionNode*>( { $2 } )); }

              // produces a (harmless) shift-reduce warning but unknown how to suppress that:
              | LBRACKET expr RBRACKET conv_type_expr LPAREN expression_list RPAREN  { $$ = new TxFilledArrayLitNode(@$, $4, $6, $2); }
              | LBRACKET expr RBRACKET conv_type_expr LPAREN RPAREN                  { $$ = new TxUnfilledArrayCompLitNode(@$, $4, $2); }
              | LBRACKET RBRACKET conv_type_expr LPAREN expression_list RPAREN       { $$ = new TxFilledArrayLitNode(@$, $3, $5); }
              | LBRACKET RBRACKET conv_type_expr LPAREN RPAREN                       { $$ = new TxFilledArrayLitNode(@$, $3); }
              ;
              // LBRACKET RBRACKET - empty, unqualified array literal "[]" illegal since element type can't be determined

array_lit_expr_list : expr  { $$ = new std::vector<TxExpressionNode*>();  $$->push_back(NULL);  $$->push_back($1); }
                    | array_lit_expr_list COMMA expr  { $$ = $1;  $$->push_back($3); }
                    ;

make_expr : KW_NEW qual_type_expr call_params { $$ = new TxNewConstructionNode(@$, $2, $3); }
          | LT qual_type_expr GT call_params  { $$ = new TxStackConstructionNode(@$, $2, $4); }
;

call_expr : expr call_params  { $$ = new TxFunctionCallNode(@$, $1, $2); }
;

call_params : LPAREN expression_list RPAREN  { $$ = $2; }
            | LPAREN RPAREN  { $$ = new std::vector<TxExpressionNode*>(); }
;

expression_list : expr
                      { $$ = new std::vector<TxExpressionNode*>();
                        $$->push_back($1); }
                | expression_list COMMA expr
                      { $$ = $1;
                        $$->push_back($3); }
;

intrinsics_expr : KW__ADDRESS LPAREN expr RPAREN  { $$ = new TxRefAddressNode(@$, $3); }
                | KW__TYPEID  LPAREN expr RPAREN  { $$ = new TxRefTypeIdNode(@$, $3); }
                | KW__SIZEOF  LPAREN expr RPAREN  { $$ = new TxSizeofExprNode(@$, $3); }
                ;


string_format_expr : string_format expr       %prec STRFORMAT
                        { $$ = new TxStackConstructionNode( @$, new TxNamedTypeNode( @$, "tx.FormattedStringer"),
                                                            new std::vector<TxExpressionNode*>( { $1, $2 } ) ); }
                   | expr string_format expr  %prec PERCENT
                        { $$ = TxConcatenateStringsNode::make_strcat_node( @$, $1, 
                                    new TxStackConstructionNode( @2, new TxNamedTypeNode( @2, "tx.FormattedStringer"),
                                                                        new std::vector<TxExpressionNode*>( { $2, $3 } ) ) ); }
                   | expr PERCENTPERCENT expr
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
    :   LBRACE RBRACE                      { $$ = new TxSuiteNode(@$); }
    |   LBRACE statement_list RBRACE       { $$ = new TxSuiteNode(@$, $2); }
    |   LBRACE error RBRACE                { $$ = new TxSuiteNode(@$);     TX_SYNTAX_ERROR; }
    |   LBRACE statement_list error RBRACE { $$ = new TxSuiteNode(@$, $2); TX_SYNTAX_ERROR; }
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
    :   type_decl_stmt             %prec STMT    { $$ = $1; }
    |   elementary_stmt SEMICOLON  %prec STMT    { $$ = $1; }
    |   terminal_stmt   SEMICOLON  %prec STMT    { $$ = $1; }
    |   flow_else_stmt             %prec KW_ELSE { $$ = $1; }
    |   experr_stmt                %prec STMT    { $$ = $1; }
    |   error SEMICOLON            %prec STMT    { $$ = new TxNoOpStmtNode(@$); TX_SYNTAX_ERROR; }
    ;

elementary_stmt
    :   local_field_def { $$ = new TxFieldStmtNode(@1, $1); }
    |   call_expr       { $$ = new TxCallStmtNode(@1, $1); } // function call without return value assignment
    |   assignment_stmt { $$ = $1; }
    |   assert_stmt     { $$ = $1; }
    |   panic_stmt      { $$ = $1; }
    ;

terminal_stmt
    :   return_stmt     { $$ = $1; }
    |   break_stmt      { $$ = $1; }
    |   continue_stmt   { $$ = $1; }
    ;

experr_stmt : KW_EXPERR COLON              { BEGIN_TXEXPERR(@1, new ExpectedErrorClause(-1)); }
              statement                    { $$ = new TxExpErrStmtNode(@$, END_TXEXPERR(@4), static_cast<TxStatementNode*>($4)); }
            | KW_EXPERR LIT_DEC_INT COLON  { BEGIN_TXEXPERR(@1, new ExpectedErrorClause(std::stoi($2))); }
              statement                    { $$ = new TxExpErrStmtNode(@$, END_TXEXPERR(@5), $5); }
            ;

flow_stmt        : KW_IF    cond_expr      COLON single_statement  %prec STMT  { $$ = new TxIfStmtNode (@$, $2, $4); }
                 | KW_IF    cond_expr      suite                   %prec STMT  { $$ = new TxIfStmtNode (@$, $2, $3); }
                 | KW_WHILE cond_expr      COLON single_statement  %prec STMT  { $$ = new TxForStmtNode(@$, new TxWhileHeaderNode(@2, $2), $4); }
                 | KW_WHILE cond_expr      suite                   %prec STMT  { $$ = new TxForStmtNode(@$, new TxWhileHeaderNode(@2, $2), $3); }
                 | KW_FOR   in_clause_list COLON single_statement  %prec STMT  { $$ = new TxForStmtNode(@$, $2, $4); }
                 | KW_FOR   in_clause_list suite                   %prec STMT  { $$ = new TxForStmtNode(@$, $2, $3); }
                 | KW_FOR   for_header     COLON single_statement  %prec STMT  { $$ = new TxForStmtNode(@$, $2, $4); }
                 | KW_FOR   for_header     suite                   %prec STMT  { $$ = new TxForStmtNode(@$, $2, $3); }
                 ;

flow_else_stmt   : KW_IF    cond_expr      COLON simple_stmt else_clause  { $$ = new TxIfStmtNode (@$, $2, $4, $5); }
                 | KW_IF    cond_expr      suite             else_clause  { $$ = new TxIfStmtNode (@$, $2, $3, $4); }
                 | KW_WHILE cond_expr      COLON simple_stmt else_clause  { $$ = new TxForStmtNode(@$, new TxWhileHeaderNode(@2, $2), $4, $5); }
                 | KW_WHILE cond_expr      suite             else_clause  { $$ = new TxForStmtNode(@$, new TxWhileHeaderNode(@2, $2), $3, $4); }
                 | KW_FOR   in_clause_list COLON simple_stmt else_clause  { $$ = new TxForStmtNode(@$, $2, $4, $5); }
                 | KW_FOR   in_clause_list suite             else_clause  { $$ = new TxForStmtNode(@$, $2, $3, $4); }
                 | KW_FOR   for_header     COLON simple_stmt else_clause  { $$ = new TxForStmtNode(@$, $2, $4, $5); }
                 | KW_FOR   for_header     suite             else_clause  { $$ = new TxForStmtNode(@$, $2, $3, $4); }
                 ;

cond_expr        : expr %prec STMT    { $$ = $1; } ;

else_clause      : KW_ELSE COLON statement  { $$ = new TxElseClauseNode(@$, $3); }
                 | KW_ELSE statement  { $$ = new TxElseClauseNode(@$, $2); }  // colon is currently optional since unambiguous
                 ;


in_clause_list   : in_clause                        { $$ = new std::vector<TxLoopHeaderNode*>( { $1 } ); }
                 | in_clause_list COMMA in_clause   { $$ = $1; $$->push_back($3); }
                 | error                            { $$ = new std::vector<TxLoopHeaderNode*>(); }
                 ;

in_clause        : NAME KW_IN expr             { $$ = new TxInClauseNode( @$, $1, $3 ); }
                 | NAME COMMA NAME KW_IN expr  { $$ = new TxInClauseNode( @$, $1, $3, $5 ); }
                 ;

for_header       : elementary_stmt SEMICOLON expr SEMICOLON elementary_stmt  { $$ = new TxForHeaderNode( @$, $1, $3, $5 ); }
                 ;

local_field_def  : NAME COLON qual_type_expr              { $$ = new TxLocalFieldDefNode(@$, $1, $3,      nullptr); }
                 | NAME COLON qual_type_expr EQUAL expr   { $$ = new TxLocalFieldDefNode(@$, $1, $3,      $5); }
                 | NAME COLEQUAL expr                     { $$ = new TxLocalFieldDefNode(@$, $1, nullptr, $3); }
                 | NAME COLEQUAL mut_token expr           { $$ = new TxLocalFieldDefNode(@$, $1, nullptr, $4, true); }
;

// TODO: support declaration flags abstract, final, and maybe static
type_decl_stmt   : type_or_if opt_mutable NAME type_derivation
                     { $$ = new TxTypeStmtNode(@$, $3, NULL, $4, $1, $2); }
                 | type_or_if opt_mutable NAME LT type_param_list GT type_derivation
                     { $$ = new TxTypeStmtNode(@$, $3, $5,   $7, $1, $2); }

                 // error recovery, handles when an error occurs before a type body's LBRACE:
                 | error type_body  { $$ = new TxNoOpStmtNode(@$); TX_SYNTAX_ERROR; }
                 ;


return_stmt : KW_RETURN expr  { $$ = new TxReturnStmtNode(@$, $2); }
            | KW_RETURN       { $$ = new TxReturnStmtNode(@$); }
            ;

break_stmt     : KW_BREAK     { $$ = new TxBreakStmtNode(@$); }  ;
continue_stmt  : KW_CONTINUE  { $$ = new TxContinueStmtNode(@$); }  ;

assert_stmt : KW_ASSERT expr  { $$ = new TxAssertStmtNode(@$, $2); }
            // | KW_ASSERT expr COMMA expr
            ;

panic_stmt : KW_PANIC expr  { $$ = new TxPanicStmtNode(@$, $2); }
           ;


assignment_stmt //:    assignee_pattern EQUAL expr
                :    assignee_expr EQUAL expr  { $$ = new TxAssignStmtNode(@$, $1, $3); }
//                |    assignee_expr PLUSEQUAL expr
//                |    assignee_expr MINUSEQUAL expr
//                |    assignee_expr ASTERISKEQUAL expr
//                |    assignee_expr FSLASHEQUAL expr
                ;

//assignee_pattern : nested_assignee (',' nested_assignee)* ;
//nested_assignee  : assignee_expr | '{' assignee_pattern '}' ;

assignee_expr  // expressions capable of (but not guaranteed) to produce an lvalue
    :   NAME             { $$ = new TxFieldAssigneeNode(@$, new TxFieldValueNode(@1, NULL, $1)); }
    |   expr DOT NAME    { $$ = new TxFieldAssigneeNode(@$, new TxFieldValueNode(@3, $1,   $3)); }
    |   expr CARET       { $$ = new TxDerefAssigneeNode(@$, $1); }  // unary value-of suffix
    |   expr LBRACKET expr RBRACKET { $$ = new TxElemAssigneeNode(@$, $1, $3); }
    ;

%%

void yy::TxParser::error (const location_type& l, const std::string& m) {
    parserCtx->cerror (l, m);
}
