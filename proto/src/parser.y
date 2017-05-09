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
#include "location.hpp"
#include "ast.hpp"

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
%token DOT COLON COMMA ELLIPSIS ASTERISK PLUS MINUS FSLASH BSLASH AAND
%token PIPE CARET TILDE AT PERCENT DOLLAR EURO LPAREN RPAREN LBRACE
%token RBRACE LBRACKET RBRACKET QMARK EMARK DASHGT
%token EQUAL EEQUAL NEQUAL EEEQUAL NEEQUAL LT GT LEQUAL GEQUAL
%token COLEQUAL PLUSEQUAL MINUSEQUAL ASTERISKEQUAL FSLASHEQUAL

/* keywords: */
%token KW_MODULE KW_IMPORT KW_TYPE KW_INTERFACE
%token KW_PUBLIC KW_PROTECTED KW_STATIC KW_ABSTRACT KW_FINAL KW_OVERRIDE KW_EXTERN
%token KW_MODIFIABLE KW_REFERENCE KW_DERIVES
%token KW_WHILE KW_FOR KW_IF KW_ELSE KW_SWITCH KW_CASE KW_WITH KW_IN KW_IS KW_AS KW_OR
%token KW_RETURN KW_BREAK KW_CONTINUE KW_NEW KW_DELETE KW_FROM
%token KW_NULL KW_TRUE KW_FALSE
%token KW_ASSERT KW_EXPERR

/* keywords reserved but not currently used */
%token KW_TUPLE KW_UNION KW_ENUM
%token KW_RAISES KW_TRY KW_EXCEPT KW_FINALLY KW_RAISE
%token KW_AND KW_XOR KW_NOT KW_BUILTIN KW_FUNC KW_LAMBDA KW_CLASS KW_EXTENDS KW_IMPLEMENTS

 /* literals: */
%token <std::string> NAME LIT_DEC_INT LIT_RADIX_INT LIT_FLOATING LIT_CHARACTER LIT_CSTRING LIT_STRING

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above.
 */
%type <TxIdentifier*> compound_identifier import_identifier 
%type <TxIdentifier*> opt_module_decl opt_dataspace

%type <TxDeclarationFlags> declaration_flags opt_visibility opt_extern opt_static opt_abstract opt_override opt_final opt_builtin
%type <bool> opt_modifiable type_or_if
%type <TxParsingUnitNode*> parsing_unit
%type <TxModuleNode*> sub_module

%type <std::vector<TxImportNode*> *> import_statements opt_import_stmts
%type <TxImportNode *> import_statement

%type <TxModuleMembers*> module_members opt_module_members
%type <TxDeclarationNode *> member_declaration experr_decl type_param
%type <std::vector<TxDeclarationNode*> *> type_body member_list type_param_list

%type <TxIdentifiedSymbolNode*> named_symbol

%type <std::vector<TxTypeExpressionNode*> *> opt_base_types conv_type_list

%type <TxTypeArgumentNode *> type_arg
%type <std::vector<TxTypeArgumentNode*> *> type_arg_list

%type <TxFieldDefNode*> field_def field_assignment_def method_def

%type <TxArgTypeDefNode*> func_arg_def
%type <std::vector<TxArgTypeDefNode*> *> func_args func_args_list

%type <TxTypeExpressionNode*> type_spec type_extension type_expression base_type_expression conv_type_expr produced_type
%type <TxTypeExpressionNode*> named_type specialized_type reference_type array_type //data_tuple_type

%type <TxFunctionTypeNode*> function_signature
%type <TxExpressionNode*> expr make_expr lambda_expr value_literal array_literal array_dimensions cond_expr
%type <TxFunctionCallNode*> call_expr
%type <std::vector<TxExpressionNode*> *> expression_list call_params array_lit_expr_list
%type <std::vector<TxStatementNode*> *> statement_list
%type <TxSuiteNode*> suite
%type <TxStatementNode*> statement single_statement assignment_stmt return_stmt break_stmt continue_stmt assert_stmt type_decl_stmt
%type <TxStatementNode*> cond_stmt simple_stmt elementary_stmt cond_else_stmt
%type <TxElseClauseNode*> else_clause
%type <TxStatementNode*> experr_stmt
%type <TxAssigneeNode*> assignee_expr


/* Operator precedence for expression operators (higher line no = higher precedence) */
%precedence STMT /* used to specify statement / member rule precedence, to be lower than e.g. separator  */
%precedence EXPR
%precedence ELLIPSIS
%left COMMA COLON
%right EQUAL
%left PIPE  // boolean (logical, not bitwise) operator
%left AAND  // boolean (logical, not bitwise) operator
%precedence NOT   /* unary logical not */
%left EEQUAL NEQUAL
%left LT GT LEQUAL GEQUAL
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
                   { auto module = new TxModuleNode( @1, $1, $2, &$3->declarations, &$3->modules );
                     $$ = new TxParsingUnitNode( @2, module );
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
                 { $$ = new TxModuleNode( @1, $2, $4, &$5->declarations, &$5->modules );
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

import_statement   : KW_IMPORT import_identifier opt_sc  { $$ = new TxImportNode(@1, $2); }
                   | KW_IMPORT error opt_sc                { $$ = NULL; }
                   ;


member_declaration
    // field
    : declaration_flags field_def SEMICOLON
            { $$ = new TxFieldDeclNode(@2, $1, $2); }

    // type
    | declaration_flags type_or_if NAME type_spec  
            { $$ = new TxTypeDeclNode(@2, $1, $3, NULL, $4, $2); }
    | declaration_flags type_or_if NAME LT type_param_list GT type_spec
            { $$ = new TxTypeDeclNode(@2, $1, $3, $5,   $7, $2); }

    // function / method
    |   declaration_flags method_def
            { $$ = new TxFieldDeclNode(@2, $1, $2, true); }

    // error recovery
    |   error SEMICOLON  { $$ = NULL; }

    |   experr_decl      { $$ = $1; }
    ;

experr_decl : KW_EXPERR COLON              { BEGIN_TXEXPERR(@1, new ExpectedErrorClause(-1)); }
              member_declaration           { $$ = new TxExpErrDeclNode(@1, END_TXEXPERR(@4), $4); }
            | KW_EXPERR LIT_DEC_INT COLON  { BEGIN_TXEXPERR(@1, new ExpectedErrorClause(std::stoi($2))); }
              member_declaration           { $$ = new TxExpErrDeclNode(@1, END_TXEXPERR(@5), $5); }
            ;


declaration_flags : opt_visibility opt_builtin opt_extern opt_static opt_abstract opt_override opt_final  { $$ = ($1 | $2 | $3 | $4 | $5 | $6 | $7); } ;

opt_visibility : %empty        { $$ = TXD_NONE; }
               | KW_PUBLIC     { $$ = TXD_PUBLIC; }
               | KW_PROTECTED  { $$ = TXD_PROTECTED; }
               ;
opt_builtin    : %empty { $$ = TXD_NONE; } | KW_BUILTIN  { $$ = TXD_BUILTIN;  } ;
opt_extern     : %empty { $$ = TXD_NONE; } | KW_EXTERN   { $$ = TXD_EXTERN;   } ;
opt_static     : %empty { $$ = TXD_NONE; } | KW_STATIC   { $$ = TXD_STATIC;   } ;
opt_abstract   : %empty { $$ = TXD_NONE; } | KW_ABSTRACT { $$ = TXD_ABSTRACT; } ;
opt_override   : %empty { $$ = TXD_NONE; } | KW_OVERRIDE { $$ = TXD_OVERRIDE; } ;
opt_final      : %empty { $$ = TXD_NONE; } | KW_FINAL    { $$ = TXD_FINAL;    } ;


type_or_if : KW_TYPE        { $$ = false; }
           | KW_INTERFACE   { $$ = true;  }
           ;

field_def : NAME COLON type_expression  { $$ = new TxFieldDefNode(@1, $1, $3, nullptr); }
          | field_assignment_def        { $$ = $1; }
          ;

field_assignment_def : NAME COLON type_expression EQUAL expr
                           { $$ = new TxFieldDefNode(@1, $1, $3,      $5); }
                     | NAME COLEQUAL expr
                           { $$ = new TxFieldDefNode(@1, $1, nullptr, $3); }
                     | TILDE NAME COLEQUAL expr
                           { $$ = new TxFieldDefNode(@1, $2, nullptr, $4, true); }
                     // TODO |   assignee_pattern COLEQUAL expr
;


//// types:

type_param_list : type_param  { $$ = new std::vector<TxDeclarationNode*>(); $$->push_back($1); }
                | type_param_list COMMA type_param  { $$ = $1; $$->push_back($3); }
                ;
type_param      : NAME  { $$ = new TxTypeDeclNode(@1, TXD_PUBLIC | TXD_GENPARAM, $1, NULL,
                                                  new TxNamedTypeNode(@1, "tx.Any")); }
                | NAME KW_DERIVES conv_type_expr { $$ = new TxTypeDeclNode (@1, TXD_PUBLIC | TXD_GENPARAM, $1, NULL, $3); }
                | NAME COLON type_expression     { $$ = new TxFieldDeclNode(@1, TXD_PUBLIC | TXD_GENPARAM, new TxFieldDefNode(@1, $1, $3, nullptr)); }
                ;


type_spec : type_expression SEMICOLON { $$ = $1; }
          | type_extension            { $$ = $1; }
//          | type_interface { $$ = $1; }
          ;

type_extension : opt_modifiable opt_base_types type_body  //LBRACE  opt_type_members  RBRACE
                        { $$ = new TxDerivedTypeNode(@1, $1, $2, $3); }
               ;

type_body
    //:   %empty                          { $$ = new std::vector<TxDeclarationNode*>(); }
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

opt_base_types  : %empty    { $$ = new std::vector<TxTypeExpressionNode*>(); }
                | KW_DERIVES conv_type_list  { $$ = $2; }
                // (all but the first must be interface types)
                ;

conv_type_list  : conv_type_expr  { $$ = new std::vector<TxTypeExpressionNode*>();  $$->push_back($1); }
                | conv_type_list COMMA conv_type_expr  { $$ = $1;  $$->push_back($3); }
                ;

// conventional type expressions are named types (and possibly specialized) (i.e. not using not syntatic sugar to describe the type)
conv_type_expr  : named_symbol                      { $$ = new TxNamedTypeNode(@1, $1); }
                | named_symbol LT type_arg_list GT  { $$ = new TxGenSpecTypeNode(@1, new TxNamedTypeNode(@1, $1), $3); }
                ;


named_symbol    : NAME                   { $$ = new TxIdentifiedSymbolNode(@1, NULL, $1); }
                | named_symbol DOT NAME  { $$ = new TxIdentifiedSymbolNode(@1, $1, $3); }
                ;


named_type      : named_symbol            %prec EXPR { $$ = new TxNamedTypeNode(@1, $1); }
                | produced_type DOT NAME  %prec DOT  { $$ = new TxMemberTypeNode(@1, $1, $3); }
                ;

specialized_type  : named_type LT type_arg_list GT  { $$ = new TxGenSpecTypeNode(@1, $1, $3); }
// not supported: | named_type LT GT  { $$ = new TxGenSpecTypeNode(@1, $1, new std::vector<TxTypeArgumentNode*>()); }
                  ;

type_arg_list   : type_arg  { $$ = new std::vector<TxTypeArgumentNode*>();  $$->push_back($1); }
                | type_arg_list COMMA type_arg  { $$ = $1;  $$->push_back($3); }
                ;

type_arg        : value_literal       { $$ = new TxValueTypeArgumentNode($1); }  // unambiguous value expr
                | LPAREN expr RPAREN  { $$ = new TxValueTypeArgumentNode($2); }  // parens prevent conflation with type expr
                | opt_modifiable conv_type_expr   { $$ = new TxTypeTypeArgumentNode(( $1 ? new TxModifiableTypeNode(@1, $2)
                                                                                      : new TxMaybeModTypeNode(@2, $2) )); }
                // TODO: support reference and array types (possibly chained):
                // | type_expression     { $$ = new TxTypeTypeArgumentNode($1); }
                // | opt_modifiable AAND conv_type_expr
                // | opt_modifiable LBRACKET RBRACKET conv_type_expr
                ;


type_expression  // can identify or construct new type but can't extend (specify interfaces or add members to) one
    :   opt_modifiable base_type_expression  %prec EXPR
             { $$ = ( $1 ? new TxModifiableTypeNode(@1, $2)
                         : new TxMaybeModTypeNode(@2, $2) ); }
    |   opt_modifiable base_type_expression ELLIPSIS
             { $$ = ( $1 ? new TxReferenceTypeNode(@1, nullptr, new TxArrayTypeNode(@1, new TxModifiableTypeNode(@1, $2)))
                         : new TxReferenceTypeNode(@2, nullptr, new TxArrayTypeNode(@2, new TxMaybeModTypeNode(@2, $2))) ); }
    ;

opt_modifiable : %empty { $$ = false; } | TILDE { $$ = true; } | KW_MODIFIABLE { $$ = true; } ;

base_type_expression    : named_type     %prec DOT  { $$ = $1; }
                        | produced_type  %prec EXPR { $$ = $1; }
                        ;

produced_type
    :  specialized_type   { $$ = $1; }
    |  reference_type     { $$ = $1; }
    |  array_type         { $$ = $1; }
    |  function_signature { $$ = $1; }
//    |  data_tuple_type
//    |  union_type
//    |  enum_type
//    |  range_type
//    |  shared_obj_type
    ;

reference_type : opt_dataspace ref_token type_expression
                    { /* (custom ast node needed to handle dataspaces) */
                      $$ = new TxReferenceTypeNode(@2, $1, $3);
                    } ;
opt_dataspace : %empty { $$ = NULL; } | QMARK { $$ = NULL; } | NAME { $$ = new TxIdentifier($1); } ;
ref_token : KW_REFERENCE | AAND ;

array_type : array_dimensions type_expression
                    { /* (custom ast node needed to provide syntactic sugar for modifiable decl) */
                      $$ = new TxArrayTypeNode(@1, $2, $1);
                    } ;
array_dimensions : LBRACKET expr RBRACKET  { $$ = $2; }
                 //| LBRACKET conv_type_expr RBRACKET  // type must be an enum
                 | LBRACKET RBRACKET  { $$ = NULL; }
                 ;

//data_tuple_type  // simple tuple type with no static fields; fields implicitly public
//    : KW_TUPLE opt_modifiable LBRACE field_type_list RBRACE
//            { $$ = new TxTupleTypeNode(@1, $3, $7); }
//    ;

//union_type     : KW_UNION LBRACE type_expr_list RBRACE ;
//type_expr_list : type_expression
//               | type_expr_list COMMA type_expression
//               ;
//
//enum_type       : KW_ENUM LBRACE enum_value_list RBRACE ;
//enum_value_list : NAME
//                | enum_value_list COMMA NAME
//                ;
//
//range_type : expr ':' ( expr ':' )? expr ;  // Enum and Scalar types are legal



/// function type and function declarations:

lambda_expr : function_signature suite  { $$ = new TxLambdaExprNode(@1, $1, $2); } ;

function_signature : func_args opt_modifiable DASHGT type_expression
                        { $$ = new TxFunctionTypeNode(@1, $2, $1, $4); }
                   | func_args opt_modifiable
                        { $$ = new TxFunctionTypeNode(@1, $2, $1, NULL); }
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

func_arg_def   : NAME COLON type_expression  { $$ = new TxArgTypeDefNode(@1, $1, $3); }
               ;


method_def  : NAME function_signature suite
                { $$ = new TxFieldDefNode(@1, $1, nullptr, new TxLambdaExprNode(@1, $2, $3, true)); }
            | NAME function_signature SEMICOLON  // abstract method (KW_ABSTRACT should be specified)
                { $$ = new TxFieldDefNode(@1, $1, $2,      nullptr); }
            ;



//// (value) expressions:

expr
    :   LPAREN expr RPAREN           { $$ = $2; }
    |   value_literal                { $$ = $1; }
    |   array_literal                { $$ = $1; }
    |   lambda_expr                  { $$ = $1; }
    |   call_expr                    { $$ = $1; }
    |   make_expr                    { $$ = $1; }

    |   NAME                         { $$ = new TxFieldValueNode(@1, NULL, $1); }
    |   expr DOT NAME                { $$ = new TxFieldValueNode(@3, $1,   $3); }
    |   expr LBRACKET expr RBRACKET  { $$ = new TxElemDerefNode(@2, $1, $3); }
    |   expr CARET                   { $$ = new TxReferenceDerefNode(@2, $1); }
    |   AAND expr   %prec ADDR       { $$ = new TxReferenceToNode(@1, $2); }

    |   MINUS expr  %prec NEG        { $$ = new TxUnaryMinusNode(@1, $2); }  // unary minus
    |   expr PLUS expr               { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_PLUS, $3); }
    |   expr MINUS expr              { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_MINUS, $3); }
    |   expr ASTERISK expr           { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_MUL, $3); }
    |   expr FSLASH expr             { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_DIV, $3); }
    |   expr EEQUAL expr             { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_EQ, $3); }
    |   expr NEQUAL expr             { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_NE, $3); }
    |   expr LT expr                 { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_LT, $3); }
    |   expr GT expr                 { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_GT, $3); }
    |   expr LEQUAL expr             { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_LE, $3); }
    |   expr GEQUAL expr             { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_GE, $3); }

    |   EMARK expr  %prec NOT        { $$ = new TxUnaryLogicalNotNode(@1, $2); }  // unary not
    |   expr AAND expr               { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_AND, $3); }
    |   expr PIPE expr               { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_OR,  $3); }
    ;

value_literal
        :       LIT_DEC_INT   { $$ = new TxIntegerLitNode(@1, $1, false); }
        |       LIT_RADIX_INT { $$ = new TxIntegerLitNode(@1, $1, true); }
        |       LIT_FLOATING  { $$ = new TxFloatingLitNode(@1, $1); }
        |       LIT_CHARACTER { $$ = new TxCharacterLitNode(@1, $1); }
        |       LIT_CSTRING   { $$ = new TxCStringLitNode(@1, $1); }
        |       KW_NULL       { $$ = new TxBoolLitNode(@1, false); }  // TODO: proper Null type
        |       KW_FALSE      { $$ = new TxBoolLitNode(@1, false); }
        |       KW_TRUE       { $$ = new TxBoolLitNode(@1, true); }
        // |       LIT_STRING    { $$ = new TxStringLitNode(@1, $1); }
    ;

array_literal : LBRACKET expr COMMA array_lit_expr_list RBRACKET  { (*$4)[0] = $2;  $$ = new TxArrayLitNode(@1, $4); }
              | LBRACKET expr RBRACKET  { $$ = new TxArrayLitNode(@1, new std::vector<TxExpressionNode*>( { $2 } )); }

              // produces a (harmless) shift-reduce warning but unknown how to suppress that:
              | LBRACKET expr RBRACKET conv_type_expr LPAREN expression_list RPAREN  { $$ = new TxArrayLitNode(@1, $4, $6, $2); }
              | LBRACKET expr RBRACKET conv_type_expr LPAREN RPAREN  { $$ = new TxArrayLitNode(@1, $4, new std::vector<TxExpressionNode*>(), $2); }
              | LBRACKET RBRACKET conv_type_expr LPAREN expression_list RPAREN       { $$ = new TxArrayLitNode(@1, $3, $5); }
              | LBRACKET RBRACKET conv_type_expr LPAREN RPAREN                       { $$ = new TxArrayLitNode(@1, $3); }
              ;
              // LBRACKET RBRACKET - empty, unqualified array literal "[]" illegal since element type can't be determined

array_lit_expr_list : expr  { $$ = new std::vector<TxExpressionNode*>();  $$->push_back(NULL);  $$->push_back($1); }
                    | array_lit_expr_list COMMA expr  { $$ = $1;  $$->push_back($3); }
                    ;

make_expr : KW_NEW type_expression call_params { $$ = new TxNewConstructionNode(@1, $2, $3); }
          | LT type_expression GT call_params { $$ = new TxStackConstructionNode(@1, $2, $4); }
;

call_expr : expr call_params  { $$ = new TxFunctionCallNode(@1, $1, $2); }
;  //  FUTURE: Interface(adaptedObj)

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


//// statements

suite
    :   LBRACE RBRACE                      { $$ = new TxSuiteNode(@1); }
    |   LBRACE statement_list RBRACE       { $$ = new TxSuiteNode(@1, $2); }
    |   LBRACE error RBRACE                { $$ = new TxSuiteNode(@1);     TX_SYNTAX_ERROR; }
    |   LBRACE statement_list error RBRACE { $$ = new TxSuiteNode(@1, $2); TX_SYNTAX_ERROR; }
    ;

statement_list : statement  { $$ = new std::vector<TxStatementNode*>();
                              if ($1) $$->push_back($1); }
               | statement_list statement  { $$ = $1;  if ($2) $$->push_back($2); }
               ;

// statement is a syntactically terminated program statement, either with a separator token,
// or in the case of a suite with a }.
// Conditional statements can be seen as statements prefixed with a condition clause
// (which in itself is not "terminated").
statement
    :   single_statement                         { $$ = $1; }
    |   suite                                    { $$ = $1; }
    ;

single_statement
    :   cond_stmt                  %prec STMT    { $$ = $1; }
    |   simple_stmt                %prec STMT    { $$ = $1; }
    ;

simple_stmt
    :   type_decl_stmt             %prec STMT    { $$ = $1; }
    |   elementary_stmt SEMICOLON  %prec STMT    { $$ = $1; }
    |   cond_else_stmt             %prec KW_ELSE { $$ = $1; }
    |   experr_stmt                %prec STMT    { $$ = $1; }
    |   error SEMICOLON            %prec STMT    { $$ = new TxNoOpStmtNode(@1); TX_SYNTAX_ERROR; }
    ;


cond_stmt        : KW_IF    cond_expr COLON single_statement  %prec STMT  { $$ = new TxIfStmtNode   (@1, $2, $4); }
                 | KW_IF    cond_expr suite                   %prec STMT  { $$ = new TxIfStmtNode   (@1, $2, $3); }
                 | KW_WHILE cond_expr COLON single_statement  %prec STMT  { $$ = new TxWhileStmtNode(@1, $2, $4); }
                 | KW_WHILE cond_expr suite                   %prec STMT  { $$ = new TxWhileStmtNode(@1, $2, $3); }
                 ;

cond_else_stmt   : KW_IF    cond_expr COLON simple_stmt else_clause  %prec KW_ELSE  { $$ = new TxIfStmtNode   (@1, $2, $4, $5); }
                 | KW_IF    cond_expr suite             else_clause  %prec KW_ELSE  { $$ = new TxIfStmtNode   (@1, $2, $3, $4); }
                 | KW_WHILE cond_expr COLON simple_stmt else_clause  %prec KW_ELSE  { $$ = new TxWhileStmtNode(@1, $2, $4, $5); }
                 | KW_WHILE cond_expr suite             else_clause  %prec KW_ELSE  { $$ = new TxWhileStmtNode(@1, $2, $3, $4); }
                 ;

cond_expr        : expr %prec STMT    { $$ = $1; } ;

else_clause      : KW_ELSE statement  { $$ = new TxElseClauseNode(@1, $2); } ;


experr_stmt : KW_EXPERR COLON              { BEGIN_TXEXPERR(@1, new ExpectedErrorClause(-1)); }
              statement                    { $$ = new TxExpErrStmtNode(@1, END_TXEXPERR(@4), static_cast<TxStatementNode*>($4)); }
            | KW_EXPERR LIT_DEC_INT COLON  { BEGIN_TXEXPERR(@1, new ExpectedErrorClause(std::stoi($2))); }
              statement                    { $$ = new TxExpErrStmtNode(@1, END_TXEXPERR(@5), $5); }
            ;

elementary_stmt
    :   field_def       { $$ = new TxFieldStmtNode(@1, $1); }
    |   call_expr       { $$ = new TxCallStmtNode(@1, $1); } // function call without return value assignment
    |   assignment_stmt { $$ = $1; }
    |   return_stmt     { $$ = $1; }
    |   break_stmt      { $$ = $1; }
    |   continue_stmt   { $$ = $1; }
    |   assert_stmt     { $$ = $1; }
    ;

// TODO: support declaration flags abstract, final, and maybe static
type_decl_stmt  : type_or_if NAME type_spec
                    { $$ = new TxTypeStmtNode(@1, $2, NULL, $3, $1); }
                | type_or_if NAME LT type_param_list GT type_spec
                    { $$ = new TxTypeStmtNode(@1, $2, $4,   $6, $1); }
                ;


return_stmt : KW_RETURN expr  { $$ = new TxReturnStmtNode(@1, $2); }
            | KW_RETURN       { $$ = new TxReturnStmtNode(@1); }
            ;

break_stmt     : KW_BREAK     { $$ = new TxBreakStmtNode(@1); }  ;
continue_stmt  : KW_CONTINUE  { $$ = new TxContinueStmtNode(@1); }  ;

assert_stmt : KW_ASSERT expr  { $$ = new TxAssertStmtNode(@1, $2); }
            // | KW_ASSERT expr COMMA expr
            ;


assignment_stmt //:    assignee_pattern EQUAL expr
                :    assignee_expr EQUAL expr  { $$ = new TxAssignStmtNode(@1, $1, $3); }
//                |    assignee_expr PLUSEQUAL expr
//                |    assignee_expr MINUSEQUAL expr
//                |    assignee_expr ASTERISKEQUAL expr
//                |    assignee_expr FSLASHEQUAL expr
                ;

//assignee_pattern : nested_assignee (',' nested_assignee)* ;
//nested_assignee  : assignee_expr | '{' assignee_pattern '}' ;

assignee_expr  // expressions capable of (but not guaranteed) to produce an lvalue
    :   NAME             { $$ = new TxFieldAssigneeNode(@1, new TxFieldValueNode(@1, NULL, $1)); }
    |   expr DOT NAME    { $$ = new TxFieldAssigneeNode(@1, new TxFieldValueNode(@1, $1,   $3)); }
    |   expr CARET       { $$ = new TxDerefAssigneeNode(@1, $1); }  // unary value-of suffix
    |   expr LBRACKET expr RBRACKET { $$ = new TxElemAssigneeNode(@1, $1, $3); }
    ;

%%

void yy::TxParser::error (const location_type& l, const std::string& m) {
    parserCtx->cerror (l, m);
}
