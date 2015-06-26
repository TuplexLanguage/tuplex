%require "3.0"  // initially authored with Bison 3.0.2
%language "C++"
%skeleton "lalr1.cc"

%defines
%define parser_class_name {TxParser}
//%define api.token.constructor
%define api.value.type variant
//%define parse.assert

// Pass the parsing context to yylex() and yyparse()
%param { TxDriver& driver }
%locations  // populates YYLTYPE


// what YYSTYPE and YYLTYPE require:
%code requires
{
#include <string>
#include "ast.hpp"
class TxDriver;
}

// Tell Flex the lexer's prototype:
%code provides
{
# define YY_DECL                    \
  yy::TxParser::token_type                         \
  yylex (yy::TxParser::semantic_type* yylval,      \
         yy::TxParser::location_type* yylloc,      \
         TxDriver& driver)
// declare yylex for the parser's sake
YY_DECL;
}

%{
#include "tx_lang_defs.hpp"
#include "tx_operations.hpp"
%}


%initial-action
{
    // Initialize the initial location.
    // Afterward new locations are computed relatively to the previous locations: the file name will be propagated.
    @$.begin.filename = @$.end.filename = driver.current_input_filepath();
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
%token DOT COLON COMMA ASTERISK PLUS MINUS FSLASH BSLASH AAND
%token PIPE CARET TILDE AT PERCENT DOLLAR EURO LPAREN RPAREN LBRACE
%token RBRACE LBRACKET RBRACKET QMARK EMARK
%token EQUAL EEQUAL NEQUAL EEEQUAL NEEQUAL LT GT LEQUAL GEQUAL
%token COLEQUAL PLUSEQUAL MINUSEQUAL ASTERISKEQUAL FSLASHEQUAL

/* keywords: */
%token KW_MODULE KW_IMPORT KW_PUBLIC KW_PROTECTED KW_STATIC KW_TYPE KW_CLASS KW_INTERFACE KW_ABSTRACT
%token KW_FINAL KW_OVERRIDE KW_MODIFIABLE KW_REFERENCE KW_EXTENDS KW_IMPLEMENTS KW_DERIVES
%token KW_FUNC KW_TUPLE KW_UNION KW_ENUM
%token KW_WHILE KW_FOR KW_IF KW_ELSE KW_SWITCH KW_CASE KW_WITH KW_IN KW_IS KW_AS KW_OR
%token KW_RAISES KW_TRY KW_EXCEPT KW_FINALLY KW_RAISE
%token KW_RETURN KW_BREAK KW_CONTINUE KW_NEW KW_FROM
%token KW_NULL KW_TRUE KW_FALSE

/* keywords reserved but not currently used */
%token KW_AND KW_XOR KW_NOT KW_BUILTIN KW_LAMBDA

 /* literals: */
%token <std::string> NAME LIT_DEC_INT LIT_RADIX_INT LIT_FLOATING LIT_CHARACTER LIT_CSTRING LIT_STRING

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above.
 */
%type <TxDeclarationFlags> declaration_flags
%type <bool> opt_modifiable
%type <TxIdentifier*> identifier

%type <TxIdentifierNode*> compound_identifier
%type <TxIdentifierNode*> module_declaration opt_module_decl opt_dataspace

%type <TxParsingUnitNode*> parsing_unit
%type <TxModuleNode*> sub_module

%type <std::vector<TxImportNode*> *> import_statements opt_import_stmts
%type <TxImportNode *> import_statement

%type <std::vector<TxDeclarationNode*> *> module_members opt_module_members type_members opt_type_members type_param_list
%type <TxDeclarationNode *> module_member member_declaration type_param

%type <std::vector<TxPredefinedTypeNode*> *> opt_base_types predef_type_list
%type <TxPredefinedTypeNode*> predef_type

%type <TxTypeArgumentNode *> type_arg
%type <std::vector<TxTypeArgumentNode*> *> type_arg_list

%type <TxFieldDefNode*> field_def field_type_def field_assignment_def method_def
%type <std::vector<TxFieldDefNode*> *> params_def field_type_list

%type <TxTypeExpressionNode*> type_spec type_extension type_expression base_type_expression return_type_def
%type <TxTypeExpressionNode*> reference_type array_type //data_tuple_type

%type <TxFunctionTypeNode*> function_type function_header
%type <TxExpressionNode*> expr make_expr lambda_expr value_literal array_dimensions
%type <TxFunctionCallNode*> call_expr
%type <std::vector<TxExpressionNode*> *> expression_list call_params
%type <TxSuiteNode*> suite
%type <std::vector<TxStatementNode*> *> statement_list
%type <TxStatementNode*> statement assignment_stmt return_stmt break_stmt continue_stmt type_decl_stmt
%type <TxStatementNode*> non_cond_stmt cond_stmt cond_else_stmt else_clause
%type <TxAssigneeNode*> assignee_expr


/* Operator precedence for expression operators (higher line no = higher precedence) */
%precedence STMT /* used to specify statement / member rule precedence, to be lower than e.g. separator  */
%precedence EXPR
%left COMMA COLON
%right EQUAL
%left AAND PIPE  // boolean (logical, not bitwise) operators
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
%precedence DOT
%right KW_MODULE KW_IMPORT  /* high token shift precedence */
%right KW_ELSE
%right NL SEMICOLON     /* semantic statement separator, always/greedily shift */

%start parsing_unit

%%

sep_token : NL | SEMICOLON ;
sep       : sep_token %prec STMT
          | sep sep_token %prec NL ;     // one or more successive separator tokens
opt_sep   : sep     %prec NL     // zero or more successive separator tokens
          | %empty  %prec STMT;  // (lower prio to reduce than shift another separator token)

// tokens that may be adjacent to any number of successive separators:
sCOMMA  : COMMA opt_sep   %prec COMMA;
sCOLON  : COLON opt_sep   %prec COLON;
sEQUAL  : EQUAL opt_sep   %prec EQUAL;
sLPAREN : LPAREN opt_sep  %prec LPAREN;
sRPAREN : opt_sep RPAREN  %prec RPAREN;
sLBRACE : LBRACE opt_sep  %prec LBRACE;
sRBRACE : opt_sep RBRACE  %prec RBRACE;
// Note: Difficult to enable free sprinkling of statement separators around tokens, since these tend
// to be shifted instead of being reduced with statement-terminating separator.



parsing_unit : opt_sep  opt_module_decl
               opt_sep  opt_import_stmts
               opt_sep  opt_module_members
                   { if (!driver.parsingUnit)  driver.parsingUnit = new TxParsingUnitNode(@2);
                     $$ = driver.parsingUnit;
                     driver.validate_module_name($2->ident);
                     driver.parsingUnit->add_module(new TxModuleNode(@2, $2, $4, $6, NULL));
                   }
    ;

sub_module : KW_MODULE compound_identifier opt_sep
               sLBRACE  opt_import_stmts  opt_sep  opt_module_members  sRBRACE
                 { if (!driver.parsingUnit)  driver.parsingUnit = new TxParsingUnitNode(@1);
                   $$ = new TxModuleNode(@1, $2, $5, $7, NULL);
                   driver.validate_module_name($2->ident);
                   driver.parsingUnit->add_module($$);
                 }
    ;



// An 'identifier' refers to an entity declared elsewhere, and contains one or more period-separated 'segments'.
// Example of fully qualified static name: my.mod.MyClass.staticField.myMethod.$.InnerClass.staticField2
// Example of fully qualified local name: my.mod.MyClass.staticField.myMethod.$.self

identifier : NAME                     { $$ = new TxIdentifier($1); }
           | identifier DOT NAME      { $$ = $1; $$->append($3); }
           | identifier DOT ASTERISK  { $$ = $1; $$->append("*"); }
           ;

compound_identifier : identifier  %prec EXPR  { $$ = new TxIdentifierNode(@1, $1); } ;



opt_module_decl    : %empty %prec STMT { $$ = new TxIdentifierNode(@$, new TxIdentifier(LOCAL_NS)); }
                   | module_declaration { $$ = $1; } ;

module_declaration : KW_MODULE compound_identifier sep { $$ = $2; } ;

opt_import_stmts   : %empty { $$ = new std::vector<TxImportNode*>(); }
                   | import_statements { $$ = $1; } ;

import_statements  : import_statement
                     { $$ = new std::vector<TxImportNode*>(); if ($1) $$->push_back($1); }
                   | import_statements import_statement
                     { $$ = $1; if ($2) $$->push_back($2); }
                   ;

import_statement   : KW_IMPORT compound_identifier sep  %prec STMT
                        { $$ = new TxImportNode(@1, $2);
                          if ($2->ident.is_qualified())
                              if (! driver.add_import($2->ident.parent()))
                                  driver.cerror(@1, "Failed to import module (source not found): " + $2->ident.parent().to_string()); }
                   | KW_IMPORT error sep              %prec STMT { $$ = NULL; }
                   ;


opt_module_members : %empty { $$ = new std::vector<TxDeclarationNode*>(); }
                   | module_members { $$ = $1; } ;

module_members : module_member
                     { $$ = new std::vector<TxDeclarationNode*>();
                       if ($1 != NULL)
                           $$->push_back($1); }
               | module_members module_member
                     { $$ = $1;
                       if ($2 != NULL)
                           $$->push_back($2); }
;

module_member : member_declaration { $$ = $1; }
              | sub_module opt_sep { $$ = NULL; }
;

member_declaration
    // field
    : declaration_flags field_def sep  %prec STMT  { $$ = new TxFieldDeclNode(@1, $1, $2); }

    // type
    | declaration_flags KW_TYPE NAME type_spec sep  %prec STMT
            { $$ = new TxTypeDeclNode(@1, $1, $3, NULL, $4); }
    | declaration_flags KW_TYPE NAME LT type_param_list GT type_spec sep  %prec STMT
            { $$ = new TxTypeDeclNode(@1, $1, $3, $5, $7); }

    // function / method
    |   declaration_flags method_def sep  %prec STMT
            { $$ = new TxFieldDeclNode(@1, $1, $2, true); }

    // error recovery
    |   error sep  %prec STMT  { $$ = NULL; }
    ;

// TODO: add KW_ABSTRACT, and KW_OVERRIDE (valid for static member fields)
declaration_flags
    : %empty                                { $$ = TXD_NONE; }
    | KW_PUBLIC                             { $$ = TXD_PUBLIC; }
    | KW_PUBLIC KW_STATIC                   { $$ = TXD_PUBLIC | TXD_STATIC; }
    | KW_PUBLIC KW_FINAL                    { $$ = TXD_PUBLIC | TXD_FINAL; }
    | KW_PUBLIC KW_STATIC KW_FINAL          { $$ = TXD_PUBLIC | TXD_STATIC | TXD_FINAL; }
    | KW_PROTECTED                          { $$ = TXD_PROTECTED; }
    | KW_PROTECTED KW_STATIC                { $$ = TXD_PROTECTED | TXD_STATIC; }
    | KW_PROTECTED KW_FINAL                 { $$ = TXD_PROTECTED | TXD_FINAL; }
    | KW_PROTECTED KW_STATIC KW_FINAL       { $$ = TXD_PROTECTED | TXD_STATIC | TXD_FINAL; }
    | KW_STATIC                             { $$ = TXD_STATIC; }
    | KW_FINAL                              { $$ = TXD_FINAL; }
    | KW_STATIC KW_FINAL                    { $$ = TXD_STATIC | TXD_FINAL; }
    ;


field_def : field_type_def { $$ = $1; } | field_assignment_def { $$ = $1; } ;

field_type_def : NAME sCOLON type_expression
                     { $$ = new TxFieldDefNode(@1, $1, $3, NULL); }
;

field_assignment_def : NAME sCOLON type_expression sEQUAL expr
                           { $$ = new TxFieldDefNode(@1, $1, $3, $5); }
                     | NAME COLEQUAL expr
                           { $$ = new TxFieldDefNode(@1, $1, $3); }
                     | TILDE NAME COLEQUAL expr
                           { $$ = new TxFieldDefNode(@1, $2, $4, true); }
                     // TODO |   assignee_pattern COLEQUAL expr
;


//// types:

type_param_list : type_param  { $$ = new std::vector<TxDeclarationNode*>(); $$->push_back($1); }
                | type_param_list sCOMMA type_param  { $$ = $1; $$->push_back($3); }
                ;
type_param      : NAME  { $$ = new TxTypeDeclNode(@1, TXD_PUBLIC | TXD_GENPARAM, $1, NULL, new TxPredefinedTypeNode(@1, new TxIdentifierNode(@1, new TxIdentifier("tx.Any")))); }
                | NAME KW_DERIVES predef_type { $$ = new TxTypeDeclNode(@1, TXD_PUBLIC | TXD_GENPARAM, $1, NULL, $3); }
                | field_type_def  { $$ = new TxFieldDeclNode(@1, TXD_PUBLIC | TXD_GENPARAM, $1); }
                ;


type_spec : type_expression { $$ = $1; }
          | type_extension { $$ = $1; }
//          | type_interface { $$ = $1; }
          ;

type_extension : opt_modifiable opt_base_types opt_sep sLBRACE  opt_type_members  sRBRACE
                        { $$ = new TxDerivedTypeNode(@1, $1, $2, $5); }
               ;

opt_type_members : %empty { $$ = new std::vector<TxDeclarationNode*>(); }
                 | type_members { $$ = $1; } ;

type_members : member_declaration
                     { $$ = new std::vector<TxDeclarationNode*>();
                       if ($1 != NULL)
                           $$->push_back($1); }
             | type_members member_declaration
                     { $$ = $1;
                       if ($2 != NULL)
                           $$->push_back($2); }
;

opt_base_types  : %empty    { $$ = new std::vector<TxPredefinedTypeNode*>(); }
                | KW_DERIVES predef_type_list  { $$ = $2; }
                // (all but the first must be interface types)
                ;

predef_type_list: predef_type  { $$ = new std::vector<TxPredefinedTypeNode*>();  $$->push_back($1); }
                | predef_type_list sCOMMA predef_type  { $$ = $1;  $$->push_back($3); }
                ;

predef_type     : compound_identifier                      { $$ = new TxPredefinedTypeNode(@1, $1); }
                | compound_identifier LT GT  { $$ = new TxPredefinedTypeNode(@1, $1, new std::vector<TxTypeArgumentNode*>()); }
                | compound_identifier LT type_arg_list GT  { $$ = new TxPredefinedTypeNode(@1, $1, $3); }
                ;

type_arg_list   : type_arg  { $$ = new std::vector<TxTypeArgumentNode*>();  $$->push_back($1); }
                | type_arg_list sCOMMA type_arg  { $$ = $1;  $$->push_back($3); }
                ;

type_arg        : value_literal       { $$ = new TxTypeArgumentNode($1); }  // unambiguous value expr
                | LPAREN expr RPAREN  { $$ = new TxTypeArgumentNode($2); }  // parens prevent conflation with type expr
                | opt_modifiable predef_type   { auto typeNode = ( $1 ? new TxModifiableTypeNode(@1, $2)
                                                                      : new TxMaybeModTypeNode(@2, $2) );
                                                 $$  = new TxTypeArgumentNode(typeNode); }
                // TODO: syntactic sugar for reference and array types (possibly chained):
                // | opt_modifiable AAND predef_type
                // | opt_modifiable LBRACKET RBRACKET predef_type
                ;


type_expression  // can construct new "literal" type but can't extend (subclass / add members to) one
    :   opt_modifiable base_type_expression  { $$ = ( $1 ? new TxModifiableTypeNode(@1, $2)
                                                         : new TxMaybeModTypeNode(@2, $2) ); }
    ;

opt_modifiable : %empty { $$ = false; } | TILDE { $$ = true; } | KW_MODIFIABLE { $$ = true; } ;

base_type_expression
    // the tuple base type is extendable; the others are not (though non-references may be virtually inherited)
    :  predef_type      { $$ = $1; }
    |  reference_type   { $$ = $1; }
    |  array_type       { $$ = $1; }
    |  function_type    { $$ = $1; }
//    |  data_tuple_type  { $$ = $1; }
//    |  union_type
//    |  enum_type
//    |  range_type
//    |  shared_obj_type
    ;

reference_type : opt_dataspace ref_token type_expression
                    { /* (custom ast node needed to handle dataspaces) */
                      $$ = new TxReferenceTypeNode(@2, $1, $3);
                    } ;
opt_dataspace : %empty { $$ = NULL; } | QMARK { $$ = NULL; } | compound_identifier { $$ = $1; } ;
ref_token : KW_REFERENCE | AAND ;

array_type : array_dimensions type_expression
                    { /* (custom ast node needed to provide syntactic sugar for modifiable decl) */
                      $$ = new TxArrayTypeNode(@1, $2, $1);
                    } ;
array_dimensions : LBRACKET expr RBRACKET  { $$ = $2; }
                 //| LBRACKET predef_type RBRACKET  // predef_type must be an enum
                 | LBRACKET RBRACKET  { $$ = NULL; }
                 ;

//data_tuple_type  // simple tuple type with no static fields; fields implicitly public
//    : KW_TUPLE opt_sep opt_modifiable opt_sep LBRACE opt_sep field_type_list opt_sep RBRACE
//            { $$ = new TxTupleTypeNode(@1, $3, $7); }
//    ;

//union_type     : KW_UNION sLBRACE type_expr_list sRBRACE ;
//type_expr_list : type_expression
//               | type_expr_list sCOMMA type_expression
//               ;
//
//enum_type       : KW_ENUM sLBRACE enum_value_list sRBRACE ;
//enum_value_list : NAME
//                | enum_value_list sCOMMA NAME
//                ;
//
//range_type : expr ':' ( expr ':' )? expr ;  // Enum and Scalar types are legal



/// function type and function declarations:

lambda_expr : function_type sep suite  { $$ = new TxLambdaExprNode(@1, $1, $3); } ;

function_type : KW_FUNC function_header { $$ = $2; } ;

function_header : opt_modifiable params_def return_type_def
                  { $$ = new TxFunctionTypeNode(@1, $1, $2, $3); }
                ;

params_def : sLPAREN field_type_list sRPAREN  { $$ = $2; }
           | sLPAREN sRPAREN  { $$ = new std::vector<TxFieldDefNode*>(); }
           ;

field_type_list : field_type_def
                      { $$ = new std::vector<TxFieldDefNode*>();
                        $$->push_back($1); }
                | field_type_list sCOMMA field_type_def
                      { $$ = $1;
                        $$->push_back($3); }
                | error  { $$ = new std::vector<TxFieldDefNode*>(); }
                ;

return_type_def : %empty { $$ = NULL; } | type_expression { $$ = $1; } ;



method_def  :   KW_FUNC NAME function_header sep suite
                { $$ = new TxFieldDefNode(@1, $2, NULL, new TxLambdaExprNode(@1, $3, $5, true)); }
            |   NAME function_header sep suite
                { $$ = new TxFieldDefNode(@1, $1, NULL, new TxLambdaExprNode(@1, $2, $4, true)); }
            ;



//// (value) expressions:

expr
    :   LPAREN expr RPAREN  { $$ = $2; }
    |   value_literal       { $$ = $1; }
    |   lambda_expr NL      { $$ = $1; }  // (eats the extra NL emitted by the suite's closing brace)
    |   call_expr           { $$ = $1; }
    |   make_expr           { $$ = $1; }

    |   NAME                                 { $$ = new TxFieldValueNode(@1, NULL, $1); }
    |   expr DOT NAME                        { $$ = new TxFieldValueNode(@3, $1,   $3); }
    |   expr LBRACKET expr RBRACKET          { $$ = new TxElemDerefNode(@2, $1, $3); }
    |   expr CARET                           { $$ = new TxReferenceDerefNode(@2, $1); }
    |   AAND expr                %prec ADDR  { $$ = new TxReferenceToNode(@1, $2); }

    |   MINUS expr  %prec NEG      { $$ = new TxUnaryMinusNode(@1, $2); }  // unary minus
    |   expr PLUS opt_sep expr     { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_PLUS, $4); }
    |   expr MINUS opt_sep expr    { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_MINUS, $4); }
    |   expr ASTERISK opt_sep expr { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_MUL, $4); }
    |   expr FSLASH opt_sep expr   { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_DIV, $4); }
    |   expr EEQUAL opt_sep expr   { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_EQ, $4); }
    |   expr NEQUAL opt_sep expr   { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_NE, $4); }
    |   expr LT opt_sep expr       { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_LT, $4); }
    |   expr GT opt_sep expr       { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_GT, $4); }
    |   expr LEQUAL opt_sep expr   { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_LE, $4); }
    |   expr GEQUAL opt_sep expr   { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_GE, $4); }

    |   EMARK expr  %prec NOT      { $$ = new TxUnaryLogicalNotNode(@1, $2); }  // unary not
    |   expr AAND opt_sep expr     { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_AND, $4); }
    |   expr PIPE opt_sep expr     { $$ = new TxBinaryOperatorNode(@2, $1, TXOP_OR,  $4); }
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

make_expr : KW_NEW type_expression call_params { $$ = new TxNewExprNode(@1, $2, $3); }
          | LT type_expression GT call_params { $$ = new TxStackConstructorNode(@1, $2, $4); }
;

call_expr : expr call_params  { $$ = new TxFunctionCallNode(@1, $1, $2); }
;  //  FUTURE: Interface(adaptedObj)

call_params : LPAREN expression_list RPAREN  { $$ = $2; }
            | LPAREN RPAREN  { $$ = new std::vector<TxExpressionNode*>(); }
;

expression_list : expr
                      { $$ = new std::vector<TxExpressionNode*>();
                        $$->push_back($1); }
                | expression_list sCOMMA expr
                      { $$ = $1;
                        $$->push_back($3); }
;


//// statements

suite
    :   LBRACE opt_sep statement_list opt_sep RBRACE          { $$ = new TxSuiteNode(@1, $3); }
    |   LBRACE opt_sep RBRACE           { $$ = new TxSuiteNode(@1); }
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
    :   suite opt_sep      %prec STMT { $$ = $1; }  // (casts from suite to statement)
    |   non_cond_stmt sep  %prec STMT { $$ = $1; }
    |   cond_else_stmt     %prec KW_ELSE { $$ = $1; }
    |   cond_stmt          %prec STMT { $$ = $1; }
    |   error sep          %prec STMT { $$ = NULL; }
    ;

non_cond_stmt
    :   field_def { $$ = new TxFieldStmtNode(@1, $1); }
    |   call_expr { $$ = new TxCallStmtNode(@1, $1); } // function call without return value assignment
    |   type_decl_stmt  { $$ = $1; }
    |   assignment_stmt { $$ = $1; }
    |   return_stmt     { $$ = $1; }
    |   break_stmt      { $$ = $1; }
    |   continue_stmt   { $$ = $1; }
    ;

// TODO: support declaration flags abstract, final, and maybe static
type_decl_stmt  : KW_TYPE NAME type_spec
                    { $$ = new TxTypeStmtNode(@1, $2, NULL, $3); }
                | KW_TYPE NAME LT type_param_list GT type_spec
                    { $$ = new TxTypeStmtNode(@1, $2, $4, $6); }
                ;

else_clause     : KW_ELSE opt_sep statement     { $$ = new TxElseClauseNode(@1, $3); } ;

cond_stmt       : KW_IF    expr opt_sep statement  { $$ = new TxIfStmtNode(@1, $2, $4); }
                | KW_WHILE expr opt_sep statement  { $$ = new TxWhileStmtNode(@1, $2, $4); }
                ;

cond_else_stmt  : KW_IF    expr opt_sep non_cond_stmt sep else_clause  { $$ = new TxIfStmtNode(@1, $2, $4, (TxElseClauseNode*)$6); }
                | KW_IF    expr opt_sep cond_else_stmt    else_clause  { $$ = new TxIfStmtNode(@1, $2, $4, (TxElseClauseNode*)$5); }
                | KW_IF    expr opt_sep suite     opt_sep else_clause  { $$ = new TxIfStmtNode(@1, $2, $4, (TxElseClauseNode*)$6); }
                | KW_WHILE expr opt_sep non_cond_stmt sep else_clause  { $$ = new TxWhileStmtNode(@1, $2, $4, (TxElseClauseNode*)$6); }
                | KW_WHILE expr opt_sep cond_else_stmt    else_clause  { $$ = new TxWhileStmtNode(@1, $2, $4, (TxElseClauseNode*)$5); }
                | KW_WHILE expr opt_sep suite     opt_sep else_clause  { $$ = new TxWhileStmtNode(@1, $2, $4, (TxElseClauseNode*)$6); }
                ;


return_stmt : KW_RETURN expr  { $$ = new TxReturnStmtNode(@1, $2); }
            | KW_RETURN       { $$ = new TxReturnStmtNode(@1, NULL); }
            ;

break_stmt     : KW_BREAK     { $$ = new TxBreakStmtNode(@1); }  ;
continue_stmt  : KW_CONTINUE  { $$ = new TxContinueStmtNode(@1); }  ;


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
    driver.cerror (l, m);
}
