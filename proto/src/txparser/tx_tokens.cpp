#pragma clang diagnostic push
#pragma ide diagnostic ignored "cert-err58-cpp"
#include "tx_tokens.hpp"

#include <map>


#define TOKEN_LABEL( token )  { TxTokenId::token, #token }

static const std::map<TxTokenId, std::string> token_labels = {
        TOKEN_LABEL( WHITESPACE ),
        TOKEN_LABEL( COMMENT ),
        TOKEN_LABEL( COMMENT ),

        /* statement separators */
        TOKEN_LABEL( NEWLINE ),
        TOKEN_LABEL( SEMICOLON ),
        TOKEN_LABEL( LBRACE ),
        TOKEN_LABEL( RBRACE ),

        /* operators */
        TOKEN_LABEL( LPAREN ),
        TOKEN_LABEL( RPAREN ),
        TOKEN_LABEL( LBRACKET ),
        TOKEN_LABEL( RBRACKET ),
        TOKEN_LABEL( COMMA ),
        TOKEN_LABEL( COLON ),
        TOKEN_LABEL( DOT ),
        TOKEN_LABEL( DOTDOT ),
        TOKEN_LABEL( ELLIPSIS ),
        TOKEN_LABEL( ASTERISK ),
        TOKEN_LABEL( PLUS ),
        TOKEN_LABEL( MINUS ),
        TOKEN_LABEL( FSLASH ),
        TOKEN_LABEL( BSLASH ),
        TOKEN_LABEL( AAND ),
        TOKEN_LABEL( PIPE ),
        TOKEN_LABEL( CARET ),
        TOKEN_LABEL( TILDE ),
        TOKEN_LABEL( PERCENT ),
        TOKEN_LABEL( PERCENTPERCENT ),
        TOKEN_LABEL( DOLLAR ),
        TOKEN_LABEL( QMARK ),
        TOKEN_LABEL( EMARK ),
        TOKEN_LABEL( DASHGT ),
        TOKEN_LABEL( LTCOLON ),
        TOKEN_LABEL( EQUAL ),
        TOKEN_LABEL( EEQUAL ),
        TOKEN_LABEL( NEQUAL ),
        TOKEN_LABEL( EEEQUAL ),
        TOKEN_LABEL( NEEQUAL ),
        TOKEN_LABEL( LT ),
        TOKEN_LABEL( LTLT ),
        TOKEN_LABEL( GT ),
        TOKEN_LABEL( GTGT  ),
        TOKEN_LABEL( GTGTGT  ),
        TOKEN_LABEL( LEQUAL ),
        TOKEN_LABEL( GEQUAL ),
        TOKEN_LABEL( COLEQUAL ),
        TOKEN_LABEL( PLUSEQUAL ),
        TOKEN_LABEL( MINUSEQUAL ),
        TOKEN_LABEL( ASTERISKEQUAL ),
        TOKEN_LABEL( FSLASHEQUAL ),

        /* keywords */
        TOKEN_LABEL( KW_MODULE ),
        TOKEN_LABEL( KW_IMPORT ),
        TOKEN_LABEL( KW_TYPE ),
        TOKEN_LABEL( KW_INTERFACE ),
        TOKEN_LABEL( KW_BUILTIN ),
        TOKEN_LABEL( KW_VIRTUAL ),
        TOKEN_LABEL( KW_ABSTRACT ),
        TOKEN_LABEL( KW_FINAL ),
        TOKEN_LABEL( KW_OVERRIDE ),
        TOKEN_LABEL( KW_EXTERNC ),
        TOKEN_LABEL( KW_MUTABLE ),
        TOKEN_LABEL( KW_REFERENCE ),
        TOKEN_LABEL( KW_DERIVES ),
        TOKEN_LABEL( KW_WHILE ),
        TOKEN_LABEL( KW_FOR ),
        TOKEN_LABEL( KW_IF ),
        TOKEN_LABEL( KW_ELSE ),
        TOKEN_LABEL( KW_IN ),
        TOKEN_LABEL( KW_IS ),
        TOKEN_LABEL( KW_RETURN ),
        TOKEN_LABEL( KW_BREAK ),
        TOKEN_LABEL( KW_CONTINUE ),
        TOKEN_LABEL( KW_NEW ),
        TOKEN_LABEL( KW_DELETE ),
        TOKEN_LABEL( KW_XOR ),
        TOKEN_LABEL( KW_PANIC ),
        TOKEN_LABEL( KW_ASSERT ),
        TOKEN_LABEL( KW_EXPERR ),

        TOKEN_LABEL( KW__ADDRESS ),
        TOKEN_LABEL( KW__TYPEID ),
        TOKEN_LABEL( KW__SIZEOF ),
        TOKEN_LABEL( KW__SUPERTYPES ),

        /* reserved but not currently used: */
        TOKEN_LABEL( AT ),
        TOKEN_LABEL( EURO ),
        TOKEN_LABEL( COLONGT ),
        TOKEN_LABEL( KW_PUBLIC ),
        TOKEN_LABEL( KW_PROTECTED ),
        TOKEN_LABEL( KW_STATIC ),
        TOKEN_LABEL( KW_CONST ),
        TOKEN_LABEL( KW_EXTENDS ),
        TOKEN_LABEL( KW_IMPLEMENTS ),
        TOKEN_LABEL( KW_SWITCH ),
        TOKEN_LABEL( KW_CASE ),
        TOKEN_LABEL( KW_WITH ),
        TOKEN_LABEL( KW_AS ),
        TOKEN_LABEL( KW_AND ),
        TOKEN_LABEL( KW_OR ),
        TOKEN_LABEL( KW_NOT ),
        TOKEN_LABEL( KW_TRY ),
        TOKEN_LABEL( KW_EXCEPT ),
        TOKEN_LABEL( KW_FINALLY ),
        TOKEN_LABEL( KW_RAISE ),
        TOKEN_LABEL( KW_RAISES ),

        /* literals */
        TOKEN_LABEL( LIT_DEC_INT ),
        TOKEN_LABEL( LIT_RADIX_INT ),
        TOKEN_LABEL( LIT_FLOATING ),
        TOKEN_LABEL( LIT_CHARACTER ),
        TOKEN_LABEL( LIT_CSTRING ),
        TOKEN_LABEL( LIT_STRING ),
        TOKEN_LABEL( ARRAY_LIT ),

        /* TRUE and FALSE are parsed as keywords until they can be implemented using proper Enum facility */
        TOKEN_LABEL( KW_TRUE ),
        TOKEN_LABEL( KW_FALSE ),

        /* string format operators */
        TOKEN_LABEL( SF_PARAM ),
        TOKEN_LABEL( SF_FLAGS ),
        TOKEN_LABEL( SF_WIDTH ),
        TOKEN_LABEL( SF_PREC ),
        TOKEN_LABEL( SF_TYPE ),
        TOKEN_LABEL( SF_MINUS ),
        TOKEN_LABEL( SF_PLUS ),
        TOKEN_LABEL( SF_SPACE ),
        TOKEN_LABEL( SF_ZERO ),
        TOKEN_LABEL( SF_HASH ),

        /* identifiers */
        TOKEN_LABEL( NAME ),
        TOKEN_LABEL( HASHINIT ),
        TOKEN_LABEL( HASHSELF ),

        /* precedence operators, not actually lexically produced */
        TOKEN_LABEL( STMT ),
        TOKEN_LABEL( TYPE ),
        TOKEN_LABEL( EXPR ),
        TOKEN_LABEL( NOT ),
        TOKEN_LABEL( NEG ),
        TOKEN_LABEL( ADDR ),

        /* for symbol lookup completeness, matched separately */
        TOKEN_LABEL( INDENT ),
        TOKEN_LABEL( DEDENT ),

        /* for symbol lookup completeness, not actually lexically produced */
        TOKEN_LABEL( ERROR ),
        TOKEN_LABEL( END ),
};

const std::string& get_tx_token_label( TxTokenId id ) {
    return token_labels.at( id );
}

#pragma clang diagnostic pop
