#include "token.h"

#include <stdio.h>
#include <inttypes.h>

namespace Zodiac
{

    static const char* _token_kind_strings[] =
    {
        "TOK_INVALID",

        "TOK_PLUS",       // +
        "TOK_MINUS",      // -
        "TOK_MUL",        // *
        "TOK_DIV",        // /
        "TOK_EQ",         // =
        "TOK_LT",         // <
        "TOK_LTEQ",       // <=
        "TOK_GT",         // >
        "TOK_GTEQ",       // >=

        "TOK_POUND",      // #

        "TOK_COLON",      // :
        "TOK_SEMICOLON",  // ;
        "TOK_COMMA",      // ,

        "TOK_LPAREN",     // (
        "TOK_RPAREN",     // )
        "TOK_LBRACE",     // {
        "TOK_RBRACE",     // }
        "TOK_LBRACK",     // [
        "TOK_RBRACK",     // ]

        "TOK_RARROW",     // ->
        "TOK_ELLIPSIS",   // ...

        "TOK_IDENTIFIER",
        "TOK_INTEGER",
        "TOK_CHAR_LIT",
        "TOK_STRING_LIT",

        "TOK_KW_RETURN",
        "TOK_KW_IF",
        "TOK_KW_ELSE",
        "TOK_KW_FOREIGN",
        "TOK_KW_DYN_LINK",
        "TOK_KW_WHILE",
        "TOK_KW_FOR",
        "TOK_KW_ARRAY_LENGTH",

        "TOK_COUNT",
    };

    const char* token_kind_string(Token_Kind token_kind)
    {
        assert(token_kind >= TOK_INVALID && token_kind <= TOK_COUNT);

        return _token_kind_strings[token_kind];
    }

    void print_token(const Token& token)
    {
        printf("%" PRIu64 ": %-14s: '%s'\n",
               token.file_pos.line,
               _token_kind_strings[token.kind],
               token.atom.data);
    }
}
