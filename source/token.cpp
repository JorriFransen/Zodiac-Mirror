#include "token.h"

#include <stdio.h>

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
        "TOK_LTEQ"        // <=

        "TOK_COLON",      // :
        "TOK_SEMICOLON",  // ;
        "TOK_COMMA",      // ,

        "TOK_LPAREN",     // (
        "TOK_RPAREN",     // )
        "TOK_LBRACE",     // {
        "TOK_RBRACE",     // }

        "TOK_RARROW",     // ->

        "TOK_IDENTIFIER",
        "TOK_INTEGER",

        "TOK_KW_RETURN",
        "TOK_KW_IF",
        "TOK_KW_ELSE",

        "TOK_COUNT",
    };

    const char* token_kind_string(Token_Kind token_kind)
    {
        assert(token_kind >= TOK_INVALID && token_kind <= TOK_COUNT);

        return _token_kind_strings[token_kind];
    }

    void print_token(const Token& token)
    {
        printf("%lu: %-14s: '%s'\n",
               token.file_pos.line,
               _token_kind_strings[token.kind],
               token.atom.data);
    }
}
