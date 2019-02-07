#include "token.h"

#include <stdio.h>

namespace Zodiac
{

    static const char* _token_kind_strings[] =
    {
        "TOK_INVALID",

        "TOK_PLUS",       // +
        "TOK_MINUS",      // -
        "TOK_DIV",        // /

        "TOK_COLON",      // :
        "TOK_SEMICOLON",  // ;

        "TOK_LPAREN",     // (
        "TOK_RPAREN",     // )
        "TOK_LBRACE",     // {
        "TOK_RBRACE",     // }

        "TOK_RARROW",     // ->

        "TOK_IDENTIFIER",
        "TOK_INTEGER",

        "TOK_COUNT",
    };

    void print_token(const Token& token)
    {
        printf("%lu: %-14s: '%s'\n",
               token.file_pos.line,
               _token_kind_strings[token.kind],
               token.atom.data);
    }
}
