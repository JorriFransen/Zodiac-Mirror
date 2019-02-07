#pragma once

#include "atom.h"
#include "file_pos.h"

namespace Zodiac
{
    enum Token_Kind
    {
        TOK_INVALID,

        TOK_PLUS,       // +
        TOK_MINUS,      // -
        TOK_DIV,        // /

        TOK_COLON,      // :
        TOK_SEMICOLON,  // ;

        TOK_LPAREN,     // (
        TOK_RPAREN,     // )
        TOK_LBRACE,     // {
        TOK_RBRACE,     // }

        TOK_RARROW,     // ->

        TOK_IDENTIFIER,
        TOK_INTEGER,

        TOK_COUNT,
    };
    struct Token
    {
        Token_Kind kind = TOK_INVALID;
        File_Pos file_pos = {};
        Atom atom = {};
    };

    void print_token(const Token& token);
}
