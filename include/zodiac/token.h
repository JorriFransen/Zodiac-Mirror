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
        TOK_MUL,        // *
        TOK_DIV,        // /
        TOK_EQ,         // =

        TOK_COLON,      // :
        TOK_SEMICOLON,  // ;
        TOK_COMMA,      // ,

        TOK_LPAREN,     // (
        TOK_RPAREN,     // )
        TOK_LBRACE,     // {
        TOK_RBRACE,     // }

        TOK_RARROW,     // ->

        TOK_IDENTIFIER,
        TOK_INTEGER,

        TOK_KW_RETURN,

        TOK_COUNT,
    };

    struct Token
    {
        Token_Kind kind = TOK_INVALID;
        File_Pos file_pos = {};
        Atom atom = {};
    };

    const char* token_kind_string(Token_Kind token_kind);
    void print_token(const Token& token);
}
