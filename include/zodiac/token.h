#pragma once

#include "atom.h"
#include "file_pos.h"

namespace Zodiac
{

#define DEFINE_TOKEN_KINDS      \
    DEFINE_TOKEN(TOK_INVALID),  \
    DEFINE_TOKEN(TOK_PLUS),       /* + */ \
    DEFINE_TOKEN(TOK_MINUS),      /* - */ \
    DEFINE_TOKEN(TOK_MUL),        /* * */ \
    DEFINE_TOKEN(TOK_DIV),        /* / */ \
    DEFINE_TOKEN(TOK_EQ),         /* = */ \
    DEFINE_TOKEN(TOK_EQEQ),       /* == */ \
    DEFINE_TOKEN(TOK_LT),         /* < */ \
    DEFINE_TOKEN(TOK_LTEQ),       /* <= */ \
    DEFINE_TOKEN(TOK_GT),         /* > */ \
    DEFINE_TOKEN(TOK_GTEQ),       /* >= */ \
    DEFINE_TOKEN(TOK_POUND),      /* # */ \
    DEFINE_TOKEN(TOK_COLON),      /* : */ \
    DEFINE_TOKEN(TOK_SEMICOLON),  /* ; */ \
    DEFINE_TOKEN(TOK_COMMA),      /* , */ \
    DEFINE_TOKEN(TOK_LPAREN),     /* ( */ \
    DEFINE_TOKEN(TOK_RPAREN),     /* ) */ \
    DEFINE_TOKEN(TOK_LBRACE),     /* { */ \
    DEFINE_TOKEN(TOK_RBRACE),     /* } */ \
    DEFINE_TOKEN(TOK_LBRACK),     /* [ */ \
    DEFINE_TOKEN(TOK_RBRACK),     /* ] */ \
    DEFINE_TOKEN(TOK_RARROW),     /* -> */ \
    DEFINE_TOKEN(TOK_DOT),        /* . */ \
    DEFINE_TOKEN(TOK_ELLIPSIS),   /* ... */ \
    DEFINE_TOKEN(TOK_IDENTIFIER), \
    DEFINE_TOKEN(TOK_INTEGER), \
    DEFINE_TOKEN(TOK_FLOAT), \
    DEFINE_TOKEN(TOK_CHAR_LIT), \
    DEFINE_TOKEN(TOK_STRING_LIT), \
    DEFINE_TOKEN(TOK_KW_TRUE), \
    DEFINE_TOKEN(TOK_KW_FALSE), \
    DEFINE_TOKEN(TOK_KW_RETURN), \
    DEFINE_TOKEN(TOK_KW_STATIC_IF), \
    DEFINE_TOKEN(TOK_KW_IF), \
    DEFINE_TOKEN(TOK_KW_ELSE), \
    DEFINE_TOKEN(TOK_KW_FOREIGN), \
    DEFINE_TOKEN(TOK_KW_DYN_LINK), \
    DEFINE_TOKEN(TOK_KW_WHILE), \
    DEFINE_TOKEN(TOK_KW_FOR), \
    DEFINE_TOKEN(TOK_KW_ARRAY_LENGTH), \
    DEFINE_TOKEN(TOK_KW_STATIC_ASSERT), \
    DEFINE_TOKEN(TOK_KW_IMPORT), \
    DEFINE_TOKEN(TOK_KW_STRUCT), \
    DEFINE_TOKEN(TOK_KW_ENUM), \
    DEFINE_TOKEN(TOK_COUNT), \

    enum Token_Kind
    {
#define DEFINE_TOKEN(x) x
        DEFINE_TOKEN_KINDS
#undef DEFINE_TOKEN
    };

    static const char* _token_kind_strings[] =
    {
#define DEFINE_TOKEN(x) #x
        DEFINE_TOKEN_KINDS
#undef DEFINE_TOKEN
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
