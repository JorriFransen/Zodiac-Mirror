#pragma once

#include "zodiac.h"

#include "file_pos.h"
#include "token.h"

namespace Zodiac
{
    struct Lex_Error
    {
        File_Pos file_pos;
        const char* message = nullptr;
    };

    struct Lex_Result
    {
        BUF(Lex_Error) errors;
        BUF(Token) tokens;
    };

    struct Lexer
    {
        Context* context = nullptr;
        const char* file_name = nullptr;
        const char* file_data = nullptr;
        uint64_t file_size = 0;

        Lex_Result result = {};
        File_Pos current_file_pos = {};
    };

    void init_lexer(Lexer* lexer, Context* context);

    Lex_Result lex_file(Lexer* lexer, const char* file_data, const char* file_name);
    void lex_token(Lexer* lexer);

    static void lex_identifier(Lexer* lexer);
    static void lex_integer(Lexer* lexer);

    static void lexer_consume_character(Lexer* lexer);
    static void lexer_consume_line(Lexer* lexer);
    static void lexer_consume_whitespace(Lexer* lexer);
    static char current_char(Lexer* lexer);

    static Token lexer_maybe_convert_token_to_keyword(Lexer* lexer, Token token);

    static void lexer_push_token(Lexer* lexer, Token_Kind token_kind, File_Pos file_pos,
                                 uint64_t token_length);

    static bool char_is_first_ident(char c);
    static bool char_is_ident(char c);
    static bool char_is_alpha(char c);
    static bool char_is_num(char c);
    static bool char_is_alpha_num(char c);
    static bool char_is_whitespace(char c);

    void lexer_report_error(Lexer* lexer, const char* format, ...);
    void lexer_report_errors(Lexer* lexer);
}
