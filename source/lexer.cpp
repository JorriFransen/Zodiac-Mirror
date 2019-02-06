#include "lexer.h"

#include <stdarg.h>

namespace Zodiac
{

    void init_lexer(Lexer* lexer, Context* context)
    {
        assert(lexer);
        assert(context);

        lexer->context = context;
    }

    Lex_Result lex_file(Lexer* lexer, const char* file_data, const char* file_name)
    {
        assert(lexer);
        assert(file_data);
        assert(file_name);
        fprintf(stderr, "lexing file: %s\n", file_name);

        lexer->file_data = file_data;
        lexer->file_name = file_name;
        lexer->file_size = strlen(file_data);
        lexer->current_file_pos = {};
        lexer->current_file_pos.file_name = file_name;
        lexer->current_file_pos.line = 1;
        lexer->current_file_pos.line_relative_char_pos = 0;
        lexer->current_file_pos.char_pos = 0;

        lexer->result = {};

        while (lexer->current_file_pos.char_pos < lexer->file_size &&
               BUF_LENGTH(lexer->result.errors) == 0)
        {
            lexer_consume_whitespace(lexer);
            lex_token(lexer);
        }

        return lexer->result;
    }

#define _SINGLE_CHAR_TOKEN_CASE(c, kind)                                  \
    case c: {                                                             \
    lexer_push_token(lexer, kind, lexer->current_file_pos, 1);            \
    lexer_consume_character(lexer);                                       \
    break; }                                                              \


    void lex_token(Lexer* lexer)
    {
        assert(lexer);

        switch (current_char(lexer))
        {
            case '\n':
                lexer_consume_character(lexer);
                break;


            _SINGLE_CHAR_TOKEN_CASE('+', TOK_PLUS);
            _SINGLE_CHAR_TOKEN_CASE(':', TOK_COLON);
            _SINGLE_CHAR_TOKEN_CASE(';', TOK_SEMICOLON);
            _SINGLE_CHAR_TOKEN_CASE('(', TOK_LPAREN);
            _SINGLE_CHAR_TOKEN_CASE(')', TOK_RPAREN);
            _SINGLE_CHAR_TOKEN_CASE('{', TOK_LBRACE);
            _SINGLE_CHAR_TOKEN_CASE('}', TOK_RBRACE);

            case '-':
            {
                File_Pos file_pos = lexer->current_file_pos;
                uint64_t token_length = 1;
                Token_Kind token_kind = TOK_MINUS;
                lexer_consume_character(lexer);
                if (current_char(lexer) == '>')
                {
                    token_length++;
                    token_kind = TOK_RARROW;
                    lexer_consume_character(lexer);
                }

                lexer_push_token(lexer, token_kind, file_pos, token_length);
                break;
            }

            case '/':
            {
                File_Pos file_pos = lexer->current_file_pos;
                lexer_consume_character(lexer);
                if (current_char(lexer) == '/')
                {
                    lexer_consume_line(lexer);
                }
                else
                {
                    lexer_push_token(lexer, TOK_DIV, file_pos, 1);
                }
                break;
            }

            case EOF: break;

            default:
            {
                if (char_is_first_ident(current_char(lexer)))
                {
                    lex_identifier(lexer);
                }
                else if (char_is_num(current_char(lexer)))
                {
                    lex_integer(lexer);
                }
                else
                {
                    auto cc = current_char(lexer);
                    lexer_report_error(lexer, "Unexpected character: '%c' (%d)", cc, cc);
                }

                break;
            }
        }
    }

#undef _SINGLE_CHAR_TOKEN_CASE

    static void lex_identifier(Lexer* lexer)
    {
        assert(lexer);

        auto cc = current_char(lexer);
        assert(char_is_first_ident(current_char(lexer)));

        File_Pos ident_file_pos = lexer->current_file_pos;
        uint64_t ident_length = 0;

        while (char_is_ident(current_char(lexer)))
        {
            lexer_consume_character(lexer);
            ident_length++;
        }

        lexer_push_token(lexer, TOK_IDENTIFIER, ident_file_pos, ident_length);
    }

    static void lex_integer(Lexer* lexer)
    {
        assert(lexer);
        assert(char_is_num(current_char(lexer)));

        File_Pos file_pos = lexer->current_file_pos;
        uint64_t integer_length = 0;

        while (char_is_num(current_char(lexer)))
        {
            lexer_consume_character(lexer);
            integer_length++;
        }

        lexer_push_token(lexer, TOK_INTEGER, file_pos, integer_length);
    }

    static void lexer_consume_character(Lexer* lexer)
    {
        assert(lexer);

        auto cur_char = current_char(lexer);
        auto fp = &lexer->current_file_pos;

        fp->char_pos++;

        if (cur_char == '\n')
        {
            fp->line++;
            fp->line_relative_char_pos = 0;
        }
        else
        {
            fp->line_relative_char_pos++;
        }
    }

    static void lexer_consume_line(Lexer* lexer)
    {
        assert(lexer);

        while (current_char(lexer) != '\n')
        {
            lexer_consume_character(lexer);
        }

        lexer_consume_character(lexer);
    }

    static void lexer_consume_whitespace(Lexer* lexer)
    {
        assert(lexer);

        while (char_is_whitespace(current_char(lexer)))
        {
            lexer_consume_character(lexer);
        }
    }

    static char current_char(Lexer* lexer)
    {
        assert(lexer);

        auto cur_char_pos = lexer->current_file_pos.char_pos;
        if (cur_char_pos < lexer->file_size)
        {
            return lexer->file_data[lexer->current_file_pos.char_pos];
        }

        return EOF;
    }

    static void lexer_push_token(Lexer* lexer, Token_Kind token_kind, File_Pos file_pos, uint64_t token_length)
    {
        assert(lexer);
        assert(token_kind > TOK_INVALID && token_kind < TOK_COUNT);
        assert(token_length > 0);

        Token result = {};
        result.kind = token_kind;
        result.file_pos = file_pos;
        result.string = &lexer->file_data[file_pos.char_pos];
        result.string_len = token_length;

        BUF_PUSH(lexer->result.tokens, result);
    }

    static bool char_is_first_ident(char c)
    {
        return (c == '_' || char_is_alpha(c));
    }

    static bool char_is_ident(char c)
    {
        return (c == '_' || char_is_alpha_num(c));
    }

    static bool char_is_alpha(char c)
    {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    }

    static bool char_is_num(char c)
    {
        return (c >= '0' && c <= '9');
    }
    static bool char_is_alpha_num(char c)
    {
        return (char_is_alpha(c) || char_is_num(c));
    }

    static bool char_is_whitespace(char c)
    {
        return (c == ' ' || c == '\t' || c == '\n');
    }

    void lexer_report_error(Lexer* lexer, const char* format, ...)
    {
        assert(lexer);
        assert(format);

        static const uint64_t message_buf_size = 2048;
        static char message_buf[message_buf_size];

        va_list va_args;
        va_start(va_args, format);
        vsprintf(message_buf, format, va_args);
        va_end(va_args);

        auto message_size = strlen(message_buf);

        Lex_Error result = {};
        result.file_pos = lexer->current_file_pos;
        message_size = strlen(message_buf);
        result.message = (char*)mem_alloc(message_size + 1);
        memcpy((void*)result.message, (void*)message_buf, message_size);
        *(char*)(result.message + message_size) = '\0';
        BUF_PUSH(lexer->result.errors, result);
    }

    void lexer_report_errors(Lexer* lexer)
    {
        assert(lexer);

        for (uint64_t i = 0; i < BUF_LENGTH(lexer->result.errors); i++)
        {
            Lex_Error error = lexer->result.errors[i];
            fprintf(stderr, "Error: %s:%lu: %s\n", error.file_pos.file_name, error.file_pos.line, error.message);
        }
    }
}
