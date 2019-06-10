#include "lexer.h"

#include <stdarg.h>
#include <inttypes.h>

namespace Zodiac
{

    void init_lexer(Lexer* lexer, Context* context)
    {
        assert(lexer);
        assert(context);

        lexer->context = context;
    }

    void free_lex_result(Lex_Result* lex_result)
    {
        assert(lex_result);
        if (lex_result->errors)
            BUF_FREE(lex_result->errors);

        if (lex_result->tokens)
            BUF_FREE(lex_result->tokens);
    }

    Lex_Result lex_file(Lexer* lexer, const char* file_data, const char* file_name)
    {
        assert(lexer);
        assert(file_data);
        assert(file_name);

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


#define _DOUBLE_CHAR_TOKEN_CASE(c1, k1, c2, k2)\
    case c1: { \
        File_Pos file_pos = lexer->current_file_pos; \
        uint64_t token_length = 1; \
        Token_Kind token_kind = k1; \
        lexer_consume_character(lexer); \
        if (current_char(lexer) == c2) { \
            token_length++; \
            token_kind = k2; \
            lexer_consume_character(lexer); \
        } \
        lexer_push_token(lexer, token_kind, file_pos, token_length); \
        break; }


    void lex_token(Lexer* lexer)
    {
        assert(lexer);

        switch (current_char(lexer))
        {
            case '\n':
                lexer_consume_character(lexer);
                break;

            _SINGLE_CHAR_TOKEN_CASE('+', TOK_PLUS);
            _SINGLE_CHAR_TOKEN_CASE('*', TOK_MUL);
            _SINGLE_CHAR_TOKEN_CASE('%', TOK_PERCENT);
            _SINGLE_CHAR_TOKEN_CASE(':', TOK_COLON);
            _SINGLE_CHAR_TOKEN_CASE(';', TOK_SEMICOLON);
            _SINGLE_CHAR_TOKEN_CASE(',', TOK_COMMA);
            _SINGLE_CHAR_TOKEN_CASE('(', TOK_LPAREN);
            _SINGLE_CHAR_TOKEN_CASE(')', TOK_RPAREN);
            _SINGLE_CHAR_TOKEN_CASE('{', TOK_LBRACE);
            _SINGLE_CHAR_TOKEN_CASE('}', TOK_RBRACE);
            _SINGLE_CHAR_TOKEN_CASE('[', TOK_LBRACK);
            _SINGLE_CHAR_TOKEN_CASE(']', TOK_RBRACK);
            _SINGLE_CHAR_TOKEN_CASE('#', TOK_POUND);

            _DOUBLE_CHAR_TOKEN_CASE('<', TOK_LT, '=', TOK_LTEQ);
            _DOUBLE_CHAR_TOKEN_CASE('>', TOK_GT, '=', TOK_GTEQ);
            _DOUBLE_CHAR_TOKEN_CASE('=', TOK_EQ, '=', TOK_EQEQ);
            _DOUBLE_CHAR_TOKEN_CASE('-', TOK_MINUS, '>', TOK_RARROW);
            _DOUBLE_CHAR_TOKEN_CASE('!', TOK_BANG, '=', TOK_NEQ);
            _DOUBLE_CHAR_TOKEN_CASE('&', TOK_AND, '&', TOK_AND_AND);
            _DOUBLE_CHAR_TOKEN_CASE('|', TOK_OR, '|', TOK_OR_OR);

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

            case '"':
            {
                lexer_consume_character(lexer);
                File_Pos file_pos = lexer->current_file_pos;
                uint64_t length = 0;
                while (current_char(lexer) != '"')
                {
                    length++;
                    lexer_consume_character(lexer);
                }
                lexer_consume_character(lexer);

                lexer_push_token(lexer, TOK_STRING_LIT, file_pos, length);
                break;
            }

            case '\'':
            {
                lexer_consume_character(lexer);
                File_Pos file_pos = lexer->current_file_pos;
                uint64_t length = 0;
                while (current_char(lexer) != '\'')
                {
                    length++;
                    lexer_consume_character(lexer);
                }
                lexer_consume_character(lexer);
                lexer_push_token(lexer, TOK_CHAR_LIT, file_pos, length);
                break;
            }

            case '.':
            {
                File_Pos file_pos = lexer->current_file_pos;
                lexer_consume_character(lexer);
                if (current_char(lexer) == '.')
                {
                    lexer_consume_character(lexer);
                    if (current_char(lexer) == '.')
                    {
                        lexer_consume_character(lexer);
                        lexer_push_token(lexer, TOK_ELLIPSIS, file_pos, 3);
                    }
                    else
                    {
                        lexer_push_token(lexer, TOK_DOUBLE_DOT, file_pos, 2);
                    }
                }
                else
                {
                    lexer_push_token(lexer, TOK_DOT, file_pos, 1);
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
                    if (current_char(lexer) == '0' &&
                        next_char(lexer) == 'x')
                    {
                        lex_hex_number(lexer);
                    }
                    else
                    {
                        lex_integer_or_float(lexer);
                    }
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

    static void lex_integer_or_float(Lexer* lexer)
    {
        assert(lexer);
        assert(char_is_num(current_char(lexer)));

        File_Pos file_pos = lexer->current_file_pos;
        uint64_t integer_length = 0;

        bool found_dot = false;
        bool found_x = false;

        while (char_is_num(current_char(lexer)))
        {
            lexer_consume_character(lexer);
            integer_length++;

            if ((current_char(lexer) == '.') && !(next_char(lexer) == '.'))
            {
                assert(!found_dot);
                found_dot = true;
                lexer_consume_character(lexer);
                integer_length++;
            }
        }

        Token_Kind kind = found_dot ? TOK_FLOAT : TOK_INTEGER;
        lexer_push_token(lexer, kind, file_pos, integer_length);
    }

    static void lex_hex_number(Lexer* lexer)
    {
        assert(lexer);

        assert(current_char(lexer) == '0');
        lexer_consume_character(lexer);
        assert(current_char(lexer) == 'x');
        lexer_consume_character(lexer);

        File_Pos file_pos = lexer->current_file_pos;
        uint64_t hex_num_length = 0;

        while (char_is_alpha_num(current_char(lexer)))
        {
            lexer_consume_character(lexer);
            hex_num_length++;
        }

        lexer_push_token(lexer, TOK_HEX_NUM, file_pos, hex_num_length);

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

    static char next_char(Lexer* lexer)
    {
        assert(lexer);

        auto index = lexer->current_file_pos.char_pos + 1;
        if (index < lexer->file_size)
        {
            return lexer->file_data[index];
        }

        return EOF;
    }

    static Atom lexer_replace_escape_chars(Lexer* lexer, File_Pos file_pos, uint64_t token_length)
    {
        assert(lexer);
        assert(token_length);

        const char* cursor = &lexer->file_data[file_pos.char_pos];

        // TODO: temporary memory
        BUF(char) new_literal = nullptr;

        for (uint64_t i = 0; i < token_length; i++)
        {
            if (*cursor == '\\')
            {
                cursor++;
                switch (*cursor)
                {
                    case 'n':
                        BUF_PUSH(new_literal, '\n');
                        break;

                    case 't':
                        BUF_PUSH(new_literal, '\t');
                        break;

                    default: assert(false);
                }
                token_length--;
            }
            else
            {
                BUF_PUSH(new_literal, *cursor);
            }

            cursor++;
        }

        BUF_PUSH(new_literal, '\0');

        Atom result = atom_get(lexer->context->atom_table, new_literal);
        BUF_FREE(new_literal);

        return result;
    }

    static Token lexer_maybe_convert_token_to_keyword(Lexer* lexer, Token token)
    {
        assert(lexer);

        for (uint64_t i = 0; i < BUF_LENGTH(lexer->context->keywords); i++)
        {
            Registered_Keyword rkw = lexer->context->keywords[i];
            if (rkw.atom == token.atom)
            {
                token.kind = rkw.token_kind;
                return token;
            }
        }

        return token;
    }

    static void lexer_push_token(Lexer* lexer, Token_Kind token_kind, File_Pos file_pos,
                                 uint64_t token_length)
    {
        assert(lexer);
        assert(token_kind > TOK_INVALID && token_kind < TOK_COUNT);
        assert(token_length > 0);

        auto context = lexer->context;
        Atom atom = {};

        if (token_kind == TOK_STRING_LIT ||
            token_kind == TOK_CHAR_LIT)
        {
            atom = lexer_replace_escape_chars(lexer, file_pos, token_length);
        }
        else
        {
            atom = atom_get(context->atom_table, &lexer->file_data[file_pos.char_pos],
                            token_length);
        }

        Token result = {};
        result.kind = token_kind;
        result.file_pos = file_pos;
        result.atom = atom;

        if (result.kind == TOK_IDENTIFIER)
        {
            result = lexer_maybe_convert_token_to_keyword(lexer, result);
        }

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
        return (c == ' ' || c == '\t' || c == '\n' || c == 13);
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
            fprintf(stderr, "Error: %s:%" PRIu64 ": %s\n", error.file_pos.file_name, error.file_pos.line, error.message);
        }
    }
}
