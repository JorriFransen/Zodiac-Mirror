#include "parser.h"

namespace Zodiac
{
    void parser_init(Parser* parser, Context* context)
    {
        assert(parser);
        assert(context);

        parser->context = context;
    }

    Parse_Result parse_module(Parser* parser, BUF(Token) tokens, const char* module_name)
    {
        assert(parser);
        assert(tokens);
        assert(module_name);

        parser->result.module_name = module_name;
        auto ast_module = ast_module_new(parser->context, module_name);
        parser->result.ast_module = ast_module;
        parser->tokens = tokens;
        parser->ti = 0;

        while (parser->ti < BUF_LENGTH(parser->tokens) &&
               BUF_LENGTH(parser->result.errors) == 0)
        {
            AST_Declaration* global_decl = parse_declaration(parser);
            assert(global_decl);
            BUF_PUSH(ast_module->global_declarations, global_decl);
        }

        return parser->result;
    }

    static AST_Identifier* parse_identifier(Parser* parser)
    {
        assert(parser);

        Token id_token = current_token(parser);

        expect_token(parser, TOK_IDENTIFIER);

        return ast_identifier_new(parser->context, id_token.atom, id_token.file_pos);
    }

    static AST_Declaration* parse_declaration(Parser* parser)
    {
        assert(parser);

        AST_Declaration* result = nullptr;

        AST_Identifier* identifier = parse_identifier(parser);
        AST_Type_Spec* type_spec = nullptr;
        expect_token(parser, TOK_COLON);

        if (!is_token(parser, TOK_COLON) &&
            !is_token(parser, TOK_EQ))
        {
            type_spec = parse_type_spec(parser);
        }

        if (match_token(parser, TOK_COLON))
        {
            result = parse_constant_declaration(parser, identifier, type_spec);
        }
        else
        {
            result = parse_mutable_declaration(parser, identifier, type_spec);
        }

        assert(result);
        return result;
    }

    static AST_Declaration* parse_constant_declaration(Parser* parser, AST_Identifier* identifier,
                                                       AST_Type_Spec* type_spec)
    {
        assert(parser);
        assert(identifier);

        auto fp = current_token(parser).file_pos;
        expect_token(parser, TOK_COLON);

        assert(!type_spec);
        AST_Function_Proto* proto = parse_function_prototype(parser, identifier);
        AST_Statement* body_block = nullptr;

        if (is_token(parser, TOK_LBRACE))
        {
            body_block = parse_block_statement(parser);
        }
        else
        {
            expect_token(parser, TOK_SEMICOLON);
        }

        AST_Declaration* result = ast_function_declaration_new(parser->context, fp, proto, body_block);
        return result;
    }

    static AST_Declaration* parse_mutable_declaration(Parser* parser, AST_Identifier* identifier,
                                                      AST_Type_Spec* type_spec)
    {
        assert(parser);
        assert(identifier);

        assert(false);
    }

    static AST_Function_Proto* parse_function_prototype(Parser* parser, AST_Identifier* identifier)
    {
        assert(parser);
        assert(identifier);

        expect_token(parser, TOK_LPAREN);

        BUF(AST_Declaration*) args = nullptr;

        while (!match_token(parser, TOK_RPAREN))
        {
            if (args)
            {
                expect_token(parser, TOK_COMMA);
            }
            AST_Declaration* arg_decl = parse_function_prototype_argument(parser);
            BUF_PUSH(args, arg_decl);
        }

        AST_Type_Spec* return_type_spec = nullptr;
        if (match_token(parser, TOK_RARROW))
        {
            return_type_spec = parse_type_spec(parser);
        }

        return ast_function_prototype_new(parser->context, identifier->file_pos, args, return_type_spec);
    }

    static AST_Declaration* parse_function_prototype_argument(Parser* parser)
    {
        assert(parser);

        assert(false);
    }

    static AST_Statement* parse_block_statement(Parser* parser)
    {
        assert(parser);

        expect_token(parser, TOK_LBRACE);

        assert(false);
    }

    static AST_Type_Spec* parse_type_spec(Parser* parser)
    {
        assert(parser);

        assert(false);
    }

    static Token current_token(Parser* parser)
    {
        assert(parser);
        assert(parser->ti < BUF_LENGTH(parser->tokens));

        return parser->tokens[parser->ti];
    }

    static void consume_token(Parser* parser)
    {
        assert(parser);
        parser->ti++;
    }

    static bool expect_token(Parser* parser, Token_Kind token_kind)
    {
        assert(parser);

        auto tt = current_token(parser);
        if (is_token(parser, token_kind))
        {
            consume_token(parser);
            return true;
        }

        parser_report_error(parser, tt.file_pos, "Expected token of type: %s, got token: %s: '%s'\n",
                            token_kind_string(token_kind), token_kind_string(tt.kind),
                            tt.atom.data);
        return false;
    }

    static bool match_token(Parser* parser, Token_Kind token_kind)
    {
        assert(parser);

        if (is_token(parser, token_kind))
        {
            consume_token(parser);
            return true;
        }

        return false;
    }

    static bool is_token(Parser* parser, Token_Kind token_kind)
    {
        assert(parser);

        auto ct = current_token(parser);
        return ct.kind == token_kind;
    }

    void parser_report_error(Parser* parser, File_Pos file_pos, const char* format, ...)
    {
        assert(parser);
        assert(format);

        static const uint64_t message_buf_size = 2048;
        static char message_buf[message_buf_size];

        va_list va_args;
        va_start(va_args, format);
        vsprintf(message_buf, format, va_args);
        va_end(va_args);

        auto message_size = strlen(message_buf);

        Parse_Error result = {};
        result.file_pos = file_pos;
        message_size = strlen(message_buf);
        result.message = (char*)mem_alloc(message_size + 1);
        memcpy((void*)result.message, (void*)message_buf, message_size);
        *(char*)(result.message + message_size) = '\0';
        BUF_PUSH(parser->result.errors, result);

    }

    void parser_report_errors(Parser* parser)
    {
        assert(parser);

        assert(false);
    }
}
