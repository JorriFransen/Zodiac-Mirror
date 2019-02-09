#pragma once

#include "zodiac.h"
#include "ast.h"
#include "token.h"

namespace Zodiac
{
    struct Parse_Error
    {
        File_Pos file_pos = {};
        const char* message = nullptr;
    };

    struct Parse_Result
    {
        const char* module_name = nullptr;
        BUF(Parse_Error) errors = nullptr;
        AST_Module* ast_module = nullptr;
    };

    struct  Parser
    {
        Context* context = nullptr;
        BUF(Token) tokens = nullptr;
        uint64_t ti = 0;
        Parse_Result result = {};
    };

    void parser_init(Parser* parser, Context* context);

    Parse_Result parse_module(Parser* parser, BUF(Token) tokens, const char* module_name);

    static AST_Identifier* parse_identifier(Parser* parser);

    static AST_Declaration* parse_declaration(Parser* parser, AST_Scope* scope);
    static AST_Declaration* parse_constant_declaration(Parser* parser, AST_Identifier* identifier,
                                                       AST_Type_Spec* type_spec, AST_Scope* scope);
    static AST_Declaration* parse_mutable_declaration(Parser* parser, AST_Identifier* identifier,
                                                      AST_Type_Spec* type_spec);

    static AST_Declaration* parse_function_prototype_argument(Parser* parser);

    static AST_Statement* parse_statement(Parser* parser, AST_Scope* scope);
    static AST_Statement* parse_block_statement(Parser* parser, AST_Scope* scope);
    static AST_Statement* parse_return_statement(Parser* parser);

    static AST_Expression* parse_expression(Parser* parser);
    static AST_Expression* parse_ternary_expression(Parser* parser);
    static AST_Expression* parse_or_or_expression(Parser* parser);
    static AST_Expression* parse_and_and_expression(Parser* parser);
    static AST_Expression* parse_or_expression(Parser* parser);
    static AST_Expression* parse_cmp_expression(Parser* parser);
    static AST_Expression* parse_add_expression(Parser* parser);
    static AST_Expression* parse_mul_expression(Parser* parser);
    static AST_Expression* parse_unary_expression(Parser* parser);
    static AST_Expression* parse_base_expression(Parser* parser);
    static AST_Expression* parse_literal_expression(Parser* parser);

    static AST_Type_Spec* parse_type_spec(Parser* parser);

    static bool is_add_op(Parser* parser);
    static bool is_mul_op(Parser* parser);
    static bool is_unary_op(Parser* parser);
    static AST_Binop_Kind parse_add_op(Parser* parser);
    static AST_Binop_Kind parse_mul_op(Parser* parser);
    static AST_Unop_Kind parse_unary_op(Parser* parser);

    static Token current_token(Parser* parser);
    static void consume_token(Parser* parser);
    static bool expect_token(Parser* parser, Token_Kind token_kind);
    static bool match_token(Parser* parser, Token_Kind token_kind);
    static bool is_token(Parser* parser, Token_Kind token_kind);

    void parser_report_error(Parser* parser, File_Pos file_pos, const char* format, ...);
    void parser_report_errors(Parser* parser);
}