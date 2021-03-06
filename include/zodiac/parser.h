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

        bool allow_vararg_type_specs = false;
    };

    void parser_init(Parser* parser, Context* context);

    Parse_Result parse_module(Parser* parser, BUF(Token) tokens, const char* module_name,
                              const char* path);

    static AST_Identifier* parse_identifier(Parser* parser);
    static AST_Directive* parse_directive(Parser* parser);

    AST_Declaration* parse_declaration(Parser* parser, AST_Scope* scope, bool global,
                                       AST_Declaration_Location location = AST_DECL_LOC_INVALID);

    AST_Declaration* parse_declaration(Parser* parser, AST_Scope* scope, bool global,
                                       AST_Directive* directive,
                                       AST_Declaration_Location location = AST_DECL_LOC_INVALID);

    AST_Declaration* parse_declaration(Parser* parser, AST_Identifier* identifier,
                                       AST_Scope* scope, bool global, AST_Directive* directive,
                                       AST_Declaration_Location location = AST_DECL_LOC_INVALID);
    AST_Declaration* parse_list_declaration(Parser* parser, AST_Expression* list_expr,
                                            AST_Scope* scope);

    static AST_Declaration* parse_constant_declaration(Parser* parser, AST_Identifier* identifier,
                                                       AST_Type_Spec* type_spec, AST_Scope* scope,
                                                       AST_Declaration_Location location);
    static AST_Declaration* parse_mutable_declaration(Parser* parser, AST_Identifier* identifier,
                                                      AST_Type_Spec* type_spec,
                                                      AST_Declaration_Location location,
		AST_Scope* scope);
    static AST_Declaration* parse_link_declaration(Parser* parser, bool global, AST_Scope* scope,
                                                   AST_Directive* directive);
    static AST_Declaration* parse_static_if_declaration(Parser* parser, bool global,
                                                        AST_Scope* scope,
                                                        AST_Declaration_Location location);
    static AST_Declaration* parse_block_declaration(Parser* parser, bool global, AST_Scope* scope,
                                                    AST_Declaration_Location location);
    static AST_Declaration* parse_static_assert_declaration(Parser* parser, bool global, AST_Scope* scope);
    static AST_Declaration* parse_using_declaration(Parser* parser,
                                                    AST_Declaration_Location location,
                                                    AST_Scope* scope);

    static AST_Aggregate_Declaration* parse_aggregate(Parser* parser, AST_Scope* scope,
                                                      bool is_enum = false);

    AST_Statement* parse_statement(Parser* parser, AST_Scope* scope);
    static AST_Statement* parse_block_statement(Parser* parser, AST_Scope* scope,
                                                uint64_t opening_line);
    static AST_Statement* parse_return_statement(Parser* parser, AST_Scope* scope);
    static AST_Statement* parse_if_statement(Parser* parser, AST_Scope* scope);
    static AST_Statement* parse_while_statement(Parser* parser, AST_Scope* scope);
    static AST_Statement* parse_for_statement(Parser* parser, AST_Scope* scope);
    static AST_Statement* parse_switch_statement(Parser* parser, AST_Scope* scope);

    static AST_Expression* parse_list_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_ternary_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_or_or_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_and_and_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_or_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_and_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_cmp_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_bitshift_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_add_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_mul_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_unary_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_base_expression(Parser* parser, AST_Scope* scope,
                                                 AST_Expression* base = nullptr);
    static AST_Expression* parse_literal_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_compound_literal_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_array_length_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_sizeof_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_get_type_info_expression(Parser* parser, AST_Scope* scope);
    static AST_Expression* parse_get_type_info_base_ptr_expression(Parser* parser,
                                                                   AST_Scope* scope);
    static AST_Expression* parse_call_expression(Parser* parser, AST_Expression* ident_expression,
		AST_Scope* scope);
    static AST_Expression* parse_call_expression(Parser* parser, AST_Identifier* identifier,
		AST_Scope* scope);
    static AST_Expression* parse_directive_expression(Parser* parser, AST_Scope* scope);

    static AST_Type_Spec* parse_type_spec(Parser* parser, AST_Scope* scope);
    static AST_Type_Spec* parse_return_type_spec(Parser* parser, AST_Scope* scope);
	static AST_Type_Spec* parse_function_type_spec(Parser* parser, AST_Scope* scope);

    static AST_Overload_Operator_Kind parse_overload_operator(Parser* parser);

    static bool is_add_op(Parser* parser);
    static bool is_mul_op(Parser* parser);
    bool is_cmp_op(Parser* parser);
    static bool is_unary_op(Parser* parser);
    static bool is_bitshift_op(Parser* parser);
    static AST_Binop_Kind parse_add_op(Parser* parser);
    static AST_Binop_Kind parse_mul_op(Parser* parser);
    static AST_Binop_Kind parse_cmp_op(Parser* parser);
    static AST_Unop_Kind parse_unary_op(Parser* parser);
    static AST_Binop_Kind parse_bitshift_op(Parser* parser);

    static bool is_binary_assign_op(Parser* parser, uint64_t offset = 0);
    static AST_Binop_Kind parse_binary_assign_op(Parser* parser);

    static Token peek_token(Parser* parser, uint64_t offset);
    static Token current_token(Parser* parser);
    static void consume_token(Parser* parser);
    static bool expect_token(Parser* parser, Token_Kind token_kind);
    static bool match_token(Parser* parser, Token_Kind token_kind);
    static bool is_token(Parser* parser, Token_Kind token_kind);

    void parser_report_error(Parser* parser, File_Pos file_pos, const char* format, ...);
    void parser_report_errors(Parser* parser);
}
