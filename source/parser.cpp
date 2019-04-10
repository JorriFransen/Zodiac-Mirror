#include "parser.h"

#include <stdarg.h>
#include <inttypes.h>

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
            AST_Directive* directive = nullptr;
            if (match_token(parser, TOK_POUND))
            {
                directive = parse_directive(parser);
            }
            AST_Declaration* global_decl = parse_declaration(parser, ast_module->module_scope,
                                                             true, directive);
            if (!global_decl)
                break;

            BUF_PUSH(ast_module->global_declarations, global_decl);
        }

        return parser->result;
    }

    static AST_Identifier* parse_identifier(Parser* parser)
    {
        assert(parser);

        Token id_token = current_token(parser);

        if (match_token(parser, TOK_IDENTIFIER))
        {
            return ast_identifier_new(parser->context, id_token.atom, id_token.file_pos);
        }

        parser_report_error(parser, id_token.file_pos,
                            "Expected identifier token, got: %s",
                            id_token.atom.data);

        return nullptr;
    }

    static AST_Directive* parse_directive(Parser* parser)
    {
        assert(parser);

        auto ft = current_token(parser);

        AST_Directive_Kind kind = AST_DIREC_INVALID;

        if (match_token(parser, TOK_KW_FOREIGN))
        {
            kind = AST_DIREC_FOREIGN;
        }
        else if (match_token(parser, TOK_KW_DYN_LINK))
        {
            kind = AST_DIREC_DYN_LINK;
        }
        else assert(false);

        return ast_directive_new(parser->context, kind, ft.file_pos);
    }

    static AST_Declaration* parse_declaration(Parser* parser, AST_Scope* scope, bool global,
                                              AST_Directive* directive,
                                              AST_Declaration_Location location/*= AST_DECL_LOC_INVALID*/)
    {
        assert(parser);
        assert(scope);

        if (directive && directive->kind == AST_DIREC_DYN_LINK)
        {
            return parse_link_declaration(parser, global, scope, directive);
        }

        AST_Identifier* identifier = parse_identifier(parser);
        if (!identifier)
        {
            return nullptr;
        }
        return parse_declaration(parser, identifier, scope, global, directive, location);
    }

    static AST_Declaration* parse_declaration(Parser* parser, AST_Identifier* identifier, AST_Scope* scope,
                                              bool global, AST_Directive* directive,
                                              AST_Declaration_Location location/*= AST_DECL_LOC_INVALID*/)
    {
        assert(parser);
        assert(scope);
        assert(identifier);

        AST_Declaration* result = nullptr;

        AST_Type_Spec* type_spec = nullptr;
        expect_token(parser, TOK_COLON);

        if (!is_token(parser, TOK_COLON) &&
            !is_token(parser, TOK_EQ))
        {
            type_spec = parse_type_spec(parser);
        }

        if (is_token(parser, TOK_COLON))
        {
            result = parse_constant_declaration(parser, identifier, type_spec, scope);
        }
        else
        {
            auto loc = global ? AST_DECL_LOC_GLOBAL : AST_DECL_LOC_LOCAL;
            if (location != AST_DECL_LOC_INVALID)
            {
                loc = location;
            }
            result = parse_mutable_declaration(parser, identifier, type_spec, loc);
            if (!result)
            {
                return nullptr;
            }
        }

        assert(result);
        result->directive = directive;
        if (global)
        {
            assert(result->location == AST_DECL_LOC_INVALID);
            result->location = AST_DECL_LOC_GLOBAL;
        }
        return result;
    }

    static AST_Declaration* parse_constant_declaration(Parser* parser, AST_Identifier* identifier,
                                                       AST_Type_Spec* type_spec, AST_Scope* scope)
    {
        assert(parser);
        assert(identifier);
        assert(scope);

        auto fp = current_token(parser).file_pos;
        expect_token(parser, TOK_COLON);

        assert(!type_spec);

        BUF(AST_Declaration*) arg_decls = nullptr;
        expect_token(parser, TOK_LPAREN);
        while (!match_token(parser, TOK_RPAREN))
        {
            if (arg_decls)
            {
                expect_token(parser, TOK_COMMA);
            }

            AST_Declaration* decl = parse_declaration(parser, scope, false, nullptr, AST_DECL_LOC_ARGUMENT);
            if (!decl)
            {
                return nullptr;
            }
            BUF_PUSH(arg_decls, decl);
        }

        AST_Type_Spec* return_type_spec = nullptr;
        if (match_token(parser, TOK_RARROW))
        {
            return_type_spec = parse_type_spec(parser);
        }

        AST_Statement* body_block = nullptr;
        AST_Scope* argument_scope = ast_scope_new(parser->context, parser->result.ast_module->module_scope);

        if (is_token(parser, TOK_LBRACE))
        {
            body_block = parse_block_statement(parser, argument_scope);
        }
        else
        {
            expect_token(parser, TOK_SEMICOLON);
        }

        AST_Declaration* result = ast_function_declaration_new(parser->context, fp, identifier,
                                                               arg_decls, return_type_spec, body_block,
                                                               argument_scope);
        return result;
    }

    static AST_Declaration* parse_mutable_declaration(Parser* parser, AST_Identifier* identifier,
                                                      AST_Type_Spec* type_spec, AST_Declaration_Location location)
    {
        assert(parser);
        assert(identifier);
        assert(location != AST_DECL_LOC_INVALID);

        AST_Expression* init_expression = nullptr;
        if (match_token(parser, TOK_EQ))
        {
            init_expression = parse_expression(parser);
            if (!init_expression)
            {
                return nullptr;
            }
        }

        if (!(type_spec || init_expression))
        {
            return nullptr;
        }

        return ast_mutable_declaration_new(parser->context, identifier->file_pos, identifier,
                                           type_spec, init_expression, location);
    }

    static AST_Declaration* parse_link_declaration(Parser* parser, bool global, AST_Scope* scope,
                                                   AST_Directive* directive)
    {
        assert(parser);
        assert(global);
        assert(scope);
        assert(directive);
        assert(directive->kind == AST_DIREC_DYN_LINK);

        auto sl_tok = current_token(parser);
        expect_token(parser, TOK_STRING_LIT);
        expect_token(parser, TOK_SEMICOLON);

        return ast_dyn_link_declaration_new(parser->context, directive->file_pos, sl_tok.atom,
            AST_DECL_LOC_GLOBAL);
    }

    static AST_Statement* parse_statement(Parser* parser, AST_Scope* scope)
    {
        assert(parser);
        assert(scope);
        assert(scope->parent);

        auto ft = current_token(parser);

        switch (ft.kind)
        {
            case TOK_KW_RETURN:
                return parse_return_statement(parser);
                break;

            case TOK_KW_IF:
                return parse_if_statement(parser, scope);
                break;

            case TOK_LBRACE:
                return parse_block_statement(parser, scope);
                break;

            default: break;
        }

        AST_Identifier* identifier = parse_identifier(parser);
        if (!identifier)
        {
            return nullptr;
        }

        if (is_token(parser, TOK_COLON))
        {
            AST_Declaration* decl = parse_declaration(parser, identifier, scope, false, nullptr);
            if (!decl)
            {
                return  nullptr;
            }
            expect_token(parser, TOK_SEMICOLON);
            return ast_declaration_statement_new(parser->context, ft.file_pos, decl);
        }
        else if (match_token(parser, TOK_EQ))
        {
            AST_Expression* assign_expression = parse_expression(parser);
            expect_token(parser, TOK_SEMICOLON);
            return ast_assign_statement_new(parser->context, ft.file_pos, identifier,
                                            assign_expression);
        }
        else if (is_token(parser, TOK_LPAREN))
        {
            AST_Expression* call_expression = parse_call_expression(parser, identifier);
            expect_token(parser, TOK_SEMICOLON);
            return ast_call_statement_new(parser->context, call_expression);
        }
        else assert(false);

        assert(false);
        return nullptr;
    }

    static AST_Statement* parse_block_statement(Parser* parser, AST_Scope* scope)
    {
        assert(parser);
        assert(scope);

        auto ft = current_token(parser);
        expect_token(parser, TOK_LBRACE);

        BUF(AST_Statement*) block_statements = nullptr;

        AST_Scope* block_scope = ast_scope_new(parser->context, scope);

        while (!match_token(parser, TOK_RBRACE))
        {
            AST_Statement* block_statement = parse_statement(parser, block_scope);
            if (!block_statement)
            {
                return nullptr;
            }
            BUF_PUSH(block_statements, block_statement);
        }

        return ast_block_statement_new(parser->context, ft.file_pos, block_statements, block_scope);
    }

    static AST_Statement* parse_return_statement(Parser* parser)
    {
        assert(parser);

        auto ret_tok = current_token(parser);
        expect_token(parser, TOK_KW_RETURN);

        AST_Expression* return_expr = nullptr;

        if (!is_token(parser, TOK_SEMICOLON))
        {
            return_expr = parse_expression(parser);
        }

        if (!expect_token(parser, TOK_SEMICOLON))
        {
            return nullptr;
        }

        return ast_return_statement_new(parser->context, ret_tok.file_pos, return_expr);
    }

    static AST_Statement* parse_if_statement(Parser* parser, AST_Scope* scope)
    {
        assert(parser);
        assert(scope);

        auto if_tok = current_token(parser);
        expect_token(parser, TOK_KW_IF);
        expect_token(parser, TOK_LPAREN);

        AST_Expression* cond_expr = parse_expression(parser);

        expect_token(parser, TOK_RPAREN);

        AST_Statement* then_stmt = parse_statement(parser, scope);
        if (!then_stmt)
        {
            return nullptr;
        }
        AST_Statement* else_stmt = nullptr;

        if (match_token(parser, TOK_KW_ELSE))
        {
            else_stmt = parse_statement(parser, scope);
        }

        return ast_if_statement_new(parser->context, if_tok.file_pos, cond_expr,
                                    then_stmt, else_stmt);
    }

    static AST_Expression* parse_expression(Parser* parser)
    {
        assert(parser);

        return parse_ternary_expression(parser);
    }

    static AST_Expression* parse_ternary_expression(Parser* parser)
    {
        assert(parser);

        return parse_or_or_expression(parser);
    }

    static AST_Expression* parse_or_or_expression(Parser* parser)
    {
        assert(parser);

        AST_Expression* lhs = parse_and_and_expression(parser);

        // while (match_token(parser, TOK_OR_OR))
        // {
        //     AST_Expression* rhs = parse_and_and_expression(parser);
        //     auto op = AST_BINOP_OR_OR;
        //     lhs = ast_new_binary_expression(parser->context, lhs, op, rhs);
        // }

        return lhs;
    }

    static AST_Expression* parse_and_and_expression(Parser* parser)
    {
        assert(parser);

        AST_Expression* lhs = parse_or_expression(parser);

        // while (match_token(parser, TOK_AND_AND))
        // {
        //     AST_Expression* rhs = parse_or_expression(parser);
        //     auto op = AST_BINOP_OR;
        //     lhs = ast_new_binary_expression(parser->context, lhs, op, rhs);
        // }

        return lhs;
    }

    static AST_Expression* parse_or_expression(Parser* parser)
    {
        assert(parser);

        AST_Expression* lhs = parse_cmp_expression(parser);

        // while (match_token(parser, TOK_OR))
        // {
        //     AST_Expression* rhs = parse_cmp_expression(parser);
        //     auto op = AST_BINOP_OR;
        //     lhs = ast_new_binary_expression(parser->context, lhs, op, rhs);
        // }

        return lhs;
    }

    static AST_Expression* parse_cmp_expression(Parser* parser)
    {
        assert(parser);

        AST_Expression* lhs = parse_add_expression(parser);

        while (is_cmp_op(parser))
        {
            auto op_tok = current_token(parser);
            auto op = parse_cmp_op(parser);
            AST_Expression* rhs = parse_add_expression(parser);
            lhs = ast_binary_expression_new(parser->context, op_tok.file_pos, lhs, op, rhs);
        }

        return lhs;
    }

    static AST_Expression* parse_add_expression(Parser* parser)
    {
        assert(parser);

        AST_Expression* lhs = parse_mul_expression(parser);

        while (is_add_op(parser))
        {
            auto op_tok = current_token(parser);
            auto op = parse_add_op(parser);
            AST_Expression* rhs = parse_mul_expression(parser);
            lhs = ast_binary_expression_new(parser->context, op_tok.file_pos, lhs, op, rhs);
        }

        return lhs;
    }

    static AST_Expression* parse_mul_expression(Parser* parser)
    {
        assert(parser);

        AST_Expression* lhs = parse_unary_expression(parser);

        while (is_mul_op(parser))
        {
            auto op_tok = current_token(parser);
            auto op = parse_mul_op(parser);
            AST_Expression* rhs = parse_mul_expression(parser);
            lhs = ast_binary_expression_new(parser->context, op_tok.file_pos, lhs, op, rhs);
        }

        return lhs;
    }

    static AST_Expression* parse_unary_expression(Parser* parser)
    {
        assert(parser);

        if (is_unary_op(parser))
        {
            auto op_tok = current_token(parser);
            auto op = parse_unary_op(parser);
            AST_Expression* operand_expr = parse_unary_expression(parser);
            return ast_unary_expression_new(parser->context, op_tok.file_pos, op, operand_expr);
        }
        else
        {
            return parse_base_expression(parser);
        }
    }

    static AST_Expression* parse_base_expression(Parser* parser)
    {
        assert(parser);

        if (is_token(parser, TOK_IDENTIFIER))
        {
            // Identifier, Call
            AST_Identifier* identifier = parse_identifier(parser);
            if (!identifier)
            {
                return nullptr;
            }

            if (is_token(parser, TOK_LPAREN))
            {
                return parse_call_expression(parser, identifier);
            }
            else
            {
                return ast_ident_expression_new(parser->context, identifier->file_pos, identifier);
            }
        }
        else
        {
            // Paren expr
            if (match_token(parser, TOK_LPAREN))
            {
                AST_Expression* result = parse_expression(parser);
                expect_token(parser, TOK_RPAREN);
                return result;
            }

            // Literal
            return parse_literal_expression(parser);
        }
    }

    static AST_Expression* parse_literal_expression(Parser* parser)
    {
        assert(parser);

        auto integer_token = current_token(parser);
        expect_token(parser, TOK_INTEGER);

        uint64_t value = atom_to_u64(integer_token.atom);
        return ast_literal_expression_new(parser->context, integer_token.file_pos, value);
    }

    static AST_Expression* parse_call_expression(Parser* parser, AST_Identifier* identifier)
    {
        assert(parser);
        assert(identifier);

        expect_token(parser, TOK_LPAREN);

        BUF(AST_Expression*) arg_exprs = nullptr;

        while (!match_token(parser, TOK_RPAREN))
        {
            if (arg_exprs)
            {
                expect_token(parser, TOK_COMMA);
            }

            AST_Expression* arg_expr = parse_expression(parser);
            if (!arg_expr)
            {
                return nullptr;
            }
            BUF_PUSH(arg_exprs, arg_expr);
        }

        return ast_call_expression_new(parser->context, identifier->file_pos, identifier, arg_exprs);
    }

    static AST_Type_Spec* parse_type_spec(Parser* parser)

    {
        assert(parser);

        auto ft = current_token(parser);

        if (match_token(parser, TOK_MUL))
        {
            AST_Type_Spec* base_type_spec = parse_type_spec(parser);
            return ast_type_spec_pointer_new(parser->context, ft.file_pos, base_type_spec);
        }
        else
        {
            AST_Identifier* typespec_ident = parse_identifier(parser);
            if (!typespec_ident)
            {
                return nullptr;
            }

            return ast_type_spec_identifier_new(parser->context, typespec_ident->file_pos, typespec_ident);
        }
    }

    static bool is_add_op(Parser* parser)
    {
        assert(parser);

        auto ct = current_token(parser);

        return ct.kind == TOK_PLUS || ct.kind == TOK_MINUS;
    }

    static bool is_mul_op(Parser* parser)
    {
        assert(parser);

        auto ct = current_token(parser);

        return ct.kind == TOK_MUL || ct.kind == TOK_DIV;
    }

    static bool is_cmp_op(Parser* parser)
    {
        assert(parser);

        auto ct = current_token(parser);

        return ct.kind == TOK_LT ||
               ct.kind == TOK_LTEQ;
    }

    static bool is_unary_op(Parser* parser)
    {
        assert(parser);

        auto ct = current_token(parser);

        return ct.kind == TOK_MINUS || ct.kind == TOK_MUL;
    }

    static AST_Binop_Kind parse_add_op(Parser* parser)
    {
        assert(parser);

        auto ct = current_token(parser);
        AST_Binop_Kind result;

        switch (ct.kind)
        {
            case TOK_PLUS:
                result = AST_BINOP_ADD;
                break;

            case TOK_MINUS:
                result = AST_BINOP_SUB;
                break;

            default:
                assert(false);
                break;
        }

        consume_token(parser);
        return result;
    }

    static AST_Binop_Kind parse_mul_op(Parser* parser)
    {
        assert(parser);

        auto ct = current_token(parser);
        AST_Binop_Kind result;

        switch (ct.kind)
        {
            case TOK_MUL:
                result = AST_BINOP_MUL;
                break;

            case TOK_DIV:
                result = AST_BINOP_DIV;
                break;

            default: assert(false);
        }

        consume_token(parser);
        return result;
    }

    static AST_Binop_Kind parse_cmp_op(Parser* parser)
    {
        assert(parser);

        auto ct = current_token(parser);
        AST_Binop_Kind result;

        switch (ct.kind)
        {
            case TOK_LT:
                result = AST_BINOP_LT;
                break;

            case TOK_LTEQ:
                result = AST_BINOP_LTEQ;
                break;

            default: assert(false);
        }

        consume_token(parser);
        return result;
    }

    static AST_Unop_Kind parse_unary_op(Parser* parser)
    {
        assert(parser);

        auto ct = current_token(parser);

        auto result = AST_UNOP_INVALID;

        switch (ct.kind)
        {
            case TOK_MINUS:
                result = AST_UNOP_MINUS;
                break;

            case TOK_MUL:
                result = AST_UNOP_ADDROF;
                break;

            default:
                assert(false);
        }

        if (result != AST_UNOP_INVALID)
        {
            consume_token(parser);
        }

        return result;
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

        for (uint64_t i = 0; i < BUF_LENGTH(parser->result.errors); i++)
        {
            auto error = parser->result.errors[i];
            fprintf(stderr, "Error:%s:%" PRIu64 ":%" PRIu64 ": %s\n",
                    error.file_pos.file_name, error.file_pos.line,
                    error.file_pos.line_relative_char_pos,
                    error.message);
        }
    }
}
