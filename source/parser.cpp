#include "parser.h"

#include "builtin.h"

#include <stdarg.h>
#include <inttypes.h>

namespace Zodiac
{
    void parser_init(Parser* parser, Context* context)
    {
        assert(parser);
        assert(context);

        parser->context = context;
        parser->allow_vararg_type_specs = false;
    }

    Parse_Result parse_module(Parser* parser, BUF(Token) tokens, const char* module_name,
                              const char* path)
    {
        assert(parser);
        assert(tokens);
        assert(module_name);

        parser->result.module_name = module_name;
        auto ast_module = ast_module_new(parser->context, module_name, path);
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
                                                             true, directive,
                                                             AST_DECL_LOC_GLOBAL);
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
        else
        {
            auto t = current_token(parser);
            if (t.kind == TOK_IDENTIFIER)
            {
                if (t.atom == Builtin::atom_insert)
                {
                    consume_token(parser);
                    kind = AST_DIREC_INSERT;
                }
                else if (t.atom == Builtin::atom_required)
                {
                    consume_token(parser);
                    kind = AST_DIREC_REQUIRED;
                }
                else assert(false);
            }
            else assert(false);
        }

        return ast_directive_new(parser->context, kind, ft.file_pos);
    }

    AST_Declaration* parse_declaration(Parser* parser, AST_Scope* scope, bool global,
                                       AST_Declaration_Location location/*= AST_DECL_LOC_INVALID*/)
    {
        assert(parser);
        assert(scope);

        AST_Directive* directive = nullptr;

        if (match_token(parser, TOK_POUND))
        {
            directive = parse_directive(parser);
        }

        return parse_declaration(parser, scope, global, directive, location);
    }

    AST_Declaration* parse_declaration(Parser* parser, AST_Scope* scope, bool global,
                                       AST_Directive* directive,
                                       AST_Declaration_Location location/*= AST_DECL_LOC_INVALID*/)
    {
        assert(parser);
        assert(scope);

        if (directive && directive->kind == AST_DIREC_DYN_LINK)
        {
            return parse_link_declaration(parser, global, scope, directive);
        }

        if (directive && directive->kind == AST_DIREC_INSERT)
        {
            AST_Statement* stmt = parse_statement(parser, scope);
            assert(stmt);

            return ast_insert_declaration_new(parser->context, directive->file_pos, stmt);
        }

        if (is_token(parser, TOK_KW_STATIC_IF))
        {
            return parse_static_if_declaration(parser, global, scope, location);
        }

        if (is_token(parser, TOK_LBRACE))
        {
            return parse_block_declaration(parser, global, scope, location);
        }

        if (is_token(parser, TOK_KW_STATIC_ASSERT))
        {
            return parse_static_assert_declaration(parser, global, scope);
        }

        if (is_token(parser, TOK_KW_USING))
        {
            return parse_using_declaration(parser, location, scope);
        }

        AST_Identifier* identifier = parse_identifier(parser);
        if (!identifier)
        {
            return nullptr;
        }
        return parse_declaration(parser, identifier, scope, global, directive, location);
    }

    AST_Declaration* parse_declaration(Parser* parser, AST_Identifier* identifier,
                                              AST_Scope* scope,
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
            type_spec = parse_type_spec(parser, scope);
        }

        if (location == AST_DECL_LOC_INVALID)
        {
            location = global ? AST_DECL_LOC_GLOBAL : AST_DECL_LOC_LOCAL;
        }

        if (is_token(parser, TOK_COLON))
        {
            result = parse_constant_declaration(parser, identifier, type_spec, scope, location);
            if (!result)
            {
                return nullptr;
            }
        }
        else
        {
            auto loc = global ? AST_DECL_LOC_GLOBAL : AST_DECL_LOC_LOCAL;
            if (location != AST_DECL_LOC_INVALID)
            {
                loc = location;
            }
            result = parse_mutable_declaration(parser, identifier, type_spec, loc, scope);
            if (!result)
            {
                return nullptr;
            }
            match_token(parser, TOK_SEMICOLON);
        }

        assert(result);
        result->directive = directive;
        if (global)
        {
            assert(result->location == AST_DECL_LOC_INVALID ||
                   result->location == AST_DECL_LOC_GLOBAL);
            result->location = AST_DECL_LOC_GLOBAL;
        }
        return result;
    }

    AST_Declaration* parse_list_declaration(Parser* parser, AST_Expression* list_expr,
                                            AST_Scope* scope)
    {
        assert(list_expr->kind == AST_EXPR_EXPRESSION_LIST);

        bool success = expect_token(parser, TOK_COLON);
        assert(success);
        success = expect_token(parser, TOK_EQ);
        assert(success);

        AST_Expression* init_expr = parse_expression(parser, scope);
        assert(init_expr);
        assert(init_expr->kind == AST_EXPR_CALL);

        return ast_list_declaration_new(parser->context, list_expr->file_pos, list_expr,
                                             init_expr);
    }

    static AST_Declaration* parse_constant_declaration(Parser* parser, AST_Identifier* identifier,
                                                       AST_Type_Spec* type_spec, AST_Scope* scope,
                                                       AST_Declaration_Location location)
    {
        assert(parser);
        assert(identifier);
        assert(scope);

        auto fp = current_token(parser).file_pos;
        expect_token(parser, TOK_COLON);

        auto ast_module = parser->result.ast_module;
        AST_Scope* argument_scope = ast_scope_new(parser->context, ast_module->module_scope,
                                                  ast_module, false, identifier->file_pos.line);

        if (is_token(parser, TOK_LPAREN) && !(peek_token(parser, 1).kind == TOK_COLON))
        {
            assert(!type_spec);

            BUF(AST_Declaration*) arg_decls = nullptr;
            expect_token(parser, TOK_LPAREN);
            bool is_vararg = false;
            bool is_poly = false;
            while (!match_token(parser, TOK_RPAREN))
            {
                // No arguments allowed after this
                assert(!is_vararg);

                if (arg_decls)
                {
                    expect_token(parser, TOK_COMMA);
                }

                if (match_token(parser, TOK_ELLIPSIS))
                {
                    is_vararg = true;
                }
                else
                {
                    bool is_value_poly = false;
                    if (match_token(parser, TOK_DOLLAR))
                    {
                        is_value_poly = true;
                        is_poly = true;
                    }

                    parser->allow_vararg_type_specs = true;
                    AST_Declaration* decl = parse_declaration(parser, argument_scope, false,
                                                              nullptr, AST_DECL_LOC_ARGUMENT);
                    parser->allow_vararg_type_specs = false;

                    if (!decl)
                    {
                        return nullptr;
                    }

                    if (is_value_poly)
                    {
                        decl->flags |= AST_DECL_FLAG_FUNC_VALUE_POLY;
                    }

                    assert(decl->kind == AST_DECL_MUTABLE);
                    auto ts = decl->mutable_decl.type_spec;
                    assert(ts);

                    if (ts->kind == AST_TYPE_SPEC_POLY_FUNC_ARG ||
                        (ts->flags & AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN))
                    {
                        is_poly = true;
                    }

                    if (ts->kind == AST_TYPE_SPEC_VARARG)
                    {
                        is_vararg = true;
                    }

                    BUF_PUSH(arg_decls, decl);
                }
            }

            AST_Type_Spec* return_type_spec = nullptr;
            if (match_token(parser, TOK_RARROW))
            {
                return_type_spec = parse_return_type_spec(parser, scope);
            }

            AST_Statement* body_block = nullptr;

            auto _ct = current_token(parser);
            if (is_token(parser, TOK_LBRACE))
            {
                body_block = parse_block_statement(parser, argument_scope, _ct.file_pos.line);
            }
            else
            {
                expect_token(parser, TOK_SEMICOLON);
            }

            AST_Declaration* result = ast_function_declaration_new(parser->context, fp, scope,
                                                                   identifier,
                                                                   arg_decls, is_vararg,
                                                                   return_type_spec,
                                                                   body_block, argument_scope);
            if (is_poly)
            {
                result->flags |= AST_DECL_FLAG_FUNC_POLY_TEMPLATE;
            }
            return result;
        }
        else if (match_token(parser, TOK_KW_IMPORT))
        {
            assert(!type_spec);

            AST_Identifier* import_module_ident = parse_identifier(parser);
            assert(import_module_ident);
            expect_token(parser, TOK_SEMICOLON);

            return ast_import_declaration_new(parser->context, identifier->file_pos, identifier,
                                              import_module_ident);
        }
        else if (match_token(parser, TOK_KW_TYPEDEF))
        {
            assert(!type_spec);

            AST_Type_Spec* type_spec = parse_type_spec(parser, scope);
            if (!type_spec)
            {
                return nullptr;
            }
            expect_token(parser, TOK_SEMICOLON);

            return ast_typedef_declaration_new(parser->context, identifier->file_pos, identifier,
                                               type_spec);
        }
        else if (match_token(parser, TOK_KW_STRUCT))
        {
            assert(!type_spec);

            AST_Scope* struct_scope = ast_scope_new(parser->context, scope,
                                                    parser->result.ast_module,
                                                    false, identifier->file_pos.line);

            AST_Aggregate_Declaration* agg_decl = parse_aggregate(parser, struct_scope);
            if (!agg_decl) return nullptr;

            return ast_struct_declaration_new(parser->context, identifier->file_pos, identifier,
                                              agg_decl, location, struct_scope);
        }
        else if (match_token(parser, TOK_KW_UNION))
        {
            assert(!type_spec);

            AST_Scope* union_scope = ast_scope_new(parser->context, scope,
                                                   parser->result.ast_module,
                                                   false, identifier->file_pos.line);

            AST_Aggregate_Declaration* agg_decl = parse_aggregate(parser, union_scope);

            return ast_union_declaration_new(parser->context, identifier->file_pos, identifier,
                                             agg_decl, location, union_scope);
        }
        else if (match_token(parser, TOK_KW_ENUM))
        {
            AST_Scope* enum_scope = ast_scope_new(parser->context, scope,
                                                  parser->result.ast_module,
                                                  false, identifier->file_pos.line);
            enum_scope->flags |= AST_SCOPE_FLAG_IS_ENUM_SCOPE;

            AST_Aggregate_Declaration* agg_decl = parse_aggregate(parser, enum_scope, true);
            assert(agg_decl);
            return ast_enum_declaration_new(parser->context, identifier->file_pos, identifier,
                                            type_spec, agg_decl, enum_scope);
        }
        else
        {
            AST_Expression* init_expression = parse_expression(parser, scope);
            if (!expect_token(parser, TOK_SEMICOLON))
            {
                return nullptr;
            }

            return ast_constant_variable_declaration_new(parser->context, identifier->file_pos,
                                                         identifier,
                                                         type_spec, init_expression, location);
        }
    }

    static AST_Declaration* parse_mutable_declaration(Parser* parser, AST_Identifier* identifier,
                                                      AST_Type_Spec* type_spec,
                                                      AST_Declaration_Location location,
		AST_Scope* scope)
    {
        assert(parser);
        assert(identifier);
        assert(location != AST_DECL_LOC_INVALID);
		assert(scope);

        AST_Expression* init_expression = nullptr;
        if (match_token(parser, TOK_EQ))
        {
            init_expression = parse_expression(parser, scope);
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

    static AST_Declaration* parse_static_if_declaration(Parser* parser, bool global,
                                                        AST_Scope* scope,
                                                        AST_Declaration_Location location)
    {
        assert(parser);
        assert(global);
        assert(scope);

        auto ft = current_token(parser);

        assert(is_token(parser, TOK_KW_STATIC_IF));
		expect_token(parser, TOK_KW_STATIC_IF);

        expect_token(parser, TOK_LPAREN);

        AST_Expression* cond_expr = parse_expression(parser, scope);
        if (!cond_expr)
        {
            return nullptr;
        }
        expect_token(parser, TOK_RPAREN);

        AST_Declaration* then_declaration = parse_declaration(parser, scope, true, location);
        if (!then_declaration)
        {
            return nullptr;
        }

        AST_Declaration* else_declaration = nullptr;

        if (match_token(parser, TOK_KW_ELSE))
        {
            else_declaration = parse_declaration(parser, scope, true, nullptr, location);
            if (!else_declaration)
            {
                return nullptr;
            }
        }

        return ast_static_if_declaration_new(parser->context, ft.file_pos, cond_expr,
                                             then_declaration, else_declaration);
    }

    static AST_Declaration* parse_block_declaration(Parser* parser, bool global, AST_Scope* scope,
                                                    AST_Declaration_Location location)
    {
        assert(parser);
        assert(global);
        assert(scope);

        auto ft = current_token(parser);
        assert(is_token(parser, TOK_LBRACE));
		expect_token(parser, TOK_LBRACE);

        BUF(AST_Declaration*) block_decls = nullptr;

        while (!match_token(parser, TOK_RBRACE))
        {
            AST_Declaration_Location loc = AST_DECL_LOC_LOCAL;
            if (global) loc = AST_DECL_LOC_LOCAL;
            AST_Declaration* block_decl = parse_declaration(parser, scope, global, location);
            if (!block_decl)
            {
                return nullptr;
            }

            BUF_PUSH(block_decls, block_decl);
        }

        return ast_block_declaration_new(parser->context, ft.file_pos, block_decls);
    }

    static AST_Declaration* parse_static_assert_declaration(Parser* parser, bool global,
                                                            AST_Scope* scope)
    {
        assert(parser);
        assert(global);
        assert(scope);

        auto ft = current_token(parser);
        expect_token(parser, TOK_KW_STATIC_ASSERT);

        expect_token(parser, TOK_LPAREN);
        AST_Expression* assert_expr = parse_expression(parser, scope);
        expect_token(parser, TOK_RPAREN);
        expect_token(parser, TOK_SEMICOLON);

        return ast_static_assert_declaration_new(parser->context, ft.file_pos, assert_expr);
    }

    static AST_Declaration* parse_using_declaration(Parser* parser,
                                                    AST_Declaration_Location location,
                                                    AST_Scope* scope)
    {
        assert(parser);
        assert(location != AST_DECL_LOC_INVALID);
        assert(scope);

        auto ft = current_token(parser);

        expect_token(parser, TOK_KW_USING);

        AST_Expression* ident_expr = parse_expression(parser, scope);
        expect_token(parser, TOK_SEMICOLON);

        return ast_using_declaration_new(parser->context, ft.file_pos, ident_expr, location);
    }

    AST_Aggregate_Declaration* parse_aggregate(Parser* parser, AST_Scope* scope,
                                               bool is_enum/*=false*/)
    {
        assert(parser);
        assert(scope);

        auto ft = current_token(parser);

        BUF(AST_Identifier*) poly_args = nullptr;

        if (match_token(parser, TOK_LPAREN))
        {
            while (!match_token(parser, TOK_RPAREN))
            {
                if (poly_args) expect_token(parser, TOK_COMMA);
                AST_Identifier* poly_arg = parse_identifier(parser);
                BUF_PUSH(poly_args, poly_arg);
            }

            assert(poly_args);
        }

        if (!expect_token(parser, TOK_LBRACE))
        {
            return nullptr;
        }

        BUF(AST_Declaration*) members = nullptr;
        BUF(AST_Overload_Directive) overloads = nullptr;

        while (!match_token(parser, TOK_RBRACE))
        {
            if (match_token(parser, TOK_POUND))
            {
                AST_Identifier* directive_ident = parse_identifier(parser);
                if (directive_ident->atom == Builtin::atom_overload)
                {
                    AST_Overload_Directive overload;
                    overload.op = parse_overload_operator(parser);

                    for (uint64_t i = 0; i < BUF_LENGTH(overloads); i++)
                    {
                        if (overload.op == overloads[i].op)
                        {
                            parser_report_error(parser, directive_ident->file_pos,
                                                "Multiple overload directives for the same operator are illegal, use function overloading for this kind of behaviour");
                            return nullptr;
                        }
                    }
                    overload.identifier = parse_identifier(parser);
                    expect_token(parser, TOK_SEMICOLON);
                    BUF_PUSH(overloads, overload);
                }
                else assert(false);
            }
            else if (match_token(parser, TOK_KW_STRUCT))
            {
                AST_Aggregate_Declaration* nested_struct = parse_aggregate(parser, scope,
                                                                           false);
                assert(nested_struct);
                AST_Declaration* member_decl =
                    ast_struct_declaration_new(parser->context, nested_struct->file_pos, nullptr,
                                               nested_struct, AST_DECL_LOC_AGGREGATE_MEMBER,
                                               scope);
                BUF_PUSH(members, member_decl);

                // // LEAK: FIXME: We can free the buffers, but we will still leak the
                // //   aggregate declaration itself.
                // BUF_FREE(nested_aggregate->members);
            }
            else if (match_token(parser, TOK_KW_UNION))
            {
                AST_Aggregate_Declaration* nested_union = parse_aggregate(parser, scope, false);
                assert(nested_union);

                AST_Declaration* member_decl =
                    ast_union_declaration_new(parser->context, nested_union->file_pos, nullptr,
                                              nested_union, AST_DECL_LOC_AGGREGATE_MEMBER,
                                              scope);
                BUF_PUSH(members, member_decl);
            }
            else
            {
                AST_Declaration* member_decl = nullptr;
                AST_Identifier* ident = parse_identifier(parser);

                if (!ident) assert(false);
                if (is_enum && match_token(parser, TOK_SEMICOLON))
                {
                    member_decl =
                        ast_constant_variable_declaration_new(parser->context, ident->file_pos,
                                                              ident, nullptr, nullptr,
                                                              AST_DECL_LOC_AGGREGATE_MEMBER);
                }
                else
                {
                    member_decl = parse_declaration(parser, ident, scope, false, nullptr,
                                                    AST_DECL_LOC_AGGREGATE_MEMBER);
                }

                assert(member_decl);
                BUF_PUSH(members, member_decl);
            }
        }

        return ast_aggregate_declaration_new(parser->context, ft.file_pos, members,
                                             poly_args, overloads);
    }

    AST_Statement* parse_statement(Parser* parser, AST_Scope* scope)
    {
        assert(parser);
        assert(scope);
        assert(scope->parent);

        auto ft = current_token(parser);

        switch (ft.kind)
        {
            case TOK_KW_RETURN:
                return parse_return_statement(parser, scope);
                break;

            case TOK_KW_IF:
                return parse_if_statement(parser, scope);
                break;

            case TOK_KW_STATIC_IF:
            {
                auto ft = current_token(parser);
                consume_token(parser);
                expect_token(parser, TOK_LPAREN);
                AST_Expression* cond_expr = parse_expression(parser, scope);
                expect_token(parser, TOK_RPAREN);
                AST_Statement* then_stmt = parse_statement(parser, scope);
                AST_Statement* else_stmt = nullptr;
                if (match_token(parser, TOK_KW_ELSE))
                {
                    else_stmt = parse_statement(parser, scope);
                }

                return ast_static_if_statement_new(parser->context, ft.file_pos, cond_expr,
                                                   then_stmt, else_stmt);
                assert(false);
                break;
            }

            case TOK_LBRACE:
                return parse_block_statement(parser, scope, ft.file_pos.line);
                break;

            case TOK_KW_WHILE:
                return parse_while_statement(parser, scope);
                break;

            case TOK_KW_FOR:
                return parse_for_statement(parser, scope);
                break;

            case TOK_KW_SWITCH:
                return parse_switch_statement(parser, scope);
                break;

            case TOK_KW_BREAK:
            {
                consume_token(parser);
                auto break_stmt = ast_break_statement_new(parser->context, ft.file_pos);
                expect_token(parser, TOK_SEMICOLON);
                return break_stmt;
                break;
            }

            case TOK_KW_USING:
            {
                auto ft = current_token(parser);
                AST_Declaration_Location location = AST_DECL_LOC_LOCAL;
                bool is_module_scope = scope->flags & AST_SCOPE_FLAG_IS_MODULE_SCOPE;
                if (is_module_scope)
                {
                    location = AST_DECL_LOC_GLOBAL;
                }
                AST_Declaration* decl = parse_using_declaration(parser, location, scope);
                if (!decl)
                {
                    return nullptr;
                }
                return ast_declaration_statement_new(parser->context, ft.file_pos, decl);
            }

            case TOK_KW_ASSERT:
            {
                auto ft = current_token(parser);
                consume_token(parser);

                expect_token(parser, TOK_LPAREN);

                AST_Expression* assert_expr = parse_expression(parser, scope);

                expect_token(parser, TOK_RPAREN);
                expect_token(parser, TOK_SEMICOLON);

                return ast_assert_statement_new(parser->context, ft.file_pos, assert_expr);
            }

            case TOK_KW___ASSERT_FAIL:
            {
                auto ft = current_token(parser);
                consume_token(parser);

                expect_token(parser, TOK_LPAREN);
                expect_token(parser, TOK_RPAREN);
                expect_token(parser, TOK_SEMICOLON);

                return ast_assert_fail_statement_new(parser->context, ft.file_pos);
                break;
            }


            case TOK_KW_DEFER:
            {
                auto ft = current_token(parser);
                consume_token(parser);

                AST_Statement* defer_statement = parse_statement(parser, scope);
                return ast_defer_statement_new(parser->context, ft.file_pos, defer_statement);
            }

            case TOK_POUND:
            {
                auto ft = current_token(parser);
                consume_token(parser);

                // LEAK:
                AST_Directive* directive = parse_directive(parser);

                assert(directive);
                assert(directive->kind == AST_DIREC_INSERT);

                AST_Statement* stmt = parse_statement(parser, scope);
                assert(stmt);

                return ast_insert_statement_new(parser->context, ft.file_pos, stmt);
            }

            default: break;
        }

        AST_Expression* lvalue_expr = parse_list_expression(parser, scope);

        if (!lvalue_expr)
        {
            return nullptr;
        }

        if (is_binary_assign_op(parser))
        {
            auto op_tok = current_token(parser);
            auto op = parse_binary_assign_op(parser);

            expect_token(parser, TOK_EQ);
            AST_Expression* rhs = parse_expression(parser, scope);
            AST_Expression* binary_expr = ast_binary_expression_new(parser->context,
                                                                    op_tok.file_pos, lvalue_expr,
                                                                    op, rhs);
            match_token(parser, TOK_SEMICOLON);
            return ast_assign_statement_new(parser->context, lvalue_expr->file_pos, lvalue_expr,
                                            binary_expr);
        }

        if (is_token(parser, TOK_COLON))
        {
            AST_Declaration* decl = nullptr;
            if (lvalue_expr->kind == AST_EXPR_EXPRESSION_LIST)
            {
                decl = parse_list_declaration(parser, lvalue_expr, scope);
            }
            else
            {
                if (lvalue_expr->kind == AST_EXPR_IDENTIFIER)
                {
                    decl = parse_declaration(parser, lvalue_expr->identifier, scope, false,
                                 nullptr);
                }
                else
                {
                    parser_report_error(parser, ft.file_pos, "Expected identifier got a dot expression");
                    return nullptr;
                }
            }

            if (!decl)
            {
                return  nullptr;
            }

            match_token(parser, TOK_SEMICOLON);
            return ast_declaration_statement_new(parser->context, ft.file_pos, decl);
        }
        else if (match_token(parser, TOK_EQ))
        {
            AST_Expression* assign_expression = parse_expression(parser, scope);
            if (!assign_expression) return nullptr;
            match_token(parser, TOK_SEMICOLON);
            return ast_assign_statement_new(parser->context, ft.file_pos, lvalue_expr,
                                            assign_expression);
        }
        else if (lvalue_expr->kind == AST_EXPR_CALL)
        {
            expect_token(parser, TOK_SEMICOLON);
            return ast_call_statement_new(parser->context, lvalue_expr);
        }
        else if (lvalue_expr->kind == AST_EXPR_POST_INCREMENT)
        {
            match_token(parser, TOK_SEMICOLON);
            return ast_post_increment_statement_new(parser->context, lvalue_expr->file_pos,
                                                    lvalue_expr);
        }
        else if (lvalue_expr->kind == AST_EXPR_POST_DECREMENT)
        {
            match_token(parser, TOK_SEMICOLON);
            return ast_post_decrement_statement_new(parser->context, lvalue_expr->file_pos,
                                                    lvalue_expr);
        }
        else
        {
            parser_report_error(parser, lvalue_expr->file_pos,
                                "Expected ':', '=' or '(' after identifier");
            return nullptr;
        }

        assert(false);
        return nullptr;
    }

    static AST_Statement* parse_block_statement(Parser* parser, AST_Scope* scope,
                                                uint64_t opening_line)
    {
        assert(parser);
        assert(scope);

        auto ft = current_token(parser);
        expect_token(parser, TOK_LBRACE);

        BUF(AST_Statement*) block_statements = nullptr;

		auto module = parser->result.ast_module;
        AST_Scope* block_scope = ast_scope_new(parser->context, scope, module, false,
                                               opening_line);

        while (!match_token(parser, TOK_RBRACE))
        {
            AST_Statement* block_statement = parse_statement(parser, block_scope);
            if (!block_statement)
            {
                return nullptr;
            }
            BUF_PUSH(block_statements, block_statement);
        }

        return ast_block_statement_new(parser->context, ft.file_pos, block_statements,
                                       block_scope);
    }

    static AST_Statement* parse_return_statement(Parser* parser, AST_Scope* scope)
    {
        assert(parser);

        auto ret_tok = current_token(parser);
        expect_token(parser, TOK_KW_RETURN);

        AST_Expression* return_expr = nullptr;

        if (!is_token(parser, TOK_SEMICOLON))
        {
            return_expr = parse_list_expression(parser, scope);
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

        AST_Expression* cond_expr = parse_expression(parser, scope);

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

    static AST_Statement* parse_while_statement(Parser* parser, AST_Scope* scope)
    {
        assert(parser);
        assert(scope);

        auto while_tok = current_token(parser);
        expect_token(parser, TOK_KW_WHILE);
        expect_token(parser, TOK_LPAREN);

        AST_Expression* while_condition_expr = parse_expression(parser, scope);
        if (!while_condition_expr)
        {
            return nullptr;
        }

        expect_token(parser, TOK_RPAREN);

        if (scope->flags & AST_SCOPE_FLAG_BREAK_SCOPE)  assert(false);

        auto module = parser->result.ast_module;
        AST_Scope* while_scope = ast_scope_new(parser->context, scope, module, false,
                                               while_tok.file_pos.line);
        while_scope->flags |= AST_SCOPE_FLAG_BREAK_SCOPE;

        AST_Statement* while_body_statement = parse_statement(parser, while_scope);
        if (!while_body_statement)
        {
            return nullptr;
        }


        return ast_while_statement_new(parser->context, while_tok.file_pos, while_scope,
                                       while_condition_expr, while_body_statement);
    }

    static AST_Statement* parse_for_statement(Parser* parser, AST_Scope* scope)
    {
        assert(parser);
        assert(scope);

        auto for_tok = current_token(parser);
        expect_token(parser, TOK_KW_FOR);
        expect_token(parser, TOK_LPAREN);

		auto module = parser->result.ast_module;
        AST_Scope* for_scope = ast_scope_new(parser->context, scope, module, false,
                                             for_tok.file_pos.line);
        for_scope->flags |= AST_SCOPE_FLAG_BREAK_SCOPE;

        AST_Statement* for_decl_statement = parse_statement(parser, for_scope);
        if (!for_decl_statement)
        {
            return nullptr;
        }

        assert(for_decl_statement->kind == AST_STMT_DECLARATION);

        AST_Expression* for_cond_expr = parse_expression(parser, for_scope);
        if (!for_cond_expr)
        {
            return nullptr;
        }

        assert(is_token(parser, TOK_SEMICOLON));
		expect_token(parser, TOK_SEMICOLON);
        AST_Statement* for_step_stmt = parse_statement(parser, for_scope);
        if (!for_step_stmt)
        {
            return nullptr;
        }

        expect_token(parser, TOK_RPAREN);

        AST_Statement* for_body_statement = parse_statement(parser, for_scope);
        if (!for_body_statement)
        {
            return nullptr;
        }
        return ast_for_statement_new(parser->context, for_tok.file_pos, for_scope,
                                     for_decl_statement, for_cond_expr, for_step_stmt,
                                     for_body_statement);

    }

    static AST_Statement* parse_switch_statement(Parser* parser, AST_Scope* scope)
    {
        assert(parser);
        assert(scope);

        auto ft = current_token(parser);
        expect_token(parser, TOK_KW_SWITCH);

        expect_token(parser, TOK_LPAREN);
        AST_Expression* switch_expr = parse_expression(parser, scope);
        expect_token(parser, TOK_RPAREN);

        BUF(AST_Switch_Case) cases = nullptr;

        expect_token(parser, TOK_LBRACE);

        while (is_token(parser, TOK_KW_CASE) || is_token(parser, TOK_KW_DEFAULT))
        {
            auto ct = current_token(parser);

            AST_Scope* case_scope = ast_scope_new(parser->context, scope, scope->module, false,
                                                  ct.file_pos.line);
            case_scope->flags |= AST_SCOPE_FLAG_BREAK_SCOPE;

            if (match_token(parser, TOK_KW_CASE))
            {
                BUF(AST_Expression*) case_expressions = nullptr;
                BUF(AST_Expression*) range_expressions = nullptr;

                while (!match_token(parser, TOK_COLON))
                {
                    AST_Expression* case_expr = parse_expression(parser, scope);
                    assert(case_expr);

                    if (match_token(parser, TOK_DOUBLE_DOT))
                    {
                        BUF_PUSH(range_expressions, case_expr);

                        AST_Expression* rhs = parse_expression(parser, scope);
                        assert(rhs);
                        BUF_PUSH(range_expressions, rhs);
                    }
                    else
                    {
                        BUF_PUSH(case_expressions, case_expr);

                        if (!is_token(parser, TOK_COLON))
                        {
                            expect_token(parser, TOK_COMMA);
                        }
                    }
                }

                assert(BUF_LENGTH(range_expressions) % 2 == 0);

                AST_Statement* case_stmt = parse_statement(parser, case_scope);
                if (!case_stmt) return nullptr;

                AST_Switch_Case switch_case = {};
                switch_case.file_pos = ct.file_pos;
                switch_case.is_default = false;
                switch_case.case_expressions = case_expressions;
                switch_case.range_expressions = range_expressions;
                switch_case.stmt = case_stmt;

                BUF_PUSH(cases, switch_case);
            }
            else if (match_token(parser, TOK_KW_DEFAULT))
            {
                expect_token(parser, TOK_COLON);

                AST_Statement* case_stmt = parse_statement(parser, scope);

                AST_Switch_Case switch_case = {};
                switch_case.file_pos = ct.file_pos;
                switch_case.is_default = true;
                switch_case.case_expressions = nullptr;
                switch_case.stmt = case_stmt;

                BUF_PUSH(cases, switch_case);
            }
            else assert(false);
        }

        expect_token(parser, TOK_RBRACE);

        return ast_switch_statement_new(parser->context, ft.file_pos, switch_expr, cases);
    }

    static AST_Expression* parse_list_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);

        auto ft = current_token(parser);

        BUF(AST_Expression*) exprs = nullptr;

        bool done = false;

        while (!done)
        {
            AST_Expression* expr = nullptr;
            auto ct = current_token(parser);
            if (match_token(parser, TOK_UNDERSCORE))
            {
                expr = ast_expression_ignored_value_new(parser->context, ct.file_pos);
            }
            else
            {
                expr = parse_expression(parser, scope);
            }

            if (!expr)
            {
                BUF_FREE(exprs);
                return nullptr;
            }
            BUF_PUSH(exprs, expr);

            if (!match_token(parser, TOK_COMMA))
            {
                done = true;
            }
        }

        AST_Expression* result = nullptr;

        if (BUF_LENGTH(exprs) > 1)
        {
            result = ast_expression_list_expression_new(parser->context, ft.file_pos, exprs);
        }
        else
        {
            result = exprs[0];
            BUF_FREE(exprs);
        }
        assert(result);

        return result;
    }

    static AST_Expression* parse_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);

        AST_Expression* result = parse_ternary_expression(parser, scope);
        return result;
    }

    static AST_Expression* parse_ternary_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);

        return parse_or_or_expression(parser, scope);
    }

    static AST_Expression* parse_or_or_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);

        AST_Expression* lhs = parse_and_and_expression(parser, scope);

        while (match_token(parser, TOK_OR_OR))
        {
            AST_Expression* rhs = parse_and_and_expression(parser, scope);
            auto op = AST_BINOP_OR_OR;
            lhs = ast_binary_expression_new(parser->context, lhs->file_pos, lhs, op, rhs);
        }

        return lhs;
    }

    static AST_Expression* parse_and_and_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);

        AST_Expression* lhs = parse_or_expression(parser, scope);

        while (match_token(parser, TOK_AND_AND))
        {
            AST_Expression* rhs = parse_or_expression(parser, scope);
            auto op = AST_BINOP_AND_AND;
            lhs = ast_binary_expression_new(parser->context, lhs->file_pos, lhs, op, rhs);
        }

        return lhs;
    }

    static AST_Expression* parse_or_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);

        AST_Expression* lhs = parse_and_expression(parser, scope);

        while (match_token(parser, TOK_OR))
        {
            if (peek_token(parser, 0).kind == TOK_EQ) break;
            AST_Expression* rhs = parse_and_expression(parser, scope);
            if (!rhs) return nullptr;
            auto op = AST_BINOP_OR;
            lhs = ast_binary_expression_new(parser->context, lhs->file_pos, lhs, op, rhs);
        }

        return lhs;
    }

    static AST_Expression* parse_and_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);

        AST_Expression* lhs = parse_cmp_expression(parser, scope);
        if (!lhs) return nullptr;

        while (match_token(parser, TOK_AND))
        {
            if (peek_token(parser, 0).kind == TOK_EQ) break;
            AST_Expression* rhs = parse_cmp_expression(parser, scope);
			if (!rhs) return nullptr;
            auto op = AST_BINOP_AND;
            lhs = ast_binary_expression_new(parser->context, lhs->file_pos, lhs, op, rhs);
        }

        return lhs;
    }

    static AST_Expression* parse_cmp_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);

        AST_Expression* lhs = parse_bitshift_expression(parser, scope);
        if (!lhs) return nullptr;

        while (is_cmp_op(parser))
        {
            auto op_tok = current_token(parser);
            auto op = parse_cmp_op(parser);
            AST_Expression* rhs = parse_bitshift_expression(parser, scope);
            if (!rhs) return nullptr;
            lhs = ast_binary_expression_new(parser->context, op_tok.file_pos, lhs, op, rhs);
            if (!lhs) return nullptr;
        }

        return lhs;
    }

    static AST_Expression* parse_bitshift_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);

        AST_Expression* lhs = parse_add_expression(parser, scope);
        if (!lhs) return nullptr;

        while (is_bitshift_op(parser))
        {
            auto op_tok = current_token(parser);
            auto op = parse_bitshift_op(parser);
            AST_Expression* rhs = parse_add_expression(parser, scope);
            if (!rhs) return nullptr;
            lhs = ast_binary_expression_new(parser->context, op_tok.file_pos, lhs, op, rhs);
            if (!lhs) return nullptr;
        }

        return lhs;
    }

    static AST_Expression* parse_add_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);

        AST_Expression* lhs = parse_mul_expression(parser, scope);
		if (!lhs) return nullptr;

        while (is_add_op(parser))
        {
            auto op_tok = current_token(parser);
            if (peek_token(parser, 1).kind == TOK_EQ ||
                is_binary_assign_op(parser, 1))
            {
                break;
            }
            auto op = parse_add_op(parser);
            AST_Expression* rhs = parse_mul_expression(parser, scope);
			if (!rhs) return nullptr;
            lhs = ast_binary_expression_new(parser->context, op_tok.file_pos, lhs, op, rhs);
        }

        return lhs;
    }

    static AST_Expression* parse_mul_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);

        AST_Expression* lhs = parse_unary_expression(parser, scope);

        while (is_mul_op(parser))
        {
            auto op_tok = current_token(parser);
            if (peek_token(parser, 1).kind == TOK_EQ) break;
            auto op = parse_mul_op(parser);
            AST_Expression* rhs = parse_mul_expression(parser, scope);
            lhs = ast_binary_expression_new(parser->context, op_tok.file_pos, lhs, op, rhs);
        }

        return lhs;
    }

    static AST_Expression* parse_unary_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);
		assert(scope);

        if (is_unary_op(parser))
        {
            auto op_tok = current_token(parser);
            auto op = parse_unary_op(parser);
            AST_Expression* operand_expr = parse_unary_expression(parser, scope);
            return ast_unary_expression_new(parser->context, op_tok.file_pos, op, operand_expr);
        }
        else
        {
            return parse_base_expression(parser, scope);
        }
    }

    static AST_Expression* parse_base_expression(Parser* parser, AST_Scope* scope,
		                                         AST_Expression* base/*=nullptr*/)
    {
        assert(parser);
		assert(scope);

        AST_Expression* result = nullptr;
        auto ft = current_token(parser);

        if (!base)
        {
            if (is_token(parser, TOK_IDENTIFIER))
            {
                // Identifier, Call
                AST_Identifier* identifier = parse_identifier(parser);
                if (!identifier)
                {
                    assert(false);
                }

                if (is_token(parser, TOK_LPAREN))
                {
                    result = parse_call_expression(parser, identifier, scope);
                }
                else
                {
                    result = ast_ident_expression_new(parser->context, identifier->file_pos,
                                                      identifier);
                }
            }
            else if (is_token(parser, TOK_KW_ARRAY_LENGTH))
            {
                result = parse_array_length_expression(parser, scope);
            }
            else if (is_token(parser, TOK_KW_SIZEOF))
            {
                result = parse_sizeof_expression(parser, scope);
            }
            else if (is_token(parser, TOK_KW_GET_TYPE_INFO))
            {
                result = parse_get_type_info_expression(parser, scope);
            }
            else if (is_token(parser, TOK_KW_GET_TYPE_INFO_BASE_PTR))
            {
                result = parse_get_type_info_base_ptr_expression(parser, scope);
            }
            else if (match_token(parser, TOK_POUND))
            {
                return parse_directive_expression(parser, scope);
            }
            else
            {
				auto fp = current_token(parser).file_pos;
                // Paren expr
                if (match_token(parser, TOK_LPAREN))
                {
					if (match_token(parser, TOK_COLON))
					{
						AST_Type_Spec* cast_ts = parse_type_spec(parser, scope);
						expect_token(parser, TOK_RPAREN);
						AST_Expression* cast_expr = parse_base_expression(parser, scope);
						if (!cast_expr) return nullptr;
						result = ast_cast_expression_new(parser->context, fp, cast_ts, cast_expr);
					}
					else
					{
						result = parse_expression(parser, scope);
						expect_token(parser, TOK_RPAREN);
					}
                }
                else
                {
                    // Literal
                    result = parse_literal_expression(parser, scope);
                }
            }

            if (!result) return nullptr;

            assert(result);
        }
        else
        {
            result = base;
        }

        assert(result);

        auto ct = current_token(parser);

        bool done = false;

        switch (ct.kind)
        {
            case TOK_LBRACK:
            {
                expect_token(parser, TOK_LBRACK);
                AST_Expression* index_expr = parse_expression(parser, scope);
                expect_token(parser, TOK_RBRACK);
                result = ast_subscript_expression_new(parser->context, ft.file_pos, result,
                                                      index_expr);
                break;
            }

            case TOK_DOT:
            {
                expect_token(parser, TOK_DOT);
                AST_Identifier* member_ident = parse_identifier(parser);
                assert(member_ident);

                AST_Expression* member_expr = ast_ident_expression_new(parser->context,
                                                                       member_ident->file_pos,
                                                                       member_ident);
                result = ast_dot_expression_new(parser->context, ft.file_pos, result,
                                                member_expr);
                break;
            }

            case TOK_LPAREN:
            {
                result = parse_call_expression(parser, result, scope);
                break;
            }

            case TOK_PLUS:
            {
                if (peek_token(parser, 1).kind == TOK_PLUS)
                {
                    consume_token(parser);
                    consume_token(parser);
                    result = ast_post_increment_expression_new(parser->context, result->file_pos,
                                                               result);
                }

                done = true;

                break;
            }

            case TOK_MINUS:
            {
                if (peek_token(parser, 1).kind == TOK_MINUS)
                {
                    consume_token(parser);
                    consume_token(parser);
                    result = ast_post_decrement_expression_new(parser->context, result->file_pos,
                                                                result);
                }

                done = true;
                break;
            }

            default:
            {
                done = true;
                break;
            }
        }

        if (!done)
        {
            result = parse_base_expression(parser, scope, result);
        }

        return result;
    }

    static AST_Expression* parse_literal_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);
		assert(scope);

        auto ct = current_token(parser);
        //expect_token(parser, TOK_INTEGER);
        switch (ct.kind)
        {
            case TOK_KW_TRUE:
            {
                consume_token(parser);
                return ast_boolean_literal_expression_new(parser->context, ct.file_pos, true);
                break;
            }

            case TOK_KW_FALSE:
            {
                consume_token(parser);
                return ast_boolean_literal_expression_new(parser->context, ct.file_pos, false);
                break;
            }

			case TOK_KW_NULL:
			{
				consume_token(parser);
				return ast_null_literal_expression_new(parser->context, ct.file_pos);
				break;
			}

            case TOK_STRING_LIT:
            {
                consume_token(parser);
                return ast_string_literal_expression_new(parser->context, ct.file_pos, ct.atom);
                break;
            }

            case TOK_INTEGER:
            {
                consume_token(parser);
                uint64_t value = atom_to_u64(ct.atom);
                return ast_integer_literal_expression_new(parser->context, ct.file_pos, value);
                break;
            }

            case TOK_HEX_NUM:
            {
                consume_token(parser);
                uint64_t value = atom_to_u64(ct.atom, 16);
                return ast_integer_literal_expression_new(parser->context, ct.file_pos, value);
                break;
            }

            case TOK_FLOAT:
            {
                consume_token(parser);
                double r64 = atom_to_double(ct.atom);
                float r32 = atom_to_float(ct.atom);
                return ast_float_literal_expression_new(parser->context, ct.file_pos, r64, r32);
                break;
            }

            case TOK_CHAR_LIT:
            {
                consume_token(parser);
                char c = ct.atom.data[0];
                return ast_character_literal_expression_new(parser->context, ct.file_pos, c);
                break;
            }

            case TOK_LBRACE:
            {
                return parse_compound_literal_expression(parser, scope);
                break;
            }

            default:
            {
                parser_report_error(parser, ct.file_pos, "Unexptected token: '%.*s'",
                                    ct.atom.length, ct.atom.data);
                return nullptr;
                break;
            }
        }

        assert(false);
        return nullptr;
    }

    static AST_Expression* parse_compound_literal_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);
        assert(scope);

        auto ft = current_token(parser);
        expect_token(parser, TOK_LBRACE);

        BUF(AST_Expression*) compound_expressions = nullptr;

        while (!match_token(parser, TOK_RBRACE))
        {
            if (compound_expressions)
            {
                expect_token(parser, TOK_COMMA);
            }

            AST_Expression* compound_expression = parse_expression(parser, scope);
            BUF_PUSH(compound_expressions, compound_expression);
        }

        return ast_compound_literal_expression_new(parser->context, ft.file_pos,
                                                   compound_expressions);
    }

    static AST_Expression* parse_array_length_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);
		assert(scope);

        auto ft = current_token(parser);

        expect_token(parser, TOK_KW_ARRAY_LENGTH);
        expect_token(parser, TOK_LPAREN);
        AST_Expression* ident_expr = parse_expression(parser, scope);
        // assert(ident_expr->kind == AST_EXPR_IDENTIFIER ||
        //        ident_expr->kind == AST_EXPR_DOT);
        expect_token(parser, TOK_RPAREN);

        return ast_array_length_expression_new(parser->context, ft.file_pos, ident_expr);
    }

    static AST_Expression* parse_sizeof_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);
        assert(scope);

        auto ft = current_token(parser);

        expect_token(parser, TOK_KW_SIZEOF);
        expect_token(parser, TOK_LPAREN);
        AST_Type_Spec* type_spec = parse_type_spec(parser, scope);
        expect_token(parser, TOK_RPAREN);

        return ast_sizeof_expression_new(parser->context, ft.file_pos, type_spec);
    }

    static AST_Expression* parse_get_type_info_expression(Parser* parser, AST_Scope* scope)
    {
        assert(parser);
        assert(scope);

        auto ft = current_token(parser);

        expect_token(parser, TOK_KW_GET_TYPE_INFO);
        expect_token(parser, TOK_LPAREN);
        AST_Type_Spec* type_spec = parse_type_spec(parser, scope);
        expect_token(parser, TOK_RPAREN);

        return ast_get_type_info_expression_new(parser->context, ft.file_pos, type_spec);
    }

    static AST_Expression* parse_get_type_info_base_ptr_expression(Parser* parser,
                                                                   AST_Scope* scope)
    {
        auto ft = current_token(parser);

        expect_token(parser, TOK_KW_GET_TYPE_INFO_BASE_PTR);
        expect_token(parser, TOK_LPAREN);
        expect_token(parser, TOK_RPAREN);
        return ast_get_type_info_base_ptr_expression_new(parser->context, ft.file_pos);
    }

    static AST_Expression* parse_directive_expression(Parser* parser, AST_Scope* scope)
    {
        auto context = parser->context;
        auto ft = current_token(parser);

        AST_Identifier* ident = parse_identifier(parser);
        if (ident->atom == Builtin::atom_FUNC_NAME)
        {
            return ast_func_name_expression_new(context, ft.file_pos);
        }
        else if (ident->atom == Builtin::atom_FILE_NAME)
        {
            auto module = parser->result.ast_module;
            Atom a_dir = atom_get(context->atom_table, module->module_file_dir);
            Atom a_file = atom_get(context->atom_table, module->module_file_name);
            Atom a_path = atom_append(context->atom_table, a_dir, a_file);

            return ast_string_literal_expression_new(context, ft.file_pos, a_path);
        }
        else if (ident->atom == Builtin::atom_LINE_NO)
        {
            return ast_integer_literal_expression_new(context, ft.file_pos,
                                                      ident->file_pos.line);
        }

        assert(false);
        return nullptr;
    }

    static AST_Expression* parse_call_expression(Parser* parser, AST_Expression* ident_expression,
		AST_Scope* scope)
    {
        assert(parser);
        assert(ident_expression);
        assert(ident_expression->kind == AST_EXPR_IDENTIFIER ||
               ident_expression->kind == AST_EXPR_DOT);
		assert(scope);

        expect_token(parser, TOK_LPAREN);

        BUF(AST_Expression*) arg_exprs = nullptr;

        while (!match_token(parser, TOK_RPAREN))
        {
            if (arg_exprs)
            {
                expect_token(parser, TOK_COMMA);
            }

            AST_Expression* arg_expr = parse_expression(parser, scope);
            if (!arg_expr)
            {
                return nullptr;
            }
            BUF_PUSH(arg_exprs, arg_expr);
        }

        return ast_call_expression_new(parser->context, ident_expression->file_pos,
                                       ident_expression,
                                       arg_exprs);
    }

    static AST_Expression* parse_call_expression(Parser* parser, AST_Identifier* identifier,
		AST_Scope* scope)
    {
        assert(parser);
        assert(identifier);
		assert(scope);

        AST_Expression* ident_expression = ast_ident_expression_new(parser->context,
                                                                    identifier->file_pos,
                                                                    identifier);
        return parse_call_expression(parser, ident_expression, scope);
    }

    static AST_Type_Spec* parse_type_spec(Parser* parser, AST_Scope* scope)

    {
        assert(parser);
		assert(scope);

        auto ft = current_token(parser);

        if (match_token(parser, TOK_MUL))
        {
            AST_Type_Spec* base_type_spec = parse_type_spec(parser, scope);
            if (!base_type_spec)
            {
                return nullptr;
            }
            return ast_type_spec_pointer_new(parser->context, ft.file_pos, base_type_spec);
        }
        else if (match_token(parser, TOK_KW_TYPEOF))
        {
            expect_token(parser, TOK_LPAREN);
            AST_Expression* type_expr = parse_expression(parser, scope);
            expect_token(parser, TOK_RPAREN);

            return ast_type_spec_typeof_new(parser->context, ft.file_pos, type_expr);

        }
		else if (is_token(parser, TOK_LPAREN))
		{
			return parse_function_type_spec(parser, scope);
		}
        else if (match_token(parser, TOK_LBRACK))
        {
            AST_Expression* count_expr = parse_expression(parser, scope);
            expect_token(parser, TOK_RBRACK);
            AST_Type_Spec* base_type_spec = parse_type_spec(parser, scope);
            return ast_type_spec_static_array_new(parser->context, ft.file_pos, count_expr,
                                                  base_type_spec);
        }
        else if (match_token(parser, TOK_DOLLAR))
        {
            AST_Identifier* poly_arg_ident = parse_identifier(parser);
            return ast_type_spec_poly_func_arg_new(parser->context, ft.file_pos, poly_arg_ident);
        }
        else if (is_token(parser, TOK_IDENTIFIER))
        {
            AST_Identifier* base_ident = parse_identifier(parser);
            AST_Type_Spec* result = nullptr;
            if (match_token(parser, TOK_DOT))
            {
                AST_Type_Spec* member_type_spec = parse_type_spec(parser, scope);
                return ast_type_spec_dot_new(parser->context, base_ident->file_pos, base_ident,
                                             member_type_spec);
            }
            else if (match_token(parser, TOK_LPAREN))
            {
                BUF(AST_Type_Spec*) poly_args = nullptr;
                while (!match_token(parser, TOK_RPAREN))
                {
                    if (poly_args)
                    {
                        expect_token(parser, TOK_COMMA);
                    }

                    AST_Type_Spec* poly_arg = parse_type_spec(parser, scope);
                    BUF_PUSH(poly_args, poly_arg);
                }

                return ast_type_spec_identifier_new(parser->context, base_ident->file_pos,
                                                    base_ident, poly_args);
            }
            else
            {
                return ast_type_spec_identifier_new(parser->context, base_ident->file_pos,
                                                    base_ident, nullptr);
            }
            assert(false);
        }
        else if (parser->allow_vararg_type_specs && match_token(parser, TOK_ELLIPSIS))
        {
            return ast_type_spec_vararg_new(parser->context, ft.file_pos);
        }
        else if (match_token(parser, TOK_KW_TYPE))
        {
            return ast_type_spec_type_new(parser->context, ft.file_pos);
        }
        else
        {
			auto err_tok = current_token(parser);
			parser_report_error(parser, err_tok.file_pos, "Unexpected token '%s' when parsing a type spec", err_tok.atom.data);
			return nullptr;
        }

		assert(false);
		return nullptr;
    }

    static AST_Type_Spec* parse_return_type_spec(Parser* parser, AST_Scope* scope)
    {
        BUF(AST_Type_Spec*) specs = nullptr;
        BUF(AST_Directive*) directives = nullptr;

        auto rts = parse_type_spec(parser, scope);
        auto ffp = rts->file_pos;

        AST_Directive* directive = nullptr;

        if (match_token(parser, TOK_POUND))
        {
            auto directive = parse_directive(parser);
            assert(directive->kind == AST_DIREC_REQUIRED);
            BUF_PUSH(directives, directive);
        }
        else BUF_PUSH(directives, nullptr);

        if (!is_token(parser, TOK_COMMA)) return rts;

        BUF_PUSH(specs, rts);

        while (match_token(parser, TOK_COMMA))
        {
            rts = parse_type_spec(parser, scope);
            if (match_token(parser, TOK_POUND))
            {
                auto directive = parse_directive(parser);
                assert(directive->kind == AST_DIREC_REQUIRED);
                BUF_PUSH(directives, directive);
            }
            else BUF_PUSH(directives, nullptr);
            BUF_PUSH(specs, rts);
        }

        return ast_type_spec_mrv_new(parser->context, ffp, specs, directives);
    }

	static AST_Type_Spec* parse_function_type_spec(Parser* parser, AST_Scope* scope)
	{
		assert(parser);
		assert(scope);

		auto ft = current_token(parser);

		bool is_vararg = false;
		BUF(AST_Declaration*) arg_decls = nullptr;

		auto module = parser->result.ast_module;
        AST_Scope* arg_scope = ast_scope_new(parser->context, scope, module, false,
                                             ft.file_pos.line);

		expect_token(parser, TOK_LPAREN);
		while (!match_token(parser, TOK_RPAREN))
		{
			assert(!is_vararg);

			if (arg_decls)
			{
                expect_token(parser, TOK_COMMA);
			}

			if (match_token(parser, TOK_ELLIPSIS))
			{
				is_vararg = true;
			}
			else
			{
				AST_Declaration* decl = parse_declaration(parser, arg_scope, false, nullptr,
														  AST_DECL_LOC_ARGUMENT);

				if (!decl)
				{
					return nullptr;
				}
				BUF_PUSH(arg_decls, decl);
			}
		}

		AST_Type_Spec* return_type_spec = nullptr;
		if (match_token(parser, TOK_RARROW))
		{
			return_type_spec = parse_return_type_spec(parser, scope);
		}

		return ast_type_spec_function_new(parser->context, ft.file_pos, is_vararg, arg_decls,
		                                  return_type_spec, arg_scope, "");
	}

    static AST_Overload_Operator_Kind parse_overload_operator(Parser* parser)
    {
        assert(parser);

        auto ft = current_token(parser);
        consume_token(parser);

		auto result = AST_OVERLOAD_OP_INVALID;

        switch (ft.kind)
        {
            case TOK_LBRACK:
            {
                expect_token(parser, TOK_RBRACK);
                if (match_token(parser, TOK_MUL))
                {
                    result = AST_OVERLOAD_OP_INDEX_LVALUE;
                }
                else
                {
                    result = AST_OVERLOAD_OP_INDEX;
                }
				break;
            }

            case TOK_PLUS:
            {
                result = AST_OVERLOAD_OP_PLUS;
				break;
            }

            case TOK_MINUS:
            {
                result = AST_OVERLOAD_OP_MINUS;
				break;
            }

            case TOK_MUL:
            {
                result = AST_OVERLOAD_OP_MUL;
				break;
            }

            case TOK_DIV:
            {
                result = AST_OVERLOAD_OP_DIV;
				break;
            }

            default: assert(false);
        }

		return result;
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

        return ct.kind == TOK_MUL || ct.kind == TOK_DIV || ct.kind == TOK_PERCENT;
    }

    bool is_cmp_op(Parser* parser)
    {
        assert(parser);

        auto ct = current_token(parser);

        return ct.kind == TOK_EQEQ ||
               ct.kind == TOK_LT ||
               ct.kind == TOK_LTEQ ||
               ct.kind == TOK_GT ||
               ct.kind == TOK_GTEQ ||
               ct.kind == TOK_NEQ;
    }

    static bool is_unary_op(Parser* parser)
    {
        assert(parser);

        auto ct = current_token(parser);

        return ct.kind == TOK_MINUS || ct.kind == TOK_MUL || ct.kind == TOK_LT ||
               ct.kind == TOK_BANG || ct.kind == TOK_TILDE;
    }

    static bool is_bitshift_op(Parser* parser)
    {
        auto ct = current_token(parser);

        return ct.kind == TOK_LSHIFT || ct.kind == TOK_RSHIFT;
    }
    static AST_Binop_Kind parse_add_op(Parser* parser)
    {
        assert(parser);

        auto ct = current_token(parser);
		AST_Binop_Kind result = AST_BINOP_INVALID;

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

            case TOK_PERCENT:
                result = AST_BINOP_MOD;
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
		AST_Binop_Kind result = AST_BINOP_INVALID;

        switch (ct.kind)
        {
            case TOK_LT:
                result = AST_BINOP_LT;
                break;

            case TOK_LTEQ:
                result = AST_BINOP_LTEQ;
                break;

            case TOK_GT:
                result = AST_BINOP_GT;
                break;

            case TOK_GTEQ:
                result = AST_BINOP_GTEQ;
                break;

            case TOK_EQEQ:
                result = AST_BINOP_EQ;
                break;

            case TOK_NEQ:
                result = AST_BINOP_NEQ;
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

            case TOK_LT:
                result = AST_UNOP_DEREF;
                break;

            case TOK_BANG:
                result = AST_UNOP_NOT;
                break;

			case TOK_TILDE:
				result = AST_UNOP_BIN_NOT;
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

    static AST_Binop_Kind parse_bitshift_op(Parser* parser)
    {
        assert(parser);

        auto ct = current_token(parser);
		AST_Binop_Kind result = AST_BINOP_INVALID;

        switch (ct.kind)
        {
            case TOK_LSHIFT:
                result = AST_BINOP_LSHIFT;
                break;

            case TOK_RSHIFT:
                result = AST_BINOP_RSHIFT;
                break;

            default: assert(false);
        }

        consume_token(parser);
        return result;
    }

    static bool is_binary_assign_op(Parser* parser, uint64_t offset /*= 0*/)
    {
        assert(parser);

        auto ct = peek_token(parser, offset);

        switch (ct.kind)
        {
            case TOK_PLUS:
            case TOK_MINUS:
            case TOK_MUL:
            case TOK_DIV:
            case TOK_OR:
            case TOK_AND:
                return true;

            default:
                return false;
        }
    }

    static AST_Binop_Kind parse_binary_assign_op(Parser* parser)
    {
        assert(parser);

        auto ct = current_token(parser);

        auto result = AST_BINOP_INVALID;

        switch (ct.kind)
        {
            case TOK_PLUS:
            {
                result = AST_BINOP_ADD;
                break;
            }

            case TOK_MINUS:
            {
                result = AST_BINOP_SUB;
                break;
            }

            case TOK_MUL:
            {
                result = AST_BINOP_MUL;
                break;
            }

            case TOK_DIV:
            {
                result = AST_BINOP_DIV;
                break;
            }

            case TOK_OR:
            {
                result = AST_BINOP_OR;
                break;
            }

            case TOK_AND:
            {
                result = AST_BINOP_AND;
                break;
            }

            default: assert(false);
        }

        if (result != AST_BINOP_INVALID)
        {
            consume_token(parser);
        }

        return result;
    }

    static Token peek_token(Parser* parser, uint64_t offset)
    {
        assert(parser);
        assert(parser->ti + offset < BUF_LENGTH(parser->tokens));

        return parser->tokens[parser->ti + offset];
    }

    static Token current_token(Parser* parser)
    {
        assert(parser);

        return peek_token(parser, 0);
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

        parser_report_error(parser, tt.file_pos,
                            "Expected token of type: %s, got token: %s: '%s'\n",
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

        if (parser->ti < BUF_LENGTH(parser->tokens))
        {
            auto ct = current_token(parser);
            return ct.kind == token_kind;
        }

        return false;
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

		Parse_Error result;
        result.file_pos = file_pos;
        message_size = strlen(message_buf);
        result.message = (char*)mem_alloc(message_size + 1);
		assert(result.message);
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
