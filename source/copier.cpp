#include "copier.h"

namespace Zodiac
{
    AST_Declaration* copy_declaration(Context* context, AST_Declaration* declaration,
                                      Copy_Flag flags /*=AST_SCOPE_FLAG_NONE*/)
    {
        assert(context);
        assert(declaration);

        AST_Identifier* ident_copy = copy_identifier(context, declaration->identifier, flags);

        switch (declaration->kind)
        {
            case AST_DECL_FUNC:
            {
                BUF(AST_Declaration*) args_copy = nullptr;

                for (uint64_t i = 0; i < BUF_LENGTH(declaration->function.args); i++)
                {
                    auto arg = declaration->function.args[i];
                    if (arg->flags & AST_DECL_FLAG_FUNC_VALUE_POLY)
                    {
                    }
                    else
                    {
                        auto arg_copy = copy_declaration(context, arg,
                                                         flags);
                        BUF_PUSH(args_copy, arg_copy);
                    }
                }
                bool is_vararg = (declaration->flags & AST_DECL_FLAG_FUNC_VARARG);
                auto return_ts_copy = copy_type_spec(context,
                                                     declaration->function.return_type_spec,
                                                     flags);
                auto arg_scope_copy = copy_scope(context, declaration->function.argument_scope,
                                                 flags);
                auto body_block_copy = copy_statement(context, declaration->function.body_block,
                                                      arg_scope_copy, flags);

                return ast_function_declaration_new(context, declaration->file_pos,
                                                    declaration->scope,
                                                    ident_copy, args_copy, is_vararg,
                                                    return_ts_copy, body_block_copy,
                                                    arg_scope_copy);
                break;
            }

            case AST_DECL_FUNC_OVERLOAD:
            {
                assert(false);
                break;
            }

            case AST_DECL_MUTABLE:
            {
                auto type_spec_copy = copy_type_spec(context, declaration->mutable_decl.type_spec,
                                                     flags);
                auto init_expr_copy = copy_expression(context,
                                                      declaration->mutable_decl.init_expression,
                                                      flags);
                auto result = ast_mutable_declaration_new(context, declaration->file_pos,
                                                   ident_copy, type_spec_copy,
                                                   init_expr_copy, declaration->location);
                if (!(flags & COPY_FLAG_DONT_COPY_POLY))
                {
                    auto allowed_flags = (AST_DECL_FLAG_RESOLVED | AST_DECL_FLAG_FUNC_VALUE_POLY);
                    assert(declaration->flags == 0 ||
                           (declaration->flags | allowed_flags) == allowed_flags);
                    result->flags = (declaration->flags | allowed_flags);
                    result->flags &= ~AST_DECL_FLAG_RESOLVED;
                }

                return result;
                break;
            }

            case AST_DECL_CONSTANT_VAR:
            {
                assert(false);
                break;
            }

            case AST_DECL_TYPE:
            {
                assert(false);
                break;
            }

            case AST_DECL_DYN_LINK:
            {
                assert(false);
                break;
            }

            case AST_DECL_STATIC_IF:
            {
                assert(false);
                break;
            }

            case AST_DECL_BLOCK:
            {
                assert(false);
                break;
            }

            case AST_DECL_STATIC_ASSERT:
            {
                assert(false);
                break;
            }

            case AST_DECL_IMPORT:
            {
                assert(false);
                break;
            }

            case AST_DECL_AGGREGATE_TYPE:
            {
                auto agg_decl = declaration->aggregate_type.aggregate_decl;
                BUF(AST_Declaration*) members_copy = nullptr;
                for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->members); i++)
                {
                    auto member = agg_decl->members[i];
                    auto member_copy = copy_declaration(context, member, flags);
                    BUF_PUSH(members_copy, member_copy);
                }

                BUF(AST_Identifier*) poly_args_copy = nullptr;
                if (!(flags & COPY_FLAG_DONT_COPY_POLY))
                {
                    for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->poly_args); i++)
                    {
                        auto poly_arg = agg_decl->poly_args[i];
                        auto poly_arg_copy = copy_identifier(context, poly_arg, flags);
                        BUF_PUSH(poly_args_copy, poly_arg_copy);
                    }
                }

                BUF(AST_Overload_Directive) overloads_copy = nullptr;
                for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->overload_directives); i++)
                {
                    auto overload = agg_decl->overload_directives[i];
                    auto overload_copy = copy_overload_directive(context, overload, flags);
                    BUF_PUSH(overloads_copy, overload_copy);
                }

                auto agg_decl_copy = ast_aggregate_declaration_new(context, declaration->file_pos,
                                                                   members_copy, poly_args_copy,
                                                                   overloads_copy);

                AST_Declaration* result = nullptr;
                auto scope_copy = copy_scope(context, declaration->aggregate_type.scope, flags);
                if (declaration->aggregate_type.kind == AST_AGG_DECL_STRUCT)
                {
                    result = ast_struct_declaration_new(context, declaration->file_pos,
                                                        ident_copy, agg_decl_copy,
                                                        declaration->location, scope_copy);
                }
                else if (declaration->aggregate_type.kind == AST_AGG_DECL_UNION)
                {
                    result = ast_union_declaration_new(context, declaration->file_pos,
                                                       ident_copy, agg_decl_copy,
                                                       declaration->location, scope_copy);
                }
                else if (declaration->aggregate_type.kind == AST_AGG_DECL_ENUM)
                {
                    auto enum_type_spec = declaration->aggregate_type.enum_type_spec;
                    auto type_spec_copy = copy_type_spec(context, enum_type_spec);
                    result = ast_enum_declaration_new(context, declaration->file_pos, ident_copy,
                                                      type_spec_copy, agg_decl_copy, scope_copy);
                }
                else assert(false);

                return result;
                break;
            }

            case AST_DECL_TYPEDEF:
            {
                assert(false);
                break;
            }

            case AST_DECL_USING:
            {
                assert(false);
                break;
            }

            case AST_DECL_INSERT:
            {
                assert(false);
                break;
            }

            default: assert(false);
        }

		assert(false);
		return nullptr;
    }

    AST_Statement* copy_statement(Context* context, AST_Statement* statement,
                                  AST_Scope* parent_scope, Copy_Flag flags /*=COPY_FLAG_NONE*/)
    {
        assert(context);
        assert(parent_scope);

        if (!statement) return nullptr;

        switch (statement->kind)
        {
            case AST_STMT_DECLARATION:
            {
                auto decl_copy = copy_declaration(context, statement->declaration, flags);
                return ast_declaration_statement_new(context, statement->file_pos, decl_copy);
                break;
            }

            case AST_STMT_RETURN:
            {
                auto expr_copy = copy_expression(context, statement->return_expression, flags);
                return ast_return_statement_new(context, statement->file_pos, expr_copy);
                break;
            }

            case AST_STMT_BLOCK:
            {
                auto scope_copy = copy_scope(context, statement->block.scope);
                scope_copy->parent = parent_scope;

                BUF(AST_Statement*) stmts_copy = nullptr;
                for (uint64_t i = 0; i < BUF_LENGTH(statement->block.statements); i++)
                {
                    auto stmt = statement->block.statements[i];
                    auto stmt_copy = copy_statement(context, stmt, scope_copy, flags);
                    BUF_PUSH(stmts_copy, stmt_copy);
                }
                return ast_block_statement_new(context, statement->file_pos, stmts_copy,
                                               scope_copy);
                break;
            }

            case AST_STMT_IF:
            {
                auto cond_copy = copy_expression(context, statement->if_stmt.if_expression, flags);
                auto then_copy = copy_statement(context, statement->if_stmt.then_statement,
                                                parent_scope, flags);
                auto else_copy = copy_statement(context, statement->if_stmt.else_statement,
                                                parent_scope, flags);
                return ast_if_statement_new(context, statement->file_pos, cond_copy, then_copy,
                                            else_copy);
                break;
            }

            case AST_STMT_ASSIGN:
            {
                auto lvalue_copy = copy_expression(context, statement->assign.lvalue_expression,
                                                   flags);
                auto value_copy = copy_expression(context, statement->assign.expression, flags);
                return ast_assign_statement_new(context, statement->file_pos, lvalue_copy,
                                                value_copy);
                break;
            }

            case AST_STMT_CALL:
            {
                auto call_expr_copy = copy_expression(context, statement->call_expression, flags);
                return ast_call_statement_new(context, call_expr_copy);
                break;
            }

            case AST_STMT_WHILE:
            {
                auto cond_copy = copy_expression(context, statement->while_stmt.cond_expr, flags);
                assert(statement->while_stmt.body_stmt->kind == AST_STMT_BLOCK);
                auto scope_copy = copy_scope(context,
                                             statement->while_stmt.scope, flags);
                scope_copy->parent = parent_scope;
                auto body_copy = copy_statement(context, statement->while_stmt.body_stmt,
                                                scope_copy, flags);
                return ast_while_statement_new(context, statement->file_pos, scope_copy,
                                               cond_copy, body_copy);
                break;
            }

            case AST_STMT_FOR:
            {
                auto scope_copy = copy_scope(context, statement->for_stmt.scope, flags);
                scope_copy->parent = parent_scope;
                auto init_copy = copy_statement(context, statement->for_stmt.init_stmt,
                                                scope_copy, flags);
                auto cond_copy = copy_expression(context, statement->for_stmt.cond_expr, flags);
                auto step_copy = copy_statement(context, statement->for_stmt.step_stmt,
                                               scope_copy, flags);
                auto body_copy = copy_statement(context, statement->for_stmt.body_stmt,
                                                scope_copy, flags);
                return ast_for_statement_new(context, statement->file_pos, scope_copy, init_copy,
                                             cond_copy, step_copy, body_copy);
                break;
            }

            case AST_STMT_SWITCH:
            {
                assert(false);
                break;
            }

            case AST_STMT_BREAK:
            {
                return ast_break_statement_new(context, statement->file_pos);
                break;
            }

            case AST_STMT_INSERT:
            {
                assert(false);
                break;
            }

            case AST_STMT_ASSERT:
            {
                auto expr_copy = copy_expression(context, statement->assert_expression, flags);
                return ast_assert_statement_new(context, statement->file_pos, expr_copy);
                break;
            }

            case AST_STMT_DEFER:
            {
                assert(false);
                break;
            }

            case AST_STMT_POST_INCREMENT:
            {
                auto post_inc_expr_copy = copy_expression(context, statement->post_increment,
                                                          flags);
                return ast_post_increment_statement_new(context, statement->file_pos,
                                                        post_inc_expr_copy);
                break;
            }

            case AST_STMT_POST_DECREMENT:
            {
                auto post_dec_expr_copy = copy_expression(context, statement->post_decrement,
                                                          flags);
                return ast_post_decrement_statement_new(context, statement->file_pos,
                                                        post_dec_expr_copy);
                break;
            }

            default: assert(false);
        }

		assert(false);
		return nullptr;
    }

    AST_Expression* copy_expression(Context* context, AST_Expression* expression,
                                    Copy_Flag flags /*=AST_SCOPE_FLAG_NONE*/)
    {
        assert(context);

        if (!expression) return nullptr;

        assert(expression);

        switch (expression->kind)
        {
            case AST_EXPR_BINARY:
            {
                auto lhs_copy = copy_expression(context, expression->binary.lhs, flags);
                auto rhs_copy = copy_expression(context, expression->binary.rhs, flags);
                return ast_binary_expression_new(context, expression->file_pos, lhs_copy,
                                                 expression->binary.op, rhs_copy);
                break;
            }

            case AST_EXPR_UNARY:
            {
                auto operand_copy = copy_expression(context, expression->unary.operand, flags);
                return ast_unary_expression_new(context, expression->file_pos,
                                                expression->unary.op, operand_copy);
                break;
            }

            case AST_EXPR_IDENTIFIER:
            {
                auto ident_copy = copy_identifier(context, expression->identifier, flags);

                return ast_ident_expression_new(context, expression->file_pos, ident_copy);
                break;
            }

            case AST_EXPR_CALL:
            {
                auto ident_copy = copy_expression(context, expression->call.ident_expression,
                                                  flags);

                BUF(AST_Expression*) args_copy = nullptr;
                for (uint64_t i = 0; i < BUF_LENGTH(expression->call.arg_expressions); i++)
                {
                    auto arg_expr = expression->call.arg_expressions[i];
                    auto arg_expr_copy = copy_expression(context, arg_expr, flags);
                    BUF_PUSH(args_copy, arg_expr_copy);
                }

                return ast_call_expression_new(context, expression->file_pos, ident_copy,
                                               args_copy);
                break;
            }

            case AST_EXPR_SUBSCRIPT:
            {
                auto base_copy = copy_expression(context, expression->subscript.base_expression,
                                                 flags);
                auto index_copy = copy_expression(context, expression->subscript.index_expression,
                                                  flags);
                return ast_subscript_expression_new(context, expression->file_pos, base_copy,
                                                    index_copy);
                break;
            }

            case AST_EXPR_BOOL_LITERAL:
            {
                return ast_boolean_literal_expression_new(context, expression->file_pos,
                                                          expression->bool_literal.boolean);
                break;
            }

            case AST_EXPR_NULL_LITERAL:
            {
                return ast_null_literal_expression_new(context, expression->file_pos);
                break;
            }

            case AST_EXPR_STRING_LITERAL:
            {
                return ast_string_literal_expression_new(context, expression->file_pos,
                                                         expression->string_literal.atom);
                break;
            }

            case AST_EXPR_INTEGER_LITERAL:
            {
                return ast_integer_literal_expression_new(context, expression->file_pos,
                                                          expression->integer_literal.u64);
                break;
            }

            case AST_EXPR_FLOAT_LITERAL:
            {
                return ast_float_literal_expression_new(context, expression->file_pos,
                                                        expression->float_literal.r64,
                                                        expression->float_literal.r32);
                break;
            }

            case AST_EXPR_CHAR_LITERAL:
            {
                return ast_character_literal_expression_new(context, expression->file_pos,
                                                            expression->character_literal.c);
                break;
            }

            case AST_EXPR_COMPOUND_LITERAL:
            {
                BUF(AST_Expression*) exprs_copy = nullptr;
                auto exprs = expression->compound_literal.expressions;
                for (uint64_t i = 0; i < BUF_LENGTH(exprs); i++)
                {
                    BUF_PUSH(exprs_copy, copy_expression(context, exprs[i], flags));
                }
                return ast_compound_literal_expression_new(context, expression->file_pos,
                                                           exprs_copy);
                break;
            }

            case AST_EXPR_ARRAY_LENGTH:
            {
                assert(false);
                break;
            }

            case AST_EXPR_DOT:
            {
                auto base_copy = copy_expression(context, expression->dot.base_expression, flags);
                auto member_copy = copy_expression(context, expression->dot.member_expression,
                                                   flags);
                return ast_dot_expression_new(context, expression->file_pos, base_copy,
                                              member_copy);
                break;
            }

            case AST_EXPR_CAST:
            {
                auto ts_copy = copy_type_spec(context, expression->cast_expr.type_spec, flags);
                if (!ts_copy)
                {
                    assert(expression->type);
                    ts_copy = ast_type_spec_from_type_new(context, expression->file_pos, expression->type);
                }
                auto expr_copy = copy_expression(context, expression->cast_expr.expr, flags);
                auto result = ast_cast_expression_new(context, expression->file_pos, ts_copy, expr_copy);
                return result;
                break;
            }

            case AST_EXPR_SIZEOF:
            {
                auto ts_copy = copy_type_spec(context, expression->sizeof_expr.type_spec, flags);
                return ast_sizeof_expression_new(context, expression->file_pos, ts_copy);
                break;
            }

            case AST_EXPR_GET_TYPE_INFO:
            {
                auto ts_copy = copy_type_spec(context, expression->get_type_info_expr.type_spec,
                                              flags);
                return ast_get_type_info_expression_new(context, expression->file_pos, ts_copy);
                break;
            }

            case AST_EXPR_POST_INCREMENT:
            {
                auto base_copy = copy_expression(context, expression->base_expression, flags);
                return ast_post_increment_expression_new(context, expression->file_pos, base_copy);
                break;
            }

            case AST_EXPR_POST_DECREMENT:
            {
                auto base_copy = copy_expression(context, expression->base_expression, flags);
                return ast_post_decrement_expression_new(context, expression->file_pos, base_copy);
                break;
            }

            default: assert(false);
        }

		assert(false);
		return nullptr;
    }

    AST_Type_Spec* copy_type_spec(Context* context, AST_Type_Spec* type_spec,
                                  Copy_Flag flags /*=AST_SCOPE_FLAG_NONE*/)
    {
        assert(context);

        if (!type_spec) return nullptr;

        AST_Type_Spec* result = nullptr;

        switch (type_spec->kind)
        {
            case AST_TYPE_SPEC_IDENT:
            {
                auto ident_copy = copy_identifier(context, type_spec->identifier.identifier);
                BUF(AST_Type_Spec*) args_copy = nullptr;
                for (uint64_t i = 0; i < BUF_LENGTH(type_spec->identifier.arg_type_specs); i++)
                {
                    auto arg = type_spec->identifier.arg_type_specs[i];
                    auto arg_copy = copy_type_spec(context, arg);
                    BUF_PUSH(args_copy, arg_copy);
                }
                result = ast_type_spec_identifier_new(context, type_spec->file_pos, ident_copy,
                                                      args_copy);
                break;
            }

            case AST_TYPE_SPEC_DOT:
            {
                auto module_ident_copy = copy_identifier(context, type_spec->dot.module_ident);
                auto member_ts_copy = copy_type_spec(context, type_spec->dot.member_type_spec);
                result = ast_type_spec_dot_new(context, type_spec->file_pos, module_ident_copy,
                                               member_ts_copy);
                break;
            }

            case AST_TYPE_SPEC_POINTER:
            {
                auto base_copy = copy_type_spec(context, type_spec->pointer.base, flags);
                result = ast_type_spec_pointer_new(context, type_spec->file_pos, base_copy);
                break;
            }

            case AST_TYPE_SPEC_STATIC_ARRAY:
            {
                auto count_copy = copy_expression(context, type_spec->static_array.count_expr,
                                                  flags);
                auto base_copy = copy_type_spec(context, type_spec->static_array.base, flags);
                result = ast_type_spec_static_array_new(context, type_spec->file_pos, count_copy,
                                                        base_copy);
                break;
            }

            case AST_TYPE_SPEC_FUNCTION:
            {
                assert(false);
                break;
            }

            case AST_TYPE_SPEC_TYPEOF:
            {
                auto expr_copy = copy_expression(context, type_spec->typeof_expr.expr, flags);
                return ast_type_spec_typeof_new(context, type_spec->file_pos, expr_copy);
                break;
            }

            case AST_TYPE_SPEC_FROM_TYPE:
            {
                assert(false);
                break;
            }

            case AST_TYPE_SPEC_POLY_FUNC_ARG:
            {
                auto ident_copy = copy_identifier(context, type_spec->poly_func_arg.identifier,
                                                  flags);
                if (flags & COPY_FLAG_DONT_COPY_POLY)
                {
                    result = ast_type_spec_identifier_new(context, ident_copy->file_pos,
                                                          ident_copy, nullptr);
                }
                else
                {

                    result = ast_type_spec_poly_func_arg_new(context, type_spec->file_pos,
                                                             ident_copy);
                }
                break;
            }

            default: assert(false);
        }

        assert(result);

        return result;
    }

    AST_Identifier* copy_identifier(Context* context, AST_Identifier* identifier,
                                    Copy_Flag flags /*=AST_SCOPE_FLAG_NONE*/)
    {
        assert(context);
        assert(identifier);

        if (identifier->declaration)
        {
            // assert(false);
            // TODO: Assert decl is not poly???
        }

        return ast_identifier_new(context, identifier->atom, identifier->file_pos);
    }

    AST_Overload_Directive copy_overload_directive(Context* context, AST_Overload_Directive od,
                                                   Copy_Flag flags /*=AST_SCOPE_FLAG_NONE*/)
    {
        assert(context);

        AST_Overload_Directive result = {};
        result.op = od.op;
        result.identifier = copy_identifier(context, od.identifier, flags);

        return result;
    }

    AST_Scope* copy_scope(Context* context, AST_Scope* scope,
                          Copy_Flag flags /*=AST_SCOPE_FLAG_NONE*/)
    {
        assert(context);
        assert(scope);

        AST_Scope* result = ast_scope_new(context, scope->parent, scope->module, false,
                                          scope->line);
        result->flags = scope->flags;

        return result;
    }
}
