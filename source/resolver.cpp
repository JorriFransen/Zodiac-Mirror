#include "resolver.h"

#include "builtin.h"
#include "const_interpreter.h"
#include "ir.h"
#include "ir_runner.h"
#include "lexer.h"
#include "parser.h"

#include <stdarg.h>
#include <inttypes.h>

namespace Zodiac
{
    void resolver_init(Resolver* resolver, Context* context)
    {
        assert(resolver);
        assert(context);

        resolver->context = context;
        resolver->module = nullptr;
        resolver->current_break_context = nullptr;
        resolver->current_func_decl = nullptr;
        resolver->result = {};
    }

    void resolver_do_initial_scope_population(Resolver* resolver, AST_Module* module,
                                              AST_Scope* scope)
    {
        assert(resolver);
        assert(module);
        assert(scope);

        assert(scope->flags & AST_SCOPE_FLAG_IS_MODULE_SCOPE);

        for (uint64_t i = 0; i < BUF_LENGTH(module->global_declarations); i++)
        {
            AST_Declaration* global_decl = module->global_declarations[i];
            ast_scope_push_declaration(scope, global_decl);
        }

    }

    Resolve_Result resolver_resolve_module(Resolver* resolver, AST_Module* module)
    {
        assert(resolver);
        assert(module);

        auto old_module = resolver->module;

        resolver->module = module;

        resolver_do_initial_scope_population(resolver, module, module->module_scope);

        for (uint64_t i = 0; i < BUF_LENGTH(module->global_declarations); i++)
        {
            auto decl = module->global_declarations[i];
            if (decl->kind == AST_DECL_USING)
            {
                resolver_resolve_declaration(resolver, decl, module->module_scope);
            }
        }

        for (uint64_t i = 0; i < BUF_LENGTH(module->global_declarations); i++)
        {
            AST_Declaration* global_decl = module->global_declarations[i];
            bool res = resolver_resolve_declaration(resolver, global_decl, module->module_scope);
            if (global_decl->kind == AST_DECL_IMPORT && !res)
                resolver_report_error(resolver, global_decl->file_pos, "Import failed");
            if (!res) break;
        }

        resolver->module = old_module;
        return resolver->result;
    }

    bool resolver_resolve_declaration(Resolver* resolver, AST_Declaration* declaration,
                                      AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(scope);

        if (declaration->flags & AST_DECL_FLAG_RESOLVING)
        {
            if (declaration == resolver->current_func_decl)
            {
                return true;
            }

            if (!(declaration->flags & AST_DECL_FLAG_ERROR))
            {
                declaration->flags |= AST_DECL_FLAG_ERROR;

                const char* format = nullptr;
                if (declaration->identifier)
                {
                    format = "Circular dependency while trying to resolve declaration: %s";
                }
                else
                {
                    format = "Circular dependency while trying to resolve declaration";
                }
                resolver_report_error(resolver, declaration->file_pos, format,
                                      declaration->identifier->atom.data);
            }
            return false;
        }

        if (declaration->flags & AST_DECL_FLAG_RESOLVED)
        {
            return true;
        }

        declaration->flags |= AST_DECL_FLAG_RESOLVING;

        if (declaration->directive)
        {
            switch (declaration->directive->kind)
            {
                case AST_DIREC_FOREIGN:
                {
                    declaration->flags |= AST_DECL_FLAG_FOREIGN;
                    break;
                }

                default: assert(false);
            }
        }


        auto old_resolving_func = resolver->current_func_decl;
        if (declaration->kind == AST_DECL_FUNC)
        {
            resolver->current_func_decl = declaration;
        }

        bool result = true;

        switch (declaration->kind)
        {
            case AST_DECL_IMPORT:
            {
                auto at = resolver->context->atom_table;

                Atom module_name = declaration->import.module_identifier->atom;
                Atom module_file_name = atom_append(at, module_name, ".zdc");

                bool found = false;
                for (uint64_t i = 0; i < BUF_LENGTH(resolver->context->module_search_path); i++)
                {
                    Atom module_search_path = resolver->context->module_search_path[i];

                    Atom module_path = atom_append(at, module_search_path, module_file_name);

                    if (!file_exists(module_path.data))
                    {
                        found = false;
                        continue;
                    }
                    else
                    {
                        found = true;
                    }

                    // printf("module path: %s\n", module_path.data);
                    AST_Module* import_module = resolver_add_import_to_module(resolver,
                                                                              resolver->module,
                                                                              module_path,
                                                                              module_name);
                    if (!import_module)
                    {
                        result = false;
                        break;
                    }

                    declaration->import.module = import_module;

                    BUF_PUSH(resolver->module->import_decls, declaration);

                    result = true;
                    break;
                }


                if (!found)
                {
                    resolver_report_error(resolver, declaration->file_pos,
                                        "Failed to find module: %s",
                                        module_name.data);
                    result = false;
                }

                break;
            }

            case AST_DECL_STATIC_IF:
            {
                result &= resolver_resolve_static_if_declaration(resolver, declaration, scope);
                break;
            }

            case AST_DECL_BLOCK:
            {
                for (uint64_t i = 0; i < BUF_LENGTH(declaration->block.decls); i++)
                {
                    result &= resolver_resolve_declaration(resolver, declaration->block.decls[i],
                                                           scope);
                }
                break;
            }

            case AST_DECL_DYN_LINK:
            {
                // Nothing to do
                result = true;
                break;
            }

            case AST_DECL_FUNC:
            {
                assert(declaration->function.locals == nullptr);
                bool arg_result = true;
                for (uint64_t i = 0; i < BUF_LENGTH(declaration->function.args); i++)
                {
                    AST_Declaration* arg_decl = declaration->function.args[i];
                    if (!(arg_decl->flags & AST_DECL_FLAG_RESOLVED))
                    {
                        arg_result &=
                            resolver_resolve_declaration(resolver, arg_decl,
                                                         declaration->function.argument_scope);
                        ast_scope_push_declaration(declaration->function.argument_scope, arg_decl);
                    }
                }

                if (!arg_result)
                {
                    result = false;
                    break;
                }

                if (declaration->function.return_type_spec)
                {
                    result &= resolver_resolve_type_spec(resolver,
                                                         declaration->function.return_type_spec,
                                                         &declaration->function.return_type,
                                                         scope);
                    if (!result) break;
                }

                if (declaration->flags & AST_DECL_FLAG_FOREIGN)
                {
                    assert(!declaration->function.body_block);
                }
                else
                {
                    assert(declaration->function.body_block);
                    result &=
                        resolver_resolve_statement(resolver, declaration->function.body_block,
                                                   declaration->function.body_block->block.scope);
                    if (!result) break;
                }

                if (!declaration->function.return_type &&
                    declaration->function.inferred_return_type)
                {
                    declaration->function.return_type = declaration->function.inferred_return_type;
                }

                if (!declaration->function.return_type && !declaration->function.return_type_spec)
                {
                    declaration->function.return_type = Builtin::type_void;
                }

                assert(result);
                bool is_vararg = false;
                if (declaration->flags & AST_DECL_FLAG_FUNC_VARARG)
                {
                    is_vararg = true;
                }
                BUF(AST_Type*) arg_types = nullptr;
                for (uint64_t i = 0; i < BUF_LENGTH(declaration->function.args); i++)
                {
                    AST_Declaration* arg_decl = declaration->function.args[i];
                    AST_Type* type = resolver_get_declaration_type(arg_decl);
                    BUF_PUSH(arg_types, type);
                }
                AST_Type* return_type = declaration->function.return_type;
                declaration->function.type = ast_find_or_create_function_type(resolver->context,
                                                                              is_vararg,
                                                                              arg_types,
                                                                              return_type);
                break;
            }

            case AST_DECL_MUTABLE:
            {
                if (declaration->mutable_decl.type_spec)
                {
                    result &= resolver_resolve_type_spec(resolver,
                                                         declaration->mutable_decl.type_spec,
                                                         &declaration->mutable_decl.type, scope);
                    if (!result) break;
                }

                AST_Type* specified_type = declaration->mutable_decl.type;

                if (declaration->mutable_decl.init_expression)
                {
                    auto init_expr = declaration->mutable_decl.init_expression;

                    result &=
                        resolver_resolve_expression(resolver, init_expr, scope, specified_type);

                    if (result)
                    {
                        if (specified_type)
                        {
                            result &= resolver_check_assign_types(resolver,
                                                                  declaration->mutable_decl.type,
                                                                  init_expr->type);
                        }
                        else
                        {
                            declaration->mutable_decl.type = init_expr->type;
                        }
                    }
                }

                if (result && (declaration->location == AST_DECL_LOC_LOCAL))
                {
                    ast_scope_push_declaration(scope, declaration);
                }

                break;
            }

            case AST_DECL_CONSTANT_VAR:
            {
                if (declaration->constant_var.type_spec)
                {
                    result &= resolver_resolve_type_spec(resolver,
                                                         declaration->constant_var.type_spec,
                                                         &declaration->constant_var.type, scope);
                    if (!result) break;
                }


                auto init_expr = declaration->constant_var.init_expression;
                if (init_expr)
                {
                    result &= resolver_resolve_expression(resolver, init_expr, scope,
                                                          declaration->constant_var.type);
                }
                else if (scope->flags & AST_SCOPE_FLAG_IS_ENUM_SCOPE)
                {
                    result = false;
                    break;
                }
                else assert(false);

                if (!declaration->constant_var.type)
                {
                    declaration->constant_var.type =
                        declaration->constant_var.init_expression->type;
                }

                break;
            }

            case AST_DECL_TYPEDEF:
            {
                result &= resolver_resolve_type_spec(resolver, declaration->typedef_decl.type_spec,
                                                     &declaration->typedef_decl.type, scope);
                break;
            }

            case AST_DECL_AGGREGATE_TYPE:
            {
                AST_Aggregate_Declaration* agg_decl = declaration->aggregate_type.aggregate_decl;

                if (declaration->aggregate_type.kind == AST_AGG_DECL_STRUCT ||
                    declaration->aggregate_type.kind == AST_AGG_DECL_UNION)
                {
                     uint64_t total_size = 0;
                     uint64_t biggest_size = 0;
                     auto agg_scope = declaration->aggregate_type.scope;

                     bool all_members_resolved = true;

                     for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->members); i++)
                     {
                         AST_Declaration* member_decl = agg_decl->members[i];
                         if (member_decl->kind != AST_DECL_MUTABLE)
                         {
                             assert(member_decl->kind == AST_DECL_AGGREGATE_TYPE);
                             auto member_ptr = &agg_decl->members[i];
                             resolver_replace_aggregate_declaration_with_mutable(resolver,
                                                                                 member_ptr,
                                                                                 agg_scope);
                             member_decl = *member_ptr;
                         }
                         assert(member_decl->kind == AST_DECL_MUTABLE);
                         AST_Type_Spec* member_ts = member_decl->mutable_decl.type_spec;
                         AST_Type* member_type = nullptr;

                         if (member_ts->kind == AST_TYPE_SPEC_POINTER)
                         {
                             total_size += Builtin::pointer_size;
                             biggest_size = MAX(biggest_size, Builtin::pointer_size);

                             all_members_resolved = false;
                         }
                         else
                         {
                             bool member_res = resolver_resolve_declaration(resolver, member_decl,
                                                                            agg_scope);

                             if (member_res)
                             {
                                 AST_Type* member_type = resolver_get_declaration_type(member_decl);

                                 total_size += member_type->bit_size;
                                 biggest_size = MAX(biggest_size, member_type->bit_size);
                             }
                             else
                             {
                                 result = false;
                             }
                         }
                     }

                     if (!result) return false;

                     uint64_t bit_size = declaration->aggregate_type.kind == AST_AGG_DECL_STRUCT ?
                                                total_size : biggest_size;
                    const char* name = nullptr;
                    if (declaration->identifier)
                    {
                        name = declaration->identifier->atom.data;
                    }

                    AST_Type* agg_type = nullptr;
                    if (declaration->aggregate_type.kind == AST_AGG_DECL_STRUCT)
                    {
                        agg_type = ast_type_struct_new(resolver->context, agg_decl->members,
                                                       name, bit_size, agg_scope,
                                                       agg_decl->overload_directives);
                    }
                    else if (declaration->aggregate_type.kind == AST_AGG_DECL_UNION)
                    {
                        agg_type = ast_type_union_new(resolver->context, agg_decl->members,
                                                      name, bit_size, agg_scope,
                                                      agg_decl->overload_directives);
                    }
                    assert(agg_type);
                    declaration->aggregate_type.type = agg_type;

                    assert(result);
                    declaration->flags |= AST_DECL_FLAG_RESOLVED;
                    declaration->flags ^= AST_DECL_FLAG_RESOLVING;

                    if (!all_members_resolved)
                    {
                        for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->members); i++)
                        {
                            AST_Declaration* member_decl = agg_decl->members[i];
                            result &= resolver_resolve_declaration(resolver, member_decl,
                                                                   agg_scope);
                            if (!result) break;
                        }
                    }

                }
                else
                {
                    assert(declaration->aggregate_type.kind == AST_AGG_DECL_ENUM);

                    AST_Type* enum_type = nullptr;
                    AST_Type_Spec* enum_type_spec = declaration->aggregate_type.enum_type_spec;
                    if (enum_type_spec)
                    {
                        result &= resolver_resolve_type_spec(resolver, enum_type_spec, &enum_type,
                                                             scope);
                        if (!result) break;
                    }

                    if (!enum_type)
                    {
                        enum_type = Builtin::type_int;
                    }

                    assert(enum_type->flags & AST_TYPE_FLAG_INT);

                    auto enum_scope = declaration->aggregate_type.scope;
                    assert(enum_scope);

                    for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->members); i++)
                    {
                        AST_Declaration* member_decl = agg_decl->members[i];
                        assert(member_decl->kind == AST_DECL_CONSTANT_VAR);

                        ast_scope_push_declaration(enum_scope, member_decl);
                    }

                    bool all_resolved = false;
                    while (!all_resolved)
                    {
                        int64_t next_value = 0;
                        bool member_result = true;

                        for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->members); i++)
                        {
                            AST_Declaration* member_decl = agg_decl->members[i];
                            assert(member_decl->kind == AST_DECL_CONSTANT_VAR);

                            if (member_decl->constant_var.init_expression)
                            {
                                auto init_expr = member_decl->constant_var.init_expression;

                                if (resolver_resolve_expression(resolver, init_expr, enum_scope,
                                                                enum_type) &&
                                    resolver_resolve_declaration(resolver, member_decl, enum_scope))
                                {
                                }
                                else
                                {
                                    member_result = false;
                                    continue;
                                }


                                next_value = const_interpret_int_expression(resolver->context,
                                                                            init_expr,
                                                                            init_expr->type,
                                                                            enum_scope);

                                auto new_init_expr =
                                    ast_integer_literal_expression_new(resolver->context,
                                                                       init_expr->file_pos,
                                                                       next_value);
                                bool expr_res = resolver_resolve_expression(resolver,
                                                                            new_init_expr,
                                                                            enum_scope,
                                                                            enum_type);
                                assert(expr_res);

                                // LEAK: FIXME: TODO:
                                member_decl->constant_var.init_expression = new_init_expr;

                                member_decl->flags ^= AST_DECL_FLAG_RESOLVED;
                                result &= resolver_resolve_declaration(resolver, member_decl,
                                                                       enum_scope);
                            }
                            else
                            {
                                AST_Expression* init_expr =
                                    ast_integer_literal_expression_new(resolver->context,
                                                                       member_decl->file_pos,
                                                                       next_value);
                                member_result &= resolver_resolve_expression(resolver, init_expr,
                                                                             enum_scope, enum_type);
                                member_decl->constant_var.init_expression = init_expr;
                                member_result &= resolver_resolve_declaration(resolver,
                                                                              member_decl, scope);
                            }

                            next_value++;
                        }

                        if (member_result)
                        {
                            all_resolved = true;
                        }
                    }

                    auto enum_name = declaration->identifier->atom.data;
                    AST_Type* enum_member_type = ast_type_enum_new(resolver->context,
                                                                   agg_decl->members, enum_name,
                                                                   enum_type, enum_scope);
                    declaration->aggregate_type.type = enum_member_type;

                    for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->members); i++)
                    {
                        AST_Declaration* member_decl = agg_decl->members[i];
                        assert(member_decl->kind == AST_DECL_CONSTANT_VAR);
                        assert(member_decl->constant_var.type->flags & AST_TYPE_FLAG_INT);
                        assert(member_decl->constant_var.type->bit_size <=
                               enum_member_type->bit_size);

                        member_decl->constant_var.type = enum_member_type;
                        member_decl->constant_var.init_expression->type = enum_member_type;
                    }
                }

                if (result) assert(declaration->aggregate_type.type);
                break;
            }

            case AST_DECL_USING:
            {
                AST_Expression* ident_expr = declaration->using_decl.ident_expression;
                result &= resolver_resolve_expression(resolver, ident_expr, scope);

                if (!result) break;

                AST_Declaration* decl = nullptr;
                if (ident_expr->kind == AST_EXPR_IDENTIFIER)
                {
                    assert(ident_expr->identifier->declaration);
                    decl = ident_expr->identifier->declaration;
                }
                else if (ident_expr->kind == AST_EXPR_DOT)
                {
                    assert(ident_expr->dot.declaration);
                    decl = ident_expr->dot.declaration;
                }
                else assert(false);

                assert(decl);

                if (decl->kind == AST_DECL_IMPORT)
                {
                    assert(decl->import.module);
                    BUF_PUSH(scope->using_modules, decl->import.module);
                }
                else if (decl->kind == AST_DECL_AGGREGATE_TYPE)
                {
                    BUF_PUSH(scope->using_declarations, decl);
                }
                else assert(false);
                break;
            }

            case AST_DECL_INSERT:
            {
                assert(scope->flags & AST_SCOPE_FLAG_IS_MODULE_SCOPE);

                if (declaration->flags & AST_DECL_FLAG_INSERT_GENERATED)
                {
                    break;
                }

                AST_Statement* insert_stmt = declaration->insert_decl.call_statement;

                bool insert_res = resolver_resolve_statement(resolver, insert_stmt, scope);
                if (insert_res)
                {
                    assert(insert_stmt->kind == AST_STMT_CALL);
                    assert(insert_stmt->call_expression->type == Builtin::type_pointer_to_u8);
                    char* insert_string = run_insert(resolver, insert_stmt->call_expression);
                    assert(insert_string);

                    if (resolver->context->options.verbose)
                    {
                        printf("Insert string:\n%s\n", insert_string);
                    }

                    Lexer lexer;
                    init_lexer(&lexer, resolver->context);

                    Lex_Result lex_result = lex_file(&lexer, insert_string, "<insert_auto_gen>");
                    if (BUF_LENGTH(lex_result.errors) != 0)
                    {
                        lexer_report_errors(&lexer);
                        result = false;
                        break;
                    }

                    Parser parser;
                    parser_init(&parser, resolver->context);

                    parser.result.module_name = resolver->module->module_name;
                    parser.result.ast_module = resolver->module;
                    parser.tokens = lex_result.tokens;
                    parser.ti = 0;

                    BUF(AST_Declaration*) insert_decls = nullptr;

                    while (parser.ti < BUF_LENGTH(parser.tokens) && result)
                    {
                        AST_Declaration* insert_decl = parse_declaration(&parser, scope, true);

                        if (BUF_LENGTH(parser.result.errors) > 0)
                        {
                            result = false;
                            break;
                        }

                        BUF_PUSH(insert_decls, insert_decl);
                    }

                    if (BUF_LENGTH(parser.result.errors) > 0)
                    {
                        parser_report_errors(&parser);
                        result = false;
                        break;
                    }

                    for (uint64_t i = 0; i < BUF_LENGTH(insert_decls); i++)
                    {
                        AST_Declaration* insert_decl = insert_decls[i];
                        ast_scope_push_declaration(scope, insert_decl);
                    }

                    for (uint64_t i = 0; i < BUF_LENGTH(insert_decls); i++)
                    {
                        AST_Declaration* insert_decl = insert_decls[i];
                        if (insert_decl->kind == AST_DECL_USING)
                        {
                            resolver_resolve_declaration(resolver, insert_decl, scope);
                        }
                    }

                    for (uint64_t i = 0; i < BUF_LENGTH(insert_decls); i++)
                    {
                        AST_Declaration* insert_decl = insert_decls[i];
                        bool res = resolver_resolve_declaration(resolver, insert_decl, scope);
                        if (insert_decl->kind == AST_DECL_IMPORT && !res)
                        {
                            resolver_report_error(resolver, insert_decl->file_pos, "Import failed");
                        }

                        if (!res)
                        {
                            result = false;
                            break;
                        } else
                        {
                            BUF_PUSH(resolver->module->global_declarations, insert_decl);
                        }
                    }

                    if (result)
                    {
                        declaration->flags |= AST_DECL_FLAG_INSERT_GENERATED;
                    }
                }
                else
                {
                    resolver_report_error(resolver, insert_stmt->file_pos,
                                          "Failed to resolve insert call");
                    result = false;
                    break;
                }
                break;
            }

            default: assert(false);
        }

        if (declaration->kind == AST_DECL_FUNC)
        {
            resolver->current_func_decl = old_resolving_func;
        }

        declaration->flags &= ~AST_DECL_FLAG_RESOLVING;
        if (result)
        {
            declaration->flags |= AST_DECL_FLAG_RESOLVED;
        }

        return result;
    }

    bool resolver_resolve_statement(Resolver* resolver, AST_Statement* statement, AST_Scope* scope)
    {
        assert(resolver);
        assert(statement);
        assert(scope);

        bool result = true;

        switch (statement->kind)
        {
            case AST_STMT_BLOCK:
            {
                for (uint64_t i = 0; i < BUF_LENGTH(statement->block.statements); i++)
                {
                    AST_Statement* stmt = statement->block.statements[i];
                    result &= resolver_resolve_statement(resolver, stmt, statement->block.scope);
                }
                break;
            }

            case AST_STMT_CALL:
            {
                result &= resolver_resolve_expression(resolver, statement->call_expression, scope);
                break;
            }

            case AST_STMT_ASSIGN:
            {
                result &= resolver_resolve_expression(resolver,
                                                      statement->assign.lvalue_expression, scope);
                AST_Type* suggested_type = nullptr;
                if (result)
                {
                    suggested_type = statement->assign.lvalue_expression->type;
                }
                result &= resolver_resolve_expression(resolver, statement->assign.expression,
                                                      scope, suggested_type);

                if (!result) break;

                bool types_match =
                    resolver_check_assign_types(resolver,
                                                statement->assign.lvalue_expression->type,
                                                statement->assign.expression->type);

                if (!types_match)
                {
                    auto lvalue = statement->assign.lvalue_expression;
                    auto expr = statement->assign.expression;
                    types_match = true;

                    if ((lvalue->type->flags & AST_TYPE_FLAG_FLOAT) &&
                        (expr->type->flags & AST_TYPE_FLAG_INT))
                    {
                        resolver_transform_to_cast_expression(resolver, expr, lvalue->type);
                    }
                    else
                    {
                        types_match = false;
                    }
                }

                if (!types_match)
                {
                    result = false;
                    auto expected_str =
                        ast_type_to_string(statement->assign.lvalue_expression->type);
                    auto got_str = ast_type_to_string(statement->assign.expression->type);
                    resolver_report_error(resolver, statement->file_pos,
                                          "Mismatching types in assignement statement\n\tExpected: %s\n\tGot: %s",
                                          expected_str, got_str);
                    mem_free(expected_str);
                    mem_free(got_str);
                }
                break;
            }

            case AST_STMT_RETURN:
            {
                if (statement->return_expression)
                {
                    result &= resolver_resolve_expression(resolver, statement->return_expression,
                                                          scope);

                    if (result)
                    {
                        auto current_func = resolver->current_func_decl;
                        assert(current_func);

                        if (current_func->function.inferred_return_type)
                        {
                            assert(current_func->function.inferred_return_type ==
                                   statement->return_expression->type);
                        }
                        else
                        {
                            current_func->function.inferred_return_type =
                                statement->return_expression->type;
                        }
                    }
                }
                break;
            }

            case AST_STMT_DECLARATION:
            {
                result &= resolver_resolve_declaration(resolver, statement->declaration, scope);
                break;
            }

            case AST_STMT_ASSERT:
            {
                AST_Expression* assert_expr = statement->assert_expression;
                result &= resolver_resolve_expression(resolver, assert_expr, scope);

                if (!result) return false;

                assert(assert_expr->type == Builtin::type_bool);
                break;
            }

            case AST_STMT_IF:
            {
                auto if_expr = statement->if_stmt.if_expression;
                result &= resolver_resolve_expression(resolver, if_expr,
                                                      scope);
                if (!result) return false;
                assert(if_expr->type == Builtin::type_bool ||
                       if_expr->type->kind == AST_TYPE_POINTER ||
                       (if_expr->type->flags & AST_TYPE_FLAG_INT));
                result &= resolver_resolve_statement(resolver, statement->if_stmt.then_statement,
                                                     scope);
                if (!result) return false;

                if (statement->if_stmt.else_statement)
                {
                    result &= resolver_resolve_statement(resolver,
                                                         statement->if_stmt.else_statement, scope);
                }
                break;
            }

            case AST_STMT_WHILE:
            {
                auto cond_expr = statement->while_stmt.cond_expr;
                result &= resolver_resolve_expression(resolver, cond_expr, scope);
                if (!result) return false;

                assert(cond_expr->type == Builtin::type_bool ||
                       cond_expr->type->kind == AST_TYPE_POINTER ||
                       (cond_expr->type->flags & AST_TYPE_FLAG_INT));

                auto old_break_context = resolver->current_break_context;
                resolver->current_break_context = statement;
                result &= resolver_resolve_statement(resolver, statement->while_stmt.body_stmt,
                                                     scope);
                resolver->current_break_context = old_break_context;
                break;
            }

            case AST_STMT_FOR:
            {
                result &= resolver_resolve_statement(resolver, statement->for_stmt.init_stmt,
                                                     statement->for_stmt.scope);
                if (!result) break;

                result &= resolver_resolve_expression(resolver, statement->for_stmt.cond_expr,
                                                      statement->for_stmt.scope);
                result &= resolver_resolve_statement(resolver, statement->for_stmt.step_stmt,
                                                     statement->for_stmt.scope);

                auto old_break_context = resolver->current_break_context;
                resolver->current_break_context = statement;
                result &= resolver_resolve_statement(resolver, statement->for_stmt.body_stmt,
                                                     statement->for_stmt.scope);
                resolver->current_break_context = old_break_context;
                break;
            }

            case AST_STMT_BREAK:
            {
                assert(resolver->current_break_context);
                assert(resolver->current_break_context->kind == AST_STMT_WHILE ||
                       resolver->current_break_context->kind == AST_STMT_FOR);
                break;
            }

            case AST_STMT_DEFER:
            {
                result &= resolver_resolve_statement(resolver, statement->defer_statement, scope);
                if (!result) break;

                result &= defer_statement_is_legal(resolver, statement->defer_statement);
                if (!result) break;

                BUF_PUSH(scope->defer_statements, statement->defer_statement);

                break;
            }

            case AST_STMT_SWITCH:
            {
                AST_Expression* switch_expr = statement->switch_stmt.switch_expression;
                result &= resolver_resolve_expression(resolver, switch_expr, scope);
                if (!result) break;

                AST_Type* switch_type = switch_expr->type;
                assert(switch_type->flags & AST_TYPE_FLAG_INT ||
                       ((switch_type->kind == AST_TYPE_ENUM) &&
                        (switch_type->aggregate_type.base_type->flags & AST_TYPE_FLAG_INT)));

                bool found_default = false;

                auto cases = statement->switch_stmt.cases;
                for (uint64_t i = 0; i < BUF_LENGTH(cases); i++)
                {
                    const AST_Switch_Case& switch_case = cases[i];
                    if (switch_case.is_default)
                    {
                        assert(!found_default);
                        found_default = true;
                    }
                    else
                    {
                        for (uint64_t i = 0; i < BUF_LENGTH(switch_case.case_expressions); i++)
                        {
                            AST_Expression* case_expr = switch_case.case_expressions[i];
                            result &= resolver_resolve_expression(resolver, case_expr, scope,
                                                                  switch_expr->type);
                            if (result)
                            {
                                if (!(case_expr->flags & AST_EXPR_FLAG_CONST))
                                {
                                    resolver_report_error(resolver, case_expr->file_pos,
                                                          "Case expression must be const");
                                }
                            }
                        }

                        for (uint64_t i = 0; i < BUF_LENGTH(switch_case.range_expressions); i += 2)
                        {
                            AST_Expression* min = switch_case.range_expressions[i];
                            AST_Expression* max = switch_case.range_expressions[i + 1];

                            result &= resolver_resolve_expression(resolver, min, scope);
                            result &= resolver_resolve_expression(resolver, max, scope);

                            if (result)
                            {
                                assert(min->flags & AST_EXPR_FLAG_CONST);
                                assert(max->flags & AST_EXPR_FLAG_CONST);
                                assert(min->type == max->type);
                            }
                        }
                    }

                    auto old_break_context = resolver->current_break_context;
                    resolver->current_break_context = statement;
                    result &= resolver_resolve_statement(resolver, switch_case.stmt, scope);
                    resolver->current_break_context = old_break_context;
                }
                break;
            }

            case AST_STMT_INSERT:
            {
                AST_Statement* insert_stmt = statement->insert.statement;

                bool insert_res = resolver_resolve_statement(resolver, insert_stmt, scope);

                if (insert_res)
                {
                    assert(insert_stmt->kind == AST_STMT_CALL);
                    assert(insert_stmt->call_expression->type == Builtin::type_pointer_to_u8);

                    char* insert_string = run_insert(resolver, insert_stmt->call_expression);
                    assert(insert_string);

                    Lexer lexer;
                    init_lexer(&lexer, resolver->context);

                    Lex_Result lex_result = lex_file(&lexer, insert_string, "<insert_auto_gen>");
                    if (BUF_LENGTH(lex_result.errors) != 0)
                    {
                        lexer_report_errors(&lexer);
                        result = false;
                        break;
                    }

                    Parser parser;
                    parser_init(&parser, resolver->context);

                    parser.result.module_name = resolver->module->module_name;
                    parser.result.ast_module = resolver->module;
                    parser.tokens = lex_result.tokens;
                    parser.ti = 0;

                    AST_Statement* gen_stmt = parse_statement(&parser, scope);
                    Parse_Result parse_result = parser.result;
                    if (BUF_LENGTH(parse_result.errors) != 0)
                    {
                        parser_report_errors(&parser);
                        result = false;
                        break;
                    }

                    result &= resolver_resolve_statement(resolver, gen_stmt, scope);
                    if (result)
                    {
                        statement->insert.gen_statement = gen_stmt;
                    }
                }
                else
                {
                    result = false;
                }
                break;
            }

            case AST_STMT_POST_INCREMENT:
            {
                result &= resolver_resolve_expression(resolver, statement->post_increment, scope);
                break;
            }

            default: assert(false);
        }

        return result;
    }

    bool resolver_resolve_expression(Resolver* resolver, AST_Expression* expression,
                                     AST_Scope* scope, AST_Type* suggested_type /*=nullptr*/)
    {
        assert(resolver);
        assert(expression);
        assert(scope);

        if (expression->flags & AST_EXPR_FLAG_RESOLVED)
        {
            return true;
        }

        bool result = true;
        bool points_to_import_decl = false;

        switch (expression->kind)
        {
            case AST_EXPR_CALL:
            {
                AST_Expression* ident_expr = expression->call.ident_expression;
                bool recursive = false;

                if (ident_expr->kind == AST_EXPR_IDENTIFIER)
                {
                    AST_Declaration* decl = find_declaration(resolver->context, scope,
                                                             ident_expr->identifier);
                    if (decl == resolver->current_func_decl)
                    {
                        decl->function.type = resolver_create_recursive_function_type(resolver,
                                                                                      decl);
                        recursive = true;
                    }
                }

                result &= resolver_resolve_expression(resolver, ident_expr, scope);
                if (!result) break;

                AST_Declaration* func_decl = nullptr;

                if (ident_expr->kind == AST_EXPR_IDENTIFIER)
                {
                    AST_Identifier* ident = expression->call.ident_expression->identifier;
                    func_decl = ident->declaration;
                }
                else if (ident_expr->kind == AST_EXPR_DOT)
                {
                    func_decl = ident_expr->dot.declaration;
                }
                assert(func_decl);

                AST_Type* func_type = nullptr;

                if (func_decl->kind == AST_DECL_FUNC)
                {
                    func_type = func_decl->function.type;
                }
                else if (func_decl->kind == AST_DECL_MUTABLE &&
                         func_decl->mutable_decl.type->kind == AST_TYPE_POINTER &&
                         func_decl->mutable_decl.type->pointer.base->kind == AST_TYPE_FUNCTION)
                {
                    func_type = func_decl->mutable_decl.type->pointer.base;
                }

                assert(func_type);

                expression->call.callee_declaration = func_decl;

                auto arg_expr_count = BUF_LENGTH(expression->call.arg_expressions);
                auto arg_decl_count = BUF_LENGTH(func_type->function.arg_types);

                if (func_decl->flags & AST_DECL_FLAG_FUNC_VARARG)
                {
                    assert(arg_expr_count >= arg_decl_count);
                }
                else
                {
                    assert(arg_expr_count == arg_decl_count);
                }

                for (uint64_t i = 0; i < arg_expr_count; i++)
                {
                    AST_Expression* arg_expr = expression->call.arg_expressions[i];
                    AST_Declaration* arg_decl = nullptr;
                    AST_Type* arg_type = nullptr;

                    if (i < arg_decl_count)
                    {
                        arg_type = func_type->function.arg_types[i];
                    }

                    result &= resolver_resolve_expression(resolver, arg_expr, scope, arg_type);
                    if (!result) break;

                    if (arg_type)
                    {
                        bool valid_conversion = resolver_check_assign_types(resolver, arg_type,
                                                                            arg_expr->type);

                        if (valid_conversion && arg_type == Builtin::type_String &&
                            arg_expr->type == Builtin::type_pointer_to_u8)
                        {
                            resolver_convert_to_builtin_string(resolver, arg_expr);
                        }
                        else if (valid_conversion && arg_type != arg_expr->type)
                        {
                            resolver_transform_to_cast_expression(resolver, arg_expr, arg_type);
                        }

                        if (!valid_conversion && arg_type != arg_expr->type)
                        {
                            result = false;

                            auto arg_type_str = ast_type_to_string(arg_type);
                            auto expr_type_str = ast_type_to_string(arg_expr->type);

                            resolver_report_error(resolver, arg_expr->file_pos,
                                                  "Mismatching types for argument %lu\n\tExpected: %s\n\tGot: %s",
                                                  i, arg_type_str, expr_type_str);
                            mem_free(arg_type_str);
                            mem_free(expr_type_str);
                        }
                    }
                }

                assert(func_type->function.return_type);
                expression->type = func_type->function.return_type;

                break;
            }

            case AST_EXPR_IDENTIFIER:
            {
                result &= resolver_resolve_identifier(resolver, expression->identifier, scope);

                if (!result) break;

                AST_Declaration* decl = expression->identifier->declaration;
                assert(decl);

                if (decl->kind == AST_DECL_IMPORT)
                {
                    points_to_import_decl = true;
                }

                if (decl->kind == AST_DECL_CONSTANT_VAR)
                {
                    expression->flags |= AST_EXPR_FLAG_CONST;
                }

                expression->type = resolver_get_declaration_type(decl);

                if (suggested_type && suggested_type != expression->type &&
                    resolver_check_assign_types(resolver, suggested_type, expression->type))
                {
                    if (suggested_type == Builtin::type_String &&
                        expression->type == Builtin::type_pointer_to_u8)
                    {
                        resolver_convert_to_builtin_string(resolver, expression);
                    }
                    else
                    {
                        resolver_transform_to_cast_expression(resolver, expression,
                                                              suggested_type);
                    }
                }

                break;
            }

            case AST_EXPR_INTEGER_LITERAL:
            {
                if (suggested_type)
                {
                    if (suggested_type->flags & AST_TYPE_FLAG_INT ||
                        suggested_type->kind == AST_TYPE_ENUM)
                    {
                    }
                    else if (suggested_type->flags & AST_TYPE_FLAG_FLOAT)
                    {
                        uint64_t int_value = expression->integer_literal.u64;
                        expression->kind = AST_EXPR_FLOAT_LITERAL;
                        expression->float_literal.r32 = (float)int_value;
                        expression->float_literal.r64 = (double)int_value;
                    }
                    else if (suggested_type->kind == AST_TYPE_POINTER)
                    {
                        suggested_type = Builtin::type_int;
                    }
                    else assert(false);

                    expression->type = suggested_type;
                }
                else
                {
                    expression->type = Builtin::type_int;
                }

                expression->flags |= AST_EXPR_FLAG_LITERAL;
                break;
            }

            case AST_EXPR_FLOAT_LITERAL:
            {
                if (suggested_type && (suggested_type->flags & AST_TYPE_FLAG_FLOAT))
                {
                    expression->type = suggested_type;
                }
                else
                {
                    expression->type = Builtin::type_float;
                }

                expression->flags |= AST_EXPR_FLAG_CONST;
                break;
            }

            case AST_EXPR_BOOL_LITERAL:
            {
                expression->type = Builtin::type_bool;
                expression->flags |= AST_EXPR_FLAG_CONST;
                break;
            }

            case AST_EXPR_BINARY:
            {
                result &= resolver_resolve_binary_expression(resolver, expression, scope);
                break;
            }

            case AST_EXPR_UNARY:
            {
                result &= resolver_resolve_expression(resolver, expression->unary.operand,
                                                      scope);
                if (!result) break;

                AST_Type* operand_type = expression->unary.operand->type;

                bool inherit_const = false;

                switch (expression->unary.op)
                {
                    case AST_UNOP_MINUS:
                    {
                        expression->type = operand_type;
                        inherit_const = true;
                        break;
                    }

                    case AST_UNOP_ADDROF:
                    {
                        expression->type = ast_find_or_create_pointer_type(resolver->context,
                                                                           operand_type);
                        break;
                    }

                    case AST_UNOP_DEREF:
                    {
                        assert(operand_type->kind == AST_TYPE_POINTER);
                        expression->type = operand_type->pointer.base;
                        break;
                    }

                    case AST_UNOP_NOT:
                    {
                        expression->type = operand_type;
                        inherit_const = true;
                        break;
                    }

                    default: assert(false);
                }

                if (inherit_const && (expression->unary.operand->flags & AST_EXPR_FLAG_CONST))
                {
                    expression->flags |= AST_EXPR_FLAG_CONST;
                }

                break;
            }

            case AST_EXPR_CAST:
            {
                result &= resolver_resolve_type_spec(resolver, expression->cast_expr.type_spec,
                                                     &expression->type, scope);
                if (!result) break;

                result &= resolver_resolve_expression(resolver, expression->cast_expr.expr, scope);
                break;
            }

            case AST_EXPR_SUBSCRIPT:
            {
                result &=
                    resolver_resolve_expression(resolver, expression->subscript.base_expression,
                                                scope);
                if (!result) break;

                result &= resolver_resolve_expression(resolver,
                                                      expression->subscript.index_expression,
                                                      scope);

                if (!result) break;

                AST_Type* base_type = expression->subscript.base_expression->type;
                AST_Type* index_type = expression->subscript.index_expression->type;

                AST_Identifier* index_overload_ident = find_overload(base_type,
                                                                     AST_OVERLOAD_OP_INDEX);

                assert(base_type->kind == AST_TYPE_STATIC_ARRAY ||
                       base_type->kind == AST_TYPE_POINTER ||
                       index_overload_ident);


                if (index_overload_ident)
                {
                    auto lvalue_expr = expression->subscript.base_expression;

                    result &= resolver_resolve_identifier(resolver, index_overload_ident, scope);
                    if (!result) break;

                    auto overload_decl = index_overload_ident->declaration;
                    assert(overload_decl->flags & AST_DECL_FLAG_RESOLVED);
                    assert(overload_decl->kind == AST_DECL_FUNC);
                    assert(BUF_LENGTH(overload_decl->function.args) == 2);

                    auto index_expr = expression->subscript.index_expression;
                    auto index_arg_decl = overload_decl->function.args[1];
                    assert(index_arg_decl->kind == AST_DECL_MUTABLE);
                    auto index_arg_type = index_arg_decl->mutable_decl.type;
                    if (index_type != index_arg_type)
                    {
                        assert(index_type->flags & AST_TYPE_FLAG_INT);
                        assert(index_arg_type->flags & AST_TYPE_FLAG_INT);

                        resolver_transform_to_cast_expression(resolver, index_expr, index_arg_type);
                    }

                    BUF(AST_Expression*) args = nullptr;
                    BUF_PUSH(args, lvalue_expr);
                    BUF_PUSH(args, index_expr);

                    auto overload_ident_expr = ast_ident_expression_new(resolver->context,
                                                                        lvalue_expr->file_pos,
                                                                        index_overload_ident);
                    auto call_expression = ast_call_expression_new(resolver->context,
                                                                   expression->file_pos,
                                                                   overload_ident_expr, args);

                    result &= resolver_resolve_expression(resolver, call_expression, scope);
                    if (!result) break;

                    auto callee_decl = call_expression->call.callee_declaration;
                    assert(callee_decl->flags & AST_DECL_FLAG_RESOLVED);

                    expression->type = callee_decl->function.return_type;
                    expression->subscript.call_expression = call_expression;

                }
                else if (base_type->kind == AST_TYPE_STATIC_ARRAY)
                {
                    assert(base_type->static_array.base);
                    expression->type = base_type->static_array.base;
                }
                else if (base_type->kind == AST_TYPE_POINTER)
                {
                    assert(base_type->pointer.base);
                    expression->type = base_type->pointer.base;
                }
                else assert(false);

                assert(index_type->flags & AST_TYPE_FLAG_INT ||
                       index_type->kind == AST_TYPE_ENUM);
                break;
            }

            case AST_EXPR_DOT:
            {
                result &= resolver_resolve_dot_expression(resolver, expression, scope);
                break;
            }

            case AST_EXPR_NULL_LITERAL:
            {
                if (suggested_type)
                {
                    assert(suggested_type->kind == AST_TYPE_POINTER);
                    expression->type = suggested_type;
                }
                else
                {
                    expression->type = Builtin::type_pointer_to_void;
                }
                break;
            }

            case AST_EXPR_STRING_LITERAL:
            {
                expression->type = Builtin::type_pointer_to_u8;
                break;
            }

            case AST_EXPR_CHAR_LITERAL:
            {
                expression->type = Builtin::type_u8;
                break;
            }

            case AST_EXPR_SIZEOF:
            {
                AST_Type* type = nullptr;
                result &= resolver_resolve_type_spec(resolver, expression->sizeof_expr.type_spec,
                                                     &type, scope);
                if (!result) break;

                assert(type);
                expression->sizeof_expr.byte_size = type->bit_size / 8;
                expression->type = Builtin::type_u64;
                break;
            }

            case AST_EXPR_COMPOUND_LITERAL:
            {
                assert(suggested_type);

                if (suggested_type->kind == AST_TYPE_STRUCT)
                {
                    assert(BUF_LENGTH(suggested_type->aggregate_type.member_declarations) ==
                           BUF_LENGTH(expression->compound_literal.expressions));

                    auto member_decls = suggested_type->aggregate_type.member_declarations;
                    auto compound_exprs = expression->compound_literal.expressions;

                    for (uint64_t i = 0; i < BUF_LENGTH(member_decls); i++)
                    {
                        AST_Declaration* member_decl = member_decls[i];
                        AST_Expression* compound_expr = compound_exprs[i];

                        assert(member_decl->flags & AST_DECL_FLAG_RESOLVED);

                        AST_Type* member_type = resolver_get_declaration_type(member_decl);

                        result &= resolver_resolve_expression(resolver, compound_expr, scope,
                                                              member_type);

                        if (result && compound_expr->type != member_type)
                        {
                            result = false;
                            auto got_str = ast_type_to_string(compound_expr->type);
                            auto expected_str = ast_type_to_string(member_type);
                            resolver_report_error(resolver, compound_expr->file_pos,
                                                  "Mismatching types in compound literal\n\tExpected: %s\n\tGot: %s",
                                                  expected_str, got_str);
                            mem_free(got_str);
                            mem_free(expected_str);
                        }
                    }

                    if (!result) break;

                    expression->type = suggested_type;
                }
                else if (suggested_type->kind == AST_TYPE_STATIC_ARRAY)
                {
                    assert(suggested_type->static_array.count ==
                           BUF_LENGTH(expression->compound_literal.expressions));

                    for (uint64_t i = 0; i < BUF_LENGTH(expression->compound_literal.expressions);
                         i++)
                    {
                        AST_Expression* member_expr = expression->compound_literal.expressions[i];
                        AST_Type* suggested_member_type = nullptr;
                        if (suggested_type && suggested_type->kind == AST_TYPE_STRUCT)
                        {
                            auto member_decls = suggested_type->aggregate_type.member_declarations;
                            assert(i < BUF_LENGTH(member_decls));
                            AST_Declaration* member_decl = member_decls[i];

                            assert(member_decl->kind == AST_DECL_MUTABLE);
                            assert(member_decl->mutable_decl.type);
                            suggested_member_type = member_decl->mutable_decl.type;
                        }
                        else if (suggested_type && suggested_type->kind == AST_TYPE_STATIC_ARRAY)
                        {
                            suggested_member_type = suggested_type->static_array.base;
                        }

                        result &= resolver_resolve_expression(resolver, member_expr, scope,
                                                              suggested_member_type);

                        if (result)
                        {
                            assert(member_expr->type == suggested_member_type);
                            expression->type = suggested_type;
                        }
                    }
                }
                else assert(false);
                break;
            }

            case AST_EXPR_ARRAY_LENGTH:
            {
                result &= resolver_resolve_expression(resolver,
                                                      expression->array_length.ident_expr,
                                                      scope);
                if (result)
                {
                    expression->type = Builtin::type_int;
                }
                break;
            }

            case AST_EXPR_POST_INCREMENT:
            case AST_EXPR_POST_DECREMENT:
            {
                result &= resolver_resolve_expression(resolver, expression->base_expression,
                                                      scope);
                if (result)
                {
                    expression->type = expression->base_expression->type;
                }
                break;
            }

            default: assert(false);
        }

        if (result)
        {
            assert(expression->type || points_to_import_decl);
            expression->flags |= AST_EXPR_FLAG_RESOLVED;
        }
        return result;
    }

    bool resolver_resolve_binary_expression(Resolver* resolver, AST_Expression* expression,
                                            AST_Scope* scope)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_BINARY);
        assert(scope);

        bool result = true;

        AST_Expression* lhs = expression->binary.lhs;
        AST_Expression* rhs = expression->binary.rhs;

        if ((lhs->flags & AST_EXPR_FLAG_LITERAL) && !(rhs->flags & AST_EXPR_FLAG_LITERAL))
        {
            result &= resolver_resolve_expression(resolver, rhs, scope);
            if (!result) return false;
            result &= resolver_resolve_expression(resolver, lhs, scope, rhs->type);
        }
        else if (!(lhs->flags & AST_EXPR_FLAG_LITERAL) && (rhs->flags & AST_EXPR_FLAG_LITERAL))
        {
            result &= resolver_resolve_expression(resolver, lhs, scope);
            if (!result) return false;
            result &= resolver_resolve_expression(resolver, rhs, scope, lhs->type);
        }
        else
        {
            result &= resolver_resolve_expression(resolver, lhs, scope);
            result &= resolver_resolve_expression(resolver, rhs, scope);
        }

        if (!result) return false;

        AST_Overload_Operator_Kind overload_op = binary_op_to_overload_op(expression->binary.op);
        if (overload_op != AST_OVERLOAD_OP_INVALID)
        {
            AST_Identifier* overload_ident = find_overload(lhs->type, overload_op);
            if (overload_ident)
            {
                result &= resolver_resolve_identifier(resolver, overload_ident, scope);
                if (!result) return false;
                assert(overload_ident->declaration);
                auto overload_decl = overload_ident->declaration;
                assert(overload_decl->flags & AST_DECL_FLAG_RESOLVED);
                assert(overload_decl->kind == AST_DECL_FUNC);
                assert(BUF_LENGTH(overload_decl->function.args) == 2);

                BUF(AST_Expression*) args = nullptr;
                BUF_PUSH(args, lhs);
                BUF_PUSH(args, rhs);

                auto overload_ident_expr = ast_ident_expression_new(resolver->context,
                                                                    expression->file_pos,
                                                                    overload_ident);

                auto call_expression = ast_call_expression_new(resolver->context,
                                                               expression->file_pos,
                                                               overload_ident_expr, args);

                result &= resolver_resolve_expression(resolver, call_expression, scope);
                if (!result) return false;

                assert(call_expression->call.callee_declaration);
                auto callee_decl = call_expression->call.callee_declaration;
                assert(callee_decl->flags & AST_DECL_FLAG_RESOLVED);

                expression->type = callee_decl->function.return_type;
                expression->binary.call_expression = call_expression;
                return result;
            }
        }

        if (lhs->type != rhs->type)
        {
            if ((lhs->type->flags & AST_TYPE_FLAG_INT) &&
                (rhs->type->flags & AST_TYPE_FLAG_FLOAT))
            {
                resolver_transform_to_cast_expression(resolver, lhs, rhs->type);
            }
            else if ((lhs->type->flags & AST_TYPE_FLAG_FLOAT) &&
                        (rhs->type->flags & AST_TYPE_FLAG_INT))
            {
                resolver_transform_to_cast_expression(resolver, rhs, lhs->type);
            }
            else if ((lhs->type->flags & AST_TYPE_FLAG_INT) &&
                     rhs->type == Builtin::type_bool)
            {
                resolver_transform_to_cast_expression(resolver, lhs, rhs->type);
            }
            else if (lhs->type->kind == AST_TYPE_POINTER &&
                     (rhs->type->flags & AST_TYPE_FLAG_INT))
            {
                expression->flags |= AST_EXPR_FLAG_POINTER_MATH;
                expression->type = lhs->type;
            }
            else if ((lhs->type->flags & AST_TYPE_FLAG_INT) &&
                     rhs->type->kind == AST_TYPE_POINTER)
            {
                expression->flags |= AST_EXPR_FLAG_POINTER_MATH;
                expression->type = rhs->type;
            }
            else assert(false);
        }

        if (!((lhs->type->flags & AST_TYPE_FLAG_INT) ||
              (lhs->type->flags & AST_TYPE_FLAG_FLOAT) ||
              lhs->type->kind == AST_TYPE_ENUM ||
              lhs->type == Builtin::type_bool ||
              (expression->flags & AST_EXPR_FLAG_POINTER_MATH)))
        {
            resolver_report_error(resolver, expression->file_pos,
                                  "Binary expression is not of numeric type, and no overload was found");
            return false;
        }

        if (!expression->type)
        {
            if (binop_is_cmp(expression))
            {
                expression->type = Builtin::type_bool;
            }
            else
            {
                expression->type = lhs->type;
            }
        }
        else
        {
            assert(expression->type->kind == AST_TYPE_POINTER);
            assert(expression->flags & AST_EXPR_FLAG_POINTER_MATH);
        }

        return result;
    }

    bool resolver_resolve_dot_expression(Resolver* resolver, AST_Expression* dot_expr,
                                         AST_Scope* scope)
    {
        assert(resolver);
        assert(dot_expr);
        assert(dot_expr->kind == AST_EXPR_DOT);
        assert(scope);

        assert(dot_expr->dot.declaration == nullptr);
        bool result = true;

        AST_Expression* base_expr = dot_expr->dot.base_expression;
        AST_Expression* member_expr = dot_expr->dot.member_expression;

        result &= resolver_resolve_expression(resolver, base_expr, scope);
        if (!result) return false;

        assert(member_expr->kind == AST_EXPR_IDENTIFIER);

        AST_Declaration* base_decl = nullptr;
        if (base_expr->kind == AST_EXPR_IDENTIFIER)
        {
            base_decl = base_expr->identifier->declaration;
        }
        else if (base_expr->kind == AST_EXPR_DOT)
        {
            base_decl = base_expr->dot.declaration;
        }
        else assert(false);
        assert(base_decl);

        if (base_decl->kind == AST_DECL_MUTABLE)
        {
            AST_Type* base_type = resolver_get_declaration_type(base_decl);

            if (base_type->kind == AST_TYPE_POINTER &&
                base_type->pointer.base->kind == AST_TYPE_STRUCT) {
                base_type = base_type->pointer.base;
            }

            if (base_type->kind == AST_TYPE_STRUCT || base_type->kind == AST_TYPE_UNION)
            {
                auto member_decls = base_type->aggregate_type.member_declarations;
                bool found = false;
                for (uint64_t i = 0; i < BUF_LENGTH(member_decls); i++)
                {
                    AST_Declaration* member_decl = member_decls[i];
                    assert(member_decl->flags & AST_DECL_FLAG_RESOLVED);
                    assert(member_decl->kind == AST_DECL_MUTABLE);

                    if (member_decl->identifier)
                    {
                        if (member_decl->identifier->atom == member_expr->identifier->atom)
                        {
                            member_expr->type = resolver_get_declaration_type(member_decl);
                            dot_expr->dot.declaration = member_decl;
                            found = true;
                            break;
                        }
                    }
                }

                if (!found)
                {
                    for (uint64_t i = 0; i < BUF_LENGTH(member_decls); i++)
                    {
                        AST_Declaration* member_decl = member_decls[i];
                        assert(member_decl->kind == AST_DECL_MUTABLE);
                        if (!member_decl->identifier)
                        {
                            // assert(false);
                            auto anon_aggregate_type = member_decl->mutable_decl.type;
                            assert(anon_aggregate_type->kind == AST_TYPE_STRUCT ||
                                   anon_aggregate_type->kind == AST_TYPE_UNION);

                            auto anon_members =
                                anon_aggregate_type->aggregate_type.member_declarations;
                            for (uint64_t j = 0; j < BUF_LENGTH(anon_members); j++)
                            {
                                AST_Declaration* anon_member = anon_members[j];
                                assert(anon_member->kind == AST_DECL_MUTABLE);
                                if (!anon_member->identifier &&
                                    (anon_member->mutable_decl.type->kind == AST_TYPE_STRUCT ||
                                     anon_member->mutable_decl.type->kind == AST_TYPE_UNION))
                                {
                                    resolver_report_error(resolver, anon_member->file_pos,
                                                          "Multiple levels of anonymous aggregates are not allowed");
                                    return false;
                                }

                                if (anon_member->identifier->atom == member_expr->identifier->atom)
                                {
                                    member_expr->type = resolver_get_declaration_type(anon_member);
                                    dot_expr->dot.declaration = anon_member;
                                    found = true;
                                    break;
                                }
                            }
                        }

                        if (found) break;
                    }
                }

                assert(found);
            }
            else assert(false);
        }
        else if (base_decl->kind == AST_DECL_IMPORT)
        {
            assert(base_decl->import.module);

            AST_Module* ast_module = base_decl->import.module;

            result &= resolver_resolve_expression(resolver, member_expr, ast_module->module_scope);

            if (result)
            {
                assert(member_expr->identifier->declaration);
                dot_expr->dot.declaration = member_expr->identifier->declaration;
            }
        }
        else if (base_decl->kind == AST_DECL_AGGREGATE_TYPE)
        {
            assert(base_decl->aggregate_type.kind == AST_AGG_DECL_ENUM);
            assert(base_decl->aggregate_type.scope);

            AST_Declaration* member_decl = find_declaration(resolver->context,
                                                            base_decl->aggregate_type.scope,
                                                            member_expr->identifier);

            assert(member_decl);

            result &= resolver_resolve_expression(resolver, member_expr,
                                                  base_decl->aggregate_type.scope,
                                                  base_decl->aggregate_type.type);


            if (result)
            {
                dot_expr->type = base_decl->aggregate_type.type;
                member_expr->flags |= AST_EXPR_FLAG_CONST;
                dot_expr->dot.declaration = member_decl;
            }

        }
        else assert(false);

        if (result)
        {
            dot_expr->type = member_expr->type;
            assert(dot_expr->type);
            if (member_expr->flags & AST_EXPR_FLAG_CONST)
            {
                dot_expr->flags |= AST_EXPR_FLAG_CONST;
            }
        }

        return result;
    }

    bool resolver_resolve_identifier(Resolver* resolver, AST_Identifier* identifier,
                                     AST_Scope* scope)
    {
        assert(resolver);
        assert(identifier);
        assert(scope);

        if (identifier->declaration)
        {
            return true;
        }

        AST_Declaration* decl = find_declaration(resolver->context, scope, identifier);
        if (!decl)
        {
            resolver_report_error(resolver, identifier->file_pos,
                                  "Reference to undeclared identifier: %s",
                                  identifier->atom.data);
            return false;
        }

        bool result = resolver_resolve_declaration(resolver, decl, scope);

        if (result)
        {
            identifier->declaration = decl;
        }

        return result;
    }

    bool resolver_resolve_type_spec(Resolver* resolver, AST_Type_Spec* type_spec,
                                    AST_Type** type_dest, AST_Scope* scope)
    {
        assert(resolver);
        assert(type_spec);
        assert(type_dest);
        assert(scope);

        if (type_spec->type)
        {
            *type_dest = type_spec->type;
            return true;
        }

        assert(*type_dest == nullptr);

        bool result = true;

        switch (type_spec->kind)
        {
            case AST_TYPE_SPEC_IDENT:
            {
                AST_Identifier* ident = type_spec->identifier;
                result &= resolver_resolve_identifier(resolver, ident, scope);
                if (!result) break;

                assert(ident->declaration);
                AST_Declaration* decl = ident->declaration;
                *type_dest = resolver_get_declaration_type(decl);
                break;
            }

            case AST_TYPE_SPEC_POINTER:
            {
                AST_Type* base_type = nullptr;
                result &= resolver_resolve_type_spec(resolver, type_spec->pointer.base,
                                                     &base_type, scope);
                if (!result) break;

                assert(base_type);

                *type_dest = ast_find_or_create_pointer_type(resolver->context, base_type);
                break;
            }

            case AST_TYPE_SPEC_STATIC_ARRAY:
            {
                AST_Type* element_type = nullptr;
                result &= resolver_resolve_type_spec(resolver, type_spec->static_array.base,
                                                     &element_type, scope);
                if (!result) break;

                assert(element_type);

                AST_Expression* count_expr = type_spec->static_array.count_expr;

                result &= resolver_resolve_expression(resolver, count_expr, scope);

                if (!result) break;

                assert((count_expr->type->flags & AST_TYPE_FLAG_INT) ||
                       (count_expr->type->kind == AST_TYPE_ENUM &&
                        (count_expr->type->aggregate_type.base_type->flags & AST_TYPE_FLAG_INT)));

                *type_dest = ast_find_or_create_array_type(resolver->context, element_type,
                                                           count_expr, scope);
                break;
            }

            case AST_TYPE_SPEC_FUNCTION:
            {
                bool result = true;
                BUF(AST_Type*) arg_types = nullptr;
                AST_Scope* arg_scope = type_spec->function.arg_scope;

                assert(arg_scope);

                for (uint64_t i = 0; i < BUF_LENGTH(type_spec->function.args); i++)
                {
                    AST_Declaration* arg_decl = type_spec->function.args[i];
                    bool arg_result = resolver_resolve_declaration(resolver, arg_decl, arg_scope);

                    if (!arg_result)
                    {
                        result = false;
                        BUF_FREE(arg_types);
                        break;
                    }

                    assert(arg_decl->kind == AST_DECL_MUTABLE);
                    BUF_PUSH(arg_types, arg_decl->mutable_decl.type);
                }

                AST_Type* return_type = nullptr;
                AST_Type_Spec* return_type_spec = type_spec->function.return_type_spec;

                if (return_type_spec)
                {
                    result &= resolver_resolve_type_spec(resolver, return_type_spec, &return_type,
                                                         scope);
                }
                else
                {
                    return_type = Builtin::type_void;
                }

                if (result)
                {
                    bool is_vararg = type_spec->flags & AST_TYPE_SPEC_FLAG_FUNC_VARARG;
                    AST_Type* result_type = ast_find_or_create_function_type(resolver->context,
                                                                             is_vararg, arg_types,
                                                                             return_type);
                    *type_dest = result_type;
                    type_spec->type = result_type;
                }

                break;
            }

            case AST_TYPE_SPEC_DOT:
            {
                auto module_ident = type_spec->dot.module_ident;
                result &= resolver_resolve_identifier(resolver, module_ident, scope);

                if (result)
                {
                    assert(module_ident->declaration);
                    auto module_decl = module_ident->declaration;
                    assert(module_decl->kind == AST_DECL_IMPORT);
                    assert(module_decl->import.module);
                    auto module = module_decl->import.module;

                    AST_Type* result_type = nullptr;
                    result &= resolver_resolve_type_spec(resolver, type_spec->dot.member_type_spec,
                                                         &result_type, module->module_scope);
                    if (!result) break;
                    assert(result_type);
                    *type_dest = result_type;
                    type_spec->type = result_type;
                }

                break;
            }

            default: assert(false);
        }


        if (result)
        {
            assert(*type_dest);
            type_spec->type = *type_dest;
        }
        return result;
    }

    AST_Type* resolver_get_declaration_type(AST_Declaration* decl)
    {
        assert(decl);

        switch (decl->kind)
        {
            case AST_DECL_TYPE:
            {
                assert(decl->type.type);
                return decl->type.type;
            }

            case AST_DECL_TYPEDEF:
            {
                assert(decl->typedef_decl.type);
                return decl->typedef_decl.type;
            }

            case AST_DECL_FUNC:
            {
                assert(decl->function.type);
                return decl->function.type;
            }

            case AST_DECL_MUTABLE:
            {
                assert(decl->mutable_decl.type);
                return decl->mutable_decl.type;
            }

            case AST_DECL_CONSTANT_VAR:
            {
                assert(decl->constant_var.type);
                return decl->constant_var.type;
            }

            case AST_DECL_AGGREGATE_TYPE:
            {
                assert(decl->aggregate_type.type);
                return decl->aggregate_type.type;
            }

            case AST_DECL_IMPORT:
            {
                return nullptr;
            }

            default: assert(false);
        }
    }

    bool resolver_check_assign_types(Resolver* resolver, AST_Type* lhs, AST_Type* rhs)
    {
        assert(resolver);
        assert(lhs);
        assert(rhs);

        if (lhs == rhs) return true;

        bool lhs_integer = lhs->flags & AST_TYPE_FLAG_INT;
        bool rhs_integer = rhs->flags & AST_TYPE_FLAG_INT;
        bool lhs_float = lhs->flags & AST_TYPE_FLAG_FLOAT;
        bool rhs_float = rhs->flags & AST_TYPE_FLAG_FLOAT;
        bool lhs_sign = lhs->flags & AST_TYPE_FLAG_SIGNED;
        bool rhs_sign = rhs->flags & AST_TYPE_FLAG_SIGNED;
        bool lhs_pointer = lhs->kind == AST_TYPE_POINTER;
        bool rhs_pointer = rhs->kind == AST_TYPE_POINTER;

        if (lhs_integer && rhs_integer)
        {
            if (lhs_sign == rhs_sign)
            {
                return lhs->bit_size >= rhs->bit_size;
            }
            else if (lhs_sign)
            {
                return lhs->bit_size >= rhs->bit_size * 2;
            }
        }
        else if (lhs_float && rhs_integer)
        {
            return lhs->bit_size >= rhs->bit_size;
        }
        else if (lhs == Builtin::type_pointer_to_void && rhs_pointer)
        {
            return true;
        }
        else if (lhs == Builtin::type_String && rhs == Builtin::type_pointer_to_u8)
        {
            return true;
        }

        return false;
    }

    void resolver_transform_to_cast_expression(Resolver* resolver, AST_Expression* expr,
                                               AST_Type* type)
    {
        assert(resolver);
        assert(expr);
        assert(type);

        auto expr_copy = ast_expression_new(resolver->context, expr->file_pos, expr->kind);

        *expr_copy = *expr;

        expr->kind = AST_EXPR_CAST;
        expr->cast_expr.expr = expr_copy;
        expr->type = type;
    }

    bool defer_statement_is_legal(Resolver* resolver, AST_Statement* statement)
    {
        assert(resolver);
        assert(statement);

        switch (statement->kind)
        {
            case AST_STMT_DECLARATION:
            {
                return true;
            }

            case AST_STMT_RETURN:
            {
                resolver_report_error(resolver, statement->file_pos,
                                      "Return statement is not allow in a defer statement");
                return false;
            }

            case AST_STMT_BLOCK:
            {
                for (uint64_t i = 0; i < BUF_LENGTH(statement->block.statements); i++)
                {
                    bool legal = defer_statement_is_legal(resolver,
                                                          statement->block.statements[i]);
                    if (!legal)
                    {
                        return false;
                    }
                }

                return true;
            }

            case AST_STMT_IF:
            {
                bool then_legal = defer_statement_is_legal(resolver,
                                                           statement->if_stmt.then_statement);
                if (!then_legal)
                    return false;

                if (statement->if_stmt.else_statement)
                {
                    return defer_statement_is_legal(resolver, statement->if_stmt.else_statement);
                }

                return true;
            }

            case AST_STMT_ASSIGN:
            {
                return true;
            }

            case AST_STMT_CALL:
            {
                return true;
            }

            case AST_STMT_WHILE:
            {
                return defer_statement_is_legal(resolver, statement->while_stmt.body_stmt);
            }

            case AST_STMT_FOR:
            {
                bool init_legal = defer_statement_is_legal(resolver,
                                                           statement->for_stmt.init_stmt);
                if (!init_legal)
                    return false;

                bool step_legal = defer_statement_is_legal(resolver,
                                                           statement->for_stmt.step_stmt);
                if (!step_legal)
                    return false;

                return defer_statement_is_legal(resolver, statement->for_stmt.body_stmt);
            }

            case AST_STMT_SWITCH:
            {
                resolver_report_error(resolver, statement->file_pos,
                                      "Switch statement not allowed in defer statement");
                return false;
            }

            case AST_STMT_BREAK:
            {
                resolver_report_error(resolver, statement->file_pos,
                                      "Break statement not allowed in defer statement");
                return false;
            }

            case AST_STMT_INSERT:
            {
                // Disallow this for now, need to think about if we want to support this
                return false;
            }

            case AST_STMT_ASSERT:
            {
                return true;
            }

            case AST_STMT_DEFER:
            {
                resolver_report_error(resolver, statement->file_pos,
                                      "Defer statement not allow in defer statement");
                    return false;
            }

            default: assert(false);
        }

		assert(false);
		return false;
    }

    void resolver_convert_to_builtin_string(Resolver* resolver, AST_Expression* string_lit_expr)
    {
        assert(resolver);
        assert(string_lit_expr);
        assert(string_lit_expr->type == Builtin::type_pointer_to_u8);

        AST_Expression* old_expr = ast_expression_new(resolver->context, string_lit_expr->file_pos,
                                                      string_lit_expr->kind);

        *old_expr = *string_lit_expr;

        string_lit_expr->kind = AST_EXPR_COMPOUND_LITERAL;
        string_lit_expr->type = Builtin::type_String;
        string_lit_expr->compound_literal.expressions = nullptr;

        BUF_PUSH(string_lit_expr->compound_literal.expressions, old_expr);

        AST_Expression* length_expr = nullptr;
        if (old_expr->kind == AST_EXPR_STRING_LITERAL)
        {
            auto str_length = old_expr->string_literal.atom.length;
            length_expr = ast_integer_literal_expression_new(resolver->context,
                                                             string_lit_expr->file_pos,
                                                             str_length);
            length_expr->type = Builtin::type_u64;
        }
        else
        {
            BUF(AST_Expression*) args = nullptr;
            BUF_PUSH(args, old_expr);

            auto strlen_ident_expr =
                ast_ident_expression_new(resolver->context, old_expr->file_pos,
                                         Builtin::decl_string_length->identifier);
            length_expr = ast_call_expression_new(resolver->context, old_expr->file_pos,
                                                  strlen_ident_expr, args);
            length_expr->call.callee_declaration = Builtin::decl_string_length;
        }

        assert(length_expr);
        BUF_PUSH(string_lit_expr->compound_literal.expressions, length_expr);
    }

    AST_Module* resolver_add_import_to_module(Resolver* resolver, AST_Module* module,
                                              Atom module_path, Atom module_name)
    {
        assert(resolver);
        assert(module);

        assert(file_exists(module_path.data));

        AST_Module* import_module = zodiac_compile_or_get_module(resolver->context, module_path,
                                                                 module_name);
        if (!import_module)
        {
            return nullptr;
        }

        bool found = false;

        for (uint64_t i = 0; i < BUF_LENGTH(module->import_modules); i++)
        {
            if (module->import_modules[i] == import_module)
            {
                found = true;
                break;
            }
        }

        if (!found)
        {
            BUF_PUSH(module->import_modules, import_module);
        }

        return import_module;
    }

    void resolver_replace_aggregate_declaration_with_mutable(Resolver* resolver,
                                                             AST_Declaration** decl_ptr,
                                                             AST_Scope* scope)
    {
        assert(resolver);
        assert(decl_ptr);
        auto decl = *decl_ptr;
        assert(decl);
        assert(decl->kind == AST_DECL_AGGREGATE_TYPE);
        assert(scope);

        bool result = resolver_resolve_declaration(resolver, decl, scope);

        assert(result);

        AST_Identifier* ident_copy = nullptr;
        if (decl->identifier)
        {
            ident_copy = ast_identifier_new(resolver->context, decl->identifier->atom,
                                            decl->identifier->file_pos);
        }
        else
        {
            auto agg_decl = decl->aggregate_type.aggregate_decl;
            for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->members); i++)
            {
                auto member = agg_decl->members[i];
                if (!member->identifier)
                {
                    resolver_report_error(resolver, member->file_pos,
                                          "Multiple levels of anonymous aggregates not supported");
                }
            }
        }

        AST_Type_Spec* type_spec = ast_type_spec_from_type_new(resolver->context, decl->file_pos,
                                                               decl->aggregate_type.type);
        auto new_decl = ast_mutable_declaration_new(resolver->context, decl->file_pos, ident_copy,
                                                    type_spec, nullptr,
                                                    AST_DECL_LOC_AGGREGATE_MEMBER);

        *decl_ptr = new_decl;
    }

    bool resolve_result_has_errors(Resolve_Result* rr)
    {
        assert(rr);

        return BUF_LENGTH(rr->errors) != 0;
    }

    void resolve_result_report_errors(Resolve_Result* rr)
    {
        assert(rr);

        for (uint64_t i = 0; i < BUF_LENGTH(rr->errors); i++)
        {
            Resolve_Error error = rr->errors[i];

            fprintf(stderr, "%s:%" PRIu64 ":%" PRIu64 ": %s\n", error.file_pos.file_name,
                    error.file_pos.line, error.file_pos.line_relative_char_pos,
                    error.message);
        }
    }

    bool resolver_resolve_static_if_declaration(Resolver* resolver, AST_Declaration* if_decl,
                                                AST_Scope* scope)
    {
        assert(resolver);
        assert(if_decl);
        assert(if_decl->kind == AST_DECL_STATIC_IF);
        assert(scope);

        bool result = true;

        auto if_expr = if_decl->static_if.cond_expr;
        result &= resolver_resolve_expression(resolver, if_expr, scope);

        if (result)
        {
            assert(if_expr->flags & AST_EXPR_FLAG_CONST);
            bool cond_value = const_interpret_bool_expression(resolver->context, if_expr,
                                                                scope);
            AST_Declaration* decl_to_emit = nullptr;
            if (cond_value)
            {
                decl_to_emit = if_decl->static_if.then_declaration;
            }
            else
            {
                decl_to_emit = if_decl->static_if.else_declaration;
            }

            resolver_push_declaration_to_scope(resolver, decl_to_emit, scope);
            if (decl_to_emit->kind != AST_DECL_STATIC_IF &&
                decl_to_emit->kind != AST_DECL_BLOCK)
            {
                result &= resolver_resolve_declaration(resolver, decl_to_emit, scope);
                assert(scope->flags & AST_SCOPE_FLAG_IS_MODULE_SCOPE);
                BUF_PUSH(resolver->module->global_declarations, decl_to_emit);
            }
        }

        return result;
    }

    void resolver_push_declaration_to_scope(Resolver* resolver, AST_Declaration* decl_to_emit,
                                            AST_Scope* scope)
    {
        assert(resolver);
        assert(decl_to_emit);
        assert(scope);

        if (decl_to_emit->kind == AST_DECL_BLOCK)
        {
            for (uint64_t i = 0; i < BUF_LENGTH(decl_to_emit->block.decls); i++)
            {
                auto block_decl = decl_to_emit->block.decls[i];
                resolver_push_declaration_to_scope(resolver, block_decl, scope);
            }
        }
        else if (decl_to_emit->kind == AST_DECL_STATIC_IF)
        {
            bool res = resolver_resolve_static_if_declaration(resolver, decl_to_emit, scope);
            assert(res);
        }
        else
        {
            ast_scope_push_declaration(scope, decl_to_emit);
            assert(scope->flags & AST_SCOPE_FLAG_IS_MODULE_SCOPE);
            BUF_PUSH(resolver->module->global_declarations, decl_to_emit);
        }
    }

    AST_Declaration* find_declaration(Context* context, AST_Scope* scope,
                                      AST_Identifier* identifier,
                                      bool allow_import_check/*=true*/)
    {
        assert(scope);
		assert(scope->module || !scope->parent);
        assert(identifier);

		if (identifier->declaration)
		{
			return identifier->declaration;
		}

		AST_Declaration* decl = ast_scope_find_declaration(context, scope, identifier->atom);
		if (decl)
		{
            // FIXME: TODO: This doesn't really seem like the best way to do this, but
            //               I can't think of a better way to do it right now, without
            //               majorly restructuring the resolver.
            if (!(scope->flags & AST_SCOPE_FLAG_IS_MODULE_SCOPE) &&
                !(scope->flags & AST_SCOPE_FLAG_IS_ENUM_SCOPE) &&
                (identifier->file_pos.file_name == decl->file_pos.file_name) &&
                (identifier->file_pos.line < decl->file_pos.line ||
                (identifier->file_pos.line == decl->file_pos.line &&
                 identifier->file_pos.line_relative_char_pos <=
                 decl->file_pos.line_relative_char_pos)))
            {
                // printf("Use before declare: %s\n", identifier->atom.data);
                return nullptr;
            }
			return decl;
		}

        for (uint64_t i = 0; i < BUF_LENGTH(scope->using_modules); i++)
        {
            AST_Module* um = scope->using_modules[i];
            decl = ast_scope_find_declaration(context, um->module_scope, identifier->atom);
            if (decl)
            {
                return decl;
            }
        }

        for (uint64_t i = 0; i < BUF_LENGTH(scope->using_declarations); i++)
        {
            AST_Declaration* ud = scope->using_declarations[i];
            if (ud->kind == AST_DECL_AGGREGATE_TYPE &&
                ud->aggregate_type.kind == AST_AGG_DECL_ENUM)
            {
                auto agg_decls = ud->aggregate_type.aggregate_decl->members;
                for (uint64_t j = 0; j < BUF_LENGTH(agg_decls); j++)
                {
                    AST_Declaration* member_decl = agg_decls[j];
                    assert(member_decl->kind == AST_DECL_CONSTANT_VAR);
                    assert(member_decl->location == AST_DECL_LOC_AGGREGATE_MEMBER);
                    if (member_decl->identifier->atom == identifier->atom)
                    {
                        return member_decl;
                    }
                }
            }
            else
            {
                assert(false);
            }
        }

        if (scope->parent)
        {
            return find_declaration(context, scope->parent, identifier, allow_import_check);
        }
		else
		{
            assert(!scope->module);
			for (uint64_t i = 0; i < BUF_LENGTH(context->builtin_decls); i++)
			{
				AST_Declaration* builtin_decl = context->builtin_decls[i];
				if (builtin_decl->identifier->atom == identifier->atom)
				{
					return builtin_decl;
				}
			}
		}

        return nullptr;
    }

    bool binop_is_cmp(AST_Expression* expression)
    {
        assert(expression);
        assert(expression->kind == AST_EXPR_BINARY);

        switch (expression->binary.op)
        {
            case AST_BINOP_ADD:
            case AST_BINOP_SUB:
            case AST_BINOP_MUL:
            case AST_BINOP_DIV:
            case AST_BINOP_MOD:
            case AST_BINOP_AND:
            case AST_BINOP_OR:
            {
                return false;
            }

            case AST_BINOP_EQ:
            case AST_BINOP_LT:
            case AST_BINOP_LTEQ:
            case AST_BINOP_GT:
            case AST_BINOP_GTEQ:
            case AST_BINOP_NEQ:
            case AST_BINOP_AND_AND:
            case AST_BINOP_OR_OR:
            {
                return true;
            }

            default: assert(false);
        }

        assert(false);
        return false;
    }

    char* run_insert(Resolver* resolver, AST_Expression* call_expression)
    {
        assert(resolver);
        assert(call_expression);
        assert(call_expression->kind == AST_EXPR_CALL);

        IR_Builder ir_builder;
        ir_builder_init(&ir_builder, resolver->context);

        IR_Module ir_module = ir_builder_emit_module(&ir_builder, resolver->module);

        if (ir_module.error_count)
        {
            fprintf(stderr, "Exitting with error(s) from insert module\n");
            return nullptr;
        }

        IR_Validation_Result validation = ir_validate(&ir_builder);

        if (resolver->context->options.print_ir)
        {
            ir_builder_print_result(&ir_builder);
        }

        if (!validation.messages)
        {
            IR_Runner ir_runner;
            ir_runner_init(resolver->context, &ir_runner);

            AST_Declaration* insert_decl = call_expression->call.callee_declaration;
            assert(insert_decl);

            IR_Value* func_value = ir_builder_value_for_declaration(&ir_builder, insert_decl);
            assert(func_value);
            assert(func_value->function);

            if (!ir_runner_load_dynamic_libs(&ir_runner, resolver->module, &ir_module))
            {
                return nullptr;
            }
            ir_runner_load_foreigns(&ir_runner, &ir_module);
            ir_runner_execute_block(&ir_runner, ir_runner.context->global_init_block->block);

            auto num_args = BUF_LENGTH(call_expression->call.arg_expressions);

            for (uint64_t i = 0; i < num_args; i++)
            {
                AST_Expression* arg_expr = call_expression->call.arg_expressions[i];

                IR_Value* arg_value= nullptr;
                if (arg_expr->kind == AST_EXPR_STRING_LITERAL)
                {
                    arg_value = ir_string_literal(&ir_builder, Builtin::type_pointer_to_u8,
                                                  arg_expr->string_literal.atom);
                }
                else if (arg_expr->kind == AST_EXPR_INTEGER_LITERAL)
                {
                    arg_value = ir_integer_literal(&ir_builder, arg_expr->type,
                                                   arg_expr->integer_literal.u64);
                }
                assert(arg_value);

                IR_Pushed_Arg pa = { *arg_value, false };
                stack_push(ir_runner.arg_stack, pa);
            }

            IR_Value return_value = {};
            IR_Stack_Frame* entry_stack_frame = ir_runner_call_function(&ir_runner,
                                                                        call_expression->file_pos,
                                                                        func_value->function,
                                                                        num_args, &return_value);

            resolver->module->gen_data = nullptr;

            return (char*)return_value.value.pointer;
        }
        else
        {
            for (uint64_t i = 0; i < BUF_LENGTH(validation.messages); i++)
            {
                fprintf(stderr, "%s\n", validation.messages[i]);
            }
        }

        return nullptr;
    }

    AST_Type* resolver_create_recursive_function_type(Resolver* resolver, AST_Declaration* decl)
    {
        assert(resolver);
        assert(decl);
        assert(decl->kind == AST_DECL_FUNC);


        if (decl->function.type)
        {
            return decl->function.type;
        }

        bool is_vararg = false;
        BUF(AST_Type*) arg_types = nullptr;
        AST_Type* return_type = Builtin::type_void;

        bool result = true;

        if (decl->flags & AST_DECL_FLAG_FUNC_VARARG)
        {
            is_vararg = true;
        }

        for (uint64_t i = 0; i < BUF_LENGTH(decl->function.args); i++)
        {
            AST_Declaration* arg_decl = decl->function.args[i];

            result &= resolver_resolve_declaration(resolver, arg_decl,
                                                   decl->function.argument_scope);
            assert(arg_decl->kind == AST_DECL_MUTABLE);
            assert(arg_decl->mutable_decl.type);

            BUF_PUSH(arg_types, arg_decl->mutable_decl.type);
        }

        if (!result) return nullptr;

        if (decl->function.return_type_spec)
        {
            AST_Type* new_ret_type = nullptr;
            result &= resolver_resolve_type_spec(resolver, decl->function.return_type_spec,
                                                 &new_ret_type, decl->function.argument_scope);

            if (result)
            {
                assert(new_ret_type);
                return_type = new_ret_type;
            }
        }

        AST_Type* func_type = ast_find_or_create_function_type(resolver->context, is_vararg,
                                                               arg_types, return_type);

        return func_type;
    }

    Resolve_Error* resolver_report_error(Resolver* resolver, File_Pos file_pos,
                                      const char* format, ...)
    {
        va_list args;
        va_start(args, format);
        Resolve_Error* result = resolver_report_error(resolver, file_pos, RE_FLAG_NONE,
                                                      format, args);
        va_end(args);

        return result;
    }

    Resolve_Error* resolver_report_error(Resolver* resolver, File_Pos file_pos,
                                                Resolve_Error_Flag flags, const char* format,
                                                va_list args)
    {
        assert(resolver);
        assert(format);

        const size_t print_buf_size = 2048;
        static char print_buf[print_buf_size];

        vsprintf(print_buf, format, args);

        auto message_length = strlen(print_buf);
        char* message = (char*)mem_alloc(message_length + 1);
        assert(message);
        memcpy(message, print_buf, message_length);
        message[message_length] = '\0';

        Resolve_Error error = { flags, message, file_pos };
        // if (resolver->resolving_auto_gen)
        // {
        //     error.auto_gen = true;
        //     error.auto_gen_file_pos = resolver->auto_gen_file_pos;
        // }
        BUF_PUSH(resolver->result.errors, error);

        Resolve_Error* result = &resolver->result.errors[BUF_LENGTH(resolver->result.errors) - 1];
        return result;
    }

    Resolve_Error* resolver_report_error(Resolver* resolver, File_Pos file_pos,
                                                Resolve_Error_Flag flags,
                                                const char* format, ...)
    {
        assert(resolver);
        assert(format);

        va_list args;
        va_start(args, format);
        Resolve_Error* result = resolver_report_error(resolver, file_pos, flags, format, args);
        va_end(args);

        return result;
    }
}
