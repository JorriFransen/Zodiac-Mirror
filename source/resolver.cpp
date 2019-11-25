#include "resolver.h"

#include "builtin.h"
#include "const_interpreter.h"
#include "ir.h"
#include "ir_runner.h"
#include "lexer.h"
#include "parser.h"
#include "polymorph.h"

#include <stdarg.h>
#include <inttypes.h>

namespace Zodiac
{
    void resolver_init(Resolver* resolver, Context* context, bool is_builtin)
    {
        assert(resolver);
        assert(context);

        resolver->context = context;
        resolver->module = nullptr;
        resolver->current_break_context = nullptr;
        resolver->current_func_decl = nullptr;
        resolver->result = {};
        resolver->is_builtin = is_builtin;
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
            resolver_push_declaration_to_scope(resolver, global_decl, scope);
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

            if (resolver->is_builtin && global_decl->kind == AST_DECL_CONSTANT_VAR)
            {
                bool platform_linux = false;
                bool platform_windows = false;

#ifdef WIN32 
                platform_windows = true;
#elif defined __linux__
                platform_linux = true;
#elif
                assert(false);
#endif
                auto at = resolver->context->atom_table;
                if (global_decl->identifier->atom == atom_get(at, "PLATFORM_LINUX"))
                {
                    global_decl->constant_var.init_expression->bool_literal.boolean =
                        platform_linux;
                }
                else if (global_decl->identifier->atom == atom_get(at, "PLATFORM_WINDOWS"))
                {
                    global_decl->constant_var.init_expression->bool_literal.boolean =
                        platform_windows;
                }
            }
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

            if (declaration->kind != AST_DECL_FUNC_OVERLOAD)
            {
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
            else
            {
                return true;
            }
        }

        if (declaration->flags & AST_DECL_FLAG_RESOLVED)
        {
            return true;
        }
        else if (declaration->flags & AST_DECL_FLAG_ERROR)
        {
            return false;
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
        bool dont_mark_errors = false;

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
                if (declaration->flags & AST_DECL_FLAG_FUNC_POLY_TEMPLATE)
                {
                    result &= resolve_poly_func_decl(resolver, declaration, scope);
                    break;
                }

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
                        resolver_push_declaration_to_scope(resolver, arg_decl,
                                                           declaration->function.argument_scope);
                    }
                }

                if (!arg_result)
                {
                    result = false;
                    break;
                }

                if (declaration->function.return_type_spec)
                {
                    AST_Scope* return_type_scope = scope;
                    if (declaration->flags & AST_DECL_FLAG_FUNC_POLY)
                    {
                        return_type_scope = declaration->function.argument_scope->parent;
                        assert(return_type_scope != scope);
                    }

                    auto ts = declaration->function.return_type_spec;
                    result &= resolver_resolve_type_spec(resolver,
                                                         ts->file_pos, ts,
                                                         &declaration->function.return_type,
                                                         return_type_scope, scope);
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
                assert(return_type);
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
                    auto ts = declaration->mutable_decl.type_spec;
                    result &= resolver_resolve_type_spec(resolver, ts->file_pos, ts,
                                                         &declaration->mutable_decl.type, scope,
                                                         scope);
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
                        if (declaration->location == AST_DECL_LOC_GLOBAL)
                        {
                            assert(init_expr->flags & AST_EXPR_FLAG_CONST);
                        }

                        if (specified_type)
                        {
                            bool asign_res = resolver_check_assign_types(resolver,
                                                                  declaration->mutable_decl.type,
                                                                  init_expr->type);
                            result &= asign_res;
                            if (!asign_res)
                            {
                                auto got_str = ast_type_to_string(init_expr->type);
                                auto expected_str = ast_type_to_string(declaration->mutable_decl.type);
                                resolver_report_error(resolver, declaration->file_pos,
                                                      "Mismatching types in declaration asignment\n\tGot: %s\n\tExpected: %s",
                                                      got_str, expected_str);
                                mem_free(got_str);
                                mem_free(expected_str);

                            }
                        }
                        else
                        {
                            declaration->mutable_decl.type = init_expr->type;
                        }
                    }
                }

                if (result && (declaration->location == AST_DECL_LOC_LOCAL))
                {
                    resolver_push_declaration_to_scope(resolver, declaration, scope);
                }

                break;
            }

            case AST_DECL_CONSTANT_VAR:
            {
                if (declaration->constant_var.type_spec)
                {
                    auto ts = declaration->constant_var.type_spec;
                    result &= resolver_resolve_type_spec(resolver,
                                                         ts->file_pos, ts,
                                                         &declaration->constant_var.type, scope);
                    if (!result) break;
                }


                auto init_expr = declaration->constant_var.init_expression;
                if (init_expr)
                {
                    result &= resolver_resolve_expression(resolver, init_expr, scope,
                                                          declaration->constant_var.type);
                    assert(init_expr->flags & AST_EXPR_FLAG_CONST);
                }
                else if (scope->flags & AST_SCOPE_FLAG_IS_ENUM_SCOPE)
                {
                    result = false;
                    dont_mark_errors = true;
                    break;
                }
                else assert(false);

                if (!declaration->constant_var.type)
                {
                    declaration->constant_var.type =
                        declaration->constant_var.init_expression->type;
                }

                if (result && (declaration->location == AST_DECL_LOC_LOCAL))
                {
                    resolver_push_declaration_to_scope(resolver, declaration, scope);
                }

                break;
            }

            case AST_DECL_TYPEDEF:
            {
                auto ts = declaration->typedef_decl.type_spec;
                result &= resolver_resolve_type_spec(resolver, ts->file_pos, ts,
                                                     &declaration->typedef_decl.type, scope);
                break;
            }

            case AST_DECL_AGGREGATE_TYPE:
            {
                AST_Aggregate_Declaration* agg_decl = declaration->aggregate_type.aggregate_decl;

                if (declaration->aggregate_type.kind == AST_AGG_DECL_STRUCT ||
                    declaration->aggregate_type.kind == AST_AGG_DECL_UNION)
                {
                    result &= resolver_resolve_struct_or_union_decl(resolver, declaration, scope);
                    if (result && agg_decl->poly_args)
                    {
                        break;
                    }
                }
                else
                {
                    assert(declaration->aggregate_type.kind == AST_AGG_DECL_ENUM);

                    AST_Type* enum_type = nullptr;
                    AST_Type_Spec* enum_type_spec = declaration->aggregate_type.enum_type_spec;
                    if (enum_type_spec)
                    {
                        result &= resolver_resolve_type_spec(resolver, enum_type_spec->file_pos,
                                                             enum_type_spec, &enum_type,
                                                             scope);
                        if (!result) break;
                    }

                    if (!enum_type)
                    {
                        enum_type = Builtin::type_s64;
                    }

                    assert(enum_type->flags & AST_TYPE_FLAG_INT);

                    auto enum_scope = declaration->aggregate_type.scope;
                    assert(enum_scope);

                    for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->members); i++)
                    {
                        AST_Declaration* member_decl = agg_decl->members[i];
                        assert(member_decl->kind == AST_DECL_CONSTANT_VAR);

                        resolver_push_declaration_to_scope(resolver, member_decl, enum_scope);
                    }

                    bool progressed = true;
                    int64_t unresolved_last_cycle = BUF_LENGTH(agg_decl->members);

                    while (unresolved_last_cycle > 0 && progressed)
                    {
                        int64_t next_value = 0;
                        bool member_result = true;
                        int64_t unresolved_this_cycle = 0;

                        for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->members); i++)
                        {
                            AST_Declaration* member_decl = agg_decl->members[i];
                            assert(member_decl->kind == AST_DECL_CONSTANT_VAR);

                            if (member_decl->constant_var.init_expression)
                            {
                                auto init_expr = member_decl->constant_var.init_expression;
                                init_expr->flags |= AST_EXPR_FLAG_LITERAL;

                                if (resolver_resolve_expression(resolver, init_expr, enum_scope,
                                                                enum_type) &&
                                    resolver_resolve_declaration(resolver, member_decl,
                                                                 enum_scope))
                                {
                                }
                                else
                                {
                                    member_result = false;
                                    unresolved_this_cycle++;
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

                                member_decl->flags &= ~AST_DECL_FLAG_RESOLVED;
                                result &= resolver_resolve_declaration(resolver, member_decl,
                                                                       enum_scope);
                            }
                            else
                            {
                                AST_Expression* init_expr =
                                    ast_integer_literal_expression_new(resolver->context,
                                                                       member_decl->file_pos,
                                                                       next_value);
                                bool expr_res = resolver_resolve_expression(resolver, init_expr,
                                                                            enum_scope, enum_type);
                                member_result &= expr_res;
                                member_decl->constant_var.init_expression = init_expr;
                                bool decl_res = resolver_resolve_declaration(resolver,
                                                                             member_decl, scope);
                                member_result &= decl_res;

                                if (!expr_res || !decl_res)
                                {
                                    unresolved_this_cycle++;
                                }

                            }

                            next_value++;
                        }

                        // printf("unresolved_this_cycle: %ld\n", unresolved_this_cycle);
                        // printf("\tunresolved_last_cycle: %ld\n", unresolved_last_cycle);
                        if (unresolved_this_cycle < unresolved_last_cycle)
                        {
                            progressed = true;
                            unresolved_last_cycle = unresolved_this_cycle;
                        }
                        else
                        {
                            progressed = false;
                            assert(false); // Report error
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
                        resolver_push_declaration_to_scope(resolver, insert_decl, scope);
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

            case AST_DECL_FUNC_OVERLOAD:
            {
                auto overloads = declaration->function_overload.overloads;
                for (uint64_t i = 0; i < BUF_LENGTH(overloads); i++)
                {
                    auto overload_decl = overloads[i];
                    result &= resolver_resolve_declaration(resolver, overload_decl, scope);
                }
                break;
            }

            case AST_DECL_POLY_TYPE_SPEC:
            {
                AST_Type* poly_type = nullptr;
                auto ts = declaration->poly_type_spec.type_spec;
                result &= resolver_resolve_type_spec(resolver, ts->file_pos, ts, &poly_type,
                                                     scope);
                break;
            }

            case AST_DECL_STATIC_ASSERT:
            {
                auto sa_expr = declaration->static_assert_expression;
                result &= resolver_resolve_expression(resolver, sa_expr, scope);

                if (!result) return false;

                bool expr_value = const_interpret_bool_expression(resolver->context, sa_expr,
                                                                  scope);
                if (!expr_value)
                {
                    resolver_report_error(resolver, declaration->file_pos,
                                          "Static assert failed!");
                    return false;
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
        else if (!dont_mark_errors)
        {
            declaration->flags |= AST_DECL_FLAG_ERROR;
        }

        return result;
    }

    bool
    resolver_resolve_struct_or_union_decl(Resolver* resolver, AST_Declaration* decl,
                                          AST_Scope* scope, bool is_nested /*=false*/,
                                          Aggregate_Members_To_Resolve* parent_mtr /*=nullptr*/)
    {
        assert(resolver);
        assert(decl);
        assert(decl->kind == AST_DECL_AGGREGATE_TYPE);
        assert(scope);
        if (is_nested) assert(parent_mtr);

        auto aggregate = decl->aggregate_type.aggregate_decl;
        assert(decl->aggregate_type.kind == AST_AGG_DECL_STRUCT ||
               decl->aggregate_type.kind == AST_AGG_DECL_UNION);

        if (aggregate->poly_args)
        {
            return true;
        }

        Aggregate_Members_To_Resolve members_to_resolve = {};
        BUF(AST_Declaration**) members_to_replace = nullptr;

        bool is_struct = decl->aggregate_type.kind == AST_AGG_DECL_STRUCT;

        auto agg_scope = decl->aggregate_type.scope;

        bool result = true;
        uint64_t total_bit_size = 0;
        uint64_t biggest_bit_size = 0;

        for (uint64_t i = 0; i < BUF_LENGTH(aggregate->members); i++)
        {
            AST_Declaration* member_decl = aggregate->members[i];
            assert(member_decl->kind == AST_DECL_MUTABLE ||
                   member_decl->kind == AST_DECL_AGGREGATE_TYPE);

            if (member_decl->kind == AST_DECL_MUTABLE)
            {
                assert(!member_decl->mutable_decl.init_expression);

                uint64_t member_bit_size = 0;

                auto member_ts = member_decl->mutable_decl.type_spec;
                if (member_ts->kind == AST_TYPE_SPEC_POINTER)
                {
                    total_bit_size += Builtin::pointer_size;
                    biggest_bit_size = MAX(biggest_bit_size, Builtin::pointer_size);

                    Aggregate_Member_To_Resolve m = { member_decl, agg_scope };
                    if (is_nested)
                    {
                        BUF_PUSH(parent_mtr->members, m);
                    }
                    else
                    {
                        BUF_PUSH(members_to_resolve.members, m);
                    }
                }
                else
                {
                    bool member_result = resolver_resolve_declaration(resolver, member_decl,
                                                                    agg_scope);

                    if (member_result)
                    {
                        AST_Type* member_type = member_decl->mutable_decl.type;
                        total_bit_size += member_type->bit_size;
                        biggest_bit_size = MAX(biggest_bit_size, member_type->bit_size);
                    }
                    else
                    {
                        result = false;
                    }
                }
            }
            else if (member_decl->kind == AST_DECL_AGGREGATE_TYPE)
            {
                // result &= resolver_resolve_declaration(resolver, member_decl, agg_scope);
                assert(member_decl->aggregate_type.kind == AST_AGG_DECL_STRUCT ||
                       member_decl->aggregate_type.kind == AST_AGG_DECL_UNION);

                auto mtr = &members_to_resolve;
                if (is_nested)
                {
                    mtr = parent_mtr;
                }

                result &= resolver_resolve_struct_or_union_decl(resolver, member_decl,
                                                                agg_scope, true, mtr);

                if (result)
                {
                    BUF_PUSH(members_to_replace, &aggregate->members[i]);
                    AST_Type* member_type = member_decl->aggregate_type.type;
                    assert(member_type);
                    total_bit_size += member_type->bit_size;
                    biggest_bit_size = MAX(biggest_bit_size, member_type->bit_size);
                }
            }
        }

        if (result)
        {
            const char* name = nullptr;
            if (decl->identifier)
            {
                name = decl->identifier->atom.data;
            }
            if (is_struct)
            {
                decl->aggregate_type.type = ast_type_struct_new(resolver->context,
                                                                aggregate->members,
                                                                name, total_bit_size,
                                                                agg_scope,
                                                                aggregate->overload_directives);
            }
            else
            {
                decl->aggregate_type.type = ast_type_union_new(resolver->context,
                                                               aggregate->members,
                                                               name, biggest_bit_size,
                                                               agg_scope,
                                                               aggregate->overload_directives);
            }

            decl->flags |= AST_DECL_FLAG_RESOLVED;
            decl->flags &= ~AST_DECL_FLAG_RESOLVING;

            if (!is_nested)
            {
                for (uint64_t i = 0; i < BUF_LENGTH(members_to_resolve.members); i++)
                {
                    Aggregate_Member_To_Resolve m = members_to_resolve.members[i];
                    result &= resolver_resolve_declaration(resolver, m.decl, m.scope);
                }

                resolver_check_aggregate_for_redecl(resolver, decl);
            }

            for (uint64_t i = 0; i < BUF_LENGTH(members_to_replace); i++)
            {
                AST_Declaration** member_to_replace = members_to_replace[i];
                resolver_replace_aggregate_declaration_with_mutable(resolver, member_to_replace,
                                                                    agg_scope);
            }
        }

        BUF_FREE(members_to_resolve.members);
        BUF_FREE(members_to_replace);

        return result;
    }

    bool resolver_check_aggregate_for_redecl(Resolver* resolver, AST_Declaration* decl)
    {
        assert(resolver);
        assert(decl);
        assert(decl->kind == AST_DECL_AGGREGATE_TYPE);
        assert(decl->aggregate_type.kind == AST_AGG_DECL_STRUCT ||
               decl->aggregate_type.kind == AST_AGG_DECL_UNION);

        auto aggregate = decl->aggregate_type.aggregate_decl;
        BUF(AST_Identifier*) declared_members = nullptr;

        bool result = true;

        for (uint64_t i = 0; i < BUF_LENGTH(aggregate->members); i++)
        {
            AST_Declaration* member = aggregate->members[i];
            if (member->kind == AST_DECL_MUTABLE)
            {
                bool member_res = resolver_check_aggregate_member_for_redecl(resolver,
                                                                             declared_members,
                                                                             member->identifier);
                result &= member_res;
                if (member_res)
                {
                    BUF_PUSH(declared_members, member->identifier);
                }
            }
            else
            {
                assert(member->kind == AST_DECL_AGGREGATE_TYPE);
                assert(member->aggregate_type.kind == AST_AGG_DECL_STRUCT ||
                       member->aggregate_type.kind == AST_AGG_DECL_UNION);

				bool member_res = resolver_check_aggregate_for_redecl(resolver, member);

                if (member->identifier)
                {
                    member_res &= resolver_check_aggregate_member_for_redecl(resolver,
                                                                             declared_members,
                                                                             member->identifier);
                    result &= member_res;
                    if (member_res)
                    {
                        BUF_PUSH(declared_members, member->identifier);
                    }
                }
                else
                {
                    auto member_aggregate = member->aggregate_type.aggregate_decl;
                    for (uint64_t j = 0; j < BUF_LENGTH(member_aggregate->members); j++)
                    {
                        auto nested_member = member_aggregate->members[j];
						if (nested_member->identifier)
						{
							if (nested_member->kind == AST_DECL_MUTABLE)
							{
								auto member_ident = nested_member->identifier;
								bool member_res =
									resolver_check_aggregate_member_for_redecl(resolver,
																			   declared_members,
																			   member_ident);
								result &= member_res;
								if (member_res)
								{
									BUF_PUSH(declared_members, member_ident);
								}
							}
							else
							{
								assert(false);
							}
						}
                    }
                }
            }
        }

        BUF_FREE(declared_members);
        return result;
    }

    bool resolver_check_aggregate_member_for_redecl(Resolver* resolver,
                                                           BUF(AST_Identifier*) declared_members,
                                                           AST_Identifier* identifier)
    {
        assert(resolver);
        if (!identifier) return true;

        for (uint64_t i = 0; i < BUF_LENGTH(declared_members); i++)
        {
            if (identifier->atom == declared_members[i]->atom)
            {
                resolver_report_error(resolver, identifier->file_pos,
                                      "Redeclaration of aggregate member '%s', previous declaration was here: ",
                                      identifier->atom.data);
                resolver_report_error(resolver, declared_members[i]->file_pos, "");

                return false;
            }
        }

        return true;
    }

    bool resolver_resolve_statement(Resolver* resolver, AST_Statement* statement,

                                    AST_Scope* scope)
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
                result &= resolver_resolve_expression(resolver, statement->call_expression,
                                                      scope);
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
                    result &= resolver_resolve_expression(resolver, statement->assign.expression,
                                                          scope, suggested_type);
                }

                if (!result) break;

                bool types_match =
                    resolver_check_assign_types(resolver,
                                                statement->assign.lvalue_expression->type,
                                                statement->assign.expression->type);

                auto lvalue_expr = statement->assign.lvalue_expression;
                auto assign_type = statement->assign.expression->type;
				if (types_match && lvalue_expr->type != assign_type &&
                    lvalue_expr->type->flags & AST_TYPE_FLAG_INT)
				{
					resolver_transform_to_cast_expression(resolver, statement->assign.expression,
                                                          lvalue_expr->type);
				}

                if (!types_match)
                {
                    auto lvalue = statement->assign.lvalue_expression;
                    auto expr = statement->assign.expression;

                    if ((lvalue->type->flags & AST_TYPE_FLAG_FLOAT) &&
                        (expr->type->flags & AST_TYPE_FLAG_INT))
                    {
                        resolver_transform_to_cast_expression(resolver, expr, lvalue->type);
                        types_match = true;
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
                    auto current_func = resolver->current_func_decl;
                    assert(current_func);

                    auto suggested_type = current_func->function.return_type;
                    if (!suggested_type)
                    {
                        suggested_type = current_func->function.inferred_return_type;
                    }

                    result &= resolver_resolve_expression(resolver, statement->return_expression,
                                                          scope, suggested_type);

                    if (result && suggested_type)
                    {
                        auto ret_match = resolver_check_assign_types(resolver,
                                                    suggested_type,
                                                    statement->return_expression->type);
                        if (!ret_match)
                        {
                            auto got_str =
                                ast_type_to_string(statement->return_expression->type);
                            auto exp_str = ast_type_to_string(suggested_type);
                            resolver_report_error(resolver, statement->file_pos,
                                                  "Mismatching type in return statement,\n\tgot: %s\n\texpected: %s\n",
                                                  got_str, exp_str);
                            mem_free(got_str);
                            mem_free(exp_str);
                            return false;
                        }
                    }
                    else
                    {
                        current_func->function.inferred_return_type =
                            statement->return_expression->type;
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

                assert(assert_expr->type == Builtin::type_bool ||
                       assert_expr->type->kind == AST_TYPE_POINTER ||
                       (assert_expr->type->flags & AST_TYPE_FLAG_INT));
                break;
            }

            case AST_STMT_ASSERT_FAIL:
            {
                // Nothing to do for now
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
                                                         statement->if_stmt.else_statement,
                                                         scope);
                }
                break;
            }

            case AST_STMT_STATIC_IF:
            {
                auto if_expr = statement->static_if_stmt.if_expression;
                result &= resolver_resolve_expression(resolver, if_expr, scope);

                if (result)
                {
                    assert(if_expr->type == Builtin::type_bool ||
                           if_expr->type->kind == AST_TYPE_POINTER ||
                           (if_expr->type->flags & AST_TYPE_FLAG_INT));
                    assert(if_expr->flags & AST_EXPR_FLAG_CONST);
                }

                result &= resolver_resolve_statement(resolver,
                                                     statement->static_if_stmt.then_statement,
                                                     scope);
                if (statement->static_if_stmt.else_statement)
                {
                    result &= resolver_resolve_statement(
                        resolver, statement->static_if_stmt.else_statement, scope);
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

                        for (uint64_t i = 0; i < BUF_LENGTH(switch_case.range_expressions);
                             i += 2)
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
                if (result && statement->post_increment->type->kind == AST_TYPE_POINTER)
                {
                    statement->post_increment->flags |= AST_EXPR_FLAG_POINTER_MATH;
                }
                break;
            }

            case AST_STMT_POST_DECREMENT:
            {
                result &= resolver_resolve_expression(resolver, statement->post_decrement, scope);
                if (result && statement->post_decrement->type->kind == AST_TYPE_POINTER)
                {
                    statement->post_decrement->flags |= AST_EXPR_FLAG_POINTER_MATH;
                }
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
        bool points_to_overload_decl = false;

        switch (expression->kind)
        {
            case AST_EXPR_CALL:
            {
                AST_Expression* ident_expr = expression->call.ident_expression;
                bool recursive = false;

                if (ident_expr->kind == AST_EXPR_IDENTIFIER)
                {
                    if (match_builtin_function(resolver, expression, scope))
                    {
                        return true;
                    }

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

                bool is_overload = false;

                if (func_decl->kind == AST_DECL_FUNC_OVERLOAD)
                {
                    func_decl = resolver_find_overload(resolver, func_decl, expression, scope);
                    if (!func_decl) return false;
                    is_overload = true;
                }

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
                    // assert(arg_expr_count == arg_decl_count);
                    if (arg_expr_count != arg_decl_count)
                    {
                        resolver_report_error(resolver, expression->file_pos,
                                              "Expected %d arguments for call, got: %d",
                                              arg_decl_count, arg_expr_count);
                        return false;
                    }
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

                    if (is_overload)
                    {
                        arg_expr->flags &= ~AST_EXPR_FLAG_RESOLVED;
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
                else if (decl->kind == AST_DECL_FUNC_OVERLOAD)
                {
                    points_to_overload_decl = true;
                }

                if (decl->kind == AST_DECL_CONSTANT_VAR)
                {
                    expression->flags |= AST_EXPR_FLAG_CONST;
                }

                expression->type = resolver_get_declaration_type(decl);

                if (!points_to_import_decl && suggested_type && suggested_type != expression->type &&
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
                    else if (suggested_type->kind == AST_TYPE_POINTER ||
                             suggested_type->kind == AST_TYPE_STRUCT)
                    {
                        suggested_type = Builtin::type_s64;
                    }
                    else assert(false);

                    expression->type = suggested_type;
                }
                else
                {
                    expression->type = Builtin::type_s64;
                }

                expression->flags |= AST_EXPR_FLAG_LITERAL | AST_EXPR_FLAG_CONST;
                break;
            }

            case AST_EXPR_FLOAT_LITERAL:
            {
                if (suggested_type)
                {
                    if (suggested_type->flags & AST_TYPE_FLAG_FLOAT)
                    {
                        expression->type = suggested_type;
                    }
                    else if ((suggested_type->flags & AST_TYPE_FLAG_INT) &&
                             (expression->flags & AST_EXPR_FLAG_INTEGER_LITERAL))
                    {
                        if (expression->type == Builtin::type_float)
                        {
                            float float_value = expression->float_literal.r32;
                            expression->kind = AST_EXPR_INTEGER_LITERAL;
                            expression->integer_literal.u64 = (uint64_t)float_value;
                            expression->type = suggested_type;
                        }
                        else if (expression->type == Builtin::type_double)
                        {
                            assert(false);
                        }
                        else assert(false);
                    }
                    else
                    {
                        expression->type = Builtin::type_float;
                    }
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
                                                      scope, suggested_type);
                if (!result) break;

                AST_Type* operand_type = expression->unary.operand->type;

                bool inherit_const = false;

                switch (expression->unary.op)
                {
                    case AST_UNOP_MINUS:
                    {
                        expression->type = operand_type;
                        if (expression->unary.operand->flags & AST_EXPR_FLAG_CONST &&
                                operand_type != suggested_type)
                        {
                            if (expression->unary.operand->flags & AST_EXPR_FLAG_LITERAL)
                            {
                                if (operand_type->flags & AST_TYPE_FLAG_INT)
                                {
                                    expression->unary.operand->type = Builtin::type_s64;
                                    expression->type = Builtin::type_s64;
                                }
                                else if (operand_type == Builtin::type_float)
                                {
                                    expression->type = Builtin::type_float;
                                }
                                else if (operand_type == Builtin::type_double)
                                {
                                   expression->type = Builtin::type_double;
                                }
                                else assert(false);
                            }
                        }
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

					case AST_UNOP_BIN_NOT:
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

                if ((expression->flags & AST_EXPR_FLAG_CONST) &&
                    ((expression->type->flags & AST_TYPE_FLAG_INT) ||
                     (expression->type->flags & AST_TYPE_FLAG_FLOAT)))
                {
                    resolver_transform_const_expr_to_literal(resolver, expression, scope);
                }

                break;
            }

            case AST_EXPR_CAST:
            {
                if (expression->cast_expr.type_spec)
                {
                    auto ts = expression->cast_expr.type_spec;
                    result &= resolver_resolve_type_spec(resolver, ts->file_pos, ts,
                                                         &expression->type, scope);
                }
                else
                {
                    assert(expression->type);
                }

                if (!result) break;

                result &= resolver_resolve_expression(resolver, expression->cast_expr.expr,
                                                      scope);

				if (result && (expression->cast_expr.expr->flags & AST_EXPR_FLAG_CONST))
				{
					expression->flags |= AST_EXPR_FLAG_CONST;	
				}
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

                if (!(base_type->kind == AST_TYPE_STATIC_ARRAY ||
                       base_type->kind == AST_TYPE_POINTER ||
                      index_overload_ident))
                {
                    resolver_report_error(resolver, expression->file_pos,
                                         "Base type of subscript expression is not of pointer or array type\n\tand no suitable overloads where found");
                    return false;
                }

                if (index_overload_ident && (base_type->kind == AST_TYPE_STRUCT ||
                                             base_type->kind == AST_TYPE_UNION))
                {
                    auto lvalue_expr = expression->subscript.base_expression;

                    result &= resolver_resolve_identifier(resolver, index_overload_ident, scope);
                    if (!result) break;

                    auto index_expr = expression->subscript.index_expression;
                    auto overload_ident_expr = ast_ident_expression_new(resolver->context,
                                                                        lvalue_expr->file_pos,
                                                                        index_overload_ident);
                    BUF(AST_Expression*) args = nullptr;
                    BUF_PUSH(args, lvalue_expr);
                    BUF_PUSH(args, index_expr);
                    auto call_expression = ast_call_expression_new(resolver->context,
                                                                   expression->file_pos,
                                                                   overload_ident_expr, args);

                    auto overload_decl = index_overload_ident->declaration;
                    assert(overload_decl->flags & AST_DECL_FLAG_RESOLVED);
                    if (overload_decl->kind == AST_DECL_FUNC_OVERLOAD)
                    {
                        overload_decl = resolver_find_overload(resolver, overload_decl,
                                                               call_expression, scope);
                        if (!overload_decl)
                        {
                            return false;
                        }
                    }
                    assert(overload_decl->kind == AST_DECL_FUNC);
                    assert(BUF_LENGTH(overload_decl->function.args) == 2);

                    auto index_arg_decl = overload_decl->function.args[1];
                    assert(index_arg_decl->kind == AST_DECL_MUTABLE);
                    auto index_arg_type = index_arg_decl->mutable_decl.type;
                    if (index_type != index_arg_type)
                    {
                        assert(index_type->flags & AST_TYPE_FLAG_INT);
                        assert(index_arg_type->flags & AST_TYPE_FLAG_INT);

                        resolver_transform_to_cast_expression(resolver, index_expr,
                                                              index_arg_type);
                    }



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

                if (result && expression->dot.declaration->kind == AST_DECL_FUNC_OVERLOAD)
                {
                    points_to_overload_decl = true;
                }
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

                expression->flags |= AST_EXPR_FLAG_CONST;
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
                auto ts = expression->sizeof_expr.type_spec;
                result &= resolver_resolve_type_spec(resolver, ts->file_pos, ts, &type, scope,
                                                     scope);
                if (!result) break;

                assert(type);
                expression->sizeof_expr.byte_size = type->bit_size / 8;
                expression->type = Builtin::type_s64;
                break;
            }

            case AST_EXPR_COMPOUND_LITERAL:
            {
                assert(suggested_type);

                bool all_members_const = true;

                if (suggested_type->kind == AST_TYPE_STRUCT)
                {
                    auto expected_mem_count =
                        BUF_LENGTH(suggested_type->aggregate_type.member_declarations);
                    auto got_mem_count = BUF_LENGTH(expression->compound_literal.expressions);
                    if (!(expected_mem_count == got_mem_count))
                    {
                        resolver_report_error(resolver, expression->file_pos,
                                              "Compount literal member count mismatch, expected %d, got %d",
                                              expected_mem_count, got_mem_count);
                        return false;
                    }

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

                        if (result)
                        {
                            all_members_const &=
                                (bool)(compound_expr->flags & AST_EXPR_FLAG_CONST);
                        }
                    }

                    if (!result) break;

                    expression->type = suggested_type;
                    if (all_members_const)
                    {
                        expression->flags |= AST_EXPR_FLAG_CONST;
                    }
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
                else
                {
                    result = false;
                }
                break;
            }

            case AST_EXPR_ARRAY_LENGTH:
            {
                result &= resolver_resolve_expression(resolver,
                                                      expression->array_length.ident_expr,
                                                      scope);
                if (result)
                {
                    expression->type = Builtin::type_s64;
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

                    if (expression->type->kind == AST_TYPE_POINTER)
                    {
                        expression->flags |= AST_EXPR_FLAG_POINTER_MATH;
                    }
                }
                break;
            }

            case AST_EXPR_GET_TYPE_INFO:
            {
                auto ts = expression->get_type_info_expr.type_spec;
                result &= resolver_resolve_type_spec(resolver, ts->file_pos, ts,
                                                     &expression->get_type_info_expr.type,
                                                     scope);

                if (result)
                {
                    expression->type = Builtin::type_pointer_to_Type_Info;
                    maybe_register_type_info(resolver->context,
                                              expression->get_type_info_expr.type);
                }
                break;
            }

            default: assert(false);
        }

        if (result)
        {
            assert(expression->type || points_to_import_decl || points_to_overload_decl);
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
            else if ((lhs->type->flags & AST_TYPE_FLAG_INT) &&
                     rhs->type->kind == AST_TYPE_ENUM &&
                     lhs->type == rhs->type->aggregate_type.base_type)
            {
                expression->type = lhs->type;
            }
            else if (lhs->type->flags & AST_TYPE_FLAG_INT &&
                     rhs->type->flags & AST_TYPE_FLAG_INT)
            {
                if (!(lhs->type->flags & AST_TYPE_FLAG_SIGNED) &&
                    rhs->type->flags & AST_TYPE_FLAG_SIGNED &&
                    rhs->type->bit_size > lhs->type->bit_size)
                {
                    resolver_transform_to_cast_expression(resolver, lhs, rhs->type);
                }
				else if ((lhs->type->flags & AST_TYPE_FLAG_SIGNED) ==
					(rhs->type->flags & AST_TYPE_FLAG_SIGNED) &&
					rhs->type->bit_size > lhs->type->bit_size)
				{
					resolver_transform_to_cast_expression(resolver, lhs, rhs->type);
				}
				else assert(false);
            }
            else
            {
                auto lhs_str = ast_type_to_string(lhs->type);
                auto rhs_str = ast_type_to_string(rhs->type);
                resolver_report_error(resolver, expression->file_pos,
                                      "Mismatching types in binary expression:\n\tLeft size: %s\n\tRight size: %s",
                                      lhs_str, rhs_str);
                mem_free(lhs_str);
                mem_free(rhs_str);
            }
        }

        if (!((lhs->type->flags & AST_TYPE_FLAG_INT) ||
              (lhs->type->flags & AST_TYPE_FLAG_FLOAT) ||
              lhs->type->kind == AST_TYPE_ENUM ||
              lhs->type == Builtin::type_bool ||
              (expression->flags & AST_EXPR_FLAG_POINTER_MATH) ||
              (binop_is_cmp(expression) && lhs->type->kind == AST_TYPE_POINTER &&
               rhs->type->kind == AST_TYPE_POINTER)))
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
            // assert(expression->type->kind == AST_TYPE_POINTER);
            // assert(expression->flags & AST_EXPR_FLAG_POINTER_MATH);
        }

        if (lhs->flags & AST_EXPR_FLAG_CONST && rhs->flags & AST_EXPR_FLAG_CONST)
        {
            expression->flags |= AST_EXPR_FLAG_CONST;

            if (expression->type->flags & AST_TYPE_FLAG_INT)
            {
                resolver_transform_const_expr_to_literal(resolver, expression, scope);
            }
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

        if (dot_expr->flags & AST_EXPR_FLAG_RESOLVED)
        {
            assert(dot_expr->dot.declaration);
        }
        else if (dot_expr->dot.declaration)
        {
            dot_expr->dot.declaration = nullptr;
        }

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

                if (!found)
                {
                    auto base_type_str = ast_type_to_string(base_type);
                    resolver_report_error(resolver, dot_expr->file_pos,
                                          "Aggregate type '%s' does not have a member named '%s'",
                                          base_type_str, member_expr->identifier->atom.data);
                    mem_free(base_type_str);
                    return false;
                }
            }
            else assert(false);
        }
        else if (base_decl->kind == AST_DECL_IMPORT)
        {
            assert(base_decl->import.module);

            AST_Module* ast_module = base_decl->import.module;

            result &= resolver_resolve_expression(resolver, member_expr,
                                                  ast_module->module_scope);

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

            if (!member_decl)
            {
                auto enum_type_str = ast_type_to_string(base_decl->aggregate_type.type);
                resolver_report_error(resolver, member_expr->file_pos,
                                      "Enum type %s does not have a member named %s",
                                      enum_type_str, member_expr->identifier->atom.data);
                mem_free(enum_type_str);
                result = false;
            }

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
            assert(dot_expr->type ||
                   (dot_expr->dot.declaration &&
                    dot_expr->dot.declaration->kind == AST_DECL_FUNC_OVERLOAD));
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

    bool resolver_resolve_type_spec(Resolver* resolver, File_Pos base_fp,
                                    AST_Type_Spec* type_spec, AST_Type** type_dest,
                                    AST_Scope* scope, AST_Scope* poly_scope /*=nullptr*/)
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
                AST_Identifier* ident = type_spec->identifier.identifier;
                result &= resolver_resolve_identifier(resolver, ident, scope);
                if (!result) break;

                assert(ident->declaration);
                AST_Declaration* decl = ident->declaration;

                if (type_spec->identifier.arg_type_specs &&
                    !(decl->kind == AST_DECL_AGGREGATE_TYPE &&
                      (decl->aggregate_type.kind == AST_AGG_DECL_STRUCT ||
                       decl->aggregate_type.kind == AST_AGG_DECL_UNION)))
                {
                    resolver_report_error(resolver, base_fp, "Type spec has polymorphic arguments, but it's declaration is not of struct/union type");
                    resolver_report_error(resolver, decl->file_pos, "\tDeclaration is here");
                }

                if (decl->kind == AST_DECL_AGGREGATE_TYPE &&
                    (decl->aggregate_type.kind == AST_AGG_DECL_STRUCT ||
                     decl->aggregate_type.kind == AST_AGG_DECL_UNION) &&
                    decl->aggregate_type.aggregate_decl->poly_args)
                {
                    assert(type_spec->identifier.arg_type_specs);
                    assert(poly_scope);
                    *type_dest = find_or_create_poly_aggregate_type(resolver, decl, type_spec,
                                                                    scope, poly_scope);
                    if (!(*type_dest))
                    {
                        resolver_report_error(resolver, type_spec->file_pos, "Failed to create polymorphic aggregate instance");
                        result = false;
                    }
                }
                else
                {
                    *type_dest = resolver_get_declaration_type(decl);
                }
                break;
            }

            case AST_TYPE_SPEC_POINTER:
            {
                AST_Type* base_type = nullptr;
                auto ts = type_spec->pointer.base;
                result &= resolver_resolve_type_spec(resolver, ts->file_pos, ts,
                                                     &base_type, scope, poly_scope);
                if (!result) break;

                assert(base_type);

                *type_dest = ast_find_or_create_pointer_type(resolver->context, base_type);
                break;
            }

            case AST_TYPE_SPEC_STATIC_ARRAY:
            {
                AST_Type* element_type = nullptr;
                auto ts = type_spec->static_array.base;
                result &= resolver_resolve_type_spec(resolver, ts->file_pos, ts, &element_type,
                                                     scope);
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
                    result &= resolver_resolve_type_spec(resolver, return_type_spec->file_pos,
                                                         return_type_spec, &return_type,
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
                    result &= resolver_resolve_type_spec(resolver,
                                                        base_fp,
                                                        type_spec->dot.member_type_spec,
                                                        &result_type, module->module_scope,
                                                        scope);
                    if (!result) break;
                    assert(result_type);
                    *type_dest = result_type;
                    type_spec->type = result_type;
                }

                break;
            }

            case AST_TYPE_SPEC_TYPEOF:
            {
                auto expr = type_spec->typeof_expr.expr;
                result &= resolver_resolve_expression(resolver, expr, scope);
                if (result)
                {
                    assert(expr->type);
                    *type_dest = expr->type;
                }
                break;
            }

            case AST_TYPE_SPEC_POLY_FUNC_ARG:
            {
                AST_Declaration* type_decl =
                    find_declaration(resolver->context, scope,
                                     type_spec->poly_func_arg.identifier);
                assert(type_decl);
                assert(type_decl->kind == AST_DECL_POLY_TYPE_SPEC);
                assert(type_decl->poly_type_spec.type_spec->type);

                *type_dest = type_decl->poly_type_spec.type_spec->type;

                //
                // Transform this type spec in to a regular identifier, so it will not
                //  match as poly later, since this is a specified poly instance
                //
                auto ident_temp = type_spec->poly_func_arg.identifier; // Probably not necessary
                type_spec->kind = AST_TYPE_SPEC_IDENT;
                type_spec->identifier.identifier = ident_temp;
                type_spec->identifier.arg_type_specs = nullptr;
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

            case AST_DECL_FUNC_OVERLOAD:
            {
                return nullptr;
            }

            case AST_DECL_POLY_TYPE_SPEC:
            {
                assert(decl->poly_type_spec.type_spec->type);
                return decl->poly_type_spec.type_spec->type;
            }

            default: assert(false);
        }
        assert(false);
        return nullptr;
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
        bool lhs_enum = lhs->kind == AST_TYPE_ENUM;
        bool rhs_enum = rhs->kind == AST_TYPE_ENUM;

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
        else if (lhs_integer && rhs_enum && lhs == rhs->aggregate_type.base_type)
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
        expr->cast_expr.type_spec = nullptr;
        expr->cast_expr.expr = expr_copy;
        expr->type = type;
    }

    AST_Declaration* resolver_transform_to_function_overload_decl(Resolver* resolver,
                                                                  AST_Declaration* declaration)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_FUNC);

        auto func_decl = ast_declaration_new(resolver->context, declaration->file_pos,
                                             declaration->kind, declaration->location,
                                             declaration->identifier, declaration->directive);

        func_decl->function = declaration->function;
        func_decl->flags |= AST_DECL_FLAG_FUNC_OVERLOAD;
        func_decl->flags |= declaration->flags;

        declaration->kind = AST_DECL_FUNC_OVERLOAD;
        declaration->function_overload.overloads = nullptr;
        declaration->function_overload.poly_templates = nullptr;

        return func_decl;
    }

    void resolver_transform_const_expr_to_literal(Resolver* resolver, AST_Expression* expr,
                                                  AST_Scope* scope)
    {
        assert(expr->flags & AST_EXPR_FLAG_CONST);
        assert((expr->type->flags & AST_TYPE_FLAG_INT) ||
               (expr->type->flags & AST_TYPE_FLAG_FLOAT));

        // if (expr->flags & AST_EXPR_FLAG_LITERAL)
        // {
        //     return;
        // }

        if (expr->kind == AST_EXPR_INTEGER_LITERAL || expr->kind == AST_EXPR_FLOAT_LITERAL)
        {
            assert(false);
        }

        if (expr->type->flags & AST_TYPE_FLAG_INT)
        {
            uint64_t int_lit = const_interpret_int_expression(resolver->context, expr,
                                                              expr->type, scope);
            expr->kind = AST_EXPR_INTEGER_LITERAL;
            expr->integer_literal.u64 = int_lit;
        }
        else if (expr->type->flags & AST_TYPE_FLAG_FLOAT)
        {
            float float_lit;
            double double_lit;

            if (expr->type == Builtin::type_float)
            {
                float_lit = const_interpret_float_expression(resolver->context, expr, scope);
                double_lit = (double)float_lit;
            }
            else
            {
                assert(false);
            }

            expr->kind = AST_EXPR_FLOAT_LITERAL;
            expr->float_literal.r32 = float_lit;
            expr->float_literal.r64 = double_lit;
        }

    }

    void resolver_add_overload(Resolver* resolver, AST_Declaration* overload_decl,
                               AST_Declaration* func_decl)
    {
        assert(resolver);
        assert(overload_decl);
        assert(overload_decl->kind == AST_DECL_FUNC_OVERLOAD);
        assert(func_decl);
        assert(func_decl->kind == AST_DECL_FUNC);

        if (func_decl->flags & AST_DECL_FLAG_FUNC_POLY_TEMPLATE)
        {
            auto poly_templates = overload_decl->function_overload.poly_templates;
            for (uint64_t i = 0; i < BUF_LENGTH(poly_templates); i++)
            {
                auto ex_poly_template = poly_templates[i];
                if (func_decl == ex_poly_template)
                {
                    assert(false);
                }
            }

            BUF_PUSH(overload_decl->function_overload.poly_templates, func_decl);
        }
        else
        {
            auto overloads = overload_decl->function_overload.overloads;
            for (uint64_t i = 0; i < BUF_LENGTH(overloads); i++)
            {
                auto ex_overload_decl = overloads[i];
                if (func_decl == ex_overload_decl)
                {
                    assert(false);
                }
            }

            BUF_PUSH(overload_decl->function_overload.overloads, func_decl);
            func_decl->flags |= AST_DECL_FLAG_FUNC_OVERLOAD;
        }
    }

    struct _Overload_Match
    {
        uint64_t distance = UINT64_MAX;
        uint64_t index = 0;
    };

    AST_Declaration* resolver_find_overload(Resolver* resolver, AST_Declaration* overload_decl,
                                            AST_Expression* call_expression,
                                            AST_Scope* call_scope)
    {
        assert(resolver);
        assert(overload_decl);
        assert(overload_decl->kind == AST_DECL_FUNC_OVERLOAD);
        assert(call_expression);
        assert(call_expression->kind == AST_EXPR_CALL);
        assert(call_scope);

        BUF(_Overload_Match) matches = nullptr;

        auto overloads = overload_decl->function_overload.overloads;
        for (uint64_t i = 0; i < BUF_LENGTH(overloads); i++)
        {
            AST_Declaration* overload = overloads[i];
            bool valid_match = true;
            uint64_t distance = resolver_get_overload_match_distance(resolver, overload,
                                                                     call_expression, call_scope,
                                                                     &valid_match, false);
            if (valid_match)
            {
                _Overload_Match match = { distance, i };
                BUF_PUSH(matches, match);
            }
        }

        uint64_t smallest_distance = UINT64_MAX;
        uint64_t smallest_distance_index = 0;
        for (uint64_t i = 0; i < BUF_LENGTH(matches); i++)
        {
            auto match = matches[i];
            if (match.distance < smallest_distance)
            {
                smallest_distance = match.distance;
                smallest_distance_index = match.index;
            }
        }

        uint64_t match_count = 0;
        for (uint64_t i = 0; i < BUF_LENGTH(matches); i++)
        {
            auto match = matches[i];
            if (smallest_distance == match.distance)
            {
                match_count += 1;
            }
        }

        if (match_count == 0 && overload_decl->function_overload.poly_templates)
        {
            bool report_failure;
            AST_Declaration* poly_decl =
                create_poly_function_declaration(resolver, overload_decl, call_expression,
                                                 call_scope, &report_failure);
            if (!poly_decl)
            {
                if (report_failure)
                    resolver_report_error(resolver, call_expression->file_pos,
                                          "Could not create a polymorphic function instance");
                return nullptr;
            }
            else
            {
                return poly_decl;
            }
        }
        else if (match_count == 0)
        {
            resolver_report_error(resolver, call_expression->file_pos,
                                  "No suitable (polymorphic) overload was found");
        }
        else assert(match_count == 1);

        BUF_FREE(matches);

        return overloads[smallest_distance_index];
    }

    uint64_t resolver_get_overload_match_distance(Resolver* resolver, AST_Declaration* func_decl,
                                                  AST_Expression* call_expression,
                                                  AST_Scope* call_scope, bool* valid_match,
                                                  bool allow_poly_templates)
    {
        assert(resolver);
        assert(func_decl);
        assert(func_decl->kind == AST_DECL_FUNC);
        assert(call_expression);
        assert(call_expression->kind == AST_EXPR_CALL);
        assert(call_scope);
        assert(valid_match);
        assert(*valid_match);

        if ((func_decl->flags & AST_DECL_FLAG_RESOLVING) && !allow_poly_templates)
        {
            *valid_match = false;
            return UINT64_MAX;
        }

        if (!(func_decl->flags & AST_DECL_FLAG_RESOLVED) && !allow_poly_templates)
        {
            *valid_match = false;
            return UINT64_MAX;
        }

        assert((func_decl->flags & AST_DECL_FLAG_RESOLVED) || allow_poly_templates);

        if (BUF_LENGTH(func_decl->function.args) !=
            BUF_LENGTH(call_expression->call.arg_expressions))
        {
            *valid_match = false;
            return UINT64_MAX;
        }

        auto arg_decls = func_decl->function.args;
        auto arg_exprs = call_expression->call.arg_expressions;

        uint64_t distance = 0;

        for (uint64_t i = 0; i < BUF_LENGTH(arg_decls); i++)
        {
            AST_Declaration* arg_decl = arg_decls[i];
            AST_Expression* arg_expr = arg_exprs[i];

            assert(arg_decl->kind == AST_DECL_MUTABLE);
            auto arg_ts = arg_decl->mutable_decl.type_spec;
            // if (arg_ts->kind == AST_TYPE_SPEC_IDENTIFIER &&
            //     type_spec_matches_poly_arg(resolver, func_decl, arg_ts))
            // {
            //     assert(false);
            // }

            auto poly_flags = (AST_TYPE_SPEC_POLY_FUNC_ARG |
                               AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN |
                               AST_TYPE_SPEC_FLAG_IS_POLY_ARG_MATCH |
                               AST_TYPE_SPEC_FLAG_HAS_POLY_ARG_MATCH_CHILDREN);
            if (allow_poly_templates && ((arg_ts->flags & poly_flags) ||
                                         arg_ts->kind == AST_TYPE_SPEC_POLY_FUNC_ARG))
            {
                if (!(arg_expr->flags & AST_EXPR_FLAG_RESOLVED))
                {
                    bool arg_res = resolver_resolve_expression(resolver, arg_expr, call_scope);
                    if (!arg_res)
                    {
                        *valid_match = false;
                        return UINT64_MAX;
                    }
                }
                bool success = true;
                distance += get_poly_overload_match_distance(resolver, arg_ts, arg_expr->type,
                                                             &success);
                if (!success)
                {
                    *valid_match = false;
                    break;
                }
                continue;
            }
            bool arg_result = resolver_resolve_declaration(resolver, arg_decl,
                                                           func_decl->function.argument_scope);
            AST_Type* suggested_type = nullptr;
            if (arg_result)
            {
                suggested_type = arg_decl->mutable_decl.type;
            }

            if (arg_expr->flags & AST_EXPR_FLAG_RESOLVED)
            {
                assert(arg_expr->type);
                assert(suggested_type);
                if (arg_expr->type != suggested_type)

                {
                    distance += 5;
                }
                arg_expr->flags &= ~AST_EXPR_FLAG_RESOLVED;
            }

            bool expr_result = resolver_resolve_expression(resolver, arg_expr, call_scope,
                                                           suggested_type);

            if (arg_result && expr_result)
            {
                bool valid_arg_match = true;
                distance += resolver_get_type_match_distance(resolver,
                                                             arg_decl->mutable_decl.type,
                                                             arg_expr->type, &valid_arg_match);
                if (!valid_arg_match)
                {
                    *valid_match = false;
                    break;
                }
            }
            else
            {
                *valid_match = false;
                return UINT64_MAX;
            }
        }

        return distance;
    }

    uint64_t resolver_get_type_match_distance(Resolver* resolver, AST_Type* lhs, AST_Type* rhs,
                                              bool* valid_match)
    {
        assert(resolver);
        assert(lhs);
        assert(rhs);
        assert(valid_match);
        assert(*valid_match);

        if (lhs == rhs)
        {
            return 0;
        }

        if (resolver_check_assign_types(resolver, lhs, rhs))
        {
            return 1;
        }

        if (lhs == Builtin::type_String && rhs == Builtin::type_pointer_to_u8)
        {
            assert(false);
            return 1;
        }

        *valid_match = false;
        return 2;
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

        AST_Expression* old_expr = ast_expression_new(resolver->context,
                                                      string_lit_expr->file_pos,
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

            assert(Builtin::decl_string_length->identifier);
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

        bool result = true;

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

        bool new_decl_res = resolver_resolve_declaration(resolver, new_decl, scope);
        assert(new_decl_res);

        new_decl->flags |= AST_DECL_FLAG_REPLACED_NESTED_AGGREGATE;
    }

    bool match_builtin_function(Resolver* resolver, AST_Expression* call_expr, AST_Scope* scope)
    {
        assert(resolver);
        assert(call_expr);
        assert(call_expr->kind == AST_EXPR_CALL);
        assert(scope);

        auto ident_expr = call_expr->call.ident_expression;
        assert(ident_expr->kind == AST_EXPR_IDENTIFIER);
        const Atom& ident_atom = ident_expr->identifier->atom;

        bool result = false;


		if (ident_atom == Builtin::atom___create_thread__ ||
			ident_atom == Builtin::atom___join_thread__)
		{

			auto thread_struct_decl = find_declaration(resolver->context, scope,
													   Builtin::identifier_Thread);
			if (!thread_struct_decl) return false;

			assert(thread_struct_decl->flags & AST_DECL_FLAG_RESOLVED);
			assert(thread_struct_decl->kind == AST_DECL_AGGREGATE_TYPE);
			assert(thread_struct_decl->aggregate_type.kind == AST_AGG_DECL_STRUCT);
			assert(thread_struct_decl->aggregate_type.type);
			assert(thread_struct_decl->aggregate_type.type->kind == AST_TYPE_STRUCT);

			auto thread_type = thread_struct_decl->aggregate_type.type;

			if (ident_atom == Builtin::atom___create_thread__)
			{
				call_expr->call.builtin_function = AST_BUILTIN_FUNC_CREATE_THREAD;

				if (thread_struct_decl)
				{
					if (!Builtin::type_Thread)
					{
						Builtin::type_Thread = thread_type;
						Builtin::type_pointer_to_Thread =
							ast_find_or_create_pointer_type(resolver->context, thread_type);
					}

					assert(BUF_LENGTH(call_expr->call.arg_expressions) == 2);
					auto arg_0 = call_expr->call.arg_expressions[0];
					auto arg_1 = call_expr->call.arg_expressions[1];

					bool arg_result = resolver_resolve_expression(resolver, arg_0, scope);
					arg_result &= resolver_resolve_expression(resolver, arg_1, scope);

					if (arg_result)
					{
						assert(arg_0->type->kind == AST_TYPE_FUNCTION ||
							(arg_0->type->kind == AST_TYPE_POINTER &&
								arg_0->type->pointer.base->kind == AST_TYPE_FUNCTION));
						assert(arg_1->type == Builtin::type_pointer_to_void);

						call_expr->type = thread_type;
						result = true;
					}
				}
				else assert(false);
			}
			else if (ident_atom == Builtin::atom___join_thread__)
			{
				call_expr->call.builtin_function = AST_BUILTIN_FUNC_JOIN_THREAD;

				assert(BUF_LENGTH(call_expr->call.arg_expressions) == 1);
				auto arg_0 = call_expr->call.arg_expressions[0];

				bool arg_result = resolver_resolve_expression(resolver, arg_0, scope);

				if (arg_result)
				{
					assert(arg_0->type == thread_type);
					call_expr->type = Builtin::type_pointer_to_void;
					result = true;
				}

			}
		}
        else if (ident_atom == Builtin::atom___compare_and_swap__)
        {
            call_expr->call.builtin_function = AST_BUILTIN_FUNC_COMPARE_AND_SWAP;

            assert(BUF_LENGTH(call_expr->call.arg_expressions) == 3);
            auto arg_0 = call_expr->call.arg_expressions[0];
            auto arg_1 = call_expr->call.arg_expressions[1];
            auto arg_2 = call_expr->call.arg_expressions[2];

            bool arg_result = resolver_resolve_expression(resolver, arg_0, scope);
            arg_result &= resolver_resolve_expression(resolver, arg_1, scope);
            arg_result &= resolver_resolve_expression(resolver, arg_2, scope);

            if (arg_result)
            {
                assert(arg_0->type == Builtin::type_pointer_to_u64);
                assert(arg_1->type == Builtin::type_u64);
                assert(arg_2->type == Builtin::type_u64);
                call_expr->type = Builtin::type_bool;
                result = true;
            }
        }

        return result;
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

            if (decl_to_emit)
            {
                result &= resolver_push_static_declarations_to_scope(resolver, decl_to_emit,
                                                                     scope);
                if (decl_to_emit->kind != AST_DECL_STATIC_IF &&
                    decl_to_emit->kind != AST_DECL_BLOCK)
                {
                    result &= resolver_resolve_declaration(resolver, decl_to_emit, scope);
                    assert(scope->flags & AST_SCOPE_FLAG_IS_MODULE_SCOPE);
                    BUF_PUSH(resolver->module->global_declarations, decl_to_emit);
                }
            }
        }

        return result;
    }

    void resolver_push_declaration_to_scope(Resolver* resolver, AST_Declaration* decl,
                                            AST_Scope* scope)
    {
        assert(resolver);
        assert(decl);
        assert(scope);

        bool push = true;

        if (decl->identifier)
        {
            auto redecl = find_declaration(resolver->context, scope, decl->identifier);

            if (decl->kind == AST_DECL_FUNC && (decl->flags & AST_DECL_FLAG_FUNC_POLY_TEMPLATE))
            {
                AST_Declaration* original = nullptr;
                if (!redecl)
                {
                    original = resolver_transform_to_function_overload_decl(resolver, decl);
                }
                else
                {
                    original = decl;
                    push = false;
                }
                assert(original);
                bool res = resolve_poly_func_decl(resolver, original, scope);
                assert(res);
                BUF_PUSH(decl->function_overload.poly_templates, original);
            }

            if (redecl)
            {
                if (decl->kind == AST_DECL_FUNC && redecl->kind == AST_DECL_FUNC)
                {
                    auto original = resolver_transform_to_function_overload_decl(resolver,
                                                                                 redecl);
                    resolver_add_overload(resolver, redecl, original);
                    resolver_add_overload(resolver, redecl, decl);
                }
                else if (decl->kind == AST_DECL_FUNC && redecl->kind == AST_DECL_FUNC_OVERLOAD)
                {
                    resolver_add_overload(resolver, redecl, decl);
                }
                else
                {
                    auto fmt = "Redeclaration of identifier: %s\n\tPrevious declaration is here: %s:%" PRIu64 ":%" PRIu64;
                    resolver_report_error(resolver, decl->file_pos, fmt,
                                          decl->identifier->atom.data,
                                          redecl->file_pos.file_name, redecl->file_pos.line,
                                          redecl->file_pos.line_relative_char_pos);
                }
                push = false;
            }
        }

        if (push)
        {
            ast_scope_push_declaration(scope, decl);
        }
    }

    bool resolver_push_static_declarations_to_scope(Resolver* resolver,
                                                    AST_Declaration* decl_to_emit,
                                                    AST_Scope* scope)
    {
        assert(resolver);
        assert(decl_to_emit);
        assert(scope);

        bool res = true;

        if (decl_to_emit->kind == AST_DECL_BLOCK)
        {
            for (uint64_t i = 0; i < BUF_LENGTH(decl_to_emit->block.decls); i++)
            {
                auto block_decl = decl_to_emit->block.decls[i];
                res &= resolver_push_static_declarations_to_scope(resolver, block_decl, scope);
            }
        }
        else if (decl_to_emit->kind == AST_DECL_STATIC_IF)
        {
            res = resolver_resolve_static_if_declaration(resolver, decl_to_emit, scope);
        }
        else
        {
            resolver_push_declaration_to_scope(resolver, decl_to_emit, scope);
            assert(scope->flags & AST_SCOPE_FLAG_IS_MODULE_SCOPE);
            BUF_PUSH(resolver->module->global_declarations, decl_to_emit);
        }

        return res;
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
			else if (ud->kind == AST_DECL_AGGREGATE_TYPE &&
				     ud->aggregate_type.kind == AST_AGG_DECL_STRUCT)
			{
				if (ud->identifier && ud->identifier->atom == identifier->atom)
				{
					return ud;
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
            case AST_BINOP_LSHIFT:
            case AST_BINOP_RSHIFT:
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


        // @HACK: Overwrite ZODIAC_RUNNING_BYTECODE temporarily
        auto zrb_decl = Builtin::decl_ZODIAC_RUNNING_BYTECODE;
        auto bool_lit_expr = zrb_decl->constant_var.init_expression;
        bool old_zrb_val = bool_lit_expr->bool_literal.boolean;
        zrb_decl->constant_var.init_expression->bool_literal.boolean = true;

        IR_Module ir_module = ir_builder_emit_module(&ir_builder, resolver->module, false);
        resolver_clean_module_after_emit(resolver, resolver->module);

        zrb_decl->constant_var.init_expression->bool_literal.boolean = old_zrb_val;

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

    void resolver_clean_module_after_emit(Resolver* resolver, AST_Module* module)
    {
        assert(resolver);
        assert(module);

        for (uint64_t i = 0; i < BUF_LENGTH(module->global_declarations); i++)
        {
            AST_Declaration* global_decl = module->global_declarations[i];
            resolver_clean_declaration_after_emit(resolver, global_decl);
        }

        for (uint64_t i = 0; i < BUF_LENGTH(module->import_modules); i++)
        {
            AST_Module* import_module = module->import_modules[i];
            resolver_clean_module_after_emit(resolver, import_module);
        }
    }

    void resolver_clean_declaration_after_emit(Resolver* resolver, AST_Declaration* declaration)
    {
        assert(resolver);
        assert(declaration);

        switch (declaration->kind)
        {
            case AST_DECL_FUNC:
            {
                declaration->function.body_generated = false;
                break;
            }

            case AST_DECL_FUNC_OVERLOAD:
            {
                for (uint64_t i = 0; i < BUF_LENGTH(declaration->function_overload.overloads); i++)
                {
                    AST_Declaration* overload = declaration->function_overload.overloads[i];
                    resolver_clean_declaration_after_emit(resolver, overload);
                }
                break;
            }

            case AST_DECL_STATIC_IF:
            {
                resolver_clean_declaration_after_emit(resolver,
                                                        declaration->static_if.then_declaration);
                resolver_clean_declaration_after_emit(resolver,
                                                      declaration->static_if.else_declaration);
                break;
            }

            case AST_DECL_BLOCK:
            {
                for (uint64_t i = 0; i < BUF_LENGTH(declaration->block.decls);  i++)
                {
                    AST_Declaration* block_mem = declaration->block.decls[i];
                    resolver_clean_declaration_after_emit(resolver, block_mem);
                }
                break;
            }

            case AST_DECL_MUTABLE:
            case AST_DECL_CONSTANT_VAR:
            case AST_DECL_IMPORT:
            case AST_DECL_USING:
            case AST_DECL_INSERT:
            case AST_DECL_TYPEDEF:
            case AST_DECL_DYN_LINK:
            case AST_DECL_STATIC_ASSERT:
            case AST_DECL_AGGREGATE_TYPE:
                break;

            default: assert(false);
        }
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
            auto ts = decl->function.return_type_spec;
            result &= resolver_resolve_type_spec(resolver, ts->file_pos, ts, &new_ret_type,
                                                 decl->function.argument_scope);

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
