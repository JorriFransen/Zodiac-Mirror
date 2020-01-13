#include "polymorph.h"
#include "resolver.h"
#include "copier.h"

namespace Zodiac
{
    AST_Type* find_or_create_poly_aggregate_type(Resolver* resolver, AST_Declaration* poly_decl,
                                                 AST_Type_Spec* instance_type_spec,
                                                 AST_Scope* scope, AST_Scope* poly_scope)
    {
        assert(resolver);
        assert(poly_decl);
        assert(poly_decl->kind == AST_DECL_AGGREGATE_TYPE);
        assert(poly_decl->aggregate_type.kind == AST_AGG_DECL_STRUCT ||
               poly_decl->aggregate_type.kind == AST_AGG_DECL_UNION);
        assert(poly_decl->aggregate_type.aggregate_decl->poly_args);
        assert(instance_type_spec);
        assert(instance_type_spec->kind == AST_TYPE_SPEC_IDENT);
        assert(instance_type_spec->identifier.arg_type_specs);
        assert(scope);
        assert(poly_scope);

        BUF(AST_Identifier*) poly_args = poly_decl->aggregate_type.aggregate_decl->poly_args;
        BUF(AST_Type_Spec*) instance_type_specs = instance_type_spec->identifier.arg_type_specs;

        assert(BUF_LENGTH(poly_args) == BUF_LENGTH(instance_type_specs));

        bool instance_arg_result = true;

        for (uint64_t i = 0; i < BUF_LENGTH(instance_type_specs); i++)
        {
            AST_Type_Spec* instance_type_spec = instance_type_specs[i];

            AST_Type* instance_type = nullptr;
            bool result = resolver_resolve_type_spec(resolver, instance_type_spec->file_pos,
                                                     instance_type_spec, &instance_type,
                                                     poly_scope);
            instance_arg_result &= result;
        }

        if (!instance_arg_result) return nullptr;

        auto ex_instances = poly_decl->aggregate_type.aggregate_decl->poly_instances;
        for (uint64_t i = 0; i < BUF_LENGTH(ex_instances); i++)
        {
            AST_Aggregate_Poly ap = ex_instances[i];
            assert(BUF_LENGTH(ap.types) == BUF_LENGTH(instance_type_specs));

            bool match = true;
            for (uint64_t j = 0; j < BUF_LENGTH(ap.types); j++)
            {
                AST_Type* ap_type = ap.types[j];
                if (ap_type != instance_type_specs[j]->type)
                {
                    match = false;
                    break;
                }
            }

            if (match)
            {
                assert(ap.instance->aggregate_type.type);
                return ap.instance->aggregate_type.type;
            }
        }

        AST_Declaration* instance = copy_declaration(resolver->context, poly_decl,
                                                     COPY_FLAG_DONT_COPY_POLY);

        AST_Scope* instance_scope = instance->aggregate_type.scope;
        insert_poly_scope(resolver, instance_scope, poly_args, instance_type_specs);
        bool instance_result = resolver_resolve_declaration(resolver, instance, instance_scope);

        if (instance_result)
        {
            assert(instance->aggregate_type.type);

            AST_Aggregate_Poly ap = {};
            ap.instance = instance;
            for (uint64_t i = 0; i < BUF_LENGTH(instance_type_specs); i++)
            {
                BUF_PUSH(ap.types, instance_type_specs[i]->type);
            }
            BUF_PUSH(poly_decl->aggregate_type.aggregate_decl->poly_instances, ap);
            instance->aggregate_type.type->aggregate_type.poly_from = poly_decl;
            return instance->aggregate_type.type;
        }
        else
        {
            return nullptr;
        }
    }

    AST_Declaration* create_poly_function_declaration(Resolver* resolver,
                                                      AST_Declaration* overload_decl,
                                                      AST_Expression* call_expression,
                                                      AST_Scope* scope, bool* report_failure)
    {
        assert(resolver);
        assert(overload_decl);
        assert(overload_decl->kind == AST_DECL_FUNC_OVERLOAD);
        assert(overload_decl->function_overload.poly_templates);
        assert(report_failure);

        *report_failure = true;

        assert(overload_decl->scope);
        auto module = overload_decl->scope->module;
        assert(module);

        auto matches = collect_poly_function_matches(resolver, overload_decl, call_expression,
                                                     scope);

        if (!matches.matches)
        {
            resolver_report_error(resolver, call_expression->file_pos, "No poly match found");
            return nullptr;
        }

        auto shortest_distance = matches.shortest_distance_index;
        AST_Declaration* best_match = matches.matches[shortest_distance].poly_template;
        assert(best_match->flags & AST_DECL_FLAG_FUNC_POLY_TEMPLATE);

        if (best_match->flags & AST_DECL_FLAG_FUNC_POLY_TEMPLATE_ERROR)
        {
            free_poly_function_matches(matches);
            *report_failure = false;
            return nullptr;
        }

        AST_Declaration* instance = copy_declaration(resolver->context, best_match);
        instance->flags |= AST_DECL_FLAG_FUNC_POLY;
        assert(!(instance->flags & AST_DECL_FLAG_FUNC_POLY_TEMPLATE));

        insert_poly_scope(resolver, instance->function.argument_scope, instance, call_expression,
                          scope);

        convert_value_poly_args(resolver, instance, call_expression,
                                instance->function.argument_scope);

        bool res = resolver_resolve_declaration(resolver, instance, scope);

        free_poly_function_matches(matches);

        if (res)
        {
            BUF_PUSH(overload_decl->function_overload.overloads, instance);
            module->poly_dirty = true;
            return instance;
        }
        else
        {
            best_match->flags |= AST_DECL_FLAG_FUNC_POLY_TEMPLATE_ERROR;
            return nullptr;
        }
    }

    uint64_t get_poly_overload_match_distance(Resolver* resolver, AST_Type_Spec* arg_type_spec,
                                              AST_Type* arg_type, bool* success)
    {
        assert(resolver);
        assert(arg_type_spec);
        assert(arg_type);
        assert(success);
        assert(*success);

        if (arg_type_spec->kind == AST_TYPE_SPEC_POLY_FUNC_ARG)
        {
            return 0;
        }
        else if (arg_type_spec->flags & AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN)
        {
            switch (arg_type_spec->kind)
            {
                case AST_TYPE_SPEC_POINTER:
                {
                    if (arg_type->kind != AST_TYPE_POINTER)
                    {
                        *success = false;
                        return 10;
                    }
                    return get_poly_overload_match_distance(resolver, arg_type_spec->pointer.base,
                                                            arg_type->pointer.base, success);
                }

                case AST_TYPE_SPEC_IDENT:
                {
                    assert(arg_type_spec->identifier.arg_type_specs);
                    if (!(arg_type->kind == AST_TYPE_STRUCT || arg_type->kind == AST_TYPE_UNION))
                    {
                        *success = false;
                        return 10;
                    }

                    if (!arg_type->aggregate_type.poly_from)
                    {
                        *success = false;
                        return 10;
                    }
                    AST_Declaration* poly_decl = arg_type->aggregate_type.poly_from;
                    assert(poly_decl->kind == AST_DECL_AGGREGATE_TYPE);

                    assert(BUF_LENGTH(poly_decl->aggregate_type.aggregate_decl->poly_args) ==
                           BUF_LENGTH(arg_type_spec->identifier.arg_type_specs));
                    return 0;
                    break;
                }

                default: assert(false);
            }
        }
        else if (arg_type_spec->flags & AST_TYPE_SPEC_FLAG_IS_POLY_ARG_MATCH)
        {
            return 1;
        }
        else assert(false);

		assert(false);
		return 0;
    }

    // TODO: Make aggregate polymorphism call this with types instead of type specs, so it's the
    //        same as functions.
    void insert_poly_scope(Resolver* resolver, AST_Scope* scope, BUF(AST_Identifier*) poly_idents,
                           BUF(AST_Type_Spec*) poly_type_specs)
    {
        assert(resolver);
        assert(scope);
        assert(scope->parent);

        auto ident_count = BUF_LENGTH(poly_idents);
        auto ts_count = BUF_LENGTH(poly_type_specs);

        assert(ident_count == ts_count);

        AST_Scope* poly_scope = ast_scope_new(resolver->context, scope->parent, scope->module,
                                              false, scope->line);
        poly_scope->flags |= AST_SCOPE_FLAG_IS_POLY_SCOPE;
        scope->parent = poly_scope;

        for (uint64_t i = 0; i < ident_count; i++)
        {
            auto ident = poly_idents[i];
            auto ts = poly_type_specs[i];
            AST_Declaration* poly_ts_decl =  ast_poly_type_spec_declaration_new(resolver->context,
                                                                                ident->file_pos,
                                                                                ident, ts);
            bool res = resolver_resolve_declaration(resolver, poly_ts_decl, scope);
            assert(res);
            ast_scope_push_declaration(poly_scope, poly_ts_decl);
        }
    }

    void insert_poly_scope(Resolver* resolver, AST_Scope* scope, AST_Declaration* func_decl,
                           AST_Expression* call_expr, AST_Scope* call_scope)
    {
        assert(resolver);
        assert(scope);
        assert(func_decl);
        assert(func_decl->kind == AST_DECL_FUNC);
        assert(call_expr);
        assert(call_expr->kind == AST_EXPR_CALL);
        assert(call_scope);

        uint64_t arg_decl_count = BUF_LENGTH(func_decl->function.args);
        uint64_t arg_expr_count = BUF_LENGTH(call_expr->call.arg_expressions);
        assert(arg_decl_count == arg_expr_count);

        Poly_Function_Args poly_func_args = {};
        for (uint64_t i = 0; i < arg_decl_count; i++)
        {
            AST_Declaration* arg_decl = func_decl->function.args[i];
            AST_Expression* arg_expr = call_expr->call.arg_expressions[i];

            assert(arg_decl->kind == AST_DECL_MUTABLE);
            AST_Type_Spec* arg_decl_ts = arg_decl->mutable_decl.type_spec;

            bool arg_res = resolver_resolve_expression(resolver, arg_expr, call_scope);
            assert(arg_res);
            assert(arg_expr->type);
            AST_Type* arg_type = arg_expr->type;

            collect_poly_func_arg(resolver, &poly_func_args, arg_decl_ts, arg_type,
                                  arg_expr->file_pos, scope);
        }

        insert_poly_scope(resolver, scope, poly_func_args.identifiers, poly_func_args.type_specs);

        BUF_FREE(poly_func_args.identifiers);
        BUF_FREE(poly_func_args.type_specs);
    }

    Poly_Function_Matches collect_poly_function_matches(Resolver* resolver,
                                                        AST_Declaration* overload_decl,
                                                        AST_Expression* call_expression,
                                                        AST_Scope* scope)
    {
        assert(resolver);
        assert(overload_decl);
        assert(overload_decl->kind == AST_DECL_FUNC_OVERLOAD);
        assert(call_expression);
        assert(call_expression->kind == AST_EXPR_CALL);
        assert(scope);

        uint64_t smallest_distance = UINT64_MAX;
        uint64_t smallest_distance_index = 0;

        BUF(Poly_Function_Match) matches = nullptr;

        auto poly_templates = overload_decl->function_overload.poly_templates;
        for (uint64_t i = 0; i < BUF_LENGTH(poly_templates); i++)
        {
            AST_Declaration* poly_template = poly_templates[i];
            bool valid_match = true;
            uint64_t distance = resolver_get_overload_match_distance(resolver, poly_template,
                                                                     call_expression, scope,
                                                                     &valid_match, true);
            if (valid_match)
            {
                Poly_Function_Match match = { distance, poly_template };
                BUF_PUSH(matches, match);

                if (distance < smallest_distance)
                {
                    smallest_distance = distance;
                    smallest_distance_index = BUF_LENGTH(matches) - 1;
                }
            }
        }

        uint64_t match_count = 0;
        for (uint64_t i = 0; i < BUF_LENGTH(matches); i++)
        {
            if (smallest_distance == matches[i].distance) match_count++;
        }

        return { matches, smallest_distance, smallest_distance_index };
    }

    void free_poly_function_matches(Poly_Function_Matches matches)
    {
        assert(matches.matches);
        BUF_FREE(matches.matches);
    }

    void collect_poly_func_arg(Resolver* resolver, Poly_Function_Args* pfa,
                               AST_Type_Spec* arg_decl_ts, AST_Type* arg_type,
                               File_Pos arg_expr_fp, AST_Scope* scope)
    {
        assert(resolver);
        assert(pfa);
        assert(arg_decl_ts);
        assert(arg_type);
        assert(scope);

        if (arg_decl_ts->kind == AST_TYPE_SPEC_POLY_FUNC_ARG)
        {
            BUF_PUSH(pfa->identifiers, arg_decl_ts->poly_func_arg.identifier);
            auto type_spec = ast_type_spec_from_type_new(resolver->context, arg_expr_fp, arg_type);
            BUF_PUSH(pfa->type_specs, type_spec);
        }
        else if (arg_decl_ts->flags & AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN)
        {
            switch (arg_decl_ts->kind)
            {
                case AST_TYPE_SPEC_POINTER:
                {
                    assert(arg_type->kind == AST_TYPE_POINTER);
                    collect_poly_func_arg(resolver, pfa, arg_decl_ts->pointer.base,
                                        arg_type->pointer.base, arg_expr_fp, scope);
                    break;
                }

                case AST_TYPE_SPEC_IDENT:
                {
                    assert(arg_decl_ts->identifier.arg_type_specs);
                    assert(arg_type->kind == AST_TYPE_STRUCT ||
                           arg_type->kind == AST_TYPE_UNION);
                    assert(arg_type->aggregate_type.poly_from);
                    AST_Declaration* poly_decl = arg_type->aggregate_type.poly_from;
                    assert(poly_decl->kind == AST_DECL_AGGREGATE_TYPE);
                    assert(poly_decl->aggregate_type.aggregate_decl->poly_args);

                    assert(BUF_LENGTH(poly_decl->aggregate_type.aggregate_decl->poly_args) ==
                           BUF_LENGTH(arg_decl_ts->identifier.arg_type_specs));

                    auto arg_type_specs = arg_decl_ts->identifier.arg_type_specs;
                    auto poly_arg_idents = poly_decl->aggregate_type.aggregate_decl->poly_args;

                    for (uint64_t i = 0; i < BUF_LENGTH(arg_type_specs); i++)
                    {
                        auto arg_ts = arg_type_specs[i];
                        auto poly_arg_ident = poly_arg_idents[i];

                        auto poly_arg_type_decl = find_declaration(resolver->context,
                                                                   arg_type->aggregate_type.scope,
                                                                   poly_arg_ident);
                        auto poly_arg_type = resolver_get_declaration_type(poly_arg_type_decl);

                        collect_poly_func_arg(resolver, pfa, arg_ts, poly_arg_type, arg_expr_fp,
                                              scope);
                    }

                    break;
                }

                default: assert(false);
            }
        }
        // else if (arg_decl_ts->flags & AST_TYPE_SPEC_FLAG_IS_POLY_ARG_MATCH)
        // {
        //     assert(false);
        // }
        // else if (arg_decl_ts->flags & AST_TYPE_SPEC_FLAG_HAS_POLY_ARG_MATCH_CHILDREN)
        // {
        //     assert(false);
        // }
        // else assert(false);
    }

    bool resolve_poly_func_decl(Resolver* resolver, AST_Declaration* func_decl, AST_Scope* scope)
    {
        assert(resolver);
        assert(func_decl);
        assert(func_decl->kind == AST_DECL_FUNC);
        assert(func_decl->flags & AST_DECL_FLAG_FUNC_POLY_TEMPLATE);
        assert(scope);

        Poly_Func_Arg_Idents pfi = {};

        for (uint64_t i = 0; i < BUF_LENGTH(func_decl->function.args); i++)
        {
            AST_Declaration* arg_decl = func_decl->function.args[i];
            assert(arg_decl->kind == AST_DECL_MUTABLE);
            AST_Type_Spec* arg_ts = arg_decl->mutable_decl.type_spec;
            collect_poly_func_arg_idents(&pfi, arg_ts);
        }

        for (uint64_t i = 0; i < BUF_LENGTH(func_decl->function.args); i++)
        {
            auto arg_decl = func_decl->function.args[i];
            auto arg_ts = arg_decl->mutable_decl.type_spec;
            match_poly_func_arg_idents(&pfi, arg_ts);
        }

        BUF_FREE(pfi.args);

        return true;
    }

    void collect_poly_func_arg_idents(Poly_Func_Arg_Idents* pfi, AST_Type_Spec* arg_ts)
    {
        assert(pfi);
        assert(arg_ts);

        if (arg_ts->kind == AST_TYPE_SPEC_POLY_FUNC_ARG)
        {
            BUF_PUSH(pfi->args, arg_ts->poly_func_arg.identifier);
        }
        else if (arg_ts->flags & AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN)
        {
            switch (arg_ts->kind)
            {
                case AST_TYPE_SPEC_POINTER:
                {
                    collect_poly_func_arg_idents(pfi, arg_ts->pointer.base);
                    break;
                }

                case AST_TYPE_SPEC_IDENT:
                {
                    for (uint64_t i = 0; i < BUF_LENGTH(arg_ts->identifier.arg_type_specs); i++)
                    {
                        AST_Type_Spec* ident_arg_ts = arg_ts->identifier.arg_type_specs[i];
                        collect_poly_func_arg_idents(pfi, ident_arg_ts);
                    }
                    break;
                }

                default: assert(false);
            }
        }
    }

    void inherit_poly_arg_match(AST_Type_Spec* type_spec, AST_Type_Spec* base)
    {
        if ((base->flags & AST_TYPE_SPEC_FLAG_IS_POLY_ARG_MATCH) ||
            (base->flags & AST_TYPE_SPEC_FLAG_HAS_POLY_ARG_MATCH_CHILDREN))
        {
            assert(false);
        }
    }

    void match_poly_func_arg_idents(Poly_Func_Arg_Idents* pfi, AST_Type_Spec* arg_ts)
    {
        assert(pfi);
        assert(arg_ts);

        switch (arg_ts->kind)
        {
            case AST_TYPE_SPEC_POINTER:
            {
                auto base = arg_ts->pointer.base;
                match_poly_func_arg_idents(pfi, base);
                inherit_poly_arg_match(arg_ts, base);
                break;
            }

            case AST_TYPE_SPEC_IDENT:
            {
                if (is_poly_func_arg_match(pfi, arg_ts->identifier.identifier))
                {
                    arg_ts->flags |= AST_TYPE_SPEC_FLAG_IS_POLY_ARG_MATCH;
                }

                auto ident_arg_type_specs = arg_ts->identifier.arg_type_specs;
                for (uint64_t i = 0; i < BUF_LENGTH(ident_arg_type_specs); i++)
                {
                    auto ident_arg_ts = ident_arg_type_specs[i];
                    match_poly_func_arg_idents(pfi, ident_arg_ts);
                    inherit_poly_arg_match(arg_ts, ident_arg_ts);
                }
            }

            case AST_TYPE_SPEC_POLY_FUNC_ARG:
            case AST_TYPE_SPEC_TYPE:
            {
                break;
            }

            default: assert(false);
        }
    }

    bool is_poly_func_arg_match(Poly_Func_Arg_Idents* pfi, AST_Identifier* ident)
    {
        assert(pfi);
        assert(ident);

        for (uint64_t i = 0; i < BUF_LENGTH(pfi->args); i++)
        {
            if (pfi->args[i]->atom == ident->atom)
            {
                return true;
            }
        }

        return false;
    }

    void convert_value_poly_args(Resolver* resolver, AST_Declaration* func_decl,
                                 AST_Expression* call_expr, AST_Scope* scope)
    {
        assert(func_decl->kind == AST_DECL_FUNC);
        assert(call_expr->kind == AST_EXPR_CALL);

        auto arg_decls = func_decl->function.args;
        auto arg_exprs = call_expr->call.arg_expressions;

        auto arg_decl_count = BUF_LENGTH(arg_decls);
        auto arg_expr_count = BUF_LENGTH(arg_exprs);

        assert(arg_decl_count == arg_expr_count);

        for (uint64_t i = 0; i < arg_decl_count; i++)
        {
            auto arg_decl = arg_decls[i];
            auto arg_expr = arg_exprs[i];

            assert(arg_decl->kind == AST_DECL_MUTABLE);
            if ((arg_decl->flags & AST_DECL_FLAG_FUNC_VALUE_POLY))
            {
                assert(arg_expr->flags & AST_EXPR_FLAG_CONST);

                arg_decl->kind = AST_DECL_CONSTANT_VAR;
                arg_decl->constant_var.init_expression = arg_expr;
                if (arg_expr->flags & AST_EXPR_FLAG_TYPE)
                {
                    arg_decl->constant_var.type = arg_expr->type;
                }

                bool arg_res = resolver_resolve_declaration(resolver, arg_decl, scope);
                assert(arg_res);
                resolver_push_declaration_to_scope(resolver, arg_decl, scope);

            }
        }
    }
}
