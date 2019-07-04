#include "polymorph.h"

namespace Zodiac
{
    BUF(Poly_Type_Spec_Replacement) find_type_spec_replacements(Context* context,
                                                                AST_Declaration* poly_decl,
                                                                AST_Expression* call_expr)
    {
        assert(context);
        assert(poly_decl);
        assert(call_expr);
        assert(call_expr->kind == AST_EXPR_CALL);


        BUF(Poly_Type_Spec_Replacement) replacements = nullptr;

        switch (poly_decl->kind)
        {
            case AST_DECL_FUNC:
            {
                auto arg_decls = poly_decl->function.args;
                auto call_exprs = call_expr->call.arg_expressions;

                for (uint64_t i = 0; i < BUF_LENGTH(arg_decls); i++)
                {
                    auto arg_decl = arg_decls[i];
                    auto call_expr = call_exprs[i];

                    if (arg_decl->mutable_decl.type_spec->flags & AST_TYPE_SPEC_FLAG_POLY)
                    {
                        find_type_spec_replacements(context,
                                                    arg_decl->mutable_decl.type_spec,
                                                    arg_decl->mutable_decl.type_spec->file_pos,
                                                    call_expr->type, &replacements);
                    }
                }
                break;
            }

            default: assert(false);
        }


        return replacements;
    }

    void find_type_spec_replacements(Context* context, AST_Type_Spec* type_spec,
                                     File_Pos file_pos, AST_Type* given_type,
                                     BUF(Poly_Type_Spec_Replacement)* replacements)
    {
        assert(context);
        assert(type_spec);
        assert(given_type);

        switch (type_spec->kind)
        {
            case AST_TYPE_SPEC_IDENT:
            {
                if (type_spec->identifier.poly_args)
                {
                    assert(given_type->kind == AST_TYPE_STRUCT);
                    assert(given_type->aggregate_type.poly_from);

                    auto poly_args = type_spec->identifier.poly_args;
                    auto poly_types = given_type->aggregate_type.poly_types;
                    assert(BUF_LENGTH(poly_args) == BUF_LENGTH(poly_types));

                    for (uint64_t i = 0; i < BUF_LENGTH(poly_args); i++)
                    {
                        auto poly_arg = poly_args[i];
                        assert(poly_arg->kind == AST_TYPE_SPEC_IDENT);

                        Poly_Type_Spec_Replacement replacement;
                        replacement.poly_name = poly_args[i]->identifier.identifier->atom;
                        replacement.replacement = poly_types[i];
                        BUF_PUSH(*replacements, replacement);
                    }
                }
                else
                {
                    Poly_Type_Spec_Replacement replacement;
                    replacement.poly_name = type_spec->identifier.identifier->atom;

                    AST_Type_Spec* replacement_ts = ast_type_spec_from_type_new(context,
                                                                                file_pos,
                                                                                given_type);
                    replacement.replacement = replacement_ts;
                    BUF_PUSH(*replacements, replacement);
                }
                break;
            }

            case AST_TYPE_SPEC_POINTER:
            {
                assert(given_type->kind == AST_TYPE_POINTER);
                find_type_spec_replacements(context, type_spec->pointer.base,
                                            file_pos, given_type->pointer.base,
                                            replacements);
                break;
            }

            default: assert(false);
        }
    }

    bool find_or_create_poly_function(Resolver* resolver, AST_Expression* call_expression,
                                      AST_Declaration** func_decl_dest, AST_Scope* scope)
    {
        assert(resolver);
        assert(call_expression);
        assert(call_expression->kind == AST_EXPR_CALL);
        assert(func_decl_dest);

        AST_Declaration* poly_func_decl = *func_decl_dest;
        assert(poly_func_decl);
        assert(poly_func_decl->kind == AST_DECL_FUNC);

        bool result = true;
        uint64_t poly_hash = 0;

        auto call_args = call_expression->call.arg_expressions;
        assert(BUF_LENGTH(call_args));

        for (uint64_t i = 0; i < BUF_LENGTH(call_args); i++)
        {
            auto call_arg = call_args[i];
            result &= try_resolve_expression(resolver, call_arg, scope);

            if (result)
            {
                assert(call_arg->type);
                poly_hash = hash_mix(poly_hash, ast_get_type_hash(call_arg->type));
            }
            else
            {
                return false;
            }
        }

        AST_Declaration* poly_function_instance = nullptr;
        if (find_poly_function_instance(poly_func_decl, poly_hash, &poly_function_instance))
        {
            *func_decl_dest = poly_function_instance;
            return true;
        }

        poly_function_instance = create_poly_function_instance(resolver, poly_func_decl);
        auto replacements = find_type_spec_replacements(resolver->context, poly_func_decl,
                                                        call_expression);
        replace_poly_type_specs(poly_function_instance, replacements);


        auto was_resolving = resolver->current_func_decl;
        resolver->current_func_decl = nullptr;
        result &= try_resolve_declaration(resolver, poly_function_instance,
                                          poly_func_decl->function.argument_scope->parent);
        resolver->current_func_decl = was_resolving;


        if (result)
        {
            if (poly_func_decl->location == AST_DECL_LOC_GLOBAL)
            {
                BUF_PUSH(resolver->module->global_declarations, poly_function_instance);
            }

            assert(poly_function_instance->function.type);
            *func_decl_dest = poly_function_instance;

            AST_Poly_Instance pi = { poly_hash, poly_function_instance };
            BUF_PUSH(poly_func_decl->function.poly_instances, pi);
        }

        return result;
    }

    bool find_or_create_poly_struct_type(Resolver* resolver, AST_Declaration* type_decl,
                                         AST_Type_Spec* type_spec, AST_Type** type_dest,
                                         AST_Scope* scope)
    {
        assert(resolver);
        assert(type_decl);
        assert(type_decl->kind == AST_DECL_AGGREGATE_TYPE);
        assert(type_decl->aggregate_type.kind == AST_AGG_DECL_STRUCT);
        assert(type_spec);
        assert(type_spec->kind == AST_TYPE_SPEC_IDENT);
        assert(type_dest);
        assert(*type_dest == nullptr);
        assert(scope);

        if (!check_poly_struct_arguments(resolver, type_decl, type_spec))
        {
            return false;
        }

        bool result = true;

        uint64_t poly_hash = 0;

        for (uint64_t i = 0; i < BUF_LENGTH(type_spec->identifier.poly_args); i++)
        {
            AST_Type_Spec* arg_type_spec = type_spec->identifier.poly_args[i];
            AST_Type* arg_type_dest = nullptr;
            result &= try_resolve_type_spec(resolver, arg_type_spec, &arg_type_dest, scope);

            if (result)
            {
                poly_hash = hash_mix(poly_hash, ast_get_type_hash(arg_type_dest));
            }
            else
            {
                return false;
            }
        }

        AST_Declaration* poly_struct_decl = nullptr;
        if (find_poly_struct_instance(type_decl, poly_hash, &poly_struct_decl))
        {
            assert(poly_struct_decl);
            assert(poly_struct_decl->kind == AST_DECL_AGGREGATE_TYPE);
            assert(poly_struct_decl->aggregate_type.type);
            *type_dest = poly_struct_decl->aggregate_type.type;
            return true;
        }

        poly_struct_decl = create_poly_struct_instance(resolver, type_decl, scope);
        assert(poly_struct_decl);
        replace_poly_type_specs(type_decl->aggregate_type.parameter_idents, poly_struct_decl,
                                type_spec->identifier.poly_args);

        result &= try_resolve_declaration(resolver, poly_struct_decl, scope);

        if (result)
        {
            assert(poly_struct_decl->aggregate_type.type);
            auto poly_struct_type = poly_struct_decl->aggregate_type.type;
            *type_dest = poly_struct_type;

            poly_struct_type->aggregate_type.poly_from = type_decl;

            for (uint64_t i = 0; i < BUF_LENGTH(type_spec->identifier.poly_args); i++)
            {
                BUF_PUSH(poly_struct_type->aggregate_type.poly_types,
                         type_spec->identifier.poly_args[i]);
            }

            AST_Poly_Instance pi = { poly_hash, poly_struct_decl };
            BUF_PUSH(type_decl->aggregate_type.poly_instances, pi);
        }

        return result;
    }

    bool check_poly_struct_arguments(Resolver* resolver, AST_Declaration* type_decl,
                                     AST_Type_Spec* type_spec)
    {
        assert(resolver);
        assert(type_decl);
        assert(type_decl->kind == AST_DECL_AGGREGATE_TYPE);
        assert(type_decl->aggregate_type.kind == AST_AGG_DECL_STRUCT);
        assert(type_spec);
        assert(type_spec->kind == AST_TYPE_SPEC_IDENT);

        if (BUF_LENGTH(type_decl->aggregate_type.parameter_idents) !=
            BUF_LENGTH(type_spec->identifier.poly_args))
        {
            resolver_report_error(resolver, type_spec->file_pos,
                                  "Mismatching poly arguments for struct type");
            return false;
        }

        return true;
    }

    bool find_poly_function_instance(AST_Declaration* poly_func_decl, uint64_t poly_hash,
                                     AST_Declaration** poly_func_decl_dest)
    {
        assert(poly_func_decl);
        assert(poly_func_decl->kind == AST_DECL_FUNC);
        assert(poly_func_decl->function.is_poly);

        assert(poly_func_decl_dest);
        assert(*poly_func_decl_dest == nullptr);

        auto instances = poly_func_decl->function.poly_instances;
        for (uint64_t i = 0; i < BUF_LENGTH(instances); i++)
        {
            auto instance = instances[i];
            if (poly_hash == instance.hash)
            {
                *poly_func_decl_dest = instance.instance;
                return true;
            }
        }

        return false;
    }

    bool find_poly_struct_instance(AST_Declaration* type_decl, uint64_t poly_hash,
                                               AST_Declaration** decl_dest)
    {
        assert(type_decl);
        assert(type_decl->kind == AST_DECL_AGGREGATE_TYPE);
        assert(type_decl->aggregate_type.kind == AST_AGG_DECL_STRUCT);
        assert(decl_dest);
        assert(*decl_dest == nullptr);

        auto aggregate_polys = type_decl->aggregate_type.poly_instances;
        for (uint64_t i = 0; i < BUF_LENGTH(aggregate_polys); i++)
        {
            auto ap = aggregate_polys[i];
            if (ap.hash == poly_hash)
            {
                *decl_dest = ap.instance;
                return true;
            }
        }

        return false;
    }

    AST_Declaration* create_poly_function_instance(Resolver* resolver,
                                                   AST_Declaration* poly_func_decl)
    {
        assert(resolver);
        assert(poly_func_decl);
        assert(poly_func_decl->kind == AST_DECL_FUNC);
        assert(poly_func_decl->function.is_poly);

        auto poly_atom = atom_append(resolver->context->atom_table,
                                     "_poly_", poly_func_decl->function.poly_count++);
        poly_atom = atom_append(resolver->context->atom_table,
                                poly_func_decl->identifier->atom, poly_atom);

        AST_Identifier* poly_identifier = ast_identifier_new(resolver->context, poly_atom,
                                                             poly_func_decl->identifier->file_pos);

        AST_Declaration* instance = ast_declaration_new(resolver->context,
                                                        poly_func_decl->file_pos,
                                                        AST_DECL_FUNC, poly_func_decl->location,
                                                        poly_identifier, nullptr, true);

        for (uint64_t i = 0; i < BUF_LENGTH(poly_func_decl->function.args); i++)
        {
            AST_Declaration* arg_copy = copy_declaration(resolver->context,
                                                         poly_func_decl->function.args[i]);
            BUF_PUSH(instance->function.args, arg_copy);
        }

        if (poly_func_decl->function.return_type_spec)
        {
            instance->function.return_type_spec =
                copy_type_spec(resolver->context, poly_func_decl->function.return_type_spec);
        }

        instance->function.argument_scope =
            ast_scope_new(resolver->context,
                          poly_func_decl->function.argument_scope->parent,
                          poly_func_decl->function.argument_scope->module,
                          false);

        AST_Scope* block_scope =
            ast_scope_new(resolver->context, instance->function.argument_scope,
                          poly_func_decl->function.body_block->block.scope->module,
                          false);

        instance->function.body_block = copy_statement(resolver->context,
                                                       poly_func_decl->function.body_block,
                                                       block_scope);

        return instance;

    }

    AST_Declaration* create_poly_struct_instance(Resolver* resolver, AST_Declaration* type_decl,
                                                 AST_Scope* scope)
    {
        assert(resolver);
        assert(type_decl);
        assert(type_decl->kind == AST_DECL_AGGREGATE_TYPE);
        assert(type_decl->aggregate_type.kind == AST_AGG_DECL_STRUCT);
        assert(scope);

        auto agg_decls = type_decl->aggregate_type.aggregate_declarations;

        auto poly_atom = atom_append(resolver->context->atom_table,
                                     "_poly_", type_decl->aggregate_type.poly_count++);
        poly_atom = atom_append(resolver->context->atom_table,
                                type_decl->identifier->atom, poly_atom);
        AST_Identifier* poly_identifier = ast_identifier_new(resolver->context, poly_atom,
                                                             type_decl->identifier->file_pos);

        AST_Declaration* instance = ast_declaration_new(resolver->context, type_decl->file_pos,
                                                        AST_DECL_AGGREGATE_TYPE,
                                                        type_decl->location,
                                                        poly_identifier,
                                                        nullptr, true);

        instance->aggregate_type.kind = AST_AGG_DECL_STRUCT;

        BUF(AST_Declaration*) instance_agg_decls = nullptr;
        for (uint64_t i = 0; i < BUF_LENGTH(agg_decls); i++)
        {
            AST_Declaration* agg_decl_copy = copy_declaration(resolver->context, agg_decls[i]);
            BUF_PUSH(instance_agg_decls, agg_decl_copy);
        }

        instance->aggregate_type.aggregate_declarations = instance_agg_decls;
        instance->aggregate_type.scope = ast_scope_new(resolver->context, scope, scope->module,
                                                       false);

        return instance;
    }

    AST_Declaration* copy_declaration(Context* context, AST_Declaration* declaration)
    {
        assert(context);
        assert(declaration);

        switch (declaration->kind)
        {
            case AST_DECL_MUTABLE:
            {
                AST_Identifier* identifier = copy_identifier(context, declaration->identifier);
                AST_Type_Spec* type_spec = copy_type_spec(context,
                                                          declaration->mutable_decl.type_spec);
                auto init_copy = copy_expression(context,
                                                 declaration->mutable_decl.init_expression);
                return ast_mutable_declaration_new(context, declaration->file_pos, identifier,
                                                   type_spec, init_copy,
                                                   declaration->location);
            }

            default: assert(false);
        }
    }

    AST_Statement* copy_statement(Context* context, AST_Statement* statement,
                                  AST_Scope* scope)
    {
        assert(context);
        assert(statement);

        switch (statement->kind)
        {
            case AST_STMT_BLOCK:
            {
                assert(scope);
                BUF(AST_Statement*) block_statements = nullptr;
                for (uint64_t i = 0; i < BUF_LENGTH(statement->block.statements); i++)
                {
                    auto block_stmt_copy = copy_statement(context, statement->block.statements[i],
                                                          scope);
                    BUF_PUSH(block_statements, block_stmt_copy);
                }
                return ast_block_statement_new(context, statement->file_pos, block_statements,
                                               scope);
            }

            case AST_STMT_ASSIGN:
            {
                auto lvalue_copy = copy_expression(context, statement->assign.lvalue_expression);
                auto expr_copy = copy_expression(context, statement->assign.expression);
                return ast_assign_statement_new(context, statement->file_pos,
                                                lvalue_copy, expr_copy);
            }

            case AST_STMT_RETURN:
            {
                auto expr_copy = copy_expression(context, statement->return_expression);
                return ast_return_statement_new(context, statement->file_pos, expr_copy);
            }

            case AST_STMT_IF:
            {
                auto cond_copy = copy_expression(context, statement->if_stmt.if_expression);
                auto then_copy = copy_statement(context, statement->if_stmt.then_statement, scope);
                AST_Statement* else_copy = nullptr;
                if (statement->if_stmt.else_statement)
                {
                    else_copy = copy_statement(context, statement->if_stmt.else_statement, scope);
                }
                return ast_if_statement_new(context, statement->file_pos, cond_copy,
                                            then_copy, else_copy);
            }

            case AST_STMT_DECLARATION:
            {
                auto decl_copy = copy_declaration(context, statement->declaration);
                return ast_declaration_statement_new(context, statement->file_pos, decl_copy);
            }

            case AST_STMT_CALL:
            {
                auto call_expr_copy = copy_expression(context, statement->call_expression);
                return ast_call_statement_new(context, call_expr_copy);
            }

            default: assert(false);
        }
    }

    AST_Expression* copy_expression(Context* context, AST_Expression* expression)
    {
        assert(context);

        if (!expression) return nullptr;

        switch (expression->kind)
        {
            case AST_EXPR_IDENTIFIER:
            {
                auto ident_copy = copy_identifier(context, expression->identifier);
                return ast_ident_expression_new(context, expression->file_pos, ident_copy);
            }

            case AST_EXPR_DOT:
            {
                auto base_copy = copy_expression(context, expression->dot.base_expression);
                auto member_copy = copy_expression(context, expression->dot.member_expression);
                return ast_dot_expression_new(context, expression->file_pos, base_copy,
                                              member_copy);
            }

            case AST_EXPR_CALL:
            {
                auto ident_expr_copy = copy_expression(context, expression->call.ident_expression);
                BUF(AST_Expression*) arg_exprs_copy = nullptr;
                for (uint64_t i = 0; i < BUF_LENGTH(expression->call.arg_expressions); i++)
                {
                    auto arg_copy = copy_expression(context, expression->call.arg_expressions[i]);
                    BUF_PUSH(arg_exprs_copy, arg_copy);
                }
                return ast_call_expression_new(context, expression->file_pos, ident_expr_copy,
                                               arg_exprs_copy);
            }

            case AST_EXPR_BINARY:
            {
                auto lhs_copy = copy_expression(context, expression->binary.lhs);
                auto rhs_copy = copy_expression(context, expression->binary.rhs);
                return ast_binary_expression_new(context, expression->file_pos, lhs_copy,
                                                 expression->binary.op, rhs_copy);
            }

            case AST_EXPR_SIZEOF:
            {
                auto type_spec_copy = copy_type_spec(context, expression->sizeof_expr.type_spec);
                return ast_sizeof_expression_new(context, expression->file_pos, type_spec_copy);
            }

            case AST_EXPR_INTEGER_LITERAL:
            {
                return ast_integer_literal_expression_new(context, expression->file_pos,
                                                        expression->integer_literal.u64);
            }

            case AST_EXPR_SUBSCRIPT:
            {
                auto base_copy = copy_expression(context, expression->subscript.base_expression);
                auto index_copy = copy_expression(context, expression->subscript.index_expression);
                return ast_subscript_expression_new(context, expression->file_pos,
                                                    base_copy, index_copy);
            }

            case AST_EXPR_CAST:
            {
                auto type_spec_copy = copy_type_spec(context, expression->cast_expr.type_spec);
                auto expr_copy = copy_expression(context, expression->cast_expr.expr);
                return ast_cast_expression_new(context, expression->file_pos, type_spec_copy,
                                               expr_copy);
            }

            default: assert(false);
        }
    }

    AST_Type_Spec* copy_type_spec(Context* context, AST_Type_Spec* type_spec)
    {
        assert(context);

        if (!type_spec) return nullptr;

        switch (type_spec->kind)
        {
            case AST_TYPE_SPEC_IDENT:
            {
                auto ident_copy = copy_identifier(context, type_spec->identifier.identifier);
                BUF(AST_Type_Spec*) poly_args = nullptr;
                for (uint64_t i = 0; i < BUF_LENGTH(type_spec->identifier.poly_args); i++)
                {
                    BUF_PUSH(poly_args, copy_type_spec(context,
                                                       type_spec->identifier.poly_args[i]));
                }

                return ast_type_spec_identifier_new(context, type_spec->file_pos, ident_copy,
                                                    poly_args);
            }

            case AST_TYPE_SPEC_POINTER:
            {
                auto base_copy = copy_type_spec(context, type_spec->pointer.base);
                return ast_type_spec_pointer_new(context, type_spec->file_pos, base_copy);
            }

            default: assert(false);
        }
    }

    AST_Identifier* copy_identifier(Context* context, AST_Identifier* identifier)
    {
        assert(context);
        assert(identifier);

        return ast_identifier_new(context, identifier->atom, identifier->file_pos);
    }

    void replace_poly_type_specs(AST_Declaration* poly_decl_instance,
                                 BUF(Poly_Type_Spec_Replacement) replacements)
    {
        assert(poly_decl_instance);
        assert(replacements);

        switch (poly_decl_instance->kind)
        {
            case AST_DECL_FUNC:
            {
                auto arg_decls = poly_decl_instance->function.args;
                for (uint64_t i = 0; i < BUF_LENGTH(arg_decls); i++)
                {
                    auto arg_decl = arg_decls[i];
                    maybe_replace_poly_type_spec(&arg_decl->mutable_decl.type_spec, replacements);
                }

                if (poly_decl_instance->function.return_type_spec)
                {
                    maybe_replace_poly_type_spec(&poly_decl_instance->function.return_type_spec,
                                                 replacements);
                }

                if (poly_decl_instance->function.body_block)
                {
                    replace_poly_type_specs(poly_decl_instance->function.body_block, replacements);
                }
                break;
            }

            case AST_DECL_MUTABLE:
            {
                auto mut = &poly_decl_instance->mutable_decl;
                if (mut->type_spec)
                {
                    maybe_replace_poly_type_spec(&mut->type_spec, replacements);
                }

                if (mut->init_expression)
                {
                    replace_poly_type_specs(mut->init_expression, replacements);
                }
                break;
            }

            default: assert(false);
        }
    }

    void replace_poly_type_specs(AST_Statement* statement,
                                 BUF(Poly_Type_Spec_Replacement) replacements)
    {
        assert(statement);
        assert(replacements);

        switch (statement->kind)
        {
            case AST_STMT_BLOCK:
            {
                for (uint64_t i = 0; i < BUF_LENGTH(statement->block.statements); i++)
                {
                    replace_poly_type_specs(statement->block.statements[i], replacements);
                }
                break;
            }

            case AST_STMT_ASSIGN:
            {
                replace_poly_type_specs(statement->assign.lvalue_expression, replacements);
                replace_poly_type_specs(statement->assign.expression, replacements);
                break;
            }

            case AST_STMT_RETURN:
            {
                replace_poly_type_specs(statement->return_expression, replacements);
                break;
            }

            case AST_STMT_IF:
            {
                replace_poly_type_specs(statement->if_stmt.if_expression, replacements);
                replace_poly_type_specs(statement->if_stmt.then_statement, replacements);
                if (statement->if_stmt.else_statement)
                {
                    replace_poly_type_specs(statement->if_stmt.else_statement, replacements);
                }
                break;
            }

            case AST_STMT_DECLARATION:
            {
                replace_poly_type_specs(statement->declaration, replacements);
                break;
            }

            case AST_STMT_CALL:
            {
                replace_poly_type_specs(statement->call_expression, replacements);
                break;
            }

            default: assert(false);
        }
    }

    void replace_poly_type_specs(AST_Expression* expression,
                                 BUF(Poly_Type_Spec_Replacement) replacements)
    {
        assert(expression);
        assert(replacements);

        switch (expression->kind)
        {
            case AST_EXPR_DOT:
            {
                replace_poly_type_specs(expression->dot.base_expression, replacements);
                replace_poly_type_specs(expression->dot.member_expression, replacements);
                break;
            }

            case AST_EXPR_CALL:
            {
                replace_poly_type_specs(expression->call.ident_expression, replacements);
                for (uint64_t i = 0; i < BUF_LENGTH(expression->call.arg_expressions); i++)
                {
                    replace_poly_type_specs(expression->call.arg_expressions[i], replacements);
                }
                break;
            }

            case AST_EXPR_BINARY:
            {
                replace_poly_type_specs(expression->binary.lhs, replacements);
                replace_poly_type_specs(expression->binary.rhs, replacements);
                break;
            }

            case AST_EXPR_SIZEOF:
            {
                maybe_replace_poly_type_spec(&expression->sizeof_expr.type_spec, replacements);
                break;
            }

            case AST_EXPR_SUBSCRIPT:
            {
                replace_poly_type_specs(expression->subscript.base_expression, replacements);
                replace_poly_type_specs(expression->subscript.index_expression, replacements);
                break;
            }

            case AST_EXPR_CAST:
            {
                maybe_replace_poly_type_spec(&expression->cast_expr.type_spec, replacements);
                replace_poly_type_specs(expression->cast_expr.expr, replacements);
                break;
            }

            case AST_EXPR_IDENTIFIER:
            case AST_EXPR_INTEGER_LITERAL:
            {
                break;
            }

            default: assert(false);
        }
    }

    void replace_poly_type_specs(BUF(AST_Identifier*) poly_type_names, AST_Declaration* type_decl,
                                 BUF(AST_Type_Spec*) given_type_specs)
    {
        assert(poly_type_names);
        assert(type_decl);
        assert(given_type_specs);
        assert(BUF_LENGTH(poly_type_names) == BUF_LENGTH(given_type_specs));

        switch (type_decl->kind)
        {
            case AST_DECL_AGGREGATE_TYPE:
            {
                assert(type_decl->aggregate_type.kind == AST_AGG_DECL_STRUCT);

                auto agg_decls = type_decl->aggregate_type.aggregate_declarations;


                for (uint64_t i = 0; i < BUF_LENGTH(poly_type_names); i++)
                {
                    auto poly_arg = poly_type_names[i];

                    for (uint64_t j = 0; j < BUF_LENGTH(agg_decls); j++)
                    {
                        auto agg_decl = agg_decls[j];
                        maybe_replace_poly_arg(poly_arg, agg_decl, given_type_specs[i]);
                    }
                }
                break;
            }

            case AST_DECL_FUNC:
            {
                for (uint64_t i = 0; i < BUF_LENGTH(poly_type_names); i++)
                {
                    auto poly_name = poly_type_names[i];
                    auto arg_decls=  type_decl->function.args;

                    for (uint64_t j = 0; j < BUF_LENGTH(arg_decls); j++)
                    {
                        auto arg_decl = arg_decls[j];
                        maybe_replace_poly_arg(poly_name, arg_decl, given_type_specs[i]);
                    }
                }
                break;
            }

            default: assert(false);
        }

    }

    void maybe_replace_poly_type_spec(AST_Type_Spec** target_ts,
                                      BUF(Poly_Type_Spec_Replacement) replacements)
    {
        assert(target_ts);
        assert(replacements);

        AST_Type_Spec* type_spec = *target_ts;

        switch (type_spec->kind)
        {
            case AST_TYPE_SPEC_IDENT:
            {
                if (type_spec->identifier.poly_args)
                {
                    for (uint64_t i = 0; i < BUF_LENGTH(type_spec->identifier.poly_args); i++)
                    {
                        maybe_replace_poly_type_spec(&type_spec->identifier.poly_args[i],
                                                     replacements);
                    }
                }
                else
                {
                    for (uint64_t i = 0; i < BUF_LENGTH(replacements); i++)
                    {
                        auto replacement = replacements[i];
                        if (replacement.poly_name == type_spec->identifier.identifier->atom)
                        {
                            *target_ts = replacement.replacement;
                            break;
                        }
                    }
                }
                break;
            }

            case AST_TYPE_SPEC_POINTER:
            {
                maybe_replace_poly_type_spec(&type_spec->pointer.base, replacements);
                break;
            }

            default: assert(false);
        }
    }

    void maybe_replace_poly_arg(AST_Identifier* poly_name, AST_Declaration* poly_decl,
                                AST_Type_Spec* given_type_spec)
    {
        assert(poly_name);
        assert(poly_decl);
        assert(given_type_spec);

        switch (poly_decl->kind)
        {
            case AST_DECL_MUTABLE:
            {
                maybe_replace_poly_arg(poly_name, &poly_decl->mutable_decl.type_spec,
                                       given_type_spec);
                break;
            }

            default: assert(false);
        }
    }

    void maybe_replace_poly_arg(AST_Identifier* poly_name, AST_Type_Spec** poly_type_spec,
                                AST_Type_Spec* given_type_spec)
    {
        assert(poly_name);
        assert(poly_type_spec);
        auto pts = *poly_type_spec;
        assert(given_type_spec);
        // assert(given_type_spec->kind == AST_TYPE_SPEC_IDENT);

        switch (pts->kind)
        {
            case AST_TYPE_SPEC_POINTER:
            {
                maybe_replace_poly_arg(poly_name, &pts->pointer.base, given_type_spec);
                break;
            }

            case AST_TYPE_SPEC_IDENT:
            {
                if (poly_name->atom == pts->identifier.identifier->atom)
                {
                    *poly_type_spec = given_type_spec;
                }
                break;
            }

            default: assert(false);
        }
    }
}
