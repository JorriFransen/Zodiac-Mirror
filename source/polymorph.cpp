#include "polymorph.h"

namespace Zodiac
{
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

        assert(false);
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
        replace_poly_type_specs(type_decl, poly_struct_decl, type_spec);

        result &= try_resolve_declaration(resolver, poly_struct_decl, scope);

        if (result)
        {
            assert(poly_struct_decl->aggregate_type.type);
            *type_dest = poly_struct_decl->aggregate_type.type;

            AST_Aggregate_Poly ap = { poly_hash, poly_struct_decl };
            BUF_PUSH(type_decl->aggregate_type.poly_instances, ap);
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
        assert(declaration->kind == AST_DECL_MUTABLE);

        assert(!declaration->mutable_decl.init_expression);

        AST_Identifier* identifier = copy_identifier(context, declaration->identifier);
        AST_Type_Spec* type_spec = copy_type_spec(context, declaration->mutable_decl.type_spec);

        return ast_mutable_declaration_new(context, declaration->file_pos, identifier,
                                           type_spec, nullptr,
                                           declaration->location);
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
                                                          nullptr);
                    BUF_PUSH(block_statements, block_stmt_copy);
                }
                return ast_block_statement_new(context, statement->file_pos, block_statements,
                                               scope);
                break;
            }

            case AST_STMT_ASSIGN:
            {
                auto lvalue_copy = copy_expression(context, statement->assign.lvalue_expression);
                auto expr_copy = copy_expression(context, statement->assign.expression);
                return ast_assign_statement_new(context, statement->file_pos,
                                                lvalue_copy, expr_copy);
                break;
            }

            default: assert(false);
        }
    }

    AST_Expression* copy_expression(Context* context, AST_Expression* expression)
    {
        assert(context);
        assert(expression);

        switch (expression->kind)
        {
            default: assert(false);
        }
    }

    AST_Type_Spec* copy_type_spec(Context* context, AST_Type_Spec* type_spec)
    {
        assert(context);
        assert(type_spec);

        switch (type_spec->kind)
        {
            case AST_TYPE_SPEC_IDENT:
            {
                auto ident_copy = copy_identifier(context, type_spec->identifier.identifier);
                return ast_type_spec_identifier_new(context, type_spec->file_pos, ident_copy,
                                                    type_spec->identifier.poly_args);
                break;
            }

            case AST_TYPE_SPEC_POINTER:
            {
                auto base_copy = copy_type_spec(context, type_spec->pointer.base);
                return ast_type_spec_pointer_new(context, type_spec->file_pos, base_copy);
                break;
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

    void replace_poly_type_specs(AST_Declaration* poly_type_decl, AST_Declaration* type_decl,
                                 AST_Type_Spec* type_spec)
    {
        assert(poly_type_decl);
        assert(poly_type_decl->kind == AST_DECL_AGGREGATE_TYPE);
        assert(poly_type_decl->aggregate_type.kind == AST_AGG_DECL_STRUCT);
        assert(type_decl);
        assert(type_decl->kind == AST_DECL_AGGREGATE_TYPE);
        assert(type_decl->aggregate_type.kind == AST_AGG_DECL_STRUCT);
        assert(type_spec);
        assert(type_spec->kind == AST_TYPE_SPEC_IDENT);

        auto poly_args = poly_type_decl->aggregate_type.parameter_idents;
        auto agg_decls = type_decl->aggregate_type.aggregate_declarations;

        for (uint64_t i = 0; i < BUF_LENGTH(poly_args); i++)
        {
            auto poly_arg = poly_args[i];

            for (uint64_t j = 0; j < BUF_LENGTH(agg_decls); j++)
            {
                auto agg_decl = agg_decls[j];
                maybe_replace_poly_arg(poly_arg, i, agg_decl, type_spec);
            }
        }
    }

    void maybe_replace_poly_arg(AST_Identifier* poly_name, uint64_t poly_index,
                                AST_Declaration* poly_decl, AST_Type_Spec* poly_args_ts)
    {
        assert(poly_name);
        assert(poly_decl);
        assert(poly_args_ts);

        switch (poly_decl->kind)
        {
            case AST_DECL_MUTABLE:
            {
                maybe_replace_poly_arg(poly_name, poly_index, &poly_decl->mutable_decl.type_spec,
                                       poly_args_ts);
                break;
            }

            default: assert(false);
        }
    }

    void maybe_replace_poly_arg(AST_Identifier* poly_name, uint64_t poly_index,
                                AST_Type_Spec** poly_type_spec, AST_Type_Spec* poly_args_ts)
    {
        assert(poly_name);
        assert(poly_type_spec);
        auto pts = *poly_type_spec;
        assert(poly_args_ts);
        assert(poly_args_ts->kind == AST_TYPE_SPEC_IDENT);

        switch (pts->kind)
        {
            case AST_TYPE_SPEC_POINTER:
            {
                maybe_replace_poly_arg(poly_name, poly_index, &pts->pointer.base, poly_args_ts);
                break;
            }

            case AST_TYPE_SPEC_IDENT:
            {
                if (poly_name->atom == pts->identifier.identifier->atom)
                {
                    *poly_type_spec = poly_args_ts->identifier.poly_args[poly_index];
                }
                break;
            }

            default: assert(false);
        }
    }
}
