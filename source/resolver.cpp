#include "resolver.h"

#include "builtin.h"
#include "const_interpreter.h"

#include "lexer.h"
#include "parser.h"
#include "ir.h"
#include "ir_runner.h"
#include "polymorph.h"

#include <string.h>
#include <stdarg.h>
#include <inttypes.h>

namespace Zodiac
{
    void resolver_init(Resolver* resolver, Context* context, AST_Module* module)
    {
        assert(resolver);
        assert(context);
        assert(module);

        resolver->context = context;
        resolver->module = module;

        resolver->done = false;
        resolver->progressed_on_last_cycle = true;
        resolver->unresolved_decl_count = 0;
        resolver->unresolved_decl_count_last_cycle = UINT64_MAX;
        resolver->undeclared_decl_count = 0;
        resolver->undeclared_decl_count_last_cycle = UINT64_MAX;
        resolver->silent = false;
        resolver->import_error = false;
        resolver->resolving_auto_gen = false;
        resolver->auto_gen_file_pos = {};
        resolver->unresolved_decls = nullptr;

        resolver->errors = nullptr;

        resolver->current_func_decl = nullptr;
    }

    void resolver_do_cycle(Resolver* resolver)
    {
        assert(resolver);
        assert(resolver->progressed_on_last_cycle);

        if (resolver->module->resolved)
        {
            resolver->done = true;
            return;
        }

        resolver->unresolved_decl_count = 0;
        resolver->undeclared_decl_count = 0;

        if (resolver->errors)
        {
            auto err_hdr = _BUF_HDR(resolver->errors);
            err_hdr->length = 0;
        }

        if (resolver->unresolved_decls)
        {
            auto unres_hdr = _BUF_HDR(resolver->unresolved_decls);
            unres_hdr->length = 0;
        }

        for (uint64_t i = 0;
             (i < BUF_LENGTH(resolver->module->global_declarations) && !resolver->import_error);
             i++)
        {
            AST_Declaration* global_decl = resolver->module->global_declarations[i];
            if (!(global_decl->flags & AST_DECL_FLAG_RESOLVED))
            {
                try_resolve_declaration(resolver, global_decl, resolver->module->module_scope);
            }
        }

        resolver->progressed_on_last_cycle = (resolver->unresolved_decl_count <
                                              resolver->unresolved_decl_count_last_cycle) &&
                                             (resolver->undeclared_decl_count <
                                              resolver->undeclared_decl_count_last_cycle);

        resolver->unresolved_decl_count_last_cycle = resolver->unresolved_decl_count;
        resolver->undeclared_decl_count_last_cycle = resolver->undeclared_decl_count;

        if (resolver->unresolved_decl_count == 0 &&
            resolver->undeclared_decl_count == 0)
        {
            resolver->done = true;
            resolver->module->resolved = true;
        }
    }

    bool try_resolve_declaration(Resolver* resolver, AST_Declaration* declaration,
                                        AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(scope);

        bool result = true;

        AST_Declaration* overload_container = nullptr;

        if (!(declaration->flags & AST_DECL_FLAG_RESOLVED))
        {
            if (declaration->identifier && declaration->kind != AST_DECL_STATIC_IF)
            {
                // printf("Checking for redecl: %s\n", declaration->identifier->atom.data);
                auto redecl = find_declaration(resolver->context, scope, declaration->identifier);
                if (redecl)
                {
                    if (declaration->kind == AST_DECL_FUNC && redecl->kind == AST_DECL_FUNC &&
                        !function_signatures_match(resolver, declaration, redecl, scope))
                    {
                        overload_container = redecl;
                        // assert(false);
                    }
                    else
                    {
                        // printf("Found redecl: %s\n", declaration->identifier->atom.data);
                        resolver_report_error(resolver, declaration->file_pos,
                                              "Redeclaration of identifier '%s'\n\tPreviously defined here: '%s:%" PRIu64 ":%" PRIu64 "'",
                                              declaration->identifier->atom.data,
                                              redecl->file_pos.file_name, redecl->file_pos.line,
                                              redecl->file_pos.line_relative_char_pos);
                        result = false;
                    }
                }
            }

            if (result)
            {
                switch (declaration->kind)
                {
                    case AST_DECL_FUNC:
                    {
                        result &= try_resolve_function_declaration(resolver, declaration, scope);
                        if (result && overload_container)
                            add_overload(resolver, overload_container, declaration);
                        break;
                    }

                    case AST_DECL_MUTABLE:
                    {
                        result &= try_resolve_mutable_declaration(resolver, declaration, scope);
                        break;
                    }

                    case AST_DECL_CONSTANT_VAR:
                    {
                        result &= try_resolve_constant_var_declaration(resolver, declaration,
                                                                       scope);
                        break;
                    }

                    case AST_DECL_DYN_LINK:
                    {
                        break;
                    }

                    case AST_DECL_STATIC_IF:
                    {
                        result &= try_resolve_static_if_declaration(resolver, declaration, scope);
                        break;
                    }

                    case AST_DECL_BLOCK:
                    {
                        result &= try_resolve_block_declaration(resolver, declaration, scope);
                        break;
                    }

                    case AST_DECL_STATIC_ASSERT:
                    {
                        result &= try_resolve_static_assert_declaration(resolver, declaration,
                                                                        scope);
                        break;
                    }

                    case AST_DECL_IMPORT:
                    {
                        result &= try_resolve_import_declaration(resolver, declaration,
                                                                 scope);
                        break;
                    }

                    case AST_DECL_AGGREGATE_TYPE:
                    {
                        if (declaration->aggregate_type.kind == AST_AGG_DECL_ENUM)
                        {
                            result &= try_resolve_enum_aggregate_type_declaration(resolver,
                                                                                  declaration,
                                                                                  scope);
                        }
                        else
                        {
                            if (declaration->aggregate_type.parameter_idents)
                            {
                                // Polymorph
                            }
                            else
                            {
                                result &= try_resolve_aggregate_type_declaration(resolver,
                                                                                 declaration,
                                                                                 scope);
                            }
                        }
                        break;
                    }

                    case AST_DECL_TYPEDEF:
                    {
                        result &= try_resolve_typedef_declaration(resolver, declaration, scope);
                        break;
                    }

                    case AST_DECL_USING:
                    {
                        result &= try_resolve_using_declaration(resolver, declaration, scope);
                        break;
                    }

                    case AST_DECL_INSERT:
                    {
                        result &= try_resolve_insert_declaration(resolver, declaration, scope);
                        break;
                    }

                    default:
                        assert(false);
                        break;
                    }
            }

            if (!result)
            {
                resolver->unresolved_decl_count++;
                BUF_PUSH(resolver->unresolved_decls, declaration);
            }
            else
            {
                declaration->flags |= AST_DECL_FLAG_RESOLVED;
                if (!overload_container)
                {
                    ast_scope_push_declaration(scope, declaration);
                    if (declaration->location != AST_DECL_LOC_AGGREGATE_MEMBER)
                    {
                        if (resolver->current_func_decl &&
                            declaration->location == AST_DECL_LOC_LOCAL)
                        {
                            BUF_PUSH(resolver->current_func_decl->function.locals,
                                     declaration);
                        }
                    }
                }
            }
        }

        return result;
    }

    static bool try_resolve_function_declaration(Resolver* resolver, AST_Declaration* declaration,
                                                 AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_FUNC);
        assert(scope);

        assert(!resolver->current_func_decl);
        resolver->current_func_decl = declaration;

        bool result = true;
        AST_Scope* arg_scope  = declaration->function.argument_scope;
        assert(!arg_scope->is_module_scope);
        assert(arg_scope->parent == scope);

		AST_Type* func_type = nullptr;
		BUF(AST_Type*) arg_types = nullptr;

        for (uint64_t i = 0; i < BUF_LENGTH(declaration->function.args); i++)
        {
            AST_Declaration* arg_decl = declaration->function.args[i];
            result &= try_resolve_declaration(resolver, arg_decl, arg_scope);
			if (result)
			{
				assert(arg_decl->kind == AST_DECL_MUTABLE);
				BUF_PUSH(arg_types, arg_decl->mutable_decl.type);
			}
			else
			{
				BUF_FREE(arg_types);
                resolver->current_func_decl = nullptr;
                return false;
			}
        }

        if (!declaration->function.return_type && declaration->function.return_type_spec)
        {
            result &= try_resolve_type_spec(resolver, declaration->function.return_type_spec,
                                            &declaration->function.return_type, scope);
        }

        if (declaration->directive)
        {
            if (declaration->directive->kind == AST_DIREC_FOREIGN)
            {
                declaration->flags |= AST_DECL_FLAG_FOREIGN;
                assert(!declaration->function.body_block);
            }
        }

        if (declaration->function.body_block)
        {
            assert(declaration->function.body_block->block.scope->parent == arg_scope);
            result &= try_resolve_statement(resolver, declaration->function.body_block,
                                            declaration->function.body_block->block.scope,
                                            nullptr);

            if (!declaration->function.return_type_spec &&
                !declaration->function.return_type &&
                declaration->function.inferred_return_type)
            {
                declaration->function.return_type = declaration->function.inferred_return_type;
            }
        }
        else
        {
            assert(declaration->flags &= AST_DECL_FLAG_FOREIGN);
        }

        if (result)
        {
            if (!declaration->function.return_type)
            {
                declaration->function.return_type = Builtin::type_void;
            }
			declaration->function.type = ast_find_or_create_function_type(resolver->context,

				declaration->function.is_vararg, arg_types,
				declaration->function.return_type);

            auto main_atom = Builtin::atom_main;
            if (main_atom == declaration->identifier->atom)
            {
                assert(!resolver->module->entry_point);
                resolver->module->entry_point = declaration;
            }
        }

        resolver->current_func_decl = nullptr;

        return result;
    }

    static bool try_resolve_mutable_declaration(Resolver* resolver, AST_Declaration* declaration,
                                                AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_MUTABLE);
        assert(scope);

        bool result = true;

        if (!declaration->mutable_decl.type &&
            declaration->mutable_decl.type_spec)
        {
            result &= try_resolve_type_spec(resolver, declaration->mutable_decl.type_spec,
                                            &declaration->mutable_decl.type, scope);

            if (!result)
            {
                return false;
            }
        }

        AST_Expression* init_expr = declaration->mutable_decl.init_expression;

        if (init_expr)
        {
            AST_Type* suggested_type = nullptr;
            if (declaration->mutable_decl.type &&
                (declaration->mutable_decl.type->kind == AST_TYPE_STRUCT ||
                 declaration->mutable_decl.type->kind == AST_TYPE_STATIC_ARRAY))
            {
                suggested_type = declaration->mutable_decl.type;
            }
            else if (declaration->mutable_decl.type == Builtin::type_double)
            {
                suggested_type = Builtin::type_double;
            }
            else if (declaration->mutable_decl.type == Builtin::type_float)
            {
                suggested_type = Builtin::type_float;
            }
            else if (init_expr->kind == AST_EXPR_INTEGER_LITERAL)
            {
                if (declaration->mutable_decl.type)
                    assert(declaration->mutable_decl.type->flags & AST_TYPE_FLAG_INT);
                suggested_type = declaration->mutable_decl.type;
            }
            result &= try_resolve_expression(resolver, init_expr, scope, suggested_type);

            if (result && !declaration->mutable_decl.type_spec)
            {
                declaration->mutable_decl.type = init_expr->type;
            }
        }

        if (result)
        {
            assert(declaration->location != AST_DECL_LOC_INVALID);
            assert(declaration->mutable_decl.type);
            if (init_expr)
            {
				if (!(declaration->mutable_decl.type == init_expr->type))
				{
					assert((declaration->mutable_decl.type->kind == AST_TYPE_POINTER &&
                            init_expr->kind == AST_EXPR_NULL_LITERAL) ||
                           declaration->mutable_decl.type == Builtin::type_double &&
                           init_expr->type == Builtin::type_float);
				}
            }
        }
        return result;
    }

    static bool try_resolve_constant_var_declaration(Resolver* resolver,
                                                     AST_Declaration* declaration,
                                                     AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_CONSTANT_VAR);
        assert(scope);

        bool result = true;

        AST_Type* specified_type = nullptr;

        if (declaration->constant_var.type_spec)
        {
            result &= try_resolve_type_spec(resolver, declaration->constant_var.type_spec,
                                            &specified_type,
                                            scope);
            if (!result)
            {
                return false;
            }
        }

        AST_Expression* init_expr = declaration->constant_var.init_expression;
        assert(init_expr);
        result &= try_resolve_expression(resolver, init_expr, scope, specified_type);

        if (!result)
        {
            return false;
        }

        assert(init_expr->is_const);

        if (specified_type)
        {
            AST_Type* init_expr_type = init_expr->type;
			if (!(specified_type == init_expr_type))
			{
				assert(is_valid_integer_conversion(resolver, specified_type, init_expr_type) ||
                       (specified_type->kind == AST_TYPE_POINTER &&
                        init_expr->kind == AST_EXPR_NULL_LITERAL));
			}
        }

        declaration->constant_var.type = declaration->constant_var.init_expression->type;

        return result;
    }

    static bool try_resolve_static_if_declaration(Resolver* resolver,
                                                  AST_Declaration* declaration, AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_STATIC_IF);
        assert(scope);

        bool result = try_resolve_expression(resolver, declaration->static_if.cond_expr, scope);

        if (result)
        {
            bool cond_value = const_interpret_bool_expression(resolver->context,
				                                              declaration->static_if.cond_expr,
                                                              scope);
            if (cond_value)
            {
                result &= try_resolve_declaration(resolver,
                                                  declaration->static_if.then_declaration, scope);
            }
            else if (declaration->static_if.else_declaration)
            {
                result &= try_resolve_declaration(resolver,
                                                  declaration->static_if.else_declaration, scope);
            }
        }


        return result;
    }

    static bool try_resolve_block_declaration(Resolver* resolver, AST_Declaration* declaration, AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_BLOCK);
        assert(scope);

        bool result = true;

        for (uint64_t i = 0; i < BUF_LENGTH(declaration->block.decls); i++)
        {
            result &= try_resolve_declaration(resolver, declaration->block.decls[i], scope);
        }

        return result;
    }

    static bool try_resolve_static_assert_declaration(Resolver* resolver,
                                                      AST_Declaration* declaration,
                                                      AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_STATIC_ASSERT);
        assert(scope);

        AST_Expression* assert_expr = declaration->static_assert_expression;

        bool result = try_resolve_expression(resolver, assert_expr, scope);

        if (result)
        {
            assert(assert_expr->is_const);
        }

        return result;
    }

    static bool try_resolve_aggregate_type_declaration(Resolver* resolver,
                                                       AST_Declaration* declaration,
                                                       AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_AGGREGATE_TYPE);
        assert(scope);

        bool result = true;

        assert(declaration->identifier);

        assert(!find_declaration(resolver->context, scope, declaration->identifier));

        auto aggregate_decls = declaration->aggregate_type.aggregate_declarations;
        BUF(AST_Declaration*) pointers_to_self = nullptr;
        for (uint64_t i = 0; i < BUF_LENGTH(aggregate_decls); i++)
        {
            AST_Declaration* member_decl = aggregate_decls[i];
            assert(member_decl->kind == AST_DECL_MUTABLE);
            assert(member_decl->location == AST_DECL_LOC_AGGREGATE_MEMBER);
            bool mem_result = try_resolve_declaration(resolver, member_decl,
                                                      declaration->aggregate_type.scope);

            // Special case pointer to self
            if (!mem_result)
            {
                AST_Type_Spec* mem_ts = member_decl->mutable_decl.type_spec;
                assert(mem_ts);
                if (mem_ts->kind == AST_TYPE_SPEC_POINTER &&
                    mem_ts->pointer.base->kind == AST_TYPE_SPEC_IDENT)
                {
                    AST_Type_Spec* mem_base_ts = mem_ts->pointer.base;
                    if (mem_base_ts->identifier.identifier->atom == declaration->identifier->atom)
                    {
                        mem_result = true;
                        BUF_PUSH(pointers_to_self, member_decl);
                    }
                }
            }

            result &= mem_result;
        }

        if (result && !declaration->aggregate_type.type)
        {
            declaration->aggregate_type.type = create_struct_type(resolver,
                                                                  declaration->identifier,
                                                                  aggregate_decls);

            if (pointers_to_self)
            {
                auto self_pointer_type = ast_find_or_create_pointer_type(resolver->context,
                                                                        declaration->aggregate_type.type);

                for (uint64_t i = 0; i < BUF_LENGTH(pointers_to_self); i++)
                {
                    AST_Declaration* pointer_to_self = pointers_to_self[i];
                    pointer_to_self->mutable_decl.type = self_pointer_type;
                    assert(try_resolve_declaration(resolver, pointer_to_self,
                                                declaration->aggregate_type.scope));
                }
            }
        }

        if (result)
        {
            assert(declaration->aggregate_type.type);
        }

        BUF_FREE(pointers_to_self);

        return result;
    }

    static bool try_resolve_enum_aggregate_type_declaration(Resolver* resolver,
                                                            AST_Declaration* declaration,
                                                            AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_AGGREGATE_TYPE);
        assert(declaration->aggregate_type.kind == AST_AGG_DECL_ENUM);
        assert(scope);

        auto aggregate_decls = declaration->aggregate_type.aggregate_declarations;

        int64_t index_value = 0;
        bool result = true;
        for (uint64_t i = 0; i < BUF_LENGTH(aggregate_decls); i++)
        {
            AST_Declaration* member_decl = aggregate_decls[i];
            assert(member_decl->kind == AST_DECL_CONSTANT_VAR);

            if (!member_decl->constant_var.init_expression)
            {
                File_Pos gen_fp = { "<auto generated>", 0, 0, 0 };
                member_decl->constant_var.init_expression =
                    ast_integer_literal_expression_new(resolver->context, gen_fp, index_value);
            }

            result &= try_resolve_declaration(resolver, member_decl,
                                              declaration->aggregate_type.scope);

            index_value = const_interpret_s64_expression(resolver->context,
                                                         member_decl->constant_var.init_expression,
                                                         scope);
            index_value++;
        }

        if (result && !declaration->aggregate_type.type)
        {
            declaration->aggregate_type.type = create_enum_type(resolver,
                                                                declaration->identifier,
                                                                aggregate_decls);
        }

        return result;
    }

    static bool try_resolve_import_declaration(Resolver* resolver, AST_Declaration* declaration,
                                               AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_IMPORT);
        assert(scope);

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
            AST_Module* import_module = resolver_add_import_to_module(resolver, resolver->module,
                                                                    module_path, module_name);
            if (!import_module)
            {
                return false;
            }

            declaration->import.module = import_module;

            BUF_PUSH(resolver->module->import_decls, declaration);

            return true;
        }


        if (!found)
        {
            resolver_report_error(resolver, declaration->file_pos,
                                  "Failed to find module: %s",
                                  module_name.data);
        }

        return false;
    }

	static bool try_resolve_typedef_declaration(Resolver* resolver, AST_Declaration* declaration,
		                                        AST_Scope* scope)
	{
		assert(resolver);
		assert(declaration);
		assert(declaration->kind == AST_DECL_TYPEDEF);
		assert(scope);

		AST_Type* type = nullptr;
		bool result = try_resolve_type_spec(resolver, declaration->typedef_decl.type_spec,
			                                &type, scope);
		if (result && type)
		{
			declaration->typedef_decl.type = type;
		}

		return result;
	}

    static bool try_resolve_using_declaration(Resolver* resolver, AST_Declaration* declaration,
                                              AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_USING);
        assert(scope);

        AST_Expression* ident_expr = declaration->using_decl.ident_expression;
        bool result = try_resolve_expression(resolver, ident_expr, scope);
        if (!result)
        {
            return false;
        }

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

        return true;
    }

    static bool try_resolve_insert_declaration(Resolver* resolver, AST_Declaration* declaration,
                                               AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_INSERT);
        assert(scope);
        assert(scope->is_module_scope);

        if (declaration->insert_decl.generated)
        {
            return true;
        }

        AST_Statement* insert_stmt = declaration->insert_decl.call_statement;

        bool result = try_resolve_statement(resolver, insert_stmt, scope, nullptr);
        if (result)
        {
            assert(insert_stmt->kind == AST_STMT_CALL);
            auto u8_ptr_type = ast_find_or_create_pointer_type(resolver->context, Builtin::type_u8);
            assert(insert_stmt->call_expression->type == u8_ptr_type);

            char* insert_string = run_insert(resolver, insert_stmt->call_expression);
            // printf("Insert string: %s\n", insert_string);
            assert(insert_string);

            Lexer lexer;
            init_lexer(&lexer, resolver->context);

            Lex_Result lex_result = lex_file(&lexer, insert_string, "<insert_auto_gen>");
            if (BUF_LENGTH(lex_result.errors) != 0)
            {
                lexer_report_errors(&lexer);

                // We might want to continue here, to try and parse what we have?
                return -1;
            }

            Parser parser;
            parser_init(&parser, resolver->context);

            parser.result.module_name = resolver->module->module_name;
            parser.result.ast_module = resolver->module;
            parser.tokens = lex_result.tokens;
            parser.ti = 0;

            AST_Declaration* gen_decl = nullptr;
            do
            {
                gen_decl = parse_declaration(&parser, scope, true);
                Parse_Result parse_result = parser.result;
                if (BUF_LENGTH(parse_result.errors) != 0)
                {
                    parser_report_errors(&parser);

                    // Again, do we want to continue here?
                    return -1;
                }

                resolver->resolving_auto_gen = true;
                resolver->auto_gen_file_pos = declaration->file_pos;
                result &= try_resolve_declaration(resolver, gen_decl, scope);
                resolver->resolving_auto_gen = false;
                if (result)
                {
                    BUF_PUSH(resolver->module->global_declarations, gen_decl);
                }
            } while (gen_decl && parser.ti < BUF_LENGTH(parser.tokens) && result);

            if (result)
            {
                declaration->insert_decl.generated = true;
            }
        }

        return result;
    }

    static bool try_resolve_statement(Resolver* resolver, AST_Statement* statement,
                                      AST_Scope* scope, AST_Statement* break_context)
    {
        assert(resolver);
        assert(statement);
        assert(scope);

        bool result = true;

        switch (statement->kind)
        {
            case AST_STMT_DECLARATION:
            {
                result &= try_resolve_declaration(resolver, statement->declaration, scope);
                break;
            }

            case AST_STMT_RETURN:
            {
                result &= try_resolve_return_statement(resolver, statement, scope);
                break;
            }

            case AST_STMT_BLOCK:
            {
                for (uint64_t i = 0; i < BUF_LENGTH(statement->block.statements); i++)
                {
                    AST_Statement* stmt = statement->block.statements[i];
                    result &= try_resolve_statement(resolver, stmt, statement->block.scope,
                                                    break_context);
                }
                break;
            }

            case AST_STMT_IF:
            {
                result &= try_resolve_if_statement(resolver, statement, scope, break_context);
                break;
            }

            case AST_STMT_ASSIGN:
            {
                AST_Expression* lvalue_expr = statement->assign.lvalue_expression;
                result &= try_resolve_expression(resolver, lvalue_expr, scope);
                if (!result)
                {
                    return false;
                }

                if (result && lvalue_expr->is_const)
                {
                    resolver_report_error(resolver, statement->file_pos,
                                          "Cannot assign to constant expression");
                    return false;
                }

                AST_Type* suggested_type = nullptr;
                if (result)
                {
                    if (lvalue_expr->type->kind == AST_TYPE_STRUCT &&
                        statement->assign.expression->kind == AST_EXPR_COMPOUND_LITERAL)
                    {
                        suggested_type = statement->assign.lvalue_expression->type;
                    }
                    else if (lvalue_expr->type->kind == AST_TYPE_STATIC_ARRAY)
                    {
                        suggested_type = lvalue_expr->type;
                    }
                }
                result &= try_resolve_expression(resolver, statement->assign.expression,
                                                 scope, suggested_type);
                if (result && lvalue_expr->type->kind == AST_TYPE_ENUM)
                {
                    if (statement->assign.expression->type->kind == AST_TYPE_ENUM)
                    {
                        assert(lvalue_expr->type->aggregate_type.base_type ==
                               statement->assign.expression->type->aggregate_type.base_type);
                    }
                    else
                    {
                        assert(lvalue_expr->type->aggregate_type.base_type ==
                               statement->assign.expression->type);
                    }
                }
                else if (result && lvalue_expr->kind == AST_EXPR_IDENTIFIER)
                {
                    AST_Expression* assign_expr = statement->assign.expression;
                    if (lvalue_expr->type != assign_expr->type)
                    {
                        if ((lvalue_expr->type->flags & AST_TYPE_FLAG_FLOAT) &&
                            (assign_expr->type->flags & AST_TYPE_FLAG_INT))
                        {
                            AST_Expression* old_assign_expr =
                                ast_expression_new(resolver->context, assign_expr->file_pos,
                                                   assign_expr->kind);

                            *old_assign_expr = *assign_expr;
                            assign_expr->kind = AST_EXPR_CAST;
                            assign_expr->type = lvalue_expr->type;
                            assign_expr->cast_expr.type_spec = nullptr;
                            assign_expr->cast_expr.expr = old_assign_expr;
                        }
                        else
                        {
                            const char* format = "Mismatching types in assign statement\n\texpected: %s\n\tgot:\t%s";
                            auto expected_type_string = ast_type_to_string(lvalue_expr->type);
                            auto expr_type_string = ast_type_to_string(assign_expr->type);
                            resolver_report_error(resolver, statement->file_pos, format,
                                                  expected_type_string, expr_type_string);
                            mem_free(expected_type_string);
                            mem_free(expr_type_string);
                            return false;
                        }
                    }
                }
                break;
            }

            case AST_STMT_CALL:
            {
                result &= try_resolve_expression(resolver, statement->call_expression,
                                                 scope);
                break;
            }

            case AST_STMT_WHILE:
            {
                result &= try_resolve_expression(resolver, statement->while_stmt.cond_expr, scope);
                result &= try_resolve_statement(resolver, statement->while_stmt.body_stmt, scope,
                                                statement);
                break;
            }

            case AST_STMT_FOR:
            {
                AST_Scope* for_scope = statement->for_stmt.scope;
                result &= try_resolve_statement(resolver, statement->for_stmt.init_stmt, for_scope,
                                                break_context);
                result &= try_resolve_expression(resolver, statement->for_stmt.cond_expr, for_scope);
                result &= try_resolve_statement(resolver, statement->for_stmt.step_stmt, for_scope,
                                                break_context);
                result &= try_resolve_statement(resolver, statement->for_stmt.body_stmt, for_scope,
                                                statement);
                break;
            }

            case AST_STMT_SWITCH:
            {
                AST_Expression* switch_expr = statement->switch_stmt.switch_expression;
                result &= try_resolve_expression(resolver, switch_expr, scope);
                if (result)
                {
                    auto switch_type = switch_expr->type;
                    assert(switch_type->flags & AST_TYPE_FLAG_INT ||
                           (switch_type->kind == AST_TYPE_ENUM) &&
                            switch_type->aggregate_type.base_type->flags & AST_TYPE_FLAG_INT);

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
                                result &= try_resolve_expression(resolver, case_expr, scope,
                                                                 switch_expr->type);
                                if (!result)
                                {
                                    break;
                                }
                                assert(case_expr->is_const);
                            }

                            for (uint64_t i = 0; i < BUF_LENGTH(switch_case.range_expressions);
                                 i += 2)
                            {
                                AST_Expression* min = switch_case.range_expressions[i];
                                AST_Expression* max = switch_case.range_expressions[i + 1];

                                result &= try_resolve_expression(resolver, min, scope);
                                result &= try_resolve_expression(resolver, max, scope);

                                if (result)
                                {
                                    assert(min->is_const);
                                    assert(max->is_const);
                                    assert(min->type == max->type);
                                }
                            }
                        }

                        result &= try_resolve_statement(resolver, switch_case.stmt, scope,
                                                        break_context);
                    }
                }
                break;
            }

            case AST_STMT_BREAK:
            {
                assert(break_context);
                break;
            }

            case AST_STMT_INSERT:
            {
                result &= try_resolve_insert_statement(resolver, statement, scope, break_context);
                break;
            };

            default:
                assert(false);
                result = false;
                break;
        }

        return result;
    }

    static bool try_resolve_return_statement(Resolver* resolver, AST_Statement* statement,
                                             AST_Scope* scope)
    {
        assert(resolver);
        assert(statement);
        assert(statement->kind == AST_STMT_RETURN);
        assert(scope);

        assert(resolver->current_func_decl);
        AST_Declaration* func_decl = resolver->current_func_decl;

        bool result = true;

        AST_Expression* return_expression = statement->return_expression;
        if (return_expression)
        {
            result &= try_resolve_expression(resolver, return_expression, scope);

            if (result) assert(return_expression->type);

            if (func_decl->function.inferred_return_type &&
                return_expression->type)
            {
                assert(func_decl->function.inferred_return_type ==
                       return_expression->type);
            }
            else
            {
                func_decl->function.inferred_return_type = return_expression->type;
            }
        }

        return result;
    }

    static bool try_resolve_if_statement(Resolver* resolver, AST_Statement* statement,
                                         AST_Scope* scope, AST_Statement* break_context)
    {
        assert(resolver);
        assert(statement);
        assert(statement->kind == AST_STMT_IF);
        assert(scope);

        bool result = true;

        result &= try_resolve_expression(resolver, statement->if_stmt.if_expression, scope);
        result &= try_resolve_statement(resolver, statement->if_stmt.then_statement, scope,
                                        break_context);

        if (statement->if_stmt.else_statement)
        {
            result &= try_resolve_statement(resolver, statement->if_stmt.else_statement, scope,
                break_context);
        }

        return result;
    }

    static bool try_resolve_insert_statement(Resolver* resolver, AST_Statement* statement, AST_Scope* scope,
                                             AST_Statement* break_context)
    {
        assert(resolver);
        assert(statement);
        assert(statement->kind == AST_STMT_INSERT);
        assert(scope);

        AST_Statement* insert_stmt = statement->insert.statement;

        bool result = try_resolve_statement(resolver, insert_stmt, scope, break_context);
        if (result)
        {
            assert(insert_stmt->kind == AST_STMT_CALL);
            auto u8_ptr_type = ast_find_or_create_pointer_type(resolver->context, Builtin::type_u8);
            assert(insert_stmt->call_expression->type == u8_ptr_type);

            char* insert_string = run_insert(resolver, insert_stmt->call_expression);
            assert(insert_string);

            Lexer lexer;
            init_lexer(&lexer, resolver->context);

            Lex_Result lex_result = lex_file(&lexer, insert_string, "<insert_auto_gen>");
            if (BUF_LENGTH(lex_result.errors) != 0)
            {
                lexer_report_errors(&lexer);

                // We might want to continue here, to try and parse what we have?
                return -1;
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

                // Again, do we want to continue here?
                return -1;
            }

            resolver->resolving_auto_gen = true;
            resolver->auto_gen_file_pos = statement->file_pos;
            result &= try_resolve_statement(resolver, gen_stmt, scope, break_context);
            resolver->resolving_auto_gen = false;
            if (result)
            {
                statement->insert.gen_statement = gen_stmt;
            }
        }

        return result;
    }

    static bool try_resolve_expression(Resolver* resolver, AST_Expression* expression,
                                       AST_Scope* scope,
                                       AST_Type* suggested_type/*=nullptr*/)
    {
        assert(resolver);
        assert(expression);
        assert(scope);

        bool result = true;

        switch (expression->kind)
        {
            case AST_EXPR_BINARY:
            {
                result &= try_resolve_binary_expression(resolver, expression, scope);
                break;
            }

            case AST_EXPR_UNARY:
            {
                result &= try_resolve_unary_expression(resolver, expression, scope);
                break;
            }

            case AST_EXPR_IDENTIFIER:
            {
                result &= try_resolve_identifier_expression(resolver, expression, scope);
                break;
            }

            case AST_EXPR_CALL:
            {
                result &= try_resolve_call_expression(resolver, expression, scope);
                break;
            }

            case AST_EXPR_SUBSCRIPT:
            {
                AST_Expression* base_expr = expression->subscript.base_expression;
                result &= try_resolve_expression(resolver, expression->subscript.index_expression,
                                                 scope);
                result &= try_resolve_expression(resolver, base_expr, scope);
                if (result)
                {
                    if (base_expr->type->kind == AST_TYPE_POINTER)
                    {
                        expression->type = base_expr->type->pointer.base;
                    }
                    else if (base_expr->type->kind == AST_TYPE_STATIC_ARRAY)
                    {
                        expression->type = base_expr->type->static_array.base;
                    }
                    else
                    {
                        auto got_type_str = ast_type_to_string(base_expr->type);
                        resolver_report_error(resolver, base_expr->file_pos,
                                              "lvalue of subscript must be of pointer or array type, got: %s\n",
                                              got_type_str);
                        mem_free(got_type_str);
                        return false;
                    }
                }
                break;
            }

            case AST_EXPR_BOOL_LITERAL:
            {
                result &= try_resolve_boolean_literal_expression(resolver, expression);
                break;
            }

			case AST_EXPR_NULL_LITERAL:
			{
                result &= try_resolve_null_literal_expression(resolver, expression);
				break;
			}

            case AST_EXPR_STRING_LITERAL:
            {
                result &= try_resolve_string_literal_expression(resolver, expression);
                break;
            }

            case AST_EXPR_INTEGER_LITERAL:
            {
                result &= try_resolve_integer_literal_expression(resolver, expression,
                                                                 suggested_type);
                break;
            }

            case AST_EXPR_FLOAT_LITERAL:
            {
                result &= try_resolve_float_literal_expression(resolver, expression,
                                                               suggested_type);
                break;
            }

            case AST_EXPR_CHAR_LITERAL:
            {
                result &= try_resolve_character_literal_expression(resolver, expression);
                break;
            }

            case AST_EXPR_COMPOUND_LITERAL:
            {
                result &= try_resolve_compound_literal_expression(resolver, expression, scope,
                                                                  suggested_type);
                break;
            }

            case AST_EXPR_ARRAY_LENGTH:
            {
                result &= try_resolve_array_length_expression(resolver, expression, scope);
                break;
            }

            case AST_EXPR_DOT:
            {
                result &= try_resolve_dot_expression(resolver, expression, scope);
                break;
            }

			case AST_EXPR_CAST:
			{
				result &= try_resolve_cast_expression(resolver, expression, scope);
				break;
			}

            case AST_EXPR_SIZEOF:
            {
                AST_Type* type = nullptr;
                result &= try_resolve_type_spec(resolver, expression->sizeof_expr.type_spec, &type, scope);
                if (result)
                {
                    assert(type);
                    expression->sizeof_expr.byte_size = type->bit_size / 8;
                    expression->type = Builtin::type_u64;
                }
                break;
            }

            default:
            {
                assert(false);
                result = false;
                break;
            }
        }

        if (result)
        {
            assert(expression->type ||
                   (expression->kind == AST_EXPR_IDENTIFIER &&
                    expression->identifier->declaration->kind == AST_DECL_IMPORT) ||
                   (expression->kind == AST_EXPR_IDENTIFIER &&
                       expression->identifier->declaration->kind == AST_DECL_AGGREGATE_TYPE &&
                       expression->identifier->declaration->aggregate_type.kind == AST_AGG_DECL_ENUM) ||
                   expression->kind == AST_EXPR_DOT);
        }

        return result;
    }

    static bool try_resolve_call_expression(Resolver* resolver, AST_Expression* expression,
                                            AST_Scope* scope)
    {
        assert(resolver);
        assert(expression);
        assert(scope);

        bool result = true;

        AST_Declaration* func_decl = nullptr;
        AST_Expression* ident_expr = expression->call.ident_expression;
        bool recursive = false;

        if (ident_expr->kind == AST_EXPR_IDENTIFIER &&
            resolver->current_func_decl &&
            ident_expr->identifier->atom == resolver->current_func_decl->identifier->atom)
        {
            recursive = true;
        }

        result &= try_resolve_expression(resolver, ident_expr, scope);
        if (!result && !recursive)
        {
            return false;
        }

        if (ident_expr->kind == AST_EXPR_IDENTIFIER)
        {
            if (ident_expr->identifier->declaration)
            {
                func_decl = ident_expr->identifier->declaration;
            }
            else if (recursive)
            {
                func_decl = resolver->current_func_decl;
                assert(!result);
                result = true;
            }
            else assert(false);
        }
        else if (ident_expr->kind == AST_EXPR_DOT)
        {
            assert(ident_expr->dot.declaration);
            func_decl = ident_expr->dot.declaration;
        }
        else assert(false);

        assert(func_decl);
        if (func_decl->function.overloads)
        {
            func_decl = find_overload_signature_match(resolver, func_decl, expression, scope);
            if (!func_decl) return false;
        }
        AST_Type* func_type = nullptr;

		if (func_decl->kind == AST_DECL_FUNC)
		{
            func_type = func_decl->function.type;
		}
		else if (func_decl->kind == AST_DECL_MUTABLE && func_decl->mutable_decl.type &&
			func_decl->mutable_decl.type->kind == AST_TYPE_POINTER &&
			func_decl->mutable_decl.type->pointer.base->kind == AST_TYPE_FUNCTION)
		{
            func_type = func_decl->mutable_decl.type->pointer.base;
		}
		else assert(false);

		assert(func_type || recursive);
        if (func_type)
        {
            assert(func_type->kind == AST_TYPE_FUNCTION);
            auto arg_count = BUF_LENGTH(expression->call.arg_expressions);
            auto expected_arg_count = BUF_LENGTH(func_type->function.arg_types);

            if (arg_count != expected_arg_count)
            {
                if ((arg_count > expected_arg_count && !func_type->function.is_vararg) ||
                    arg_count < expected_arg_count)
                {
                    resolver_report_error(resolver, expression->file_pos,
                                          "Got %lu argument(s) for function call, expected %lu for function: '%s'",
                                          arg_count, expected_arg_count,
                                          ident_expr->identifier->atom.data);
                    return false;
                }
            }
        }

        for (uint64_t i = 0; i < BUF_LENGTH(expression->call.arg_expressions); i++)
        {
            AST_Type* specified_arg_type = nullptr;
            if (func_type)
            {
                if (i < BUF_LENGTH(func_type->function.arg_types))
                {
                    specified_arg_type = func_type->function.arg_types[i];
                }
                else
                {
                    assert(func_type->function.is_vararg);
                }
            }

            AST_Expression* arg_expr = expression->call.arg_expressions[i];
            result &= try_resolve_expression(resolver, arg_expr, scope, specified_arg_type);

            if (!result)
            {
                return false;
            }

            if (result && specified_arg_type && !(arg_expr->type == specified_arg_type))
            {
                if (specified_arg_type->kind == AST_TYPE_POINTER &&
                    (arg_expr->type->kind == AST_TYPE_POINTER &&
                     arg_expr->type->pointer.base == Builtin::type_void))
                {
                }
                else if ((specified_arg_type->kind == AST_TYPE_POINTER &&
                          specified_arg_type->pointer.base == Builtin::type_void) &&
                         arg_expr->type->kind == AST_TYPE_POINTER)
                {
                }
                else if (specified_arg_type->kind == AST_TYPE_ENUM &&
                         specified_arg_type->aggregate_type.base_type == arg_expr->type)
                {
                }
                else if (is_valid_integer_promotion(arg_expr->type, specified_arg_type))
                {
                }
                else if ((specified_arg_type->flags & AST_TYPE_FLAG_FLOAT) &&
                         (arg_expr->type->flags & AST_TYPE_FLAG_INT) &&
                         arg_expr->type->bit_size >= specified_arg_type->bit_size)
                {
                    AST_Expression* old_arg_expr = ast_expression_new(resolver->context,
                                                                      arg_expr->file_pos,
                                                                      arg_expr->kind);
                    *old_arg_expr = *arg_expr;

                    arg_expr->kind = AST_EXPR_CAST;
                    arg_expr->type = specified_arg_type;
                    arg_expr->cast_expr.type_spec = nullptr;
                    arg_expr->cast_expr.expr = old_arg_expr;
                }
                else if (specified_arg_type == Builtin::type_String &&
                         arg_expr->type == Builtin::type_pointer_to_u8)
                {
                    AST_Expression* old_arg_expr = ast_expression_new(resolver->context,
                                                                     arg_expr->file_pos,
                                                                     arg_expr->kind);
                    *old_arg_expr = *arg_expr;

                    arg_expr->kind = AST_EXPR_COMPOUND_LITERAL;
                    arg_expr->type = specified_arg_type;
                    arg_expr->compound_literal.expressions = nullptr;
                    BUF_PUSH(arg_expr->compound_literal.expressions, old_arg_expr);

                    AST_Expression* length_expr = nullptr;
                    if (old_arg_expr->kind == AST_EXPR_STRING_LITERAL)
                    {
                        auto str_length = old_arg_expr->string_literal.atom.length;
                        length_expr =
                            ast_integer_literal_expression_new(resolver->context,
                                                               arg_expr->file_pos,
                                                               str_length);
                        length_expr->type = Builtin::type_u64;
                    }
                    else
                    {
                        BUF(AST_Expression*) args = nullptr;
                        BUF_PUSH(args, old_arg_expr);
                        auto strlen_ident_expr =
                            ast_ident_expression_new(resolver->context,
                                                     old_arg_expr->file_pos,
                                                     Builtin::decl_string_length->identifier);

                        length_expr = ast_call_expression_new(resolver->context, old_arg_expr->file_pos,
                                                              strlen_ident_expr, args);
                        length_expr->call.callee_declaration = Builtin::decl_string_length;
                    }

                    assert(length_expr);
                    BUF_PUSH(arg_expr->compound_literal.expressions, length_expr);
                }
                else
                {
                    const char* format = "Mismatching type for argument %lu\n\tExpected type: %s\n\tGot type: %s";
                    // TODO: temp mem
                    const char* expected_type_string = ast_type_to_string(specified_arg_type);
                    const char* arg_type_string = ast_type_to_string(arg_expr->type);
                    resolver_report_error(resolver, arg_expr->file_pos, format, i,
                                          expected_type_string, arg_type_string);
                    mem_free(expected_type_string);
                    mem_free(arg_type_string);
                    return false;
                }
            }
        }

        if (result && !expression->type)
        {
            if (func_type)
            {
                assert(func_type->function.return_type);
                expression->type = func_type->function.return_type;
            }
            else
            {
                assert(func_decl);
                assert(func_decl->function.return_type_spec);

                AST_Type* return_type = nullptr;
                result &= try_resolve_type_spec(resolver, func_decl->function.return_type_spec,
                                                &return_type, scope);
                if (result)
                {
                    expression->type = return_type;
                }
            }

            assert(expression->type);
        }

        if (!expression->type)
        {
            if (expression->call.ident_expression->kind == AST_EXPR_IDENTIFIER)
            {
                resolver_report_error(resolver,expression->file_pos,
                                      "Circular dependency when trying to infer return type of function '%s'",
                                      ident_expr->identifier->atom.data);
            }
            result = false;
        }

        if (result && func_decl)
        {
            expression->call.callee_declaration = func_decl;
        }

        return result;
    }

    static bool try_resolve_boolean_literal_expression(Resolver* resolver, AST_Expression* expression)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_BOOL_LITERAL);

        if (!expression->type)
        {
            expression->type = Builtin::type_bool;
        }
        expression->is_const = true;

        return true;
    }

    static bool try_resolve_null_literal_expression(Resolver* resolver,
                                                    AST_Expression* expression)
    {
        assert(resolver);
        assert(expression);

        if (!expression->type)
        {
            expression->type = ast_find_or_create_pointer_type(resolver->context, Builtin::type_void);
        }
        expression->is_const = true;

        return true;
    }

    static bool try_resolve_string_literal_expression(Resolver* resolver, AST_Expression* expression)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_STRING_LITERAL);

        if (!expression->type)
        {
            expression->type = ast_find_or_create_pointer_type(resolver->context,
                                                               Builtin::type_u8);
        }
        expression->is_const = true;

        return true;
    }

    static bool try_resolve_integer_literal_expression(Resolver* resolver,
                                                       AST_Expression* expression,
                                                       AST_Type* suggested_type)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_INTEGER_LITERAL);

        if (!expression->type || suggested_type != expression->type)
        {
            if (suggested_type)
            {
                if (suggested_type->flags & AST_TYPE_FLAG_INT)
                {}
                else if (suggested_type->flags & AST_TYPE_FLAG_FLOAT)
                {
                    uint64_t int_value = expression->integer_literal.u64;
                    expression->kind = AST_EXPR_FLOAT_LITERAL;
                    expression->float_literal.r32 = (float)int_value;
                    expression->float_literal.r64 = (double)int_value;
                }
                else assert(false);

                expression->type = suggested_type;
            }
            else
            {
                expression->type = Builtin::type_int;
            }
        }
        expression->is_const = true;

        return true;
    }

    static bool try_resolve_float_literal_expression(Resolver* resolver,
                                                     AST_Expression* expression,
                                                     AST_Type* suggested_type)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_FLOAT_LITERAL);

        if (!expression->type)
        {
            if (suggested_type && suggested_type->flags & AST_TYPE_FLAG_FLOAT)
            {
                expression->type = suggested_type;
            }
            else
            {
                expression->type = Builtin::type_float;
            }
        }

        expression->is_const = true;

        return true;
    }

    static bool try_resolve_character_literal_expression(Resolver* resolver, AST_Expression* expression)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_CHAR_LITERAL);

        if (!expression->type)
        {
            expression->type = Builtin::type_u8;
        }
        expression->is_const = true;

        return true;
    }

    static bool try_resolve_compound_literal_expression(Resolver* resolver, AST_Expression* expression, AST_Scope* scope,
                                                        AST_Type* suggested_type/*=nullptr*/)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_COMPOUND_LITERAL);
        assert(scope);

        bool result = true;
        bool same_type = true;
        AST_Type* first_type = nullptr;

        for (uint64_t i = 0; i < BUF_LENGTH(expression->compound_literal.expressions); i++)
        {
            AST_Type* suggested_member_type = nullptr;
            if (suggested_type && suggested_type->kind == AST_TYPE_STRUCT)
            {
                assert(i < BUF_LENGTH(suggested_type->aggregate_type.member_declarations));
                AST_Declaration* member_decl = suggested_type->aggregate_type.member_declarations[i];
                assert(member_decl->kind == AST_DECL_MUTABLE);
                suggested_member_type = member_decl->mutable_decl.type;
            }
            else if (suggested_type && suggested_type->kind == AST_TYPE_STATIC_ARRAY)
            {
                suggested_member_type = suggested_type->static_array.base;
            }
            AST_Expression* expr = expression->compound_literal.expressions[i];
            result &= try_resolve_expression(resolver, expr, scope, suggested_member_type);

            if (result)
            {
                if (!first_type)
                {
                    first_type = expr->type;
                }

                same_type = first_type == expr->type;
            }
            else
            {
                return false;
            }
        }


        if (suggested_type && suggested_type->kind == AST_TYPE_STRUCT)
        {
            if (BUF_LENGTH(suggested_type->aggregate_type.member_declarations) !=
                BUF_LENGTH(expression->compound_literal.expressions))
            {
                result = false;
            }
            else
            {
                bool match = true;
                for (uint64_t i = 0; i < BUF_LENGTH(expression->compound_literal.expressions); i++)
                {
                    AST_Expression* expr = expression->compound_literal.expressions[i];
                    AST_Declaration* struct_member_decl =
                        suggested_type->aggregate_type.member_declarations[i];
                    AST_Type* member_type = struct_member_decl->mutable_decl.type;
                    if (!try_resolve_expression(resolver, expr, scope, member_type))
                    {
                        return false;
                    }
                    assert(expr->type);

                    if (expr->type != member_type)
                    {
                        if (expr->type == Builtin::type_float && member_type ==
                            Builtin::type_double)
                        {
                        }
                        if ((expr->type->flags & AST_TYPE_FLAG_INT) &&
                            (member_type->flags & AST_TYPE_FLAG_FLOAT))
                        {
                            AST_Expression* old_expr = ast_expression_new(resolver->context,
                                                                          expr->file_pos,
                                                                          expr->kind);
                            *old_expr = *expr;

                            expr->kind = AST_EXPR_CAST;
                            expr->type = member_type;
                            expr->cast_expr.type_spec = nullptr;
                            expr->cast_expr.expr = old_expr;
                        }
                        else
                        {
                            match = false;
                            auto expected_type_str = ast_type_to_string(member_type);
                            auto got_type_str = ast_type_to_string(expr->type);
                            resolver_report_error(resolver, expr->file_pos,
                                                  "Type of compound member does not match type of struct member: %s\n\texpected: %s\n\tgot: %s",
                                                  struct_member_decl->identifier->atom.data,
                                                  expected_type_str, got_type_str);
                            mem_free(expected_type_str);
                            mem_free(got_type_str);
                        }
                    }
                }

                if (match)
                {
                    if (!expression->type) expression->type = suggested_type;
                }
                else
                {
                    result = false;
                }
            }
        }
        else if (suggested_type && suggested_type->kind == AST_TYPE_STATIC_ARRAY)
        {
            assert(same_type);
            // if (!same_type)
            // {
            //     resolver_report_error(resolver, expression->file_pos,
            //                           "Elements of array compound expression are not of the same type");
            //     return false;
            // }
            expression->type = ast_find_or_create_array_type(resolver->context, first_type,
                                                             BUF_LENGTH(expression->compound_literal.expressions));
        }
        else
        {
            resolver_report_error(resolver, expression->file_pos, "Could not infer type for compound literal");
            return false;
        }

        return result;
    }

    static bool try_resolve_array_length_expression(Resolver* resolver,
                                                    AST_Expression* expression,
                                                    AST_Scope* scope)
    {
        assert(resolver);
        assert(expression);
        assert(scope);

        bool result = true;

        AST_Expression* ident_expr = expression->array_length.ident_expr;
        result &= try_resolve_expression(resolver, ident_expr, scope);
        if (result)
        {
            AST_Declaration* array_decl = nullptr;
            if (ident_expr->kind == AST_EXPR_IDENTIFIER)
            {
                array_decl = ident_expr->identifier->declaration;
            }
            else if (ident_expr->kind == AST_EXPR_DOT)
            {
                array_decl = ident_expr->dot.declaration;
            }
            else assert(false);

            assert(array_decl);
            assert(array_decl->kind == AST_DECL_MUTABLE);
            assert(array_decl->mutable_decl.type);
            expression->type = Builtin::type_int;
        }

        return result;
    }

    static bool try_resolve_identifier_expression(Resolver* resolver, AST_Expression* expression,
                                                  AST_Scope* scope)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_IDENTIFIER);

        AST_Identifier* ident = expression->identifier;

        bool result = try_resolve_identifier(resolver, ident, scope);

        if (result)
        {
            assert(ident->declaration);
            AST_Declaration* decl = ident->declaration;
            if (decl->kind == AST_DECL_MUTABLE)
            {
                expression->type = ident->declaration->mutable_decl.type;
            }
            else if (decl->kind == AST_DECL_CONSTANT_VAR)
            {
                expression->type = ident->declaration->constant_var.type;
                expression->is_const = true;
            }
            else if (decl->kind == AST_DECL_IMPORT)
            {
                expression->type = nullptr;
                expression->is_const = true;
            }
            else if (decl->kind == AST_DECL_FUNC)
            {
                expression->type = decl->function.type;
                expression->is_const = true;
            }
            else if (decl->kind == AST_DECL_AGGREGATE_TYPE &&
                     decl->aggregate_type.kind == AST_AGG_DECL_ENUM)
            {
                expression->is_const = true;
            }
            else assert(false);

            assert(expression->type || decl->kind == AST_DECL_IMPORT ||
                   (decl->kind == AST_DECL_AGGREGATE_TYPE &&
                    decl->aggregate_type.kind == AST_AGG_DECL_ENUM));
        }

        return result;
    }

    static bool try_resolve_binary_expression(Resolver* resolver, AST_Expression* expression,
                                              AST_Scope* scope)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_BINARY);
        assert(scope);

        bool result = true;

        AST_Expression* lhs = expression->binary.lhs;
        AST_Expression* rhs = expression->binary.rhs;

        if (lhs->kind == AST_EXPR_INTEGER_LITERAL && rhs->kind != AST_EXPR_INTEGER_LITERAL)
        {
            result &= try_resolve_expression(resolver, rhs, scope);
            if (!result) return false;
            assert(rhs->type);
            result &= try_resolve_expression(resolver, lhs, scope, rhs->type);
            if (!result) return false;
        }
        else if (rhs->kind == AST_EXPR_INTEGER_LITERAL && lhs->kind != AST_EXPR_INTEGER_LITERAL)
        {
            result &= try_resolve_expression(resolver, lhs, scope);
            if (!result) return false;
            assert(lhs->type);
            result &= try_resolve_expression(resolver, rhs, scope, lhs->type);
            if (!result) return false;
        }
        else
        {
            result &= try_resolve_expression(resolver, lhs, scope);
            if (!result) return false;
            result &= try_resolve_expression(resolver, rhs, scope);
            if (!result) return false;
        }

        if (result && !expression->type)
        {
            if (lhs->type == rhs->type)
            {
                expression->type = lhs->type;
            }
            else if (lhs->type->kind == AST_TYPE_ENUM)
            {
                assert(lhs->type->aggregate_type.base_type == rhs->type);
                expression->type = rhs->type;
            }
            else if (rhs->type->kind == AST_TYPE_ENUM)
            {
                assert(rhs->type->aggregate_type.base_type == lhs->type);

            }
            else if ((lhs->type->flags & AST_TYPE_FLAG_FLOAT) &&
                     (rhs->type->flags & AST_TYPE_FLAG_INT))
            {
                AST_Expression* old_rhs = ast_expression_new(resolver->context, rhs->file_pos,
                                                             rhs->kind);
                *old_rhs = *rhs;
                rhs->kind = AST_EXPR_CAST;
                rhs->type = lhs->type;
                rhs->cast_expr.type_spec = nullptr;
                rhs->cast_expr.expr = old_rhs;
                rhs->is_const = old_rhs->is_const;

                expression->type = lhs->type;
            }
            else if ((lhs->type->flags & AST_TYPE_FLAG_INT) &&
                     (rhs->type->flags & AST_TYPE_FLAG_FLOAT))
            {
                AST_Expression* old_lhs = ast_expression_new(resolver->context, lhs->file_pos,
                                                             lhs->kind);
                *old_lhs = *lhs;
                lhs->kind = AST_EXPR_CAST;
                lhs->type = rhs->type;
                lhs->cast_expr.type_spec = nullptr;
                lhs->cast_expr.expr = old_lhs;
                lhs->is_const = old_lhs->is_const;

                expression->type = rhs->type;
            }
            else
            {
                const char* lhs_type_string = ast_type_to_string(lhs->type);
                const char* rhs_type_string = ast_type_to_string(rhs->type);
                resolver_report_error(resolver, expression->file_pos,
                                        "Mismatching types in binary expression\n\tLeft: %s\n\tRight: %s",
                                        lhs_type_string, rhs_type_string);
                return false;
            }


            if (lhs->is_const && rhs->is_const)
            {
                expression->is_const = true;
            }
        }

        return result;
    }

    static bool try_resolve_unary_expression(Resolver* resolver, AST_Expression* expression,
                                             AST_Scope* scope)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_UNARY);
        assert(scope);

        AST_Expression* operand_expr = expression->unary.operand;
        bool result = try_resolve_expression(resolver, operand_expr, scope);

        if (result)
        {
            switch (expression->unary.op)
            {
                case AST_UNOP_MINUS:
                {
                    assert((operand_expr->type->flags & AST_TYPE_FLAG_INT) ||
                           (operand_expr->type->flags & AST_TYPE_FLAG_FLOAT));

                    if (operand_expr->is_const)
                    {
                        if (operand_expr->type == Builtin::type_int)
                        {
                            expression->type = Builtin::type_int;
                            int64_t value = const_interpret_s64_expression(resolver->context,
                                                                           expression, scope);
                            expression->kind = AST_EXPR_INTEGER_LITERAL;
                            expression->integer_literal.u64 = value;
                        }
                        else if (operand_expr->type == Builtin::type_float)
                        {
                            expression->type = Builtin::type_float;
                            float value = const_interpret_float_expression(expression, scope);
                            expression->kind = AST_EXPR_FLOAT_LITERAL;
                            expression->float_literal.r32 = value;
                            expression->float_literal.r64 = (double)value;
                        }
                        else assert(false);
                    }
                    else
                    {
                        expression->type = operand_expr->type;
                    }
                    expression->is_const = operand_expr->is_const;
                    break;
                }

                case AST_UNOP_ADDROF:
                {
                    assert(operand_expr->kind == AST_EXPR_IDENTIFIER ||
                           operand_expr->kind == AST_EXPR_SUBSCRIPT ||
                            operand_expr->kind == AST_EXPR_DOT);

                    if (operand_expr->kind == AST_EXPR_DOT)
                    {
                        assert(operand_expr->dot.base_expression->kind == AST_EXPR_IDENTIFIER);
                        assert(operand_expr->dot.member_expression->kind == AST_EXPR_IDENTIFIER);
                    }
                    expression->type = ast_find_or_create_pointer_type(resolver->context,
                                                                       operand_expr->type);
                    break;
                }

                case AST_UNOP_DEREF:
                {
                    assert(operand_expr->kind == AST_EXPR_IDENTIFIER);
                    assert(operand_expr->type->kind == AST_TYPE_POINTER);
                    expression->type = operand_expr->type->pointer.base;
                    break;
                }

                case AST_UNOP_NOT:
                {
                    assert(operand_expr->type == Builtin::type_bool ||
                           (operand_expr->type->flags & AST_TYPE_FLAG_INT) ||
                           operand_expr->type->kind == AST_TYPE_POINTER);
                    expression->type = Builtin::type_bool;
                    break;
                }

                default: assert(false);
            }
        }

        return result;
    }

    static bool try_resolve_dot_expression(Resolver* resolver, AST_Expression* expression,
                                           AST_Scope* scope)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_DOT);
        assert(scope);

        bool result = true;

        auto base_expr = expression->dot.base_expression;
        auto member_expr = expression->dot.member_expression;

        assert(base_expr->kind == AST_EXPR_IDENTIFIER ||
               base_expr->kind == AST_EXPR_DOT);
        assert(member_expr->kind == AST_EXPR_IDENTIFIER);

        result &= try_resolve_expression(resolver, base_expr, scope);
        if (!result)
        {
            return false;
        }

        AST_Declaration* base_decl = nullptr;
        if (base_expr->kind == AST_EXPR_IDENTIFIER)
        {
            assert(base_expr->identifier->declaration);
            base_decl = base_expr->identifier->declaration;
        }
        else if (base_expr->kind == AST_EXPR_DOT)
        {
            assert(base_expr->dot.declaration);
            base_decl = base_expr->dot.declaration;
        }

        assert(base_decl);

        switch (base_decl->kind)
        {
            case AST_DECL_AGGREGATE_TYPE:
            {
                if (base_decl->aggregate_type.kind == AST_AGG_DECL_ENUM)
                {
                    AST_Declaration* member_decl =
                        find_declaration(resolver->context, base_decl->aggregate_type.scope,
                                         member_expr->identifier);

                    if (!member_decl)
                    {
                        report_undeclared_identifier(resolver, member_expr->file_pos,
                                                     resolver->module, member_expr->identifier);
                        return false;
                        assert(false);
                    }
                    assert(member_decl->kind == AST_DECL_CONSTANT_VAR);

                    expression->type = member_decl->constant_var.type;
                    expression->is_const = true;
                    expression->dot.declaration = member_decl;
                }
                else assert(false);
                break;
            }

            case AST_DECL_MUTABLE:
            {
                AST_Type* struct_type = nullptr;
                bool is_struct_type = false;
                if (base_decl->mutable_decl.type->kind == AST_TYPE_STRUCT)
                {
                    struct_type = base_decl->mutable_decl.type;
                    is_struct_type = true;
                }
                else if (base_decl->mutable_decl.type->kind == AST_TYPE_POINTER)
                {
                    AST_Type* pointer_type = base_decl->mutable_decl.type;
                    if (pointer_type->pointer.base->kind == AST_TYPE_STRUCT)
                    {
                        struct_type = pointer_type->pointer.base;
                        is_struct_type = true;
                    }
                }
                else assert(false);

                if (is_struct_type)
                {
                    assert(struct_type);
                    auto struct_members = struct_type->aggregate_type.member_declarations;

                    bool found = false;
                    for (uint64_t i = 0; i < BUF_LENGTH(struct_members); i++)
                    {
                        AST_Declaration* struct_member = struct_members[i];
                        assert(struct_member->kind == AST_DECL_MUTABLE);
                        assert(struct_member->location = AST_DECL_LOC_AGGREGATE_MEMBER);
                        assert(struct_member->mutable_decl.type);

                        if (struct_member->identifier->atom == member_expr->identifier->atom)
                        {
                            expression->type = struct_member->mutable_decl.type;
                            expression->dot.declaration = struct_member;
                            found = true;
                            break;
                        }
                    }

                    if (!found)
                    {
                        resolver_report_error(resolver, expression->file_pos,
                                              "Reference to undeclared struct member '%s' in struct '%s'",
                                              member_expr->identifier->atom.data,
                                              struct_type->name);
                        return false;
                    }
                }
                else assert(false);
                break;
            }

            case AST_DECL_IMPORT:
            {
                assert(base_decl->import.module);

                AST_Module* ast_module = base_decl->import.module;

                result &= try_resolve_identifier_expression(resolver, member_expr,
                                                            ast_module->module_scope);
                if (result)
                {
                    expression->type = member_expr->type;
                    expression->is_const = member_expr->is_const;
                    assert(member_expr->identifier->declaration);
                    expression->dot.declaration = member_expr->identifier->declaration;
                }

                break;
            }

            default: assert(false);
        }

        if (result)
        {
            assert(expression->dot.declaration);
            assert(expression->type ||
                   (expression->dot.declaration->kind == AST_DECL_AGGREGATE_TYPE &&
                       expression->dot.declaration->aggregate_type.kind == AST_AGG_DECL_ENUM));
        }

        return result;
    }

	static bool try_resolve_cast_expression(Resolver* resolver, AST_Expression* expression,
		AST_Scope* scope)
	{
		assert(resolver);
		assert(expression);
		assert(scope);

		bool result = true;

		AST_Type* type = nullptr;
        if (!expression->type)
        {
            result &= try_resolve_type_spec(resolver, expression->cast_expr.type_spec, &type,
                                            scope);
        }

		result &= try_resolve_expression(resolver, expression->cast_expr.expr, scope);

		if (result && !expression->type)
		{
			assert(type);
			expression->type = type;
		}

        if (result)
        {
            expression->is_const = expression->cast_expr.expr->is_const;
        }

		return result;
	}

    static bool try_resolve_identifier(Resolver* resolver, AST_Identifier* identifier,
                                       AST_Scope* scope)
    {
        assert(resolver);
        assert(identifier);
        assert(scope);

        AST_Declaration* decl = find_declaration(resolver->context, scope, identifier);
        if (!decl)
        {
            report_undeclared_identifier(resolver, identifier->file_pos, identifier);
            return false;
        }

        if (!(decl->flags & AST_DECL_FLAG_RESOLVED))
        {
            return false;
        }

        if (decl->kind == AST_DECL_MUTABLE)
        {
            if (!decl->mutable_decl.type)
            {
                return false;
            }
        }
        else if (decl->kind == AST_DECL_CONSTANT_VAR)
        {
            if (!decl->constant_var.type)
            {
                return false;
            }
        }
		else if (decl->kind == AST_DECL_FUNC)
		{
			if (!decl->function.type)
			{
				return false;
			}
		}
        else if (decl->kind == AST_DECL_IMPORT)
        {
            // Do nothing for now.
        }
        else if (decl->kind == AST_DECL_AGGREGATE_TYPE)
        {
            // Do nothing for now
        }
        else assert(false);

        identifier->declaration = decl;

        return true;
    }

    bool try_resolve_type_spec(Resolver* resolver, AST_Type_Spec* type_spec,
                                      AST_Type** type_dest, AST_Scope* scope)
    {
        assert(resolver);
        assert(type_spec);
        assert(type_dest);
        assert(*type_dest == nullptr);
        assert(scope);

        switch (type_spec->kind)
        {
            case AST_TYPE_SPEC_IDENT:
            {
                assert(type_spec->identifier.identifier);
                AST_Identifier* identifier = type_spec->identifier.identifier;

                AST_Declaration* type_decl = find_declaration(resolver->context, scope,
					                                          type_spec->identifier.identifier);
                if (type_decl && type_decl->kind == AST_DECL_AGGREGATE_TYPE &&
                    type_decl->aggregate_type.kind == AST_AGG_DECL_STRUCT &&
                    type_decl->aggregate_type.parameter_idents)
                {
                    return find_or_create_poly_struct_type(resolver, type_decl, type_spec,
                                                           type_dest, scope);
                }

                if (!type_decl)
                {
                    report_undeclared_identifier(resolver, identifier->file_pos, identifier);
                    return false;
                }

                switch (type_decl->kind)
                {
                    case AST_DECL_TYPE:
                    {
                        AST_Type* type = type_decl->type.type;
                        if (type)
                        {
                            *type_dest = type;
                            return true;
                        }
                        break;
                    }

                    case AST_DECL_AGGREGATE_TYPE:
                    {
                        AST_Type* type = type_decl->aggregate_type.type;
                        if (type)
                        {
                            *type_dest = type;
                            return true;
                        }
                        break;
                    }

					case AST_DECL_TYPEDEF:
					{
						AST_Type* type = type_decl->typedef_decl.type;
						if (type)
						{
							*type_dest = type;
							return true;
						}
						break;
					}

                    default: assert(false);
                }

                return false;
            }

            case AST_TYPE_SPEC_DOT:
            {
                bool base_result = try_resolve_identifier(resolver, type_spec->dot.module_ident,
                                                          scope);
                if (base_result)
                {
                    assert(type_spec->dot.module_ident);
                    assert(type_spec->dot.module_ident->declaration);
                    AST_Declaration* module_decl = type_spec->dot.module_ident->declaration;
                    assert(module_decl->kind == AST_DECL_IMPORT);
                    assert(module_decl->import.module);

                    AST_Scope* module_scope = module_decl->import.module->module_scope;
                    AST_Type* type = nullptr;
                    bool member_result = try_resolve_type_spec(resolver,
                                                               type_spec->dot.member_type_spec,
                                                               &type, module_scope);
                    if (member_result)
                    {
                        *type_dest = type;
                        return true;
                    }
                }

                return false;
                break;
            }

            case AST_TYPE_SPEC_POINTER:
            {
                AST_Type* base_type = nullptr;
                bool base_result = try_resolve_type_spec(resolver, type_spec->pointer.base,
                                                         &base_type, scope);
                if (base_result)
                {
                    AST_Type* pointer_type = ast_find_or_create_pointer_type(resolver->context,
                                                                             base_type);
                    assert(pointer_type);
                    *type_dest = pointer_type;
                    return true;
                }
                else
                {
                    return false;
                }
                break;
            }

            case AST_TYPE_SPEC_STATIC_ARRAY:
            {
                AST_Type* base_type = nullptr;
                bool base_result = try_resolve_type_spec(resolver, type_spec->static_array.base,
                                                         &base_type, scope);
                bool count_result = try_resolve_expression(resolver,
                                                           type_spec->static_array.count_expr,
                                                           scope);
                if (!count_result)
                {
                    return false;
                }

                if (base_result && count_result)
                {
                    AST_Type* array_type =
                        ast_find_or_create_array_type(resolver->context, base_type,
                                                      type_spec->static_array.count_expr,
                                                      scope);
                    *type_dest = array_type;
                    return true;
                }
                else
                {
                    return false;
                }
                break;
            }

			case AST_TYPE_SPEC_FUNCTION:
			{
				bool result = true;
				BUF(AST_Type*) arg_types = nullptr;
                AST_Scope* arg_scope = type_spec->function.arg_scope;
                assert(arg_scope);
                assert(!arg_scope->is_module_scope);
				for (uint64_t i = 0; i < BUF_LENGTH(type_spec->function.args); i++)
				{
					AST_Declaration* arg_decl = type_spec->function.args[i];
					bool arg_result = try_resolve_declaration(resolver, arg_decl, arg_scope);
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
					result &= try_resolve_type_spec(resolver, return_type_spec, &return_type,
                                                    scope);
				}
                else
                {
                    return_type = Builtin::type_void;
                }

				if (result)
				{
					AST_Type* result_type =
						ast_find_or_create_function_type(resolver->context,
                                                         type_spec->function.is_vararg,
						                                 arg_types, return_type);
					*type_dest = result_type;
					return true;
				}
				return false;
			}

            default: assert(false);
        }

        assert(false);
        return false;
    }

    AST_Module* resolver_add_import_to_module(Resolver* resolver, AST_Module* module,
                                              const Atom& module_path,
                                              const Atom& module_name)
    {
        assert(resolver);
        assert(module);

        assert(file_exists(module_path.data));

        AST_Module* import_module = zodiac_compile_or_get_module(resolver->context, module_path,
                                                                 module_name);
        if (!import_module)
        {
            resolver->import_error = true;
            return nullptr;
        }

        bool found = false;

        for (uint64_t i = 0; i < BUF_LENGTH(module->import_modules); i++)
        {
            if (module->import_modules[i] == import_module)
            {
                found = true;
                break;;
            }
        }

        if (!found)
        {
            BUF_PUSH(module->import_modules, import_module);
        }

        return import_module;
    }

    static bool is_valid_integer_conversion(Resolver* resolver, AST_Type* dest_type, AST_Type* source_type)
    {
        assert(resolver);
        assert(dest_type);
        assert(source_type);

        assert(dest_type->flags & AST_TYPE_FLAG_INT);
        assert(source_type->flags & AST_TYPE_FLAG_INT);

        if (dest_type == source_type)
        {
            return true;
        }

        if ((dest_type->flags & AST_TYPE_FLAG_SIGNED) ==
            (source_type->flags & AST_TYPE_FLAG_SIGNED))
        {
            return dest_type->bit_size >= source_type->bit_size;
        }

        return false;
    }

    AST_Type* create_struct_type(Resolver* resolver, AST_Identifier* identifier,
        BUF(AST_Declaration*) member_decls)
    {
        assert(resolver);
        assert(identifier);
        // assert(member_decls);

        uint64_t bit_size = 0;

        for (uint64_t i = 0; i < BUF_LENGTH(member_decls); i++)
        {
            AST_Declaration* decl = member_decls[i];
            assert(decl->kind == AST_DECL_MUTABLE);
            assert(decl->location == AST_DECL_LOC_AGGREGATE_MEMBER);

            if (decl->mutable_decl.type)
            {
                AST_Type* member_type = decl->mutable_decl.type;
                assert(member_type->bit_size);
                bit_size += member_type->bit_size;
            }
            else if (decl->mutable_decl.type_spec->kind == AST_TYPE_SPEC_POINTER)
            {
                bit_size += Builtin::pointer_size;
            }
            else assert(false);
        }

        AST_Type* struct_type = ast_type_struct_new(resolver->context, member_decls,
                                                    identifier->atom.data,
                                                    bit_size);

        assert(struct_type->bit_size || BUF_LENGTH(member_decls) == 0);
        return struct_type;
    }

    AST_Type* create_enum_type(Resolver* resolver, AST_Identifier* identifier,
                               BUF(AST_Declaration*) member_decls)
    {
        assert(resolver);
        assert(identifier);
        assert(member_decls);

        AST_Type* enum_type = ast_type_enum_new(resolver->context, member_decls,
                                                Builtin::type_int);

        assert(enum_type->bit_size);
        return enum_type;
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
            if (!scope->is_module_scope &&
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
                auto agg_decls = ud->aggregate_type.aggregate_declarations;
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

    bool is_valid_integer_promotion(AST_Type* source_type, AST_Type* target_type)
    {
        assert(source_type);
        assert(target_type);

        if ((source_type->flags & AST_TYPE_FLAG_INT) &&
            target_type->flags & AST_TYPE_FLAG_INT)
        {
            if (target_type->flags & AST_TYPE_FLAG_SIGNED)
            {
                if (source_type->flags & AST_TYPE_FLAG_SIGNED)
                {
                    return target_type->bit_size >= source_type->bit_size;
                }
                else
                {
                    return target_type->bit_size > source_type->bit_size;
                }
            }
            else
            {
                if (source_type->flags & AST_TYPE_FLAG_SIGNED)
                {
                    return false;
                }
                else
                {
                    return target_type->bit_size >= source_type->bit_size;
                }
            }
        }

        return false;
    }

    bool function_signatures_match(Resolver* resolver, AST_Declaration* decl_a,
                                   AST_Declaration* decl_b,
                                   AST_Scope* scope)
    {
        assert(decl_a);
        assert(decl_a->kind == AST_DECL_FUNC);
        assert(decl_b);
        assert(decl_b->kind == AST_DECL_FUNC);
        assert(scope);

        bool arg_count_match = BUF_LENGTH(decl_a->function.args) ==
            BUF_LENGTH(decl_b->function.args);

        if (!arg_count_match) return false;
        if (decl_a->function.is_vararg != decl_b->function.is_vararg) return false;

        for (uint64_t i = 0; i < BUF_LENGTH(decl_a->function.args); i++)
        {
            AST_Declaration* arg_a = decl_a->function.args[i];
            AST_Declaration* arg_b = decl_b->function.args[i];

            AST_Type* arg_a_type = nullptr;
            AST_Type* arg_b_type = nullptr;

            bool a_type_result = try_resolve_type_spec(resolver, arg_a->mutable_decl.type_spec,
                                                       &arg_a_type, scope);
            bool b_type_result = try_resolve_type_spec(resolver, arg_b->mutable_decl.type_spec,
                                                       &arg_b_type, scope);

            if (a_type_result && b_type_result)
            {
                assert(arg_a_type && arg_b_type);
                if (arg_a_type != arg_b_type)
                {
                    return false;
                }
            }
        }

        return true;
    }

    AST_Declaration* find_overload_signature_match(Resolver* resolver,
                                                   AST_Declaration* overload_decl,
                                                   AST_Expression* call_expr, AST_Scope* scope)
    {
        assert(resolver);
        assert(overload_decl);
        assert(overload_decl->kind == AST_DECL_FUNC);
        assert(call_expr);
        assert(call_expr->kind == AST_EXPR_CALL);
        assert(scope);

        for (uint64_t i = 0; i < BUF_LENGTH(overload_decl->function.overloads); i++)
        {
            AST_Declaration* overload = overload_decl->function.overloads[i];

            if (BUF_LENGTH(overload->function.args) != BUF_LENGTH(call_expr->call.arg_expressions))
                continue;

            bool match = true;
            for (uint64_t ai = 0; ai < BUF_LENGTH(overload->function.args); ai++)
            {
                AST_Declaration* overload_arg = overload->function.args[ai];
                AST_Expression* call_arg_expr = call_expr->call.arg_expressions[ai];

                AST_Type* overload_arg_type = nullptr;
                resolver->silent = true;
                if (try_resolve_type_spec(resolver, overload_arg->mutable_decl.type_spec,
                                          &overload_arg_type, overload->function.argument_scope))
                {
                    assert(overload_arg_type);
                    if (try_resolve_expression(resolver, call_arg_expr, scope, overload_arg_type))
                    {
                        if (overload_arg_type != call_arg_expr->type)
                        {
                            match = false;
                        }
                    }
                    else {

                        match = false;
                    }
                } else match = false;
                resolver->silent = false;
            }

            if (match)
            {
                return overload;
            }

        }

        return overload_decl;
    }

    void add_overload(Resolver* resolver, AST_Declaration* container, AST_Declaration* overload)
    {
        assert(resolver);
        assert(container);
        assert(container->kind == AST_DECL_FUNC);
        assert(overload);
        assert(overload->kind == AST_DECL_FUNC);
        assert(container->identifier->atom == overload->identifier->atom);

        overload->identifier->atom = atom_append(resolver->context->atom_table,
                                                 overload->identifier->atom,
                                                 BUF_LENGTH(container->function.overloads));

        BUF_PUSH(container->function.overloads, overload);
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
            fprintf(stderr, "Exiting with error(s)\n");
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

            {
                AST_Declaration* insert_decl = call_expression->call.callee_declaration;
                assert(insert_decl);

                IR_Value* func_value = ir_builder_value_for_declaration(&ir_builder, insert_decl);
                assert(func_value);
                assert(func_value->function);

                ir_runner_load_dynamic_libs(&ir_runner, resolver->module, &ir_module);
                ir_runner_load_foreigns(&ir_runner, &ir_module);

                ir_runner_execute_block(&ir_runner, ir_runner.context->global_init_block->block);

                auto num_args = BUF_LENGTH(call_expression->call.arg_expressions);
                auto u8_ptr_type = ast_find_or_create_pointer_type(resolver->context,
                                                                   Builtin::type_u8);

                for (uint64_t i = 0; i < num_args; i++)
                {
                    AST_Expression* arg_expr = call_expression->call.arg_expressions[i];

                    IR_Value* arg_value = nullptr;
                    if (arg_expr->kind == AST_EXPR_STRING_LITERAL)
                    {
                        arg_value = ir_string_literal(&ir_builder, u8_ptr_type,
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
                                                                            func_value->function, num_args,
                                                                            &return_value);

                resolver->module->gen_data = nullptr;

                return (char*)return_value.value.string;
            }
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

    static void report_undeclared_identifier(Resolver* resolver, File_Pos file_pos,
                                             AST_Identifier* identifier)
    {
        assert(resolver);
        assert(identifier);

        resolver->undeclared_decl_count++;

        auto error = resolver_report_error(resolver, file_pos, RE_FLAG_UNDECLARED,
                                           "Reference to undeclared identifier: %s",
                                           identifier->atom.data);

        error->identifier = identifier->atom;
    }

    static void report_undeclared_identifier(Resolver* resolver, File_Pos file_pos,
                                             AST_Module* module,
                                             AST_Identifier* identifier)
    {
        assert(resolver);
        assert(module);
        assert(identifier);

        resolver->undeclared_decl_count++;

        auto error = resolver_report_error(resolver, file_pos, RE_FLAG_UNDECLARED,
                                           "Reference to undeclared identifier '%s' in module '%s'",
                                           identifier->atom.data, module->module_name);

        error->identifier = identifier->atom;
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

        if (!resolver->silent)
        {
            const size_t print_buf_size = 2048;
            static char print_buf[print_buf_size];

            vsprintf(print_buf, format, args);

            auto message_length = strlen(print_buf);
            char* message = (char*)mem_alloc(message_length + 1);
            assert(message);
            memcpy(message, print_buf, message_length);
            message[message_length] = '\0';

            Resolve_Error error = { flags, message, file_pos };
            if (resolver->resolving_auto_gen)
            {
                error.auto_gen = true;
                error.auto_gen_file_pos = resolver->auto_gen_file_pos;
            }
            BUF_PUSH(resolver->errors, error);

            Resolve_Error* result = &resolver->errors[BUF_LENGTH(resolver->errors) - 1];
            return result;
        }
        else
        {
            return nullptr;
        }
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

    void resolver_report_errors(Resolver* resolver)
    {
        assert(resolver);

        for (uint64_t i = 0; i < BUF_LENGTH(resolver->errors); i++)
        {
            auto error = resolver->errors[i];

            bool report = true;
            if (error.flags & RE_FLAG_UNDECLARED)
            {
                for (uint64_t i = 0; i < BUF_LENGTH(resolver->unresolved_decls); i++)
                {
                    AST_Declaration* unres_decl = resolver->unresolved_decls[i];
                    if (unres_decl->identifier &&
                        unres_decl->identifier->atom == error.identifier)
                    {
                        report = false;
                        break;
                    }
                }
            }

            if (report)
            {
                if (error.auto_gen)
                {
                    fprintf(stderr, "Error:%s:%" PRIu64 ":%" PRIu64 ": Error generated from insert:\n\t",
                            error.auto_gen_file_pos.file_name, error.auto_gen_file_pos.line,
                            error.auto_gen_file_pos.line_relative_char_pos);
                }
                fprintf(stderr, "Error:%s:%" PRIu64 ":%" PRIu64 ": %s\n",
                        error.file_pos.file_name,
                        error.file_pos.line, error.file_pos.line_relative_char_pos,
                        error.message);
            }
        }
    }
}
