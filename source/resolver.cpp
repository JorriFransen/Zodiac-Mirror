#include "resolver.h"

#include "builtin.h"
#include "types.h"

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

        resolver->errors = nullptr;

        resolver->current_func_decl = nullptr;
    }

    void resolver_do_cycle(Resolver* resolver)
    {
        assert(resolver);
        assert(resolver->progressed_on_last_cycle);

        resolver->unresolved_decl_count = 0;
        resolver->undeclared_decl_count = 0;

        if (resolver->errors)
        {
            auto err_hdr = _BUF_HDR(resolver->errors);
            err_hdr->length = 0;
        }

        for (uint64_t i = 0; i < BUF_LENGTH(resolver->module->global_declarations); i++)
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
        }
    }

    static bool try_resolve_declaration(Resolver* resolver, AST_Declaration* declaration,
                                        AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(scope);

        bool result = true;

        if (!(declaration->flags & AST_DECL_FLAG_RESOLVED))
        {
            switch (declaration->kind)
            {
                case AST_DECL_FUNC:
                {
                    result &= try_resolve_function_declaration(resolver, declaration, scope);
                    break;
                }

                case AST_DECL_MUTABLE:
                {
                    result &= try_resolve_mutable_declaration(resolver, declaration, scope);
                    break;
                }

                case AST_DECL_CONSTANT_VAR:
                {
                    result &= try_resolve_constant_var_declaration(resolver, declaration, scope);
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
                    result &= try_resolve_static_assert_declaration(resolver, declaration, scope);
                    break;
                }

                case AST_DECL_IMPORT:
                {
                    result &= try_resolve_import_declaration(resolver, declaration, scope);
                    break;
                }

                case AST_DECL_AGGREGATE_TYPE:
                {
                    result &= try_resolve_aggregate_type_declaration(resolver, declaration, scope);
                    break;
                }

                default:
                    assert(false);
                    break;
            }

            if (!result)
            {
                resolver->unresolved_decl_count++;
            }
            else
            {
                declaration->flags |= AST_DECL_FLAG_RESOLVED;
                BUF_PUSH(scope->declarations, declaration);

                if (resolver->current_func_decl &&
                    declaration->location == AST_DECL_LOC_LOCAL)
                {
                    BUF_PUSH(resolver->current_func_decl->function.locals,
                             declaration);
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
        assert(arg_scope->parent == scope);

        for (uint64_t i = 0; i < BUF_LENGTH(declaration->function.args); i++)
        {
            AST_Declaration* arg_decl = declaration->function.args[i];
            result &= try_resolve_declaration(resolver, arg_decl, arg_scope);
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
                                            declaration->function.body_block->block.scope);

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
            auto main_atom = atom_get(resolver->context->atom_table, "main");
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
        }

        AST_Expression* init_expr = declaration->mutable_decl.init_expression;

        if (init_expr)
        {
            AST_Type* suggested_type = nullptr;
            if (declaration->mutable_decl.type &&
                declaration->mutable_decl.type->kind == AST_TYPE_STRUCT)
            {
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
                assert(declaration->mutable_decl.type == init_expr->type);
            }
        }
        return result;
    }

    static bool try_resolve_constant_var_declaration(Resolver* resolver, AST_Declaration* declaration,
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
            result &= try_resolve_type_spec(resolver, declaration->constant_var.type_spec, &specified_type, scope);
            if (!result)
            {
                return false;
            }
        }

        result &= try_resolve_expression(resolver, declaration->constant_var.init_expression, scope);

        if (!result)
        {
            return false;
        }

        assert(declaration->constant_var.init_expression->is_const);

        if (specified_type)
        {
            assert(specified_type == declaration->constant_var.init_expression->type);
        }

        declaration->constant_var.type = declaration->constant_var.init_expression->type;

        return result;
    }

    static bool try_resolve_static_if_declaration(Resolver* resolver, AST_Declaration* declaration, AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_STATIC_IF);
        assert(scope);

        bool result = try_resolve_expression(resolver, declaration->static_if.cond_expr, scope);
        result &= try_resolve_declaration(resolver, declaration->static_if.then_declaration, scope);

        if (declaration->static_if.else_declaration)
        {
            result &= try_resolve_declaration(resolver, declaration->static_if.else_declaration, scope);
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

    static bool try_resolve_static_assert_declaration(Resolver* resolver, AST_Declaration* declaration,
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
        AST_Declaration* declaration, AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_AGGREGATE_TYPE);
        assert(scope);

        bool result = true;

        assert(declaration->identifier);

        assert(!find_declaration(scope, declaration->identifier));

        for (uint64_t i = 0; i < BUF_LENGTH(declaration->aggregate_type.aggregate_declarations); i++)
        {
            AST_Declaration* member_decl = declaration->aggregate_type.aggregate_declarations[i];
            assert(member_decl->kind == AST_DECL_MUTABLE);
            assert(member_decl->location == AST_DECL_LOC_AGGREGATE_MEMBER);
            result &= try_resolve_declaration(resolver, member_decl, scope);
        }

        if (result && !declaration->aggregate_type.type)
        {
            declaration->aggregate_type.type = create_struct_type(resolver, declaration->identifier,
                declaration->aggregate_type.aggregate_declarations);
        }

        if (result)
        {
            assert(declaration->aggregate_type.type);
        }

        return result;
    }

    static bool try_resolve_import_declaration(Resolver* resolver, AST_Declaration* declaration, AST_Scope* scope)
    {
        assert(resolver);
        assert(declaration);
        assert(declaration->kind == AST_DECL_IMPORT);
        assert(scope);

        auto at = resolver->context->atom_table;

        Atom module_name = declaration->import.module_identifier->atom;
        Atom module_file_name = atom_append(at, module_name, ".zdc");
        Atom module_search_path = resolver->context->module_search_path;

        Atom module_path = atom_append(at, module_search_path, module_file_name);

        if (!file_exists(module_path.data))
        {
            resolver_report_error(resolver, declaration->file_pos, "Failed to find module: %s", module_name.data);
            resolver_report_error(resolver, declaration->file_pos, "\tExpected path: %s", module_path.data);
            return false;
        }

        AST_Module* import_module = resolver_add_import_to_module(resolver, resolver->module, module_path, module_name);
        assert(import_module);

        declaration->import.module = import_module;

        return true;
    }

    static bool try_resolve_statement(Resolver* resolver, AST_Statement* statement,
                                      AST_Scope* scope)
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
                    result &= try_resolve_statement(resolver, stmt, statement->block.scope);
                }
                break;
            }

            case AST_STMT_IF:
            {
                result &= try_resolve_if_statement(resolver, statement, scope);
                break;
            }

            case AST_STMT_ASSIGN:
            {
                result &= try_resolve_expression(resolver, statement->assign.lvalue_expression,
                                                 scope);
                AST_Type* suggested_type = nullptr;
                if (result &&
                    statement->assign.lvalue_expression->type->kind == AST_TYPE_STRUCT &&
                    statement->assign.expression->kind == AST_EXPR_COMPOUND_LITERAL)
                {
                    suggested_type = statement->assign.lvalue_expression->type;
                }
                result &= try_resolve_expression(resolver, statement->assign.expression,
                                                 scope, suggested_type);
                if (result && statement->assign.lvalue_expression->kind == AST_EXPR_IDENTIFIER)
                {
                    assert(statement->assign.lvalue_expression->identifier->declaration->mutable_decl.type ==
                           statement->assign.expression->type);
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
                result &= try_resolve_statement(resolver, statement->while_stmt.body_stmt, scope);
                break;
            }

            case AST_STMT_FOR:
            {
                AST_Scope* for_scope = statement->for_stmt.scope;
                result &= try_resolve_statement(resolver, statement->for_stmt.init_stmt, for_scope);
                result &= try_resolve_expression(resolver, statement->for_stmt.cond_expr, for_scope);
                result &= try_resolve_statement(resolver, statement->for_stmt.step_stmt, for_scope);
                result &= try_resolve_statement(resolver, statement->for_stmt.body_stmt, for_scope);
                break;
            }

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
                                         AST_Scope* scope)
    {
        assert(resolver);
        assert(statement);
        assert(statement->kind == AST_STMT_IF);
        assert(scope);

        bool result = true;

        result &= try_resolve_expression(resolver, statement->if_stmt.if_expression, scope);
        result &= try_resolve_statement(resolver, statement->if_stmt.then_statement, scope);

        if (statement->if_stmt.else_statement)
        {
            result &= try_resolve_statement(resolver, statement->if_stmt.else_statement, scope);
        }

        return result;
    }

    static bool try_resolve_expression(Resolver* resolver, AST_Expression* expression, AST_Scope* scope,
                                       AST_Type* suggested_type/*=nullptr*/)
    {
        assert(resolver);
        assert(expression);
        assert(scope);

        bool result = true;

        if (suggested_type)
        {
            assert(expression->kind == AST_EXPR_COMPOUND_LITERAL);
        }

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
                result &= try_resolve_expression(resolver, expression->subscript.index_expression, scope);
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
                    else assert(false);
                }
                break;
            }

            case AST_EXPR_BOOL_LITERAL:
            {
                result &= try_resolve_boolean_literal_expression(resolver, expression);
                if (result)
                break;
            }

            case AST_EXPR_STRING_LITERAL:
            {
                result &= try_resolve_string_literal_expression(resolver, expression);
                break;
            }

            case AST_EXPR_INTEGER_LITERAL:
            {
                result &= try_resolve_integer_literal_expression(resolver, expression);
                break;
            }

            case AST_EXPR_FLOAT_LITERAL:
            {
                result &= try_resolve_float_literal_expression(resolver, expression);
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

            default:
            {
                assert(false);
                result = false;
                break;
            }
        }

        if (result)
        {
            assert(expression->type);
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
        if (ident_expr->kind == AST_EXPR_IDENTIFIER)
        {
            func_decl = find_declaration(scope, ident_expr->identifier);
            if (!func_decl &&
                (ident_expr->identifier->atom == resolver->current_func_decl->identifier->atom))
            {
                func_decl = resolver->current_func_decl;
            }
            else if (!func_decl)
            {
                report_undeclared_identifier(resolver, ident_expr->identifier->file_pos,
                                             ident_expr->identifier);
                return false;
            }
            else
            {
                if (!(func_decl->flags & AST_DECL_FLAG_RESOLVED))
                {
                    result = false;
                }
            }
        }
        else
        {
            assert(ident_expr->kind == AST_EXPR_DOT);
            AST_Expression* base_expression = ident_expr->dot.base_expression;
            AST_Expression* member_expression = ident_expr->dot.member_expression;
            assert(base_expression->kind == AST_EXPR_IDENTIFIER);
            assert(member_expression->kind == AST_EXPR_IDENTIFIER);

            AST_Declaration* import_decl = find_declaration(scope, base_expression->identifier);
            if (!import_decl)
            {
                return false;
            }
            assert(import_decl->kind == AST_DECL_IMPORT);
            assert(import_decl->import.module);

            AST_Module* import_module = import_decl->import.module;
            func_decl = find_declaration(import_module->module_scope, member_expression->identifier);
            if (!func_decl)
            {
                report_undeclared_identifier(resolver, ident_expr->file_pos, import_module,
                                             member_expression->identifier);
                return false;
            }

            if (!(func_decl->flags & AST_DECL_FLAG_RESOLVED))
            {
                result = false;
            }

            assert(func_decl->kind == AST_DECL_FUNC);
        }

        assert(func_decl);

        for (uint64_t i = 0; i < BUF_LENGTH(expression->call.arg_expressions); i++)
        {
            AST_Expression* arg_expr = expression->call.arg_expressions[i];
            result &= try_resolve_expression(resolver, arg_expr, scope);
        }

        if (result && !expression->type && func_decl->function.return_type)
        {
            expression->type = func_decl->function.return_type;
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

    static bool try_resolve_string_literal_expression(Resolver* resolver, AST_Expression* expression)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_STRING_LITERAL);

        if (!expression->type)
        {
            expression->type = ast_find_or_create_pointer_type(resolver->context, resolver->module, Builtin::type_u8);
        }
        expression->is_const = true;

        return true;
    }

    static bool try_resolve_integer_literal_expression(Resolver* resolver, AST_Expression* expression)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_INTEGER_LITERAL);

        if (!expression->type)
        {
            expression->type = Builtin::type_int;
        }
        expression->is_const = true;

        return true;
    }

    static bool try_resolve_float_literal_expression(Resolver* resolver,
                                                     AST_Expression* expression)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_FLOAT_LITERAL);

        if (!expression->type)
        {
            expression->type = Builtin::type_float;
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
            AST_Expression* expr = expression->compound_literal.expressions[i];
            result &= try_resolve_expression(resolver, expr, scope);

            if (result)
            {
                if (!first_type)
                {
                    first_type = expr->type;
                }

                same_type = first_type == expr->type;

                assert(same_type);
            }
        }

        assert(same_type);

        if (suggested_type && suggested_type->kind == AST_TYPE_STRUCT)
        {
            if (BUF_LENGTH(suggested_type->aggregate_type.member_declarations) !=
                BUF_LENGTH(expression->compound_literal.expressions))
            {
                return result = false;
            }
            else
            {
                bool match = true;
                for (uint64_t i = 0; i < BUF_LENGTH(expression->compound_literal.expressions); i++)
                {
                    AST_Expression* expr = expression->compound_literal.expressions[i];
                    assert(expr->type);
                    AST_Declaration* struct_member_decl = suggested_type->aggregate_type.member_declarations[i];

                    if (expr->type != struct_member_decl->mutable_decl.type)
                    {
                        match = false;
                        break;
                    }
                }

                if (match)
                {
                    expression->type = suggested_type;
                }
                else
                {
                    result = false;
                }
            }
        }
        else
        {
            expression->type = ast_find_or_create_array_type(resolver->context, resolver->module, first_type,
                                                             BUF_LENGTH(expression->compound_literal.expressions));
        }

        return result;
    }

    static bool try_resolve_array_length_expression(Resolver* resolver, AST_Expression* expression, AST_Scope* scope)
    {
        assert(resolver);
        assert(expression);
        assert(scope);

        bool result = true;

        AST_Expression* ident_expr = expression->array_length.ident_expr;
        result &= try_resolve_expression(resolver, ident_expr, scope);
        if (result)
        {
            AST_Declaration* array_decl = ident_expr->identifier->declaration;
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
            assert(ident->declaration->mutable_decl.type);
            expression->type = ident->declaration->mutable_decl.type;
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

        result &= try_resolve_expression(resolver, expression->binary.lhs, scope);
        result &= try_resolve_expression(resolver, expression->binary.rhs, scope);

        if (result && !expression->type)
        {
            assert(expression->binary.lhs->type ==
                   expression->binary.rhs->type);

            expression->type = expression->binary.lhs->type;
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
                    assert(false);
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
                        assert(operand_expr->dot.base_expression->type->kind == AST_TYPE_STRUCT);
                        assert(operand_expr->dot.member_expression->kind == AST_EXPR_IDENTIFIER);
                    }
                    expression->type = ast_find_or_create_pointer_type(resolver->context,
                                                                       resolver->module,
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

        AST_Expression*  base_expression = expression->dot.base_expression;
        AST_Expression* member_expression = expression->dot.member_expression;
        assert(base_expression->kind == AST_EXPR_IDENTIFIER);
        assert(member_expression->kind == AST_EXPR_IDENTIFIER);

        result &= try_resolve_expression(resolver, base_expression, scope);

        AST_Type* struct_type = nullptr;

        if (result)
        {
            if (base_expression->type->kind == AST_TYPE_STRUCT)
            {
                struct_type = base_expression->type;
            }
            else if (base_expression->type->kind == AST_TYPE_POINTER)
            {
                assert(base_expression->type->pointer.base->kind == AST_TYPE_STRUCT);
                struct_type = base_expression->type->pointer.base;
            }
            else assert(false);
        }

        AST_Type* type = nullptr;

        bool found = false;
        for (uint64_t i = 0; i < BUF_LENGTH(struct_type->aggregate_type.member_declarations); i++)
        {
            AST_Declaration* member_decl = struct_type->aggregate_type.member_declarations[i];
            if (member_decl->identifier->atom == member_expression->identifier->atom)
            {
                found = true;
                assert(member_decl->kind == AST_DECL_MUTABLE);
                assert(member_decl->location == AST_DECL_LOC_AGGREGATE_MEMBER);
                assert(member_decl->aggregate_type.type);

                type = member_decl->aggregate_type.type;
            }
        }

        if (!found)
        {
            result = false;
        }
        else if (result && !expression->type)
        {
            expression->type = type;
        }

        return result;
    }

    static bool try_resolve_identifier(Resolver* resolver, AST_Identifier* identifier, AST_Scope* scope)
    {
        assert(resolver);
        assert(identifier);
        assert(scope);

        AST_Declaration* decl = find_declaration(scope, identifier);
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
        else assert(false);

        identifier->declaration = decl;

        return true;
    }

    static bool try_resolve_type_spec(Resolver* resolver, AST_Type_Spec* type_spec, AST_Type** type_dest,
                                      AST_Scope* scope)
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
                assert(type_spec->identifier);
                AST_Identifier* identifier = type_spec->identifier;

                AST_Declaration* type_decl = find_declaration(scope, type_spec->identifier);
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

                    default: assert(false);
                }

                return false;
            }

            case AST_TYPE_SPEC_POINTER:
            {
                AST_Type* base_type = nullptr;
                bool base_result = try_resolve_type_spec(resolver, type_spec->pointer.base, &base_type,
                                                         scope);
                if (base_result)
                {
                    AST_Type* pointer_type = ast_find_or_create_pointer_type(resolver->context, resolver->module,
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
                bool base_result = try_resolve_type_spec(resolver, type_spec->static_array.base, &base_type, scope);
                bool count_result = try_resolve_expression(resolver, type_spec->static_array.count_expr, scope);
                if (base_result && count_result)
                {
                    AST_Type* array_type = ast_find_or_create_array_type(resolver->context, resolver->module, base_type,
                                                                         type_spec->static_array.count_expr);
                    *type_dest = array_type;
                    return true;
                }
                else
                {
                    return false;
                }
                break;
            }

            default: assert(false);
        }

        assert(false);
        return false;
    }

    AST_Module* resolver_add_import_to_module(Resolver* resolver, AST_Module* module, const Atom& module_path,
                                              const Atom& module_name)
    {
        assert(resolver);
        assert(module);

        assert(file_exists(module_path.data));

        AST_Module* import_module = zodiac_compile_or_get_module(resolver->context, module_path, module_name);
        assert(import_module);

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

    AST_Type* create_struct_type(Resolver* resolver, AST_Identifier* identifier,
        BUF(AST_Declaration*) member_decls)
    {
        assert(resolver);
        assert(identifier);
        assert(member_decls);

        uint64_t bit_size = 0;

        for (uint64_t i = 0; i < BUF_LENGTH(member_decls); i++)
        {
            AST_Declaration* decl = member_decls[i];
            assert(decl->kind == AST_DECL_MUTABLE);
            assert(decl->location == AST_DECL_LOC_AGGREGATE_MEMBER);
            assert(decl->mutable_decl.type);

            AST_Type* member_type = decl->mutable_decl.type;
            assert(member_type->bit_size);
            bit_size += member_type->bit_size;
        }

        AST_Type* struct_type = ast_type_struct_new(resolver->context, member_decls, bit_size);

        assert(struct_type->bit_size);
        return struct_type;
    }

    AST_Declaration* find_declaration(AST_Scope* scope, AST_Identifier* identifier)
    {
        assert(scope);
        assert(identifier);

        for (uint64_t i = 0; i < BUF_LENGTH(scope->declarations); i++)
        {
            AST_Declaration* decl = scope->declarations[i];
            if (decl->identifier &&
                decl->identifier->atom == identifier->atom)
            {
                return decl;
            }
        }

        if (scope->parent)
        {
            return find_declaration(scope->parent, identifier);
        }

        return nullptr;
    }

    static void report_undeclared_identifier(Resolver* resolver, File_Pos file_pos, AST_Identifier* identifier)
    {
        assert(resolver);
        assert(identifier);

        resolver->undeclared_decl_count++;

        resolver_report_error(resolver, file_pos, "Reference to undeclared identifier: %s",
                              identifier->atom.data);
    }

    static void report_undeclared_identifier(Resolver* resolver, File_Pos file_pos, AST_Module* module,
                                             AST_Identifier* identifier)
    {
        assert(resolver);
        assert(module);
        assert(identifier);

        resolver->undeclared_decl_count++;

        resolver_report_error(resolver, file_pos, "Reference to undeclared identifier '%s' in module '%s'",
                              module->module_name, identifier->atom.data);
    }

    static void resolver_report_error(Resolver* resolver, File_Pos file_pos, const char* format, ...)
    {
        assert(resolver);
        assert(format);

        const size_t print_buf_size = 2048;
        static char print_buf[print_buf_size];

        va_list args;
        va_start(args, format);
        vsprintf(print_buf, format, args);
        va_end(args);

        auto message_length = strlen(print_buf);
        char* message = (char*)mem_alloc(message_length + 1);
        memcpy(message, print_buf, message_length);
        message[message_length] = '\0';

        Resolve_Error error = { message, file_pos };
        BUF_PUSH(resolver->errors, error);
    }

    void resolver_report_errors(Resolver* resolver)
    {
        assert(resolver);

        for (uint64_t i = 0; i < BUF_LENGTH(resolver->errors); i++)
        {
            auto error = resolver->errors[i];
            fprintf(stderr, "Error:%s:%" PRIu64 ":%" PRIu64 ": %s\n", error.file_pos.file_name,
                    error.file_pos.line, error.file_pos.line_relative_char_pos,
                    error.message);
        }
    }
}
