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

        assert(module->module_scope);
        AST_Scope* module_scope = module->module_scope;
        assert(module_scope->declarations == nullptr);

        for (uint64_t i = 0; i < BUF_LENGTH(context->builtin_decls); i++)
        {
            AST_Declaration* builtin_decl = context->builtin_decls[i];
            BUF_PUSH(module_scope->declarations, builtin_decl);
        }
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
                    break;;
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

        if (declaration->mutable_decl.init_expression)
        {
            result &= try_resolve_expression(resolver, declaration->mutable_decl.init_expression, scope);

            if (result && !declaration->mutable_decl.type_spec)
            {
                declaration->mutable_decl.type = declaration->mutable_decl.init_expression->type;
            }
        }

        if (result)
        {
            assert(declaration->location != AST_DECL_LOC_INVALID);
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

        return scope;
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
                result &= try_resolve_expression(resolver, statement->assign.expression,
                                                 scope);
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

    static bool try_resolve_expression(Resolver* resolver, AST_Expression* expression, AST_Scope* scope)
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

            case AST_EXPR_CHAR_LITERAL:
            {
                result &= try_resolve_character_literal_expression(resolver, expression);
                break;
            }

            case AST_EXPR_COMPOUND_LITERAL:
            {
                result &= try_resolve_compound_literal_expression(resolver, expression, scope);
                break;
            }

            case AST_EXPR_ARRAY_LENGTH:
            {
                result &= try_resolve_array_length_expression(resolver, expression, scope);
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

        AST_Declaration* func_decl = find_declaration(scope, expression->call.identifier);
        if (!func_decl &&
            (expression->call.identifier->atom == resolver->current_func_decl->identifier->atom))
        {
            func_decl = resolver->current_func_decl;
        }
        else
        {
            if (!func_decl)
            {
                report_undeclared_identifier(resolver, expression->call.identifier->file_pos,
                                             expression->call.identifier);
                result = false;
            }
            else if (func_decl->flags & AST_DECL_FLAG_RESOLVED)
            {
                assert(func_decl->kind == AST_DECL_FUNC);
            }
            else
            {
                result = false;
            }
        }

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
            resolver_report_error(resolver,expression->file_pos,
                                  "Circular dependency when trying to infer return type of function '%s'",
                                  expression->call.identifier->atom.data);
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

    static bool try_resolve_compound_literal_expression(Resolver* resolver, AST_Expression* expression, AST_Scope* scope)
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

        expression->type = ast_find_or_create_array_type(resolver->context, resolver->module, first_type,
                                                         BUF_LENGTH(expression->compound_literal.expressions));

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
                           operand_expr->kind == AST_EXPR_SUBSCRIPT);
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

                assert(type_decl->kind == AST_DECL_TYPE);
                AST_Type* type = type_decl->type.type;
                if (type)
                {
                    *type_dest = type;
                    return true;
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
                    AST_Type* pointer_type = ast_find_or_create_pointer_type(resolver->context, resolver->module, base_type);
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
