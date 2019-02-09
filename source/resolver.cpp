#include "resolver.h"

#include "builtin.h"
#include "types.h"

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
    }

    void resolver_do_cycle(Resolver* resolver)
    {
        assert(resolver);
        assert(resolver->progressed_on_last_cycle);

        resolver->unresolved_decl_count = 0;

        for (uint64_t i = 0; i < BUF_LENGTH(resolver->module->global_declarations); i++)
        {
            AST_Declaration* global_decl = resolver->module->global_declarations[i];
            if (!(global_decl->flags & AST_DECL_FLAG_RESOLVED))
            {
                try_resolve_declaration(resolver, global_decl, resolver->module->module_scope);
            }
        }

        resolver->progressed_on_last_cycle = (resolver->unresolved_decl_count <
                                              resolver->unresolved_decl_count_last_cycle);
        resolver->unresolved_decl_count_last_cycle = resolver->unresolved_decl_count;
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

                default:
                    assert(false);
                    break;
            }
        }

        if (!result)
        {
            resolver->unresolved_decl_count++;
        }
        else
        {
            declaration->flags |= AST_DECL_FLAG_RESOLVED;
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

        bool result = true;

        for (uint64_t i = 0; i < BUF_LENGTH(declaration->function.args); i++)
        {
            AST_Declaration* arg_decl = declaration->function.args[i];
            result &= try_resolve_declaration(resolver, arg_decl, scope);
        }

        if (!declaration->function.return_type && declaration->function.return_type_spec)
        {
            result &= try_resolve_type_spec(resolver, declaration->function.return_type_spec,
                                            &declaration->function.return_type);
        }

        if (declaration->function.body_block)
        {
            result &= try_resolve_statement(resolver, declaration->function.body_block,
                                            declaration->function.body_block->block.scope);
        }

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
                                            &declaration->mutable_decl.type);
        }

        if (declaration->mutable_decl.init_expression)
        {
            result &= try_resolve_expression(resolver, declaration->mutable_decl.init_expression, scope);

            if (result && !declaration->mutable_decl.type_spec)
            {
                declaration->mutable_decl.type = declaration->mutable_decl.init_expression->type;
            }
        }

        return result;
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

        bool result = true;

        if (statement->return_expression)
        {
            result &= try_resolve_expression(resolver, statement->return_expression, scope);
        }

        return scope;
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
                assert(false);
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

            case AST_EXPR_LITERAL:
            {
                result &= try_resolve_literal_expression(resolver, expression);
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
        if (!func_decl)
        {
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

        for (uint64_t i = 0; i < BUF_LENGTH(expression->call.arg_expressions); i++)
        {
            AST_Expression* arg_expr = expression->call.arg_expressions[i];
            result &= try_resolve_expression(resolver, arg_expr, scope);
        }

        return result;
    }

    static bool try_resolve_literal_expression(Resolver* resolver, AST_Expression* expression)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_LITERAL);

        if (!expression->type)
        {
            expression->type = Builtin::type_int;
        }

        return true;
    }

    static bool try_resolve_identifier_expression(Resolver* resolver, AST_Expression* expression,
                                                  AST_Scope* scope)
    {
        assert(resolver);
        assert(expression);
        assert(expression->kind == AST_EXPR_IDENTIFIER);

        AST_Declaration* decl = find_declaration(scope, expression->identifier);
        if (!decl)
        {
            return false;
        }

        if (!(decl->flags & AST_DECL_FLAG_RESOLVED))
        {
            return false;
        }

        assert(decl->kind == AST_DECL_MUTABLE);

        if (!decl->mutable_decl.type)
        {
            return false;
        }

        expression->type = decl->mutable_decl.type;

        return true;
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

    static bool try_resolve_type_spec(Resolver* resolver, AST_Type_Spec* type_spec, AST_Type** type_dest)
    {
        assert(resolver);
        assert(type_spec);
        assert(type_dest);
        assert(*type_dest == nullptr);

        assert(type_spec->identifier);

        AST_Type* type = find_type(resolver->context, resolver->module, type_spec->identifier);
        if (type)
        {
            *type_dest = type;
            return true;
        }

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

    void resolver_report_errors(Resolver* resolver)
    {
        assert(resolver);

        assert(false);
    }
}
