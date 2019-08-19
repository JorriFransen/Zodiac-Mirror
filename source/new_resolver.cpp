#include "new_resolver.h"

#include "builtin.h"

#include <stdarg.h>
#include <inttypes.h>

namespace Zodiac_
{
    void resolver_init(Resolver* resolver, Context* context)
    {
        assert(resolver);
        assert(context);

        resolver->context = context;
        resolver->module = nullptr;
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
            AST_Declaration* global_decl = module->global_declarations[i];
            resolver_resolve_declaration(resolver, global_decl, module->module_scope);
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
            resolver_report_error(resolver, declaration->file_pos,
                                  "Circular dependency while trying to resolve declaration");
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
                }


                if (!found)
                {
                    resolver_report_error(resolver, declaration->file_pos,
                                        "Failed to find module: %s",
                                        module_name.data);
                }

                result = false;
                break;
            }

            case AST_DECL_STATIC_IF:
            {
                assert(false);
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
                    arg_result &=
                        resolver_resolve_declaration(resolver, arg_decl,
                                                     declaration->function.argument_scope);
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
                                                         &declaration->function.return_type, scope);
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

                if (!declaration->function.return_type && !declaration->function.return_type_spec)
                {
                    declaration->function.return_type = Builtin::type_void;
                }

                assert(result);
                bool is_vararg = false;
                BUF(AST_Type*) arg_types = nullptr;
                for (uint64_t i = 0; i < BUF_LENGTH(declaration->function.args); i++)
                {
                    AST_Declaration* arg_decl = declaration->function.args[i];
                    AST_Type* type = resolver_get_declaration_type(arg_decl);
                    BUF_PUSH(arg_types, type);
                }
                AST_Type* return_type = declaration->function.return_type;
                auto original_name = declaration->identifier->atom.data;
                declaration->function.type = ast_find_or_create_function_type(resolver->context,
                                                                              is_vararg,
                                                                              arg_types,
                                                                              return_type,
                                                                              original_name);
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
                        result &=
                            resolver_check_assign_types(resolver, declaration->mutable_decl.type,
                                                        init_expr->type);
                    }
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

                result &= resolver_resolve_expression(resolver,
                                                      declaration->constant_var.init_expression,
                                                      scope);
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
                    for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->members); i++)
                    {
                        AST_Declaration* member_decl = agg_decl->members[i];
                        result &= resolver_resolve_declaration(resolver, member_decl,
                                                          declaration->aggregate_type.scope);
                    }

                    uint64_t bit_size = 0;
                    const char* name = nullptr;
                    if (declaration->identifier)
                    {
                        name = declaration->identifier->atom.data;
                    }

                    if (declaration->aggregate_type.kind == AST_AGG_DECL_STRUCT)
                    {
                        declaration->aggregate_type.type = ast_type_struct_new(resolver->context,
                                                                               agg_decl->members,
                                                                               name,
                                                                               bit_size, nullptr);
                    }
                    else
                    {
                        declaration->aggregate_type.type = ast_type_union_new(resolver->context,
                                                                              agg_decl->members,
                                                                              name,
                                                                              bit_size, nullptr);
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

                    int64_t next_value = 0;
                    for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->members); i++)
                    {
                        AST_Declaration* member_decl = agg_decl->members[i];
                        assert(member_decl->kind == AST_DECL_CONSTANT_VAR);
                        if (!member_decl->constant_var.init_expression)
                        {
                            member_decl->constant_var.init_expression =
                                ast_integer_literal_expression_new(resolver->context,
                                                                   member_decl->file_pos,
                                                                   next_value);
                        }
                        else
                        {
                            assert(false);
                        }

                        AST_Expression* init_expr = member_decl->constant_var.init_expression;
                        result &= resolver_resolve_expression(resolver, init_expr,
                                                              declaration->aggregate_type.scope,
                                                              enum_type);
                        if (!result) break;
                        result &= resolver_resolve_declaration(resolver, member_decl,
                                                               declaration->aggregate_type.scope);

                        next_value++;
                    }

                    declaration->aggregate_type.type = ast_type_enum_new(resolver->context,
                                                                         agg_decl->members,
                                                                         declaration->identifier->atom.data,
                                                                         enum_type);
                }

                if (result) assert(declaration->aggregate_type.type);
                break;
            }

            default: assert(false);
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
                result &= resolver_resolve_expression(resolver, statement->assign.expression,
                                                      scope);

                if (!result) break;

                result &= resolver_check_assign_types(resolver,
                                                      statement->assign.lvalue_expression->type,
                                                      statement->assign.expression->type);
                break;
            }

            case AST_STMT_RETURN:
            {
                result &= resolver_resolve_expression(resolver, statement->return_expression, scope);
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
                result &= resolver_resolve_expression(resolver, statement->if_stmt.if_expression,
                                                      scope);
                if (!result) return false;
                assert(statement->if_stmt.if_expression->type == Builtin::type_bool);
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
                result &= resolver_resolve_expression(resolver, statement->while_stmt.cond_expr,
                                                      scope);
                if (!result) return false;

                assert(statement->while_stmt.cond_expr->type == Builtin::type_bool);

                result &= resolver_resolve_statement(resolver, statement->while_stmt.body_stmt,
                                                     scope);
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

        bool result = true;

        switch (expression->kind)
        {
            case AST_EXPR_CALL:
            {
                result &= resolver_resolve_expression(resolver, expression->call.ident_expression,
                                                      scope);
                if (!result) break;

                AST_Identifier* ident = expression->call.ident_expression->identifier;
                AST_Declaration* func_decl = ident->declaration;
                expression->call.callee_declaration = func_decl;

                auto arg_expr_count = BUF_LENGTH(expression->call.arg_expressions);
                auto arg_decl_count = BUF_LENGTH(func_decl->function.args);

                assert(BUF_LENGTH(expression->call.arg_expressions) ==
                       BUF_LENGTH(expression->call.callee_declaration->function.args));

                for (uint64_t i = 0; i < BUF_LENGTH(expression->call.arg_expressions); i++)
                {
                    AST_Expression* arg_expr = expression->call.arg_expressions[i];

                    result &= resolver_resolve_expression(resolver, arg_expr, scope);
                }

                assert(func_decl->function.return_type);
                expression->type = func_decl->function.return_type;

                break;
            }

            case AST_EXPR_IDENTIFIER:
            {
                result &= resolver_resolve_identifier(resolver, expression->identifier, scope);

                if (!result) break;

                AST_Declaration* decl = expression->identifier->declaration;
                assert(decl);
                expression->type = resolver_get_declaration_type(decl);

                break;
            }

            case AST_EXPR_INTEGER_LITERAL:
            {
                if (suggested_type)
                {
                    assert(suggested_type->flags & AST_TYPE_FLAG_INT);
                    expression->type = suggested_type;
                }
                else
                {
                    expression->type = Builtin::type_int;
                }
                break;
            }

            case AST_EXPR_BINARY:
            {
                result &= resolver_resolve_binary_expression(resolver, expression, scope);
                break;
            }

            case AST_EXPR_UNARY:
            {
                result &= resolver_resolve_expression(resolver, expression->unary.operand, scope);
                if (!result) break;

                AST_Type* operand_type = expression->unary.operand->type;

                switch (expression->unary.op)
                {
                    case AST_UNOP_MINUS:
                    {
                        expression->type = operand_type;
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
                        assert(false);
                        break;
                    }

                    default: assert(false);
                }
                break;
            }

            case AST_EXPR_CAST:
            {
                result &= resolver_resolve_type_spec(resolver, expression->cast_expr.type_spec,
                                                     &expression->type, scope);
                if (!result) return false;

                result &= resolver_resolve_expression(resolver, expression->cast_expr.expr, scope);
                break;
            }

            case AST_EXPR_SUBSCRIPT:
            {
                result &= resolver_resolve_expression(resolver,
                                                      expression->subscript.base_expression, scope);
                if (!result) return false;

                result &= resolver_resolve_expression(resolver,
                                                      expression->subscript.index_expression,
                                                      scope);

                AST_Type* base_type = expression->subscript.base_expression->type;
                AST_Type* index_type = expression->subscript.index_expression->type;

                assert(base_type->kind == AST_TYPE_STATIC_ARRAY ||
                       base_type->kind == AST_TYPE_POINTER);

                assert(index_type->flags & AST_TYPE_FLAG_INT);
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

            default: assert(false);
        }


        if (result)
        {
            assert(expression->type);
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

        if (binop_is_cmp(expression))
        {
            result &= resolver_resolve_expression(resolver, lhs, scope);
            result &= resolver_resolve_expression(resolver, rhs, scope);

            if (!result) return false;

            assert(lhs->type == rhs->type);

            assert(!expression->type);
            expression->type = Builtin::type_bool;
        }
        else
        {
            result &= resolver_resolve_expression(resolver, lhs, scope);
            result &= resolver_resolve_expression(resolver, rhs, scope);

            if (!result) return false;

            assert(lhs->type == rhs->type);

            assert(!expression->type);
            expression->type = lhs->type;
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

        assert(false);
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
                AST_Identifier* ident = type_spec->identifier.identifier;
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

                assert(count_expr->type->flags & AST_TYPE_FLAG_INT);

                *type_dest = ast_find_or_create_array_type(resolver->context, element_type,
                                                           count_expr, scope);
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

            case AST_DECL_AGGREGATE_TYPE:
            {
                assert(decl->aggregate_type.type);
                return decl->aggregate_type.type;
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

        assert(false);
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
            case AST_BINOP_AND:
            case AST_BINOP_OR:
            {
                return true;
            }

            default: assert(false);
        }

        assert(false);
        return false;
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
