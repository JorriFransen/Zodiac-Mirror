#include "ast.h"

#include "builtin.h"
#include "const_interpreter.h"

#include <inttypes.h>

namespace Zodiac
{
    AST_Module* ast_module_new(Context* context, const char* module_name, const char* path)
    {
        assert(context);
        assert(module_name);

        AST_Module* result = arena_alloc(context->arena, AST_Module);

		result->declarations = (AST_Declaration**)mem_alloc(sizeof(AST_Declaration*) * 128);
		result->declaration_count = 128;

        const char* file_name = extract_file_name_from_path(path, false);
        const char* file_dir = extract_directory_from_path(path);

        result->global_declarations = nullptr;
        result->module_scope = ast_scope_new(context, context->builtin_scope, result, true, 0);
        result->entry_point = nullptr;
        result->module_name = module_name;
        result->module_file_name = file_name;
        result->module_file_dir = file_dir;
        result->import_decls = nullptr;
        result->import_modules = nullptr;
        result->resolved = false;
        result->gen_data = nullptr;
        result->poly_dirty = false;

        return result;
    }

    AST_Identifier* ast_identifier_new(Context* context, Atom atom, File_Pos file_pos)
    {
        assert(context);

        AST_Identifier* result = arena_alloc(context->arena, AST_Identifier);
        result->atom = atom;
        result->file_pos = file_pos;

        return result;
    }

    AST_Identifier* ast_identifier_new(Context* context, const char* name, File_Pos file_pos)
    {
        return ast_identifier_new(context, atom_get(context->atom_table, name), file_pos);
    }

    AST_Directive* ast_directive_new(Context* context, AST_Directive_Kind kind, File_Pos file_pos)
    {
        assert(context);
        assert(kind != AST_DIREC_INVALID);

        AST_Directive* result = arena_alloc(context->arena, AST_Directive);
        result->kind = kind;
        result->file_pos = file_pos;

        return result;
    }

    AST_Expression* ast_expression_new(Context* context, File_Pos file_pos,
                                       AST_Expression_Kind kind)
    {
        assert(context);

        AST_Expression* result = arena_alloc(context->arena, AST_Expression);
        result->file_pos = file_pos;
        result->kind = kind;
        result->type = nullptr;
        result->flags = AST_EXPR_FLAG_NONE;

        return result;
    }

    AST_Expression* ast_binary_expression_new(Context* context, File_Pos file_pos,
        AST_Expression* lhs, AST_Binop_Kind op, AST_Expression* rhs)
    {
        assert(context);
        assert(lhs);
        assert(rhs);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_BINARY);

        result->binary.op = op;
        result->binary.lhs = lhs;
        result->binary.rhs = rhs;

        return result;
    }

    AST_Expression* ast_unary_expression_new(Context* context, File_Pos file_pos,
        AST_Unop_Kind op, AST_Expression* operand)
    {
        assert(context);
        assert(operand);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_UNARY);

        result->unary.op = op;
        result->unary.operand = operand;

        if (operand->flags & AST_EXPR_FLAG_LITERAL)
        {
            result->flags |= AST_EXPR_FLAG_LITERAL;
        }

        return result;
    }

    AST_Expression* ast_ident_expression_new(Context* context, File_Pos file_pos,
                                             AST_Identifier* identifier)
    {
        assert(context);
        assert(identifier);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_IDENTIFIER);

        result->identifier = identifier;

        return result;
    }

    AST_Expression* ast_call_expression_new(Context* context, File_Pos file_pos,
                                            AST_Expression* ident_expr,
        BUF(AST_Expression*) arg_exprs)
    {
        assert(context);
        assert(ident_expr);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_CALL);

        result->call.ident_expression = ident_expr;
        result->call.arg_expressions = arg_exprs;
        result->call.builtin_function = AST_BUILTIN_FUNC_INVALID;

        return result;
    }

    AST_Expression* ast_subscript_expression_new(Context* context, File_Pos file_pos,
                                                 AST_Expression* base_expression,
                                                 AST_Expression* index_expression)
    {
        assert(context);
        assert(base_expression);
        assert(index_expression);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_SUBSCRIPT);
        result->subscript.base_expression = base_expression;
        result->subscript.index_expression = index_expression;

        return result;
    }

    AST_Expression* ast_boolean_literal_expression_new(Context* context, File_Pos file_pos,
                                                       bool value)
    {
        assert(context);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_BOOL_LITERAL);
        result->bool_literal.boolean = value;
        result->flags |= AST_EXPR_FLAG_LITERAL;
        return result;
    }

    AST_Expression* ast_null_literal_expression_new(Context* context, File_Pos file_pos)
    {
        assert(context);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_NULL_LITERAL);
        result->flags |= AST_EXPR_FLAG_LITERAL;
        return result;
    }

    AST_Expression* ast_string_literal_expression_new(Context* context, File_Pos file_pos,
                                                      Atom value)
    {
        assert(context);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_STRING_LITERAL);
        ast_string_literal_expression_new(context, result, file_pos, value);
        return result;
    }

    AST_Expression* ast_string_literal_expression_new(Context* context, AST_Expression* ex_expr,
                                                      File_Pos file_pos, Atom value)
    {
        ex_expr->kind = AST_EXPR_STRING_LITERAL;
        ex_expr->string_literal.atom = value;
        ex_expr->flags |= AST_EXPR_FLAG_LITERAL;
        return ex_expr;
    }

    AST_Expression* ast_integer_literal_expression_new(Context* context, File_Pos file_pos,
                                                       uint64_t value)
    {
        assert(context);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_INTEGER_LITERAL);

        result->integer_literal.u64 = value;
        result->flags |= AST_EXPR_FLAG_LITERAL;
        result->flags |= AST_EXPR_FLAG_CONST;
        result->flags |= AST_EXPR_FLAG_INTEGER_LITERAL;

        return result;
    }

    AST_Expression* ast_float_literal_expression_new(Context* context, File_Pos file_pos,
                                                     double r64, float r32)
    {
        assert(context);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_FLOAT_LITERAL);

        result->float_literal.r64 = r64;
        result->float_literal.r32 = r32;
        result->flags |= AST_EXPR_FLAG_LITERAL;

        return result;
    }

    AST_Expression* ast_character_literal_expression_new(Context* context, File_Pos file_pos,
                                                         char value)
    {
        assert(context);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_CHAR_LITERAL);
        result->character_literal.c = value;
        result->flags |= AST_EXPR_FLAG_LITERAL;
        return result;
    }

    AST_Expression* ast_compound_literal_expression_new(Context* context, File_Pos file_pos,
                                                        BUF(AST_Expression*) expressions)
    {
        assert(context);
        auto result = ast_expression_new(context, file_pos, AST_EXPR_COMPOUND_LITERAL);
        result->compound_literal.expressions = expressions;
        result->flags |= AST_EXPR_FLAG_LITERAL;
        return result;
    }

    AST_Expression* ast_array_length_expression_new(Context* context, File_Pos file_pos,
                                                    AST_Expression* ident_expr)
    {
        assert(context);
        assert(ident_expr);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_ARRAY_LENGTH);
        result->array_length.ident_expr = ident_expr;
        return result;
    }

    AST_Expression* ast_sizeof_expression_new(Context* context, File_Pos file_pos,
                                              AST_Type_Spec* type_spec)
    {
        assert(context);
        assert(type_spec);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_SIZEOF);
        result->sizeof_expr.type_spec = type_spec;
        return result;
    }

    AST_Expression* ast_get_type_info_expression_new(Context* context, File_Pos file_pos,
                                                     AST_Type_Spec* type_spec)
    {
        assert(context);
        assert(type_spec);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_GET_TYPE_INFO);
        result->get_type_info_expr.type_spec = type_spec;
        return result;
    }

    AST_Expression* ast_dot_expression_new(Context* context, File_Pos file_pos,
                                           AST_Expression* base_expr,
                                           AST_Expression* member_expr)
    {
        assert(context);
        assert(base_expr);
        assert(member_expr);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_DOT);
        result->dot.base_expression = base_expr;
        result->dot.member_expression = member_expr;

        return result;
    }

	AST_Expression* ast_cast_expression_new(Context* context, File_Pos file_pos,
		AST_Type_Spec* type_spec, AST_Expression* cast_expr)
	{
		assert(context);
		assert(type_spec);
		assert(cast_expr);

		auto result = ast_expression_new(context, file_pos, AST_EXPR_CAST);
		result->cast_expr.type_spec = type_spec;
		result->cast_expr.expr = cast_expr;

		return result;
	}

    AST_Expression* ast_post_increment_expression_new(Context* context, File_Pos file_pos,
                                                      AST_Expression* base_expression)
    {
        assert(context);
        assert(base_expression);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_POST_INCREMENT);
        result->base_expression = base_expression;

        return result;
    }

    AST_Expression* ast_post_decrement_expression_new(Context* context, File_Pos file_pos,
                                                      AST_Expression* base_expression)
    {
        assert(context);
        assert(base_expression);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_POST_DECREMENT);
        result->base_expression = base_expression;

        return result;
    }

    AST_Expression* ast_expression_list_expression_new(Context* context, File_Pos file_pos,
                                                       BUF(AST_Expression*) expressions)
    {
        auto result = ast_expression_new(context, file_pos, AST_EXPR_EXPRESSION_LIST);
        result->list.expressions = expressions;

        return result;
    }

    AST_Expression* ast_expression_ignored_value_new(Context* context, File_Pos file_pos)
    {
        auto result = ast_expression_new(context, file_pos, AST_EXPR_IGNORED_VALUE);

        return result;
    }

    AST_Expression* ast_make_lvalue_expression_new(Context* context, File_Pos file_pos,
                                                   AST_Expression* non_lvalue)
    {
        auto result = ast_expression_new(context, file_pos, AST_EXPR_MAKE_LVALUE);

        assert(!(non_lvalue->flags & AST_EXPR_FLAG_LVALUE));
        result->make_lvalue.expression = non_lvalue;
        result->flags |= AST_EXPR_FLAG_LVALUE;

        return result;
    }

    AST_Expression* ast_func_name_expression_new(Context* context, File_Pos file_pos)
    {
        auto result = ast_expression_new(context, file_pos, AST_EXPR_FUNC_NAME);
        result->flags |= AST_EXPR_FLAG_CONST;
        return result;
    }

    AST_Aggregate_Declaration* ast_aggregate_declaration_new(Context* context, File_Pos file_pos,
                                                             BUF(AST_Declaration*) members,
                                                             BUF(AST_Identifier*) poly_args,
                                                             BUF(AST_Overload_Directive) overloads)
    {
        assert(context);

        auto result = arena_alloc(context->arena, AST_Aggregate_Declaration);

        result->file_pos = file_pos;
        result->members = members;
        result->poly_args = poly_args;
        result->poly_instances = nullptr;
        result->overload_directives = overloads;

        return result;
    }

    AST_Declaration* ast_declaration_new(Context* context, File_Pos file_pos,
                                         AST_Declaration_Kind kind,
                                         AST_Declaration_Location location,
                                         AST_Identifier* identifier,
                                         AST_Directive* directive)
    {
        assert(context);

        AST_Declaration* result = arena_alloc(context->arena, AST_Declaration);
        result->file_pos = file_pos;
        result->kind = kind;
        result->location = location;
        result->identifier = identifier;
        result->directive = directive;

        return result;
    }

    AST_Declaration* ast_list_declaration_new(Context* context, File_Pos file_pos,
                                              AST_Expression* list_expr,
                                              AST_Expression* init_expr)
    {
        assert(list_expr->kind == AST_EXPR_EXPRESSION_LIST);
        assert(init_expr->kind == AST_EXPR_CALL);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_LIST,
                                                      AST_DECL_LOC_LOCAL, nullptr, nullptr);

        result->list.list_expression = list_expr;
        result->list.init_expression = init_expr;
        result->list.declarations = nullptr;

        return result;
    }

    AST_Declaration* ast_function_declaration_new(Context* context, File_Pos file_pos,
                                                  AST_Scope* scope,
                                                  AST_Identifier* identifier,
                                                  BUF(AST_Declaration*) args,
                                                  bool is_vararg, 
                                                  AST_Type_Spec* return_type_spec,
                                                  AST_Statement* body_block,
                                                  AST_Scope* argument_scope)
    {
        assert(context);
        assert(identifier);
        assert(argument_scope);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_FUNC,
                                                      AST_DECL_LOC_GLOBAL,
                                                      identifier, nullptr);

        result->function.args = args;
        if (is_vararg)
        {
            result->flags |= AST_DECL_FLAG_FUNC_VARARG;
        }
        result->function.locals = nullptr;
        result->function.return_type_spec = return_type_spec;
        result->function.return_type = nullptr;
        result->function.inferred_return_type = nullptr;
        result->function.body_block = body_block;
        result->function.body_generated = false;
        result->scope = scope;

        result->function.argument_scope = argument_scope;
        if (body_block)
        {
            assert(body_block->kind == AST_STMT_BLOCK);
            assert(body_block->block.scope);
            assert(body_block->block.scope->parent == argument_scope);
        }

        return result;
    }

    AST_Declaration* ast_mutable_declaration_new(Context* context, File_Pos file_pos,
                                                 AST_Identifier* identifier,
                                                 AST_Type_Spec* type_spec,
                                                 AST_Expression* init_expr,
                                                 AST_Declaration_Location location)
    {
        assert(context);
        assert(identifier || location == AST_DECL_LOC_AGGREGATE_MEMBER);
        assert(type_spec || init_expr);
        assert(location != AST_DECL_LOC_INVALID);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_MUTABLE,
                                                      location, identifier, nullptr);

        result->mutable_decl.type_spec = type_spec;
        result->mutable_decl.init_expression = init_expr;

        return result;
    }

    AST_Declaration* ast_constant_variable_declaration_new(Context* context, File_Pos file_pos,
                                                           AST_Identifier* identifier,
                                                           AST_Type_Spec* type_spec,
                                                           AST_Expression* init_expr,
                                                           AST_Declaration_Location location)
    {
        assert(context);
        assert(identifier);
        // assert(init_expr);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_CONSTANT_VAR,
                                                      location, identifier, nullptr);

        result->constant_var.type_spec = type_spec;
        result->constant_var.init_expression = init_expr;

        return result;
    }

    AST_Declaration* ast_type_declaration_new(Context* context, File_Pos file_pos, AST_Type* type,
                                              AST_Identifier* identifier)
    {
        assert(context);
        assert(type);
        assert(identifier);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_TYPE,
                                                      AST_DECL_LOC_INVALID,
                                                      identifier, nullptr);

        result->type.type = type;

        return result;
    }

    AST_Declaration* ast_dyn_link_declaration_new(Context* context, File_Pos file_pos,
                                                  Atom link_name,
                                                  AST_Declaration_Location location)
    {
        assert(context);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_DYN_LINK,
                                                      location,
                                                      nullptr, nullptr);

        result->dyn_link_name = link_name;

        return result;
    }

    AST_Declaration* ast_static_if_declaration_new(Context* context, File_Pos file_pos,
                                                   AST_Expression* cond_expr,
                                                   AST_Declaration* then_declaration,
                                                   AST_Declaration* else_declaration)
    {
        assert(context);
        assert(cond_expr);
        assert(then_declaration);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_STATIC_IF,
                                                      AST_DECL_LOC_GLOBAL,
                                                      nullptr, nullptr);

        result->static_if.cond_expr = cond_expr;
        result->static_if.then_declaration = then_declaration;
        result->static_if.else_declaration = else_declaration;

        return result;
    }

    AST_Declaration* ast_using_declaration_new(Context* context, File_Pos file_pos,
                                               AST_Expression* ident_expr,
                                               AST_Declaration_Location location)
    {
        assert(context);
        assert(ident_expr);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_USING,
                                                      location, nullptr, nullptr);

        result->using_decl.ident_expression = ident_expr;
        result->using_decl.scope_decl = nullptr;

        return result;
    }

    AST_Declaration* ast_using_declaration_new(Context* context, File_Pos file_pos,
                                               AST_Identifier* ident,
                                               AST_Declaration_Location location)
    {
        AST_Expression* ident_expr = ast_ident_expression_new(context, file_pos, ident);
        return ast_using_declaration_new(context, file_pos, ident_expr, location);
    }

    AST_Declaration* ast_block_declaration_new(Context* context, File_Pos file_pos,
                                               BUF(AST_Declaration*) block_decls)
    {
        assert(context);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_BLOCK,
                                                      AST_DECL_LOC_GLOBAL,
                                                      nullptr, nullptr);

        result->block.decls = block_decls;

        return result;
    }

    AST_Declaration* ast_static_assert_declaration_new(Context* context, File_Pos file_pos,
        AST_Expression* assert_expr)
    {
        assert(context);
        assert(assert_expr);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_STATIC_ASSERT,
                                                      AST_DECL_LOC_GLOBAL, nullptr, nullptr);
        result->static_assert_expression = assert_expr;
        return result;
    }

    AST_Declaration* ast_import_declaration_new(Context* context, File_Pos file_pos,
                                                AST_Identifier* identifier,
                                                AST_Identifier* import_module_identifier)
    {
        assert(context);
        assert(identifier);
        assert(import_module_identifier);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_IMPORT,
                                                      AST_DECL_LOC_GLOBAL, identifier, nullptr);
        result->import.module_identifier = import_module_identifier;
        return result;
    }

    AST_Declaration* ast_struct_declaration_new(Context* context, File_Pos file_pos,
                                                AST_Identifier* identifier,
                                                AST_Aggregate_Declaration* aggregate_decl,
                                                AST_Declaration_Location location,
                                                AST_Scope* scope)
    {
        assert(context);
        // assert(identifier);
        assert(aggregate_decl);
        assert(scope);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_AGGREGATE_TYPE,
                                                      location, identifier, nullptr);
        result->aggregate_type.kind = AST_AGG_DECL_STRUCT;
        result->aggregate_type.type = nullptr;
        result->aggregate_type.aggregate_decl = aggregate_decl;
        result->aggregate_type.scope = scope;

        return result;
    }

    AST_Declaration* ast_union_declaration_new(Context* context, File_Pos file_pos,
                                               AST_Identifier* identifier,
                                               AST_Aggregate_Declaration* aggregate_decl,
                                               AST_Declaration_Location location,
                                               AST_Scope* scope)
    {
        assert(context);
        // assert(identifier);
        assert(aggregate_decl);
        assert(scope);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_AGGREGATE_TYPE,
                                                      location, identifier, nullptr);
        result->aggregate_type.kind = AST_AGG_DECL_UNION;
        result->aggregate_type.type = nullptr;
        result->aggregate_type.aggregate_decl = aggregate_decl;
        result->aggregate_type.scope = scope;

        return result;
    }

    AST_Declaration* ast_enum_declaration_new(Context* context, File_Pos file_pos,
                                              AST_Identifier* identifier,
                                              AST_Type_Spec* enum_type_spec,
                                              AST_Aggregate_Declaration* aggregate_decl,
                                              AST_Scope* scope)
    {
        assert(context);
        assert(identifier);
        assert(aggregate_decl);
        assert(scope);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_AGGREGATE_TYPE,
                                                      AST_DECL_LOC_GLOBAL, identifier, nullptr);

        result->aggregate_type.kind = AST_AGG_DECL_ENUM;
        result->aggregate_type.type = nullptr;
        result->aggregate_type.aggregate_decl = aggregate_decl;
        result->aggregate_type.scope = scope;
        result->aggregate_type.enum_type_spec = enum_type_spec;

        return result;
    }

	AST_Declaration* ast_typedef_declaration_new(Context* context, File_Pos file_pos,
			                                     AST_Identifier* identifier,
			                                     AST_Type_Spec* type_spec)
	{
		assert(context);
		assert(identifier);
		assert(type_spec);

		AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_TYPEDEF,
			                                          AST_DECL_LOC_GLOBAL, identifier, nullptr);

		result->typedef_decl.type_spec = type_spec;
		result->typedef_decl.type = nullptr;

		return result;
	}

    AST_Declaration* ast_insert_declaration_new(Context* context, File_Pos file_pos,
                                                AST_Statement* stmt)
    {
        assert(context);
        assert(stmt);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_INSERT,
                                                      AST_DECL_LOC_GLOBAL, nullptr, nullptr);

        result->insert_decl.call_statement = stmt;
        assert(!(result->flags & AST_DECL_FLAG_INSERT_GENERATED));

        return result;
    }

    AST_Declaration* ast_poly_type_spec_declaration_new(Context* context, File_Pos file_pos,
                                                        AST_Identifier* ident,
                                                        AST_Type_Spec* type_spec)
    {
        assert(context);
        assert(ident);
        assert(type_spec);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_POLY_TYPE_SPEC,
                                                      AST_DECL_LOC_AGGREGATE_MEMBER, ident,
                                                      nullptr);
        result->poly_type_spec.type_spec = type_spec;

        return result;
    }

    AST_Statement* ast_declaration_statement_new(Context* context, File_Pos file_pos,
                                                 AST_Declaration* declaration)
    {
        assert(context);
        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_DECLARATION;
        result->file_pos = file_pos;
        result->declaration = declaration;

        return result;
    }

    AST_Statement* ast_block_statement_new(Context* context, File_Pos file_pos,
                                           BUF(AST_Statement*) block_statements,
                                           AST_Scope* block_scope)
    {
        assert(context);
        assert(block_scope);
        assert(block_scope->parent);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_BLOCK;
        result->file_pos = file_pos;

        result->block.statements = block_statements;
        result->block.scope = block_scope;

        return result;
    }

    AST_Statement* ast_return_statement_new(Context* context, File_Pos file_pos,
                                            AST_Expression* return_expr)
    {
        assert(context);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_RETURN;
        result->file_pos = file_pos;

        result->return_expression = return_expr;

        return result;
    }

    AST_Statement* ast_if_statement_new(Context* context, File_Pos file_pos,
                                        AST_Expression* cond_expr,
                                        AST_Statement* then_stmt, AST_Statement* else_stmt)
    {
        assert(context);
        assert(cond_expr);
        assert(then_stmt);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_IF;
        result->file_pos = file_pos;

        result->if_stmt.if_expression = cond_expr;
        result->if_stmt.then_statement = then_stmt;
        result->if_stmt.else_statement = else_stmt;

        return result;
    }

    AST_Statement* ast_static_if_statement_new(Context* context, File_Pos file_pos,
                                               AST_Expression* cond_expr,
                                               AST_Statement* then_stmt,
                                               AST_Statement* else_stmt)
    {
        assert(context);
        assert(cond_expr);
        assert(then_stmt);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_STATIC_IF;
        result->file_pos = file_pos;

        result->static_if_stmt.if_expression = cond_expr;
        result->static_if_stmt.then_statement = then_stmt;
        result->static_if_stmt.else_statement = else_stmt;

        return result;
    }

    AST_Statement* ast_assign_statement_new(Context* context, File_Pos file_pos,
                                            AST_Expression* lvalue_expr,
                                            AST_Expression* expression)
    {
        assert(context);
        assert(lvalue_expr);
        assert(expression);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_ASSIGN;
        result->file_pos = file_pos;

        result->assign.lvalue_expression = lvalue_expr;
        result->assign.expression = expression;

        return result;
    }

    AST_Statement* ast_call_statement_new(Context* context, AST_Expression* call_expression)
    {
        assert(context);
        assert(call_expression);
        assert(call_expression->kind == AST_EXPR_CALL);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_CALL;
        result->file_pos = call_expression->file_pos;

        result->call_expression = call_expression;

        return result;
    }

    AST_Statement* ast_while_statement_new(Context* context, File_Pos file_pos, AST_Scope* scope,
                                           AST_Expression* cond_expr, AST_Statement* body_stmt)
    {
        assert(context);
        assert(cond_expr);
        assert(body_stmt);
        assert(scope);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_WHILE;
        result->file_pos = file_pos;

        result->while_stmt.scope = scope;
        result->while_stmt.cond_expr = cond_expr;
        result->while_stmt.body_stmt = body_stmt;

        return result;
    }

    AST_Statement* ast_for_statement_new(Context* context, File_Pos file_pos, AST_Scope* scope,
                                         AST_Statement* init_stmt, AST_Expression* cond_expr,
                                         AST_Statement* step_stmt, AST_Statement* body_stmt)
    {
        assert(context);
        assert(scope);
        assert(init_stmt);
        assert(cond_expr);
        assert(step_stmt);
        assert(body_stmt);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_FOR;
        result->file_pos = file_pos;

        result->for_stmt.scope = scope;
        result->for_stmt.init_stmt = init_stmt;
        result->for_stmt.cond_expr = cond_expr;
        result->for_stmt.step_stmt = step_stmt;
        result->for_stmt.body_stmt = body_stmt;

        return result;
    }

    AST_Statement* ast_switch_statement_new(Context* context, File_Pos file_pos,
                                            AST_Expression* switch_expr, BUF(AST_Switch_Case) cases)
    {
        assert(context);
        assert(switch_expr);
        assert(cases);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_SWITCH;
        result->file_pos = file_pos;

        result->switch_stmt.switch_expression = switch_expr;
        result->switch_stmt.cases = cases;

        return result;
    }

    AST_Statement* ast_break_statement_new(Context* context, File_Pos file_pos)
    {
        assert(context);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_BREAK;
        result->file_pos = file_pos;

        return result;
    }

    AST_Statement* ast_insert_statement_new(Context* context, File_Pos file_pos, AST_Statement* statement)
    {
        assert(context);
        assert(statement);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_INSERT;
        result->file_pos = file_pos;

        result->insert.statement = statement;

        return result;
    }

    AST_Statement* ast_assert_statement_new(Context* context, File_Pos file_pos,
                                            AST_Expression* assert_expr)
    {
        assert(context);
        assert(assert_expr);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_ASSERT;
        result->file_pos = file_pos;

        result->assert_expression = assert_expr;

        return result;
    }

    AST_Statement* ast_assert_fail_statement_new(Context* context, File_Pos file_pos)
    {
        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_ASSERT_FAIL;
        result->file_pos = file_pos;

        return result;
    }

    AST_Statement* ast_defer_statement_new(Context* context, File_Pos file_pos,
                                           AST_Statement* defer_statement)
    {
        assert(context);
        assert(defer_statement);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_DEFER;
        result->file_pos = file_pos;

        result->defer_statement = defer_statement;

        return result;
    }

    AST_Statement* ast_post_increment_statement_new(Context* context, File_Pos file_pos,
                                                    AST_Expression* post_inc_expr)
    {
        assert(context);
        assert(post_inc_expr);
        assert(post_inc_expr->kind == AST_EXPR_POST_INCREMENT);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_POST_INCREMENT;
        result->file_pos = file_pos;
        result->post_increment = post_inc_expr;

        return result;
    }

    AST_Statement* ast_post_decrement_statement_new(Context* context, File_Pos file_pos,
                                                    AST_Expression* post_dec_expr)
    {
        assert(context);
        assert(post_dec_expr);
        assert(post_dec_expr->kind == AST_EXPR_POST_DECREMENT);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_POST_DECREMENT;
        result->file_pos = file_pos;
        result->post_decrement = post_dec_expr;

        return result;
    }

    AST_Type* ast_type_new(Context* context, AST_Type_Kind kind, AST_Type_Flags type_flags,
                           const char* name, uint64_t bit_size)
    {
        assert(context);

        AST_Type* result = arena_alloc(context->arena, AST_Type);
        result->kind = kind;
        result->flags = type_flags;
        result->bit_size = bit_size;
        result->name = name;
        result->overloads = nullptr;
        result->pointer_to = nullptr;
        result->arrays_of = nullptr;
        result->info_index = 0;

        return result;
    }

    AST_Type* ast_type_base_new(Context* context, AST_Type_Flags type_flags, const char* name,
                                uint64_t bit_size)
    {
        assert(context);
        assert(bit_size % 8 == 0);

        AST_Type* result = ast_type_new(context, AST_TYPE_BASE, type_flags, name, bit_size);

        return result;
    }

    AST_Type* ast_type_pointer_new(Context* context, AST_Type* base_type)
    {
        assert(context);
        assert(base_type);

        AST_Type* result = ast_type_new(context, AST_TYPE_POINTER, AST_TYPE_FLAG_NONE,
                                        {}, Builtin::pointer_size);
        result->pointer.base = base_type;

        result->overloads = base_type->overloads;

        return result;
    }

    AST_Type* ast_type_static_array_new(Context* context, AST_Type* base_type, uint64_t count)
    {
        assert(context);
        assert(base_type);

        assert(base_type->bit_size);

        AST_Type* result = ast_type_new(context, AST_TYPE_STATIC_ARRAY, AST_TYPE_FLAG_NONE,
                                        {}, base_type->bit_size * count);
        result->static_array.base = base_type;
        result->static_array.count = count;

        return result;
    }

    AST_Type* ast_type_struct_new(Context* context, BUF(AST_Declaration*) member_declarations,
                                  const char* name, uint64_t bit_size, AST_Scope* scope,
                                  BUF(AST_Overload_Directive) overloads)
    {
        assert(context);
        assert(scope);
        // assert(member_declarations);

        AST_Type* result = ast_type_new(context, AST_TYPE_STRUCT, AST_TYPE_FLAG_NONE, name,
                                        bit_size);
        result->aggregate_type.member_declarations = member_declarations;
        result->aggregate_type.scope = scope;
        result->overloads = overloads;

        return result;
    }

    AST_Type* ast_type_union_new(Context* context, BUF(AST_Declaration*) member_declarations,
                                 const char* name, uint64_t bit_size, AST_Scope* scope,
                                 BUF(AST_Overload_Directive) overloads)
    {
        assert(context);
        assert(scope);
        // assert(name);

        AST_Type* result = ast_type_new(context, AST_TYPE_UNION, AST_TYPE_FLAG_NONE, name,
                                        bit_size);
        result->aggregate_type.member_declarations = member_declarations;
        result->aggregate_type.scope = scope;
        result->overloads = overloads;

        return result;
    }

    AST_Type* ast_type_enum_new(Context* context, BUF(AST_Declaration*) member_decls,
                                const char* name, AST_Type* base_type, AST_Scope* scope)
    {
        assert(context);
        assert(member_decls);
        assert(base_type);
        assert(base_type->bit_size);
        assert(base_type->flags & AST_TYPE_FLAG_INT);
        assert(scope);

        AST_Type* result = ast_type_new(context, AST_TYPE_ENUM, AST_TYPE_FLAG_NONE,
                                        name, base_type->bit_size);
        result->aggregate_type.member_declarations = member_decls;
        result->aggregate_type.scope = scope;
        result->aggregate_type.base_type = base_type;

        return result;
    }

    AST_Type* ast_type_function_new(Context* context, bool is_vararg, BUF(AST_Type*) arg_types,
                                    AST_Type* return_type)
    {
        assert(context);
        assert(return_type);

        AST_Type* result = ast_type_new(context, AST_TYPE_FUNCTION, AST_TYPE_FLAG_NONE, {}, 64);
        if (is_vararg)
        {
            result->flags |= AST_TYPE_FLAG_FUNC_VARARG;
        }
        result->function.arg_types = arg_types;
        result->function.return_type = return_type;

        return result;
    }

    AST_Type_Spec* ast_type_spec_new(Context* context, File_Pos file_pos, AST_Type_Spec_Kind kind)
    {
        assert(context);

        AST_Type_Spec* result = arena_alloc(context->arena, AST_Type_Spec);
        result->kind = kind;
        result->file_pos = file_pos;
        result->type = nullptr;

        return result;
    }

    AST_Type_Spec* ast_type_spec_identifier_new(Context* context, File_Pos file_pos,
                                                AST_Identifier* identifier,
                                                BUF(AST_Type_Spec*) arg_type_specs)
    {
        assert(context);
        assert(identifier);

        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_IDENT);
        result->identifier.identifier = identifier;
        result->identifier.arg_type_specs = arg_type_specs;

        for (uint64_t i = 0; i < BUF_LENGTH(arg_type_specs); i++)
        {
            auto arg_type_spec = arg_type_specs[i];
            if (arg_type_spec->kind == AST_TYPE_SPEC_POLY_FUNC_ARG ||
                (arg_type_spec->flags & AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN))
            {
                result->flags |= AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN;
                break;
            }
        }

        return result;
    }

    AST_Type_Spec* ast_type_spec_dot_new(Context* context, File_Pos file_pos,
                                         AST_Identifier* module_ident,
                                         AST_Type_Spec* member_type_spec)
    {
        assert(context);
        assert(module_ident);
        assert(member_type_spec);

        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_DOT);
        result->dot.module_ident = module_ident;
        result->dot.member_type_spec = member_type_spec;

        if (member_type_spec->kind == AST_TYPE_SPEC_POLY_FUNC_ARG ||
            (member_type_spec->flags & AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN))
        {
            result->flags |= AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN;
        }

        return result;
    }

    AST_Type_Spec* ast_type_spec_pointer_new(Context* context, File_Pos file_pos,
                                             AST_Type_Spec* base_type_spec)
    {
        assert(context);
        assert(base_type_spec);

        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_POINTER);
        result->pointer.base = base_type_spec;

        if (base_type_spec->kind == AST_TYPE_SPEC_POLY_FUNC_ARG ||
            (base_type_spec->flags & AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN))
        {
            result->flags |= AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN;
        }

        return result;
    }

    AST_Type_Spec* ast_type_spec_static_array_new(Context* context, File_Pos file_pos,
                                                  AST_Expression* count_expr,
                                                  AST_Type_Spec* base_type_spec)
    {
        assert(context);
        assert(count_expr);
        assert(base_type_spec);

        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_STATIC_ARRAY);
        result->static_array.count_expr = count_expr;
        result->static_array.base = base_type_spec;

        if (base_type_spec->kind == AST_TYPE_SPEC_POLY_FUNC_ARG ||
            (base_type_spec->flags & AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN))
        {
            result->flags |= AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN;
        }

        return result;
    }

    AST_Type_Spec* ast_type_spec_function_new(Context* context, File_Pos file_pos,
                                              bool is_vararg, BUF(AST_Declaration*) arg_decls,
                                              AST_Type_Spec* return_type_spec,
                                              AST_Scope* arg_scope, const char* name)
    {
        assert(context);
        assert(arg_scope);
        assert(name);

        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_FUNCTION);
        if (is_vararg)
        {
            result->flags |= AST_TYPE_SPEC_FLAG_FUNC_VARARG;
        }
        result->function.args = arg_decls;
        result->function.return_type_spec = return_type_spec;
        result->function.arg_scope = arg_scope;
        result->function.name = name;

        bool has_poly_children = false;
        for (uint64_t i = 0; i < BUF_LENGTH(arg_decls); i++)
        {
            auto arg_decl = arg_decls[i];
            assert(arg_decl->kind == AST_DECL_MUTABLE);
            auto arg_decl_type_spec = arg_decl->mutable_decl.type_spec;

            if (arg_decl_type_spec->kind == AST_TYPE_SPEC_POLY_FUNC_ARG ||
                (arg_decl_type_spec->flags & AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN))
            {
                has_poly_children = true;
                break;
            }
        }

        if (has_poly_children)
        {
            result->flags |= AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN;
        }

        return result;
    }

    AST_Type_Spec* ast_type_spec_typeof_new(Context* context, File_Pos file_pos,
                                            AST_Expression* expr)
    {
        assert(context);
        assert(expr);

        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_TYPEOF);
        result->typeof_expr.expr = expr;

        return result;
    }

    AST_Type_Spec* ast_type_spec_from_type_new(Context* context, File_Pos file_pos,
                                               AST_Type* type)
    {
        assert(context);
        assert(type);

        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_FROM_TYPE);
        result->type = type;

        return result;
    }

    AST_Type_Spec* ast_type_spec_poly_func_arg_new(Context* context, File_Pos file_pos,
                                                   AST_Identifier* identifier)
    {
        assert(context);
        assert(identifier);

        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_POLY_FUNC_ARG);
        result->poly_func_arg.identifier = identifier;

        return result;
    }

    AST_Type_Spec* ast_type_spec_mrv_new(Context* context, File_Pos file_pos,
                                         BUF(AST_Type_Spec*) specs,
                                         BUF(AST_Directive*) directives)
    {
        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_MRV);
        result->mrv.specs = specs;
        assert(directives);
        result->mrv.directives = directives;

        return result;
    }

    AST_Type_Spec* ast_type_spec_vararg_new(Context* context, File_Pos file_pos)
    {
        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_VARARG);

        return result;
    }

    AST_Scope* ast_scope_new(Context* context, AST_Scope* parent_scope, AST_Module* module,
		                     bool is_module_scope, uint64_t line)

    {
        assert(context);
		assert(module || parent_scope == nullptr);

        AST_Scope* result = arena_alloc(context->arena, AST_Scope);
        result->parent = parent_scope;
        result->flags = AST_SCOPE_FLAG_NONE;
        if (is_module_scope)
        {
            result->flags |= AST_SCOPE_FLAG_IS_MODULE_SCOPE;
        }
        result->module = module;
        result->using_modules = nullptr;
        result->using_declarations = nullptr;
        result->line = line;

        return result;
    }

    AST_Identifier* find_overload(AST_Type* type, AST_Overload_Operator_Kind op)
    {
        assert(type);

        for (uint64_t i = 0; i < BUF_LENGTH(type->overloads); i++)
        {
            if (type->overloads[i].op == op)
            {
                return type->overloads[i].identifier;
            }
        }

        return nullptr;
    }

    AST_Overload_Operator_Kind binary_op_to_overload_op(AST_Binop_Kind binop)
    {
        switch (binop)
        {
            case AST_BINOP_ADD: return AST_OVERLOAD_OP_PLUS;
            case AST_BINOP_SUB: return AST_OVERLOAD_OP_MINUS;
            case AST_BINOP_MUL: return AST_OVERLOAD_OP_MUL;
            case AST_BINOP_DIV: return AST_OVERLOAD_OP_DIV;

            default: return AST_OVERLOAD_OP_INVALID;
        }
    }

	void ast_scope_push_declaration(AST_Scope* scope, AST_Declaration* declaration)
	{
		assert(scope);
		assert(scope->module);
		assert(declaration);

		if (!declaration->identifier)
		{
			return;
		}

        if (declaration->scope)
        {
            assert(declaration->scope == scope);
        }
        else
        {
            declaration->scope = scope;
        }

		auto module = scope->module;

		uint64_t scope_hash = hash_pointer(scope);
		uint64_t ident_hash = hash_string(declaration->identifier->atom.data,
			                              declaration->identifier->atom.length);
		uint64_t hash = hash_mix(scope_hash, ident_hash);
		uint64_t hash_index = hash & (module->declaration_count - 1);

		uint64_t iterations = 0;
		while (iterations <= module->declaration_count)
		{
			AST_Declaration* decl = module->declarations[hash_index];
			if (decl)
			{
                if (decl == declaration) assert(false);
                int y = 1;
			}
			else
			{
				module->declarations[hash_index] = declaration;
				return;
			}

			iterations++;
			hash_index++;
			if (module->declaration_count <= hash_index)
			{
				hash_index = 0;
			}
		}

		ast_module_grow_declaration_hash(scope->module);
		ast_scope_push_declaration(scope, declaration);
	}

	AST_Declaration* ast_scope_find_declaration(Context* context, AST_Scope* scope,
		                                        const Atom& ident_atom)
	{
		assert(scope);

		if (scope->module)
		{
            assert(scope->parent);
			auto module = scope->module;

			uint64_t scope_hash = hash_pointer(scope);
			uint64_t ident_hash = hash_string(ident_atom.data, ident_atom.length);
			uint64_t hash = hash_mix(scope_hash, ident_hash);
			uint64_t hash_index = hash & (module->declaration_count - 1);

            // if (scope->is_module_scope)
            // {
            //     printf("hash for %s in scope %s: %lu\n", identifier->atom.data,
            //            scope->module->module_name, hash);
            // }

			uint64_t iterations = 0;
			while (iterations <= module->declaration_count)
			{
				AST_Declaration* decl = module->declarations[hash_index];
				if (decl)
				{
					if (decl->scope == scope &&
						decl->identifier->atom == ident_atom)
					{
						return decl;
					}
				}
				else
				{
					return nullptr;
				}

				iterations++;
                hash_index++;
				if (module->declaration_count <= hash_index)
				{
					hash_index = 0;
				}
			}
		}

        if (!scope->module)
		{
            assert(!scope->parent);
			for (uint64_t i = 0; i < BUF_LENGTH(context->builtin_decls); i++)
			{
				AST_Declaration* builtin_decl = context->builtin_decls[i];
				if (builtin_decl->scope == scope &&
					builtin_decl->identifier->atom == ident_atom)
				{
					return builtin_decl;
				}
			}
		}

		return nullptr;
	}

	void ast_module_grow_declaration_hash(AST_Module* module)
	{
		assert(module);
		assert(module->declarations);
		assert(module->declaration_count);

		uint64_t new_count = module->declaration_count * 2;
		AST_Declaration** old_decls = module->declarations;
		uint64_t old_decl_count = module->declaration_count;
		AST_Declaration** new_decls = (AST_Declaration * *)mem_alloc(sizeof(AST_Declaration*) *
			                                                         new_count);

		module->declarations = new_decls;
		module->declaration_count = new_count;

		for (uint64_t i = 0; i < old_decl_count; i++)
		{
			AST_Declaration* old_decl = old_decls[i];
			if (old_decl)
			{
				ast_scope_push_declaration(old_decl->scope, old_decl);
			}
		}

		mem_free(old_decls);
	}

    AST_Type* ast_find_or_create_pointer_type(Context* context, AST_Type* base_type)
    {
        assert(context);
        assert(base_type);

        if (base_type->pointer_to)
        {
            return base_type->pointer_to;
        }

        AST_Type* pointer_type = ast_type_pointer_new(context, base_type);
        base_type->pointer_to = pointer_type;
        return pointer_type;
    }

    AST_Type* ast_find_or_create_array_type(Context* context, AST_Type* base_type,
                                            AST_Expression* count_expr, AST_Scope* scope)
    {
        assert(context);
        assert(base_type);
        assert(count_expr);
        assert(count_expr->flags & AST_EXPR_FLAG_CONST);
        assert((count_expr->type->flags & AST_TYPE_FLAG_INT) ||
               (count_expr->type->kind == AST_TYPE_ENUM &&
                (count_expr->type->aggregate_type.base_type->flags & AST_TYPE_FLAG_INT)));
        assert(scope);

        int64_t count_value = const_interpret_s64_expression(context, count_expr, scope);

        return ast_find_or_create_array_type(context, base_type, count_value);
    }

    AST_Type* ast_find_or_create_array_type(Context* context, AST_Type* base_type,
                                            uint64_t count)
    {
        assert(context);
        assert(base_type);

        for (uint64_t i = 0; i < BUF_LENGTH(base_type->arrays_of); i++)
        {
            AST_Type* array_type = base_type->arrays_of[i];
            if (array_type->static_array.count == count)
            {
                return array_type;
            }
        }

        AST_Type* array_type = ast_type_static_array_new(context, base_type, count);
        BUF_PUSH(base_type->arrays_of, array_type);
        return array_type;
    }

	AST_Type* ast_find_or_create_function_type(Context* context, bool is_vararg,
                                               BUF(AST_Type*) arg_types,
                                               AST_Type* return_type)
	{
		assert(context);
        assert(return_type);

        uint64_t hash = get_function_type_hash(is_vararg, arg_types, return_type);

        // uint64_t hash = ast_get_function_type_hash(is_vararg, arg_types, return_type);
        uint64_t hash_index = hash & (context->type_count - 1);

        uint64_t iterations = 0;
        bool found_slot = false;
        while (iterations < context->type_count)
        {
            AST_Type* ex_type = context->type_hash[hash_index];
            if (ex_type && ex_type->kind == AST_TYPE_FUNCTION)
            {
                bool ret_match = ex_type->function.return_type == return_type;
                bool var_match = (bool)((ex_type->flags & AST_TYPE_FLAG_FUNC_VARARG)) ==
                                    is_vararg;
                bool ac_match = BUF_LENGTH(ex_type->function.arg_types) ==
                    BUF_LENGTH(arg_types);

                if (ret_match && var_match && ac_match)
                {
                    uint64_t a_count = BUF_LENGTH(arg_types);
                    bool a_match = true;
                    for (uint64_t ai = 0; ai < a_count; ai++)
                    {
                        AST_Type* ex_arg_type = ex_type->function.arg_types[ai];
                        AST_Type* new_arg_type = arg_types[ai];

                        if (ex_arg_type != new_arg_type)
                        {
                            a_match = false;
                            break;
                        }
                    }

                    if (a_match)
                    {
                        return ex_type;
                    }
                }
            }
            else if (!ex_type)
            {
                found_slot = true;
                break;
            }

            iterations++;
            hash_index++;
            if (context->type_count <= hash_index)
            {
                hash_index = 0;
            }
        }

        if (found_slot)
        {
            AST_Type* result = ast_type_function_new(context, is_vararg, arg_types, return_type);
            context->type_hash[hash_index] = result;
            return result;
        }
        else
        {
            ast_grow_type_hash(context);
            return ast_find_or_create_function_type(context, is_vararg, arg_types, return_type);
        }

	}

    uint64_t get_function_type_hash(bool is_vararg, BUF(AST_Type*) arg_types,
                                    AST_Type* return_type)
    {
        uint64_t hash = hash_pointer(return_type);
        for (uint64_t i = 0; i < BUF_LENGTH(arg_types); i++)
        {
            hash = hash_mix(hash, hash_pointer(arg_types[i]));
        }
        hash = hash_mix(hash, is_vararg);

        return hash;
    }

    void ast_grow_type_hash(Context* context)
    {
        assert(context);

        uint64_t old_count = context->type_count;
        AST_Type** old_data = context->type_hash;
        uint64_t new_count = old_count * 2;
        AST_Type** new_data = (AST_Type**)mem_alloc(sizeof(AST_Type*) * new_count);

        context->type_hash = new_data;
        context->type_count = new_count;

        for (uint64_t i = 0; i < old_count; i++)
        {
            AST_Type* old_type = old_data[i];
            if (old_type)
            {
                assert(old_type->kind == AST_TYPE_FUNCTION);

                uint64_t hash = get_function_type_hash((old_type->flags &
                                                        AST_TYPE_FLAG_FUNC_VARARG),
                                                       old_type->function.arg_types,
                                                       old_type->function.return_type);
                uint64_t hash_index = hash & (new_count - 1);

                uint64_t iterations = 0;
                bool found_slot = false;
                while (iterations < new_count)
                {
                    AST_Type* new_type = new_data[hash_index];
                    if (new_type)
                    {
                    }
                    else
                    {
                        new_data[hash_index] = old_type;
                        found_slot = true;
                        break;
                    }

                    iterations++;
                    hash_index++;
                    if (new_count <= hash_index)
                    {
                        hash_index = 0;
                    }
                }

                assert(found_slot);
            }
        }

        mem_free(old_data);
    }

    const char* ast_type_to_string(AST_Type* type)
    {
        assert(type);

        String_Builder sb;
        string_builder_init(&sb, 128);

        ast_type_to_string(type, &sb);

        char* result = string_builder_to_string(&sb);

        string_builder_free(&sb);

        return result;
    }

    void ast_type_to_string(AST_Type* type, String_Builder* string_builder)
    {
        assert(type);
        assert(string_builder);

        switch (type->kind)
        {
            case AST_TYPE_BASE:
            {
                string_builder_append(string_builder, type->name);
                break;
            }

            case AST_TYPE_POINTER:
            {
                // TODO: Temp mem
                string_builder_append(string_builder, "*");
                ast_type_to_string(type->pointer.base, string_builder);
                break;
            }

            case AST_TYPE_STRUCT:
            {
                if (type->name)
                {
                    string_builder_append(string_builder, "(struct ");
                    string_builder_append(string_builder, type->name);

                    if (type->aggregate_type.poly_from)
                    {
                        string_builder_append(string_builder, "(");
                        AST_Declaration* poly_decl = type->aggregate_type.poly_from;
                        auto agg_decl = poly_decl->aggregate_type.aggregate_decl;
                        for (uint64_t i = 0; i < BUF_LENGTH(agg_decl->poly_args); i++)
                        {
                            if (i > 0) string_builder_append(string_builder, ", ");
                            string_builder_append(string_builder, agg_decl->poly_args[i]->atom);
                        }
                        string_builder_append(string_builder, ")");
                    }

                    string_builder_append(string_builder, ")");
                }
                else
                {
                    string_builder_append(string_builder, "(anonymous_struct)");
                }
                break;
            }

            case AST_TYPE_UNION:
            {
                if (type->name)
                {
                    string_builder_append(string_builder, type->name);
                    string_builder_append(string_builder, "(union)");
                }
                else
                {
                    string_builder_append(string_builder, "(union) {");
                    auto members = type->aggregate_type.member_declarations;
                    for (uint64_t i = 0; i < BUF_LENGTH(members); i++)
                    {
                        if (i > 0)
                        {
                            string_builder_append(string_builder, ", ");
                        }
                        auto member = members[i];
                        assert(member->kind == AST_DECL_MUTABLE);
                        assert(member->mutable_decl.type);
                        ast_type_to_string(member->mutable_decl.type, string_builder);
                    }
                    string_builder_append(string_builder, "}");
                }
                break;
            }

            case AST_TYPE_ENUM:
            {
                if (type->name)
                {
                    string_builder_append(string_builder, type->name);
                    string_builder_append(string_builder, "(enum)");
                }
                else assert(false);
                break;
            }

            case AST_TYPE_STATIC_ARRAY:
            {
                string_builder_append(string_builder, "[");
                string_builder_append(string_builder, type->static_array.count);
                string_builder_append(string_builder, "]");
                ast_type_to_string(type->static_array.base, string_builder);
                break;
            }

            case AST_TYPE_FUNCTION:
            {
                string_builder_append(string_builder, "(");
                for (uint64_t i = 0 ; i < BUF_LENGTH(type->function.arg_types); i++)
                {
                    if (i > 0)
                    {
                        string_builder_append(string_builder, ", ");
                    }
                    AST_Type* arg_type = type->function.arg_types[i];
                    ast_type_to_string(arg_type, string_builder);
                }

                string_builder_append(string_builder, ") -> ");
                ast_type_to_string(type->function.return_type, string_builder);
                break;
            }

            default: assert(false);
        }
    }

    bool is_cmp_op(AST_Binop_Kind binop)
    {
        switch (binop)
        {
            case AST_BINOP_EQ:
            case AST_BINOP_LT:
            case AST_BINOP_LTEQ:
            case AST_BINOP_GT:
            case AST_BINOP_GTEQ:
            case AST_BINOP_NEQ:
            case AST_BINOP_AND_AND:
            case AST_BINOP_OR_OR:
                return true;

            default:
                return false;
        }
    }

    bool ast_type_is_aggregate(AST_Type* type)
    {
        assert(type);

        return type->kind == AST_TYPE_STRUCT ||
               type->kind == AST_TYPE_UNION ||
               type->kind == AST_TYPE_STATIC_ARRAY;
    }

    bool ast_type_is_mrv(AST_Type* type)
    {
        assert(type);

        return type->kind == AST_TYPE_STRUCT &&
               type->flags & AST_TYPE_FLAG_MRV;
    }
}
