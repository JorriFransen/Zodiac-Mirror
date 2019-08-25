#include "ast.h"

#include "builtin.h"
#include "const_interpreter.h"

#include <inttypes.h>

namespace Zodiac
{
    AST_Module* ast_module_new(Context* context, const char* module_name)
    {
        assert(context);
        assert(module_name);

        AST_Module* result = arena_alloc(context->arena, AST_Module);

		result->declarations = (AST_Declaration**)mem_alloc(sizeof(AST_Declaration*) * 128);
		result->declaration_count = 128;

        result->global_declarations = nullptr;
        result->module_scope = ast_scope_new(context, context->builtin_scope, result, true);
        result->entry_point = nullptr;
        result->module_name = module_name;
        result->import_decls = nullptr;
        result->import_modules = nullptr;
        result->resolved = false;
        result->gen_data = nullptr;

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

    AST_Directive* ast_directive_new(Context* context, AST_Directive_Kind kind, File_Pos file_pos)
    {
        assert(context);
        assert(kind != AST_DIREC_INVALID);

        AST_Directive* result = arena_alloc(context->arena, AST_Directive);
        result->kind = kind;
        result->file_pos = file_pos;

        return result;
    }

    AST_Expression* ast_expression_new(Context* context, File_Pos file_pos, AST_Expression_Kind kind)
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
        result->string_literal.atom = value;
        result->flags |= AST_EXPR_FLAG_LITERAL;
        return result;
    }

    AST_Expression* ast_integer_literal_expression_new(Context* context, File_Pos file_pos,
                                                       uint64_t value)
    {
        assert(context);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_INTEGER_LITERAL);

        result->integer_literal.u64 = value;
        result->flags |= AST_EXPR_FLAG_LITERAL;
        result->flags |= AST_EXPR_FLAG_CONST;

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

    AST_Aggregate_Declaration* ast_aggregate_declaration_new(Context* context, File_Pos file_pos,
                                                             BUF(AST_Declaration*) members,
                                                             BUF(AST_Overload_Directive) overloads)
    {
        assert(context);

        auto result = arena_alloc(context->arena, AST_Aggregate_Declaration);

        result->file_pos = file_pos;
        result->members = members;
        result->overload_directives = overloads;

        return result;
    }

    AST_Declaration* ast_declaration_new(Context* context, File_Pos file_pos,
                                         AST_Declaration_Kind kind,
                                         AST_Declaration_Location location,
                                         AST_Identifier* identifier, AST_Directive* directive)
    {
        assert(context);

        AST_Declaration* result = arena_alloc(context->arena, AST_Declaration);
        result->file_pos = file_pos;
        result->kind = kind;
        result->location = location;
        result->identifier = identifier;
        result->directive = directive;
        result->gen_data = nullptr;

        return result;
    }

    AST_Declaration* ast_function_declaration_new(Context* context, File_Pos file_pos,
                                                  AST_Identifier* identifier,
                                                  BUF(AST_Declaration*) args,
                                                  bool is_vararg, bool is_poly,
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
        result->function.overloads = nullptr;
        if (is_poly)
        {
            result->flags |= AST_DECL_FLAG_FUNC_POLY;
        }
        result->function.poly_count = 0;
        result->function.poly_instances = nullptr;

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
        assert(identifier);
        assert(type_spec || init_expr);
        assert(location != AST_DECL_LOC_INVALID);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_MUTABLE,
                                                      location,
                                                      identifier, nullptr);

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
                                                      location,
                                                      identifier, nullptr);

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
                                                BUF(AST_Identifier*) parameters,
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
        result->aggregate_type.parameter_idents = parameters;
        result->aggregate_type.poly_count = 0;
        result->aggregate_type.poly_instances = nullptr;
        result->aggregate_type.scope = scope;

        return result;
    }

    AST_Declaration* ast_union_declaration_new(Context* context, File_Pos file_pos,
                                               AST_Identifier* identifier,
                                               AST_Aggregate_Declaration* aggregate_decl,
                                               BUF(AST_Identifier*) parameters,
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
        result->aggregate_type.parameter_idents = parameters;
        result->aggregate_type.poly_count = 0;
        result->aggregate_type.poly_instances = nullptr;
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

    AST_Statement* ast_while_statement_new(Context* context, File_Pos file_pos,
                                           AST_Expression* cond_expr,
        AST_Statement* body_stmt)
    {
        assert(context);
        assert(cond_expr);
        assert(body_stmt);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_WHILE;
        result->file_pos = file_pos;

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
        result->aggregate_type.poly_from = nullptr;
        result->aggregate_type.poly_types = nullptr;
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
        result->aggregate_type.poly_from = nullptr;
        result->aggregate_type.poly_types = nullptr;
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
                                    AST_Type* return_type, const char* original_name)
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
        result->function.poly_from = nullptr;
        result->function.original_name = nullptr;

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
                                                BUF(AST_Type_Spec*) poly_args/*=nullptr*/)
    {
        assert(context);
        assert(identifier);

        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_IDENT);
        result->identifier.identifier = identifier;
        result->identifier.poly_args = poly_args;

        if (poly_args)
        {
            result->flags |= AST_TYPE_SPEC_FLAG_POLY;
        }

        // for (uint64_t i = 0; i < BUF_LENGTH(poly_args); i++)
        // {
        //     auto poly_arg = poly_args[i];
        //     if (poly_arg->flags & AST_TYPE_SPEC_FLAG_POLY)
        //     {
        //         result->flags |= AST_TYPE_SPEC_FLAG_POLY;
        //         break;
        //     }
        // }

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

        if (member_type_spec->flags & AST_TYPE_SPEC_FLAG_POLY)
        {
            result->flags |= AST_TYPE_SPEC_FLAG_POLY;
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

        if (base_type_spec->flags & AST_TYPE_SPEC_FLAG_POLY)
        {
            result->flags |= AST_TYPE_SPEC_FLAG_POLY;
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

        if (base_type_spec->flags & AST_TYPE_SPEC_FLAG_POLY)
        {
            result->flags |= AST_TYPE_SPEC_FLAG_POLY;
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

    AST_Type_Spec* ast_type_spec_from_type_new(Context* context, File_Pos file_pos, AST_Type* type)
    {
        assert(context);
        assert(type);

        switch (type->kind)
        {
            case AST_TYPE_POINTER:
            {
                auto base_type_spec = ast_type_spec_from_type_new(context, file_pos,
                                                                  type->pointer.base);
                return ast_type_spec_pointer_new(context, file_pos, base_type_spec);
            }

            case AST_TYPE_STRUCT:
            {
                assert(type->name);
                BUF(AST_Type_Spec*) poly_args = nullptr;
                AST_Identifier* identifier = nullptr;
                if (type->aggregate_type.poly_from)
                {
                    assert(type->aggregate_type.poly_from->identifier);
                    identifier =
                        ast_identifier_new(context,
                                           type->aggregate_type.poly_from->identifier->atom,
                                           file_pos);
                }
                else
                {
                    auto ident_atom = atom_get(context->atom_table, type->name);
                    identifier = ast_identifier_new(context, ident_atom, file_pos);
                }
                assert(identifier);
                return ast_type_spec_identifier_new(context, file_pos, identifier, poly_args);
            }

            case AST_TYPE_BASE:
            {
                assert(type->name);
                Atom ident_atom = atom_get(context->atom_table, type->name);
                AST_Identifier* identifier = ast_identifier_new(context, ident_atom, file_pos);
                return ast_type_spec_identifier_new(context, file_pos, identifier);
            }

            case AST_TYPE_ENUM:
            {
                assert(type->name);
                AST_Identifier* identifier = ast_identifier_new(context,
                                                                atom_get(context->atom_table,
                                                                         type->name),
                                                                file_pos);

                return ast_type_spec_identifier_new(context, file_pos, identifier);
            }

            default: assert(false);
        }

		assert(false);
		return nullptr;
    }

    AST_Scope* ast_scope_new(Context* context, AST_Scope* parent_scope, AST_Module* module,
		                     bool is_module_scope)

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

        uint64_t hash = ast_get_pointer_type_hash(base_type);
        uint64_t hash_index = hash & (context->type_count - 1);

        uint64_t iterations = 0;
        bool found_free_slot = false;
        while (iterations < context->type_count)
        {

            AST_Type* ex_type = context->type_hash[hash_index];
            if (ex_type)
            {
                if (ex_type->kind == AST_TYPE_POINTER  &&
                    ex_type->pointer.base == base_type)
                {
                    return ex_type;
                }
            }
            else
            {
                found_free_slot = true;
                break;
            }

            iterations++;
            hash_index++;
            if (context->type_count <= hash_index)
            {
                hash_index = 0;
            }
        }

		if (found_free_slot)
		{
			AST_Type* pointer_type = ast_type_pointer_new(context, base_type);
			context->type_hash[hash_index] = pointer_type;

			return pointer_type;
		}
		else
		{
			grow_type_hash(context);
			return ast_find_or_create_pointer_type(context, base_type);
		}
    }

    AST_Type* ast_find_or_create_array_type(Context* context, AST_Type* base_type,
                                            AST_Expression* count_expr, AST_Scope* scope)
    {
        assert(context);
        assert(base_type);
        assert(count_expr);
        assert(count_expr->flags & AST_EXPR_FLAG_CONST);
        assert(count_expr->type == Builtin::type_int);
        assert(scope);

        int64_t count_value = const_interpret_s64_expression(context, count_expr, scope);

        return ast_find_or_create_array_type(context, base_type, count_value);
    }

    AST_Type* ast_find_or_create_array_type(Context* context, AST_Type* base_type,
                                            uint64_t count)
    {
        assert(context);
        assert(base_type);

        uint64_t hash = ast_get_static_array_type_hash(base_type, count);
        uint64_t hash_index = hash & (context->type_count - 1);

        uint64_t iterations = 0;
        bool found_free_slot = false;
        while (iterations < context->type_count)
        {
            AST_Type* ex_type = context->type_hash[hash_index];
            if (ex_type)
            {
                if (ex_type->kind == AST_TYPE_STATIC_ARRAY &&
                    ex_type->static_array.base == base_type &&
                    ex_type->static_array.count == count)
                {
                    return ex_type;
                }
            }
            else
            {
                found_free_slot = true;
            }

            iterations++;
            hash_index++;
            if (context->type_count <= hash_index)
            {
                hash_index = 0;
            }
        }

        assert(found_free_slot);

        AST_Type* array_type = ast_type_static_array_new(context, base_type, count);
        context->type_hash[hash_index] = array_type;

        return array_type;
    }

	AST_Type* ast_find_or_create_function_type(Context* context, bool is_vararg,
                                               BUF(AST_Type*) arg_types,
                                               AST_Type* return_type, const char* original_name)
	{
		assert(context);
        assert(return_type);
        assert(original_name);

        uint64_t hash = ast_get_function_type_hash(is_vararg, arg_types, return_type);
        uint64_t hash_index = hash & (context->type_count - 1);

        uint64_t iterations = 0;
        bool found_slot = false;
        while (iterations < context->type_count)
        {
            AST_Type* ex_type = context->type_hash[hash_index];
            if (ex_type)
            {
                if (ex_type->kind == AST_TYPE_FUNCTION)
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
            }
            else
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
            AST_Type* result = ast_type_function_new(context, is_vararg, arg_types, return_type,
                                                     original_name);
            context->type_hash[hash_index] = result;
            return result;
        }
        else
        {
            ast_grow_type_hash(context);
            return ast_find_or_create_function_type(context, is_vararg, arg_types, return_type,
                                                    original_name);
        }

	}

    uint64_t ast_get_type_hash(AST_Type* type)
    {
        assert(type);

        uint64_t kind_hash = hash_pointer((void*)type->kind);
        uint64_t flag_hash = hash_pointer((void*)type->flags);

        uint64_t base_hash = hash_mix(kind_hash, flag_hash);
        base_hash = hash_mix(base_hash, hash_pointer((void*)type->bit_size));

        if (type->name)
        {
            base_hash = hash_mix(base_hash, hash_string(type->name));
        }

        switch (type->kind)
        {
            case AST_TYPE_BASE:
            case AST_TYPE_ENUM:
            {
                // No need to do anything else
                break;
            }

            case AST_TYPE_POINTER:
            {
                base_hash = ast_get_pointer_type_hash(type->pointer.base);
                break;
            }

            case AST_TYPE_STRUCT:
            case AST_TYPE_UNION:
            {
                base_hash = hash_mix(base_hash,
                                     BUF_LENGTH(type->aggregate_type.member_declarations));

                // for (uint64_t i = 0; i < BUF_LENGTH(type->aggregate_type.member_declarations); i++)
                // {
                //     AST_Declaration* member_decl = type->aggregate_type.member_declarations[i];
                //     assert(member_decl->kind == AST_DECL_MUTABLE);
                //     base_hash = hash_mix(base_hash,
                //                          ast_get_type_hash(member_decl->mutable_decl.type));
                // }

                break;
            }

            case AST_TYPE_FUNCTION:
            {
                assert(type->function.return_type);
                base_hash = hash_mix(base_hash, ast_get_type_hash(type->function.return_type));

                for (uint64_t i = 0; i < BUF_LENGTH(type->function.arg_types); i++)
                {
                    AST_Type* arg_type = type->function.arg_types[i];
                    base_hash = hash_mix(base_hash, ast_get_type_hash(arg_type));
                }
                break;
            }

            default: assert(false);
        }

        return base_hash;
    }
    // uint64_t ast_get_type_hash(AST_Type* type)
    // {
    //     assert(type);

    //     uint64_t kind_hash = hash_pointer((void*)type->kind);
    //     uint64_t flag_hash = hash_pointer((void*)type->flags);

    //     uint64_t base_hash = hash_mix(kind_hash, flag_hash);

    //     if (type->name)
    //     {
    //         base_hash = hash_mix(base_hash, hash_string(type->name));
    //     }

    //     switch (type->kind)
    //     {
    //         case AST_TYPE_POINTER:
    //         {
    //             return ast_get_pointer_type_hash(type->pointer.base);
    //             break;
    //         }

    //         case AST_TYPE_STATIC_ARRAY:
    //         {
    //             uint64_t count_hash = hash_pointer((void*)type->static_array.count);
    //             return hash_mix(base_hash, count_hash);
    //             break;
    //         };

    //         case AST_TYPE_FUNCTION:
    //         {
    //             return ast_get_function_type_hash((type->flags & AST_TYPE_FLAG_FUNC_VARARG),
    //                                               type->function.arg_types,
    //                                               type->function.return_type);
    //             break;
    //         }

    //         case AST_TYPE_STRUCT:
    //         case AST_TYPE_UNION:
    //         {
    //             auto member_decls = type->aggregate_type.member_declarations;
    //             for (uint64_t i = 0; i < BUF_LENGTH(member_decls); i++)
    //             {
    //                 AST_Declaration* member_decl = member_decls[i];
    //                 assert(member_decl->kind == AST_DECL_MUTABLE);
    //                 uint64_t member_hash = 0;
    //                 if (!member_decl->mutable_decl.type ||
    //                     (member_decl->mutable_decl.type->kind == AST_TYPE_POINTER &&
    //                      member_decl->mutable_decl.type->pointer.base == type))
    //                 {
    //                     assert(member_decl->mutable_decl.type_spec->kind == AST_TYPE_SPEC_POINTER);
    //                     assert(member_decl->mutable_decl.type_spec->pointer.base->kind ==
    //                            AST_TYPE_SPEC_IDENT);
    //                     const char* member_type_str =
    //                         member_decl->mutable_decl.type_spec->pointer.base->identifier.identifier->atom.data;
    //                     const char* type_str = type->name;
    //                     assert(strcmp(member_type_str, type_str) == 0);
    //                 }
    //                 else
    //                 {
    //                     if (member_decl->mutable_decl.type->kind == AST_TYPE_POINTER &&
	// 						member_decl->mutable_decl.type->pointer.base->name)
    //                     {
    //                         if (member_decl->mutable_decl.type->pointer.base == type ||
    //                             (strcmp(type->name, member_decl->mutable_decl.type->pointer.base->name) == 0))
    //                         {
    //                             member_hash = hash_string("pointer_to_self_hasd;fsadjf;aksjf");
    //                         }
    //                     }
    //                     else
    //                     {
    //                         member_hash = ast_get_type_hash(member_decl->mutable_decl.type);
    //                     }
    //                 }
    //                 base_hash = hash_mix(base_hash, member_hash);
    //             }
    //             return base_hash;
    //             break;
    //         }

    //         case AST_TYPE_BASE:
    //         {
    //             return base_hash;
    //             break;
    //         }

    //         case AST_TYPE_ENUM:
    //         {
    //             return hash_mix(base_hash, ast_get_type_hash(type->aggregate_type.base_type));
    //             break;
    //         }

    //         default: assert(false);
    //     }

    //     assert(false);
    //     return 0;
    // }

    uint64_t ast_get_pointer_type_hash(AST_Type* base_type)
    {
        assert(base_type);

        const char* pointer_type_string = "pointer_typesa;fj dofhoqw9euf";
        return hash_mix(ast_get_type_hash(base_type), hash_string(pointer_type_string));
    }

    uint64_t ast_get_static_array_type_hash(AST_Type* base_type, uint64_t count)
    {
        assert(base_type);

        uint64_t base_hash = ast_get_type_hash(base_type);
        uint64_t count_hash = hash_pointer((void*)count);
        uint64_t hash = hash_mix(base_hash, count_hash);

        const char* static_array_type_string = "static_array_typekqn2o9xb376c";
        uint64_t string_hash = hash_string(static_array_type_string);

        return hash_mix(hash, string_hash);
    }

    uint64_t ast_get_function_type_hash(bool is_vararg, BUF(AST_Type*) arg_types, AST_Type* return_type)
    {
        assert(return_type);

        uint64_t hash = ast_get_type_hash(return_type);

        if (is_vararg)
        {
            hash = hash_mix(hash, 0xaf2fdfe1f7f8fc3f);
        }

        for (uint64_t i = 0; i < BUF_LENGTH(arg_types); i++)
        {
            hash = hash_mix(hash, ast_get_type_hash(arg_types[i]));
        }

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
                uint64_t hash = ast_get_type_hash(old_type);
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
                    string_builder_append(string_builder, type->name);
                    string_builder_append(string_builder, "(struct)");
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
                    string_builder_append(string_builder, "(anonymous_union)");
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
}
