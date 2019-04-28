#include "ast.h"

#include "builtin.h"

namespace Zodiac
{
    AST_Module* ast_module_new(Context* context, const char* module_name)
    {
        assert(context);
        assert(module_name);

        AST_Module* result = arena_alloc(context->arena, AST_Module);
        result->global_declarations = nullptr;
        result->types = nullptr;
        result->module_scope = ast_scope_new(context, nullptr);
        result->entry_point = nullptr;
        result->module_name = module_name;
        result->import_modules = nullptr;
        result->gen_data = nullptr;

        for (uint64_t i = 0; i < BUF_LENGTH(context->builtin_decls); i++)
        {
            AST_Declaration* builtin_decl = context->builtin_decls[i];
            BUF_PUSH(result->module_scope->declarations, builtin_decl);
            if (builtin_decl->location == AST_DECL_LOC_GLOBAL)
            {
                BUF_PUSH(result->global_declarations, builtin_decl);
            }
        }

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
        result->is_const = false;

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

        return result;
    }

    AST_Expression* ast_ident_expression_new(Context* context, File_Pos file_pos, AST_Identifier* identifier)
    {
        assert(context);
        assert(identifier);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_IDENTIFIER);

        result->identifier = identifier;

        return result;
    }

    AST_Expression* ast_call_expression_new(Context* context, File_Pos file_pos, AST_Expression* ident_expr,
        BUF(AST_Expression*) arg_exprs)
    {
        assert(context);
        assert(ident_expr);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_CALL);

        result->call.ident_expression = ident_expr;
        result->call.arg_expressions = arg_exprs;

        return result;
    }

    AST_Expression* ast_subscript_expression_new(Context* context, File_Pos file_pos, AST_Expression* base_expression,
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

    AST_Expression* ast_boolean_literal_expression_new(Context* context, File_Pos file_pos, bool value)
    {
        assert(context);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_BOOL_LITERAL);
        result->bool_literal.boolean = value;
        return result;
    }

    AST_Expression* ast_string_literal_expression_new(Context* context, File_Pos file_pos, Atom value)
    {
        assert(context);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_STRING_LITERAL);
        result->string_literal.atom = value;
        return result;
    }

    AST_Expression* ast_integer_literal_expression_new(Context* context, File_Pos file_pos,
                                                       uint64_t value)
    {
        assert(context);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_INTEGER_LITERAL);

        result->integer_literal.u64 = value;

        return result;
    }

    AST_Expression* ast_float_literal_expression_new(Context* context, File_Pos file_pos,
                                                     double value)
    {
        assert(context);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_FLOAT_LITERAL);

        result->float_literal.r64 = value;

        return result;
    }

    AST_Expression* ast_character_literal_expression_new(Context* context, File_Pos file_pos,
                                                         char value)
    {
        assert(context);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_CHAR_LITERAL);
        result->character_literal.c = value;
        return result;
    }

    AST_Expression* ast_compound_literal_expression_new(Context* context, File_Pos file_pos,
                                                        BUF(AST_Expression*) expressions)
    {
        assert(context);
        auto result = ast_expression_new(context, file_pos, AST_EXPR_COMPOUND_LITERAL);
        result->compound_literal.expressions = expressions;
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

    AST_Declaration* ast_declaration_new(Context* context, File_Pos file_pos,
                                         AST_Declaration_Kind kind,
                                         AST_Declaration_Location location,
                                         AST_Identifier* identifier, AST_Directive* directive,
                                         bool constant)
    {
        assert(context);

        AST_Declaration* result = arena_alloc(context->arena, AST_Declaration);
        result->file_pos = file_pos;
        result->kind = kind;
        result->location = location;
        result->identifier = identifier;
        result->directive = directive;
        result->constant = constant;
        result->gen_data = nullptr;

        return result;
    }

    AST_Declaration* ast_function_declaration_new(Context* context, File_Pos file_pos,
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
                                                      identifier, nullptr, true);

        result->function.args = args;
        result->function.is_vararg = is_vararg;
        result->function.locals = nullptr;
        result->function.return_type_spec = return_type_spec;
        result->function.return_type = nullptr;
        result->function.inferred_return_type = nullptr;
        result->function.body_block = body_block;

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
                                                      identifier, nullptr, false);

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
        assert(init_expr);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_CONSTANT_VAR,
                                                      location,
                                                      identifier, nullptr, true);

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
                                                      identifier, nullptr, true);

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
                                                      nullptr, nullptr, false);

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
                                                      nullptr, nullptr, true);

        result->static_if.cond_expr = cond_expr;
        result->static_if.then_declaration = then_declaration;
        result->static_if.else_declaration = else_declaration;

        return result;
    }

    AST_Declaration* ast_block_declaration_new(Context* context, File_Pos file_pos,
                                               BUF(AST_Declaration*) block_decls)
    {
        assert(context);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_BLOCK,
                                                      AST_DECL_LOC_GLOBAL,
                                                      nullptr, nullptr, false);

        result->block.decls = block_decls;

        return result;
    }

    AST_Declaration* ast_static_assert_declaration_new(Context* context, File_Pos file_pos,
        AST_Expression* assert_expr)
    {
        assert(context);
        assert(assert_expr);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_STATIC_ASSERT,
                                                      AST_DECL_LOC_GLOBAL, nullptr, nullptr, true);
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
                                                      AST_DECL_LOC_GLOBAL, identifier,
                                                      nullptr, true);
        result->import.module_identifier = import_module_identifier;
        return result;
    }

    AST_Declaration* ast_struct_declaration_new(Context* context, File_Pos file_pos,
                                                AST_Identifier* identifier,
                                                BUF(AST_Declaration*) member_decls)
    {
        assert(context);
        assert(identifier);
        assert(member_decls);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_AGGREGATE_TYPE,
                                                      AST_DECL_LOC_GLOBAL, identifier,
                                                      nullptr, true);
        result->aggregate_type.kind = AST_AGG_DECL_STRUCT;
        result->aggregate_type.type = nullptr;
        result->aggregate_type.aggregate_declarations = member_decls;

        return result;
    }

    AST_Declaration* ast_enum_declaration_new(Context* context, File_Pos file_pos,
                                              AST_Identifier* identifier,
                                              BUF(AST_Enum_Member_Decl*) member_decls)
    {
        assert(context);
        assert(identifier);
        assert(member_decls);

        AST_Declaration* result = ast_declaration_new(context, file_pos, AST_DECL_ENUM_TYPE,
                                                      AST_DECL_LOC_GLOBAL, identifier,
                                                      nullptr, true);

        result->enum_decl.members = member_decls;
        result->enum_decl.type = nullptr;

        return result;
    }

    AST_Enum_Member_Decl* ast_enum_member_decl_new(Context* context, File_Pos file_pos,
                                                   AST_Identifier* identifier,
                                                   AST_Expression* value_expression)
    {
        assert(context);
        assert(identifier);

        if (value_expression)
        {
            assert(value_expression->kind == AST_EXPR_IDENTIFIER ||
                   value_expression->kind == AST_EXPR_INTEGER_LITERAL);
            assert(false);
        }

        AST_Enum_Member_Decl* emd = arena_alloc(context->arena, AST_Enum_Member_Decl);
        emd->file_pos = file_pos;
        emd->identifier = identifier;
        emd->value_expression = value_expression;

        return emd;
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

    AST_Type* ast_type_new(Context* context, AST_Type_Kind kind, AST_Type_Flags type_flags,
                           uint64_t bit_size)
    {
        assert(context);

        AST_Type* result = arena_alloc(context->arena, AST_Type);
        result->kind = kind;
        result->flags = type_flags;
        result->bit_size = bit_size;

        return result;
    }

    AST_Type* ast_type_base_new(Context* context, AST_Type_Flags type_flags, uint64_t bit_size)
    {
        assert(context);
        assert(bit_size % 8 == 0);

        AST_Type* result = ast_type_new(context, AST_TYPE_BASE, type_flags, bit_size);

        return result;
    }

    AST_Type* ast_type_pointer_new(Context* context, AST_Type* base_type)
    {
        assert(context);
        assert(base_type);

        AST_Type* result = ast_type_new(context, AST_TYPE_POINTER, AST_TYPE_FLAG_NONE,
                                        Builtin::pointer_size);
        result->pointer.base = base_type;

        return result;
    }

    AST_Type* ast_type_static_array_new(Context* context, AST_Type* base_type, uint64_t count)
    {
        assert(context);
        assert(base_type);

        assert(base_type->bit_size);

        AST_Type* result = ast_type_new(context, AST_TYPE_STATIC_ARRAY, AST_TYPE_FLAG_NONE,
                                        base_type->bit_size * count);
        result->static_array.base = base_type;
        result->static_array.count = count;

        return result;
    }

    AST_Type* ast_type_struct_new(Context* context, BUF(AST_Declaration*) member_declarations,
                                  uint64_t bit_size)
    {
        assert(context);
        assert(member_declarations);

        AST_Type* result = ast_type_new(context, AST_TYPE_STRUCT, AST_TYPE_FLAG_NONE, bit_size);
        result->aggregate_type.member_declarations = member_declarations;

        return result;
    }

    AST_Type* ast_type_enum_new(Context* context, BUF(AST_Enum_Member_Decl*) member_decls,
                                AST_Type* base_type)
    {
        assert(context);
        assert(member_decls);
        assert(base_type);
        assert(base_type->bit_size);
        assert(base_type->flags & AST_TYPE_FLAG_INT);

        AST_Type* result = ast_type_new(context, AST_TYPE_ENUM, AST_TYPE_FLAG_NONE,
                                        base_type->bit_size);
        result->enum_type.member_declarations = member_decls;
        result->enum_type.base_type = base_type;

        return result;
    }

    AST_Type_Spec* ast_type_spec_new(Context* context, File_Pos file_pos, AST_Type_Spec_Kind kind)
    {
        assert(context);

        AST_Type_Spec* result = arena_alloc(context->arena, AST_Type_Spec);
        result->kind = kind;
        result->file_pos = file_pos;

        return result;
    }

    AST_Type_Spec* ast_type_spec_identifier_new(Context* context, File_Pos file_pos,
                                                AST_Identifier* identifier)
    {
        assert(context);
        assert(identifier);

        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_IDENT);
        result->identifier = identifier;

        return result;
    }

    AST_Type_Spec* ast_type_spec_pointer_new(Context* context, File_Pos file_pos,
                                             AST_Type_Spec* base_type_spec)
    {
        assert(context);
        assert(base_type_spec);

        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_POINTER);
        result->pointer.base = base_type_spec;

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

        return result;
    }

    AST_Scope* ast_scope_new(Context* context, AST_Scope* parent_scope)
    {
        assert(context);

        AST_Scope* result = arena_alloc(context->arena, AST_Scope);
        result->parent = parent_scope;
        result->declarations = nullptr;

        return result;
    }

    AST_Type* ast_find_or_create_pointer_type(Context* context, AST_Module* module,
                                              AST_Type* base_type)
    {
        assert(context);
        assert(module);
        assert(base_type);

        for (uint64_t i = 0; i < BUF_LENGTH(module->types); i++)
        {
            AST_Type* ex_type = module->types[i];
            if (ex_type->kind == AST_TYPE_POINTER)
            {
                if (ex_type->pointer.base == base_type)
                {
                    return ex_type;
                }
            }
        }

        AST_Type* pointer_type = ast_type_pointer_new(context, base_type);
        BUF_PUSH(module->types, pointer_type);
        return pointer_type;
    }

    AST_Type* ast_find_or_create_array_type(Context* context, AST_Module* module,
                                            AST_Type* base_type, AST_Expression* count_expr)
    {
        assert(context);
        assert(module);
        assert(base_type);
        assert(count_expr);
        assert(count_expr->kind == AST_EXPR_INTEGER_LITERAL);

        return ast_find_or_create_array_type(context, module, base_type,
                                             count_expr->integer_literal.u64);
    }

    AST_Type* ast_find_or_create_array_type(Context* context, AST_Module* module,
                                            AST_Type* base_type, uint64_t count)
    {
        assert(context);
        assert(module);
        assert(base_type);

        for (uint64_t i = 0; i < BUF_LENGTH(module->types); i++)
        {
            AST_Type* ex_type = module->types[i];
            if (ex_type->kind == AST_TYPE_STATIC_ARRAY &&
                ex_type->static_array.base == base_type &&\
                ex_type->static_array.count == count)
            {
                return ex_type;
            }
        }

        AST_Type* array_type = ast_type_static_array_new(context, base_type, count);
        BUF_PUSH(module->types, array_type);
        return array_type;
    }
}
