#include "ast.h"

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

    AST_Expression* ast_call_expression_new(Context* context, File_Pos file_pos, AST_Identifier* identifier,
                                            BUF(AST_Expression*) arg_exprs)
    {
        assert(context);
        assert(identifier);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_CALL);

        result->call.identifier = identifier;
        result->call.arg_expressions = arg_exprs;

        return result;
    }

    AST_Expression* ast_literal_expression_new(Context* context, File_Pos file_pos, uint64_t value)
    {
        assert(context);

        auto result = ast_expression_new(context, file_pos, AST_EXPR_LITERAL);

        result->literal.u64 = value;

        return result;
    }

    AST_Declaration* ast_function_declaration_new(Context* context, File_Pos file_pos,
                                                  AST_Identifier* identifier,
                                                  BUF(AST_Declaration*) args,
                                                  AST_Type_Spec* return_type_spec,
                                                  AST_Statement* body_block,
                                                  AST_Scope* argument_scope)
    {
        assert(context);
        assert(identifier);
        assert(argument_scope);

        AST_Declaration* result = arena_alloc(context->arena, AST_Declaration);
        result->kind = AST_DECL_FUNC;
        result->file_pos = file_pos;
        result->identifier = identifier;
        result->directive = nullptr;
        result->gen_data = nullptr;

        result->function.args = args;
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

    AST_Declaration* ast_mutable_declaration_new(Context* context, File_Pos file_pos, AST_Identifier* identifier,
                                                 AST_Type_Spec* type_spec, AST_Expression* init_expr,
                                                 AST_Declaration_Location location)
    {
        assert(context);
        assert(identifier);
        assert(type_spec || init_expr);
        assert(location != AST_DECL_LOC_INVALID);

        AST_Declaration* result = arena_alloc(context->arena, AST_Declaration);
        result->kind = AST_DECL_MUTABLE;
        result->file_pos = file_pos;
        result->identifier = identifier;
        result->directive = nullptr;
        result->location = location;
        result->gen_data = nullptr;

        result->mutable_decl.type_spec = type_spec;
        result->mutable_decl.init_expression = init_expr;

        return result;
    }

    AST_Declaration* ast_type_declaration_new(Context* context, File_Pos file_pos, AST_Type* type,
                                              AST_Identifier* identifier)
    {
        assert(context);
        assert(type);
        assert(identifier);

        AST_Declaration* result = arena_alloc(context->arena, AST_Declaration);
        result->kind = AST_DECL_TYPE;
        result->file_pos = file_pos;
        result->identifier = identifier;
        result->directive = nullptr;
        result->gen_data = nullptr;

        result->type.type = type;

        return result;
    }

    AST_Declaration* ast_dyn_link_declaration_new(Context* context, File_Pos file_pos, Atom link_name,
                                                  AST_Declaration_Location location)
    {
        assert(context);

        AST_Declaration* result = arena_alloc(context->arena, AST_Declaration);
        result->kind = AST_DECL_DYN_LINK;
        result->location = location;
        result->file_pos = file_pos;

        result->dyn_link_name = link_name;

        return result;
    }

    AST_Statement* ast_declaration_statement_new(Context* context, File_Pos file_pos, AST_Declaration* declaration)
    {
        assert(context);
        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_DECLARATION;
        result->file_pos = file_pos;
        result->declaration = declaration;

        return result;
    }

    AST_Statement* ast_block_statement_new(Context* context, File_Pos file_pos, BUF(AST_Statement*) block_statements,
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

    AST_Statement* ast_return_statement_new(Context* context, File_Pos file_pos, AST_Expression* return_expr)
    {
        assert(context);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_RETURN;
        result->file_pos = file_pos;

        result->return_expression = return_expr;

        return result;
    }

    AST_Statement* ast_if_statement_new(Context* context, File_Pos file_pos, AST_Expression* cond_expr,
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

    AST_Statement* ast_assign_statement_new(Context* context, File_Pos file_pos, AST_Identifier* identifier,
                                            AST_Expression* expression)
    {
        assert(context);
        assert(identifier);
        assert(expression);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_ASSIGN;
        result->file_pos = file_pos;

        result->assign.identifier = identifier;
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

    AST_Type* ast_type_new(Context* context, AST_Type_Kind kind, AST_Type_Flags type_flags)
    {
        assert(context);

        AST_Type* result = arena_alloc(context->arena, AST_Type);
        result->kind = kind;
        result->flags = type_flags;

        return result;
    }

    AST_Type* ast_type_base_new(Context* context, AST_Type_Flags type_flags, uint64_t bit_size)
    {
        assert(context);
        assert(bit_size % 8 == 0);

        AST_Type* result = ast_type_new(context, AST_TYPE_BASE, type_flags);
        result->base.bit_size = bit_size;

        return result;
    }

    AST_Type* ast_type_pointer_new(Context* context, AST_Type* base_type)
    {
        assert(context);
        assert(base_type);

        AST_Type* result = ast_type_new(context, AST_TYPE_POINTER, AST_TYPE_FLAG_NONE);
        result->base_type = base_type;

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

    AST_Type_Spec* ast_type_spec_identifier_new(Context* context, File_Pos file_pos, AST_Identifier* identifier)
    {
        assert(context);
        assert(identifier);

        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_IDENT);
        result->identifier = identifier;

        return result;
    }

    AST_Type_Spec* ast_type_spec_pointer_new(Context* context, File_Pos file_pos, AST_Type_Spec* base_type_spec)
    {
        assert(context);
        assert(base_type_spec);

        AST_Type_Spec* result = ast_type_spec_new(context, file_pos, AST_TYPE_SPEC_POINTER);
        result->base_type_spec = base_type_spec;

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

    AST_Type* ast_find_or_create_pointer_type(Context* context, AST_Module* module, AST_Type* base_type)
    {
        assert(context);
        assert(module);
        assert(base_type);

        for (uint64_t i = 0; i < BUF_LENGTH(module->types); i++)
        {
            AST_Type* ex_type = module->types[i];
            if (ex_type->kind == AST_TYPE_POINTER)
            {
                if (ex_type->base_type == base_type)
                {
                    return ex_type;
                }
            }
        }

        AST_Type* pointer_type = ast_type_pointer_new(context, base_type);
        BUF_PUSH(module->types, pointer_type);
        return pointer_type;
    }
}
