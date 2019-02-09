#include "ast.h"

namespace Zodiac
{
    AST_Module* ast_module_new(Context* context, const char* module_name)
    {
        assert(context);
        assert(module_name);

        AST_Module* result = arena_alloc(context->arena, AST_Module);
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

    AST_Expression* ast_binary_expression_new(Context* context, File_Pos file_pos,
                                              AST_Expression* lhs, AST_Binop_Kind op, AST_Expression* rhs)
    {
        assert(context);
        assert(lhs);
        assert(rhs);

        AST_Expression* result = arena_alloc(context->arena, AST_Expression);
        result->kind = AST_EXPR_BINARY;
        result->file_pos = file_pos;

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

        AST_Expression* result = arena_alloc(context->arena, AST_Expression);
        result->kind = AST_EXPR_UNARY;
        result->file_pos = file_pos;

        result->unary.op = op;
        result->unary.operand = operand;

        return result;
    }

    AST_Expression* ast_ident_expression_new(Context* context, File_Pos file_pos, AST_Identifier* identifier)
    {
        assert(context);
        assert(identifier);

        AST_Expression* result = arena_alloc(context->arena, AST_Expression);
        result->kind = AST_EXPR_IDENTIFIER;
        result->file_pos = file_pos;

        result->identifier = identifier;

        return result;
    }

    AST_Expression* ast_call_expression_new(Context* context, File_Pos file_pos, AST_Identifier* identifier,
                                            BUF(AST_Expression*) arg_exprs)
    {
        assert(context);
        assert(identifier);

        AST_Expression* result = arena_alloc(context->arena, AST_Expression);
        result->kind = AST_EXPR_CALL;
        result->file_pos = file_pos;

        result->call.identifier = identifier;
        result->call.arg_expressions = arg_exprs;

        return result;
    }

    AST_Expression* ast_literal_expression_new(Context* context, File_Pos file_pos, uint64_t value)
    {
        assert(context);

        AST_Expression* result = arena_alloc(context->arena, AST_Expression);
        result->kind = AST_EXPR_LITERAL;
        result->file_pos = file_pos;

        result->literal.u64 = value;

        return result;
    }

    AST_Declaration* ast_function_declaration_new(Context* context, File_Pos file_pos,
                                                  AST_Identifier* identifier,
                                                  BUF(AST_Declaration*) args,
                                                  AST_Type_Spec* return_type_spec,
                                                  AST_Statement* body_block)
    {
        assert(context);
        assert(identifier);

        AST_Declaration* result = arena_alloc(context->arena, AST_Declaration);
        result->kind = AST_DECL_FUNC;
        result->file_pos = file_pos;
        result->identifier = identifier;

        result->function.args = args;
        result->function.return_type_spec = return_type_spec;
        result->function.body_block = body_block;

        return result;
    }

    AST_Declaration* ast_mutable_declaration_new(Context* context, File_Pos file_pos, AST_Identifier* identifier,
                                                 AST_Type_Spec* type_spec, AST_Expression* init_expr)
    {
        assert(context);
        assert(identifier);
        assert(type_spec || init_expr);

        AST_Declaration* result = arena_alloc(context->arena, AST_Declaration);
        result->kind = AST_DECL_MUTABLE;
        result->file_pos = file_pos;
        result->identifier = identifier;

        result->mutable_decl.type_spec = type_spec;
        result->mutable_decl.init_expression = init_expr;

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

    AST_Statement* ast_block_statement_new(Context* context, File_Pos file_pos, BUF(AST_Statement*) block_statements)
    {
        assert(context);

        AST_Statement* result = arena_alloc(context->arena, AST_Statement);
        result->kind = AST_STMT_BLOCK;
        result->file_pos = file_pos;
        result->block_statements = block_statements;

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

    AST_Type_Spec* ast_type_spec_new(Context* context, File_Pos file_pos, AST_Identifier* identifier)
    {
        assert(context);
        assert(identifier);

        AST_Type_Spec* result = arena_alloc(context->arena, AST_Type_Spec);
        result->file_pos = file_pos;
        result->identifier = identifier;

        return result;
    }
}
