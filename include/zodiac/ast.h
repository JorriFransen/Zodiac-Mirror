#pragma once

#include "zodiac.h"
#include "file_pos.h"

namespace Zodiac
{
    struct AST_Module;
    struct AST_Identifier;
    struct AST_Expression;
    struct AST_Statement;
    struct AST_Declaration;
    struct AST_Type;
    struct AST_Type_Spec;

    struct AST_Module
    {
        BUF(AST_Declaration*) global_declarations = nullptr;

        const char* module_name = nullptr;
    };

    struct AST_Identifier
    {
        File_Pos file_pos = {};
        Atom atom = {};
    };

    enum AST_Expression_Kind
    {
        AST_EXPR_BINARY,
        AST_EXPR_UNARY,
        AST_EXPR_IDENTIFIER,
        AST_EXPR_CALL,
        AST_EXPR_LITERAL,
    };

    enum AST_Binop_Kind
    {
        AST_BINOP_ADD,
        AST_BINOP_SUB,
        AST_BINOP_MUL,
        AST_BINOP_DIV,
    };

    enum AST_Unop_Kind
    {
        AST_UNOP_MINUS,
    };

    struct AST_Expression
    {
        AST_Expression_Kind kind;
        File_Pos file_pos;

        union
        {
            AST_Identifier* identifier;

            struct
            {
                AST_Binop_Kind op;
                AST_Expression* lhs;
                AST_Expression* rhs;
            } binary;

            struct
            {
                AST_Unop_Kind op;
                AST_Expression* operand;
            } unary;

            struct
            {
                AST_Identifier* identifier;
                BUF(AST_Expression*) arg_expressions;
            } call;

            struct
            {
                uint64_t u64;
            } literal;
        };
    };

    enum AST_Statement_Kind
    {
        AST_STMT_DECLARATION,
        AST_STMT_RETURN,
        AST_STMT_BLOCK,
    };

    struct AST_Statement
    {
        AST_Statement_Kind kind;
        File_Pos file_pos = {};

        union
        {
            AST_Declaration* declaration;
            AST_Expression* return_expression;
            BUF(AST_Statement*) block_statements;
        };
    };

    enum AST_Declaration_Kind
    {
        AST_DECL_FUNC,
        AST_DECL_MUTABLE,
    };

    struct AST_Function_Declaration
    {
        BUF(AST_Declaration*) args = nullptr;
        AST_Type_Spec* return_type_spec = nullptr;
        AST_Statement* body_block = nullptr;
    };

    struct AST_Mutable_Declaration
    {
        AST_Type_Spec* type_spec = nullptr;
        AST_Type* type = nullptr;
        AST_Expression* init_expression = nullptr;
    };

    struct AST_Declaration
    {
        AST_Declaration_Kind kind;
        File_Pos file_pos = {};

        AST_Identifier* identifier = nullptr;

        union
        {
            AST_Function_Declaration function;
            AST_Mutable_Declaration mutable_decl;
        };
    };

    struct AST_Type
    {
        
    };

    struct AST_Type_Spec
    {
        File_Pos file_pos;
        AST_Identifier* identifier = nullptr;
    };

    AST_Module* ast_module_new(Context* context, const char* module_name);
    AST_Identifier* ast_identifier_new(Context* context, Atom atom, File_Pos file_pos);

    AST_Expression* ast_binary_expression_new(Context* context, File_Pos file_pos,
                                              AST_Expression* lhs, AST_Binop_Kind op, AST_Expression* rhs);
    AST_Expression* ast_unary_expression_new(Context* context, File_Pos file_pos,
                                             AST_Unop_Kind op, AST_Expression* operand);
    AST_Expression* ast_ident_expression_new(Context* context, File_Pos file_pos, AST_Identifier* identifier);
    AST_Expression* ast_call_expression_new(Context* context, File_Pos file_pos, AST_Identifier* identifier,
                                            BUF(AST_Expression*) arg_exprs);
    AST_Expression* ast_literal_expression_new(Context* context, File_Pos file_pos, uint64_t value);

    AST_Declaration* ast_function_declaration_new(Context* context, File_Pos file_pos,
                                                  AST_Identifier* identifier,
                                                  BUF(AST_Declaration*) args,
                                                  AST_Type_Spec* return_type_spec,
                                                  AST_Statement* body_block);
    AST_Declaration* ast_mutable_declaration_new(Context* context, File_Pos file_pos, AST_Identifier* identifier,
                                                 AST_Type_Spec* type_spec, AST_Expression* init_expr);

    AST_Statement* ast_declaration_statement_new(Context* context, File_Pos file_pos, AST_Declaration* declaration);
    AST_Statement* ast_block_statement_new(Context* context, File_Pos file_pos, BUF(AST_Statement*) block_statements);
    AST_Statement* ast_return_statement_new(Context* context, File_Pos file_pos, AST_Expression* return_expr);

    AST_Type_Spec* ast_type_spec_new(Context* context, File_Pos file_pos, AST_Identifier* identifier);
}
