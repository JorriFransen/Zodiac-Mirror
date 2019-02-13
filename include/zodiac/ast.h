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
    struct AST_Scope;

    struct AST_Module
    {
        BUF(AST_Declaration*) global_declarations = nullptr;
        AST_Scope* module_scope = nullptr;
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

        AST_BINOP_LT,
    };

    enum AST_Unop_Kind
    {
        AST_UNOP_MINUS,
    };

    struct AST_Expression
    {
        AST_Expression_Kind kind;
        File_Pos file_pos;

        AST_Type* type = nullptr;

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
        AST_STMT_IF,
    };

    struct AST_Statement
    {
        AST_Statement_Kind kind;
        File_Pos file_pos = {};

        union
        {
            AST_Declaration* declaration;
            AST_Expression* return_expression;

            struct
            {
                BUF(AST_Statement*) statements;
                AST_Scope* scope;
            } block;

            struct
            {
                AST_Expression* if_expression;
                AST_Statement* then_statement;
                AST_Statement* else_statement;
            } if_stmt;
        };
    };

    enum AST_Declaration_Kind
    {
        AST_DECL_FUNC,
        AST_DECL_MUTABLE,
        AST_DECL_TYPE,
    };

    struct AST_Function_Declaration
    {
        BUF(AST_Declaration*) args = nullptr;
        AST_Type_Spec* return_type_spec = nullptr;
        AST_Type* return_type = nullptr;
        AST_Type* inferred_return_type = nullptr;
        AST_Statement* body_block = nullptr;

        // The body block will have it's own scope, this will be it's parent
        AST_Scope* argument_scope = nullptr;
    };

    struct AST_Mutable_Declaration
    {
        AST_Type_Spec* type_spec = nullptr;
        AST_Type* type = nullptr;
        AST_Expression* init_expression = nullptr;
    };

    typedef uint64_t _AST_DECL_FLAG_TYPE_;
    enum AST_Declaration_Flags : _AST_DECL_FLAG_TYPE_
    {
        AST_DECL_FLAG_NONE     = 0x00,
        AST_DECL_FLAG_RESOLVED = 0x01,
    };

    struct AST_Declaration
    {
        AST_Declaration_Kind kind;
        _AST_DECL_FLAG_TYPE_ flags = AST_DECL_FLAG_NONE;
        File_Pos file_pos = {};

        AST_Identifier* identifier = nullptr;

        union
        {
            AST_Function_Declaration function;
            AST_Mutable_Declaration mutable_decl;

            struct
            {
                AST_Type* type;
            } type;
        };
    };

    typedef uint64_t AST_Type_Flags;
    enum _AST_TYPE_FLAG_TYPE_ : AST_Type_Flags
    {
        AST_TYPE_FLAG_INT    = 0x01,
        AST_TYPE_FLAG_SIGNED = 0x02,
    };

    struct AST_Type
    {
        AST_Type_Flags flags = 0;
        uint64_t bit_size = 0;
    };

    struct AST_Type_Spec
    {
        File_Pos file_pos;
        AST_Identifier* identifier = nullptr;
    };

    struct AST_Scope
    {
        AST_Scope* parent;

        BUF(AST_Declaration*) declarations = nullptr;
    };

    AST_Module* ast_module_new(Context* context, const char* module_name);
    AST_Identifier* ast_identifier_new(Context* context, Atom atom, File_Pos file_pos);

    AST_Expression* ast_expression_new(Context* context, File_Pos file_pos, AST_Expression_Kind kind);
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
                                                  AST_Statement* body_block,
                                                  AST_Scope* argument_scope);
    AST_Declaration* ast_mutable_declaration_new(Context* context, File_Pos file_pos, AST_Identifier* identifier,
                                                 AST_Type_Spec* type_spec, AST_Expression* init_expr);
    AST_Declaration* ast_type_declaration_new(Context* context, File_Pos file_pos, AST_Type* type,
                                              AST_Identifier* identifier);

    AST_Statement* ast_declaration_statement_new(Context* context, File_Pos file_pos, AST_Declaration* declaration);
    AST_Statement* ast_block_statement_new(Context* context, File_Pos file_pos, BUF(AST_Statement*) block_statements,
                                           AST_Scope* block_scope);
    AST_Statement* ast_return_statement_new(Context* context, File_Pos file_pos, AST_Expression* return_expr);
    AST_Statement* ast_if_statement_new(Context* context, File_Pos file_pos, AST_Expression* cond_expr,
                                        AST_Statement* then_stmt, AST_Statement* else_stmt);

    AST_Type* ast_type_new(Context* context, AST_Type_Flags type_flags, uint64_t bit_size);

    AST_Type_Spec* ast_type_spec_new(Context* context, File_Pos file_pos, AST_Identifier* identifier);

    AST_Scope* ast_scope_new(Context* context, AST_Scope* parent_scope);
}
