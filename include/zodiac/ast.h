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
    struct AST_Directive;

    struct AST_Module
    {
        BUF(AST_Declaration*) global_declarations = nullptr;
        BUF(AST_Type*) types = nullptr;
        AST_Scope* module_scope = nullptr;
        AST_Declaration* entry_point = nullptr;
        const char* module_name = nullptr;
    };

    struct AST_Identifier
    {
        File_Pos file_pos = {};
        Atom atom = {};

        AST_Declaration* declaration = nullptr;
    };

    enum AST_Expression_Kind
    {
        AST_EXPR_BINARY,
        AST_EXPR_UNARY,
        AST_EXPR_IDENTIFIER,
        AST_EXPR_CALL,
        AST_EXPR_INTEGER_LITERAL,
        AST_EXPR_CHAR_LITERAL,
    };

    enum AST_Binop_Kind
    {
        AST_BINOP_ADD,
        AST_BINOP_SUB,
        AST_BINOP_MUL,
        AST_BINOP_DIV,

        AST_BINOP_LT,
        AST_BINOP_LTEQ,
    };

    enum AST_Unop_Kind
    {
        AST_UNOP_INVALID,
        AST_UNOP_MINUS,
        AST_UNOP_ADDROF,
        AST_UNOP_DEREF,
    };

    typedef uint64_t _AST_EXPR_FLAGS_;
    enum AST_Expression_Flag : _AST_EXPR_FLAGS_
    {
        AST_EXPR_FLAG_NONE      = 0x00,
        AST_EXPR_FLAG_GENERATED = 0x01,
    };

    struct AST_Expression
    {
        AST_Expression_Kind kind;
        _AST_EXPR_FLAGS_ flags = AST_EXPR_FLAG_NONE;
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
                AST_Declaration* callee_declaration;
                BUF(AST_Expression*) arg_expressions;
            } call;

            struct
            {
                uint64_t u64;
            } integer_literal;

            struct
            {
                char c;
            } character_literal;
        };
    };

    enum AST_Statement_Kind
    {
        AST_STMT_DECLARATION,
        AST_STMT_RETURN,
        AST_STMT_BLOCK,
        AST_STMT_IF,
        AST_STMT_ASSIGN,
        AST_STMT_CALL,
    };

    typedef uint64_t _AST_STMT_FLAGS_;
    enum AST_Statement_Flag
    {
        AST_STMT_FLAG_NONE      = 0x00,
        AST_STMT_FLAG_GENERATED = 0x01,
    };

    struct AST_Statement
    {
        AST_Statement_Kind kind;
        _AST_STMT_FLAGS_ flags = AST_STMT_FLAG_NONE;
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

            struct
            {
                AST_Expression* lvalue_expression;
                AST_Expression* expression;
            } assign;

            AST_Expression* call_expression;
        };
    };

    enum AST_Declaration_Kind
    {
        AST_DECL_FUNC,
        AST_DECL_MUTABLE,
        AST_DECL_TYPE,
        AST_DECL_DYN_LINK,
    };

    struct AST_Function_Declaration
    {
        BUF(AST_Declaration*) args = nullptr;
        BUF(AST_Declaration*) locals = nullptr;
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
        AST_DECL_FLAG_NONE      = 0x00,
        AST_DECL_FLAG_RESOLVED  = 0x01,
        AST_DECL_FLAG_GENERATED = 0x02,
        AST_DECL_FLAG_FOREIGN   = 0x04,
    };

    enum AST_Declaration_Location
    {
        AST_DECL_LOC_INVALID,
        AST_DECL_LOC_GLOBAL,
        AST_DECL_LOC_LOCAL,
        AST_DECL_LOC_ARGUMENT,
    };

    struct AST_Declaration
    {
        AST_Declaration_Kind kind;
        AST_Declaration_Location location = AST_DECL_LOC_INVALID;
        _AST_DECL_FLAG_TYPE_ flags = AST_DECL_FLAG_NONE;
        File_Pos file_pos = {};

        AST_Identifier* identifier = nullptr;
        AST_Directive* directive = nullptr;

        union
        {
            AST_Function_Declaration function;
            AST_Mutable_Declaration mutable_decl;

            struct
            {
                AST_Type* type;
            } type;

            Atom dyn_link_name;
        };

        void* gen_data = nullptr;
    };

    enum AST_Type_Kind
    {
        AST_TYPE_BASE,
        AST_TYPE_POINTER,
    };

    typedef uint64_t AST_Type_Flags;
    enum _AST_TYPE_FLAG_TYPE_ : AST_Type_Flags
    {
        AST_TYPE_FLAG_NONE   = 0x00,
        AST_TYPE_FLAG_INT    = 0x01,
        AST_TYPE_FLAG_SIGNED = 0x02,
        AST_TYPE_FLAG_VOID   = 0x04,
    };

    struct AST_Type
    {
        AST_Type_Kind kind;
        AST_Type_Flags flags = AST_TYPE_FLAG_NONE;

        union
        {
            struct
            {
                uint64_t bit_size = 0;
            } base;

            AST_Type* base_type;
        };
    };

    enum AST_Type_Spec_Kind
    {
        AST_TYPE_SPEC_IDENT,
        AST_TYPE_SPEC_POINTER,
    };

    struct AST_Type_Spec
    {
        AST_Type_Spec_Kind kind;
        File_Pos file_pos;

        union
        {
            AST_Identifier* identifier;
            AST_Type_Spec* base_type_spec;
        };
    };

    struct AST_Scope
    {
        AST_Scope* parent;

        BUF(AST_Declaration*) declarations = nullptr;
    };

    enum AST_Directive_Kind
    {
        AST_DIREC_INVALID,
        AST_DIREC_FOREIGN,
        AST_DIREC_DYN_LINK,
    };

    struct AST_Directive
    {
        AST_Directive_Kind kind = AST_DIREC_INVALID;

        File_Pos file_pos = {};
    };

    AST_Module* ast_module_new(Context* context, const char* module_name);
    AST_Identifier* ast_identifier_new(Context* context, Atom atom, File_Pos file_pos);
    AST_Directive* ast_directive_new(Context* context, AST_Directive_Kind kind, File_Pos file_pos);

    AST_Expression* ast_expression_new(Context* context, File_Pos file_pos, AST_Expression_Kind kind);
    AST_Expression* ast_binary_expression_new(Context* context, File_Pos file_pos,
                                              AST_Expression* lhs, AST_Binop_Kind op, AST_Expression* rhs);
    AST_Expression* ast_unary_expression_new(Context* context, File_Pos file_pos,
                                             AST_Unop_Kind op, AST_Expression* operand);
    AST_Expression* ast_ident_expression_new(Context* context, File_Pos file_pos, AST_Identifier* identifier);
    AST_Expression* ast_call_expression_new(Context* context, File_Pos file_pos, AST_Identifier* identifier,
                                            BUF(AST_Expression*) arg_exprs);
    AST_Expression* ast_integer_literal_expression_new(Context* context, File_Pos file_pos, uint64_t value);
    AST_Expression* ast_character_literal_expression_new(Context* context, File_Pos file_pos, char value);

    AST_Declaration* ast_function_declaration_new(Context* context, File_Pos file_pos,
                                                  AST_Identifier* identifier,
                                                  BUF(AST_Declaration*) args,
                                                  AST_Type_Spec* return_type_spec,
                                                  AST_Statement* body_block,
                                                  AST_Scope* argument_scope);
    AST_Declaration* ast_mutable_declaration_new(Context* context, File_Pos file_pos, AST_Identifier* identifier,
                                                 AST_Type_Spec* type_spec, AST_Expression* init_expr,
                                                 AST_Declaration_Location location);
    AST_Declaration* ast_type_declaration_new(Context* context, File_Pos file_pos, AST_Type* type,
                                              AST_Identifier* identifier);
    AST_Declaration* ast_dyn_link_declaration_new(Context* context, File_Pos file_pos, Atom link_name,
                                                  AST_Declaration_Location location);

    AST_Statement* ast_declaration_statement_new(Context* context, File_Pos file_pos, AST_Declaration* declaration);
    AST_Statement* ast_block_statement_new(Context* context, File_Pos file_pos, BUF(AST_Statement*) block_statements,
                                           AST_Scope* block_scope);
    AST_Statement* ast_return_statement_new(Context* context, File_Pos file_pos, AST_Expression* return_expr);
    AST_Statement* ast_if_statement_new(Context* context, File_Pos file_pos, AST_Expression* cond_expr,
                                        AST_Statement* then_stmt, AST_Statement* else_stmt);
    AST_Statement* ast_assign_statement_new(Context* context, File_Pos file_pos, AST_Expression* lvalue_expression,
                                            AST_Expression* expression);
    AST_Statement* ast_call_statement_new(Context* context, AST_Expression* call_expression);

    AST_Type* ast_type_new(Context* context, AST_Type_Kind kind, AST_Type_Flags type_flags);
    AST_Type* ast_type_base_new(Context* context, AST_Type_Flags type_flags, uint64_t bit_size);
    AST_Type* ast_type_pointer_new(Context* context, AST_Type* base_type);

    AST_Type_Spec* ast_type_spec_new(Context* context, File_Pos file_pos, AST_Type_Spec_Kind kind);
    AST_Type_Spec* ast_type_spec_identifier_new(Context* context, File_Pos file_pos, AST_Identifier* identifier);
    AST_Type_Spec* ast_type_spec_pointer_new(Context* context, File_Pos file_pos, AST_Type_Spec* base_type_spec);

    AST_Scope* ast_scope_new(Context* context, AST_Scope* parent_scope);

    AST_Type* ast_find_or_create_pointer_type(Context* context, AST_Module* module, AST_Type* base_type);
}
