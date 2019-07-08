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
		AST_Declaration** declarations = nullptr;
		uint64_t declaration_count = 32;

        BUF(AST_Declaration*) global_declarations = nullptr;
        AST_Scope* module_scope = nullptr;
        AST_Declaration* entry_point = nullptr;
        const char* module_name = nullptr;

        BUF(AST_Declaration*) import_decls = nullptr;
        BUF(AST_Module*) import_modules = nullptr;

        bool resolved = false;
        void* gen_data = nullptr;
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
        AST_EXPR_SUBSCRIPT,
        AST_EXPR_BOOL_LITERAL,
        AST_EXPR_NULL_LITERAL,
        AST_EXPR_STRING_LITERAL,
        AST_EXPR_INTEGER_LITERAL,
        AST_EXPR_FLOAT_LITERAL,
        AST_EXPR_CHAR_LITERAL,
        AST_EXPR_COMPOUND_LITERAL,
        AST_EXPR_ARRAY_LENGTH,
        AST_EXPR_DOT,
		AST_EXPR_CAST,
        AST_EXPR_SIZEOF,
    };

    enum AST_Binop_Kind
    {
		AST_BINOP_INVALID,

        AST_BINOP_ADD,
        AST_BINOP_SUB,
        AST_BINOP_MUL,
        AST_BINOP_DIV,
        AST_BINOP_MOD,

        AST_BINOP_EQ,
        AST_BINOP_LT,
        AST_BINOP_LTEQ,
        AST_BINOP_GT,
        AST_BINOP_GTEQ,
        AST_BINOP_NEQ,

        AST_BINOP_AND_AND,
        AST_BINOP_OR_OR,
    };

    enum AST_Unop_Kind
    {
        AST_UNOP_INVALID,
        AST_UNOP_MINUS,
        AST_UNOP_ADDROF,
        AST_UNOP_DEREF,
        AST_UNOP_NOT,
    };

    typedef uint64_t _AST_EXPR_FLAGS_;
    enum AST_Expression_Flag : _AST_EXPR_FLAGS_
    {
        AST_EXPR_FLAG_NONE         = 0x00,
        AST_EXPR_FLAG_GENERATED    = (1 << 0),
        AST_EXPR_FLAG_LITERAL      = (1 << 1),
        AST_EXPR_FLAG_CONST        = (1 << 2),
        AST_EXPR_FLAG_POINTER_MATH = (1 << 3),
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
                AST_Expression* ident_expression;
                AST_Declaration* callee_declaration;
                BUF(AST_Expression*) arg_expressions;
            } call;

            struct
            {
                AST_Expression* base_expression;
                AST_Expression* index_expression;
                AST_Expression* call_expression; // Used when subscript operator is overloaded
            } subscript;


            struct
            {
                bool boolean;
            } bool_literal;
            struct
            {
                Atom atom;
            } string_literal;

            struct
            {
                uint64_t u64;
            } integer_literal;

            struct
            {
                double r64;
                float r32;
            } float_literal;

            struct
            {
                char c;
            } character_literal;

            struct
            {
                BUF(AST_Expression*) expressions;
            } compound_literal;

            struct
            {
                AST_Expression* ident_expr;
            } array_length;

            struct
            {
                AST_Expression* base_expression;
                AST_Expression* member_expression;
                AST_Declaration* declaration;
            } dot;

            struct
            {
                AST_Type_Spec* type_spec;
                AST_Expression* expr;
            } cast_expr;

            struct
            {
                AST_Type_Spec* type_spec;
                uint64_t byte_size;
            } sizeof_expr;
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
        AST_STMT_WHILE,
        AST_STMT_FOR,
        AST_STMT_SWITCH,
        AST_STMT_BREAK,
        AST_STMT_INSERT,
        AST_STMT_ASSERT,
        AST_STMT_DEFER,
    };

    typedef uint64_t _AST_STMT_FLAGS_;
    enum AST_Statement_Flag
    {
        AST_STMT_FLAG_NONE      = 0x00,
        AST_STMT_FLAG_GENERATED = 0x01,
    };

    struct AST_Switch_Case
    {
        File_Pos file_pos = {};
        bool is_default = false;
        BUF(AST_Expression*) case_expressions = nullptr;
        BUF(AST_Expression*) range_expressions = nullptr;
        AST_Statement* stmt = nullptr;
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

            struct
            {
                AST_Expression* cond_expr;
                AST_Statement* body_stmt;
            } while_stmt;

            struct
            {
                AST_Scope* scope;
                AST_Statement* init_stmt;
                AST_Expression* cond_expr;
                AST_Statement* step_stmt;
                AST_Statement* body_stmt;
            } for_stmt;

            struct
            {
                AST_Expression* switch_expression;
                BUF(AST_Switch_Case) cases;
            } switch_stmt;

            struct
            {
                AST_Statement* statement;
                AST_Statement* gen_statement;
            } insert;

            AST_Expression* assert_expression;
            AST_Statement* defer_statement;
        };
    };

    enum AST_Declaration_Kind
    {
        AST_DECL_FUNC,
        AST_DECL_MUTABLE,
        AST_DECL_CONSTANT_VAR,
        AST_DECL_TYPE,
        AST_DECL_DYN_LINK,
        AST_DECL_STATIC_IF,
        AST_DECL_BLOCK,
        AST_DECL_STATIC_ASSERT,
        AST_DECL_IMPORT,
        AST_DECL_AGGREGATE_TYPE,
		AST_DECL_TYPEDEF,
        AST_DECL_USING,
        AST_DECL_INSERT,
    };

    struct AST_Poly_Instance
    {
        uint64_t hash = 0;
        AST_Declaration* instance;
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

		AST_Type* type = nullptr;

        BUF(AST_Declaration*) overloads = nullptr;
        uint64_t poly_count = 0;
        BUF(AST_Poly_Instance) poly_instances;
    };

    struct AST_Mutable_Declaration
    {
        AST_Type_Spec* type_spec = nullptr;
        AST_Type* type = nullptr;
        AST_Expression* init_expression = nullptr;
    };

    struct AST_Aggregate_Declaration
    {
        File_Pos file_pos = {};
        BUF(AST_Declaration*) members = nullptr;
        AST_Identifier* index_overload = nullptr;
    };

    typedef uint64_t _AST_DECL_FLAG_TYPE_;
    enum AST_Declaration_Flags : _AST_DECL_FLAG_TYPE_
    {
        AST_DECL_FLAG_NONE             = 0x00,
        AST_DECL_FLAG_RESOLVED         = (1 << 0),
        AST_DECL_FLAG_GENERATED        = (1 << 1),
        AST_DECL_FLAG_FOREIGN          = (1 << 2),
        AST_DECL_FLAG_FUNC_POLY        = (1 << 3),
        AST_DECL_FLAG_FUNC_VARARG      = (1 << 4),
        AST_DECL_FLAG_INSERT_GENERATED = (1 << 5),
    };

    enum AST_Declaration_Location
    {
        AST_DECL_LOC_INVALID,
        AST_DECL_LOC_GLOBAL,
        AST_DECL_LOC_LOCAL,
        AST_DECL_LOC_ARGUMENT,
        AST_DECL_LOC_AGGREGATE_MEMBER,
    };

    enum AST_Aggregate_Declaration_Kind
    {
        AST_AGG_DECL_STRUCT,
        AST_AGG_DECL_UNION,
        AST_AGG_DECL_ENUM,
    };

    struct AST_Declaration
    {
        AST_Declaration_Kind kind;
        AST_Declaration_Location location = AST_DECL_LOC_INVALID;
        _AST_DECL_FLAG_TYPE_ flags = AST_DECL_FLAG_NONE;
        File_Pos file_pos = {};

        AST_Identifier* identifier = nullptr;
        AST_Directive* directive = nullptr;
		AST_Scope* scope = nullptr;

        union
        {
            AST_Function_Declaration function;

            struct
            {
                AST_Type_Spec* type_spec;
                AST_Type* type;
                AST_Expression* init_expression;
            } constant_var;

            AST_Mutable_Declaration mutable_decl;

            struct
            {
                AST_Type* type;
            } type;

            Atom dyn_link_name;

            struct
            {
                AST_Expression* cond_expr;
                AST_Declaration* then_declaration;
                AST_Declaration* else_declaration;
            } static_if;

            struct
            {
                BUF(AST_Declaration*) decls;
            } block;

            AST_Expression* static_assert_expression;

            struct
            {
                AST_Identifier* module_identifier;
                AST_Module* module;
            } import;

            struct
            {
                AST_Aggregate_Declaration_Kind kind;
                AST_Type* type;
                AST_Aggregate_Declaration* aggregate_decl;
                BUF(AST_Identifier*) parameter_idents;
                BUF(AST_Poly_Instance) poly_instances;
                uint64_t poly_count;
                AST_Scope* scope;
            } aggregate_type;

			struct
			{
				AST_Type_Spec* type_spec;
				AST_Type* type;
			} typedef_decl;

            struct
            {
                AST_Expression* ident_expression;
                AST_Declaration* scope_decl;
            } using_decl;

            struct
            {
                AST_Statement* call_statement;
            } insert_decl;
        };

        void* gen_data = nullptr;
    };

    enum AST_Type_Kind
    {
        AST_TYPE_BASE,
        AST_TYPE_POINTER,
        AST_TYPE_STATIC_ARRAY,
        AST_TYPE_STRUCT,
        AST_TYPE_UNION,
        AST_TYPE_ENUM,
        AST_TYPE_FUNCTION,
    };

    typedef uint64_t AST_Type_Flags;
    enum _AST_TYPE_FLAG_TYPE_ : AST_Type_Flags
    {
        AST_TYPE_FLAG_NONE        = 0x00,
        AST_TYPE_FLAG_INT         = (1 << 0),
        AST_TYPE_FLAG_SIGNED      = (1 << 1),
        AST_TYPE_FLAG_VOID        = (1 << 2),
        AST_TYPE_FLAG_FLOAT       = (1 << 3),
        AST_TYPE_FLAG_FUNC_VARARG = (1 << 4),
    };

    struct AST_Type
    {
        AST_Type_Kind kind;
        AST_Type_Flags flags = AST_TYPE_FLAG_NONE;

        uint64_t bit_size = 0;

        const char* name;
        AST_Identifier* index_overload;

        union
        {
            struct
            {
                AST_Type* base;
            } pointer;

            struct
            {
                uint64_t count;
                AST_Type* base;
            } static_array;

            struct
            {
                BUF(AST_Declaration*) member_declarations;
                AST_Type* base_type; // for enums
                AST_Declaration* poly_from;
                BUF(AST_Type_Spec*) poly_types;
            } aggregate_type;

            struct
            {
                BUF(AST_Type*) arg_types;
                AST_Type* return_type;
                AST_Declaration* poly_from;
            } function;
        };
    };

    enum AST_Type_Spec_Kind
    {
        AST_TYPE_SPEC_IDENT,
        AST_TYPE_SPEC_DOT,
        AST_TYPE_SPEC_POINTER,
        AST_TYPE_SPEC_STATIC_ARRAY,
        AST_TYPE_SPEC_FUNCTION,
    };

    typedef uint64_t _AST_Type_Spec_Flags_;
    enum AST_Type_Spec_Flags : _AST_Type_Spec_Flags_
    {
        AST_TYPE_SPEC_FLAG_NONE        = 0,
        AST_TYPE_SPEC_FLAG_POLY        = (1 << 0),
        AST_TYPE_SPEC_FLAG_FUNC_VARARG = (1 << 1),
    };

    struct AST_Type_Spec
    {
        AST_Type_Spec_Kind kind;
        _AST_Type_Spec_Flags_ flags;
        File_Pos file_pos;

        union
        {
            struct
            {
                AST_Identifier* identifier;
                BUF(AST_Type_Spec*) poly_args;
            } identifier;

            struct
            {
                AST_Type_Spec* base;
            } pointer;

            struct
            {
                AST_Expression* count_expr;
                AST_Type_Spec* base;
            } static_array;

            struct
            {
                BUF(AST_Declaration*) args;
                AST_Type_Spec* return_type_spec;
                AST_Scope* arg_scope;
            } function;

            struct
            {
                AST_Identifier* module_ident;
                AST_Type_Spec* member_type_spec;
            } dot;
        };
    };

    typedef uint64_t _AST_Scope_Flags_;
    enum AST_Scope_Flags : _AST_Scope_Flags_
    {
        AST_SCOPE_FLAG_NONE            = 0,
        AST_SCOPE_FLAG_IS_MODULE_SCOPE = (1 << 0),
        AST_SCOPE_FLAG_BREAK_SCOPE     = (1 << 1),
    };

    struct AST_Scope
    {
        _AST_Scope_Flags_ flags = AST_SCOPE_FLAG_NONE;
        AST_Scope* parent = nullptr;
        AST_Module* module = nullptr;

        BUF(AST_Statement*) defer_statements = nullptr;

        BUF(AST_Module*) using_modules = nullptr;
        BUF(AST_Declaration*) using_declarations = nullptr;
    };

    enum AST_Directive_Kind
    {
        AST_DIREC_INVALID,
        AST_DIREC_FOREIGN,
        AST_DIREC_DYN_LINK,
        AST_DIREC_INSERT,
    };

    struct AST_Directive
    {
        AST_Directive_Kind kind = AST_DIREC_INVALID;

        File_Pos file_pos = {};
    };

    AST_Module* ast_module_new(Context* context, const char* module_name);
    AST_Identifier* ast_identifier_new(Context* context, Atom atom, File_Pos file_pos);
    AST_Directive* ast_directive_new(Context* context, AST_Directive_Kind kind, File_Pos file_pos);

    AST_Expression* ast_expression_new(Context* context, File_Pos file_pos,
                                       AST_Expression_Kind kind);
    AST_Expression* ast_binary_expression_new(Context* context, File_Pos file_pos,
                                              AST_Expression* lhs, AST_Binop_Kind op,
                                              AST_Expression* rhs);
    AST_Expression* ast_unary_expression_new(Context* context, File_Pos file_pos,
                                             AST_Unop_Kind op, AST_Expression* operand);
    AST_Expression* ast_ident_expression_new(Context* context, File_Pos file_pos,
                                             AST_Identifier* identifier);
    AST_Expression* ast_call_expression_new(Context* context, File_Pos file_pos,
                                            AST_Expression* ident_expression,
                                            BUF(AST_Expression*) arg_exprs);
    AST_Expression* ast_subscript_expression_new(Context* context, File_Pos file_pos,
                                                 AST_Expression* base_expression,
                                                 AST_Expression* index_expression);
    AST_Expression* ast_boolean_literal_expression_new(Context* context, File_Pos file_Pos,
                                                       bool value);
    AST_Expression* ast_null_literal_expression_new(Context* context, File_Pos file_pos);
    AST_Expression* ast_string_literal_expression_new(Context* context, File_Pos file_pos,
                                                      Atom value);
    AST_Expression* ast_integer_literal_expression_new(Context* context, File_Pos file_pos,
                                                       uint64_t value);
    AST_Expression* ast_float_literal_expression_new(Context* context, File_Pos file_pos,
                                                     double r64, float r32);
    AST_Expression* ast_character_literal_expression_new(Context* context, File_Pos file_pos,
                                                         char value);
    AST_Expression* ast_compound_literal_expression_new(Context* context, File_Pos file_pos,
                                                        BUF(AST_Expression*) expressions);
    AST_Expression* ast_array_length_expression_new(Context* context, File_Pos file_pos,
                                                    AST_Expression* ident_expr);
    AST_Expression* ast_sizeof_expression_new(Context* context, File_Pos file_pos,
                                              AST_Type_Spec* type_spec);
    AST_Expression* ast_dot_expression_new(Context* context, File_Pos file_pos,
                                           AST_Expression* base_expr,
                                           AST_Expression* member_expr);
    AST_Expression* ast_cast_expression_new(Context* context, File_Pos file_pos,
                                            AST_Type_Spec* type_spec,
                                            AST_Expression* cast_expr);

    AST_Aggregate_Declaration* ast_aggregate_declaration_new(Context* context, File_Pos file_pos,
                                                             BUF(AST_Declaration*) members,
                                                             AST_Identifier* index_overload_ident);

    AST_Declaration* ast_declaration_new(Context* context, File_Pos file_Pos,
                                         AST_Declaration_Kind kind,
                                         AST_Declaration_Location location,
                                         AST_Identifier* identifier, AST_Directive* directive);
    AST_Declaration* ast_function_declaration_new(Context* context, File_Pos file_pos,
                                                  AST_Identifier* identifier,
                                                  BUF(AST_Declaration*) args,
                                                  bool is_vararg, bool is_poly,
                                                  AST_Type_Spec* return_type_spec,
                                                  AST_Statement* body_block,
                                                  AST_Scope* argument_scope);
    AST_Declaration* ast_mutable_declaration_new(Context* context, File_Pos file_pos,
                                                 AST_Identifier* identifier,
                                                 AST_Type_Spec* type_spec,
                                                 AST_Expression* init_expr,
                                                 AST_Declaration_Location location);
    AST_Declaration* ast_constant_variable_declaration_new(Context* context, File_Pos file_pos,
                                                           AST_Identifier* identifier,
                                                           AST_Type_Spec* type_spec,
                                                           AST_Expression* init_expr,
                                                           AST_Declaration_Location location);
    AST_Declaration* ast_type_declaration_new(Context* context, File_Pos file_pos, AST_Type* type,
                                              AST_Identifier* identifier);
    AST_Declaration* ast_dyn_link_declaration_new(Context* context, File_Pos file_pos,
                                                  Atom link_name,
                                                  AST_Declaration_Location location);
    AST_Declaration* ast_static_if_declaration_new(Context* context, File_Pos file_pos,
                                                   AST_Expression* cond_expr,
                                                   AST_Declaration* then_declaration,
                                                   AST_Declaration* else_declaration);
    AST_Declaration* ast_using_declaration_new(Context* context, File_Pos file_pos,
                                               AST_Expression* ident_expr,
                                               AST_Declaration_Location location);
    AST_Declaration* ast_block_declaration_new(Context* context, File_Pos file_pos,
                                               BUF(AST_Declaration*) block_decls);
    AST_Declaration* ast_static_assert_declaration_new(Context* context, File_Pos file_pos,
                                                       AST_Expression* assert_expr);
    AST_Declaration* ast_import_declaration_new(Context* context, File_Pos file_pos,
                                                AST_Identifier* identifier,
                                                AST_Identifier* import_module_identifier);
    AST_Declaration* ast_struct_declaration_new(Context* context, File_Pos file_pos,
                                                AST_Identifier* identifier,
                                               AST_Aggregate_Declaration* aggregate_decl,
                                                BUF(AST_Identifier*) parameters,
                                                AST_Scope* scope);
    AST_Declaration* ast_union_declaration_new(Context* context, File_Pos file_pos,
                                                AST_Identifier* identifier,
                                                AST_Aggregate_Declaration* aggregate_decl,
                                                BUF(AST_Identifier*) parameters,
                                                AST_Scope* scope);
    AST_Declaration* ast_enum_declaration_new(Context* context, File_Pos file_pos,
                                              AST_Identifier* identifier,
                                              AST_Aggregate_Declaration* aggregate_decl,
                                              AST_Scope* scope);
	AST_Declaration* ast_typedef_declaration_new(Context* context, File_Pos file_pos,
		                                         AST_Identifier* identifier,
		                                         AST_Type_Spec* type_spec);
    AST_Declaration* ast_insert_declaration_new(Context* context, File_Pos file_pos,
                                                AST_Statement* stmt);

    AST_Statement* ast_declaration_statement_new(Context* context, File_Pos file_pos,
                                                 AST_Declaration* declaration);
    AST_Statement* ast_block_statement_new(Context* context, File_Pos file_pos,
                                           BUF(AST_Statement*) block_statements,
                                           AST_Scope* block_scope);
    AST_Statement* ast_return_statement_new(Context* context, File_Pos file_pos,
                                            AST_Expression* return_expr);
    AST_Statement* ast_if_statement_new(Context* context, File_Pos file_pos,
                                        AST_Expression* cond_expr,
                                        AST_Statement* then_stmt, AST_Statement* else_stmt);
    AST_Statement* ast_assign_statement_new(Context* context, File_Pos file_pos,
                                            AST_Expression* lvalue_expression,
                                            AST_Expression* expression);
    AST_Statement* ast_call_statement_new(Context* context, AST_Expression* call_expression);
    AST_Statement* ast_while_statement_new(Context* context, File_Pos file_pos,
                                           AST_Expression* cond_expr,
                                           AST_Statement* body_stmt);
    AST_Statement* ast_for_statement_new(Context* context, File_Pos file_pos, AST_Scope* scope,
                                         AST_Statement* init_stmt, AST_Expression* cond_expr,
                                         AST_Statement* step_stmt, AST_Statement* body_stmt);
    AST_Statement* ast_switch_statement_new(Context* context, File_Pos file_pos,
                                            AST_Expression* switch_expr,
                                            BUF(AST_Switch_Case) cases);
    AST_Statement* ast_break_statement_new(Context* context, File_Pos file_pos);
    AST_Statement* ast_insert_statement_new(Context* context, File_Pos file_pos,
                                            AST_Statement* statement);
    AST_Statement* ast_assert_statement_new(Context* context, File_Pos file_pos,
                                            AST_Expression* assert_expr);
    AST_Statement* ast_defer_statement_new(Context* context, File_Pos file_pos,
                                           AST_Statement* defer_statement);

    AST_Type* ast_type_new(Context* context, AST_Type_Kind kind, AST_Type_Flags type_flags,
                           const char* name, uint64_t bit_size);
    AST_Type* ast_type_base_new(Context* context, AST_Type_Flags type_flags, const char* name,
                                uint64_t bit_size);
    AST_Type* ast_type_pointer_new(Context* context, AST_Type* base_type);
    AST_Type* ast_type_static_array_new(Context* context, AST_Type* base_type, uint64_t count);
    AST_Type* ast_type_struct_new(Context* context, BUF(AST_Declaration*) member_declarations,
                                  const char* name, uint64_t bit_size,
                                  AST_Identifier* index_overload);
    AST_Type* ast_type_union_new(Context* context, BUF(AST_Declaration*) member_declarations,
                                 const char* name, uint64_t bit_size,
                                 AST_Identifier* index_overload);
    AST_Type* ast_type_enum_new(Context* context, BUF(AST_Declaration*) member_decls,
                                AST_Type* base_type);
    AST_Type* ast_type_function_new(Context* context, bool is_vararg, BUF(AST_Type*) arg_types,
                                    AST_Type* return_type);

    AST_Type_Spec* ast_type_spec_new(Context* context, File_Pos file_pos, AST_Type_Spec_Kind kind);
    AST_Type_Spec* ast_type_spec_identifier_new(Context* context, File_Pos file_pos,
                                                AST_Identifier* identifier,
                                                BUF(AST_Type_Spec*) poly_args = nullptr);
    AST_Type_Spec* ast_type_spec_dot_new(Context* context, File_Pos file_pos,
                                         AST_Identifier* module_ident,
                                         AST_Type_Spec* member_type_spec);
    AST_Type_Spec* ast_type_spec_pointer_new(Context* context, File_Pos file_pos,
                                             AST_Type_Spec* base_type_spec);
    AST_Type_Spec* ast_type_spec_static_array_new(Context* context, File_Pos file_pos,
                                                  AST_Expression* count_expr,
                                                  AST_Type_Spec* base_type_spec);
    AST_Type_Spec* ast_type_spec_function_new(Context* context, File_Pos file_pos,
                                              bool is_vararg, BUF(AST_Declaration*) arg_decls,
                                              AST_Type_Spec* return_type_spec,
                                              AST_Scope* arg_scope);
    AST_Type_Spec* ast_type_spec_from_type_new(Context* context, File_Pos file_pos,
                                               AST_Type* type);

	AST_Scope* ast_scope_new(Context* context, AST_Scope* parent_scope, AST_Module* module,
		                     bool is_module_scope);

	void ast_scope_push_declaration(AST_Scope* scope, AST_Declaration* declaration);
	AST_Declaration* ast_scope_find_declaration(Context* context, AST_Scope* scope,
                                                const Atom& ident_atom);

	void ast_module_grow_declaration_hash(AST_Module* module);

    AST_Type* ast_find_or_create_pointer_type(Context* context, AST_Type* base_type);
    AST_Type* ast_find_or_create_array_type(Context* context, AST_Type* base_type,
                                            AST_Expression* count_expr, AST_Scope* scope);
    AST_Type* ast_find_or_create_array_type(Context* context, AST_Type* base_type, uint64_t count);
	AST_Type* ast_find_or_create_function_type(Context* context, bool is_vararg,
                                               BUF(AST_Type*) arg_types,
		                                       AST_Type* return_type);

    uint64_t ast_get_type_hash(AST_Type* type);
    uint64_t ast_get_pointer_type_hash(AST_Type* base_type);
    uint64_t ast_get_static_array_type_hash(AST_Type* base_type, uint64_t count);
    uint64_t ast_get_function_type_hash(bool is_vararg, BUF(AST_Type*) arg_types,
                                        AST_Type* return_type);

    void ast_grow_type_hash(Context* context);

    const char* ast_type_to_string(AST_Type* type);

    bool is_cmp_op(AST_Binop_Kind binop);
}
