#pragma once

#include "zodiac.h"
#include "file_pos.h"
#include "string_builder.h"

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
        const char* module_file_name = nullptr;
        const char* module_file_dir = nullptr;

        BUF(AST_Declaration*) import_decls = nullptr;
        BUF(AST_Module*) import_modules = nullptr;

        bool resolved = false;
        void* gen_data = nullptr;
        bool poly_dirty = false;
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
        AST_EXPR_GET_TYPE_INFO,
        AST_EXPR_POST_INCREMENT,
        AST_EXPR_POST_DECREMENT,
        AST_EXPR_EXPRESSION_LIST,
        AST_EXPR_IGNORED_VALUE,
        AST_EXPR_MAKE_LVALUE,
    };

    enum AST_Binop_Kind
    {
		AST_BINOP_INVALID,

        AST_BINOP_ADD,
        AST_BINOP_SUB,
        AST_BINOP_MUL,
        AST_BINOP_DIV,
        AST_BINOP_MOD,

        AST_BINOP_RSHIFT,
        AST_BINOP_LSHIFT,

        AST_BINOP_EQ,
        AST_BINOP_LT,
        AST_BINOP_LTEQ,
        AST_BINOP_GT,
        AST_BINOP_GTEQ,
        AST_BINOP_NEQ,

        AST_BINOP_AND_AND,
        AST_BINOP_OR_OR,
        AST_BINOP_AND,
        AST_BINOP_OR,
    };

    enum AST_Unop_Kind
    {
        AST_UNOP_INVALID,
        AST_UNOP_MINUS,
        AST_UNOP_ADDROF,
        AST_UNOP_DEREF,
        AST_UNOP_NOT,
		AST_UNOP_BIN_NOT,
    };

    typedef uint64_t _AST_EXPR_FLAGS_;
    enum AST_Expression_Flag : _AST_EXPR_FLAGS_
    {
        AST_EXPR_FLAG_NONE            = 0x00,
        AST_EXPR_FLAG_GENERATED       = (1 << 0),
        AST_EXPR_FLAG_LITERAL         = (1 << 1),
        AST_EXPR_FLAG_CONST           = (1 << 2),
        AST_EXPR_FLAG_POINTER_MATH    = (1 << 3),
        AST_EXPR_FLAG_RESOLVED        = (1 << 4),
        AST_EXPR_FLAG_INTEGER_LITERAL = (1 << 5),
        AST_EXPR_FLAG_LVALUE          = (1 << 6),
        AST_EXPR_FLAG_FIRST_VARARG    = (1 << 7),
    };

    enum AST_Builtin_Function
    {
        AST_BUILTIN_FUNC_INVALID,
        AST_BUILTIN_FUNC_CREATE_THREAD,
        AST_BUILTIN_FUNC_JOIN_THREAD,
        AST_BUILTIN_FUNC_COMPARE_AND_SWAP,
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
            AST_Expression* base_expression;

            struct
            {
                AST_Binop_Kind op;
                AST_Expression* call_expression;
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

                AST_Builtin_Function builtin_function;
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

            struct
            {
                AST_Type_Spec* type_spec;
                AST_Type* type;
            } get_type_info_expr;

            struct
            {
                BUF(AST_Expression*) expressions;
            } list;

            struct
            {
                AST_Expression* expression;
            } make_lvalue;
        };
    };

    enum AST_Statement_Kind
    {
        AST_STMT_DECLARATION,
        AST_STMT_RETURN,
        AST_STMT_BLOCK,
        AST_STMT_IF,
        AST_STMT_STATIC_IF,
        AST_STMT_ASSIGN,
        AST_STMT_CALL,
        AST_STMT_WHILE,
        AST_STMT_FOR,
        AST_STMT_SWITCH,
        AST_STMT_BREAK,
        AST_STMT_INSERT,
        AST_STMT_ASSERT,
        AST_STMT_ASSERT_FAIL,
        AST_STMT_DEFER,
        AST_STMT_POST_INCREMENT,
        AST_STMT_POST_DECREMENT,
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
            AST_Expression* post_increment;
            AST_Expression* post_decrement;

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
            } if_stmt, static_if_stmt;

            struct
            {
                AST_Expression* lvalue_expression;
                AST_Expression* expression;
            } assign;

            AST_Expression* call_expression;

            struct
            {
                AST_Scope* scope;
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
        AST_DECL_FUNC_OVERLOAD,
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
        AST_DECL_POLY_TYPE_SPEC,
        AST_DECL_LIST,
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

        bool body_generated = false;
    };

    struct AST_Mutable_Declaration
    {
        AST_Type_Spec* type_spec = nullptr;
        AST_Type* type = nullptr;
        AST_Expression* init_expression = nullptr;
    };

    enum AST_Overload_Operator_Kind
    {
        AST_OVERLOAD_OP_INVALID,
        AST_OVERLOAD_OP_INDEX,
        AST_OVERLOAD_OP_INDEX_LVALUE,
        AST_OVERLOAD_OP_PLUS,
        AST_OVERLOAD_OP_MINUS,
        AST_OVERLOAD_OP_MUL,
        AST_OVERLOAD_OP_DIV,
    };

    struct AST_Overload_Directive
    {
        AST_Overload_Operator_Kind op;
        AST_Identifier* identifier = nullptr;
    };

    struct AST_Aggregate_Poly
    {
        BUF(AST_Type*) types = nullptr;
        AST_Declaration* instance = nullptr;
    };

    struct AST_Aggregate_Declaration
    {
        File_Pos file_pos = {};
        BUF(AST_Declaration*) members = nullptr;
        BUF(AST_Identifier*) poly_args = nullptr;
        BUF(AST_Aggregate_Poly) poly_instances = nullptr;
        BUF(AST_Overload_Directive) overload_directives = nullptr;
    };

    typedef uint64_t _AST_DECL_FLAG_TYPE_;
    enum AST_Declaration_Flags : _AST_DECL_FLAG_TYPE_
    {
        AST_DECL_FLAG_NONE                      =        0 ,
        AST_DECL_FLAG_RESOLVING                 = (1 <<  0),
        AST_DECL_FLAG_RESOLVED                  = (1 <<  1),
        AST_DECL_FLAG_ERROR                     = (1 <<  2),
        AST_DECL_FLAG_GENERATED                 = (1 <<  3),
        AST_DECL_FLAG_FOREIGN                   = (1 <<  4),
        AST_DECL_FLAG_FUNC_VARARG               = (1 <<  5),
        AST_DECL_FLAG_FUNC_OVERLOAD             = (1 <<  6),
        AST_DECL_FLAG_FUNC_POLY                 = (1 <<  7),
        AST_DECL_FLAG_FUNC_POLY_TEMPLATE        = (1 <<  8),
        AST_DECL_FLAG_FUNC_POLY_TEMPLATE_ERROR  = (1 <<  9),
        AST_DECL_FLAG_INSERT_GENERATED          = (1 << 10),
        AST_DECL_FLAG_REPLACED_NESTED_AGGREGATE = (1 << 11),
        AST_DECL_FLAG_BUILTIN                   = (1 << 12),
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
                BUF(AST_Declaration*) overloads;
                BUF(AST_Declaration*) poly_templates;
            } function_overload;

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
                AST_Scope* scope;
                AST_Type_Spec* enum_type_spec;
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

            struct
            {
                AST_Type_Spec* type_spec;
            } poly_type_spec;

            struct
            {
                AST_Expression* list_expression;
                AST_Expression* init_expression;
                BUF(AST_Declaration*) declarations;
            } list;
        };
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
        AST_TYPE_MRV,
    };

    typedef uint64_t AST_Type_Flags;
    enum _AST_TYPE_FLAG_TYPE_ : AST_Type_Flags
    {
        AST_TYPE_FLAG_NONE                   = 0x00,
        AST_TYPE_FLAG_INT                    = (1 << 0),
        AST_TYPE_FLAG_SIGNED                 = (1 << 1),
        AST_TYPE_FLAG_VOID                   = (1 << 2),
        AST_TYPE_FLAG_FLOAT                  = (1 << 3),
        AST_TYPE_FLAG_FUNC_VARARG            = (1 << 4),
        AST_TYPE_FLAG_REGISTERING_TYPE_INFO  = (1 << 5),
        AST_TYPE_FLAG_REGISTERED_TYPE_INFO   = (1 << 6),
        AST_TYPE_FLAG_FROM_MRV               = (1 << 7),
    };

    struct AST_Type
    {
        AST_Type_Kind kind;
        AST_Type_Flags flags = AST_TYPE_FLAG_NONE;

        uint64_t bit_size = 0;

        const char* name;
        BUF(AST_Overload_Directive) overloads = nullptr;

        AST_Type* pointer_to = nullptr;
        BUF(AST_Type*) arrays_of = nullptr;

        uint64_t info_index = 0;

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
                AST_Scope* scope;
                union
                {
                    AST_Type* base_type; // for enums
                    AST_Declaration* poly_from; // for structs/unions
                };
            } aggregate_type;

            struct
            {
                BUF(AST_Type*) arg_types;
                AST_Type* return_type;
            } function;

            struct
            {
                BUF(AST_Type*) types;
                BUF(AST_Directive*) directives;
                AST_Type* struct_type;
            } mrv;
        };
    };

    enum AST_Type_Spec_Kind
    {
        AST_TYPE_SPEC_IDENT,
        AST_TYPE_SPEC_DOT,
        AST_TYPE_SPEC_POINTER,
        AST_TYPE_SPEC_STATIC_ARRAY,
        AST_TYPE_SPEC_FUNCTION,
        AST_TYPE_SPEC_TYPEOF,
        AST_TYPE_SPEC_FROM_TYPE,
        AST_TYPE_SPEC_POLY_FUNC_ARG,
        AST_TYPE_SPEC_MRV,
        AST_TYPE_SPEC_VARARG,
    };

    typedef uint64_t _AST_Type_Spec_Flags_;
    enum AST_Type_Spec_Flags : _AST_Type_Spec_Flags_
    {
        AST_TYPE_SPEC_FLAG_NONE                        = 0,
        AST_TYPE_SPEC_FLAG_FUNC_VARARG                 = (1 << 0),
        AST_TYPE_SPEC_FLAG_HAS_POLY_CHILDREN           = (1 << 1),
        AST_TYPE_SPEC_FLAG_IS_POLY_ARG_MATCH           = (1 << 2),
        AST_TYPE_SPEC_FLAG_HAS_POLY_ARG_MATCH_CHILDREN = (1 << 3),
    };

    struct AST_Type_Spec
    {
        AST_Type_Spec_Kind kind;
        _AST_Type_Spec_Flags_ flags;
        File_Pos file_pos;

        AST_Type* type = nullptr;

        union
        {
            struct
            {
                AST_Identifier* identifier;
                BUF(AST_Type_Spec*) arg_type_specs;
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
                const char* name;
                BUF(AST_Declaration*) args;
                AST_Type_Spec* return_type_spec;
                AST_Scope* arg_scope;
            } function;

            struct
            {
                AST_Identifier* module_ident;
                AST_Type_Spec* member_type_spec;
            } dot;

            struct
            {
                AST_Expression* expr;
            } typeof_expr;

            struct
            {
                AST_Identifier* identifier;
            } poly_func_arg;

            struct
            {
                BUF(AST_Type_Spec*) specs;
                BUF(AST_Directive*) directives;
            } mrv;
        };
    };

    typedef uint64_t _AST_Scope_Flags_;
    enum AST_Scope_Flags : _AST_Scope_Flags_
    {
        AST_SCOPE_FLAG_NONE            = 0,
        AST_SCOPE_FLAG_IS_MODULE_SCOPE = (1 << 0),
        AST_SCOPE_FLAG_IS_ENUM_SCOPE   = (1 << 1),
        AST_SCOPE_FLAG_BREAK_SCOPE     = (1 << 2),
        AST_SCOPE_FLAG_IS_POLY_SCOPE   = (1 << 3),
    };

    struct AST_Scope
    {
        _AST_Scope_Flags_ flags = AST_SCOPE_FLAG_NONE;
        AST_Scope* parent = nullptr;
        AST_Module* module = nullptr;

        uint64_t line = 0;

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
        AST_DIREC_REQUIRED,
    };

    struct AST_Directive
    {
        AST_Directive_Kind kind = AST_DIREC_INVALID;

        File_Pos file_pos = {};
    };

    AST_Module* ast_module_new(Context* context, const char* module_name, const char* path);
    AST_Identifier* ast_identifier_new(Context* context, Atom atom, File_Pos file_pos);
    AST_Identifier* ast_identifier_new(Context* context, const char* name, File_Pos file_pos);
    AST_Directive* ast_directive_new(Context* context, AST_Directive_Kind kind,
                                     File_Pos file_pos);

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
    AST_Expression* ast_get_type_info_expression_new(Context* context, File_Pos file_pos,
                                                     AST_Type_Spec* type_spec);
    AST_Expression* ast_dot_expression_new(Context* context, File_Pos file_pos,
                                           AST_Expression* base_expr,
                                           AST_Expression* member_expr);
    AST_Expression* ast_cast_expression_new(Context* context, File_Pos file_pos,
                                            AST_Type_Spec* type_spec,
                                            AST_Expression* cast_expr);
    AST_Expression* ast_post_increment_expression_new(Context* context, File_Pos file_pos,
                                                      AST_Expression* base_expression);
    AST_Expression* ast_post_decrement_expression_new(Context* context, File_Pos file_pos,
                                                      AST_Expression* base_expression);
    AST_Expression* ast_expression_list_expression_new(Context* context, File_Pos file_pos,
                                                       BUF(AST_Expression*) expressions);
    AST_Expression* ast_expression_ignored_value_new(Context* context, File_Pos file_pos);
    AST_Expression* ast_make_lvalue_expression_new(Context* context, File_Pos file_pos,
                                                   AST_Expression* non_lvalue);

    AST_Aggregate_Declaration*
        ast_aggregate_declaration_new(Context* context, File_Pos file_pos,
                                      BUF(AST_Declaration*) members,
                                      BUF(AST_Identifier*) poly_args,
                                      BUF(AST_Overload_Directive) overloads);

    AST_Declaration* ast_declaration_new(Context* context, File_Pos file_Pos,
                                         AST_Declaration_Kind kind,
                                         AST_Declaration_Location location,
                                         AST_Identifier* identifier, AST_Directive* directive);
    AST_Declaration* ast_list_declaration_new(Context* context, File_Pos file_pos,
                                              AST_Expression* list_expr,
                                              AST_Expression* init_expr);

    AST_Declaration* ast_function_declaration_new(Context* context, File_Pos file_pos,
                                                  AST_Identifier* identifier,
                                                  BUF(AST_Declaration*) args,
                                                  bool is_vararg,
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
    AST_Declaration* ast_using_declaration_new(Context* context, File_Pos file_pos,
                                               AST_Identifier* ident,
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
                                                AST_Declaration_Location location,
                                                AST_Scope* scope);
    AST_Declaration* ast_union_declaration_new(Context* context, File_Pos file_pos,
                                               AST_Identifier* identifier,
                                               AST_Aggregate_Declaration* aggregate_decl,
                                               AST_Declaration_Location location,
                                               AST_Scope* scope);
    AST_Declaration* ast_enum_declaration_new(Context* context, File_Pos file_pos,
                                              AST_Identifier* identifier,
                                              AST_Type_Spec* enum_type_spec,
                                              AST_Aggregate_Declaration* aggregate_decl,
                                              AST_Scope* scope);
    AST_Declaration* ast_typedef_declaration_new(Context* context, File_Pos file_pos,
                                                 AST_Identifier* identifier,
                                                 AST_Type_Spec* type_spec);
    AST_Declaration* ast_insert_declaration_new(Context* context, File_Pos file_pos,
                                                AST_Statement* stmt);
    AST_Declaration* ast_poly_type_spec_declaration_new(Context* context, File_Pos file_pos,
                                                        AST_Identifier* ident,
                                                        AST_Type_Spec* type_spec);

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
    AST_Statement* ast_static_if_statement_new(Context* context, File_Pos file_pos,
                                               AST_Expression* cond_expr,
                                               AST_Statement* then_stmt,
                                               AST_Statement* else_stmt);
    AST_Statement* ast_assign_statement_new(Context* context, File_Pos file_pos,
                                            AST_Expression* lvalue_expression,
                                            AST_Expression* expression);
    AST_Statement* ast_call_statement_new(Context* context, AST_Expression* call_expression);
    AST_Statement* ast_while_statement_new(Context* context, File_Pos file_pos, AST_Scope* scope,
                                           AST_Expression* cond_expr, AST_Statement* body_stmt);
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
    AST_Statement* ast_assert_fail_statement_new(Context* context, File_Pos file_pos);
    AST_Statement* ast_defer_statement_new(Context* context, File_Pos file_pos,
                                           AST_Statement* defer_statement);
    AST_Statement* ast_post_increment_statement_new(Context* context, File_Pos file_pos,
                                                    AST_Expression* post_inc_expr);
    AST_Statement* ast_post_decrement_statement_new(Context* context, File_Pos file_pos,
                                                    AST_Expression* post_dec_expr);

    AST_Type* ast_type_new(Context* context, AST_Type_Kind kind, AST_Type_Flags type_flags,
                           const char* name, uint64_t bit_size);
    AST_Type* ast_type_base_new(Context* context, AST_Type_Flags type_flags, const char* name,
                                uint64_t bit_size);
    AST_Type* ast_type_pointer_new(Context* context, AST_Type* base_type);
    AST_Type* ast_type_static_array_new(Context* context, AST_Type* base_type, uint64_t count);
    AST_Type* ast_type_struct_new(Context* context, BUF(AST_Declaration*) member_declarations,
                                  const char* name, uint64_t bit_size, AST_Scope* scope,
                                  BUF(AST_Overload_Directive) overloads);
    AST_Type* ast_type_union_new(Context* context, BUF(AST_Declaration*) member_declarations,
                                 const char* name, uint64_t bit_size, AST_Scope* scope,
                                 BUF(AST_Overload_Directive) overloads);
    AST_Type* ast_type_enum_new(Context* context, BUF(AST_Declaration*) member_decls,
                                const char* name, AST_Type* base_type, AST_Scope* scope);
    AST_Type* ast_type_function_new(Context* context, bool is_vararg, BUF(AST_Type*) arg_types,
                                    AST_Type* return_type);
    AST_Type* ast_type_mrv_new(Context* context, BUF(AST_Type*) mrv_types,
                               BUF(AST_Directive*) directives, AST_Scope* scope);

    AST_Type_Spec* ast_type_spec_new(Context* context, File_Pos file_pos,
                                     AST_Type_Spec_Kind kind);
    AST_Type_Spec* ast_type_spec_identifier_new(Context* context, File_Pos file_pos,
                                                AST_Identifier* identifier,
                                                BUF(AST_Type_Spec*) arg_type_specs);
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
                                              AST_Scope* arg_scope, const char* name);
    AST_Type_Spec* ast_type_spec_typeof_new(Context* context, File_Pos file_pos,
                                            AST_Expression* expr);
    AST_Type_Spec* ast_type_spec_from_type_new(Context* context, File_Pos file_pos,
                                               AST_Type* type);
    AST_Type_Spec* ast_type_spec_poly_func_arg_new(Context* context, File_Pos file_pos,
                                                   AST_Identifier* identifier);
    AST_Type_Spec* ast_type_spec_mrv_new(Context* context, File_Pos file_pos,
                                         BUF(AST_Type_Spec*) specs,
                                         BUF(AST_Directive*) directives);
    AST_Type_Spec* ast_type_spec_vararg_new(Context* context, File_Pos file_pos);

	AST_Scope* ast_scope_new(Context* context, AST_Scope* parent_scope, AST_Module* module,
		                     bool is_module_scope, uint64_t line);

    AST_Identifier* find_overload(AST_Type* type, AST_Overload_Operator_Kind op);
    AST_Overload_Operator_Kind binary_op_to_overload_op(AST_Binop_Kind binop);

	void ast_scope_push_declaration(AST_Scope* scope, AST_Declaration* declaration);
	AST_Declaration* ast_scope_find_declaration(Context* context, AST_Scope* scope,
                                                const Atom& ident_atom);

	void ast_module_grow_declaration_hash(AST_Module* module);

    AST_Type* ast_find_or_create_pointer_type(Context* context, AST_Type* base_type);
    AST_Type* ast_find_or_create_array_type(Context* context, AST_Type* base_type,
                                            AST_Expression* count_expr, AST_Scope* scope);
    AST_Type* ast_find_or_create_array_type(Context* context, AST_Type* base_type,
                                            uint64_t count);
	AST_Type* ast_find_or_create_function_type(Context* context, bool is_vararg,
                                               BUF(AST_Type*) arg_types,
		                                       AST_Type* return_type);
    AST_Type* ast_find_or_create_mrv_type(Context* context, BUF(AST_Type*) mrv_types,
                                          BUF(AST_Directive*) directives, AST_Scope* scope);

    uint64_t get_function_type_hash(bool is_varag, BUF(AST_Type*) arg_types,
                                    AST_Type* return_type);
    uint64_t get_mrv_type_hash(BUF(AST_Type*) mrv_types);

    void ast_grow_type_hash(Context* context);
    const char* ast_type_to_string(AST_Type* type);
    void ast_type_to_string(AST_Type* type, String_Builder* string_builder);

    bool is_cmp_op(AST_Binop_Kind binop);
}
