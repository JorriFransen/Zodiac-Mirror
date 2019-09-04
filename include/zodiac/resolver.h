#pragma once

#include "ast.h"

namespace Zodiac
{
    enum Resolve_Error_Flag : uint64_t
    {
        RE_FLAG_NONE       = 0,
        RE_FLAG_UNDECLARED = 1 << 0,
    };

    struct Resolve_Error
    {
        Resolve_Error_Flag flags = RE_FLAG_NONE;
        char* message = nullptr;
        File_Pos file_pos = {};
        Atom identifier = {};

        bool auto_gen = false;
        File_Pos auto_gen_file_pos = {};
    };

    struct Resolve_Result
    {
        BUF(Resolve_Error) errors = nullptr;
    };

    struct Resolver
    {
        Context* context = nullptr;
        AST_Module* module = nullptr;

        AST_Statement* current_break_context = nullptr;
        AST_Declaration* current_func_decl = nullptr;

        Resolve_Result result = {};
    };


    void resolver_init(Resolver* resolver, Context* context);
    void resolver_do_initial_scope_population(Resolver* resolver, AST_Module* module,
                                              AST_Scope* scope);
    Resolve_Result resolver_resolve_module(Resolver* resolver, AST_Module* module);
    bool resolver_resolve_declaration(Resolver* resolver, AST_Declaration* declaration,
                                      AST_Scope* scope);
    bool resolver_resolve_statement(Resolver* resolver, AST_Statement* statement,
                                    AST_Scope* scope);
    bool resolver_resolve_expression(Resolver* resolver, AST_Expression* expression,
                                     AST_Scope* scope, AST_Type* suggested_type = nullptr);
    bool resolver_resolve_binary_expression(Resolver* resolver, AST_Expression* expression,
                                            AST_Scope* scope);
    bool resolver_resolve_dot_expression(Resolver* resolver, AST_Expression* dot_expr,
                                         AST_Scope* scope);
    bool resolver_resolve_identifier(Resolver* resolver, AST_Identifier* identifier,
                                     AST_Scope* scope);
    bool resolver_resolve_type_spec(Resolver* resolver, AST_Type_Spec* type_spec,
                                    AST_Type** type_dest, AST_Scope* scope);

    AST_Type* resolver_get_declaration_type(AST_Declaration* decl);

    bool resolver_check_assign_types(Resolver* resolver, AST_Type* lhs, AST_Type* rhs);

    void resolver_transform_to_cast_expression(Resolver* resolver, AST_Expression* expr,
                                               AST_Type* type);

    bool defer_statement_is_legal(Resolver* resolver, AST_Statement* statement);

    void resolver_convert_to_builtin_string(Resolver* resolver, AST_Expression* string_lit_expr);
    AST_Module* resolver_add_import_to_module(Resolver* resolver, AST_Module* module,
                                              Atom module_path, Atom module_name);
    void resolver_replace_aggregate_declaration_with_mutable(Resolver* resolver,
                                                             AST_Declaration** decl_ptr,
                                                             AST_Scope* scope);

    bool resolve_result_has_errors(Resolve_Result* rr);
    void resolve_result_report_errors(Resolve_Result* rr);

    bool resolver_resolve_static_if_declaration(Resolver* resolver, AST_Declaration* if_decl,
                                                AST_Scope* scope);
    void resolver_push_declaration_to_scope(Resolver* resolver, AST_Declaration* decl_to_emit,
                                            AST_Scope* scope);

    AST_Declaration* find_declaration(Context* context, AST_Scope* scope,
		                              AST_Identifier* identifier,
                                      bool allow_import_check = true);

    bool binop_is_cmp(AST_Expression* expression);

    char* run_insert(Resolver* resolver, AST_Expression* call_expression);

    AST_Type* resolver_create_recursive_function_type(Resolver* resolver, AST_Declaration* decl);

    Resolve_Error* resolver_report_error(Resolver* resolver, File_Pos file_pos,
                                         const char* format, ...);
    Resolve_Error* resolver_report_error(Resolver* resolver, File_Pos file_pos,
                                         Resolve_Error_Flag flags, const char* format, va_list args);
    Resolve_Error* resolver_report_error(Resolver* resolver, File_Pos file_pos,
                                         Resolve_Error_Flag flags, const char* format,
                                         ...);
}
