#pragma once

#include "zodiac.h"
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

    struct Resolver
    {
        Context* context = nullptr;
        AST_Module* module = nullptr;

        bool done = false;
        bool override_done = false;
        bool progressed_on_last_cycle = false;

        uint64_t unresolved_decl_count = 0;
        uint64_t unresolved_decl_count_last_cycle = UINT64_MAX;

        uint64_t undeclared_decl_count = 0;
        uint64_t undeclared_decl_count_last_cycle = UINT64_MAX;

        bool silent = false;
        bool import_error = false;
        bool resolving_auto_gen = false;
        File_Pos auto_gen_file_pos = {};

        BUF(AST_Declaration*) unresolved_decls = nullptr;

		BUF(Resolve_Error) errors = nullptr;

        AST_Declaration* current_func_decl = nullptr;
    };

    void resolver_init(Resolver* resolver, Context* context, AST_Module* module);

    void resolver_do_cycle(Resolver* resolver);

    bool try_resolve_declaration(Resolver* resolver, AST_Declaration* declaration,
                                 AST_Scope* scope, AST_Scope* poly_scope = nullptr);
    static bool try_resolve_function_declaration(Resolver* resolver, AST_Declaration* declaration,
                                                 AST_Scope* scope,
                                                 AST_Scope* poly_scope = nullptr);
    static bool try_resolve_mutable_declaration(Resolver* resolver, AST_Declaration* declaration,
                                                AST_Scope* scope, AST_Scope* poly_scope = nullptr);
    static bool try_resolve_constant_var_declaration(Resolver* resolver,
                                                     AST_Declaration* declaration,
                                                     AST_Scope* scope);
    static bool try_resolve_static_if_declaration(Resolver* resolver, AST_Declaration* declaration,
                                                  AST_Scope* scope);
    static bool try_resolve_block_declaration(Resolver* resolver, AST_Declaration* declaration,
                                              AST_Scope* scope);
    static bool try_resolve_static_assert_declaration(Resolver* resolver,
                                                      AST_Declaration* declaration,
                                                      AST_Scope* scope);
    static bool try_resolve_import_declaration(Resolver* resolver, AST_Declaration* declaration,
                                               AST_Scope* scope);
    static bool try_resolve_aggregate_type_declaration(Resolver* resolver,
                                                       AST_Declaration* declaration,
                                                       AST_Scope* scope,
                                                       BUF(AST_Declaration*) _pointers_to_self,
                                                       AST_Identifier* _self_ident);
    static bool try_resolve_enum_aggregate_type_declaration(Resolver* resolver, AST_Declaration* declaration,
                                                            AST_Scope* scope);
	static bool try_resolve_typedef_declaration(Resolver* resolver, AST_Declaration* declaration,
		                                        AST_Scope* scope);
    static bool try_resolve_using_declaration(Resolver* resolver, AST_Declaration* declaration,
                                              AST_Scope* scope);
    static bool try_resolve_insert_declaration(Resolver* resolver, AST_Declaration* declaration,
                                               AST_Scope* scope);

    static bool try_resolve_statement(Resolver* resolver, AST_Statement* statement,
                                      AST_Scope* scope, AST_Statement* break_context);
    static bool try_resolve_return_statement(Resolver* resolver, AST_Statement* statement,
                                             AST_Scope* scope);
    static bool try_resolve_if_statement(Resolver* resolver, AST_Statement* statement,
                                         AST_Scope* scope, AST_Statement* break_context);
    static bool try_resolve_insert_statement(Resolver* resolver, AST_Statement* statement, AST_Scope* scope,
                                             AST_Statement* break_context);

    bool try_resolve_expression(Resolver* resolver, AST_Expression* expression,
                                       AST_Scope* scope,
                                       AST_Type* suggested_type = nullptr);
    static bool try_resolve_call_expression(Resolver* resolver, AST_Expression* expression,
                                            AST_Scope* scope);
    static bool try_resolve_boolean_literal_expression(Resolver* resolver,
                                                       AST_Expression* expression);
    static bool try_resolve_null_literal_expression(Resolver* resolver,
                                                    AST_Expression* expression);
    static bool try_resolve_string_literal_expression(Resolver* resolver,
                                                      AST_Expression* expression);
    static bool try_resolve_integer_literal_expression(Resolver* resolver,
                                                       AST_Expression* expression,
                                                       AST_Type* suggested_type);
    static bool try_resolve_float_literal_expression(Resolver* resolver,
                                                     AST_Expression* expression,
                                                     AST_Type* suggested_type);
    static bool try_resolve_character_literal_expression(Resolver* resolver,
                                                         AST_Expression* expression);
    static bool try_resolve_compound_literal_expression(Resolver* resolver,
                                                        AST_Expression* expression,
                                                        AST_Scope* scope,
                                                        AST_Type* suggested_type = nullptr);
    static bool try_resolve_array_length_expression(Resolver* resolver,
                                                    AST_Expression* expression, AST_Scope* scope);
    static bool try_resolve_identifier_expression(Resolver* resolver, AST_Expression* expression,
                                                  AST_Scope* scope);
    static bool try_resolve_binary_expression(Resolver* resolver, AST_Expression* expression,
                                              AST_Scope* scope);
    static bool try_resolve_unary_expression(Resolver* resolver, AST_Expression* expression,
                                             AST_Scope* scope, AST_Type* suggested_type);
    static bool try_resolve_dot_expression(Resolver* resolver, AST_Expression* expression,
        AST_Scope* scope);
	static bool try_resolve_cast_expression(Resolver* resolver, AST_Expression* expression,
		AST_Scope* scope);

    static bool try_resolve_identifier(Resolver* resolver, AST_Identifier* identifier,
                                       AST_Scope* scope);

    bool try_resolve_type_spec(Resolver* resolver, AST_Type_Spec* type_spec,
                               AST_Type** type_dest, AST_Scope* scope,
                               AST_Scope* poly_scope = nullptr);
    bool try_resolve_poly_type_spec_args(Resolver* resolver, AST_Type_Spec* type_spec,
                                         AST_Scope* scope);

    AST_Module* resolver_add_import_to_module(Resolver* resolver, AST_Module* module,
                                              const Atom& module_path,
                                              const Atom& module_name);

    static bool is_valid_integer_conversion(Resolver* resolver, AST_Type* dest_type,
                                            AST_Type* source_type);

    AST_Type* create_struct_type(Resolver* resolver, AST_Identifier* identifier,
                                 BUF(AST_Declaration*) member_decls,
                                 BUF(AST_Overload_Directive) overloads);
    AST_Type* create_union_type(Resolver* resolver, AST_Identifier* identifier,
                                BUF(AST_Declaration*) member_decls,
                                BUF(AST_Overload_Directive) overloads);
    AST_Type* create_enum_type(Resolver* resolver, AST_Identifier* identifier,
                               AST_Type* member_type,
                               BUF(AST_Declaration*) member_decls);

    AST_Declaration* find_declaration(Context* context, AST_Scope* scope,
		                              AST_Identifier* identifier,
                                      bool allow_import_check = true);

    bool is_valid_integer_promotion(AST_Type* source_type, AST_Type* target_type);

    bool function_signatures_match(Resolver* resolver, AST_Declaration* decl_a,
                                   AST_Declaration* decl_b, AST_Scope* scope);
    AST_Declaration* find_overload_signature_match(Resolver* resolver,
                                                   AST_Declaration* overload_decl,
                                                   AST_Expression* call_expr, AST_Scope* scope);
    void add_overload(Resolver* resolver, AST_Declaration* container, AST_Declaration* overload);

    bool defer_statement_is_legal(Resolver* resolver, AST_Statement* statement);
    void add_defer_statement(AST_Scope* scope, AST_Statement* statement);

    AST_Expression* try_resolve_index_overload(Resolver* resolver, AST_Expression* subscript_expr,
                                               AST_Identifier* overload_ident, AST_Scope* scope);
    AST_Expression* try_resolve_binary_overload(Resolver* resolver, AST_Expression* bin_expr,
                                                AST_Identifier* overload_ident, AST_Scope* scope);

    bool try_replace_nested_aggregate_with_mutable(Resolver* resolver,
                                                   AST_Declaration* nested_aggregate,
                                                   AST_Scope* scope,
                                                   BUF(AST_Declaration*) pointers_to_self,
                                                   AST_Identifier* self_ident);

    char* run_insert(Resolver* resolver, AST_Expression* call_expression);

    bool match_builtin_function(Resolver* resolver, AST_Expression* call_expression, AST_Scope* scope);

    static void report_undeclared_identifier(Resolver* resolver, File_Pos file_pos,
                                             AST_Identifier* identifier);
    static void report_undeclared_identifier(Resolver* resolver, File_Pos file_pos,
                                             AST_Module* module,
                                             AST_Identifier* identifier);
    Resolve_Error* resolver_report_error(Resolver* resolver, File_Pos file_pos,
                                      const char* format, ...);
    Resolve_Error* resolver_report_error(Resolver* resolver, File_Pos file_pos,
                                      Resolve_Error_Flag flags, const char* format, va_list args);
    Resolve_Error* resolver_report_error(Resolver* resolver, File_Pos file_pos,
                                      Resolve_Error_Flag flags, const char* format,
                                      ...);
    void resolver_report_errors(Resolver* resolver);
}
