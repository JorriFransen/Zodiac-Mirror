#pragma once

#include "zodiac.h"
#include "ast.h"

namespace Zodiac
{
    struct Resolve_Error
    {
        char* message = nullptr;
        File_Pos file_pos = {};
    };

    struct Resolver
    {
        Context* context = nullptr;
        AST_Module* module = nullptr;

        bool done = false;
        bool progressed_on_last_cycle = false;

        uint64_t unresolved_decl_count = 0;
        uint64_t unresolved_decl_count_last_cycle = UINT64_MAX;

        uint64_t undeclared_decl_count = 0;
        uint64_t undeclared_decl_count_last_cycle = UINT64_MAX;

        BUF(Resolve_Error) errors;

        AST_Declaration* current_func_decl = nullptr;
    };

    void resolver_init(Resolver* resolver, Context* context, AST_Module* module);

    void resolver_do_cycle(Resolver* resolver);

    static bool try_resolve_declaration(Resolver* resolver, AST_Declaration* declaration,
                                        AST_Scope* scope);
    static bool try_resolve_function_declaration(Resolver* resolver, AST_Declaration* declaration,
                                                 AST_Scope* scope);
    static bool try_resolve_mutable_declaration(Resolver* resolver, AST_Declaration* declaration,
                                                AST_Scope* scope);

    static bool try_resolve_statement(Resolver* resolver, AST_Statement* statement,
                                      AST_Scope* scope);
    static bool try_resolve_return_statement(Resolver* resolver, AST_Statement* statement,
                                             AST_Scope* scope);
    static bool try_resolve_if_statement(Resolver* resolver, AST_Statement* statement,
                                         AST_Scope* scope);

    static bool try_resolve_expression(Resolver* resolver, AST_Expression* expression, AST_Scope* scope);
    static bool try_resolve_call_expression(Resolver* resolver, AST_Expression* expression,
                                            AST_Scope* scope);
    static bool try_resolve_literal_expression(Resolver* resolver, AST_Expression* expression);
    static bool try_resolve_identifier_expression(Resolver* resolver, AST_Expression* expression,
                                                  AST_Scope* scope);
    static bool try_resolve_binary_expression(Resolver* resolver, AST_Expression* expression,
                                              AST_Scope* scope);

    static bool try_resolve_type_spec(Resolver* resolver, AST_Type_Spec* type_spec, AST_Type** type_dest,
                                      AST_Scope* scope);

    AST_Declaration* find_declaration(AST_Scope* scope, AST_Identifier* identifier);

    static void report_undeclared_identifier(Resolver* resolver, File_Pos file_pos, AST_Identifier* identifier);
    static void resolver_report_error(Resolver* resolver, File_Pos file_pos, const char* format, ...);
    void resolver_report_errors(Resolver* resolver);
}
