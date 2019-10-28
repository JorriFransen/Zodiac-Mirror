#pragma once

#include "ast.h"

namespace Zodiac
{
    struct Resolver;

    AST_Type* find_or_create_poly_aggregate_type(Resolver* resolver, AST_Declaration* poly_decl,
                                                 AST_Type_Spec* instance_type_spec,
                                                 AST_Scope* scope, AST_Scope* poly_scope);
    AST_Declaration* create_poly_function_declaration(Resolver* resolver,
                                                      AST_Declaration* overload_decl,
                                                      AST_Expression* call_expression,
                                                      AST_Scope* scope, bool* report_failure);

    uint64_t get_poly_overload_match_distance(Resolver* resolver, AST_Type_Spec* arg_type_spec,
                                              AST_Type* arg_type, bool* success);

    void insert_poly_scope(Resolver* resolver, AST_Scope* scope, BUF(AST_Identifier*) poly_idents,
                           BUF(AST_Type_Spec*) poly_type_specs);
    void insert_poly_scope(Resolver* resolver, AST_Scope* scope, AST_Declaration* func_decl,
                           AST_Expression* call_expr, AST_Scope* call_scope);

    struct Poly_Function_Match
    {
        uint64_t distance = UINT64_MAX;
        AST_Declaration* poly_template = nullptr;
    };

    struct Poly_Function_Matches
    {
        BUF(Poly_Function_Match) matches = nullptr;
        uint64_t shortest_distance = UINT64_MAX;
        uint64_t shortest_distance_index = 0;
    };

    Poly_Function_Matches collect_poly_function_matches(Resolver* resolver,
                                                        AST_Declaration* overload_decl,
                                                        AST_Expression* call_expression,
                                                        AST_Scope* scope);
    void free_poly_function_matches(Poly_Function_Matches matches);

    struct Poly_Function_Args
    {
        BUF(AST_Identifier*) identifiers = nullptr;
        BUF(AST_Type_Spec*) type_specs = nullptr;
    };

    void collect_poly_func_arg(Resolver* resolver, Poly_Function_Args* pfa,
                               AST_Type_Spec* arg_decl_ts, AST_Type* arg_type,
                               File_Pos arg_expr_fp, AST_Scope* scope);


    struct Poly_Func_Arg_Idents
    {
        BUF(AST_Identifier*) args = nullptr;
    };

    bool resolve_poly_func_decl(Resolver* resolver, AST_Declaration* func_decl, AST_Scope* scope);
    void collect_poly_func_arg_idents(Poly_Func_Arg_Idents* pfi, AST_Type_Spec* arg_ts);
    void match_poly_func_arg_idents(Poly_Func_Arg_Idents* pfi, AST_Type_Spec* arg_ts);
    bool is_poly_func_arg_match(Poly_Func_Arg_Idents* pfi, AST_Identifier* ident);
}
