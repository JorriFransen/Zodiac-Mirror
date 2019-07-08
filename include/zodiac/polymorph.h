#pragma once

#include "resolver.h"

namespace Zodiac
{
    struct Poly_Type_Spec_Replacement
    {
        Atom poly_name = {};
        AST_Type_Spec* replacement = nullptr;
    };

    BUF(Poly_Type_Spec_Replacement) find_type_spec_replacements(Context* context,
                                                                AST_Declaration* poly_decl,
                                                                AST_Expression* call_expr);
    void find_type_spec_replacements(Context* context, AST_Type_Spec* type_spec,
                                     File_Pos file_pos, AST_Type* given_type,
                                     BUF(Poly_Type_Spec_Replacement)* replacements);
    Atom find_poly_name(AST_Type_Spec* poly_ts);

    bool find_or_create_poly_function(Resolver* resolver, AST_Expression* call_expression,
                                      AST_Declaration** func_decl_dest, AST_Scope* scope);
    bool find_or_create_poly_aggregate_type(Resolver* resolver, AST_Declaration* type_decl,
                                            AST_Type_Spec* type_spec, AST_Type** type_dest,
                                            AST_Scope* scope);
    bool check_poly_aggregate_arguments(Resolver* resolver, AST_Declaration* type_decl,
                                        AST_Type_Spec* type_spec);
    bool find_poly_function_instance(AST_Declaration* poly_func_decl, uint64_t poly_hash,
                                     AST_Declaration** poly_func_decl_dest);
    bool find_poly_aggregate_instance(AST_Declaration* type_decl, uint64_t poly_hash,
                                      AST_Declaration** decl_dest);
    AST_Declaration* create_poly_function_instance(Resolver* resolver,
                                                   AST_Declaration* poly_func_decl);
    AST_Declaration* create_poly_aggregate_instance(Resolver* resolver, AST_Declaration* type_decl,
                                                    AST_Scope* scope);

    AST_Declaration* copy_declaration(Context* context, AST_Declaration* declaration);
    AST_Statement* copy_statement(Context* context, AST_Statement* statement,
                                  AST_Scope* block_scope);
    AST_Expression* copy_expression(Context* context, AST_Expression* expression);
    AST_Identifier* copy_identifier(Context* context, AST_Identifier* identifier);
    AST_Type_Spec* copy_type_spec(Context* context, AST_Type_Spec* type_spec);
    AST_Aggregate_Declaration* copy_aggregate_declaration(Context* context,
                                                          AST_Aggregate_Declaration* agg_decl);

    bool poly_type_spec_matches_type(AST_Type_Spec* poly_type_spec, AST_Type* type);

    void replace_poly_type_specs(AST_Declaration* poly_decl_instance,
                                 BUF(Poly_Type_Spec_Replacement) replacements);
    void replace_poly_type_specs(AST_Statement* statement,
                                 BUF(Poly_Type_Spec_Replacement) replacements);
    void replace_poly_type_specs(AST_Expression* expression,
                                 BUF(Poly_Type_Spec_Replacement) replacements);
    void replace_poly_type_specs(BUF(AST_Identifier*) poly_type_names, AST_Declaration* type_decl,
                                 BUF(AST_Type_Spec*) given_type_specs);
    void maybe_replace_poly_type_spec(AST_Type_Spec** target_ts,
                                      BUF(Poly_Type_Spec_Replacement) repacements);
    void maybe_replace_poly_arg(AST_Identifier* poly_name, AST_Declaration* poly_decl,
                                AST_Type_Spec* given_type_spec);
    void maybe_replace_poly_arg(AST_Identifier* poly_name, AST_Type_Spec** poly_type_spec,
                                AST_Type_Spec* given_type_spec);
}
