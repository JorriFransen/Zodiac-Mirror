#pragma once

#include "resolver.h"

namespace Zodiac
{
    bool find_or_create_poly_function(Resolver* resolver, AST_Expression* call_expression,
                                      AST_Declaration** func_decl_dest, AST_Scope* scope);
    bool find_or_create_poly_struct_type(Resolver* resolver, AST_Declaration* type_decl,
                                         AST_Type_Spec* type_spec, AST_Type** type_dest,
                                         AST_Scope* scope);
    bool check_poly_struct_arguments(Resolver* resolver, AST_Declaration* type_decl,
                                     AST_Type_Spec* type_spec);
    bool find_poly_function_instance(AST_Declaration* poly_func_decl, uint64_t poly_hash,
                                     AST_Declaration** poly_func_decl_dest);
    bool find_poly_struct_instance(AST_Declaration* type_decl, uint64_t poly_hash,
                                               AST_Declaration** decl_dest);
    AST_Declaration* create_poly_function_instance(Resolver* resolver,
                                                   AST_Declaration* poly_func_decl);
    AST_Declaration* create_poly_struct_instance(Resolver* resolver, AST_Declaration* type_decl,
                                                 AST_Scope* scope);

    AST_Declaration* copy_declaration(Context* context, AST_Declaration* declaration);
    AST_Statement* copy_statement(Context* context, AST_Statement* statement,
                                  AST_Scope* block_scope);
    AST_Expression* copy_expression(Context* context, AST_Expression* expression);
    AST_Identifier* copy_identifier(Context* context, AST_Identifier* identifier);
    AST_Type_Spec* copy_type_spec(Context* context, AST_Type_Spec* type_spec);

    void replace_poly_type_specs(AST_Declaration* poly_type_decl, AST_Declaration* type_decl,
                                 AST_Type_Spec* type_spec);
    void maybe_replace_poly_arg(AST_Identifier* poly_name, uint64_t poly_index,
                                AST_Declaration* poly_decl, AST_Type_Spec* poly_args_ts);
    void maybe_replace_poly_arg(AST_Identifier* poly_name, uint64_t poly_index,
                                AST_Type_Spec** poly_type_spec, AST_Type_Spec* poly_args_ts);
}
