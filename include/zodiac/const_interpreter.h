#pragma once

#include "ast.h"

namespace Zodiac
{
    float const_interpret_float_expression(Context* context, AST_Expression* expression,
                                           AST_Scope* scope);
    float const_interpret_float_unary_expression(Context* context, AST_Expression* expression,
                                                 AST_Scope* scope);
    bool const_interpret_bool_expression(Context* context, AST_Expression* expression,
                                         AST_Scope* scope);
    bool const_bool_decl_value(Context* context, AST_Declaration* declaration, AST_Scope* scope);
    bool const_interpret_bool_unary_expression(Context* context, AST_Expression* expression,
                                               AST_Scope* scope);

    int64_t const_interpret_int_expression(Context* context, AST_Expression* expression,
                                           AST_Type* type, AST_Scope* scope);
    int64_t const_interpret_int_unary_expression(Context* context, AST_Expression* expression,
                                                 AST_Scope* scope);
    int64_t const_interpret_int_literal_expression(Context* context, AST_Expression* expression,
                                                   AST_Type* type);
    uint64_t const_interpret_uint_expression(Context* context, AST_Expression* expression,
                                             AST_Type* type, AST_Scope* scope);
    uint64_t const_interpret_uint_literal_expression(Context* context, AST_Expression* expression,
                                                   AST_Type* type);

    int64_t const_interpret_s64_expression(Context* context, AST_Expression* expression,
                                           AST_Scope* scope);
    int64_t const_interpret_s64_unary_expression(Context* context, AST_Expression* expression,
                                                 AST_Scope* scope);
    uint64_t const_interpret_u64_expression(Context* context, AST_Expression* expression,
                                            AST_Scope* scope);
    uint64_t const_interpret_u64_unary_expression(Context* context, AST_Expression* expression,
                                                  AST_Scope* scope);
    int64_t const_int_decl_value(Context* context, AST_Declaration* declaration, AST_Scope* scope);
    uint64_t const_uint_decl_value(Context* context, AST_Declaration* declaration,
                                   AST_Scope* scope);

    float const_interpret_float_expression(AST_Expression* expression, AST_Scope* scope);
    float const_interpret_float_unary_expression(AST_Expression* expression, AST_Scope* scope);

    bool const_expression_value_equal(AST_Expression* a, AST_Expression* b);
}
