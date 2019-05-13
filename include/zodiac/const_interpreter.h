#pragma once

#include "ast.h"

namespace Zodiac
{
    bool const_interpret_bool_expression(AST_Expression* expression, AST_Scope* scope);
    bool const_bool_decl_value(AST_Declaration* declaration, AST_Scope* scope);

    int64_t const_interpret_s64_expression(AST_Expression* expression, AST_Scope* scope);
    int64_t const_interpret_s64_unary_expression(AST_Expression* expression, AST_Scope* scope);

    float const_interpret_float_expression(AST_Expression* expression, AST_Scope* scope);
    float const_interpret_float_unary_expression(AST_Expression* expression, AST_Scope* scope);
}
