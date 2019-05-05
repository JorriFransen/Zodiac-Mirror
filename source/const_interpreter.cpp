#include "const_interpreter.h"

#include "builtin.h"
#include "resolver.h"

namespace Zodiac
{
    bool const_interpret_bool_expression(AST_Expression* expression, AST_Scope* scope)
    {
        assert(expression);
        assert(expression->type == Builtin::type_bool);
        assert(scope);

        switch (expression->kind)
        {
            case AST_EXPR_IDENTIFIER:
            {
                AST_Declaration* decl = find_declaration(scope, expression->identifier);
                return const_bool_decl_value(decl, scope);
                break;
            }

            case AST_EXPR_BOOL_LITERAL:
            {
                return expression->bool_literal.boolean;
                break;
            }

            default: assert(false);
        }
    }

    bool const_bool_decl_value(AST_Declaration* declaration, AST_Scope* scope)
    {
        assert(declaration);
        assert(scope);

        switch (declaration->kind)
        {
            case AST_DECL_CONSTANT_VAR:
            {
                return const_interpret_bool_expression(declaration->constant_var.init_expression,
                                                       scope);
                break;
            }

            default: assert(false);
        }
    }
}
