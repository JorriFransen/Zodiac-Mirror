#include "const_interpreter.h"

#include "builtin.h"
#include "resolver.h"

namespace Zodiac
{
    bool const_interpret_bool_expression(Context* context, AST_Expression* expression,
                                         AST_Scope* scope)
    {
        assert(expression);
        assert(expression->type == Builtin::type_bool);
        assert(scope);

        switch (expression->kind)
        {
            case AST_EXPR_IDENTIFIER:
            {
                AST_Declaration* decl = find_declaration(context, scope, expression->identifier);
                return const_bool_decl_value(context, decl, scope);
                break;
            }

            case AST_EXPR_BOOL_LITERAL:
            {
                return expression->bool_literal.boolean;
                break;
            }

            default: assert(false);
        }

		assert(false);
		return false;
    }

    bool const_bool_decl_value(Context* context, AST_Declaration* declaration, AST_Scope* scope)
    {
        assert(declaration);
        assert(scope);

        switch (declaration->kind)
        {
            case AST_DECL_CONSTANT_VAR:
            {
                return const_interpret_bool_expression(context, 
					                                   declaration->constant_var.init_expression,
                                                       scope);
                break;
            }

            default: assert(false);
        }

		assert(false);
		return false;
    }

    int64_t const_interpret_s64_expression(Context* context, AST_Expression* expression,
                                           AST_Scope* scope)
    {
        assert(context);
        assert(expression);
        assert(expression->type == Builtin::type_int);
        assert(scope);

        switch (expression->kind)
        {
            case AST_EXPR_UNARY:
            {
                return const_interpret_s64_unary_expression(context, expression, scope);
                break;
            }

            case AST_EXPR_INTEGER_LITERAL:
            {
                return expression->integer_literal.u64;
            }

            case AST_EXPR_IDENTIFIER:
            {
                AST_Declaration* decl = find_declaration(context, scope, expression->identifier);
                return const_s64_decl_value(context, decl, scope);
                break;
            }

            case AST_EXPR_DOT:
            {
                assert(expression->dot.declaration);
                AST_Declaration* decl = expression->dot.declaration;
                return const_s64_decl_value(context, decl, scope);
            }

            default: assert(false);
        }

        assert(false);
		return 0;
    }

    int64_t const_interpret_s64_unary_expression(Context* context, AST_Expression* expression,
                                                 AST_Scope* scope)
    {
        assert(context);
        assert(expression);
        assert(expression->type == Builtin::type_int);
        assert(expression->kind == AST_EXPR_UNARY);
        assert(scope);

        switch(expression->unary.op)
        {
            case AST_UNOP_MINUS:
            {
                int64_t value = const_interpret_s64_expression(context,
                                                               expression->unary.operand, scope);
                return -value;
                break;
            }

            default: assert(false);
        }

        assert(false);
		return 0;
    }

    int64_t const_s64_decl_value(Context* context, AST_Declaration* declaration, AST_Scope* scope)
    {
        assert(context);
        assert(declaration);
        assert(scope);

        switch (declaration->kind)
        {
            case AST_DECL_CONSTANT_VAR:
            {
                return const_interpret_s64_expression(context,
                                                      declaration->constant_var.init_expression,
                                                      scope);
                break;
            }

            default: assert(false);
        }

		assert(false);
		return false;
    }

    float const_interpret_float_expression(AST_Expression* expression, AST_Scope* scope)
    {
        assert(expression);
        assert(expression->type == Builtin::type_float);
        assert(scope);

        switch (expression->kind)
        {
            case AST_EXPR_FLOAT_LITERAL:
            {
                return expression->float_literal.r32;
            }

            case AST_EXPR_UNARY:
            {
                return const_interpret_float_unary_expression(expression, scope);
            }

            default: assert(false);
        }
		return 0;
    }

    float const_interpret_float_unary_expression(AST_Expression* expression, AST_Scope* scope)
    {
        assert(expression);
        assert(expression->type == Builtin::type_float);
        assert(expression->kind == AST_EXPR_UNARY);


        switch (expression->unary.op)
        {
            case AST_UNOP_MINUS:
            {
                float value = const_interpret_float_expression(expression->unary.operand, scope);
                return -value;
                break;
            }

            default: assert(false);
        }

        assert(false);
		return 0;
    }
}
