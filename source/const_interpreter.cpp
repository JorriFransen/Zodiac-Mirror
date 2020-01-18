#include "const_interpreter.h"

#include "builtin.h"
#include "resolver.h"

namespace Zodiac
{
    float const_interpret_float_expression(Context* context, AST_Expression* expression,
                                           AST_Scope* scope)
    {
        assert(expression->type == Builtin::type_float);

        switch (expression->kind)
        {
            case AST_EXPR_FLOAT_LITERAL:
            {
                return expression->float_literal.r32;
                break;
            }

            case AST_EXPR_UNARY:
            {
                return const_interpret_float_unary_expression(context, expression, scope);
                break;
            }

            default: assert(false);
        }

		assert(false);
		return 0;
    }

    float const_interpret_float_unary_expression(Context* context, AST_Expression* expression,
                                                 AST_Scope* scope)
    {
        assert(expression->type == Builtin::type_float);
        assert(expression->kind == AST_EXPR_UNARY);

        switch (expression->unary.op)
        {
            case AST_UNOP_MINUS:
            {
                return - const_interpret_float_expression(context, expression->unary.operand,
                                                         scope);
                break;
            }
            default: assert(false);
        }

		assert(false);
		return 0;
    }

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

            case AST_EXPR_UNARY:
            {
                return const_interpret_bool_unary_expression(context, expression, scope);
                break;
            }

            case AST_EXPR_INTEGER_LITERAL:
            {
                return expression->integer_literal.u64 != 0;
                break;
            }

            case AST_EXPR_DOT:
            {
                AST_Declaration* decl = expression->dot.declaration;
                return const_bool_decl_value(context, decl, scope);
                break;
            }

            default: assert(false);
        }

		assert(false);
		return false;
    }

    bool const_interpret_bool_unary_expression(Context* context, AST_Expression* expression,
                                               AST_Scope* scope)
    {
        assert(expression->kind == AST_EXPR_UNARY);
        assert(expression->type == Builtin::type_bool);

        switch (expression->unary.op)
        {
            case AST_UNOP_NOT:
            {
                return !const_interpret_bool_expression(context, expression->unary.operand,
                                                        scope);
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

    int64_t const_interpret_int_expression(Context* context, AST_Expression* expression,
                                           AST_Type* type, AST_Scope* scope)
    {
        assert(context);
        assert(expression);
        // assert(expression->type);
        // assert(expression->type->flags & AST_TYPE_FLAG_INT);
        assert(type);
        assert((type->flags & AST_TYPE_FLAG_INT) ||
               (type->kind == AST_TYPE_ENUM &&
                (type->aggregate_type.base_type->flags & AST_TYPE_FLAG_INT)));
        // assert(type->flags & AST_TYPE_FLAG_SIGNED);
        assert(scope);

        switch (expression->kind)
        {
            case AST_EXPR_IDENTIFIER:
            {
                AST_Declaration* decl = find_declaration(context, scope, expression->identifier);
                return const_int_decl_value(context, decl, scope);
                break;
            }

            case AST_EXPR_INTEGER_LITERAL:
            {
                return const_interpret_int_literal_expression(context, expression, type);
            }


            case AST_EXPR_UNARY:
            {
                return const_interpret_int_unary_expression(context, expression, scope);
            }

            case AST_EXPR_BINARY:
            {
                uint64_t lhs = const_interpret_int_expression(context, expression->binary.lhs,
                                                               expression->binary.lhs->type,
                                                               scope);
                uint64_t rhs = const_interpret_int_expression(context, expression->binary.rhs,
                                                               expression->binary.rhs->type,
                                                               scope);

                switch (expression->binary.op)
                {
                    case AST_BINOP_ADD: return lhs + rhs;
                    case AST_BINOP_SUB: return lhs - rhs;
                    case AST_BINOP_MUL: return lhs * rhs;
                    case AST_BINOP_DIV: return lhs / rhs;
                    case AST_BINOP_EQ: return lhs == rhs;
                    case AST_BINOP_AND_AND: return lhs && rhs;
                    case AST_BINOP_OR_OR: return lhs || rhs;
					case AST_BINOP_AND: return lhs & rhs;
					case AST_BINOP_OR: return lhs | rhs;
                    case AST_BINOP_MOD: return lhs % rhs;

                    default: assert(false);
                }
                break;
            }

            case AST_EXPR_BOOL_LITERAL:
            {
                return (int64_t)expression->bool_literal.boolean;
                break;
            }

            case AST_EXPR_DOT:
            {
                AST_Declaration* decl = expression->dot.declaration;
                return const_int_decl_value(context, decl, scope);
                break;
            }

            case AST_EXPR_SIZEOF:
            {
                assert(expression->sizeof_expr.type_spec->type);
                return expression->sizeof_expr.byte_size;
                break;
            }

            case AST_EXPR_TYPE:
            {
                assert(expression->type_expr_type);
                maybe_register_type_info(context, expression->type_expr_type);
                return expression->type_expr_type->info_index;
                break;
            }

            default: assert(false);
        }

        assert(false);
		return 0;
    }

    int64_t const_interpret_int_unary_expression(Context* context, AST_Expression* expression,
                                                 AST_Scope* scope)
    {
        assert(context);
        assert(expression);
        assert(scope);

        if (expression->type->flags & AST_TYPE_FLAG_SIGNED)
        {
            return const_interpret_s64_unary_expression(context, expression, scope);
        }
        else
        {
            return const_interpret_u64_unary_expression(context, expression, scope);
        }

		assert(false);
		return 0;
    }

    int64_t const_interpret_int_literal_expression(Context* context, AST_Expression* expression,
                                                   AST_Type* type)
    {
        assert(context);
        assert(expression);
        assert(expression->type);
        assert((expression->type->flags & AST_TYPE_FLAG_INT) ||
               (expression->type->kind == AST_TYPE_ENUM &&
                (expression->type->aggregate_type.base_type->flags & AST_TYPE_FLAG_INT)));
        assert(type);
        assert(type->flags & AST_TYPE_FLAG_INT ||
               type->kind == AST_TYPE_ENUM);
        // assert(type->flags & AST_TYPE_FLAG_SIGNED);

        if (type->kind == AST_TYPE_ENUM)
        {
            type = type->aggregate_type.base_type;
        }

        if (type == Builtin::type_s64)
        {
            return (uint64_t)expression->integer_literal.u64;
        }
        else if (type == Builtin::type_u64)
        {
            return (uint64_t)expression->integer_literal.u64;
        }
        else if (type == Builtin::type_u32)
        {
            return (uint32_t)expression->integer_literal.u64;
        }
        else if (type == Builtin::type_s32)
        {
            return (int32_t)expression->integer_literal.u64;
        }
		else if (type == Builtin::type_u16)
		{
			return (uint16_t)expression->integer_literal.u64;
		}
        else if (type == Builtin::type_s16)
        {
            return (int16_t)expression->integer_literal.u64;
        }

        assert(false);
		return 0;
    }

    uint64_t const_interpret_uint_expression(Context* context, AST_Expression* expression,
                                             AST_Type* type, AST_Scope* scope)
    {
        assert(context);
        assert(expression);
        // assert(expression->type);
        // assert(expression->type->flags & AST_TYPE_FLAG_INT);
        assert(type);
        assert(type->flags & AST_TYPE_FLAG_INT);
        // assert(type->flags & AST_TYPE_FLAG_SIGNED);
        assert(scope);

        switch (expression->kind)
        {
            case AST_EXPR_IDENTIFIER:
            {
                AST_Declaration* decl = find_declaration(context, scope, expression->identifier);
                return const_uint_decl_value(context, decl, scope);
                break;
            }

            case AST_EXPR_INTEGER_LITERAL:
            {
                return const_interpret_uint_literal_expression(context, expression, type);
            }

            case AST_EXPR_BINARY:
            {
                uint64_t lhs = const_interpret_uint_expression(context, expression->binary.lhs,
                                                               expression->binary.lhs->type,
                                                               scope);
                uint64_t rhs = const_interpret_uint_expression(context, expression->binary.rhs,
                                                               expression->binary.rhs->type,
                                                               scope);

                switch (expression->binary.op)
                {
                    case AST_BINOP_MUL:
                    {
                        return lhs * rhs;
                        break;
                    }

                    case AST_BINOP_EQ:
                    {
                        return lhs == rhs;
                        break;
                    }

                    default: assert(false);
                }
                break;
            }

            case AST_EXPR_SIZEOF:
            {
                return expression->sizeof_expr.byte_size;
            }

            case AST_EXPR_DOT:
            {
                AST_Declaration* decl = expression->dot.declaration;
                return const_uint_decl_value(context, decl, scope);
                break;
            }

            default: assert(false);
        }

        assert(false);
		return 0;
    }

    uint64_t const_interpret_uint_literal_expression(Context* context, AST_Expression* expression,
                                                   AST_Type* type)
    {
        assert(context);
        assert(expression);
        assert(expression->type);
        assert(expression->type->flags & AST_TYPE_FLAG_INT);
        assert(type);
        assert(type->flags & AST_TYPE_FLAG_INT);
        // assert(type->flags & AST_TYPE_FLAG_SIGNED);

        if (type == Builtin::type_u64 ||
            type == Builtin::type_s64)
        {
            return (uint64_t)expression->integer_literal.u64;
        }
        else if (type == Builtin::type_u32)
        {
            return (uint32_t)expression->integer_literal.u64;
        }

        assert(false);
		return 0;
    }

    int64_t const_interpret_s64_expression(Context* context, AST_Expression* expression,
                                           AST_Scope* scope)
    {
        assert(context);
        assert(expression);
        if (expression->type->kind != AST_TYPE_ENUM)
        {
            assert(expression->type->flags & AST_TYPE_FLAG_INT);
            assert(expression->type->flags & AST_TYPE_FLAG_SIGNED);
            assert(expression->type->bit_size <= 64);
        }
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
                return const_int_decl_value(context, decl, scope);
                break;
            }

            case AST_EXPR_DOT:
            {
                assert(expression->dot.declaration);
                AST_Declaration* decl = expression->dot.declaration;
                return const_int_decl_value(context, decl, scope);
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
        assert(expression->type->flags & AST_TYPE_FLAG_INT);
        assert(expression->type->flags & AST_TYPE_FLAG_SIGNED);
        assert(expression->type->bit_size <= 64);
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

    uint64_t const_interpret_u64_expression(Context* context, AST_Expression* expression,
                                            AST_Scope* scope)
    {
        assert(context);
        assert(expression);
        assert(scope);

        switch (expression->kind)
        {
            case AST_EXPR_UNARY:
            {
                return const_interpret_u64_unary_expression(context, expression, scope);
                break;
            }

            case AST_EXPR_INTEGER_LITERAL:
            {
                return expression->integer_literal.u64;
            }

            case AST_EXPR_IDENTIFIER:
            {
                AST_Declaration* decl = find_declaration(context, scope, expression->identifier);
                return const_int_decl_value(context, decl, scope);
                break;
            }

            case AST_EXPR_DOT:
            {
                assert(expression->dot.declaration);
                AST_Declaration* decl = expression->dot.declaration;
                return const_int_decl_value(context, decl, scope);
            }

            default: assert(false);
        }

        assert(false);
		return 0;
    }

    uint64_t const_interpret_u64_unary_expression(Context* context, AST_Expression* expression,
                                                  AST_Scope* scope)
    {
        assert(context);
        assert(expression);
        assert(expression->kind == AST_EXPR_UNARY);
        assert(scope);

        switch(expression->unary.op)
        {
            case AST_UNOP_MINUS:
            {
                uint64_t value = const_interpret_u64_expression(context,
                                                                expression->unary.operand, scope);
                return -(int64_t)value;
                break;
            }

            case AST_UNOP_NOT:
            {
                return !const_interpret_u64_expression(context, expression->unary.operand, scope);
            }

			case AST_UNOP_BIN_NOT:
			{
				return ~const_interpret_u64_expression(context, expression->unary.operand, scope);
			}

            default: assert(false);
        }

        assert(false);
		return 0;
    }

    int64_t const_int_decl_value(Context* context, AST_Declaration* declaration, AST_Scope* scope)
    {
        assert(context);
        assert(declaration);
        assert(scope);

        switch (declaration->kind)
        {
            case AST_DECL_CONSTANT_VAR:
            {
                auto init_expr = declaration->constant_var.init_expression;
                return const_interpret_int_expression(context, init_expr, init_expr->type,
                                                      scope);
                break;
            }

            default: assert(false);
        }

		assert(false);
		return false;
    }

    uint64_t const_uint_decl_value(Context* context, AST_Declaration* declaration,
                                   AST_Scope* scope)
    {
        assert(context);
        assert(declaration);
        assert(scope);

        switch (declaration->kind)
        {
            case AST_DECL_CONSTANT_VAR:
            {
                auto init_expr = declaration->constant_var.init_expression;
                return const_interpret_uint_expression(context, init_expr, init_expr->type,
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

    bool const_expression_value_equal(Resolver* resolver, AST_Expression* a, AST_Expression* b)
    {
        assert(a->type);
        assert(b->type);

        if (a->type != b->type)
        {
            return false;
        }

        if (a->flags & AST_EXPR_FLAG_TYPE)
        {
            assert(b->flags & AST_EXPR_FLAG_TYPE ||
                   b->type == Builtin::type_Type);
        }
        else if (b->flags & AST_EXPR_FLAG_TYPE) assert(false);

        switch (a->kind)
        {
            case AST_EXPR_INTEGER_LITERAL:
            {
                if (a->kind == b->kind)
                {
                    return a->integer_literal.u64 == b->integer_literal.u64;
                }
                else assert(false);
                break;
            }

            case AST_EXPR_TYPE:
            {
                if (a->kind == b->kind)
                {
                    return a->type_expr_type == b->type_expr_type;
                }
                else
                {
                    assert(b->flags & AST_EXPR_FLAG_TYPE ||
                           b->type == Builtin::type_Type);

                    uint64_t a_id = a->type_expr_type->info_index;
                    uint64_t b_id = 0;

                    switch (b->kind)
                    {
                        case AST_EXPR_INTEGER_LITERAL:
                        {
                            b_id = b->integer_literal.u64;
                            break;
                        }

                        default: assert(false);
                    }

                    assert(a_id);
                    assert(b_id);
                    return a_id == b_id;
                }
            }

            case AST_EXPR_IDENTIFIER:
            {
                if (a->kind == b->kind)
                {
                    assert(a->identifier->declaration);
                    assert(b->identifier->declaration);

                    auto a_decl = a->identifier->declaration;
                    auto b_decl = b->identifier->declaration;

                    return a_decl == b_decl;
                }
                else
                {
                    assert(false);
                }
            }

            default: assert(false);
        }

        assert(false);
        return false;
    }

    AST_Type* const_interpret_type_expr(Resolver* resolver, AST_Expression* expr)
    {
        assert(expr->flags & AST_EXPR_FLAG_TYPE);
        assert(expr->flags & AST_EXPR_FLAG_RESOLVED);

        switch (expr->kind)
        {
            case AST_EXPR_IDENTIFIER:
            {
                assert(false);
                break;
            }

            case AST_EXPR_TYPE:
            {
                assert(expr->type_expr_type);
                return expr->type_expr_type;
                break;
            }

            default: assert(false);
        }
    }
}
