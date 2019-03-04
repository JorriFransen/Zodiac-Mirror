
#include <stdio.h>

#include "ir.h"
#include "builtin.h"

using namespace Zodiac;

int main(int argc, const char** argv)
{
    printf("ir_test\n\n");

    IR_Builder ir_builder;
    ir_builder_init(&ir_builder);

    Context context;
    context_init(&context, &ir_builder.arena);

    IR_Value* add_func = ir_builder_begin_function(&ir_builder, "add", Builtin::type_int);
    {
        IR_Value* add_entry_block = ir_builder_create_block(&ir_builder, "add_entry", add_func);
        ir_builder_set_insert_block(&ir_builder, add_entry_block);
        IR_Value* x_arg = ir_builder_emit_function_arg(&ir_builder, "x", Builtin::type_int);
        IR_Value* y_arg = ir_builder_emit_function_arg(&ir_builder, "y", Builtin::type_int);;

        IR_Value* result = ir_builder_emit_add(&ir_builder, x_arg, y_arg);
        ir_builder_emit_return(&ir_builder, result);
    }
    ir_builder_end_function(&ir_builder, add_func);

    IR_Value* sub_func = ir_builder_begin_function(&ir_builder, "sub", Builtin::type_int);
    {
        IR_Value* sub_entry_block = ir_builder_create_block(&ir_builder, "sub_entry", sub_func);
        ir_builder_set_insert_block(&ir_builder, sub_entry_block);
        IR_Value* x_arg = ir_builder_emit_function_arg(&ir_builder, "x", Builtin::type_int);
        IR_Value* y_arg = ir_builder_emit_function_arg(&ir_builder, "y", Builtin::type_int);

        IR_Value* result = ir_builder_emit_sub(&ir_builder, x_arg, y_arg);
        ir_builder_emit_return(&ir_builder, result);
    }
    ir_builder_end_function(&ir_builder, sub_func);

    IR_Value* fib_func = ir_builder_begin_function(&ir_builder, "fib_recursive", Builtin::type_int);
    {
        IR_Value* fib_entry_block = ir_builder_create_block(&ir_builder, "fib_entry", fib_func);
        IR_Value* then_block = ir_builder_create_block(&ir_builder, "fib_then", fib_func);
        IR_Value* else_block = ir_builder_create_block(&ir_builder, "fib_else", fib_func);

        IR_Value* x_arg = ir_builder_emit_function_arg(&ir_builder, "x", Builtin::type_int);
        IR_Value* one_value = ir_integer_literal(&ir_builder, Builtin::type_int, 1);
        IR_Value* two_value = ir_integer_literal(&ir_builder, Builtin::type_int, 2);

        ir_builder_set_insert_block(&ir_builder, fib_entry_block);
        {
            IR_Value* cond = ir_builder_emit_lt(&ir_builder, x_arg, two_value);
            ir_builder_emit_jmp_if(&ir_builder, cond, then_block);
            ir_builder_emit_jmp(&ir_builder, else_block);
        }

        ir_builder_set_insert_block(&ir_builder, then_block);
        {
            ir_builder_emit_return(&ir_builder, x_arg);
        }

        ir_builder_set_insert_block(&ir_builder, else_block);
        {
            ir_builder_emit_call_arg(&ir_builder, ir_builder_emit_sub(&ir_builder, x_arg, one_value));
            IR_Value* r1 = ir_builder_emit_call(&ir_builder, fib_func);

            ir_builder_emit_call_arg(&ir_builder, ir_builder_emit_sub(&ir_builder, x_arg, two_value));
            IR_Value* r2 = ir_builder_emit_call(&ir_builder, fib_func);

            ir_builder_emit_return(&ir_builder, ir_builder_emit_add(&ir_builder,
                                                                    r1, r2));
        }
    }
    ir_builder_end_function(&ir_builder, fib_func);

    IR_Value* fact_func = ir_builder_begin_function(&ir_builder, "fact_recursive", Builtin::type_int);
    {
        IR_Value* n_arg = ir_builder_emit_function_arg(&ir_builder, "n", Builtin::type_int);

        IR_Value* entry_block = ir_builder_create_block(&ir_builder, "entry", fact_func);
        IR_Value* then_block = ir_builder_create_block(&ir_builder, "then", fact_func);
        IR_Value* else_block = ir_builder_create_block(&ir_builder, "else", fact_func);

        IR_Value* one_value = ir_integer_literal(&ir_builder, Builtin::type_int, 1);

        ir_builder_set_insert_block(&ir_builder, entry_block);
        {
            IR_Value* cond = ir_builder_emit_lteq(&ir_builder, n_arg, one_value);
            ir_builder_emit_jmp_if(&ir_builder, cond, then_block);
            ir_builder_emit_jmp(&ir_builder, else_block);
        }

        ir_builder_set_insert_block(&ir_builder, then_block);
        {
            ir_builder_emit_return(&ir_builder, one_value);
        }

        ir_builder_set_insert_block(&ir_builder, else_block);
        {
            ir_builder_emit_call_arg(&ir_builder, ir_builder_emit_sub(&ir_builder, n_arg, one_value));
            IR_Value* r1 = ir_builder_emit_call(&ir_builder, fact_func);
            ir_builder_emit_return(&ir_builder, ir_builder_emit_mul(&ir_builder,
                                                                    n_arg, r1));
        }
    }
    ir_builder_end_function(&ir_builder, fact_func);

    IR_Value* main_func = ir_builder_begin_function(&ir_builder, "main", Builtin::type_void);
    {
        IR_Value* main_entry_block = ir_builder_create_block(&ir_builder, "main_entry", main_func);
        ir_builder_set_insert_block(&ir_builder, main_entry_block);

        IR_Value* a = ir_integer_literal(&ir_builder, Builtin::type_int, 3);
        IR_Value* b = ir_integer_literal(&ir_builder, Builtin::type_int, 6);
        IR_Value* c = ir_integer_literal(&ir_builder, Builtin::type_int, 11);

        ir_builder_emit_call_arg(&ir_builder, a);
        ir_builder_emit_call_arg(&ir_builder, b);
        IR_Value* r1 = ir_builder_emit_call(&ir_builder, add_func);

        ir_builder_emit_call_arg(&ir_builder, r1);
        ir_builder_emit_call_arg(&ir_builder, c);
        r1 = ir_builder_emit_call(&ir_builder, sub_func);
        IR_Value* r2 = ir_builder_emit_sub(&ir_builder, r1, c);

        ir_builder_emit_call_arg(&ir_builder, ir_integer_literal(&ir_builder,
                                                                 Builtin::type_int,
                                                                 8));
        IR_Value* fib_result = ir_builder_emit_call(&ir_builder, fib_func);

        ir_builder_emit_call_arg(&ir_builder, ir_integer_literal(&ir_builder,
                                                                 Builtin::type_int,
                                                                 8));
        IR_Value* fact_result = ir_builder_emit_call(&ir_builder, fact_func);

        ir_builder_emit_return(&ir_builder, nullptr);
    }
    ir_builder_end_function(&ir_builder, main_func);

    IR_Validation_Result validation = ir_validate(&ir_builder);

    if (!validation.messages)
    {
        ir_builder_print_functions(&ir_builder);
    }
    else
    {
        for (uint64_t i = 0; i < BUF_LENGTH(validation.messages); i++)
        {
            fprintf(stderr, "%s\n", validation.messages[i]);
        }
    }
}
