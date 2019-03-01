
#include <stdio.h>

#include "ir.h"
#include "builtin.h"

using namespace Zodiac;

int main(int argc, const char** argv)
{
    printf("ir_test\n\n");

    Arena context_arena = arena_create(MB(1));

    Context context;
    context_init(&context, &context_arena);

    init_builtin_types(&context);
    assert(Builtin::type_int);
    assert(Builtin::type_void);

    IR_Builder ir_builder;
    ir_builder_init(&ir_builder);

    IR_Function* add_func = ir_builder_create_function(&ir_builder, "add",
                                                       Builtin::type_int);
    {
        IR_Value* x_value = ir_function_add_argument(&ir_builder, add_func, "x",
                                                     Builtin::type_int);
        IR_Value* y_value = ir_function_add_argument(&ir_builder, add_func, "y",
                                                     Builtin::type_int);

        IR_Block* add_func_entry_block = ir_builder_create_block(&ir_builder, "add_entry");
        IR_Value* result_value = ir_builder_insert_add(&ir_builder, add_func_entry_block,
                                                    x_value, y_value);
        ir_builder_insert_return(&ir_builder, add_func_entry_block,  result_value);

        ir_function_add_block(add_func, add_func_entry_block);
    }

    IR_Function* fib_func = ir_builder_create_function(&ir_builder, "fib_recursive",
                                                       Builtin::type_int);

    IR_Function* main_func = ir_builder_create_function(&ir_builder, "main",
                                                        Builtin::type_void);
    {
        IR_Block* main_func_entry_block = ir_builder_create_block(&ir_builder, "main_entry");
        ir_function_add_block(main_func, main_func_entry_block);
        IR_Value* x_value = ir_builder_get_literal(&ir_builder, Builtin::type_int, 3);
        IR_Value* y_value = ir_builder_get_literal(&ir_builder, Builtin::type_int, 9);
        IR_Value* z_value = ir_builder_get_literal(&ir_builder, Builtin::type_int, 6);
        IR_Value* add_args[] = { x_value, y_value };
        IR_Value* a = ir_builder_insert_call(&ir_builder, main_func_entry_block,
                                            add_func, add_args, 2);


        IR_Value* b = ir_builder_insert_add(&ir_builder, main_func_entry_block,
                                        z_value, a);

        IR_Value* fib_arg = ir_builder_get_literal(&ir_builder, Builtin::type_int, 8);
        IR_Value* c = ir_builder_insert_call(&ir_builder, main_func_entry_block,
                                             fib_func, &fib_arg, 1);

        ir_builder_insert_return(&ir_builder, main_func_entry_block);
    }

    // fib_func
    {
        IR_Block* fib_func_entry_block = ir_builder_create_block(&ir_builder, "fib_entry");
        IR_Block* then_block = ir_builder_create_block(&ir_builder, "then_block");
        IR_Block* else_block = ir_builder_create_block(&ir_builder, "else_block");

        ir_function_add_block(fib_func, fib_func_entry_block);
        ir_function_add_block(fib_func, then_block);
        ir_function_add_block(fib_func, else_block);

        IR_Value* x_value = ir_function_add_argument(&ir_builder, add_func, "x",
                                                     Builtin::type_int);

        IR_Value* one_value = ir_builder_get_literal(&ir_builder, Builtin::type_int, 1);
        IR_Value* two_value = ir_builder_get_literal(&ir_builder, Builtin::type_int, 2);

        IR_Value* cond_value = ir_builder_insert_lt(&ir_builder, fib_func_entry_block,
                                                    x_value, two_value);

        ir_builder_insert_cond_jump(&ir_builder, fib_func_entry_block, cond_value,
                                    then_block, else_block);

        // then
        ir_builder_insert_return(&ir_builder, then_block, x_value);

        // else
        IR_Value* x1_value = ir_builder_insert_sub(&ir_builder, else_block,
                                                   x_value, one_value);
        IR_Value* x2_value = ir_builder_insert_sub(&ir_builder, else_block,
                                                   x_value, two_value);
        IR_Value* r1 = ir_builder_insert_call(&ir_builder, else_block, fib_func,
                                              &x1_value, 1);
        IR_Value* r2 = ir_builder_insert_call(&ir_builder, else_block, fib_func,
                                              &x2_value, 1);

        IR_Value* r = ir_builder_insert_add(&ir_builder, else_block,
                                            r1, r2);
        ir_builder_insert_return(&ir_builder, else_block, r);
    }

    ir_builder_print(&ir_builder);

    return 0;
}
