
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
    }
    ir_builder_end_function(&ir_builder, main_func);

    ir_builder_print_functions(&ir_builder);
}
