#include <stdio.h>

#include "common.h"

#include "ir_builder.h"
#include "ir_runner.h"

using namespace Zodiac;

int main(int argc, char** argv)
{
    printf("compiler_main()\n");

    Arena ir_arena;
    IR_Builder ir_builder;

    ir_arena = arena_create(MB(4));
    ir_builder_init(&ir_builder, &ir_arena);

    IR_Value* s32 = ir_type(&ir_builder, 32, true, "s32");
    IR_Value* entry_label = ir_label(&ir_builder, "entry");

    // Jump to the entry point
    ir_builder_emit(&ir_builder, IRI_JMP_LABEL, entry_label, nullptr);

    // Create add function
    auto add = ir_builder_emit(&ir_builder, IRI_FUNC_DEFN, s32, ir_literal(&ir_builder, "add"));
    assert(add);
    {
        // These will be allocl's
        auto t0 = ir_builder_emit(&ir_builder, IRI_POP_FUNC_ARG, s32, nullptr); // y
        auto t1 = ir_builder_emit(&ir_builder, IRI_POP_FUNC_ARG, s32, nullptr); // x
        auto result = ir_builder_emit(&ir_builder, IRI_ALLOCL, s32, nullptr);   // result

        // Load the arguments
        auto t2 = ir_builder_emit(&ir_builder, IRI_LOADL, t0, nullptr);
        auto t3 = ir_builder_emit(&ir_builder, IRI_LOADL, t1, nullptr);

        // Add them together and store the result
        auto t4 = ir_builder_emit(&ir_builder, IRI_ADD_S32, t3, t2);
        ir_builder_emit(&ir_builder, IRI_STOREL, result, t4);

        // // Return the result directly
        // ir_builder_emit(&ir_builder, IRI_RET, t4, nullptr);

        // Load the result and return it
        auto result_value = ir_builder_emit(&ir_builder, IRI_LOADL, result, nullptr);
        ir_builder_emit(&ir_builder, IRI_RET, result_value, nullptr);
    }

    // Create fib function
    auto fib_recursive = ir_builder_emit(&ir_builder, IRI_FUNC_DEFN, s32, ir_literal(&ir_builder, "fib_recursive"));
    assert(fib_recursive);
    {
        auto t0 = ir_builder_emit(&ir_builder, IRI_POP_FUNC_ARG, s32, nullptr);
        auto x = ir_builder_emit(&ir_builder, IRI_LOADL, t0, nullptr);

        auto recurse_label = ir_label(&ir_builder, "recurse");

        auto if_cond = ir_builder_emit(&ir_builder, IRI_LT_S32, x, ir_literal(&ir_builder, 2, s32));
        if_cond = ir_builder_emit(&ir_builder, IRI_NOT_BOOL, if_cond, nullptr);
        ir_builder_emit(&ir_builder, IRI_JMP_LABEL_COND, recurse_label, if_cond);
        // (x < 2)
        {
            ir_builder_emit(&ir_builder, IRI_RET, x, nullptr);
        }
        // else
        {
            ir_builder_emit(&ir_builder, IRI_LABEL, recurse_label, nullptr);

            auto x1 = ir_builder_emit(&ir_builder, IRI_SUB_S32, x, ir_literal(&ir_builder, 1, s32));
            auto x2 = ir_builder_emit(&ir_builder, IRI_SUB_S32, x, ir_literal(&ir_builder, 2, s32));

            ir_builder_emit(&ir_builder, IRI_PUSH_CALL_ARG, x1, nullptr);
            auto f1 = ir_builder_emit(&ir_builder, IRI_CALL, fib_recursive, ir_literal(&ir_builder, 1, s32));

            ir_builder_emit(&ir_builder, IRI_PUSH_CALL_ARG, x2, nullptr);
            auto f2 = ir_builder_emit(&ir_builder, IRI_CALL, fib_recursive, ir_literal(&ir_builder, 1, s32));

            auto result = ir_builder_emit(&ir_builder, IRI_ADD_S32, f1, f2);
            ir_builder_emit(&ir_builder, IRI_RET, result, nullptr);
        }

    }

    ir_builder_emit(&ir_builder, IRI_LABEL, entry_label, nullptr);

    ir_builder_emit(&ir_builder, IRI_PUSH_CALL_ARG, ir_literal(&ir_builder, 6, s32), nullptr);
    ir_builder_emit(&ir_builder, IRI_PUSH_CALL_ARG, ir_literal(&ir_builder, 8, s32), nullptr);
    auto add_result = ir_builder_emit(&ir_builder, IRI_CALL, add, ir_literal(&ir_builder, 2, s32));
    ir_builder_emit(&ir_builder, IRI_PRINT, add_result, nullptr);

    ir_builder_emit(&ir_builder, IRI_PUSH_CALL_ARG, ir_literal(&ir_builder, 3, s32), nullptr);
    auto fib_result = ir_builder_emit(&ir_builder, IRI_CALL, fib_recursive, ir_literal(&ir_builder, 1, s32));
    ir_builder_emit(&ir_builder, IRI_PRINT, fib_result, nullptr);


    IR_Runner ir_runner;
    IR_Instructions instructions = ir_builder_export(&ir_builder);
    ir_run(&ir_runner, instructions);

    return 0;
}

