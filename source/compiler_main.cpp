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

        // Load the arguments
        auto t2 = ir_builder_emit(&ir_builder, IRI_LOADL, t0, nullptr);
        auto t3 = ir_builder_emit(&ir_builder, IRI_LOADL, t1, nullptr);

        // Add them together
        auto t4 = ir_builder_emit(&ir_builder, IRI_ADD_S32, t3, t2);

        // Return the result directly
        ir_builder_emit(&ir_builder, IRI_RET, t4, nullptr);
    }

    ir_builder_emit(&ir_builder, IRI_LABEL, entry_label, nullptr);
    ir_builder_emit(&ir_builder, IRI_PUSH_CALL_ARG, ir_literal(&ir_builder, 6, s32), nullptr);
    ir_builder_emit(&ir_builder, IRI_PUSH_CALL_ARG, ir_literal(&ir_builder, 8, s32), nullptr);
    auto add_result = ir_builder_emit(&ir_builder, IRI_CALL, add, ir_literal(&ir_builder, 2, s32));

    ir_builder_emit(&ir_builder, IRI_PRINT, add_result, nullptr);


    IR_Runner ir_runner;
    IR_Instructions instructions = ir_builder_export(&ir_builder);
    ir_run(&ir_runner, instructions);

    return 0;
}

