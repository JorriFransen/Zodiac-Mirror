#pragma once

#include "common.h"
#include "ir.h"

namespace Zodiac
{
    struct IR_Call_Stack_Frame
    {
        // These are the values for the arguments, they will be used by the
        //  IRI_POP_FUNC_ARG instruction to create allocl's holding these values.
        Stack<IR_Value*> args;
    };

    struct IR_Runner
    {
        uint64_t ip;

        Stack<IR_Value*> arg_stack;
        Stack<IR_Call_Stack_Frame*> call_stack;
    };

    void ir_run(IR_Runner* runner, IR_Instructions instructions);

    static bool ir_execute(IR_Runner* runner, IR_Instruction iri);
    static void runner_push_stack_frame(IR_Runner* runner);
    static void runner_pop_stack_frame(IR_Runner* runner);
    static IR_Value* runner_pop_function_argument(IR_Runner* runner);
}
