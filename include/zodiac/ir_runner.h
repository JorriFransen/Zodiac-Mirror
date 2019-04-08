#pragma once

#include "ir.h"

namespace Zodiac
{
    struct IR_Stack_Frame
    {
        IR_Function* function = nullptr;
        BUF(IR_Value) args = nullptr;
        BUF(IR_Value) temps = nullptr;
        IR_Value return_value = {};
    };

    struct IR_Runner
    {
        Stack<IR_Stack_Frame> call_stack = {};
        Stack<IR_Value> arg_stack = {};
        IR_Stack_Frame last_popped_stack_frame;
    };

    void ir_runner_init(IR_Runner* ir_runner);
    void ir_runner_execute(IR_Runner* ir_runner, IR_Module* ir_module);

    IR_Value* ir_runner_get_local_temporary(IR_Runner* ir_runner, uint64_t temp_index);

    void ir_runner_call_function(IR_Runner* runner, IR_Function* function, uint64_t num_args);
    void ir_runner_execute_block(IR_Runner* runner, IR_Block* block);
    void ir_runner_execute_instruction(IR_Runner* runner, IR_Instruction* iri);

    void ir_runner_push_stack_frame(IR_Runner* ir_runner, IR_Function* function, BUF(IR_Value) args);
    IR_Stack_Frame* ir_runner_top_stack_frame(IR_Runner* ir_runner);
    void ir_runner_pop_stack_frame(IR_Runner* ir_runner);
}
