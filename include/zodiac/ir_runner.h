#pragma once

#include "ir.h"

#include <dyncall/dyncall.h>
#include <dynload/dynload.h>

namespace Zodiac
{
    struct IR_Stack_Frame
    {
        IR_Function* function = nullptr;
        Arena arena = {};
        BUF(IR_Value) args = nullptr;
        BUF(IR_Value) temps = nullptr;
        IR_Value return_value = {};
    };

    struct IR_Loaded_Dynamic_Lib
    {
        Atom name = {};
        DLLib* lib = nullptr;
    };

    struct IR_Runner
    {
        Context* context = nullptr;
        Arena arena = {};
        Stack<IR_Stack_Frame*> call_stack = {};
        Stack<IR_Value> arg_stack = {};
        IR_Block* jump_block = nullptr;

        DCCallVM* dyn_vm = nullptr;
        BUF(IR_Loaded_Dynamic_Lib) loaded_dyn_libs = nullptr;
        BUF(void*) loaded_foreign_symbols = nullptr;
    };

    void ir_runner_init(Context* context, IR_Runner* ir_runner);
    void ir_runner_execute(IR_Runner* ir_runner, AST_Module* ast_module, IR_Module* ir_module);

    void ir_runner_load_dynamic_libs(IR_Runner* ir_runner, AST_Module* AST_Module,
                                     IR_Module* ir_module);
    void ir_runner_load_dynamic_lib(IR_Runner* ir_runner, Atom lib_name);
    void ir_runner_load_foreigns(IR_Runner* ir_runner, IR_Module* ir_module);

    IR_Value* ir_runner_get_local_temporary(IR_Runner* ir_runner, uint64_t temp_index);
    IR_Value* ir_runner_get_local_temporary(IR_Runner* ir_runner, IR_Value* code_value);

    IR_Stack_Frame* ir_runner_call_function(IR_Runner* runner, IR_Function* function, uint64_t num_args);
    void ir_runner_execute_block(IR_Runner* runner, IR_Block* block);
    void ir_runner_execute_instruction(IR_Runner* runner, IR_Instruction* iri);

    IR_Stack_Frame* ir_runner_new_stack_frame(IR_Runner* ir_runner, IR_Function* function,
                                              BUF(IR_Value) args);

    IR_Stack_Frame* ir_runner_push_stack_frame(IR_Runner* ir_runner, IR_Function* function,
                                               BUF(IR_Value) args);
    IR_Stack_Frame* ir_runner_top_stack_frame(IR_Runner* ir_runner);
    void ir_runner_pop_stack_frame(IR_Runner* ir_runner);
}
