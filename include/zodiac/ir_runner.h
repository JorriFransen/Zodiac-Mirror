#pragma once

#include "ir.h"
#include "platform.h"

#include <dyncall.h>
#include <dyncall_callback.h>
#include <dynload.h>

namespace Zodiac
{
    struct IR_Stack_Frame
    {
        IR_Function* function = nullptr;
        File_Pos call_site;
        Arena arena = {};
        BUF(IR_Value) args = nullptr;
        BUF(IR_Value) temps = nullptr;
        IR_Value* return_value = nullptr;

        IR_Stack_Frame* next_free = nullptr;
    };

    struct IR_Loaded_Dynamic_Lib
    {
        Atom name = {};
        DLLib* lib = nullptr;
    };

    struct IR_Pushed_Arg
    {
        IR_Value arg_value = {};
        bool is_vararg = false;
    };

    struct IR_Builtin_Thread
    {
		Thread_Handle handle;
        void* user_data;
    };

    struct IR_Thread
    {
        union
        {
			Thread_Handle handle;
            IR_Builtin_Thread builtin_thread;
        };

        IR_Value* function_value;
        IR_Function* function;

        IR_Runner* parent_ir_runner;

        IR_Thread* next;
    };

    struct IR_Runner
    {
        Context* context = nullptr;
        IR_Module* ir_module = nullptr;
        Arena arena = {};
        Stack<IR_Stack_Frame*> call_stack = {};
        IR_Stack_Frame* free_stack_frames = nullptr;
        Stack<IR_Pushed_Arg> arg_stack = {};
        IR_Block* jump_block = nullptr;
        IR_Block* from_block = nullptr;
        bool returned = false;
        bool asserted = false;

        DCCallVM* dyn_vm = nullptr;
        BUF(IR_Loaded_Dynamic_Lib) loaded_dyn_libs = nullptr;
        BUF(void*) loaded_foreign_symbols = nullptr;

        IR_Thread* threads = nullptr;
        IR_Thread* free_threads = nullptr;

        Type_Info_Data type_info_data = {};
    };

    void ir_runner_init(
        Context* context, IR_Runner* ir_runner, IR_Runner* thread_parent = nullptr);

    uint64_t ir_runner_execute_entry(
        IR_Runner* ir_runner, AST_Module* ast_module, IR_Module* ir_module);

	void ir_runner_allocate_global_structs(IR_Module* ir_module);
    void ir_runner_free_global_structs(IR_Module* ir_module);
    void ir_runner_initialize_globals(IR_Runner* ir_runner);

    void* ir_runner_thread_entry(void* user_data);
    void ir_runner_cancel_all_threads(IR_Runner* runner);

    bool ir_runner_load_dynamic_libs(IR_Runner* ir_runner, AST_Module* AST_Module,
                                     IR_Module* ir_module, bool load_defaults = true);

	bool ir_runner_load_dynamic_lib(IR_Runner* ir_runner, const char* lib_name);
    bool ir_runner_load_dynamic_lib(IR_Runner* ir_runner, Atom lib_name);
    void ir_runner_load_foreigns(IR_Runner* ir_runner, IR_Module* ir_module);

    IR_Value* ir_runner_get_local_temporary(IR_Runner* ir_runner, uint64_t temp_index);
    IR_Value* ir_runner_get_local_temporary(IR_Runner* ir_runner, IR_Value* code_value);

    void ir_runner_push_ex_call_args(IR_Runner* runner, IR_Value* num_args_val);

    void ir_runner_push_ex_call_arg(
        IR_Runner* runner,
        IR_Value* arg_value,
        AST_Type* arg_type,
        bool is_vararg);

    IR_Stack_Frame* ir_runner_call_function(
        IR_Runner* runner,
        File_Pos origin,
        IR_Function* function,
        uint64_t num_args,
        IR_Value* return_value);

    void ir_runner_execute_block(IR_Runner* runner, IR_Block* block);
    void ir_runner_execute_instruction(IR_Runner* runner, IR_Instruction* iri);

    IR_Stack_Frame* ir_runner_new_stack_frame(
        IR_Runner* ir_runner,
        File_Pos call_site,
        IR_Function* function,
        BUF(IR_Value) args,
        IR_Value* return_value);

    IR_Stack_Frame* ir_runner_push_stack_frame(
        IR_Runner* ir_runner,
        File_Pos call_site,
        IR_Function* function,
        BUF(IR_Value) args,
        IR_Value* return_value);

    IR_Stack_Frame* ir_runner_top_stack_frame(IR_Runner* ir_runner);
    void ir_runner_pop_stack_frame(IR_Runner* ir_runner);

    void ir_runner_store_global(
        IR_Runner* ir_runner, IR_Value* dest_value, IR_Value* source_value);

    void ir_runner_store_temporary(IR_Runner* runner, AST_Type* type, uint8_t* dest_pointer,
                                   IR_Value* temp);

    void ir_runner_store_aggregate_literal(IR_Runner* runner, AST_Type* aggregate_type,
                                           uint8_t* dest_pointer, IR_Value* literal_value);
    void ir_runner_store_aggregate_literal(IR_Runner* runner, AST_Type* aggregate_type,
                                           IR_Value* dest_value, IR_Value* literal_value);
    void ir_runner_store_array_literal(IR_Runner* runner, AST_Type* array_type,
                                       uint8_t* dest_pointer, IR_Value* literal_value);
    void ir_runner_store_array_literal(IR_Runner* runner, AST_Type* array_type,
                                       IR_Value* dest_value, IR_Value* literal_value);

    void ir_runner_print_stack_trace(IR_Runner* ir_runner, File_Pos origin);

    static const char* get_dcb_signature(AST_Type* type);
    static char get_dcb_signature_char(AST_Type* type);
}
