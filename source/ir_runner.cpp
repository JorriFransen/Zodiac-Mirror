#include "ir_runner.h"

#include <inttypes.h>

namespace Zodiac
{
    void ir_runner_init(IR_Runner* ir_runner)
    {
        assert(ir_runner);

        ir_runner->arena = arena_create(MB(4));

        stack_init(&ir_runner->call_stack, 8);
        stack_init(&ir_runner->arg_stack, 8);
        ir_runner->jump_block = nullptr;

        ir_runner->dyn_vm = dcNewCallVM(MB(4));
        dcMode(ir_runner->dyn_vm, DC_CALL_C_DEFAULT);
        dcReset(ir_runner->dyn_vm);

        ir_runner->loaded_dyn_libs = nullptr;
        ir_runner->loaded_foreign_symbols = nullptr;
    }

    void ir_runner_execute(IR_Runner* ir_runner, IR_Module* ir_module)
    {
        assert(ir_runner);
        assert(ir_module);

        assert(ir_module->entry_function);

        ir_runner_load_dynamic_libs(ir_runner, ir_module);
        ir_runner_load_foreigns(ir_runner, ir_module);

        IR_Stack_Frame* entry_stack_frame = ir_runner_call_function(ir_runner, ir_module->entry_function, 0);

        printf("Entry point returned: %" PRId64 "\n", entry_stack_frame->return_value.temp.s64);
        uint64_t arena_cap = 0;
        auto block = ir_runner->arena.blocks;
        while (block)
        {
            arena_cap += block->data_length * sizeof(void*);
            block = block->next_block;
        }
        printf("Arena size: %.2fMB\n", (double)arena_cap / MB(1));
    }

    void ir_runner_load_dynamic_libs(IR_Runner* ir_runner, IR_Module* ir_module)
    {
        assert(ir_runner);
        assert(ir_module);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_module->dynamic_lib_names); i++)
        {
            ir_runner_load_dynamic_lib(ir_runner, ir_module->dynamic_lib_names[i]);
        }
    }

    void ir_runner_load_dynamic_lib(IR_Runner* ir_runner, Atom lib_name)
    {
        assert(ir_runner);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_runner->loaded_dyn_libs); i++)
        {
            const IR_Loaded_Dynamic_Lib& loaded_lib = ir_runner->loaded_dyn_libs[i];
            if (loaded_lib.name == lib_name)
            {
                return;
            }
        }

        DLLib* lib = dlLoadLibrary(lib_name.data);
        if (!lib)
        {
            fprintf(stderr, "Could not find library: %s\n", lib_name.data);
            assert(false);
        }

        IR_Loaded_Dynamic_Lib loaded_lib = { lib_name, lib };
        BUF_PUSH(ir_runner->loaded_dyn_libs, loaded_lib);
        //printf("Loaded dynamic library: %s\n", lib_name.data);
    }

    void ir_runner_load_foreigns(IR_Runner* ir_runner, IR_Module* ir_module)
    {
        assert(ir_runner);
        assert(ir_module);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_module->foreign_table); i++)
        {
            const Atom& foreign_name = ir_module->foreign_table[i];

            bool found = false;
            
            for (uint64_t j = 0; j < BUF_LENGTH(ir_runner->loaded_dyn_libs); j++)
            {
                const IR_Loaded_Dynamic_Lib& loaded_lib = ir_runner->loaded_dyn_libs[j];

                //printf("Trying to load foreign \"%s\" from library \"%s\"\n",
                //    foreign_name.data, loaded_lib.name.data);

                void* foreign_symbol = dlFindSymbol(loaded_lib.lib, foreign_name.data);
                if (foreign_symbol)
                {
                    //printf("Loaded foreign \"%s\" from library \"%s\"\n",
                    //    foreign_name.data, loaded_lib.name.data);
                    BUF_PUSH(ir_runner->loaded_foreign_symbols, foreign_symbol);
                    found = true;
                    break;
                }
            }

            if (!found)
            {
                fprintf(stderr, "Failed to load foreign symbol: %s\n", foreign_name.data);
                assert(false);
            }
        }
    }

    IR_Value* ir_runner_get_local_temporary(IR_Runner* ir_runner, uint64_t temp_index)
    {
        assert(ir_runner);
        assert(stack_count(ir_runner->call_stack));

        IR_Stack_Frame* stack_frame = stack_top(ir_runner->call_stack);
        assert(stack_frame);
        auto buf_length = BUF_LENGTH(stack_frame->temps);
        assert(BUF_LENGTH(stack_frame->temps) > temp_index);

        return &stack_frame->temps[temp_index];
    }

    IR_Stack_Frame* ir_runner_call_function(IR_Runner* runner, IR_Function* function, uint64_t num_args)
    {
        assert(runner);
        assert(function);

        BUF(IR_Value) args = nullptr;
        assert(stack_count(runner->arg_stack) >= num_args);

        for (uint64_t i = 0; i < num_args; i++)
        {
            IR_Value arg = stack_peek(runner->arg_stack, (num_args - 1) - i);
            BUF_PUSH(args, arg);
        }
        for (uint64_t i = 0; i < num_args; i++)
        {
            stack_pop(runner->arg_stack);
        }

        IR_Stack_Frame* stack_frame = ir_runner_push_stack_frame(runner, function, args);

        auto block = function->first_block;
        while (block && ir_runner_top_stack_frame(runner) == stack_frame)
        {
            ir_runner_execute_block(runner, block);
            if (runner->jump_block)
            {
                block = runner->jump_block;
                runner->jump_block = nullptr;
            }
            else
            {
                block = block->next;
            }
        }

        return stack_frame;
    }

    void ir_runner_execute_block(IR_Runner* runner, IR_Block* block)
    {
        assert(runner);
        assert(block);

        auto iri = block->first_instruction;
        while (iri)
        {
            ir_runner_execute_instruction(runner, iri);

            if (runner->jump_block)
            {
                break;
            }

            iri = iri->next;
        }
    }

    void ir_runner_execute_instruction(IR_Runner* runner, IR_Instruction* iri)
    {
        assert(runner);
        assert(iri);

        switch (iri->op)
        {
            case IR_OP_NOP:
            {
                assert(false);
                break;
            }

            case IR_OP_ADD:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_TEMPORARY);
                assert(iri->arg2);
                assert(iri->arg2->kind == IRV_TEMPORARY);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1->temp.index);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2->temp.index);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result->temp.index);

                dest->temp.s64 = arg1->temp.s64 + arg2->temp.s64;
                break;
            }

            case IR_OP_SUB:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_TEMPORARY);
                assert(iri->arg2);
                assert(iri->arg2->kind == IRV_TEMPORARY);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1->temp.index);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2->temp.index);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result->temp.index);

                dest->temp.s64 = arg1->temp.s64 - arg2->temp.s64;
                break;
            }

            case IR_OP_MUL:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_TEMPORARY);
                assert(iri->arg2);
                assert(iri->arg2->kind == IRV_TEMPORARY);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1->temp.index);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2->temp.index);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result->temp.index);

                dest->temp.s64 = arg1->temp.s64 * arg2->temp.s64;
                break;
            }

            case IR_OP_DIV:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_TEMPORARY);
                assert(iri->arg2);
                assert(iri->arg2->kind == IRV_TEMPORARY);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1->temp.index);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2->temp.index);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result->temp.index);

                dest->temp.s64 = arg1->temp.s64 / arg2->temp.s64;
                break;
            }

            case IR_OP_LT:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_TEMPORARY);
                assert(iri->arg2);
                assert(iri->arg2->kind == IRV_TEMPORARY);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1->temp.index);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2->temp.index);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result->temp.index);

                dest->temp.s64 = arg1->temp.s64 < arg2->temp.s64;
                break;
            }

            case IR_OP_LTEQ:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_TEMPORARY);
                assert(iri->arg2);
                assert(iri->arg2->kind == IRV_TEMPORARY);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1->temp.index);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2->temp.index);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result->temp.index);

                dest->temp.s64 = arg1->temp.s64 <= arg2->temp.s64;
                break;
            }

            case IR_OP_PUSH_CALL_ARG:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_TEMPORARY);

                IR_Stack_Frame* stack_frame = stack_top(runner->call_stack);

                auto temp_index = iri->arg1->temp.index;
                IR_Value arg_value = stack_frame->temps[temp_index];
                stack_push(runner->arg_stack, arg_value);
                break;
            }

            case IR_OP_CALL:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_FUNCTION);
                assert(iri->arg2);
                assert(iri->arg2->kind == IRV_LITERAL);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Function* function = iri->arg1->function;
                auto num_args = iri->arg2->literal.s64;
                IR_Stack_Frame* callee_stack_frame = ir_runner_call_function(runner, function, num_args);

                auto result_index = iri->result->temp.index;
                IR_Value* result_value = ir_runner_get_local_temporary(runner, result_index);

                result_value->temp.s64 = callee_stack_frame->return_value.temp.s64;
                break;
            }

            case IR_OP_PUSH_EX_CALL_ARG:
            {
                auto temp_index = iri->arg1->temp.index;
                IR_Value* arg_value = ir_runner_get_local_temporary(runner, temp_index);
                dcArgLongLong(runner->dyn_vm, arg_value->temp.s64);
                break;
            }

            case IR_OP_CALL_EX:
            {
                dcMode(runner->dyn_vm, DC_CALL_C_DEFAULT);
                uint64_t foreign_index = iri->arg1->literal.s64;
                assert(BUF_LENGTH(runner->loaded_foreign_symbols) > foreign_index);

                assert(iri->result);
                IR_Value* result_value = ir_runner_get_local_temporary(runner, iri->result->temp.index);
                assert(result_value);
                result_value->temp.s64 = dcCallInt(runner->dyn_vm, runner->loaded_foreign_symbols[foreign_index]);
                dcReset(runner->dyn_vm);
                break;
            }

            case IR_OP_RETURN:
            {
                if (iri->arg1)
                {
                    assert(iri->arg1->kind == IRV_TEMPORARY);
                    auto temp_index = iri->arg1->temp.index;
                    IR_Value* temp = ir_runner_get_local_temporary(runner, temp_index);

                    auto current_stack_frame = ir_runner_top_stack_frame(runner);
                    current_stack_frame->return_value = *temp;
                }

                ir_runner_pop_stack_frame(runner);
                break;
            }

            case IR_OP_JMP:
            {
                IR_Value* block_value = iri->arg1;
                assert(!runner->jump_block);
                runner->jump_block = block_value->block;
                break;
            }

            case IR_OP_JMP_IF:
            {
                auto cond_index = iri->arg1->temp.index;
                IR_Value* cond_value = ir_runner_get_local_temporary(runner, cond_index);

                IR_Value* block_value = iri->arg2;

                if (cond_value->temp.s64)
                {
                    assert(!runner->jump_block);
                    runner->jump_block = block_value->block;
                }

                break;
            }

            case IR_OP_ALLOCL:
            {
                // Don't do anything for now, these are handled when pushing a new
                //  stack frame
                break;
            }

            case IR_OP_STOREL:
            {
                auto allocl_value_index = iri->arg1->allocl.index;
                IR_Value* dest_value = ir_runner_get_local_temporary(runner, allocl_value_index);

                auto source_value_index = iri->arg2->temp.index;
                IR_Value* source_value = ir_runner_get_local_temporary(runner, source_value_index);

                dest_value->temp.s64 = source_value->temp.s64;
                break;
            }

            case IR_OP_LOADL:
            {
                auto allocl_value_index = iri->arg1->allocl.index;
                IR_Value* source_value = ir_runner_get_local_temporary(runner, allocl_value_index);

                auto result_value_index = iri->result->temp.index;
                IR_Value* dest_value = ir_runner_get_local_temporary(runner, result_value_index);

                dest_value->temp.s64 = source_value->temp.s64;
                break;
            }

            case IR_OP_STOREA:
            {
                IR_Stack_Frame* stack_frame = stack_top(runner->call_stack);

                auto dest_index = iri->arg1->argument.index;
                assert(BUF_LENGTH(stack_frame->args) > dest_index);
                IR_Value* dest = &stack_frame->args[dest_index];

                auto source_index = iri->arg2->temp.index;
                IR_Value* source = ir_runner_get_local_temporary(runner, source_index);

                dest->temp.s64 = source->temp.s64;
                break;
            }

            case IR_OP_LOADA:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_ARGUMENT);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Stack_Frame* stack_frame = stack_top(runner->call_stack);

                auto source_index = iri->arg1->argument.index;
                assert(BUF_LENGTH(stack_frame->args) > source_index);
                IR_Value* source = &stack_frame->args[source_index];
                assert(source->kind == IRV_TEMPORARY);

                auto dest_index = iri->result->temp.index;
                IR_Value* dest = ir_runner_get_local_temporary(runner, dest_index);
                assert(dest->kind == IRV_TEMPORARY);

                dest->temp.s64 = source->temp.s64;
                break;
            }

            case IR_OP_LOAD_LIT:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_LITERAL);
                assert(iri->result->kind == IRV_TEMPORARY);
                auto temp_index = iri->result->temp.index;
                IR_Value* dest = ir_runner_get_local_temporary(runner, temp_index);
                dest->temp.s64 = iri->arg1->literal.s64;
                break;
            }
        }
    }

    IR_Stack_Frame* ir_runner_new_stack_frame(IR_Runner* ir_runner, IR_Function* function,
                                              BUF(IR_Value) args)
    {
        assert(ir_runner);
        assert(function);

        IR_Stack_Frame* result = nullptr;
        result = arena_alloc(&ir_runner->arena, IR_Stack_Frame);
        result->function = function;
        result->args = args;

        for (uint64_t i = 0; i < function->next_temp_index + 1; i++)
        {
            IR_Value new_temp = {};
            new_temp.kind = IRV_TEMPORARY;
            BUF_PUSH(result->temps, new_temp);
        }

        return result;
    }

    IR_Stack_Frame* ir_runner_push_stack_frame(IR_Runner* ir_runner, IR_Function* function,
                                               BUF(IR_Value) args)
    {
        assert(ir_runner);
        assert(function);

        IR_Stack_Frame* new_frame = ir_runner_new_stack_frame(ir_runner, function, args);
        assert(new_frame);

        stack_push(ir_runner->call_stack, new_frame);
        return new_frame;
    }

    IR_Stack_Frame* ir_runner_top_stack_frame(IR_Runner* ir_runner)
    {
        assert(ir_runner);
        assert(stack_count(ir_runner->call_stack));
        return stack_top(ir_runner->call_stack);
    }

    void ir_runner_pop_stack_frame(IR_Runner* ir_runner)
    {
        assert(ir_runner);
        assert(stack_count(ir_runner->call_stack));
        stack_pop(ir_runner->call_stack);
    }
}
