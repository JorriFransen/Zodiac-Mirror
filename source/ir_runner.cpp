#include "ir_runner.h"

#include <inttypes.h>

namespace Zodiac
{
    void ir_runner_init(IR_Runner* ir_runner)
    {
        assert(ir_runner);

        ir_runner->arena = arena_create(MB(1));

        // TODO: BUG: For some reason we crash when the call stack depth is grown to 32, 
        //  but previous grows work fine. This seems to be related to the call stack depth/
        //  recursion depth.
        stack_init(&ir_runner->call_stack, 32);
        stack_init(&ir_runner->arg_stack, 8);
        ir_runner->last_popped_stack_frame = nullptr;
        ir_runner->jump_block = nullptr;
    }

    void ir_runner_execute(IR_Runner* ir_runner, IR_Module* ir_module)
    {
        assert(ir_runner);
        assert(ir_module);

        assert(ir_module->entry_function);

        ir_runner_call_function(ir_runner, ir_module->entry_function, 0);

        auto entry_stack_frame = ir_runner->last_popped_stack_frame;
        IR_Value return_value = entry_stack_frame->return_value;

        printf("Entry point returned: %" PRId64 "\n", return_value.temp.s64);
        uint64_t arena_cap = 0;
        auto block = ir_runner->arena.blocks;
        while (block)
        {
            arena_cap += block->data_length * sizeof(void*);
            block = block->next_block;
        }
        printf("Arena size: %.2fMB\n", (double)arena_cap / MB(1));
    }

    IR_Value* ir_runner_get_local_temporary(IR_Runner* ir_runner, uint64_t temp_index)
    {
        assert(ir_runner);

        IR_Stack_Frame* stack_frame = stack_top(ir_runner->call_stack);
        assert(stack_frame);
        auto buf_length = BUF_LENGTH(stack_frame->temps);
        assert(BUF_LENGTH(stack_frame->temps) > temp_index);

        return &stack_frame->temps[temp_index];
    }

    void ir_runner_call_function(IR_Runner* runner, IR_Function* function, uint64_t num_args)
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
                assert(false);
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
                ir_runner_call_function(runner, function, num_args);

                auto result_index = iri->result->temp.index;
                IR_Value* result_value = ir_runner_get_local_temporary(runner, result_index);

                auto callee_stack_frame = runner->last_popped_stack_frame;
                IR_Value return_value = callee_stack_frame->return_value;

                result_value->temp.s64 = return_value.temp.s64;
                break;
            }

            case IR_OP_PUSH_EX_CALL_ARG:
            {
                assert(false);
                break;
            }

            case IR_OP_CALL_EX:
            {
                assert(false);
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

                    ir_runner_pop_stack_frame(runner);
                }
                else
                {
                    assert(false);
                }
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
                // Don't do anything for now
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

    IR_Stack_Frame* ir_runner_new_stack_frame(IR_Runner* ir_runner, IR_Function* function, BUF(IR_Value) args)
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

    IR_Stack_Frame* ir_runner_push_stack_frame(IR_Runner* ir_runner, IR_Function* function, BUF(IR_Value) args)
    {
        assert(ir_runner);
        assert(function);

        IR_Stack_Frame* new_frame = ir_runner_new_stack_frame(ir_runner, function, args);

        stack_push(ir_runner->call_stack, new_frame);
        return stack_top(ir_runner->call_stack);
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
        ir_runner->last_popped_stack_frame = stack_top(ir_runner->call_stack);
        stack_pop(ir_runner->call_stack);
    }
}
