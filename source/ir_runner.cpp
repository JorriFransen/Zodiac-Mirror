#include "ir_runner.h"

#include <inttypes.h>

namespace Zodiac
{
    void ir_runner_init(IR_Runner* ir_runner)
    {
        assert(ir_runner);

        stack_init(&ir_runner->call_stack, 16);
    }

    void ir_runner_execute(IR_Runner* ir_runner, IR_Module* ir_module)
    {
        assert(ir_runner);
        assert(ir_module);

        assert(ir_module->entry_function);

        ir_runner_call_function(ir_runner, ir_module->entry_function);

        auto entry_stack_frame = ir_runner->last_popped_stack_frame;
        IR_Value return_value = entry_stack_frame.return_value;

        printf("Entry point returned: %" PRIu64 "\n", return_value.temp.s64);
    }

    IR_Value* ir_runner_get_local_temporary(IR_Runner* ir_runner, uint64_t temp_index)
    {
        assert(ir_runner);

        auto stack_frame = stack_top(ir_runner->call_stack);
        assert(BUF_LENGTH(stack_frame.temps) > temp_index);

        return &stack_frame.temps[temp_index];
    }

    void ir_runner_call_function(IR_Runner* runner, IR_Function* function)
    {
        assert(runner);
        assert(function);

        ir_runner_push_stack_frame(runner, function);

        auto block = function->first_block;
        while (block)
        {
            ir_runner_execute_block(runner, block);
            block = block->next;
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
                assert(false);
                break;
            }

            case IR_OP_MUL:
            {
                assert(false);
                break;
            }

            case IR_OP_DIV:
            {
                assert(false);
                break;
            }

            case IR_OP_LT:
            {
                assert(false);
                break;
            }

            case IR_OP_LTEQ:
            {
                assert(false);
                break;
            }

            case IR_OP_PUSH_CALL_ARG:
            {
                assert(false);
                break;
            }

            case IR_OP_CALL:
            {
                assert(false);
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
                assert(false);
                break;
            }

            case IR_OP_JMP_IF:
            {
                assert(false);
                break;
            }

            case IR_OP_ALLOCL:
            {
                assert(false);
                break;
            }

            case IR_OP_STOREL:
            {
                assert(false);
                break;
            }

            case IR_OP_LOADL:
            {
                assert(false);
                break;
            }

            case IR_OP_STOREA:
            {
                assert(false);
                break;
            }

            case IR_OP_LOADA:
            {
                assert(false);
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

    void ir_runner_push_stack_frame(IR_Runner* ir_runner, IR_Function* function)
    {
        assert(ir_runner);
        assert(function);

        IR_Stack_Frame new_frame = {};
        new_frame.function = function;

        for (uint64_t i = 0; i < function->next_temp_index; i++)
        {
            IR_Value new_temp = {};
            new_temp.kind = IRV_TEMPORARY;
            BUF_PUSH(new_frame.temps, new_temp);
        }

        stack_push(ir_runner->call_stack, new_frame);
    }

    IR_Stack_Frame* ir_runner_top_stack_frame(IR_Runner* ir_runner)
    {
        assert(ir_runner);
        assert(stack_count(ir_runner->call_stack));
        return stack_top_ptr(ir_runner->call_stack);
    }

    void ir_runner_pop_stack_frame(IR_Runner* ir_runner)
    {
        assert(ir_runner);
        assert(stack_count(ir_runner->call_stack));
        ir_runner->last_popped_stack_frame = stack_top(ir_runner->call_stack);
    }
}
