#include "ir_runner.h"

namespace Zodiac
{
    void ir_run(IR_Runner* runner, IR_Instructions instructions)
    {
        assert(runner);

        runner->ip = 0;
        stack_init(&runner->arg_stack, 8);
        stack_init(&runner->call_stack, 64);

        while (runner->ip < instructions.count)
        {
            bool done = ir_execute(runner, instructions.data[runner->ip]);
            if (done) break;
        }
    }

    static bool ir_execute(IR_Runner* runner, IR_Instruction iri)
    {
        assert(runner);

        switch (iri.kind)
        {
            case IRI_ALLOCL:
            {
                assert(iri.result_value->allocl.value);
                assert(iri.result_value->allocl.type);
                runner->ip++;
                break;
            }

            case IRI_LOADL:
            {
                assert(iri.arg_0);
                assert(iri.arg_0->kind == IRV_ALLOCL);
                assert(iri.result_value->value.type == iri.arg_0->allocl.type);
                iri.result_value->value.val = iri.arg_0->allocl.value->value.val;
                runner->ip++;
                break;
            }

            case IRI_STOREL:
            {
                assert(iri.arg_0);
                assert(iri.arg_1);
                assert(iri.arg_0->kind == IRV_ALLOCL);
                assert(iri.arg_1->kind == IRV_LITERAL ||
                       iri.arg_1->kind == IRV_VALUE);

                assert(iri.arg_0->allocl.type == iri.arg_1->value.type);
                iri.arg_0->allocl.value->value.val = iri.arg_1->value.val;
                runner->ip++;
                break;
            }

            case IRI_ADD_S32:
            {
                assert(iri.arg_0->kind == IRV_VALUE ||
                       iri.arg_0->kind == IRV_LITERAL);
                assert(iri.arg_1->kind == IRV_VALUE ||
                       iri.arg_1->kind == IRV_LITERAL);

                int32_t lhs = iri.arg_0->value.val.s32;
                int32_t rhs = iri.arg_1->value.val.s32;

                iri.result_value->value.val.s32 = lhs + rhs;
                runner->ip++;
                break;
            }

            case IRI_SUB_S32:
            {
                assert(iri.arg_0->kind == IRV_VALUE ||
                    iri.arg_0->kind == IRV_LITERAL);
                assert(iri.arg_1->kind == IRV_VALUE ||
                    iri.arg_1->kind == IRV_LITERAL);

                int32_t lhs = iri.arg_0->value.val.s32;
                int32_t rhs = iri.arg_1->value.val.s32;

                iri.result_value->value.val.s32 = lhs - rhs;
                runner->ip++;
                break;
            }
            case IRI_LABEL:
            {
                runner->ip++;
                break;
            }

            case IRI_JMP_LABEL:
            {
                assert(iri.arg_0->label.emitted);
                runner->ip = iri.arg_0->label.index;
                break;
            }

            case IRI_JMP_LABEL_COND:
            {
                assert(iri.arg_0->kind == IRV_LABEL);
                assert(iri.arg_0->label.emitted);
                assert(iri.arg_1->kind == IRV_VALUE ||
                       iri.arg_1->kind == IRV_LITERAL);

                if (iri.arg_1->value.val.b8)
                {
                    runner->ip = iri.arg_0->label.index;
                }
                else
                {
                    runner->ip++;
                }
                break;
            }

            case IRI_LT_S32:
            {
                assert(iri.arg_0->kind == IRV_VALUE ||
                       iri.arg_0->kind == IRV_LITERAL);
                assert(iri.arg_1->kind == IRV_VALUE ||
                       iri.arg_1->kind == IRV_LITERAL);

                int32_t lhs = iri.arg_0->value.val.s32;
                int32_t rhs = iri.arg_1->value.val.s32;

                iri.result_value->value.val.b8 = lhs < rhs;
                runner->ip++;
                break;
            }

            case IRI_NOT_BOOL:
            {
                assert(iri.arg_0->kind == IRV_VALUE ||
                       iri.arg_0->kind == IRV_LITERAL);

                bool b8 = iri.arg_0->value.val.b8;
                iri.result_value->value.val.b8 = !b8;
                runner->ip++;
                break;
            }

            case IRI_FUNC_DEFN:
            {
                // Don't need to do anything for now?
                // TODO: Check if we have the correct amount of arguments pushed?
                runner->ip++;
                break;
            }

            case IRI_PUSH_CALL_ARG:
            {
                stack_push(runner->arg_stack, iri.arg_0);
                runner->ip++;
                break;
            }

            case IRI_POP_FUNC_ARG:
            {
                IR_Value* arg_value = runner_pop_function_argument(runner);
                assert(arg_value->kind == IRV_VALUE ||
                       arg_value->kind == IRV_LITERAL);
                IR_Value* arg_value_type = arg_value->value.type;

                assert(arg_value_type == iri.result_value->allocl.type);
                iri.result_value->allocl.value = arg_value;
                iri.result_value->allocl.value->kind = IRV_VALUE;
                runner->ip++;
                break;
            }

            case IRI_CALL:
            {
                // Function handle is the first argument
                IR_Value* func = iri.arg_0;
                assert(func->kind == IRV_FUNCTION);

                runner_push_stack_frame(runner, runner->ip + 1, iri.result_value);

                runner->ip = func->function.index;
                break;
            }

            case IRI_RET:
            {
                IR_Value* return_value = iri.arg_0;
                runner->ip = runner_pop_stack_frame(runner, return_value);
                break;
            }

            case IRI_PRINT:
            {
                switch (iri.arg_0->value.type->type.size)
                {
                    case 32:
                    {
                        auto format = (iri.arg_0->value.type->type.sign) ? "%d\n" : "%u\n";
                        printf(format, iri.arg_0->value.val.s32);
                        break;
                    }
                    default: assert(false);
                }

                runner->ip++;
                break;
            }

            default: assert(false);
        }

        return false;
    }

    static void runner_push_stack_frame(IR_Runner* runner, uint64_t return_index, IR_Value* return_value)
    {
        assert(runner);
        assert(return_value);

        //TODO: Stack allocator
        IR_Call_Stack_Frame* stack_frame = (IR_Call_Stack_Frame*)mem_alloc(sizeof(IR_Call_Stack_Frame));
        stack_frame->args = {};
        stack_copy(stack_frame->args, runner->arg_stack);
        stack_clear(runner->arg_stack);
        stack_frame->return_index = return_index;
        stack_frame->result_value = return_value;

        stack_push(runner->call_stack, stack_frame);
    }

    static uint64_t runner_pop_stack_frame(IR_Runner* runner, IR_Value* return_value)
    {
        assert(runner);
        if (return_value)
        {
            assert(return_value->kind == IRV_VALUE ||
                   return_value->kind == IRV_LITERAL);
        }

        IR_Call_Stack_Frame* stack_frame = stack_pop(runner->call_stack);
        assert(stack_frame->result_value);

        uint64_t return_index = stack_frame->return_index;
        *(stack_frame->result_value) = *return_value;

        stack_free(&stack_frame->args);
        mem_free(stack_frame);

        return return_index;
    }

    static IR_Value* runner_pop_function_argument(IR_Runner* runner)
    {
        assert(runner);

        IR_Call_Stack_Frame* stack_frame = stack_top(runner->call_stack);
        IR_Value* func_arg = stack_pop(stack_frame->args);
        assert(func_arg->kind == IRV_VALUE ||
               func_arg->kind == IRV_LITERAL);
        return func_arg;
    }
}
