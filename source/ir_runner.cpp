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
                assert(false);
                runner->ip++;
                break;
            }

            case IRI_LOADL:
            {
                assert(false);
                break;
            }

            case IRI_ADD_S32:
            {
                int32_t lhs, rhs;
                if (iri.arg_0->kind == IRV_LITERAL)
                {
                    lhs = iri.arg_0->literal.s32;
                }
                else if (iri.arg_0->kind == IRV_VALUE)
                {
                    lhs = iri.arg_0->value.val.s32;
                }
                else assert(false);
                if (iri.arg_1->kind == IRV_LITERAL)
                {
                    rhs = iri.arg_1->literal.s32;
                }
                else if (iri.arg_1->kind == IRV_VALUE)
                {
                    rhs = iri.arg_1->value.val.s32;
                }
                else assert(false);

                iri.result_value->value.val.s32 = lhs + rhs;
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
                IR_Value* arg_value_type = get_value_or_literal_type(arg_value);

                assert(arg_value_type == iri.result_value->allocl.type);
                iri.result_value->allocl.value = arg_value;
                runner->ip++;
                break;
            }

            case IRI_CALL:
            {
                // Function handle is the first argument
                IR_Value* func = iri.arg_0;
                assert(func->kind == IRV_FUNCTION);

                runner_push_stack_frame(runner);

                runner->ip = func->function.index;
                break;
            }

            case IRI_RET:
            {
                assert(false);
                runner_pop_stack_frame(runner);
                runner->ip++;
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
                break;

                runner->ip++;
                break;
            }

            default: assert(false);
        }

        return false;
    }

    static void runner_push_stack_frame(IR_Runner* runner)
    {
        assert(runner);

        //TODO: Stack allocator
        IR_Call_Stack_Frame* stack_frame = (IR_Call_Stack_Frame*)mem_alloc(sizeof(IR_Call_Stack_Frame));
        stack_frame->args = {};
        stack_copy(stack_frame->args, runner->arg_stack);
        stack_clear(runner->arg_stack);

        stack_push(runner->call_stack, stack_frame);
    }

    static void runner_pop_stack_frame(IR_Runner* runner)
    {
        assert(runner);
        assert(false);
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
