#include "ir_runner.h"

namespace Zodiac
{
    void ir_run(IR_Runner* runner, IR_Instructions instructions)
    {
        assert(runner);

        runner->ip = 0;

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
                runner->ip++;
                break;
            }

            case IRI_PUSH_CALL_ARG:
            {
                assert(false);
                runner->ip++;
                break;
            }

            case IRI_POP_FUNC_ARG:
            {
                assert(false);
                runner->ip++;
                break;
            }

            case IRI_CALL:
            {
                assert(false);
                runner->ip++;
                break;
            }

            case IRI_RET:
            {
                assert(false);
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
}
