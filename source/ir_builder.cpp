#include "ir_builder.h"

namespace Zodiac
{
    void ir_builder_init(IR_Builder* ir_builder, Arena* arena)
    {
        assert(ir_builder);
        assert(arena);

        ir_builder->arena = arena;
    }

    IR_Value* ir_builder_emit(IR_Builder* builder, IR_Instruction_Kind iri_kind,
                         IR_Value* arg_0, IR_Value* arg_1)
    {
        assert(builder);
        assert(iri_kind != IRI_INVALID);
        assert(iri_kind < IRI_COUNT);

        IR_Instruction instruction = {};
        IR_Value* result_value = nullptr;

        switch (iri_kind)
        {
            case IRI_ALLOCL:
            {
                assert(arg_0);
                assert(arg_1);
                assert(arg_0->kind == IRV_LITERAL || arg_0->kind == IRV_VALUE);
                assert(arg_1->kind == IRV_TYPE);

                assert(arg_1->name);
                result_value = ir_allocl(builder, arg_1, arg_1->name);
                break;
            }

            case IRI_ADD_S32:
            {
                assert(arg_0);
                assert(arg_1);
                assert(arg_0->kind == IRV_LITERAL || arg_0->kind == IRV_VALUE);
                assert(arg_1->kind == IRV_LITERAL || arg_1->kind == IRV_VALUE);

                IR_Value* type_value = nullptr;

                if (arg_0->kind == IRV_LITERAL)
                {
                    assert(arg_0->literal.type);
                    assert(arg_1->literal.type);
                    assert(arg_0->literal.type == arg_1->literal.type);
                    type_value = arg_0->literal.type;
                }
                else if (arg_0->kind == IRV_VALUE)
                {
                    assert(arg_0->value.type);
                    assert(arg_1->value.type);
                    assert(arg_0->value.type == arg_1->value.type);
                    type_value = arg_0->value.type;
                }
                else assert(false);

                assert(type_value);

                result_value = ir_value(builder, type_value, nullptr);
                break;
            }

            case IRI_STOREL:
            {
                assert(arg_0);
                assert(arg_1);
                assert(arg_0->kind == IRV_LITERAL || arg_0->kind == IRV_VALUE);
                assert(arg_1->kind == IRV_ALLOCL);
                assert(arg_1->allocl.type);

                IR_Value* source_type = nullptr;
                if (arg_0->kind == IRV_LITERAL)
                {
                    source_type = arg_0->literal.type;
                }
                else if (arg_0->kind == IRV_VALUE)
                {
                    source_type = arg_0->value.type;
                }
                else assert(false);

                assert(source_type);
                assert(source_type == arg_1->allocl.type);

                result_value = nullptr;
                break;
            }

            case IRI_LOADL:
            {
                assert(arg_0);
                assert(arg_1 == nullptr);
                assert(arg_0->kind == IRV_ALLOCL);

                result_value = ir_value(builder, arg_0->allocl.type, nullptr);
                break;
            }

            case IRI_PRINT:
            {
                assert(arg_0);
                assert(arg_1 == nullptr);
                assert(arg_0->kind == IRV_VALUE);

                result_value = nullptr;
                break;
            }

            case IRI_LABEL:
            {
                assert(arg_0);
                assert(arg_1 == nullptr);
                assert(arg_0->kind == IRV_LABEL);
                assert(arg_0->label.emitted == false);
                arg_0->label.emitted = true;
                arg_0->label.index = BUF_LENGTH(builder->instructions);

                result_value = nullptr;
                break;
            }

            case IRI_JMP_LABEL:
            {
                assert(arg_0);
                assert(arg_1 == nullptr);
                assert(arg_0->kind == IRV_LABEL);

                result_value = nullptr;
                break;
            }

            case IRI_FUNC_DEFN:
            {
                assert(arg_0);
                assert(arg_1);
                assert(arg_0->kind == IRV_TYPE);
                assert(arg_1->kind == IRV_LITERAL);
                // assert(arg_1->literal.type == nullptr);

                result_value = ir_function(builder, BUF_LENGTH(builder->instructions), arg_0, arg_1);
                break;
            }

            case IRI_PUSH_CALL_ARG:
            {
                assert(arg_0);
                assert(arg_1 == nullptr);
                assert(arg_0->kind == IRV_VALUE || arg_0->kind == IRV_LITERAL);

                result_value = nullptr;
                break;
            }

            case IRI_POP_FUNC_ARG:
            {
                assert(arg_0);
                assert(arg_1 == nullptr);
                assert(arg_0->kind == IRV_TYPE);

                result_value = ir_allocl(builder, arg_0, nullptr);
                break;
            }

            case IRI_CALL:
            {
                assert(arg_0);
                assert(arg_1);
                assert(arg_0->kind == IRV_FUNCTION);
                assert(arg_1->kind == IRV_LITERAL);

                assert(arg_0->function.return_type);

                result_value = ir_value(builder, arg_0->function.return_type, nullptr);
                break;
            }

            case IRI_RET:
            {
                assert(arg_0);
                assert(arg_1 == nullptr);
                assert(arg_0->kind == IRV_VALUE);

                result_value = nullptr;
                break;
            }

            default: assert(false);
        }

        instruction.kind = iri_kind;
        instruction.arg_0 = arg_0;
        instruction.arg_1 = arg_1;
        instruction.result_value = result_value;

        BUF_PUSH(builder->instructions, instruction);

        return result_value;
    }

    IR_Instructions ir_builder_export(IR_Builder* builder)
    {
        assert(builder);
        assert(BUF_LENGTH(builder->instructions));

        IR_Instructions result = {};
        result.data = builder->instructions;
        result.count = BUF_LENGTH(builder->instructions);
        return result;
    }
}
