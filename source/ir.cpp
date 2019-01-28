#include "ir.h"

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

            default: assert(false);
        }

        instruction.kind = iri_kind;
        instruction.arg_0 = arg_0;
        instruction.arg_1 = arg_1;
        instruction.result_value = result_value;

        BUF_PUSH(builder->instructions, instruction);

        return result_value;
    }

    IR_Value* ir_type(IR_Builder* builder, uint64_t size, bool sign, const char* name)
    {
        assert(builder);
        assert(name);
        //TODO: Assert size is 0, 1, 2, 4, 8, 16, etc...

        IR_Value* result = ir_value_make(builder, IRV_TYPE, name);
        result->type.size = size;
        result->type.sign = sign;
        return result;
    }

    IR_Value* ir_literal(IR_Builder* builder, const char* str)
    {
        assert(builder);
        assert(str);

        IR_Value* result = ir_value_make(builder, IRV_LITERAL);
        result->literal.kind = IR_LIT_STRING;
        result->literal.str = str;
        return result;
    }

    IR_Value* ir_literal(IR_Builder* builder, int32_t s32, IR_Value* type_value)
    {
        assert(builder);
        assert(type_value);
        //TODO: Assert type is integral, etc
        assert(type_value->kind == IRV_TYPE);

        IR_Value* result = ir_value_make(builder, IRV_LITERAL);
        result->literal.kind = IR_LIT_INT;
        result->literal.s32 = s32;
        result->literal.type = type_value;
        return result;
    }

    IR_Value* ir_value(IR_Builder* builder, IR_Value* type_value, const char* name)
    {
        assert(builder);
        assert(type_value);
        assert(type_value->kind == IRV_TYPE);
        // assert(name);

        IR_Value* result = ir_value_make(builder, IRV_VALUE, name);
        result->value.type = type_value;
        return result;
    }

    IR_Value* ir_allocl(IR_Builder* builder, IR_Value* type_value, const char* name)
    {
        assert(builder);
        assert(type_value);
        assert(type_value->kind == IRV_TYPE);
        assert(name);

        IR_Value* result = ir_value_make(builder, IRV_ALLOCL, name);
        result->allocl.type = type_value;
        return result;
    }

    IR_Value* ir_value_make(IR_Builder* builder, IR_Value_Kind irv_kind,
                            const char* name/*= nullptr*/)
    {
        assert(builder);
        assert(irv_kind != IRV_INVALID);
        assert(irv_kind < IRV_COUNT);
        // assert(name);

        IR_Value* result = arena_alloc(builder->arena, IR_Value);
        *result = {};
        result->kind = irv_kind;
        result->name = name;
        return result;
    }
}
