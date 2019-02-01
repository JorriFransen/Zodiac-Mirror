#include "ir.h"

#include "ir_builder.h"

namespace Zodiac
{
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
        result->literal.val.str = str;
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
        result->literal.val.s32 = s32;
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
        // assert(name);

        IR_Value* result = ir_value_make(builder, IRV_ALLOCL, name);
        result->allocl.type = type_value;
        result->allocl.value = ir_value(builder, type_value, nullptr);
        return result;
    }

    IR_Value* ir_function(IR_Builder* builder, uint64_t index, IR_Value* return_type, IR_Value* name_lit)
    {
        assert(builder);
        assert(return_type);
        assert(return_type->kind == IRV_TYPE);
        assert(name_lit);
        assert(name_lit->kind == IRV_LITERAL);

        IR_Value* result = ir_value_make(builder, IRV_FUNCTION, name_lit->literal.val.str);
        result->name = name_lit->literal.val.str;
        result->function.index = index;
        result->function.return_type = return_type;
        return result;
    }

    IR_Value* ir_label(IR_Builder* builder, const char* name)
    {
        assert(builder);
        assert(name);

        IR_Value* result = ir_value_make(builder, IRV_LABEL, name);
        result->label.emitted = false;
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
