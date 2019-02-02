#pragma once

#include "common.h"

#include <stdint.h>

namespace Zodiac
{
    struct IR_Builder;

    enum IR_Instruction_Kind
    {
        IRI_INVALID,

        IRI_ALLOCL,
        IRI_LOADL,
        IRI_STOREL,
        IRI_ADD_S32,
        IRI_SUB_S32,

        IRI_LABEL,
        IRI_JMP_LABEL,
        IRI_JMP_LABEL_COND,

        IRI_LT_S32,
        IRI_NOT_BOOL,

        IRI_FUNC_DEFN,
        IRI_PUSH_CALL_ARG,
        IRI_POP_FUNC_ARG,
        IRI_CALL,
        IRI_RET,

        IRI_PRINT,

        IRI_COUNT
    };

    enum IR_Value_Kind
    {
        IRV_INVALID,

        IRV_TYPE,
        IRV_LITERAL,
        IRV_VALUE,
        IRV_ALLOCL,
        IRV_FUNCTION,
        IRV_LABEL,

        IRV_COUNT
    };

    enum IR_Literal_Kind
    {
        IR_LIT_INVALID,

        IR_LIT_STRING,
        IR_LIT_INT,

        IR_LIT_COUNT,
    };

    struct IR_Value
    {
        IR_Value_Kind kind = IRV_INVALID;

        const char* name = nullptr;

        union
        {
            struct
            {
                uint64_t size = 0;
                bool sign = false;
            } type;

            struct
            {
                IR_Value* type;
                IR_Literal_Kind kind = IR_LIT_INVALID;
                union
                {
                    const char* str;
                    int32_t s32;
                    bool b8;
                } val;
            } value, literal;

            struct
            {
                IR_Value* type;
                IR_Value* value;
            } allocl;

            struct
            {
                uint64_t index;
                IR_Value* return_type;
            } function;

            struct
            {
                uint64_t index;
                bool emitted;
            } label;
        };
    };

    struct IR_Instruction
    {
        IR_Instruction_Kind kind = IRI_INVALID;

        IR_Value* arg_0;
        IR_Value* arg_1;

        IR_Value* result_value;
    };

    struct IR_Instructions
    {
        IR_Instruction* data = nullptr;
        uint64_t count = 0;
    };

    IR_Value* ir_type(IR_Builder* builder, uint64_t size, bool sign, const char* name);
    IR_Value* ir_literal(IR_Builder* builder, const char* str);
    IR_Value* ir_literal(IR_Builder* builder, int32_t s32, IR_Value* type_value);
    IR_Value* ir_value(IR_Builder* builder, IR_Value* type_value, const char* name);
    IR_Value* ir_allocl(IR_Builder* builder, IR_Value* type_value, const char* name);
    IR_Value* ir_function(IR_Builder* builder, uint64_t index, IR_Value* return_type, IR_Value* name_lit);
    IR_Value* ir_label(IR_Builder* builder, const char* name);
    IR_Value* ir_value_make(IR_Builder* builder, IR_Value_Kind irv_kind,
                            const char* name = nullptr);
}
