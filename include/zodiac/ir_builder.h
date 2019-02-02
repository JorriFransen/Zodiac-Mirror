#pragma once

#include "ir.h"

namespace Zodiac
{
    struct IR_Builder
    {
        Arena* arena = nullptr;

        BUF(IR_Instruction) instructions = nullptr;
        BUF(IR_Value*) types = nullptr;
    };

    void ir_builder_init(IR_Builder* ir_builder, Arena* arena);
    IR_Value* ir_builder_emit(IR_Builder* builder, IR_Instruction_Kind iri_kind,
                              IR_Value* arg_0, IR_Value* arg_1);
    IR_Instructions ir_builder_export(IR_Builder* builder);
}
