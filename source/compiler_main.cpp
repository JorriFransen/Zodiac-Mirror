#include <stdio.h>

#include "common.h"

#include "ir.h"

using namespace Zodiac;

int main(int argc, char** argv)
{
    printf("compiler_main()\n");

    Arena ir_arena;
    IR_Builder ir_builder;

    ir_arena = arena_create(MB(4));
    ir_builder_init(&ir_builder, &ir_arena);

    IR_Value* s32 = ir_type(&ir_builder, 32, true, "s32");

    auto t0 = ir_builder_emit(&ir_builder, IRI_ALLOCL,
                              ir_literal(&ir_builder, "result"),
                              s32);
    auto t1 = ir_builder_emit(&ir_builder, IRI_ADD_S32,
                              ir_literal(&ir_builder, 5, s32),
                              ir_literal(&ir_builder, 8, s32));
    ir_builder_emit(&ir_builder, IRI_STOREL, t1, t0);
    auto t3 = ir_builder_emit(&ir_builder, IRI_LOADL, t0, nullptr);
    ir_builder_emit(&ir_builder, IRI_PRINT, t3, nullptr);

    return 0;
}

