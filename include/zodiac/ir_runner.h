#pragma once

#include "ir.h"

namespace Zodiac
{
    struct IR_Runner
    {
        uint64_t ip;
    };

    void ir_run(IR_Runner* runner, IR_Instructions instructions);

    static bool ir_execute(IR_Runner* runner, IR_Instruction iri);
}
