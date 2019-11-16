#pragma once

#include "llvm.h"
#include "ir.h"

#include <llvm/IR/DIBuilder.h>

namespace Zodiac
{
    using namespace llvm;

    struct Debug_Info
    {
        DICompileUnit* compile_unit = nullptr;
    };
}
