#pragma once

#include "atom.h"
#include "common.h"

namespace Zodiac
{
    struct Context
    {
        Arena* arena = nullptr;
        Atom_Table* atom_table;
    };

    void context_init(Context* context, Arena* arena);
}
