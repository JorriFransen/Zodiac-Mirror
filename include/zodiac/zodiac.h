#pragma once

#include "common.h"

namespace Zodiac
{
    struct Context
    {
        Arena* arena = nullptr;
    };

    void context_init(Context* context, Arena* arena);
}
