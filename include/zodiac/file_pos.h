#pragma once

#include <stdint.h>

namespace Zodiac
{
    struct File_Pos
    {
        const char* file_name = nullptr;
        uint64_t line = 0;
        uint64_t line_relative_char_pos = 0;
        uint64_t char_pos = 0;
    };
}
