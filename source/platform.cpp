
#include "platform.h"

#include "common.h"

#include <cassert>

#ifdef WIN32
#include <Windows.h>
#elif defined __linux__
#include <cstdlib>
#endif

namespace Zodiac
{
    const char* platform_get_environment_var(const char* name)
    {
        assert(name);

        char*  result = nullptr;

#ifdef WIN32
        auto var_length = GetEnvironmentVariable(name, nullptr, 0);
        if (var_length)
        {
            char* buffer = (char*)malloc(var_length);
            assert(GetEnvironmentVariable(name, buffer, var_length) == var_length - 1);
            result = (char*)normalize_path(buffer);
            free(buffer);
        }
#elif defined __linux__
        result = getenv(name);
#endif


        return result;
    }
}
