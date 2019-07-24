
#include "platform.h"

namespace Zodiac
{
    const char* platform_get_environment_var(const char* name)
    {
		return _GET_ENVIRONMENT_VARIABLE(name);
    }
}
