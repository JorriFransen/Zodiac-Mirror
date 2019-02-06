#include "zodiac.h"

namespace Zodiac
{
    void context_init(Context* context, Arena* arena)
    {
        assert(context);
        assert(arena);

        context->arena = arena;
    }
}
