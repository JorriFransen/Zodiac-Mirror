#include "zodiac.h"

namespace Zodiac
{
    void context_init(Context* context, Arena* arena)
    {
        assert(context);
        assert(arena);

        context->arena = arena;

        context->atom_table = (Atom_Table*)mem_alloc(sizeof(Atom_Table));
        atom_table_init(context->atom_table);
    }
}
