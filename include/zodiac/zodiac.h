#pragma once

#include "atom.h"
#include "common.h"
#include "token.h"

namespace Zodiac
{
    struct Registered_Keyword
    {
        Atom atom = {};
        Token_Kind token_kind = TOK_INVALID;
    };

    struct Context
    {
        Arena* arena = nullptr;
        Atom_Table* atom_table;

        BUF(Registered_Keyword) keywords = nullptr;
    };

    void context_init(Context* context, Arena* arena);

    static void context_init_keywords(Context* context);
}
