#include "token.h"

#include <stdio.h>
#include <inttypes.h>

namespace Zodiac
{


    const char* token_kind_string(Token_Kind token_kind)
    {
        assert(token_kind >= TOK_INVALID && token_kind <= TOK_COUNT);

        return _token_kind_strings[token_kind];
    }

    void print_token(const Token& token)
    {
        printf("%" PRIu64 ": %-14s: '%s'\n",
               token.file_pos.line,
               _token_kind_strings[token.kind],
               token.atom.data);
    }
}
