#pragma once

#include "zodiac.h"
#include "ast.h"

namespace Zodiac
{
    void init_builtin_types(Context* context);
    AST_Type* register_builtin_type(Context* context, AST_Type_Flags flags, uint64_t size, const char* name);

    struct Builtin
    {
        static AST_Type* type_void;
        static AST_Type* type_int;
        static AST_Type* type_bool;
    };
}
