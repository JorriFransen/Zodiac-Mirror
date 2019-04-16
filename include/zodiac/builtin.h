#pragma once

#include "zodiac.h"
#include "ast.h"

namespace Zodiac
{
    struct Builtin
    {
        static AST_Type* type_void;
        static AST_Type* type_int;
        static AST_Type* type_u8;
        static AST_Type* type_bool;

        static AST_Declaration* decl_PLATFORM_WINDOWS;
        static AST_Declaration* decl_PLATFORM_LINUX;
    };

    void init_builtin_types(Context* context);
    AST_Type* register_builtin_type(Context* context, AST_Type_Flags flags, uint64_t size, const char* name);

    void init_builtin_decls(Context* context);
    AST_Declaration* register_builtin_constant_bool(Context* context, const char* name, bool value);

}
