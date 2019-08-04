#pragma once

#include "common.h"

namespace Zodiac
{
    enum Type_Info_Kind : uint64_t
    {
        INVALID,
        POINTER,
        STRUCT,
        UNION,
        ENUM,
        FUNCTION,
        BASE,
    };

    struct Type_Info;
    union Type_Info_Pointer_Or_Id
    {
        Type_Info* type_info;
        uint64_t id;
    };

    struct Type_Info
    {
        Type_Info_Kind kind = INVALID;

        struct
        {
            const char* data = nullptr;
            uint64_t length = 0;
        } name;

        uint64_t byte_size = 0;

        union
        {
            Type_Info_Pointer_Or_Id base;
        };
    };

    struct Type_Info_Data
    {
        Type_Info* type_infos = nullptr;
        uint64_t type_info_count = 0;
        uint64_t type_info_cap = 0;
    };

    struct Context;
    struct AST_Type;

    void maybe_register_type_info(Context* context, AST_Type* type);
    uint64_t next_type_info_index(Context* context);
    void grow_type_info_data(Context* context);

    void copy_type_info(Arena* arena, Type_Info_Data* dest, Type_Info_Data* source);
    void patch_type_info_ids_with_pointers(Type_Info_Data* tid);
}
