#pragma once

#include "atom.h"
#include "common.h"

namespace Zodiac
{
    enum Type_Info_Kind : uint64_t
    {
        INVALID,
        BASE,
        POINTER,
        STRUCT,
        UNION,
        ENUM,
        FUNCTION,
        STATIC_ARRAY,
    };

    typedef uint64_t Type_Info_Flag_T;
    enum Type_Info_Flag : Type_Info_Flag_T
    {
        INT    = 1,
        FLOAT  = 2,
        SIGNED = 4,
        MRV    = 8,
    };

    struct Type_Info;
    struct Type_Info_Aggregate_Member;
    struct Type_Info_Enum_Member;
    union Type_Info_Pointer_Or_Id
    {
        Type_Info* type_info;
        Type_Info_Aggregate_Member* aggregate_member;
        Type_Info_Enum_Member* enum_member;
        uint64_t id;
    };

    struct Type_Info
    {
        Type_Info_Kind kind = INVALID;
        Type_Info_Flag_T flags = 0;

        struct
        {
            const char* data = nullptr;
            uint64_t length = 0;
        } name;

        uint64_t byte_size = 0;

        union
        {
            Type_Info_Pointer_Or_Id base;

            struct
            {
                uint64_t count;
                Type_Info_Pointer_Or_Id first;
            } aggregate;

            struct
            {
                Type_Info_Pointer_Or_Id base;
                uint64_t member_count;
                Type_Info_Pointer_Or_Id first;
            } enum_info;

            struct
            {
                Type_Info_Pointer_Or_Id return_type;
                uint64_t arg_count;
                Type_Info_Pointer_Or_Id first_arg;
            } function;

            struct
            {
                Type_Info_Pointer_Or_Id base;
                uint64_t count;
            } static_array;
        };
    };

    struct Type_Info_Aggregate_Member
    {
        struct
        {
            const char* data = nullptr;
            uint64_t length = 0;
        } name;

        Type_Info_Pointer_Or_Id type;
    };

    struct Type_Info_Enum_Member
    {
        struct
        {
            const char* data = nullptr;
            uint64_t length = 0;
        } name;

        uint64_t value = 0;

        Type_Info_Pointer_Or_Id type;
    };

    struct Type_Info_Data
    {
        Type_Info* type_infos = nullptr;
        uint64_t type_info_count = 0;
        uint64_t type_info_cap = 0;

        Type_Info_Aggregate_Member* aggregate_members = nullptr;
        uint64_t agg_count = 0;
        uint64_t agg_cap = 0;

        Type_Info_Enum_Member* enum_members = nullptr;
        uint64_t enum_count = 0;
        uint64_t enum_cap = 0;
    };

    struct Context;
    struct AST_Type;
    struct AST_Declaration;

    struct Type_Info_ID_Placeholder
    {
        uint64_t target_index = 0;
        AST_Type* target_type = nullptr;
    };

    uint64_t maybe_register_type_info(Context* context, AST_Type* type, bool root = true,
                                      BUF(Type_Info_ID_Placeholder) = nullptr);
    uint64_t next_type_info_index(Context* context);
    void replace_type_info_placeholders(Type_Info_Data* tid,
                                        BUF(Type_Info_ID_Placeholder) placeholders);
    void grow_type_info_data(Context* context);
    uint64_t register_aggregate_members(Context* context, BUF(uint64_t) indices,
                                        BUF(AST_Declaration*) member_decls);
    uint64_t register_enum_members(Context* context, BUF(AST_Declaration*) member_decls,
                                   AST_Type* base_type);
    void ensure_aggregate_member_capacity(Context* context, uint64_t free_required);
    void ensure_enum_member_capacity(Context* context, uint64_t free_required);

    void copy_type_info(Arena* arena, Type_Info_Data* dest, Type_Info_Data* source);
    void patch_type_info_ids_with_pointers(Type_Info_Data* tid);
}
