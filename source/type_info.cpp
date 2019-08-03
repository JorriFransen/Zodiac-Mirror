#include "type_info.h"

#include "ast.h"
#include "zodiac.h"

namespace Zodiac
{
    void maybe_register_type_info(Context* context, AST_Type* type)
    {
        assert(context);
        assert(type);

        // TODO: Seperate the init
        Type_Info_Data* tid = &context->type_info_data;
        if (!tid->type_infos)
        {
            assert(!tid->type_info_count);
            tid->type_infos = (Type_Info*)mem_alloc(sizeof(Type_Info) * 16);
            tid->type_info_count = 1;
            tid->type_info_cap = 16;
        }

        if (type->info_index != 0)
        {
            return;
        }

        uint64_t index = next_type_info_index(context);

        switch (type->kind)
        {
            case AST_TYPE_BASE:
            {
                tid->type_infos[index].kind = BASE;
                break;
            }

            case AST_TYPE_POINTER:
            {
                tid->type_infos[index].kind = POINTER;
                break;
            }

            default: assert(false);
        }

        Type_Info* type_info = &tid->type_infos[index];
        // TODO: We should copy this string or replace it with a pointer to the actual
        //        string in code later
        if (type->name)
        {
            type_info->name.data = type->name;
            type_info->name.length = strlen(type->name);
        }
        else
        {
            type_info->name.data = nullptr;
            type_info->name.length = 0;
        }
        type_info->byte_size = type->bit_size / 8;

        type->info_index = index;
    }

    uint64_t next_type_info_index(Context* context)
    {
        assert(context);

        auto tid = &context->type_info_data;
        if (tid->type_info_count >= tid->type_info_cap)
        {
            grow_type_info_data(context);
        }

        uint64_t index = tid->type_info_count;
        tid->type_info_count += 1;
        return index;
    }

    void grow_type_info_data(Context* context)
    {
        assert(context);

        auto tid = &context->type_info_data;

        uint64_t new_cap = tid->type_info_cap * 2;
        Type_Info* new_infos = (Type_Info*)mem_alloc(sizeof(Type_Info) * new_cap);
        memcpy(new_infos, tid->type_infos, sizeof(Type_Info) * tid->type_info_cap);
        mem_free(tid->type_infos);
        tid->type_infos = new_infos;
        tid->type_info_cap = new_cap;
    }
}
