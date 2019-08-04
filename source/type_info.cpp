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
                maybe_register_type_info(context, type->pointer.base);
                tid->type_infos[index].base.id = type->pointer.base->info_index;
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

    void copy_type_info(Arena* arena, Type_Info_Data* dest, Type_Info_Data* source)
    {
        assert(arena);
        assert(dest);
        assert(dest->type_infos == nullptr);
        assert(dest->type_info_count == 0);
        assert(source);
        if (!source->type_infos || !source->type_info_count)
        {
            return;
        }
        assert(source->type_infos);
        assert(source->type_info_count);

        dest->type_infos = (Type_Info*)arena_alloc_array(arena, Type_Info, source->type_info_count);
        dest->type_info_count = source->type_info_count;
        dest->type_info_cap = source->type_info_cap;

        memcpy(dest->type_infos, source->type_infos, sizeof(Type_Info) * source->type_info_count);
    }

    void patch_type_info_ids_with_pointers(Type_Info_Data* tid)
    {
        assert(tid);

        if (!tid->type_infos || !tid->type_info_count)
        {
            return;
        }

        assert(tid->type_infos);
        assert(tid->type_info_count);

        for (uint64_t i = 0; i < tid->type_info_count; i++)
        {
            Type_Info* type_info = &tid->type_infos[i];

            switch (type_info->kind)
            {
                case INVALID:
                {
                    *type_info = {};
                    break;
                }

                case BASE:
                {
                    break;
                }

                case POINTER:
                {
                    auto id = type_info->base.id;
                    assert(id < tid->type_info_count);
                    type_info->base.type_info = &tid->type_infos[id];
                    break;
                }

                default: assert(false);
            }
        }
    }
}
