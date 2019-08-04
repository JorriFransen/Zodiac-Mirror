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

            tid->aggregate_members =
                (Type_Info_Aggregate_Member*)mem_alloc(sizeof(Type_Info_Aggregate_Member) * 16);
            tid->agg_count = 0;
            tid->agg_cap = 16;
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

            case AST_TYPE_STRUCT:
            {
                tid->type_infos[index].kind = STRUCT;
                BUF(uint64_t) indices = nullptr;

                auto aggregate_members = type->aggregate_type.member_declarations;
                for (uint64_t i = 0; i < BUF_LENGTH(aggregate_members); i++)
                {
                    AST_Declaration* member_decl = aggregate_members[i];
                    AST_Type* member_type = nullptr;

                    if (member_decl->kind == AST_DECL_MUTABLE)
                    {
                        member_type = member_decl->mutable_decl.type;
                    }
                    else assert(false);

                    assert(member_type);

                    maybe_register_type_info(context, member_type);
                    BUF_PUSH(indices, member_type->info_index);
                }

                tid->type_infos[index].aggregate.count = BUF_LENGTH(indices);
                tid->type_infos[index].aggregate.first.id =
                    register_aggregate_members(context, indices, aggregate_members);
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

    uint64_t register_aggregate_members(Context* context, BUF(uint64_t) indices,
                                        BUF(AST_Declaration*) member_decls)
    {
        assert(context);
        assert(indices);
        assert(member_decls);
        assert(BUF_LENGTH(indices) == BUF_LENGTH(member_decls));

        auto count = BUF_LENGTH(indices);

        auto tid = &context->type_info_data;

        ensure_aggregate_member_capacity(context, count);

        auto first_index = tid->agg_count;

        for (uint64_t i = 0; i < count; i++)
        {
            AST_Declaration* decl = member_decls[i];

            Type_Info_Aggregate_Member agg_mem = {};
            agg_mem.type.id = indices[i];

            if (decl->identifier)
            {
                agg_mem.name.data = decl->identifier->atom.data;
                agg_mem.name.length = decl->identifier->atom.length;
            }

            tid->aggregate_members[first_index + i] = agg_mem;
        }

        tid->agg_count += count;

        return first_index;
    }

    void ensure_aggregate_member_capacity(Context* context, uint64_t free_required)
    {
        assert(context);
        assert(free_required);

        auto tid = &context->type_info_data;

        if (tid->agg_count + free_required > tid->agg_cap)
        {
            uint64_t new_cap = MAX(tid->agg_cap + free_required, tid->agg_cap * 2);
            auto new_agg_members =
                (Type_Info_Aggregate_Member*)mem_alloc(sizeof(Type_Info_Aggregate_Member) *
                                                       new_cap);
            memcpy(new_agg_members, tid->aggregate_members,
                   sizeof(Type_Info_Aggregate_Member) * tid->agg_cap);
            mem_free(tid->aggregate_members);
            tid->aggregate_members = new_agg_members;
            tid->agg_cap = new_cap;
        }
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

        auto type_info_count = source->type_info_count;
        auto agg_member_count = source->agg_count;

        auto total_size = (type_info_count * sizeof(Type_Info)) +
            (agg_member_count * sizeof(Type_Info_Aggregate_Member));

        dest->type_infos = (Type_Info*)_arena_alloc(arena, total_size);
        dest->type_info_count = source->type_info_count;
        dest->type_info_cap = source->type_info_count;

        dest->aggregate_members = (Type_Info_Aggregate_Member*)(dest->type_infos +
                                                                dest->type_info_count);
        dest->agg_count = source->agg_count;
        dest->agg_cap = source->agg_count;

        memcpy(dest->type_infos, source->type_infos, sizeof(Type_Info) * source->type_info_count);
        memcpy(dest->aggregate_members, source->aggregate_members,
               sizeof(Type_Info_Aggregate_Member) * agg_member_count);
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

                case STRUCT:
                {
                    auto id = type_info->aggregate.first.id;
                    assert(id < tid->agg_count);
                    type_info->aggregate.first.aggregate_member = &tid->aggregate_members[id];
                    break;
                }

                default: assert(false);
            }
        }

        for (uint64_t i = 0; i < tid->agg_count; i++)
        {
            Type_Info_Aggregate_Member* mem_info = &tid->aggregate_members[i];

            auto id = mem_info->type.id;
            assert(id < tid->type_info_count);
            mem_info->type.type_info = &tid->type_infos[id];
        }
    }
}
