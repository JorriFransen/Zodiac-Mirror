#include "llvm_types.h"

#include "builtin.h"
#include "llvm.h"

namespace Zodiac
{
    bool LLVM_Type::generated = false;

    LLVMTypeRef LLVM_Type::u8 = nullptr;
    LLVMTypeRef LLVM_Type::u16 = nullptr;
    LLVMTypeRef LLVM_Type::u32 = nullptr;
    LLVMTypeRef LLVM_Type::u64 = nullptr;

    LLVMTypeRef LLVM_Type::String = nullptr;
    LLVMTypeRef LLVM_Type::Type_Info = nullptr;
    LLVMTypeRef LLVM_Type::Type_Info_Kind = nullptr;
    LLVMTypeRef LLVM_Type::Type_Info_Info_Union = nullptr;
    LLVMTypeRef LLVM_Type::Type_Info_Aggregate_Member = nullptr;
    LLVMTypeRef LLVM_Type::Type_Info_Enum_Member = nullptr;

    LLVMTypeRef LLVM_Type::_instance_Type_Info_Base = nullptr;
    LLVMTypeRef LLVM_Type::_instance_Type_Info_Pointer = nullptr;
    LLVMTypeRef LLVM_Type::_instance_Type_Info_Aggregate = nullptr;
    LLVMTypeRef LLVM_Type::_instance_Type_Info_Enum = nullptr;
    LLVMTypeRef LLVM_Type::_instance_Type_Info_Function = nullptr;

    LLVMTypeRef LLVM_Type::ptr_to_u8 = nullptr;
    LLVMTypeRef LLVM_Type::ptr_to_Type_Info = nullptr;

    void maybe_init_llvm_types(LLVM_IR_Builder* builder)
    {
        if (LLVM_Type::generated)
        {
            return;
        }

        LLVM_Type::u8 = llvm_type_from_ast(builder, Builtin::type_u8);
        LLVM_Type::u16 = llvm_type_from_ast(builder, Builtin::type_u16);
        LLVM_Type::u32 = llvm_type_from_ast(builder, Builtin::type_u32);
        LLVM_Type::u64 = llvm_type_from_ast(builder, Builtin::type_u64);

        LLVM_Type::ptr_to_u8 = llvm_type_from_ast(builder, Builtin::type_pointer_to_u8);
        LLVM_Type::ptr_to_Type_Info = llvm_type_from_ast(builder,
                                                         Builtin::type_pointer_to_Type_Info);

        assert(Builtin::type_String);
        LLVM_Type::String = llvm_type_from_ast(builder, Builtin::type_String);
        assert(Builtin::type_Type_Info);
        LLVM_Type::Type_Info = llvm_type_from_ast(builder, Builtin::type_Type_Info);
        assert(Builtin::type_Type_Info_Kind);
        LLVM_Type::Type_Info_Kind = llvm_type_from_ast(builder, Builtin::type_Type_Info_Kind);

        AST_Declaration* info_type_decl =
            Builtin::type_Type_Info->aggregate_type.member_declarations[3];
        LLVM_Type::Type_Info_Info_Union = llvm_type_from_ast(builder,
                                                             info_type_decl->mutable_decl.type);

        LLVM_Type::Type_Info_Aggregate_Member =
            llvm_type_from_ast(builder, Builtin::type_Type_Info_Aggregate_Member);
        LLVM_Type::Type_Info_Enum_Member = llvm_type_from_ast(builder,
                                                              Builtin::type_Type_Info_Enum_Member);

        LLVMTypeRef base_types[] = { LLVM_Type::Type_Info_Kind, LLVM_Type::String, LLVM_Type::u64,
                                     LLVM_Type::Type_Info_Info_Union };
        unsigned base_type_count = STATIC_ARRAY_LENGTH(base_types);
        LLVM_Type::_instance_Type_Info_Base = LLVMStructType(base_types, base_type_count, false);

        LLVMTypeRef pointer_aggregate_members[] = { LLVM_Type::ptr_to_Type_Info };
        unsigned pointer_aggregate_member_count = STATIC_ARRAY_LENGTH(pointer_aggregate_members);
        LLVMTypeRef pointer_aggregate = LLVMStructType(pointer_aggregate_members,
                                                       pointer_aggregate_member_count, false);
        LLVMTypeRef pointer_types[] = { LLVM_Type::Type_Info_Kind, LLVM_Type::String,
                                        LLVM_Type::u64, pointer_aggregate };
        unsigned pointer_type_count = STATIC_ARRAY_LENGTH(pointer_types);
        LLVM_Type::_instance_Type_Info_Pointer = LLVMStructType(pointer_types, pointer_type_count,
                                                                false);

        LLVMTypeRef agg_mem_info_type =
            llvm_type_from_ast(builder, Builtin::type_Type_Info_Aggregate_Member);
        LLVMTypeRef ptr_agg_mem_info_type = LLVMPointerType(agg_mem_info_type, 0);

        LLVMTypeRef struct_aggregates[] = { LLVM_Type::u64, ptr_agg_mem_info_type };
        unsigned struct_aggregate_count = STATIC_ARRAY_LENGTH(struct_aggregates);
        LLVMTypeRef struct_aggregate = LLVMStructType(struct_aggregates, struct_aggregate_count,
                                                      false);
        struct_aggregate = LLVMStructType(&struct_aggregate, 1, false);

        LLVMTypeRef struct_types[] = { LLVM_Type::Type_Info_Kind, LLVM_Type::String,
                                       LLVM_Type::u64, struct_aggregate };
        unsigned struct_type_count = STATIC_ARRAY_LENGTH(struct_types);
        LLVM_Type::_instance_Type_Info_Aggregate = LLVMStructType(struct_types,
                                                                  struct_type_count, false);

        auto zir_ti_type = Builtin::type_Type_Info;
        AST_Declaration* enum_info_type_decl = zir_ti_type->aggregate_type.member_declarations[3];
        auto zir_enum_info_type = enum_info_type_decl->mutable_decl.type;
        enum_info_type_decl = zir_enum_info_type->aggregate_type.member_declarations[2];
        LLVMTypeRef llvm_enum_info_type =
            llvm_type_from_ast(builder, enum_info_type_decl->mutable_decl.type);
        llvm_enum_info_type = LLVMStructType(&llvm_enum_info_type, 1, false);
        LLVMTypeRef enum_types[] = { LLVM_Type::Type_Info_Kind, LLVM_Type::String, LLVM_Type::u64,
                                     llvm_enum_info_type };
        unsigned enum_type_count = STATIC_ARRAY_LENGTH(enum_types);
        LLVM_Type::_instance_Type_Info_Enum = LLVMStructType(enum_types, enum_type_count, false);

        AST_Declaration* ti_info_decl = zir_ti_type->aggregate_type.member_declarations[3];
        AST_Type* zir_ti_info_type = ti_info_decl->mutable_decl.type;
        AST_Declaration* function_info_decl =
            zir_ti_info_type->aggregate_type.member_declarations[3];
        AST_Type* zir_function_info_type = function_info_decl->mutable_decl.type;
        LLVMTypeRef llvm_function_info_type = llvm_type_from_ast(builder, zir_function_info_type);
        llvm_function_info_type = LLVMStructType(&llvm_function_info_type, 1, false);

        LLVMTypeRef function_types[] = { LLVM_Type::Type_Info_Kind, LLVM_Type::String,
                                         LLVM_Type::u64, llvm_function_info_type };
        unsigned function_type_count = STATIC_ARRAY_LENGTH(function_types);
        LLVM_Type::_instance_Type_Info_Function = LLVMStructType(function_types,
                                                                 function_type_count, false);

        LLVM_Type::generated = true;
    }
}
