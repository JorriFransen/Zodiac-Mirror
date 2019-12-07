#pragma once

#include <llvm-c/Core.h>

namespace Zodiac
{
    struct LLVM_Type
    {
        static bool generated;

        static LLVMTypeRef u8;
        static LLVMTypeRef u16;
        static LLVMTypeRef u32;
        static LLVMTypeRef u64;

        static LLVMTypeRef String;
        static LLVMTypeRef Type_Info;
        static LLVMTypeRef Type_Info_Kind;
        static LLVMTypeRef Type_Info_Flag;
        static LLVMTypeRef Type_Info_Info_Union;
        static LLVMTypeRef Type_Info_Aggregate_Member;
        static LLVMTypeRef Type_Info_Enum_Member;

        static LLVMTypeRef _instance_Type_Info_Base;
        static LLVMTypeRef _instance_Type_Info_Pointer;
        static LLVMTypeRef _instance_Type_Info_Aggregate;
        static LLVMTypeRef _instance_Type_Info_Enum;
        static LLVMTypeRef _instance_Type_Info_Function;

        static LLVMTypeRef ptr_to_u8;
        static LLVMTypeRef ptr_to_Type_Info;
    };

    struct LLVM_IR_Builder;

    void maybe_init_llvm_types(LLVM_IR_Builder* builder);
}
