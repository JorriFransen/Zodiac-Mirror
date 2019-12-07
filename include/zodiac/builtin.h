#pragma once

#include "zodiac.h"
#include "ast.h"

namespace Zodiac
{
    struct Builtin
    {
        static AST_Type* type_void;
        // static AST_Type* type_int;
        static AST_Type* type_u8;
        static AST_Type* type_u16;
        static AST_Type* type_u32;
        static AST_Type* type_u64;
        static AST_Type* type_s8;
        static AST_Type* type_s32;
        static AST_Type* type_s16;
        static AST_Type* type_s64;
        static AST_Type* type_bool;
        static AST_Type* type_float;
        static AST_Type* type_double;
        static AST_Type* type_pointer_to_void;
        static AST_Type* type_pointer_to_u8;
        static AST_Type* type_pointer_to_u64;
        static AST_Type* type_String;
        static AST_Type* type_Thread;
        static AST_Type* type_pointer_to_Thread;
        static AST_Type* type_Type_Info;
        static AST_Type* type_pointer_to_Type_Info;
        static AST_Type* type_Type_Info_Kind;
        static AST_Type* type_Type_Info_Flags;
        static AST_Type* type_Type_Info_Aggregate_Member;
        static AST_Type* type_Type_Info_Enum_Member;

        static uint64_t pointer_size;

        static AST_Declaration* decl_ZODIAC_RUNNING_BYTECODE;
        static AST_Declaration* decl_string_length;
        static AST_Declaration* decl_default_assert_handler;

		static Atom atom_main;
        static Atom atom_insert;
        static Atom atom_required;
        static Atom atom_String;
        static Atom atom_string_length;
        static Atom atom_overload;
        static Atom atom_Thread;
        static Atom atom___create_thread__;
        static Atom atom___join_thread__;
        static Atom atom___compare_and_swap__;
        static Atom atom_Type_Info;
        static Atom atom_Type_Info_Kind;
        static Atom atom_Type_Info_Flags;
        static Atom atom_Type_Info_Aggregate_Member;
        static Atom atom_Type_Info_Enum_Member;
        static Atom atom_default_assert_handler;

        static AST_Identifier* identifier_Thread;
    };

    void init_builtin_types(Context* context);
    AST_Type* register_builtin_type(Context* context, AST_Type_Flags flags, uint64_t size,
                                    const char* name);

    void init_builtin_decls(Context* context);
    AST_Declaration* register_builtin_constant_bool(Context* context, const char* name,
                                                    bool value);

}
