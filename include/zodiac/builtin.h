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
        static AST_Type* type_u32;
        static AST_Type* type_u64;
        static AST_Type* type_s8;
        static AST_Type* type_s32;
        static AST_Type* type_bool;
        static AST_Type* type_float;
        static AST_Type* type_double;
        static AST_Type* type_pointer_to_void;
        static AST_Type* type_pointer_to_u8;
        static AST_Type* type_String;

        static uint64_t pointer_size;

        static AST_Declaration* decl_PLATFORM_WINDOWS;
        static AST_Declaration* decl_PLATFORM_LINUX;
        static AST_Declaration* decl_string_length;

		static Atom atom_main;
        static Atom atom_insert;
        static Atom atom_String;
        static Atom atom_string_length;
        static Atom atom_overload;
        static Atom atom_Thread;
        static Atom atom___create_thread__;
        static Atom atom___join_thread__;

        static AST_Identifier* identifier_Thread;
    };

    void init_builtin_types(Context* context);
    AST_Type* register_builtin_type(Context* context, AST_Type_Flags flags, uint64_t size, const char* name);

    void init_builtin_decls(Context* context);
    AST_Declaration* register_builtin_constant_bool(Context* context, const char* name, bool value);

}
