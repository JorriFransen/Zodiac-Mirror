#include "builtin.h"

namespace Zodiac
{
    AST_Type* Builtin::type_void = nullptr;
    // AST_Type* Builtin::type_int = nullptr;
    AST_Type* Builtin::type_u8 = nullptr;
    AST_Type* Builtin::type_u16 = nullptr;
    AST_Type* Builtin::type_u32 = nullptr;
    AST_Type* Builtin::type_u64 = nullptr;
    AST_Type* Builtin::type_s8 = nullptr;
    AST_Type* Builtin::type_s16 = nullptr;
    AST_Type* Builtin::type_s32 = nullptr;
    AST_Type* Builtin::type_s64 = nullptr;
    AST_Type* Builtin::type_bool = nullptr;
    AST_Type* Builtin::type_float = nullptr;
    AST_Type* Builtin::type_double = nullptr;
    AST_Type* Builtin::type_pointer_to_void = nullptr;
    AST_Type* Builtin::type_pointer_to_u64 = nullptr;
    AST_Type* Builtin::type_pointer_to_u8 = nullptr;
    AST_Type* Builtin::type_String = nullptr;
    AST_Type* Builtin::type_Thread = nullptr;
    AST_Type* Builtin::type_pointer_to_Thread = nullptr;
    AST_Type* Builtin::type_Type_Info = nullptr;
    AST_Type* Builtin::type_pointer_to_Type_Info = nullptr;
    AST_Type* Builtin::type_Type_Info_Kind = nullptr;
    AST_Type* Builtin::type_Type_Info_Flags = nullptr;
    AST_Type* Builtin::type_Type_Info_Aggregate_Member = nullptr;
    AST_Type* Builtin::type_Type_Info_Enum_Member = nullptr;
    AST_Type* Builtin::type_Any = nullptr;
    AST_Type* Builtin::type_pointer_to_Any = nullptr;
    AST_Type* Builtin::type_Array_Ref_of_Any = nullptr;

    uint64_t Builtin::pointer_size = 0;

    AST_Declaration* Builtin::decl_ZODIAC_RUNNING_BYTECODE = nullptr;
    AST_Declaration* Builtin::decl_string_length = nullptr;
    AST_Declaration* Builtin::decl_default_assert_handler = nullptr;

	Atom Builtin::atom_main;
    Atom Builtin::atom_Any;
    Atom Builtin::atom_Array_Ref_of_Any;
    Atom Builtin::atom_insert;
    Atom Builtin::atom_required;
    Atom Builtin::atom_String;
    Atom Builtin::atom_string_length;
    Atom Builtin::atom_overload;
    Atom Builtin::atom_Thread;
    Atom Builtin::atom___create_thread__;
    Atom Builtin::atom___join_thread__;
    Atom Builtin::atom___compare_and_swap__;
    Atom Builtin::atom_Type_Info;
    Atom Builtin::atom_Type_Info_Kind;
    Atom Builtin::atom_Type_Info_Flags;
    Atom Builtin::atom_Type_Info_Aggregate_Member;
    Atom Builtin::atom_Type_Info_Enum_Member;
    Atom Builtin::atom_default_assert_handler;

    AST_Identifier* Builtin::identifier_Thread = nullptr;

    void init_builtin_types(Context* context)
    {
        assert(context);


        Builtin::type_void = register_builtin_type(context, AST_TYPE_FLAG_VOID, 0, "void");

        AST_Type_Flags signed_int_flags = AST_TYPE_FLAG_INT | AST_TYPE_FLAG_SIGNED;
        // Builtin::type_int = register_builtin_type(context, signed_int_flags, 64, "int");
        Builtin::type_s8 = register_builtin_type(context, signed_int_flags, 8, "s8");
        Builtin::type_s16 = register_builtin_type(context, signed_int_flags, 16, "s16");
        Builtin::type_s32 = register_builtin_type(context, signed_int_flags, 32, "s32");
        Builtin::type_s64 = register_builtin_type(context, signed_int_flags, 64, "s64");

        AST_Type_Flags unsigned_int_flags = AST_TYPE_FLAG_INT;
        Builtin::type_u8 = register_builtin_type(context, unsigned_int_flags, 8, "u8");
        Builtin::type_u16 = register_builtin_type(context, unsigned_int_flags, 16, "u16");
        Builtin::type_u32 = register_builtin_type(context, unsigned_int_flags, 32, "u32");
        Builtin::type_u64 = register_builtin_type(context, unsigned_int_flags, 64, "u64");
        Builtin::type_bool = register_builtin_type(context, unsigned_int_flags, 8, "bool");

        Builtin::type_float = register_builtin_type(context, AST_TYPE_FLAG_FLOAT, 32, "float");
        Builtin::type_double = register_builtin_type(context, AST_TYPE_FLAG_FLOAT, 64, "double");

        Builtin::pointer_size = 64;

        Builtin::type_pointer_to_void = ast_find_or_create_pointer_type(context,
                                                                        Builtin::type_void);
        Builtin::type_pointer_to_u64 = ast_find_or_create_pointer_type(context, Builtin::type_u64);
        Builtin::type_pointer_to_u8 = ast_find_or_create_pointer_type(context, Builtin::type_u8);

		Builtin::atom_main = atom_get(context->atom_table, "main");
        Builtin::atom_Any = atom_get(context->atom_table, "Any");
        Builtin::atom_Array_Ref_of_Any = atom_get(context->atom_table, "Array_Ref_of_Any");
        Builtin::atom_insert = atom_get(context->atom_table, "insert");
        Builtin::atom_required = atom_get(context->atom_table, "required");
        Builtin::atom_String = atom_get(context->atom_table, "String");
        Builtin::atom_string_length = atom_get(context->atom_table, "string_length");
        Builtin::atom_overload = atom_get(context->atom_table, "overload");
        Builtin::atom_Thread = atom_get(context->atom_table, "Thread");
        Builtin::atom___create_thread__ = atom_get(context->atom_table, "__create_thread__");
        Builtin::atom___join_thread__ = atom_get(context->atom_table, "__join_thread__");
        Builtin::atom___compare_and_swap__ = atom_get(context->atom_table, "__compare_and_swap__");
        Builtin::atom_Type_Info = atom_get(context->atom_table, "Type_Info");
        Builtin::atom_Type_Info_Kind = atom_get(context->atom_table, "Type_Info_Kind");
        Builtin::atom_Type_Info_Flags = atom_get(context->atom_table, "Type_Info_Flags");
        Builtin::atom_Type_Info_Aggregate_Member = atom_get(context->atom_table,
                                                            "Type_Info_Aggregate_Member");
        Builtin::atom_Type_Info_Enum_Member = atom_get(context->atom_table,
                                                            "Type_Info_Enum_Member");
        Builtin::atom_default_assert_handler = atom_get(context->atom_table,
                                                        "default_assert_handler");

        File_Pos builtin_file_pos = {};
        builtin_file_pos.file_name = "<builtin>";

        Builtin::identifier_Thread = ast_identifier_new(context, Builtin::atom_Thread,
                                                        builtin_file_pos);
    }

    AST_Type* register_builtin_type(Context* context, AST_Type_Flags flags, uint64_t size,
                                    const char* name)
    {
        assert(context);
        assert(name);

        File_Pos file_pos = {};
        file_pos.file_name = "<builtin>";
        Atom name_atom = atom_get(context->atom_table, name);
        AST_Identifier* identifier = ast_identifier_new(context, name_atom, file_pos);
        AST_Type* type = ast_type_base_new(context, flags, name_atom.data, size);
        AST_Declaration* type_decl = ast_type_declaration_new(context, file_pos, type, identifier);
        type_decl->flags |= AST_DECL_FLAG_RESOLVED;
		type_decl->location = AST_DECL_LOC_GLOBAL;

        BUF_PUSH(context->builtin_decls, type_decl);

        return type;
    }

    void init_builtin_decls(Context* context)
    {
        assert(context);

        Builtin::decl_ZODIAC_RUNNING_BYTECODE = register_builtin_constant_bool(
            context, "ZODIAC_RUNNING_BYTECODE", context->options.execute_ir);
    }

    AST_Declaration* register_builtin_constant_bool(Context* context, const char* name, bool value)
    {
        assert(context);
        assert(name);

        File_Pos file_pos;
        file_pos.char_pos = 0;
        file_pos.line_relative_char_pos = 0;
        file_pos.line = 0;
        file_pos.file_name = "<builtin>";

        Atom identifier_atom = atom_get(context->atom_table, name);
        AST_Identifier* identifier = ast_identifier_new(context, identifier_atom, file_pos);

        Atom type_spec_atom = atom_get(context->atom_table, "bool");
        AST_Identifier* type_spec_ident = ast_identifier_new(context, type_spec_atom, file_pos);
        AST_Type_Spec* type_spec = ast_type_spec_identifier_new(context, file_pos, type_spec_ident,
                                                                nullptr);

        AST_Expression* init_expression = ast_boolean_literal_expression_new(context, file_pos,
            value);
        init_expression->type = Builtin::type_bool;

        AST_Declaration* result = ast_constant_variable_declaration_new(context, file_pos,
                                                                        identifier, type_spec,
                                                                        init_expression,
                                                                        AST_DECL_LOC_GLOBAL);
        result->flags |= AST_DECL_FLAG_RESOLVED;
        result->constant_var.type = Builtin::type_bool;

        result->flags |= AST_DECL_FLAG_BUILTIN;

        BUF_PUSH(context->builtin_decls, result);

        return result;
    }
}
