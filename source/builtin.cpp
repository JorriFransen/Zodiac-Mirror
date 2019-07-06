#include "builtin.h"

namespace Zodiac
{
    AST_Type* Builtin::type_void = nullptr;
    AST_Type* Builtin::type_int = nullptr;
    AST_Type* Builtin::type_u8 = nullptr;
    AST_Type* Builtin::type_u32 = nullptr;
    AST_Type* Builtin::type_u64 = nullptr;
    AST_Type* Builtin::type_s8 = nullptr;
    AST_Type* Builtin::type_s32 = nullptr;
    AST_Type* Builtin::type_bool = nullptr;
    AST_Type* Builtin::type_float = nullptr;
    AST_Type* Builtin::type_double = nullptr;
    AST_Type* Builtin::type_pointer_to_u8 = nullptr;
    AST_Type* Builtin::type_String = nullptr;

    uint64_t Builtin::pointer_size = 0;

    AST_Declaration* Builtin::decl_PLATFORM_WINDOWS = nullptr;
    AST_Declaration* Builtin::decl_PLATFORM_LINUX = nullptr;
    AST_Declaration* Builtin::decl_string_length = nullptr;

	Atom Builtin::atom_main;
    Atom Builtin::atom_insert;
    Atom Builtin::atom_String;
    Atom Builtin::atom_string_length;
    Atom Builtin::atom_overload_index;

    void init_builtin_types(Context* context)
    {
        assert(context);


        Builtin::type_void = register_builtin_type(context, AST_TYPE_FLAG_VOID, 0, "void");

        AST_Type_Flags signed_int_flags = AST_TYPE_FLAG_INT | AST_TYPE_FLAG_SIGNED;
        Builtin::type_int = register_builtin_type(context, signed_int_flags, 64, "int");
        Builtin::type_s8 = register_builtin_type(context, signed_int_flags, 8, "s8");
        Builtin::type_s32 = register_builtin_type(context, signed_int_flags, 32, "s32");

        AST_Type_Flags unsigned_int_flags = AST_TYPE_FLAG_INT;
        Builtin::type_u8 = register_builtin_type(context, unsigned_int_flags, 8, "u8");
        Builtin::type_u32 = register_builtin_type(context, unsigned_int_flags, 32, "u32");
        Builtin::type_u64 = register_builtin_type(context, unsigned_int_flags, 64, "u64");
        Builtin::type_bool = register_builtin_type(context, unsigned_int_flags, 64, "bool");

        Builtin::type_float = register_builtin_type(context, AST_TYPE_FLAG_FLOAT, 32, "float");
        Builtin::type_double = register_builtin_type(context, AST_TYPE_FLAG_FLOAT, 64, "double");

        Builtin::pointer_size = Builtin::type_int->bit_size;

        Builtin::type_pointer_to_u8 = ast_find_or_create_pointer_type(context, Builtin::type_u8);

		Builtin::atom_main = atom_get(context->atom_table, "main");
        Builtin::atom_insert = atom_get(context->atom_table, "insert");
        Builtin::atom_String = atom_get(context->atom_table, "String");
        Builtin::atom_string_length = atom_get(context->atom_table, "string_length");
        Builtin::atom_overload_index = atom_get(context->atom_table, "overload_index");
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

        bool platform_windows = false;
        bool platform_linux = false;

#ifdef WIN32 
        platform_windows = true;
#elif defined __linux__
        platform_linux = true;
#elif
        assert(false);
#endif

        if (context->options.verbose)
        {
            if (platform_windows)
            {
                printf("Windows mode\n");
            }
            else if (platform_linux)
            {
                printf("Linux mode\n");
            }
        }

        Builtin::decl_PLATFORM_WINDOWS = register_builtin_constant_bool(context,
            "PLATFORM_WINDOWS", platform_windows);
        Builtin::decl_PLATFORM_LINUX = register_builtin_constant_bool(context,
            "PLATFORM_LINUX", platform_linux);

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
        AST_Type_Spec* type_spec = ast_type_spec_identifier_new(context, file_pos, type_spec_ident);

        AST_Expression* init_expression = ast_boolean_literal_expression_new(context, file_pos,
            value);
        init_expression->type = Builtin::type_bool;

        AST_Declaration* result = ast_constant_variable_declaration_new(context, file_pos,
            identifier, type_spec, init_expression, AST_DECL_LOC_GLOBAL);
        result->flags |= AST_DECL_FLAG_RESOLVED;
        result->constant_var.type = Builtin::type_bool;

        BUF_PUSH(context->builtin_decls, result);

        return result;
    }
}
