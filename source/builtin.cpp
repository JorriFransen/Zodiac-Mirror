#include "builtin.h"

namespace Zodiac
{
    AST_Type* Builtin::type_void = nullptr;
    AST_Type* Builtin::type_int = nullptr;
    AST_Type* Builtin::type_u8 = nullptr;
    AST_Type* Builtin::type_bool = nullptr;

    AST_Declaration* Builtin::decl_PLATFORM_WINDOWS = nullptr;
    AST_Declaration* Builtin::decl_PLATFORM_LINUX = nullptr;

    void init_builtin_types(Context* context)
    {
        assert(context);

        Builtin::type_void = register_builtin_type(context, AST_TYPE_FLAG_VOID, 0, "void");

        AST_Type_Flags signed_int_flags = AST_TYPE_FLAG_INT | AST_TYPE_FLAG_SIGNED;
        Builtin::type_int = register_builtin_type(context, signed_int_flags, 64, "int");

        AST_Type_Flags unsigned_int_flags = AST_TYPE_FLAG_INT;
        Builtin::type_u8 = register_builtin_type(context, unsigned_int_flags, 8, "u8");
        Builtin::type_bool = register_builtin_type(context, unsigned_int_flags, 64, "bool");
    }

    AST_Type* register_builtin_type(Context* context, AST_Type_Flags flags, uint64_t size, const char* name)
    {
        assert(context);
        assert(name);

        File_Pos file_pos = {};
        file_pos.file_name = "<builtin>";
        Atom name_atom = atom_get(context->atom_table, name);
        AST_Identifier* identifier = ast_identifier_new(context, name_atom, file_pos);
        AST_Type* type = ast_type_base_new(context, flags, size);
        AST_Declaration* type_decl = ast_type_declaration_new(context, file_pos, type, identifier);
        type_decl->flags |= AST_DECL_FLAG_RESOLVED;

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
#elif LINUX
        platform_windows = true;
#endif

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
