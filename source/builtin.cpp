#include "builtin.h"

namespace Zodiac
{
    void init_builtin_types(Context* context)
    {
        assert(context);

        Builtin::type_void = register_builtin_type(context, AST_TYPE_FLAG_VOID, 0, "void");

        AST_Type_Flags signed_int_flags = AST_TYPE_FLAG_INT | AST_TYPE_FLAG_SIGNED;
        Builtin::type_int = register_builtin_type(context, signed_int_flags, 64, "int");

        AST_Type_Flags unsigned_int_flags = AST_TYPE_FLAG_INT;
        Builtin::type_char = register_builtin_type(context, unsigned_int_flags, 8, "char");
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

    AST_Type* Builtin::type_void = nullptr;
    AST_Type* Builtin::type_int = nullptr;
    AST_Type* Builtin::type_char = nullptr;
    AST_Type* Builtin::type_bool = nullptr;
}
