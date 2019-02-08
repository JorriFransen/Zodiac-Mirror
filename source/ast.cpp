#include "ast.h"

namespace Zodiac
{
    AST_Module* ast_module_new(Context* context, const char* module_name)
    {
        assert(context);
        assert(module_name);

        AST_Module* result = arena_alloc(context->arena, AST_Module);
        result->module_name = module_name;

        return result;
    }

    AST_Identifier* ast_identifier_new(Context* context, Atom atom, File_Pos file_pos)
    {
        assert(context);

        AST_Identifier* result = arena_alloc(context->arena, AST_Identifier);
        result->atom = atom;
        result->file_pos = file_pos;

        return result;
    }

    AST_Declaration* ast_function_declaration_new(Context* context, File_Pos file_pos,
                                                  AST_Function_Proto* proto, AST_Statement* body_block)
    {
        assert(context);
        assert(proto);

        AST_Declaration* result = arena_alloc(context->arena, AST_Declaration);
        result->file_pos = file_pos;
        AST_Function_Declaration func_decl = {};
        func_decl.proto = proto;
        func_decl.body_block = body_block;
        result->function = func_decl;

        return result;
    }

    AST_Function_Proto* ast_function_prototype_new(Context* context, File_Pos file_pos,
                                                   BUF(AST_Declaration*) args,
                                                   AST_Type_Spec* return_type_spec)
    {
        assert(context);

        AST_Function_Proto* result = arena_alloc(context->arena, AST_Function_Proto);
        result->file_pos = file_pos;
        result->args = args;
        result->return_type_spec = return_type_spec;

        return result;
    }
}
