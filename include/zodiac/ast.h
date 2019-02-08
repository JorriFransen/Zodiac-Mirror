#pragma once

#include "zodiac.h"
#include "file_pos.h"

namespace Zodiac
{
    struct AST_Module;
    struct AST_Identifier;
    struct AST_Expression;
    struct AST_Statement;
    struct AST_Declaration;
    struct AST_Type;
    struct AST_Type_Spec;

    struct AST_Module
    {
        BUF(AST_Declaration*) global_declarations = nullptr;

        const char* module_name = nullptr;
    };

    struct AST_Identifier
    {
        File_Pos file_pos = {};
        Atom atom = {};
    };

    struct AST_Expression
    {
        
    };

    struct AST_Statement
    {
        
    };

    struct AST_Function_Proto
    {
        File_Pos file_pos {};
        BUF(AST_Declaration*) args = nullptr;
        AST_Type_Spec* return_type_spec = nullptr;
    };

    struct AST_Function_Declaration
    {
        AST_Function_Proto* proto = nullptr;
        AST_Statement* body_block = nullptr;
    };

    struct AST_Declaration
    {
        File_Pos file_pos = {};
        union
        {
            AST_Function_Declaration function;
        };
    };

    struct AST_Type
    {
        
    };

    struct AST_Type_Spec
    {
        
    };

    AST_Module* ast_module_new(Context* context, const char* module_name);
    AST_Identifier* ast_identifier_new(Context* context, Atom atom, File_Pos file_pos);

    AST_Declaration* ast_function_declaration_new(Context* context, File_Pos file_pos,
                                                  AST_Function_Proto* proto, AST_Statement* body_block);
    AST_Function_Proto* ast_function_prototype_new(Context* context, File_Pos file_pos,
                                                   BUF(AST_Declaration*) args,
                                                   AST_Type_Spec* return_type_spec);
}
