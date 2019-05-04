#pragma once

#include "atom.h"
#include "common.h"
#include "token.h"

namespace Zodiac
{
    struct Registered_Keyword
    {
        Atom atom = {};
        Token_Kind token_kind = TOK_INVALID;
    };

    struct AST_Module;
    struct AST_Type;
    struct AST_Declaration;

    struct IR_Module;
    struct IR_Value;

    struct Compiled_Module
    {
        Atom module_name;
        Atom module_path;
        AST_Module* module;
    };

    struct Global_Variable
    {
        IR_Module* module = nullptr;
        IR_Value* value = nullptr;
    };

    struct Context
    {
        Arena* arena = nullptr;
        Atom_Table* atom_table;
        Atom module_search_path = {};

        BUF(Registered_Keyword) keywords = nullptr;
        BUF(AST_Declaration*) builtin_decls = nullptr;

        BUF(Compiled_Module) compiled_modules = nullptr;
        BUF(Atom) foreign_table = nullptr;

        IR_Value* global_init_block = nullptr;
        BUF(Global_Variable) global_table = nullptr;
    };

    bool context_init(Context* context, Arena* arena);

    AST_Module* zodiac_compile_or_get_module(Context* context, const Atom& module_path, const Atom& module_name);
    AST_Module* zodiac_compile_module(Context* context, const Atom& module_path, const Atom& module_name);

    static void context_init_keywords(Context* context);
    static bool init_module_path(Context* context);
}
