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
	struct AST_Scope;

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

    struct Options
    {
		const char* exe_name = nullptr;
		const char* main_file_name = nullptr;
        bool verbose = false;
        bool print_ir = false;
		bool execute_ir = false;
    };

    enum Option_Kind
    {
        OPTION_INVALID,
        OPTION_BOOL,
    };

    struct Option
    {
        Option_Kind kind = OPTION_INVALID;
        const char* long_name = nullptr;
        char short_name = 0;

        uint64_t option_offset = 0;
    };

	struct Option_Parse_Context
	{
		Options* options = nullptr;
        Option* templates = nullptr;
        uint64_t template_count;

		uint32_t arg_count = 0;
		uint32_t arg_index = 0;
		char** args = nullptr;
	};

    struct Context
    {
        Arena* arena = nullptr;
        Atom_Table* atom_table;
        BUF(Atom) module_search_path = {};

		AST_Scope* builtin_scope = nullptr;

        BUF(Registered_Keyword) keywords = nullptr;
        BUF(AST_Declaration*) builtin_decls = nullptr;

        // BUF(AST_Type*) types = nullptr;
        AST_Type** type_hash = nullptr;
        uint64_t type_count = 0;

        BUF(Compiled_Module) compiled_modules = nullptr;
        BUF(Compiled_Module) modules_with_errors = nullptr;
        BUF(Atom) foreign_table = nullptr;

        IR_Value* global_init_block = nullptr;
        BUF(Global_Variable) global_table = nullptr;

        Options options;
    };

    bool context_init(Context* context, Arena* arena, Options options);

	bool zodiac_parse_options(Options* options, int argc, char** argv);
	bool zodiac_parse_option_argument(Option_Parse_Context* opc);
    bool zodiac_match_long_option(Option_Parse_Context* opc, const char* option_name);
    bool zodiac_match_short_option(Option_Parse_Context* opc, char c);

    AST_Module* zodiac_compile_or_get_module(Context* context, const Atom& module_path,
                                             const Atom& module_name);
    AST_Module* zodiac_compile_module(Context* context, const Atom& module_path,
                                      const Atom& module_name);

    static void context_init_keywords(Context* context);
    static bool init_module_search_path(Context* context, Atom first_file_path);
}
