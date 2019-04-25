#include "zodiac.h"

#include "builtin.h"
#include "lexer.h"
#include "parser.h"
#include "platform.h"
#include "resolver.h"
#include "ir.h"

namespace Zodiac
{
    void context_init(Context* context, Arena* arena)
    {
        assert(context);
        assert(arena);

        context->arena = arena;

        context->atom_table = (Atom_Table*)mem_alloc(sizeof(Atom_Table));
        atom_table_init(context->atom_table);

        context->keywords =  nullptr;
        context->compiled_modules = nullptr;
        context->foreign_table = nullptr;
        context_init_keywords(context);

        init_builtin_types(context);
        init_builtin_decls(context);

        init_module_path(context);
    }

    AST_Module* zodiac_compile_or_get_module(Context* context, const Atom& module_path, const Atom& module_name)
    {
        assert(context);

        for (uint64_t i = 0; i < BUF_LENGTH(context->compiled_modules); i++)
        {
            const Compiled_Module& cm = context->compiled_modules[i];
            if (cm.module_path == module_path && cm.module_name == module_name)
            {
                return cm.module;
            }
        }

        return zodiac_compile_module(context, module_path, module_name);
    }

    AST_Module* zodiac_compile_module(Context* context, const Atom& module_path, const Atom& module_name)
    {
        assert(context);

        const char* module_string = read_file_string(module_path.data);
        assert(module_string);

        Lexer lexer;
        init_lexer(&lexer, context);

        Lex_Result lex_result = lex_file(&lexer, module_string, module_name.data);
        if (BUF_LENGTH(lex_result.errors) != 0)
        {
            lexer_report_errors(&lexer);
            return nullptr;
        }

        mem_free(module_string);

        Parser parser;
        parser_init(&parser, context);

        Parse_Result parse_result = parse_module(&parser, lex_result.tokens, module_name.data);
        if (BUF_LENGTH(parse_result.errors) != 0)
        {
            parser_report_errors(&parser);
            return nullptr;
        }

        Resolver resolver;
        resolver_init(&resolver, context, parse_result.ast_module);

        while (!resolver.done)
        {
            resolver_do_cycle(&resolver);

            if (!resolver.progressed_on_last_cycle)
            {
                break;
            }
        }

        if (!resolver.done)
        {
            resolver_report_errors(&resolver);
            return nullptr;
        }

        IR_Builder* ir_builder = arena_alloc(context->arena, IR_Builder);
        ir_builder_init(ir_builder, context);

        IR_Module ir_module = ir_builder_emit_module(ir_builder, parse_result.ast_module);

        if (ir_module.error_count)
        {
            fprintf(stderr, "IR builder return with errors, exiting\n");
            return nullptr;
        }

        IR_Validation_Result validation = ir_validate(ir_builder);

        if (!validation.messages)
        {
            fprintf(stderr, "Generated ir for file: %s:\n", module_path.data);
            ir_builder_print_result(ir_builder);
        }
        else
        {
            for (uint64_t i = 0; i < BUF_LENGTH(validation.messages); i++)
            {
                fprintf(stderr, "%s\n", validation.messages[i]);
            }
            return nullptr;
        }

        return parse_result.ast_module;
    }

#define DEFINE_KW(string, kw_kind) \
    { \
        Atom atom_for_string = atom_get(context->atom_table, string); \
        Registered_Keyword entry = { atom_for_string, kw_kind }; \
        BUF_PUSH(context->keywords, entry); \
    }

    static void context_init_keywords(Context* context)
    {
        assert(context);

        DEFINE_KW("true", TOK_KW_TRUE);
        DEFINE_KW("false", TOK_KW_FALSE);
        DEFINE_KW("return", TOK_KW_RETURN);
        DEFINE_KW("static_if", TOK_KW_STATIC_IF);
        DEFINE_KW("if", TOK_KW_IF);
        DEFINE_KW("else", TOK_KW_ELSE);
        DEFINE_KW("foreign", TOK_KW_FOREIGN);
        DEFINE_KW("dynamic_link", TOK_KW_DYN_LINK);
        DEFINE_KW("while", TOK_KW_WHILE);
        DEFINE_KW("for", TOK_KW_FOR);
        DEFINE_KW("array_length", TOK_KW_ARRAY_LENGTH);
        DEFINE_KW("static_assert", TOK_KW_STATIC_ASSERT);
        DEFINE_KW("import", TOK_KW_IMPORT);
        DEFINE_KW("struct", TOK_KW_STRUCT);
        DEFINE_KW("enum", TOK_KW_ENUM);
    }

#undef DEFINE_KW

    static void init_module_path(Context* context)
    {
        assert(context);

        const char* module_path = platform_get_environment_var("ZODIAC_MODULE_PATH");
        if (!module_path)
        {
            fprintf(stderr, "Zodiac module path not specified, exiting\n");
            assert(false);
        }

        printf("ZODIAC_MODULE_PATH: %s\n", module_path);
        context->module_search_path = atom_get(context->atom_table, module_path);
    }
}
