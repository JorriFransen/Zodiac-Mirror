#include "zodiac.h"

#include "builtin.h"
#include "lexer.h"
#include "parser.h"
#include "platform.h"
#include "resolver.h"
#include "ir.h"

namespace Zodiac
{
    bool context_init(Context* context, Arena* arena, Options options)
    {
        assert(context);
        assert(arena);

        context->options = options;
        context->arena = arena;

        context->atom_table = (Atom_Table*)mem_alloc(sizeof(Atom_Table));
        atom_table_init(context->atom_table);

        context->keywords =  nullptr;
        context->compiled_modules = nullptr;
        context->foreign_table = nullptr;

		context->builtin_scope = ast_scope_new(context, nullptr, nullptr, false);

        context->type_hash = (AST_Type**)mem_alloc(sizeof(AST_Type*) * 256);
        context->type_count = 256;
        context->type_info_data = {};

        context_init_keywords(context);
        init_builtin_types(context);
        init_builtin_decls(context);

        const char* main_file_dir = extract_directory_from_path(options.main_file_name);
        const char* main_file_abs_dir = full_path_from_cwd(main_file_dir);
        Atom main_file_abs_dir_atom = atom_get(context->atom_table, main_file_abs_dir);

        context->builtin_ast_module = nullptr;

        return init_module_search_path(context, main_file_abs_dir_atom);
    }

	bool zodiac_parse_options(Options* options, int argc, char** argv)
	{
		assert(options);
		assert(argc > 0);
		assert(argv);

#define BOOL_OPTION(long_name, short_name) \
    { OPTION_BOOL, #long_name, short_name, offsetof(Options, long_name) }

#define STRING_OPTION(long_name, short_name) \
        { OPTION_STRING, #long_name, short_name, offsetof(Options, long_name) }

        Option option_templates[] =
        {
            BOOL_OPTION(verbose, 'v'),
            BOOL_OPTION(print_ir, 'p'),
            BOOL_OPTION(execute_ir, 'r'),
            BOOL_OPTION(emit_llvm, 'e'),
            BOOL_OPTION(print_llvm, 'l'),
            STRING_OPTION(run_argument, 'a'),
        };

		const char* exe_name = argv[0];
		argv++;
		argc--;

		Option_Parse_Context opc;
		opc.options = options;
        opc.templates = option_templates;
        opc.template_count = STATIC_ARRAY_LENGTH(option_templates);
		opc.options->exe_name = exe_name;
		opc.arg_count = argc;
		opc.arg_index = 0;
		opc.args = argv;

		while (opc.arg_index < opc.arg_count)
		{
			if (!zodiac_parse_option_argument(&opc))
			{
				return false;
			}
		}

        if (!options->main_file_name)
        {
            return false;
        }

        BUF_PUSH(options->run_args, options->main_file_name);
        if (options->run_argument)
        {
            uint64_t start_idx = 0;
            uint64_t length = 0;

            for (uint64_t i = 0; i < strlen(options->run_argument); i++)
            {
                if (options->run_argument[i] == ' ')
                {
                    if (length > 0)
                    {
                        char* arg = (char*)mem_alloc(length + 1);
                        memcpy(arg, &options->run_argument[start_idx], length);
                        arg[length] = 0;
                        BUF_PUSH(options->run_args, arg);
                    }
                    start_idx = i + 1;
                    length = 0;
                }
                else
                {
                    length++;
                }
            }

            if (length > 0)
            {
                char* arg = (char*)mem_alloc(length + 1);
                memcpy(arg, &options->run_argument[start_idx], length);
                arg[length] = 0;
                BUF_PUSH(options->run_args, arg);
            }
        }

		return true;
	}

	bool zodiac_parse_option_argument(Option_Parse_Context* opc)
	{
		assert(opc);
		assert(opc->arg_index < opc->arg_count);

		char* arg = opc->args[opc->arg_index];
		auto arg_length = strlen(arg);

		bool result = true;

		if (arg_length > 1 && arg[0] == '-')
		{
			if (arg_length > 2 && arg[1] == '-')
			{
				const char* option_name = &arg[2];
                result = zodiac_match_long_option(opc, option_name);

                if (!result)
                {
					fprintf(stderr, "Unrecognized option: %s\n", option_name);
                }
			}
			else
			{
				for (auto i = 1; i < arg_length; i++)
				{
                    result = zodiac_match_short_option(opc, arg[i]);
					// switch (arg[i])
					// {
					// 	case 'v':
					// 	{
					// 		opc->options->verbose = true;
					// 		break;
					// 	}

					// 	default:
					// 	{
					// 		fprintf(stderr, "Unrecognized option: '%c'\n", arg[i]);
					// 		result = false;
					// 		break;
					// 	}
					// }

					if (!result)
					{
                        fprintf(stderr, "Unrecognized option: '%c'\n", arg[i]);
						break;
					}
				}
			}
		}
		else
		{
			assert(!opc->options->main_file_name);
			opc->options->main_file_name = arg;
		}

		opc->arg_index++;
		return result;
	}

    bool zodiac_match_long_option(Option_Parse_Context* opc, const char* option_name)
    {
        assert(opc);
        assert(option_name);

        for (uint64_t i = 0; i < opc->template_count; i++)
        {
            const Option& ot = opc->templates[i];
            if (ot.long_name && strcmp(option_name, ot.long_name) == 0)
            {
                switch (ot.kind)
                {
                    case OPTION_BOOL:
                    {
                        *((bool*)((uint8_t*)opc->options + ot.option_offset)) = true;
                        return true;
                        break;
                    }

                    case OPTION_STRING:
                    {
                        opc->arg_index++;
                        *(char**)((uint8_t*)opc->options + ot.option_offset) =
                            opc->args[opc->arg_index];
                        return true;
                        break;
                    }

                    default: assert(false);
                }
            }
        }

        return false;
    }

    bool zodiac_match_short_option(Option_Parse_Context* opc, char c)
    {
        for (uint64_t i = 0; i < opc->template_count; i++)
        {
            const Option& ot = opc->templates[i];
            if (ot.short_name && c == ot.short_name)
            {
                switch (ot.kind)
                {
                    case OPTION_BOOL:
                    {
                        *((bool*)((uint8_t*)opc->options + ot.option_offset)) = true;
                        return true;
                        break;
                    }

                    case OPTION_STRING:
                    {
                        opc->arg_index++;
                        *(char**)((uint8_t*)opc->options + ot.option_offset) =
                            opc->args[opc->arg_index];
                        return true;
                        break;
                    }

                    default: assert(false);
                }
            }
        }

        return false;
    }

    AST_Module* zodiac_compile_or_get_module(Context* context, const Atom& module_path,
		    const Atom& module_name)
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

        for (uint64_t i = 0; i < BUF_LENGTH(context->modules_with_errors); i++)
        {
            const Compiled_Module& cm = context->modules_with_errors[i];
            if (cm.module_path == module_path && cm.module_name == module_name)
            {
                return nullptr;
            }
        }

        AST_Module* module = zodiac_compile_module(context, module_path, module_name);
        if (!module)
        {
            Compiled_Module cm = { module_name, module_path, nullptr };
            BUF_PUSH(context->modules_with_errors, cm);
            return nullptr;
        }

		Compiled_Module cm;
		cm.module = module;
		cm.module_path = module_path;
		cm.module_name = module_name;
		BUF_PUSH(context->compiled_modules, cm);

		return module;
    }

    AST_Module* zodiac_compile_module(Context* context, const Atom& module_path,
                                      const Atom& module_name)
    {
        assert(context);

        const char* module_string = read_file_string(module_path.data);
        assert(module_string);

        Lexer lexer;
        init_lexer(&lexer, context);

        Lex_Result lex_result = lex_file(&lexer, module_string, module_path.data);
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
        resolver_init(&resolver, context);
        Resolve_Result rr = resolver_resolve_module(&resolver, parse_result.ast_module);
        if (resolve_result_has_errors(&rr))
        {
            resolve_result_report_errors(&rr);
            return nullptr;
        }
        // resolver_init(&resolver, context, parse_result.ast_module);

        // while (!resolver.done)
        // {
        //     resolver_do_cycle(&resolver);

        //     if (!resolver.progressed_on_last_cycle)
        //     {
        //         break;
        //     }

        //     if (resolver.override_done)
        //     {
        //         break;
        //     }

		// 	if (resolver.import_error)
		// 	{
		// 		break;
		// 	}
        // }

        // if (!resolver.done)
        // {
        //     resolver_report_errors(&resolver);
        //     return nullptr;
        // }

        if (context->options.verbose)
        {
            printf("Resolved module: %s\n", module_name.data);
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
		if (ir_builder->context->options.print_ir)
		{
			ir_builder_print_result(ir_builder);
		}

        if (validation.messages)
        {
            for (uint64_t i = 0; i < BUF_LENGTH(validation.messages); i++)
            {
                fprintf(stderr, "%s\n", validation.messages[i]);
            }
            return nullptr;
        }

        return parse_result.ast_module;
    }

    bool zodiac_find_module_path(Context* context, const Atom& module_name, Atom* module_path_dest)
    {
        assert(context);
        assert(module_path_dest);

        Atom module_file_name = atom_append(context->atom_table, module_name, ".zdc");

        bool found = false;

        for (uint64_t i = 0; i < BUF_LENGTH(context->module_search_path); i++)
        {
            Atom module_search_path = context->module_search_path[i];
            Atom module_path = atom_append(context->atom_table, module_search_path,
                                           module_file_name);

            if (!file_exists(module_path.data))
            {
                continue;
            }
            else
            {
                found = true;
                *module_path_dest = module_path;
                break;
            }
        }

        return found;
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
        DEFINE_KW("null", TOK_KW_NULL);
        DEFINE_KW("return", TOK_KW_RETURN);
        DEFINE_KW("static_if", TOK_KW_STATIC_IF);
        DEFINE_KW("if", TOK_KW_IF);
        DEFINE_KW("else", TOK_KW_ELSE);
        DEFINE_KW("foreign", TOK_KW_FOREIGN);
        DEFINE_KW("dynamic_link", TOK_KW_DYN_LINK);
        DEFINE_KW("while", TOK_KW_WHILE);
        DEFINE_KW("for", TOK_KW_FOR);
        DEFINE_KW("array_length", TOK_KW_ARRAY_LENGTH);
        DEFINE_KW("assert", TOK_KW_ASSERT);
        DEFINE_KW("__assert_fail", TOK_KW___ASSERT_FAIL);
        DEFINE_KW("static_assert", TOK_KW_STATIC_ASSERT);
        DEFINE_KW("import", TOK_KW_IMPORT);
        DEFINE_KW("struct", TOK_KW_STRUCT);
        DEFINE_KW("union", TOK_KW_UNION);
        DEFINE_KW("enum", TOK_KW_ENUM);
        DEFINE_KW("switch", TOK_KW_SWITCH);
        DEFINE_KW("case", TOK_KW_CASE);
        DEFINE_KW("default", TOK_KW_DEFAULT);
        DEFINE_KW("break", TOK_KW_BREAK);
		DEFINE_KW("typedef", TOK_KW_TYPEDEF);
        DEFINE_KW("using", TOK_KW_USING);
        DEFINE_KW("sizeof", TOK_KW_SIZEOF);
        DEFINE_KW("get_type_info", TOK_KW_GET_TYPE_INFO);
        DEFINE_KW("typeof", TOK_KW_TYPEOF);
        DEFINE_KW("defer", TOK_KW_DEFER);
    }

#undef DEFINE_KW

    static bool init_module_search_path(Context* context, Atom first_file_path)
    {
        assert(context);

        const char* module_path = platform_get_environment_var("ZODIAC_MODULE_PATH");
        if (!module_path)
        {
            fprintf(stderr, "Zodiac module path not specified, exiting\n");
	    return false;
        }

        if (context->options.verbose)
        {
            printf("ZODIAC_MODULE_PATH: %s\n", module_path);
        }

        BUF_PUSH(context->module_search_path, atom_get(context->atom_table, module_path));

        assert(dir_exists(first_file_path.data));
        BUF_PUSH(context->module_search_path, first_file_path);

        return true;
    }
}
