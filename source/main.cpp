#include <stdio.h>

#include "common.h"

#include "builtin.h"
#include "zodiac.h"
#include "lexer.h"
#include "parser.h"
#include "resolver.h"
#include "new_resolver.h"

#include "ir.h"
#include "ir_runner.h"

using namespace Zodiac;
using namespace Zodiac_;

void usage(const char* exe_name)
{
    printf("Usage:\n");
    printf("\t%s FILE_PATH\n", exe_name);
}

int main(int argc, char** argv)
{
	//atom_test();

    Options options;
	if (!zodiac_parse_options(&options, argc, argv))
	{
		usage(argv[0]);
		return -1;
	}

    Arena arena = arena_create(MB(2));
    Context _context;
    if (!context_init(&_context, &arena, options))
    {
        return -1;
    }
    auto context = &_context;

	const char* file_name = normalize_path(context->options.main_file_name);

    if (!path_exists(file_name))
    {
        fprintf(stderr, "File does not exist: %s\n", file_name);
        return -1;
    }

    if (!file_exists(file_name))
    {
        fprintf(stderr, "Path is not a file: %s\n", file_name);
        return -1;
    }

    Atom std_module_name = atom_get(context->atom_table, "std");
    Atom std_module_path = {};
    bool std_found = zodiac_find_module_path(context, std_module_name, &std_module_path);
    assert(std_found);
    // printf("std_module_path: %s\n", std_module_path.data);
    AST_Module* std_ast_module = zodiac_compile_or_get_module(context, std_module_path, std_module_name);
    if (!std_ast_module)
    {
        fprintf(stderr, "Compilation for std module for builtins failed, aborting\n");
        return -1;
    }
    AST_Declaration* string_type_decl = ast_scope_find_declaration(context,
                                                                   std_ast_module->module_scope,
                                                                   Builtin::atom_String);
	assert(string_type_decl);
    assert(string_type_decl->kind == AST_DECL_AGGREGATE_TYPE);
    AST_Type* string_type = string_type_decl->aggregate_type.type;
    assert(string_type->kind == AST_TYPE_STRUCT);
    Builtin::type_String = string_type;

    AST_Declaration* string_length_decl = ast_scope_find_declaration(context,
                                                                     std_ast_module->module_scope,
                                                                     Builtin::atom_string_length);
    assert(string_length_decl);
    assert(string_length_decl->identifier);
    assert(string_length_decl->kind == AST_DECL_FUNC);
    Builtin::decl_string_length = string_length_decl;

    // AST_Declaration* type_info_type_decl = ast_scope_find_declaration(context,
    //                                                                   std_ast_module->module_scope,
    //                                                                   Builtin::atom_Type_Info);
    // assert(type_info_type_decl);
    // assert(type_info_type_decl->kind == AST_DECL_AGGREGATE_TYPE);
    // AST_Type* type_info_type = type_info_type_decl->aggregate_type.type;
    // assert(type_info_type->kind == AST_TYPE_STRUCT);
    // Builtin::type_Type_Info = type_info_type;

    // AST_Type* pointer_to_type_info = ast_find_or_create_pointer_type(context, type_info_type);
    // assert(pointer_to_type_info);
    // Builtin::type_pointer_to_Type_Info = pointer_to_type_info;

    const char* file_string = read_file_string(file_name);
    // fprintf(stderr, "File contents:\n%s\n", file_string);

    Lexer lexer;
    init_lexer(&lexer, context);

    Lex_Result lex_result = lex_file(&lexer, file_string, file_name);
    if (BUF_LENGTH(lex_result.errors) != 0)
    {
        lexer_report_errors(&lexer);

        // We might want to continue here, to try and parse what we have?
        return -1;
    }

    mem_free(file_string); 

    // for (uint64_t i = 0; i < BUF_LENGTH(lex_result.tokens); i++)
    // {
    //     Token t = lex_result.tokens[i];
    //     print_token(t);
    // }

    Parser parser;
    parser_init(&parser, context);

    Parse_Result parse_result = parse_module(&parser, lex_result.tokens, "auto_main");
    if (BUF_LENGTH(parse_result.errors) != 0)
    {
        parser_report_errors(&parser);

        // Again, do we want to continue here?
        return -1;
    }


    Resolver resolver;
    resolver_init(&resolver, context);
    Resolve_Result rr = resolver_resolve_module(&resolver, parse_result.ast_module);
    if (resolve_result_has_errors(&rr))
    {
        resolve_result_report_errors(&rr);
        fprintf(stderr, "Exitting with resolve error(s)\n");
        return -1;
    }

    if (context->options.verbose)
    {
        fprintf(stderr, "Resolved file: %s\n\n", file_name);
    }

    IR_Builder ir_builder;
    ir_builder_init(&ir_builder, context);

    IR_Module ir_module = ir_builder_emit_module(&ir_builder, parse_result.ast_module);

    if (ir_module.error_count)
    {
        fprintf(stderr, "Exiting with error(s)\n");
        return -1;
    }

    IR_Validation_Result validation = ir_validate(&ir_builder);

    if (context->options.print_ir)
    {
        ir_builder_print_result(&ir_builder);
    }

    if (!validation.messages)
    {
        IR_Runner ir_runner;
        ir_runner_init(context, &ir_runner);

		if (context->options.execute_ir)
		{
			ir_runner_execute_entry(&ir_runner, parse_result.ast_module, &ir_module);
		}
    }
    else
    {
        for (uint64_t i = 0; i < BUF_LENGTH(validation.messages); i++)
        {
            fprintf(stderr, "%s\n", validation.messages[i]);
        }
    }

    return 0;
}
