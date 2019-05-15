#include <stdio.h>

#include "common.h"

#include "zodiac.h"
#include "lexer.h"
#include "parser.h"
#include "resolver.h"

#include "ir.h"
#include "ir_runner.h"

using namespace Zodiac;

void usage(const char* exe_name)
{
    printf("Usage:\n");
    printf("\t%s FILE_PATH\n", exe_name);
}

int main(int argc, char** argv)
{
    Options options;
    // options.verbose = true;
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

	const char* file_name = context->options.main_file_name;

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

        ir_runner_execute(&ir_runner, parse_result.ast_module, &ir_module);
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
