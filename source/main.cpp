#include <stdio.h>

#include "common.h"

#include "zodiac.h"
#include "lexer.h"
#include "parser.h"
#include "resolver.h"

#include "stack_vm.h"
#include "stack_vm_generator.h"

using namespace Zodiac;

void usage(const char* exe_name)
{
    printf("Usage:\n");
    printf("\t%s FILE_PATH\n", exe_name);
}

int main(int argc, char** argv)
{
    Arena arena = arena_create(MB(2));
    Context _context;
    context_init(&_context, &arena);
    auto context = &_context;

    if (argc != 2)
    {
        usage(argv[0]);
        return -1;
    }

    const char* file_name = argv[1];

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

    // printf("Parsed file: %s\n", file_name);

    Resolver resolver;
    resolver_init(&resolver, context, parse_result.ast_module);

    Stack_VM_Generator generator;
    stack_vm_generator_init(&generator, context, parse_result.ast_module);

    while (!resolver.done || !generator.done)
    {
        resolver_do_cycle(&resolver);
        stack_vm_generator_do_cycle(&generator);

        if (!resolver.progressed_on_last_cycle &&
            !generator.progressed_on_last_cycle)
        {
            break;
        }

    }

    if (!resolver.done)
    {
        resolver_report_errors(&resolver);
        return -1;
    }

    fprintf(stderr, "Resolved file: %s\n", file_name);

    if (!generator.done)
    {
        fprintf(stderr, "Generator quitted with errors\n");
        return -1;
    }

    fprintf(stderr, "Generated file: %s\n", file_name);

    stack_vm_print_program(generator.result.instructions,
                           BUF_LENGTH(generator.result.instructions));

    Stack_VM vm;
    stack_vm_init(&vm, MB(8));
    stack_vm_execute_program(&vm, generator.result.instructions,
                             BUF_LENGTH(generator.result.instructions));

    return 0;
}
