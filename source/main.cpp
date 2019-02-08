#include <stdio.h>

#include "common.h"

#include "zodiac.h"
#include "lexer.h"
#include "parser.h"

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

    if (!file_exists(file_name))
    {
        fprintf(stderr, "Error reading file: %s\n", file_name);
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

    printf("Parsed file: %s\n", file_name);

    return 0;
}
