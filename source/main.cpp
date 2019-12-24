#include <stdio.h>

#include "common.h"

#include "builtin.h"
#include "zodiac.h"
#include "lexer.h"
#include "parser.h"
#include "resolver.h"

#include "ir.h"
#include "ir_runner.h"

#include "llvm.h"

using namespace Zodiac;

void usage(const char* exe_name)
{
    printf("Usage:\n");
    printf("\t%s FILE_PATH\n", exe_name);
}

uint64_t _get_int_hash(const int& x)
{
    return x;
}

// #include <syscall.h>
// #include <sys/stat.h>
// #include <stdio.h>
// #include <unistd.h>

int main(int argc, char** argv)
{
    // printf("S_IFREG: %d\n", S_IFREG);
    // printf("SYS_lstat: %d\n", SYS_lstat);
    // printf("SEEK_SET: %d\n", SEEK_SET);
    // printf("SEEK_CUR: %d\n", SEEK_CUR);
    // printf("SEEK_END: %d\n", SEEK_END);
    // printf("EOF: %d\n", EOF);
	//printf("sizeof(DWORD): %d\n", sizeof(DWORD));
	//GetStdHandle(STD_OUTPUT_HANDLE);
    // printf("STDIN_FILENO: %d\n", STDIN_FILENO);
    // printf("STDOUT_FILENO: %d\n", STDOUT_FILENO);
    // printf("STDERR_FILENO: %d\n", STDERR_FILENO);



    Options options;
	if (!zodiac_parse_options(&options, argc, argv))
	{
		usage(argv[0]);
		return -1;
	}

    if (options.print_llvm && !options.emit_llvm)
    {
        fprintf(stderr, "Invalid option: 'print_llvm' requires 'emit_llvm'\n");
    }


    Arena arena = arena_create(MB(2));
    Context _context;
    if (!context_init(&_context, &arena, options))
    {
        return -1;
    }
    auto context = &_context;

	const char* file_name = normalize_path(context->options.main_file_name);
	const char* module_name = extract_file_name_from_path(file_name);

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

    Atom builtin_module_name = atom_get(context->atom_table, "builtin");
    Atom builtin_module_path = {};
    bool builtin_found = zodiac_find_module_path(context, builtin_module_name,
                                                 &builtin_module_path);
    assert(builtin_found);
    AST_Module* builtin_ast_module = zodiac_compile_or_get_module(context, builtin_module_path,
                                                                  builtin_module_name, true);
    if (!builtin_ast_module) return 42;

    assert(builtin_ast_module);
    context->builtin_ast_module = builtin_ast_module;
    context->builtin_ir_builder = ((IR_Builder*)builtin_ast_module->gen_data);
    AST_Declaration* default_assert_handler_decl = ast_scope_find_declaration(
        context, builtin_ast_module->module_scope, Builtin::atom_default_assert_handler);
    assert(default_assert_handler_decl);
    Builtin::decl_default_assert_handler = default_assert_handler_decl;

    AST_Declaration* string_type_decl = ast_scope_find_declaration(context,
                                                                builtin_ast_module->module_scope,
                                                                Builtin::atom_String);
    assert(string_type_decl);
    assert(string_type_decl->kind == AST_DECL_AGGREGATE_TYPE);
    AST_Type* string_type = string_type_decl->aggregate_type.type;
    assert(string_type->kind == AST_TYPE_STRUCT);
    Builtin::type_String = string_type;

    AST_Declaration* string_length_decl =
        ast_scope_find_declaration(context, builtin_ast_module->module_scope,
                                    Builtin::atom_string_length);
    assert(string_length_decl);
    assert(string_length_decl->identifier);
    assert(string_length_decl->kind == AST_DECL_FUNC);
    Builtin::decl_string_length = string_length_decl;
    Builtin::decl_string_length->identifier->declaration = Builtin::decl_string_length;

    AST_Declaration* type_info_type_decl =
        ast_scope_find_declaration(context, builtin_ast_module->module_scope,
                                    Builtin::atom_Type_Info);
    assert(type_info_type_decl);
    assert(type_info_type_decl->kind == AST_DECL_AGGREGATE_TYPE);
    AST_Type* type_info_type = type_info_type_decl->aggregate_type.type;
    assert(type_info_type->kind == AST_TYPE_STRUCT);
    Builtin::type_Type_Info = type_info_type;

    AST_Type* pointer_to_type_info = ast_find_or_create_pointer_type(context, type_info_type);
    assert(pointer_to_type_info);
    Builtin::type_pointer_to_Type_Info = pointer_to_type_info;

    AST_Declaration* type_info_kind_decl =
        ast_scope_find_declaration(context, builtin_ast_module->module_scope,
                                    Builtin::atom_Type_Info_Kind);
    assert(type_info_kind_decl);
    Builtin::type_Type_Info_Kind = type_info_kind_decl->aggregate_type.type;

    AST_Declaration* type_info_flag_decl =
        ast_scope_find_declaration(context, builtin_ast_module->module_scope,
                                   Builtin::atom_Type_Info_Flags);
    assert(type_info_flag_decl);
    Builtin::type_Type_Info_Flags = type_info_flag_decl->aggregate_type.type;

    AST_Declaration* type_info_aggregate_member_type_decl =
        ast_scope_find_declaration(context, builtin_ast_module->module_scope,
                                    Builtin::atom_Type_Info_Aggregate_Member);
    assert(type_info_aggregate_member_type_decl);
    AST_Type* type_info_aggregate_member_type =
        type_info_aggregate_member_type_decl->aggregate_type.type;
    Builtin::type_Type_Info_Aggregate_Member = type_info_aggregate_member_type;

    AST_Declaration* type_info_enum_member_type_decl =
        ast_scope_find_declaration(context, builtin_ast_module->module_scope,
                                    Builtin::atom_Type_Info_Enum_Member);
    assert(type_info_enum_member_type_decl);
    AST_Type* type_info_enum_member_type =
        type_info_enum_member_type_decl->aggregate_type.type;
    Builtin::type_Type_Info_Enum_Member = type_info_enum_member_type;

    AST_Declaration* any_decl = ast_scope_find_declaration(context,
                                                           builtin_ast_module->module_scope,
                                                           Builtin::atom_Any);
    assert(any_decl);
    Builtin::type_Any = any_decl->aggregate_type.type;
    assert(Builtin::type_Any);
    Builtin::type_pointer_to_Any = ast_find_or_create_pointer_type(context, Builtin::type_Any);

    AST_Declaration* array_ref_of_any_decl =
        ast_scope_find_declaration(context, builtin_ast_module->module_scope,
                                   Builtin::atom_Array_Ref_of_Any);
    assert(array_ref_of_any_decl);
    assert(array_ref_of_any_decl->kind == AST_DECL_TYPEDEF);
    assert(array_ref_of_any_decl->typedef_decl.type);
    Builtin::type_Array_Ref_of_Any = array_ref_of_any_decl->typedef_decl.type;

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

    Parse_Result parse_result = parse_module(&parser, lex_result.tokens, module_name, file_name);
    if (BUF_LENGTH(parse_result.errors) != 0)
    {
        parser_report_errors(&parser);

        // Again, do we want to continue here?
        return -1;
    }


    Resolver resolver;
    resolver_init(&resolver, context, false);
    Resolve_Result rr = resolver_resolve_module(&resolver, parse_result.ast_module, false);
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

    uint64_t ir_return_value = 0;

    if (!validation.messages)
    {
        IR_Runner ir_runner;
        ir_runner_init(context, &ir_runner);

		if (context->options.execute_ir)
		{
            assert(!context->options.emit_llvm);
			ir_return_value = ir_runner_execute_entry(&ir_runner, parse_result.ast_module,
                                                      &ir_module);

            if (ir_runner.asserted)
            {
                ir_return_value = 7;
            }

        }
    }
    else
    {
        for (uint64_t i = 0; i < BUF_LENGTH(validation.messages); i++)
        {
            fprintf(stderr, "%s\n", validation.messages[i]);
        }
        return -1;
    }

    if (context->options.emit_llvm)
    {
        LLVM_IR_Builder llvm_ir_builder = {};
        llvm_builder_init(&llvm_ir_builder);
        llvm_ir_builder.context = context;
        llvm_emit_ir_module(&llvm_ir_builder, &ir_module);

        llvm_builder_free(&llvm_ir_builder);
    }

    return (int)ir_return_value;
}
