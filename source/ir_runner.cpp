#include "ir_runner.h"

#include "builtin.h"

#include <inttypes.h>

namespace Zodiac
{
    void ir_runner_init(Context* context, IR_Runner* ir_runner)
    {
        assert(context);
        assert(ir_runner);

        ir_runner->context = context;
        ir_runner->arena = arena_create(MB(4));

        stack_init(&ir_runner->call_stack, 8);
        stack_init(&ir_runner->arg_stack, 8);
        ir_runner->jump_block = nullptr;
        ir_runner->returned = false;

        ir_runner->dyn_vm = dcNewCallVM(MB(4));
        dcMode(ir_runner->dyn_vm, DC_CALL_C_DEFAULT);
        dcReset(ir_runner->dyn_vm);

        ir_runner->loaded_dyn_libs = nullptr;
        ir_runner->loaded_foreign_symbols = nullptr;
    }

    void ir_runner_execute(IR_Runner* ir_runner, AST_Module* ast_module, IR_Module* ir_module)
    {
        assert(ir_runner);
        assert(ast_module);
        assert(ir_module);

        assert(ir_module->entry_function);

        ir_runner_load_dynamic_libs(ir_runner, ast_module, ir_module);
        ir_runner_load_foreigns(ir_runner, ir_module);

        ir_runner_execute_block(ir_runner, ir_runner->context->global_init_block->block);

        IR_Value return_value = {};
        IR_Stack_Frame* entry_stack_frame = ir_runner_call_function(ir_runner,
                                                                    ir_module->entry_function, 0,
                                                                    &return_value);

        printf("Entry point returned: %" PRId64 "\n", entry_stack_frame->return_value->value.s64);
        uint64_t arena_cap = 0;
        auto block = ir_runner->arena.blocks;
        while (block)
        {
            arena_cap += block->data_length * sizeof(void*);
            block = block->next_block;
        }
        printf("Arena size: %.2fMB\n", (double)arena_cap / MB(1));
    }

    void ir_runner_load_dynamic_libs(IR_Runner* ir_runner, AST_Module* ast_module,
                                     IR_Module* ir_module)
    {
        assert(ir_runner);
        assert(ir_module);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_module->dynamic_lib_names); i++)
        {
            ir_runner_load_dynamic_lib(ir_runner, ir_module->dynamic_lib_names[i]);
        }

        for (uint64_t i = 0; i < BUF_LENGTH(ast_module->import_modules); i++)
        {
            AST_Module* import_ast_module = ast_module->import_modules[i];
            assert(import_ast_module->gen_data);
            IR_Builder* import_ir_builder = (IR_Builder*)import_ast_module->gen_data;
            IR_Module* import_ir_module = &import_ir_builder->result;

            for (uint64_t ni = 0; ni < BUF_LENGTH(import_ir_module->dynamic_lib_names); ni++)
            {
                ir_runner_load_dynamic_lib(ir_runner, import_ir_module->dynamic_lib_names[ni]);
            }
        }
    }

    void ir_runner_load_dynamic_lib(IR_Runner* ir_runner, Atom lib_name)
    {
        assert(ir_runner);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_runner->loaded_dyn_libs); i++)
        {
            const IR_Loaded_Dynamic_Lib& loaded_lib = ir_runner->loaded_dyn_libs[i];
            if (loaded_lib.name == lib_name)
            {
                return;
            }
        }

        const char* lib_path = lib_name.data;

        auto context = ir_runner->context;
        bool found = false;
        for (uint64_t i = 0; i < BUF_LENGTH(context->module_search_path); i++)
        {
            Atom search_path = context->module_search_path[i];
            auto import_atom = atom_append(context->atom_table, search_path, lib_name);
            if (file_exists(import_atom.data))
            {
                lib_path = import_atom.data;
            }

            assert(lib_path);
            DLLib* lib = dlLoadLibrary(lib_path);
            if (!lib)
            {
                continue;
            }
            else
            {
                found = true;
            }

            IR_Loaded_Dynamic_Lib loaded_lib = { lib_name, lib };
            BUF_PUSH(ir_runner->loaded_dyn_libs, loaded_lib);
            // printf("Loaded dynamic library: %s\n", lib_name.data);
        }

        if (!found)
        {
            fprintf(stderr, "could not find library: %s\n", lib_name.data);
        }
    }

    void ir_runner_load_foreigns(IR_Runner* ir_runner, IR_Module* ir_module)
    {
        assert(ir_runner);
        assert(ir_module);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_runner->context->foreign_table); i++)
        {
            const Atom& foreign_name = ir_runner->context->foreign_table[i];

            bool found = false;

            for (uint64_t j = 0; j < BUF_LENGTH(ir_runner->loaded_dyn_libs); j++)
            {
                const IR_Loaded_Dynamic_Lib& loaded_lib = ir_runner->loaded_dyn_libs[j];

                // printf("Trying to load foreign \"%s\" from library \"%s\"\n",
                //    foreign_name.data, loaded_lib.name.data);

                void* foreign_symbol = dlFindSymbol(loaded_lib.lib, foreign_name.data);
                if (foreign_symbol)
                {
                    // printf("Loaded foreign \"%s\" from library \"%s\"\n",
                    //     foreign_name.data, loaded_lib.name.data);
                    BUF_PUSH(ir_runner->loaded_foreign_symbols, foreign_symbol);
                    found = true;
                    break;
                }
            }

            if (!found)
            {
                fprintf(stderr, "Failed to load foreign symbol: %s\n", foreign_name.data);
                assert(false);
            }
        }
    }

    IR_Value* ir_runner_get_local_temporary(IR_Runner* ir_runner, uint64_t temp_index)
    {
        assert(ir_runner);
        assert(stack_count(ir_runner->call_stack));

        IR_Stack_Frame* stack_frame = stack_top(ir_runner->call_stack);
        assert(stack_frame);
        auto buf_length = BUF_LENGTH(stack_frame->temps);
        assert(BUF_LENGTH(stack_frame->temps) > temp_index);

        return &stack_frame->temps[temp_index];
    }

    IR_Value* ir_runner_get_local_temporary(IR_Runner* ir_runner, IR_Value* code_value)
    {
        assert(ir_runner);
        assert(code_value);

        IR_Value* result = nullptr;

        switch (code_value->kind)
        {
            case IRV_ARGUMENT:
            {
                auto stack_frame = ir_runner_top_stack_frame(ir_runner);
                assert(BUF_LENGTH(stack_frame->args) > code_value->argument.index);
                result = &stack_frame->args[code_value->argument.index];
                break;
            }

            case IRV_TEMPORARY:
            {
                result = ir_runner_get_local_temporary(ir_runner, code_value->temp.index);
                break;
            }

            case IRV_ALLOCL:
            {
                result = ir_runner_get_local_temporary(ir_runner, code_value->allocl.index);
                break;
            }

            case IRV_GLOBAL:
            {
                assert(BUF_LENGTH(ir_runner->context->global_table) > code_value->global.index);
                result = ir_runner->context->global_table[code_value->global.index].value;
                break;
            }

            case IRV_INT_LITERAL:
            case IRV_STRING_LITERAL:
            case IRV_CHAR_LITERAL:
            case IRV_FLOAT_LITERAL:
            case IRV_BOOL_LITERAL:
			case IRV_NULL_LITERAL:
            {
                result = code_value;
                break;
            }

            default: assert(false);
        }

        assert(result);
        return result;
    }

    IR_Stack_Frame* ir_runner_call_function(IR_Runner* runner, IR_Function* function,
                                            uint64_t num_args, IR_Value* return_value)
    {
        assert(runner);
        assert(function);

        BUF(IR_Value) args = nullptr;
        assert(stack_count(runner->arg_stack) >= num_args);

        for (uint64_t i = 0; i < num_args; i++)
        {
            IR_Value arg = stack_peek(runner->arg_stack, (num_args - 1) - i);
            BUF_PUSH(args, arg);
        }
        for (uint64_t i = 0; i < num_args; i++)
        {
            stack_pop(runner->arg_stack);
        }

        IR_Stack_Frame* stack_frame = ir_runner_push_stack_frame(runner, function, args,
                                                                 return_value);

        auto block = function->first_block;
        while (block && ir_runner_top_stack_frame(runner) == stack_frame)
        {
            ir_runner_execute_block(runner, block);
            if (runner->jump_block)
            {
                block = runner->jump_block;
                runner->jump_block = nullptr;
            }
            else
            {
                block = block->next;
            }
        }

        BUF_FREE(args);

        return stack_frame;
    }

    void ir_runner_execute_block(IR_Runner* runner, IR_Block* block)
    {
        assert(runner);
        assert(block);

        auto iri = block->first_instruction;
        while (iri)
        {
            ir_runner_execute_instruction(runner, iri);

            if (runner->jump_block)
            {
                break;
            }

            if (runner->returned)
            {
                runner->returned = false;
                break;
            }

            iri = iri->next;
        }
    }

    char dcb_handler(DCCallback* cb, DCArgs* args, DCValue* result, void* userdata)
    {
        assert(userdata);

        _IR_DCB_Data* dcb_data = (_IR_DCB_Data*)userdata;
        IR_Value* func_value = dcb_data->func_value;
        assert(func_value->kind == IRV_FUNCTION);


        auto arg_types = func_value->function->type->function.arg_types;
        for (uint64_t i = 0; i < BUF_LENGTH(arg_types); i++)
        {
            AST_Type* arg_type = arg_types[i];

            IR_Value arg_value = {};
            arg_value.kind = IRV_TEMPORARY;
            arg_value.type = arg_type;
            arg_value.assigned = true;

            if (arg_type->kind == AST_TYPE_POINTER)
            {
                arg_value.value.string = (uint8_t*)dcbArgPointer(args);
            }
            else if (arg_type->flags & AST_TYPE_FLAG_INT)
            {
                if (arg_type->flags & AST_TYPE_FLAG_SIGNED)
                {
                    if (arg_type->bit_size == 64)
                    {
                        arg_value.value.u64 = dcbArgLongLong(args);
                    }
                    else assert(false);
                }
                else assert(false);
            }
            else if (arg_type->flags & AST_TYPE_FLAG_FLOAT)
            {
                assert(false);
            }
            else assert(false);

            stack_push(dcb_data->runner->arg_stack, arg_value);
        }

        AST_Type* return_type = func_value->function->type->function.return_type;
        assert(!(return_type->kind == AST_TYPE_STRUCT));
        assert(!(return_type->kind == AST_TYPE_STATIC_ARRAY));

        IR_Value return_value = {};
        IR_Stack_Frame* stack_frame = ir_runner_call_function(dcb_data->runner,
                                                              func_value->function,
                                                              BUF_LENGTH(arg_types),
                                                              &return_value);

        if (return_type->flags & AST_TYPE_FLAG_INT)
        {
            assert(false);
        }
        else if (return_type->flags & AST_TYPE_FLAG_VOID)
        {
            return 'v';
        }
        else assert(false);

		assert(false);
		return 0;
    }

    void ir_runner_execute_instruction(IR_Runner* runner, IR_Instruction* iri)
    {
        assert(runner);
        assert(iri);

        switch (iri->op)
        {
            case IR_OP_NOP:
            {
                assert(false);
                break;
            }

            case IR_OP_ADD:
            {
                assert(iri->arg1);
                assert(iri->arg2);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                AST_Type* type = arg1->type;

                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    dest->value.s64 = arg1->value.s64 + arg2->value.s64;
                }
                else if (type == Builtin::type_double)
                {
                    dest->value.r64 = arg1->value.r64 + arg2->value.r64;
                }
                else if (type == Builtin::type_float)
                {
                    dest->value.r32 = arg1->value.r32 + arg2->value.r32;
                }
                else assert(false);

                break;
            }

            case IR_OP_SUB:
            {
                assert(iri->arg1);
                assert(iri->arg2);
                assert(iri->result);

                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                AST_Type* type = arg1->type;

                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    dest->value.s64 = arg1->value.s64 - arg2->value.s64;
                }
                else if (type == Builtin::type_float)
                {
                    dest->value.r32 = arg1->value.r32 - arg2->value.r32;
                }
                else if (type == Builtin::type_double)
                {
                    dest->value.r64 = arg1->value.r64 - arg2->value.r64;
                }
                else assert(false);

                break;
            }

            case IR_OP_MUL:
            {
                assert(iri->arg1);
                assert(iri->arg2);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                AST_Type* type = arg1->type;

                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    dest->value.s64 = arg1->value.s64 * arg2->value.s64;
                }
                else if (type == Builtin::type_double)
                {
                    dest->value.r64 = arg1->value.r64 * arg2->value.r64;
                }
                else if (type == Builtin::type_float)
                {
                    dest->value.r32 = arg1->value.r32 * arg2->value.r32;
                }
                else assert(false);

                break;
            }

            case IR_OP_MOD:
            {
                assert(iri->arg1);
                assert(iri->arg2);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                AST_Type* type = arg1->type;

                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    dest->value.s64 = arg1->value.s64 % arg2->value.s64;
                }
                else assert(false);

                break;
            }

            case IR_OP_DIV:
            {
                assert(iri->arg1);
                assert(iri->arg2);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                AST_Type* type = arg1->type;

                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    dest->value.s64 = arg1->value.s64 / arg2->value.s64;
                }
                else if (type == Builtin::type_double)
                {
                    dest->value.r64 = arg1->value.r64 / arg2->value.r64;
                }
                else if (type == Builtin::type_float)
                {
                    dest->value.r32 = arg1->value.r32 / arg2->value.r32;
                }
                else assert(false);

                break;
            }

            case IR_OP_LT:
            {
                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                AST_Type* type = arg1->type;

                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    dest->value.s64 = arg1->value.s64 < arg2->value.s64;
                }
                else if (type == Builtin::type_double)
                {
                    dest->value.s64 = arg1->value.r64 < arg2->value.r64;
                }
                else if (type == Builtin::type_float)
                {
                    dest->value.s64 = arg1->value.r32 < arg2->value.r32;
                }
                else assert(false);
                break;
            }

            case IR_OP_LTEQ:
            {
                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                AST_Type* type = arg1->type;

                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    dest->value.s64 = arg1->value.s64 <= arg2->value.s64;
                }
                else if (type == Builtin::type_double)
                {
                    dest->value.s64 = arg1->value.r64 <= arg2->value.r64;
                }
                else if (type == Builtin::type_float)
                {
                    dest->value.s64 = arg1->value.r32 <= arg2->value.r32;
                }
                else assert(false);
                break;
            }

            case IR_OP_GT:
            {
                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                AST_Type* type = arg1->type;

                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    dest->value.s64 = arg1->value.s64 > arg2->value.s64;
                }
                else if (type == Builtin::type_double)
                {
                    dest->value.s64 = arg1->value.r64 > arg2->value.r64;
                }
                else if (type == Builtin::type_float)
                {
                    dest->value.s64 = arg1->value.r32 > arg2->value.r32;
                }
                else assert(false);
                break;
            }

            case IR_OP_GTEQ:
            {
                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                AST_Type* type = arg1->type;

                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    dest->value.s64 = arg1->value.s64 >= arg2->value.s64;
                }
                else if (type == Builtin::type_double)
                {
                    dest->value.s64 = arg1->value.r64 >= arg2->value.r64;
                }
                else if (type == Builtin::type_float)
                {
                    dest->value.s64 = arg1->value.r32 >= arg2->value.r32;
                }
                else assert(false);
                break;
            }

            case IR_OP_EQ:
            {
                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                AST_Type* type = arg1->type;

                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    dest->value.s64 = arg1->value.s64 == arg2->value.s64;
                }
                else if (type == Builtin::type_double)
                {
                    dest->value.s64 = arg1->value.r64 == arg2->value.r64;
                }
                else if (type->kind == AST_TYPE_ENUM)
                {
                    dest->value.s64 = arg1->value.s64 == arg2->value.s64;
                }
                else assert(false);
                break;
            }

            case IR_OP_NEQ:
            {
                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                AST_Type* type = arg1->type;

                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    dest->value.s64 = arg1->value.s64 != arg2->value.s64;
                }
                else if (type == Builtin::type_double)
                {
                    dest->value.s64 = arg1->value.r64 != arg2->value.r64;
                }
                else if (type == Builtin::type_float)
                {
                    dest->value.s64 = arg1->value.r32 != arg2->value.r32;
                }
                else if (type->kind == AST_TYPE_ENUM)
                {
                    dest->value.s64 = arg1->value.s64 != arg2->value.s64;
                }
                else if (type == Builtin::type_bool)
                {
                    assert(false);
                }
                else assert(false);
                break;
            }

            case IR_OP_AND_AND:
            {
                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                AST_Type* type = arg1->type;

                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    dest->value.s64 = arg1->value.s64 && arg2->value.s64;
                }
                else if (type == Builtin::type_double)
                {
                    dest->value.s64 = arg1->value.r64 && arg2->value.r64;
                }
                else if (type->kind == AST_TYPE_ENUM)
                {
                    dest->value.s64 = arg1->value.s64 && arg2->value.s64;
                }
                else assert(false);
                break;
            }

            case IR_OP_OR_OR:
            {
                IR_Value* arg1 = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* arg2 = ir_runner_get_local_temporary(runner, iri->arg2);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                AST_Type* type = arg1->type;

                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    dest->value.s64 = arg1->value.s64 || arg2->value.s64;
                }
                else if (type == Builtin::type_double)
                {
                    dest->value.s64 = arg1->value.r64 || arg2->value.r64;
                }
                else if (type->kind == AST_TYPE_ENUM)
                {
                    dest->value.s64 = arg1->value.s64 || arg2->value.s64;
                }
                else assert(false);
                break;
            }

            case IR_OP_PUSH_CALL_ARG:
            {
                assert(iri->arg1);

                IR_Value* value = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value arg_value = *value;
                stack_push(runner->arg_stack, arg_value);
                break;
            }

            case IR_OP_CALL:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_FUNCTION);
                assert(iri->arg2);
                assert(iri->arg2->kind == IRV_INT_LITERAL);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Function* function = iri->arg1->function;
                auto num_args = iri->arg2->value.s64;
                IR_Value* result_value = ir_runner_get_local_temporary(runner, iri->result);
                IR_Stack_Frame* callee_stack_frame = ir_runner_call_function(runner, function,
                                                                             num_args,
                                                                             result_value);


                break;
            }

            case IR_OP_PUSH_EX_CALL_ARG:
            {
                IR_Value* arg_value = ir_runner_get_local_temporary(runner, iri->arg1);
                bool is_vararg = false;
                if (iri->arg2)
                {
                    IR_Value* is_vararg_value = ir_runner_get_local_temporary(runner, iri->arg2);
                    assert(is_vararg_value->type == Builtin::type_bool);
                    assert(is_vararg_value->kind == IRV_BOOL_LITERAL);
                    is_vararg = is_vararg_value->value.boolean;
                }

                if (iri->arg1->type->flags & AST_TYPE_FLAG_FLOAT)
                {
                    if (is_vararg)
                    {
                        if (iri->arg1->type->bit_size == 64)
                        {
                            dcArgDouble(runner->dyn_vm, arg_value->value.r64);
                        }
                        else if (iri->arg1->type->bit_size == 32)
                        {
                            dcArgDouble(runner->dyn_vm, (double)arg_value->value.r32);
                        }
                        else assert(false);
                    }
                    else
                    {
                        if (iri->arg1->type->bit_size == 64)
                        {
                            dcArgDouble(runner->dyn_vm, arg_value->value.r64);
                        }
                        else if (iri->arg1->type->bit_size == 32)
                        {
                            dcArgFloat(runner->dyn_vm, arg_value->value.r32);
                        }
                        else assert(false);
                    }
                }
                else if (iri->arg1->type->kind == AST_TYPE_POINTER)
                {
                    dcArgPointer(runner->dyn_vm, arg_value->value.string);
                }
                else if (iri->arg1->type->flags & AST_TYPE_FLAG_INT ||
                         iri->arg1->type->kind == AST_TYPE_ENUM)
                {
                    if (iri->arg1->type->bit_size == 8)
                    {
                        dcArgChar(runner->dyn_vm, arg_value->value.u8);
                    }
                    else if (iri->arg1->type->bit_size == 16)
                    {
                        dcArgShort(runner->dyn_vm, arg_value->value.u16);
                    }
                    else if (iri->arg1->type->bit_size == 32)
                    {
                        dcArgInt(runner->dyn_vm, arg_value->value.u32);
                    }
                    else if (iri->arg1->type->bit_size == 64)
                    {
                        dcArgLongLong(runner->dyn_vm, arg_value->value.u64);
                    }
                    else assert(false);
                }
                else assert(false);
                break;
            }

            case IR_OP_CALL_EX:
            {
                IR_Value* func = iri->arg1;
                assert(func->kind == IRV_FUNCTION);
                uint64_t foreign_index = func->function->foreign_index;
                assert(BUF_LENGTH(runner->loaded_foreign_symbols) > foreign_index);

                DCint mode = DC_CALL_C_DEFAULT;
                // if (func->function->flags & IR_FUNC_FLAG_VARARG)
                // {
                //     mode = DC_CALL_C_ELLIPSIS;
                // }
                dcMode(runner->dyn_vm, mode);

                assert(iri->result);
                IR_Value* result_value = ir_runner_get_local_temporary(runner, iri->result);
                assert(result_value);
                if (iri->result->type->flags & AST_TYPE_FLAG_INT)
                {
                    result_value->value.s64 = dcCallInt(runner->dyn_vm,
                                                        runner->loaded_foreign_symbols[foreign_index]);
                }
                else if (iri->result->type->kind == AST_TYPE_POINTER)
                {
                    result_value->value.string = (uint8_t*)dcCallPointer(runner->dyn_vm,
                                                               runner->loaded_foreign_symbols[foreign_index]);
                }
                else if (iri->result->type == Builtin::type_void)
                {
                    dcCallVoid(runner->dyn_vm,
                               runner->loaded_foreign_symbols[foreign_index]);
                }
                else if (iri->result->type == Builtin::type_double)
                {
                    result_value->value.r64 = dcCallDouble(runner->dyn_vm,
                                                           runner->loaded_foreign_symbols[foreign_index]);
                }
                else if (iri->result->type == Builtin::type_float)
                {
                    result_value->value.r32 = dcCallFloat(runner->dyn_vm,
                                                          runner->loaded_foreign_symbols[foreign_index]);
                }
                else assert(false);
                dcReset(runner->dyn_vm);
                break;
            }

			case IR_OP_CALL_PTR:
			{
				IR_Value* ptr_val = ir_runner_get_local_temporary(runner, iri->arg1);

				dcMode(runner->dyn_vm, DC_CALL_C_DEFAULT);

				assert(iri->result);
				IR_Value* result_value = ir_runner_get_local_temporary(runner, iri->result);
				assert(result_value);

                if (iri->result->type->flags & AST_TYPE_FLAG_INT)
                {
                    result_value->value.s64 = dcCallInt(runner->dyn_vm, ptr_val->value.string);
                }
				else if (iri->result->type->kind == AST_TYPE_POINTER)
				{
					result_value->value.string = (uint8_t*)dcCallPointer(runner->dyn_vm,
                                                                         ptr_val->value.string);
				}
                else if (iri->result->type->flags & AST_TYPE_FLAG_VOID)
                {
                    dcCallVoid(runner->dyn_vm, ptr_val->value.string);
                }
				else assert(false);
                dcReset(runner->dyn_vm);
				break;
			}

			case IR_OP_ADDROF_FOREIGN:
			{
				IR_Value* func = iri->arg1;
				assert(func->kind == IRV_FUNCTION);
				uint64_t foreign_index = func->function->foreign_index;
				assert(BUF_LENGTH(runner->loaded_foreign_symbols) > foreign_index);

				IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);
				dest->value.string = (uint8_t*)runner->loaded_foreign_symbols[foreign_index];
				break;
			}

            case IR_OP_ADDROF_FUNCTION:
            {
                IR_Value* func = iri->arg1;
                assert(func->kind == IRV_FUNCTION);

                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                DCCallback* callback_address = nullptr;

                if (func->function->dcb_data.callback_address)
                {
                    callback_address = func->function->dcb_data.callback_address;
                }
                else
                {
                    // TODO: temp mem
                    const char* signature_string = get_dcb_signature(func->function->type);
                    func->function->dcb_data.func_value = func;
                    func->function->dcb_data.runner = runner;
                    callback_address = dcbNewCallback(signature_string, dcb_handler,
                                                      &func->function->dcb_data);
                    func->function->dcb_data.callback_address = callback_address;
                    BUF_FREE(signature_string);
                }

                assert(callback_address);
                dest->value.string = (uint8_t*)callback_address;
                break;
            }

            case IR_OP_RETURN:
            {
                if (iri->arg1)
                {
                    IR_Value* temp = ir_runner_get_local_temporary(runner, iri->arg1);

                    auto current_stack_frame = ir_runner_top_stack_frame(runner);

					if (temp->type->kind == AST_TYPE_STRUCT)
					{
                        uint64_t struct_byte_size = temp->type->bit_size / 8;
                        assert(struct_byte_size);
                        memcpy(current_stack_frame->return_value->value.struct_pointer,
                               temp->value.struct_pointer, struct_byte_size);
					}
					else
					{
						*current_stack_frame->return_value = *temp;
					}
                }

                ir_runner_pop_stack_frame(runner);

                assert(!runner->returned);
                runner->returned = true;
                break;
            }

            case IR_OP_SUBSCRIPT:
            {
                assert(iri->arg1->kind == IRV_TEMPORARY);

                AST_Type* element_type = nullptr;

                if (iri->arg1->type->kind == AST_TYPE_POINTER)
                {
                     element_type = iri->arg1->type->pointer.base;
                }
                else if (iri->arg1->type->kind == AST_TYPE_STATIC_ARRAY)
                {
                    element_type = iri->arg1->type->static_array.base;
                }
                else assert(false);
                assert(element_type);
                assert(element_type->bit_size % 8 == 0);

                IR_Value* base_value = ir_runner_get_local_temporary(runner, iri->arg1);

                uint8_t* base_pointer = base_value->value.string;

                IR_Value* index_value = ir_runner_get_local_temporary(runner, iri->arg2);
                assert((iri->arg2->type->flags & AST_TYPE_FLAG_INT) ||
                       iri->arg2->type->kind == AST_TYPE_ENUM);

                assert(iri->result->type == element_type);
                IR_Value* result_value = ir_runner_get_local_temporary(runner, iri->result);
                auto offset = index_value->value.s64 * (element_type->bit_size / 8);

                uint8_t* source_pointer = base_pointer + offset;
                assert(element_type->kind == AST_TYPE_BASE);
                switch (element_type->bit_size)
                {
                    case 8:
                    {
                        result_value->value.u8 = *((uint8_t*)source_pointer);
                        break;
                    }

                    case 64:
                    {
                        result_value->value.s64 = *((uint64_t*)source_pointer);
                        break;
                    }

                    default: assert(false);
                }
                break;
            }

            case IR_OP_JMP:
            {
                IR_Value* block_value = iri->arg1;
                assert(!runner->jump_block);
                runner->jump_block = block_value->block;
                break;
            }

            case IR_OP_JMP_IF:
            {
                IR_Value* cond_value = ir_runner_get_local_temporary(runner, iri->arg1);

                IR_Value* block_value = iri->arg2;

                if (cond_value->value.s64)
                {
                    assert(!runner->jump_block);
                    runner->jump_block = block_value->block;
                }

                break;
            }

            case IR_OP_ALLOCL:
            {
                // Don't do anything for now, these are handled when pushing a new
                //  stack frame
                break;
            }

            case IR_OP_STOREL:
            {
                IR_Value* dest_value = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* source_value = ir_runner_get_local_temporary(runner, iri->arg2);

                if (dest_value->type->kind == AST_TYPE_STATIC_ARRAY)
                {
                    assert(iri->arg1->kind == IRV_ALLOCL);
                    assert(iri->arg2->kind == IRV_ALLOCL);
                    assert(iri->arg1->type == iri->arg2->type);

                    AST_Type* array_type = iri->arg1->type;

                    auto byte_count = array_type->static_array.count *
                        (array_type->static_array.base->bit_size / 8);

                    memcpy(dest_value->value.static_array, source_value->value.static_array,
                           byte_count);
                }
                else if (dest_value->type->kind == AST_TYPE_STRUCT)
                {
                    assert(iri->arg1->kind == IRV_ALLOCL);
                    assert(iri->arg2->kind == IRV_TEMPORARY ||
                           iri->arg2->kind == IRV_ALLOCL);

                    assert(dest_value->type->kind == AST_TYPE_STRUCT);
                    assert(source_value->type->kind == AST_TYPE_STRUCT);

                    AST_Type* struct_type = dest_value->type;
                    uint64_t struct_byte_size = struct_type->bit_size / 8;
                    assert(struct_byte_size);

                    memcpy(dest_value->value.struct_pointer,
                           source_value->value.struct_pointer,
                           struct_byte_size);
                }
                else
                {
                    dest_value->value.s64 = source_value->value.s64;
                }
                break;
            }

            case IR_OP_LOADL:
            {
                IR_Value* source_value = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* dest_value = ir_runner_get_local_temporary(runner, iri->result);

                // dest_value->value.s64 = source_value->value.s64;
                dest_value->value = source_value->value;
                assert(iri->result->type);
                dest_value->type = iri->result->type;
                break;
            }

            case IR_OP_STOREA:
            {
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* source = ir_runner_get_local_temporary(runner, iri->arg2);

                dest->value.s64 = source->value.s64;
                break;
            }

            case IR_OP_LOADA:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_ARGUMENT);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Value* source = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);
                assert(dest->kind == IRV_TEMPORARY);

                dest->value.s64 = source->value.s64;
                break;
            }

            case IR_OP_STOREP:
            {
                IR_Value* pointer_value = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* source_value = ir_runner_get_local_temporary(runner, iri->arg2);
                void* pointer = (void*)pointer_value->value.s64;
                AST_Type* dest_type = iri->arg1->type->pointer.base;
                AST_Type* pointer_type = pointer_value->type;
                AST_Type* pointer_base_type = pointer_type->pointer.base;
                assert(dest_type == pointer_base_type);

                if (pointer_base_type->kind == AST_TYPE_STRUCT)
                {
                    uint64_t struct_byte_size = pointer_base_type->bit_size / 8;
                    assert(struct_byte_size);

                    memcpy(pointer, source_value->value.struct_pointer,
                           struct_byte_size);
                }
                else
                {
                    switch (dest_type->bit_size)
                    {
                    case 8:
                    {
                        *((uint8_t*)pointer) = source_value->value.u8;
                        break;
                    }

                    case 32:
                    {
                        *((uint32_t*)pointer) = source_value->value.u32;
                        break;
                    }

                    case 64:
                    {
                        *((uint64_t*)pointer) = source_value->value.s64;
                        break;
                    }

                    default: assert(false);
                    }
                }

                break;
            }

            case IR_OP_LOADP:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_TEMPORARY);
                assert(iri->arg1->type->kind == AST_TYPE_POINTER);

                assert(iri->result);
                assert(iri->result->type == iri->arg1->type->pointer.base);

                IR_Value* pointer_value = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* dest_value = ir_runner_get_local_temporary(runner, iri->result);
                AST_Type* dest_type = iri->result->type;

                if (dest_type->kind == AST_TYPE_STRUCT)
                {
                    dest_value->value.struct_pointer = pointer_value->value.struct_pointer;
                }
                else if (dest_type->kind == AST_TYPE_STATIC_ARRAY)
                {
                    dest_value->value.static_array = (void*)pointer_value->value.string;
                }
                else if (dest_type->flags & AST_TYPE_FLAG_INT)
                {
                    switch (dest_type->bit_size)
                    {
                        case 64:
                        {
                            dest_value->value.s64 = *((int64_t*)pointer_value->value.string);
                            break;
                        }

                        case 32:
                        {
                            dest_value->value.u32 = *((uint32_t*)pointer_value->value.string);
                            break;
                        }

                        case 16:
                        {
                            dest_value->value.u16 = *((uint16_t*)pointer_value->value.string);
                            break;
                        }

                        case 8:
                        {
                            dest_value->value.u8 = *((uint8_t*)pointer_value->value.string);
                            break;
                        }

                        default: assert(false);
                    }
                }
                else if (dest_type->flags & AST_TYPE_FLAG_FLOAT)
                {
                    switch (dest_type->bit_size)
                    {
                        case 64:
                        {
                            dest_value->value.r64 = *((double*)pointer_value->value.string);
                            break;
                        }

                        case 32:
                        {
                            dest_value->value.r32 = *((float*)pointer_value->value.string);
                            break;
                        }
                    }
                }
                else if (dest_type->kind == AST_TYPE_ENUM)
                {
                    dest_value->value.s64 = *((int64_t*)pointer_value->value.string);
                }
                else assert(false);

                break;
            }

            case IR_OP_STOREG:
            {
                IR_Value* dest_value = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* source_value = ir_runner_get_local_temporary(runner, iri->arg2);

                if (dest_value->type->kind == AST_TYPE_STATIC_ARRAY)
                {
                    assert(false); // Not tested
                    assert(iri->arg1->kind == IRV_ALLOCL);
                    assert(iri->arg2->kind == IRV_ALLOCL);
                    assert(iri->arg1->type == iri->arg2->type);

                    AST_Type* array_type = iri->arg1->type;

                    auto byte_count = array_type->static_array.count *
                        (array_type->static_array.base->bit_size / 8);

                    memcpy(dest_value->value.static_array, source_value->value.static_array,
                           byte_count);
                }
                else if (dest_value->type->kind == AST_TYPE_STRUCT)
                {
                    assert(false); // Not tested
                    assert(iri->arg1->kind == IRV_ALLOCL);
                    assert(iri->arg2->kind == IRV_TEMPORARY ||
                           iri->arg2->kind == IRV_ALLOCL);

                    assert(dest_value->type->kind == AST_TYPE_STRUCT);
                    assert(source_value->type->kind == AST_TYPE_STRUCT);

                    AST_Type* struct_type = dest_value->type;
                    uint64_t struct_byte_size = struct_type->bit_size / 8;
                    assert(struct_byte_size);

                    memcpy(dest_value->value.struct_pointer,
                           source_value->value.struct_pointer,
                           struct_byte_size);
                }
                else
                {
                    dest_value->value = source_value->value;
                }
                break;
                break;
            }

            case IR_OP_LOADG:
            {
                IR_Value* source_value = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* dest_value = ir_runner_get_local_temporary(runner, iri->result);

                dest_value->value = source_value->value;
                assert(iri->result->type);
                dest_value->type = iri->result->type;
                break;
            }

            case IR_OP_LOAD_LIT:
            {
                assert(iri->arg1);
                assert(iri->result->kind == IRV_TEMPORARY);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);
                switch (iri->arg1->kind)
                {
                    case IRV_STRING_LITERAL:
                    case IRV_INT_LITERAL:
                    case IRV_FLOAT_LITERAL:
                    case IRV_CHAR_LITERAL:
                    {
                        dest->value = iri->arg1->value;
                        break;
                    }

                    default: assert(false);
                }
                break;
            }

            case IR_OP_ADDROF:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_ALLOCL ||
                       iri->arg1->kind == IRV_GLOBAL);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);
                IR_Value* source = ir_runner_get_local_temporary(runner, iri->arg1);

                if (iri->arg1->type->kind == AST_TYPE_STRUCT)
                {
                    dest->value.struct_pointer = source->value.struct_pointer;
                }
                else
                {
                    dest->value.s64 = (int64_t)&source->value.s64;
                }

                break;
            }

            case IR_OP_DEREF:
            {
                assert(iri->arg1);
                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                IR_Value* source_allocl = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                if (iri->result->type->kind == AST_TYPE_STRUCT)
                {
                    dest->value.struct_pointer = source_allocl->value.struct_pointer;
                }
                else
                {
                    int64_t* source_ptr = (int64_t*)source_allocl->value.s64;
                    dest->value.s64 = *source_ptr;
                }

                break;
            }

            case IR_OP_NOT:
            {
                assert(iri->arg1);

                IR_Value* operand_val = ir_runner_get_local_temporary(runner, iri->arg1);
                IR_Value* result_val = ir_runner_get_local_temporary(runner, iri->result);
                if (iri->arg1->type == Builtin::type_bool)
                {
                    result_val->value.boolean = !operand_val->value.boolean;
                }
                else if (iri->arg1->type->flags & AST_TYPE_FLAG_INT)
                {
                    result_val->value.boolean = !operand_val->value.s64;
                }
                else if (iri->arg1->type->kind == AST_TYPE_POINTER)
                {
                    result_val->value.boolean = !operand_val->value.string;
                }
                else assert(false);

                break;
            }

            case IR_OP_ARRAY_OFFSET_POINTER:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_ALLOCL ||
                       iri->arg1->kind == IRV_TEMPORARY);
                assert(iri->arg1->type->kind == AST_TYPE_STATIC_ARRAY ||
                       iri->arg1->type->kind == AST_TYPE_POINTER);

                assert(iri->arg2);
                assert(iri->arg2->kind == IRV_INT_LITERAL ||
                       iri->arg2->kind == IRV_TEMPORARY);
                assert(iri->arg2->type == Builtin::type_int);

                assert(iri->result);
                assert(iri->result->kind == IRV_TEMPORARY);

                uint64_t index = 0;
                if (iri->arg2->kind == IRV_INT_LITERAL)
                {
                    index = iri->arg2->value.s64;
                }
                else
                {
                    assert(iri->arg2->kind == IRV_TEMPORARY);
                    IR_Value* index_value = ir_runner_get_local_temporary(runner, iri->arg2);
                    index = index_value->value.s64;
                }

                IR_Value* base_pointer_value = ir_runner_get_local_temporary(runner, iri->arg1);
                AST_Type* element_type = nullptr;
                void* base_pointer = nullptr;
                if (iri->arg1->type->kind == AST_TYPE_STATIC_ARRAY)
                {
                    element_type = iri->arg1->type->static_array.base;
                    base_pointer = (uint8_t*)base_pointer_value->value.static_array;
                }
                else if (iri->arg1->type->kind == AST_TYPE_POINTER)
                {
                    element_type = iri->arg1->type->pointer.base;
                    base_pointer = (uint8_t*)base_pointer_value->value.string;
                }
                else assert(false);
                void* result_pointer = ((uint8_t*)base_pointer) +
                    (index * (element_type->bit_size / 8));

                IR_Value* result_pointer_value = ir_runner_get_local_temporary(runner,
                                                                               iri->result);
                assert(iri->result->type->kind == AST_TYPE_POINTER);
                result_pointer_value->type = iri->result->type;
                result_pointer_value->value.s64 = (int64_t)result_pointer;
                break;
            }

			case IR_OP_CAST:
			{
				IR_Value* source = ir_runner_get_local_temporary(runner, iri->arg1);
				IR_Value* dest = ir_runner_get_local_temporary(runner, iri->result);

                if ((iri->arg1->type->flags & AST_TYPE_FLAG_INT) &&
                    iri->result->type == Builtin::type_float)
                {
                    if (iri->arg1->type->flags & AST_TYPE_FLAG_SIGNED)
                    {
                        dest->value.r32 = (float)source->value.s64;
                    }
                    else
                    {
                        dest->value.r32 = (float(source->value.u64));
                    }
                }
                else if ((iri->arg1->type->flags & AST_TYPE_FLAG_INT) &&
                         iri->result->type == Builtin::type_double)
                {
                    if (iri->arg1->type->flags & AST_TYPE_FLAG_SIGNED)
                    {
                        dest->value.r64 = (double)source->value.s64;
                    }
                    else
                    {
                        dest->value.r64 = (double)source->value.u64;
                    }
                }
                else if (iri->arg1->type == Builtin::type_float &&
                         (iri->result->type->flags & AST_TYPE_FLAG_INT))
                {
                    if (iri->result->type->flags & AST_TYPE_FLAG_SIGNED)
                    {
                        dest->value.s64 = (int64_t)source->value.r32;
                    }
                    else
                    {
                        dest->value.u64 = (uint64_t)source->value.r32;
                    }
                }
                else if (iri->arg1->type == Builtin::type_double &&
                         iri->result->type == Builtin::type_float)
                {
                    dest->value.r32 = (float)source->value.r64;
                }
                else if (iri->arg1->type == Builtin::type_float &&
                         iri->result->type == Builtin::type_double)
                {
                    dest->value.r64 = (double)source->value.r32;
                }
                else if (((iri->arg1->type->flags & AST_TYPE_FLAG_INT) &&
                          (iri->result->type->flags & AST_TYPE_FLAG_INT)) ||
                         (iri->arg1->type->kind == AST_TYPE_POINTER &&
                          iri->result->type->kind == AST_TYPE_POINTER))
                {
                    dest->value = source->value;
                }
                else if (iri->arg1->type->kind == AST_TYPE_POINTER &&
                         iri->result->type == Builtin::type_bool)
                {
                    dest->value.boolean = source->value.string != nullptr;
                }
                else if ((iri->arg1->type->flags & AST_TYPE_FLAG_INT) &&
                         iri->result->type->kind == AST_TYPE_ENUM)
                {
                    dest->value.u64 = source->value.u64;
                }
                else assert(false);

				break;
			}

            case IR_OP_AGGREGATE_OFFSET_POINTER:
            {
                assert(iri->arg1);
                assert(iri->arg1->kind == IRV_ALLOCL ||
                       iri->arg1->kind == IRV_ARGUMENT ||
                       iri->arg1->kind == IRV_TEMPORARY);
                assert(iri->arg1->type->kind == AST_TYPE_STRUCT);

                assert(iri->arg2);
                assert(iri->arg2->kind == IRV_INT_LITERAL);
                assert(iri->arg2->type == Builtin::type_int);

                IR_Value* struct_pointer_value = ir_runner_get_local_temporary(runner, iri->arg1);

                AST_Type* struct_type = iri->arg1->type;
                uint64_t member_offset = 0;
                uint64_t member_index = iri->arg2->value.s64;
                bool found = false;
                auto member_decls = struct_type->aggregate_type.member_declarations;
                assert(BUF_LENGTH(member_decls) > member_index);
                for (uint64_t i = 0; i < BUF_LENGTH(member_decls); i++)
                {
                    AST_Declaration* member_decl = member_decls[i];
                    AST_Type* member_type = member_decl->mutable_decl.type;
                    if (i == member_index)
                    {
                        found = true;
                        break;
                    }
                    member_offset += member_type->bit_size;
                }
                assert(found);

                void* result_pointer = ((uint8_t*)struct_pointer_value->value.struct_pointer) +
                                        (member_offset / 8);
                IR_Value* result_pointer_value = ir_runner_get_local_temporary(runner,
                                                                               iri->result);
                assert(iri->result->type->kind == AST_TYPE_POINTER);
                result_pointer_value->type = iri->result->type;
                result_pointer_value->value.struct_pointer = result_pointer;
                break;
            }

            default: assert(false);
        }
    }

    IR_Stack_Frame* ir_runner_new_stack_frame(IR_Runner* ir_runner, IR_Function* function,
                                              BUF(IR_Value) args, IR_Value* return_value)
    {
        assert(ir_runner);
        assert(function);

        IR_Stack_Frame* result = nullptr;
        if (ir_runner->free_stack_frames)
        {
            result = ir_runner->free_stack_frames;
            ir_runner->free_stack_frames = result->next_free;
            result->next_free = nullptr;
        }
        else
        {
            result = arena_alloc(&ir_runner->arena, IR_Stack_Frame);
            result->arena = arena_create(KB(1));
        }
        result->function = function;
        result->args = args;

        for (uint64_t i = 0; i < BUF_LENGTH(function->local_temps); i++)
        {
            IR_Value* code_temp = function->local_temps[i];
            IR_Value new_temp = {};
            new_temp.kind = IRV_TEMPORARY;
            new_temp.type = code_temp->type;

            // TODO: A lot of this can probably be merged together,
            if (code_temp->kind == IRV_ALLOCL &&
                code_temp->type->kind == AST_TYPE_STATIC_ARRAY)
            {
                AST_Type* array_type = code_temp->type;
                uint64_t array_byte_size = array_type->static_array.count *
                    (array_type->static_array.base->bit_size / 8);
                assert(array_byte_size == array_type->bit_size / 8);
                new_temp.value.static_array = arena_alloc_array(&result->arena, uint8_t,
                                                                array_byte_size);
            }
            else if (code_temp->kind == IRV_ALLOCL &&
                     code_temp->type->kind == AST_TYPE_STRUCT)
            {
                AST_Type* struct_type = code_temp->type;
                uint64_t struct_byte_size = struct_type->bit_size / 8;
                assert(struct_byte_size);
                new_temp.value.struct_pointer = arena_alloc_array(&result->arena, uint8_t,
                                                                  struct_byte_size);
            }
            else if (code_temp->kind == IRV_TEMPORARY &&
                     code_temp->type->kind == AST_TYPE_STRUCT)
            {
                AST_Type* struct_type = code_temp->type;
                uint64_t struct_byte_size = struct_type->bit_size / 8;
                assert(struct_byte_size);
                new_temp.value.struct_pointer = arena_alloc_array(&result->arena, uint8_t,
                                                                  struct_byte_size);
            }
            assert(new_temp.type);
            BUF_PUSH(result->temps, new_temp);

            if (function->type->function.return_type)
            {
                assert(return_value);
                result->return_value = return_value;
            }
        }

        return result;
    }

    IR_Stack_Frame* ir_runner_push_stack_frame(IR_Runner* ir_runner, IR_Function* function,
                                               BUF(IR_Value) args, IR_Value* return_value)
    {
        assert(ir_runner);
        assert(function);

        IR_Stack_Frame* new_frame = ir_runner_new_stack_frame(ir_runner, function, args,
                                                              return_value);
        assert(new_frame);

        stack_push(ir_runner->call_stack, new_frame);
        return new_frame;
    }

    IR_Stack_Frame* ir_runner_top_stack_frame(IR_Runner* ir_runner)
    {
        assert(ir_runner);
        if (stack_count(ir_runner->call_stack))
		{
			return stack_top(ir_runner->call_stack);
		}
		else
		{
			return nullptr;
		}
    }

    void ir_runner_pop_stack_frame(IR_Runner* ir_runner)
    {
        assert(ir_runner);
        assert(stack_count(ir_runner->call_stack));
        auto old_stack_frame = stack_pop(ir_runner->call_stack);

        // arena_free(&old_stack_frame->arena);
        arena_reset(&old_stack_frame->arena);

        // *old_stack_frame = {};
        old_stack_frame->function = nullptr;
        old_stack_frame->args = nullptr;
        BUF_FREE(old_stack_frame->temps);
        old_stack_frame->temps = nullptr;

        old_stack_frame->next_free = ir_runner->free_stack_frames;
        ir_runner->free_stack_frames = old_stack_frame;
    }

    static const char* get_dcb_signature(AST_Type* type)
    {
        assert(type);

        switch (type->kind)
        {
            case AST_TYPE_FUNCTION:
            {
                BUF(char) signature = nullptr;
                for (uint64_t i = 0; i < BUF_LENGTH(type->function.arg_types); i++)
                {
                    BUF_PUSH(signature, get_dcb_signature_char(type->function.arg_types[i]));
                }
                BUF_PUSH(signature, ')');
                BUF_PUSH(signature, get_dcb_signature_char(type->function.return_type));
                return signature;
                break;
            }

            default: assert(false);
        }

		assert(false);
		return 0;
    }

    static char get_dcb_signature_char(AST_Type* type)
    {
        assert(type);

        switch (type->kind)
        {
            case AST_TYPE_POINTER:
                return 'p';

            case AST_TYPE_BASE:
            {
                if (type->flags & AST_TYPE_FLAG_INT)
                {
                    char c;
                    switch (type->bit_size)
                    {
                        case 8:
                        {
                            c = 'c';
                            break;
                        }

                        case 16:
                        {
                            c = 's';
                            break;
                        }

                        case 32:
                        {
                            c = 'i';
                            break;
                        }

                        case 64:
                        {
                            c = 'l';
                            break;
                        }
                        default: assert(false);
                    }

                    if (!(type->flags & AST_TYPE_FLAG_SIGNED))
                    {
                        c -= 'a' - 'A';
                    }
                    return c;
                }
                else if (type == Builtin::type_float)
                {
                    assert(false);
                }
                else if (type == Builtin::type_double)
                {
                    assert(false);
                }
                else if (type->flags & AST_TYPE_FLAG_VOID)
                {
                    return 'v';
                }
                else assert(false);
                break;
            }

            default: assert(false);
        }

        assert(false);
		return 0;
    }
}
