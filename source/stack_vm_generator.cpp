#include "stack_vm_generator.h"

#include "builtin.h"

namespace Zodiac
{
    void stack_vm_generator_init(Stack_VM_Generator* generator)
    {
        assert(generator);

        generator->entry_address = 0;
        generator->found_entry = false;
        generator->current_function = nullptr;
        generator->gen_data = nullptr;
        generator->address_dependencies = nullptr;
        generator->foreign_names = nullptr;
        generator->result = {};
    }

    void stack_vm_generator_emit_strings(Stack_VM_Generator* generator, IR_Module* module)
    {
        assert(generator);
        assert(module);

        auto string_count = BUF_LENGTH(module->string_table);
        if (string_count)
        {
            stack_vm_generator_emit_op(generator, SVMI_STRING_TABLE);
            stack_vm_generator_emit_u64(generator, string_count);

            for (uint64_t i = 0; i < string_count; i++)
            {
                stack_vm_generator_emit_string(generator, module->string_table[i]);
            }
        }
    }

    void stack_vm_generator_emit_string(Stack_VM_Generator* generator, Atom string)
    {
        assert(generator);

        uint64_t length = string.length;

        stack_vm_generator_emit_u64(generator, length);

        uint64_t to_push = 0;
        uint64_t shift = 0;
        for (uint64_t i = 0; i < string.length; i++)
        {
            to_push |= (uint64_t)string.data[i] << shift;
            shift += 8;

            if (shift > 56)
            {
                stack_vm_generator_emit_u64(generator, to_push);
                shift = 0;
                to_push = 0;
            }
        }

        if (shift != 0)
        {
            stack_vm_generator_emit_u64(generator, to_push);
        }
    }

    void stack_vm_generator_emit_dynamic_lib_loads(Stack_VM_Generator* generator, IR_Module* module)
    {
        assert(generator);
        assert(module);

        auto idx_count = BUF_LENGTH(module->dynamic_lib_idxs);

        for (uint64_t i = 0; i < idx_count; i++)
        {
            stack_vm_generator_emit_op(generator, SVMI_LOAD_DYN_LIB);
            stack_vm_generator_emit_u64(generator, module->dynamic_lib_idxs[i]);
        }
    }

    void stack_vm_generator_emit_foreigns(Stack_VM_Generator* generator, IR_Module* module)
    {
        assert(generator);
        assert(module);

        auto foreign_count = BUF_LENGTH(module->foreign_table);
        if (foreign_count)
        {
            stack_vm_generator_emit_op(generator, SVMI_FOREIGN_TABLE);
            stack_vm_generator_emit_u64(generator, foreign_count);

            for (uint64_t i = 0; i < foreign_count; i++)
            {
                stack_vm_generator_emit_string(generator, module->foreign_table[i]);
            }
        }
    }

    uint64_t stack_vm_generator_get_func_address(Stack_VM_Generator* generator, IR_Function* func,
                                                 bool* found)
    {
        assert(generator);
        assert(func);
        assert(found);

        auto gen_data = stack_vm_generator_get_gen_data(generator, func);
        if (gen_data)
        {
            assert(gen_data->kind == SVMGD_FUNCTION);
            *found = true;
            return gen_data->function.address;
        }
        else
        {
            *found = false;
            return 0;
        }
    }

    uint64_t stack_vm_generator_get_block_address(Stack_VM_Generator* generator, IR_Block* block,
                                                  bool* found)
    {
        assert(generator);
        assert(block);
        assert(found);

        auto gen_data = stack_vm_generator_get_gen_data(generator, block);
        if (gen_data)
        {
            assert(gen_data->kind == SVMGD_BLOCK);
            *found = true;
            return gen_data->block.address;
        }
        else
        {
            *found = false;
            return 0;
        }
    }

    Stack_VM_Gen_Data* stack_vm_generator_get_gen_data(Stack_VM_Generator* generator,
                                                            IR_Function* func)
    {
        assert(generator);
        assert(func);

        for (uint64_t i = 0; i < BUF_LENGTH(generator->gen_data); i++)
        {
            auto gen_data = &generator->gen_data[i];

            if (gen_data->function.function == func)
            {
                return gen_data;
            }
        }

        return nullptr;
    }

    Stack_VM_Gen_Data* stack_vm_generator_get_gen_data(Stack_VM_Generator* generator,
                                                       IR_Block* block)
    {
        assert(generator);
        assert(block);

        for (uint64_t i = 0; i < BUF_LENGTH(generator->gen_data); i++)
        {
            auto gen_data = &generator->gen_data[i];

            if (gen_data->block.block == block)
            {
                return gen_data;
            }
        }

        return nullptr;
    }

    Stack_VM_Gen_Data* stack_vm_generator_get_gen_data(Stack_VM_Generator* generator,
                                                       IR_Value* value)
    {
        assert(generator);
        assert(value);

        assert(value->kind == IRV_ALLOCL);

        for (uint64_t i = 0; i < BUF_LENGTH(generator->gen_data); i++)
        {
            auto gen_data = &generator->gen_data[i];

            if (gen_data->allocl.allocl == value)
            {
                return gen_data;
            }
        }

        return nullptr;
    }

    Stack_VM_Gen_Data* stack_vm_generator_create_gen_data(Stack_VM_Generator* generator,
                                                               IR_Function* func)
    {
        assert(generator);
        assert(func);
        assert(!stack_vm_generator_get_gen_data(generator, func));

        Stack_VM_Gen_Data dummy = {};
        dummy.kind = SVMGD_FUNCTION;
        dummy.function.function = func;
        BUF_PUSH(generator->gen_data, dummy);

        return stack_vm_generator_get_gen_data(generator, func);
    }

    Stack_VM_Gen_Data* stack_vm_generator_create_gen_data(Stack_VM_Generator* generator,
                                                          IR_Block* block)
    {
        assert(generator);
        assert(block);
        assert(!stack_vm_generator_get_gen_data(generator, block));

        Stack_VM_Gen_Data dummy = {};
        dummy.kind = SVMGD_BLOCK;
        dummy.block.block = block;
        BUF_PUSH(generator->gen_data, dummy);

        return stack_vm_generator_get_gen_data(generator, block);
    }

    Stack_VM_Gen_Data* stack_vm_generator_create_gen_data(Stack_VM_Generator* generator,
                                                          IR_Value* value)
    {
        assert(generator);
        assert(value);
        assert(value->kind == IRV_ALLOCL);
        assert(!stack_vm_generator_get_gen_data(generator, value));

        Stack_VM_Gen_Data dummy = {};
        dummy.kind = SVMGD_ALLOCL;
        dummy.allocl.allocl = value;
        BUF_PUSH(generator->gen_data, dummy);

        return stack_vm_generator_get_gen_data(generator, value);
    }

    void stack_vm_generator_add_address_dependency(Stack_VM_Generator* generator, uint64_t address_index,
                                                   IR_Value* ir_value)
    {
        assert(generator);
        assert(ir_value);

        SVMG_Address_Dependency dep = {};
        dep.value = ir_value;
        dep.address_index = address_index;

        BUF_PUSH(generator->address_dependencies, dep);
    }

    void stack_vm_generator_satisfy_address_dependencies(Stack_VM_Generator* generator)
    {
        assert(generator);

        for (uint64_t i = 0; i < BUF_LENGTH(generator->address_dependencies); i++)
        {
            auto dep = generator->address_dependencies[i];

            bool found = false;
            uint64_t address = 0;

            switch (dep.value->kind)
            {
                case IRV_FUNCTION:
                {
                    address = stack_vm_generator_get_func_address(generator, dep.value->function, &found);
                    break;
                }

                case IRV_BLOCK:
                {
                    address = stack_vm_generator_get_block_address(generator, dep.value->block, &found);
                    break;
                }

                default: assert(false);
            }

            assert(found);

            generator->result.instructions[dep.address_index] = address;
        }
    }

    void stack_vm_generator_emit_space_for_locals(Stack_VM_Generator* generator, IR_Function* func)
    {
        assert(generator);
        assert(func);

        uint64_t offset = 0;

        for (uint64_t i = 0; i < BUF_LENGTH(func->allocls); i++)
        {
            IR_Value* allocl_value = func->allocls[i];
            assert(allocl_value->kind == IRV_ALLOCL);
            auto gd = stack_vm_generator_create_gen_data(generator, allocl_value);
            gd->allocl.function_local_offset = offset;

            offset += 1;
        }

        if (offset)
        {
            stack_vm_generator_emit_op(generator, SVMI_ALLOCL);
            stack_vm_generator_emit_u64(generator, offset);
        }
    }

    void stack_vm_generator_emit_module(Stack_VM_Generator* generator, IR_Module* ir_module)
    {
        assert(generator);
        assert(ir_module);

        stack_vm_generator_emit_dynamic_lib_loads(generator, ir_module);
        stack_vm_generator_emit_foreigns(generator, ir_module);

        stack_vm_generator_emit_op(generator, SVMI_CALL_IMM);
        // Will be replaced with the entry point address
        auto main_address_index = BUF_LENGTH(generator->result.instructions);
        stack_vm_generator_emit_address(generator, 0);
        stack_vm_generator_emit_u64(generator, 0);
        stack_vm_generator_emit_op(generator, SVMI_HALT);

        stack_vm_generator_emit_strings(generator, ir_module);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_module->functions); i++)
        {
            IR_Function* function = ir_module->functions[i];
            stack_vm_generator_emit_function(generator, function);
        }

        assert(generator->found_entry);
        generator->result.instructions[main_address_index] = generator->entry_address;

        stack_vm_generator_satisfy_address_dependencies(generator);
    }

    void stack_vm_generator_emit_function(Stack_VM_Generator* generator, IR_Function* ir_function)
    {
        assert(generator);
        assert(ir_function);

        assert(generator->current_function == nullptr);
        generator->current_function = ir_function;

        auto function_address = BUF_LENGTH(generator->result.instructions);

        if (ir_function->is_entry)
        {
            assert(!generator->found_entry);
            generator->found_entry = true;
            generator->entry_address = function_address;
        }

        if (!(ir_function->flags &= IR_FUNC_FLAG_FOREIGN))
        {
            assert(ir_function->first_block);
            stack_vm_generator_emit_space_for_locals(generator, ir_function);

            IR_Block* block = ir_function->first_block;
            while (block)
            {
                stack_vm_generator_emit_block(generator, block);
                block = block->next;
            }

            Stack_VM_Gen_Data* gd = stack_vm_generator_create_gen_data(generator, ir_function);
            gd->function.address = function_address;
        }
        else
        {
            // Foreign function, do noting for now?
        }

        generator->current_function = nullptr;
    }

    void stack_vm_generator_emit_block(Stack_VM_Generator* generator, IR_Block* ir_block)
    {
        assert(generator);
        assert(ir_block);

        auto gd = stack_vm_generator_create_gen_data(generator, ir_block);
        gd->block.address = BUF_LENGTH(generator->result.instructions);

        IR_Instruction* iri = ir_block->first_instruction;
        while (iri)
        {
            stack_vm_generator_emit_instruction(generator, iri);
            iri = iri->next;
        }
    }

    void stack_vm_generator_emit_instruction(Stack_VM_Generator* generator, IR_Instruction* iri)
    {
        assert(generator);
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
                stack_vm_generator_emit_value(generator, iri->arg1);
                stack_vm_generator_emit_value(generator, iri->arg2);
                stack_vm_generator_emit_op(generator, SVMI_ADD_S64);
                break;
            }

            case IR_OP_SUB:
            {
                stack_vm_generator_emit_value(generator, iri->arg1);
                stack_vm_generator_emit_value(generator, iri->arg2);
                stack_vm_generator_emit_op(generator, SVMI_SUB_S64);
                break;
            }

            case IR_OP_MUL:
            {
                stack_vm_generator_emit_value(generator, iri->arg1);
                stack_vm_generator_emit_value(generator, iri->arg2);
                stack_vm_generator_emit_op(generator, SVMI_MUL_S64);
                break;
            }

            case IR_OP_DIV:
            {
                stack_vm_generator_emit_value(generator, iri->arg1);
                stack_vm_generator_emit_value(generator, iri->arg2);
                stack_vm_generator_emit_op(generator, SVMI_DIV_S64);
                break;
            }

            case IR_OP_LT:
            {
                stack_vm_generator_emit_value(generator, iri->arg1);
                stack_vm_generator_emit_value(generator, iri->arg2);
                stack_vm_generator_emit_op(generator, SVMI_LT_S64);
                break;
            }

            case IR_OP_LTEQ:
            {
                stack_vm_generator_emit_value(generator, iri->arg1);
                stack_vm_generator_emit_value(generator, iri->arg2);
                stack_vm_generator_emit_op(generator, SVMI_LTEQ_S64);
                break;
            }

            case IR_OP_PUSH_CALL_ARG:
            {
                stack_vm_generator_emit_value(generator, iri->arg1);
                break;
            }

            case IR_OP_CALL:
            {
                IR_Value* func_value = iri->arg1;
                bool function_generated = false;
                uint64_t function_address = stack_vm_generator_get_func_address(generator,
                                                                                func_value->function,
                                                                                &function_generated);
                stack_vm_generator_emit_op(generator, SVMI_CALL_IMM);
                if (function_generated)
                {
                    // printf("Emitted %lu for function: %s\n", function_address, func_value->function->name);
                    stack_vm_generator_emit_address(generator, function_address);
                }
                else
                {
                    auto address_index = BUF_LENGTH(generator->result.instructions);

                    // Emmit dummy
                    stack_vm_generator_emit_address(generator, 0);

                    stack_vm_generator_add_address_dependency(generator, address_index, func_value);
                }
                stack_vm_generator_emit_u64(generator, iri->arg2->literal.s64);
                break;
            }

            case IR_OP_CALL_FOREIGN:
            {
                stack_vm_generator_emit_op(generator, SVMI_CALL_EX);
                stack_vm_generator_emit_u64(generator, iri->arg1->literal.s64);
                stack_vm_generator_emit_u64(generator, iri->arg2->literal.s64);
                break;
            }

            case IR_OP_RETURN:
            {
                if (!iri->arg1)
                {
                    assert(generator->current_function->return_type == Builtin::type_void);
                    stack_vm_generator_emit_op(generator, SVMI_PUSH_S64);
                    stack_vm_generator_emit_u64(generator, 0);
                }
                else
                {
                    stack_vm_generator_emit_value(generator, iri->arg1);
                }
                stack_vm_generator_emit_op(generator, SVMI_RETURN);
                break;
            }

            case IR_OP_JMP:
            {
                stack_vm_generator_emit_op(generator, SVMI_JMP_IMM);
                IR_Value* target_block_value = iri->arg1;
                assert(target_block_value);
                assert(target_block_value->kind == IRV_BLOCK);

                auto target_block_addr_index = BUF_LENGTH(generator->result.instructions);
                stack_vm_generator_emit_address(generator, 0);
                stack_vm_generator_add_address_dependency(generator, target_block_addr_index,
                                                          target_block_value);
                break;
            }

            case IR_OP_JMP_IF:
            {
                stack_vm_generator_emit_value(generator, iri->arg1);
                stack_vm_generator_emit_op(generator, SVMI_JMP_COND);

                IR_Value* then_block_value = iri->arg2;
                assert(then_block_value);
                assert(then_block_value->kind == IRV_BLOCK);

                auto then_address_index = BUF_LENGTH(generator->result.instructions);
                stack_vm_generator_emit_address(generator, 0);
                stack_vm_generator_add_address_dependency(generator, then_address_index,
                                                          then_block_value);
                break;
            }

            case IR_OP_ALLOCL:
            {
                // Don't do anything, space for all of these will be emitted at the
                //  beginning of every function.
                break;
            }

            case IR_OP_STOREL:
            {
                stack_vm_generator_emit_value(generator, iri->arg2);
                stack_vm_generator_emit_op(generator, SVMI_STOREL_S64);
                IR_Value* allocl_value = iri->arg1;
                assert(allocl_value);
                assert(allocl_value->kind == IRV_ALLOCL);
                auto gd = stack_vm_generator_get_gen_data(generator, allocl_value);
                assert(gd);
                assert(gd->kind == SVMGD_ALLOCL);
                stack_vm_generator_emit_s64(generator, 2 + gd->allocl.function_local_offset);
                break;
            }

            case IR_OP_LOADL:
            {
                stack_vm_generator_emit_op(generator, SVMI_LOADL_S64);
                IR_Value* allocl_value = iri->arg1;
                assert(allocl_value);
                assert(allocl_value->kind == IRV_ALLOCL);
                auto gd = stack_vm_generator_get_gen_data(generator, allocl_value);
                assert(gd);
                assert(gd->kind == SVMGD_ALLOCL);
                stack_vm_generator_emit_s64(generator, 2 + gd->allocl.function_local_offset);
                break;
            }

            case IR_OP_STOREA:
            {
                stack_vm_generator_emit_value(generator, iri->arg2);
                stack_vm_generator_emit_op(generator, SVMI_STOREL_S64);
                IR_Value* argument = iri->arg1;
                uint64_t arg_offset = (BUF_LENGTH(generator->current_function->arguments) - 1) -
                    argument->argument.index;
                stack_vm_generator_emit_s64(generator, -2 - arg_offset);
                break;
            }

            case IR_OP_LOADA:
            {
                stack_vm_generator_emit_op(generator, SVMI_LOADL_S64);
                IR_Value* argument = iri->arg1;
                assert(generator->current_function);
                uint64_t arg_offset = (BUF_LENGTH(generator->current_function->arguments) - 1) -
                    argument->argument.index;
                stack_vm_generator_emit_s64(generator, -2 - arg_offset);
                break;
            }

            case IR_OP_LOAD_LIT:
            {
                IR_Value* literal = iri->arg1;
                stack_vm_generator_emit_op(generator, SVMI_PUSH_S64);
                stack_vm_generator_emit_s64(generator, literal->literal.s64);
                break;
            }

            default: assert(false);
        }
    }

    void stack_vm_generator_emit_value(Stack_VM_Generator* generator, IR_Value* ir_value)
    {
        assert(generator);
        assert(ir_value);

        switch (ir_value->kind)
        {
            case IRV_TEMPORARY:
            {
                // assert(false);
                break;
            }

            case IRV_LITERAL:
            {
                stack_vm_generator_emit_op(generator, SVMI_PUSH_S64);
                stack_vm_generator_emit_s64(generator, ir_value->literal.s64);
                break;
            }

            case IRV_ARGUMENT:
            {
                assert(false);
                break;
            }

            case IRV_FUNCTION:
            {
                assert(false);
                break;
            }

            case IRV_BLOCK:
            {
                assert(false);
                break;
            }

            case IRV_ALLOCL:
            {
                assert(false);
                break;
            }

            default: assert(false);
        }
    }

    void stack_vm_generator_emit_op(Stack_VM_Generator* generator, Stack_VM_Instruction op)
    {
        assert(generator);

        stack_vm_generator_emit_u64(generator, op);
    }

    void stack_vm_generator_emit_address(Stack_VM_Generator* generator, uint64_t address)
    {
        assert(generator);

        stack_vm_generator_emit_u64(generator, address);
    }

    void stack_vm_generator_emit_s64(Stack_VM_Generator* generator, int64_t s64)
    {
        assert(generator);

        stack_vm_generator_emit_u64(generator, s64);
    }

    void stack_vm_generator_emit_u64(Stack_VM_Generator* generator, uint64_t u64)
    {
        assert(generator);

        BUF_PUSH(generator->result.instructions, u64);
    }
}
