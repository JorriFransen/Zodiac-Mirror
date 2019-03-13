#include "stack_vm_generator.h"

namespace Zodiac
{
    void stack_vm_generator_init(Stack_VM_Generator* generator)
    {
        assert(generator);

        generator->entry_address = 0;
        generator->found_entry = false;
        generator->func_gen_data = nullptr;
        generator->result = {};
    }

    uint64_t stack_vm_generator_get_func_address(Stack_VM_Generator* generator, IR_Function* func)
    {
        assert(generator);
        assert(func);

        return stack_vm_generator_get_gen_data(generator, func)->address;
    }

    Stack_VM_Func_Gen_Data* stack_vm_generator_get_gen_data(Stack_VM_Generator* generator,
                                                            IR_Function* func)
    {
        assert(generator);
        assert(func);

        for (uint64_t i = 0; i < BUF_LENGTH(generator->func_gen_data); i++)
        {
            auto gen_data = &generator->func_gen_data[i];

            if (gen_data->function == func)
            {
                return gen_data;
            }
        }

        return nullptr;
    }

    Stack_VM_Func_Gen_Data* stack_vm_generator_create_gen_data(Stack_VM_Generator* generator,
                                                               IR_Function* func)
    {
        assert(generator);
        assert(func);
        assert(!stack_vm_generator_get_gen_data(generator, func));

        Stack_VM_Func_Gen_Data dummy = {};
        dummy.function = func;
        BUF_PUSH(generator->func_gen_data, dummy);

        return stack_vm_generator_get_gen_data(generator, func);
    }

    void stack_vm_generator_emit_module(Stack_VM_Generator* generator, IR_Module* ir_module)
    {
        assert(generator);
        assert(ir_module);

        stack_vm_generator_emit_op(generator, SVMI_CALL_IMM);
        // Will be replaced with the entry point address
        auto main_address_index = BUF_LENGTH(generator->result.instructions);
        stack_vm_generator_emit_address(generator, 0);
        stack_vm_generator_emit_u64(generator, 0);
        stack_vm_generator_emit_op(generator, SVMI_HALT);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_module->functions); i++)
        {
            IR_Function* function = ir_module->functions[i];
            stack_vm_generator_emit_function(generator, function);
        }

        assert(generator->found_entry);
        generator->result.instructions[main_address_index] = generator->entry_address;
    }

    void stack_vm_generator_emit_function(Stack_VM_Generator* generator, IR_Function* ir_function)
    {
        assert(generator);
        assert(ir_function);

        auto function_address = BUF_LENGTH(generator->result.instructions);

        if (ir_function->is_entry)
        {
            assert(!generator->found_entry);
            generator->found_entry = true;
            generator->entry_address = function_address;
        }

        IR_Block* block = ir_function->first_block;
        while (block)
        {
            stack_vm_generator_emit_block(generator, block);
            block = block->next;
        }

        Stack_VM_Func_Gen_Data* fgd = stack_vm_generator_create_gen_data(generator, ir_function);
        fgd->address = function_address;
    }

    void stack_vm_generator_emit_block(Stack_VM_Generator* generator, IR_Block* ir_block)
    {
        assert(generator);
        assert(ir_block);

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
                assert(false);
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
                assert(false);
                break;
            }

            case IR_OP_LTEQ:
            {
                assert(false);
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
                uint64_t function_address = stack_vm_generator_get_func_address(generator,
                                                                                func_value->function);
                stack_vm_generator_emit_op(generator, SVMI_CALL_IMM);
                stack_vm_generator_emit_address(generator, function_address);
                stack_vm_generator_emit_u64(generator, iri->arg2->literal.s64);
                break;
            }

            case IR_OP_RETURN:
            {
                stack_vm_generator_emit_value(generator, iri->arg1);
                stack_vm_generator_emit_op(generator, SVMI_RETURN);
                break;
            }

            case IR_OP_JMP:
            {
                assert(false);
                break;
            }

            case IR_OP_JMP_IF:
            {
                assert(false);
                break;
            }

            case IR_OP_ALLOCL:
            {
                assert(false);
                break;
            }

            case IR_OP_STOREL:
            {
                assert(false);
                break;
            }

            case IR_OP_LOADL:
            {
                assert(false);
                break;
            }

            case IR_OP_STOREA:
            {
                stack_vm_generator_emit_value(generator, iri->arg2);
                stack_vm_generator_emit_op(generator, SVMI_STOREL_S64);
                IR_Value* argument = iri->arg1;
                stack_vm_generator_emit_s64(generator, -2 - argument->argument.index);
                break;
            }

            case IR_OP_LOADA:
            {
                stack_vm_generator_emit_op(generator, SVMI_LOADL_S64);
                IR_Value* argument = iri->arg1;
                stack_vm_generator_emit_s64(generator, -2 - argument->argument.index);
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
