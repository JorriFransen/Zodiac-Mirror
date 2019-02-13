#include "stack_vm.h"

#include "common.h"

#include <assert.h>

namespace Zodiac
{
    void stack_vm_init(Stack_VM* vm, uint64_t stack_byte_size)
    {
        assert(vm);
        assert(stack_byte_size);

        vm->ip = 0;
        vm->instructions = nullptr;
        vm->instruction_count = 0;

        vm->sp = 0;
        vm->stack = (uint8_t*)mem_alloc(stack_byte_size);
        vm->stack_size = stack_byte_size;
    }

    void stack_vm_execute_program(Stack_VM* vm, uint64_t* instructions, uint64_t instruction_count)
    {
        assert(vm);
        assert(instructions);
        assert(instruction_count);

        vm->ip = 0;
        vm->sp = 0;

        vm->instructions = instructions;
        vm->instruction_count = instruction_count;

        vm->running = true;

        while (vm->ip < vm->instruction_count &&
               vm->running)
        {
            auto instruction = stack_vm_fetch_instruction(vm);
            stack_vm_execute(vm, instruction);
        }
    }

    Stack_VM_Instruction stack_vm_fetch_instruction(Stack_VM* vm)
    {
        assert(vm);
        assert(vm->ip < vm->instruction_count);

        Stack_VM_Instruction instruction = (Stack_VM_Instruction)vm->instructions[vm->ip];
        assert(instruction >= SVMI_INVALID && instruction <= SVMI_COUNT);

        vm->ip++;

        return instruction;
    }

    int64_t stack_vm_fetch_s64(Stack_VM* vm)
    {
        assert(vm);
        assert(vm->ip < vm->instruction_count);

        uint64_t u64 = vm->instructions[vm->ip];
        vm->ip++;
        return (int64_t)u64;
    }

    void stack_vm_execute(Stack_VM* vm, Stack_VM_Instruction instruction)
    {
        assert(vm);
        assert(instruction);

        switch (instruction)
        {
            case SVMI_PUSH_S64:
            {
                int64_t s64 = stack_vm_fetch_s64(vm);
                stack_vm_push(vm, s64);
                break;
            }

            case SVMI_POP_S64:
            {
                assert(false);
                break;
            }

            case SVMI_ADD_S64:
            {
                int64_t rhs = stack_vm_pop(vm);
                int64_t lhs = stack_vm_pop(vm);
                int64_t result = lhs + rhs;
                stack_vm_push(vm, result);
                break;
            }

            case SVMI_SUB_S64:
            {
                int64_t rhs = stack_vm_pop(vm);
                int64_t lhs = stack_vm_pop(vm);
                int64_t result = lhs - rhs;
                stack_vm_push(vm, result);
                break;
            }

            case SVMI_MUL_S64:
            {
                int64_t rhs = stack_vm_pop(vm);
                int64_t lhs = stack_vm_pop(vm);
                int64_t result = lhs * rhs;
                stack_vm_push(vm, result);
                break;
            }

            case SVMI_DIV_S64:
            {
                int64_t rhs = stack_vm_pop(vm);
                int64_t lhs = stack_vm_pop(vm);
                int64_t result = lhs / rhs;
                stack_vm_push(vm, result);
                break;
            }

            case SVMI_CALL_IMM:
            {
                assert(false);
                break;
            }

            case SVMI_RETURN:
            {
                assert(false);
                break;
            }

            case SVMI_JMP_IMM:
            {
                assert(false);
                break;
            }

            case SVMI_JMP_COND:
            {
                assert(false);
                break;
            }

            case SVMI_LT_S64:
            {
                assert(false);
                break;
            }

            case SVMI_GT_S64:
            {
                assert(false);
                break;
            }

            case SVMI_EQ_S64:
            {
                assert(false);
                break;
            }

            case SVMI_NEQ_S64:
            {
                assert(false);
                break;
            }

            case SVMI_NOT_BOOL:
            {
                assert(false);
                break;
            }

            case SVMI_DUP_64:
            {
                auto value = stack_vm_pop(vm);
                stack_vm_push(vm, value);
                stack_vm_push(vm, value);
                break;
            }

            case SVMI_HALT:
            {
                vm->running = false;
                break;
            }

            case SVMI_PRINT_S64:
            {
                int64_t s64 = stack_vm_pop(vm);
                printf("%ld\n", s64);
                break;
            }

            default: assert(false);
        }
    }

    void stack_vm_push(Stack_VM* vm, uint64_t value)
    {
        assert(vm);

        uint64_t value_size = sizeof(value);
        assert(vm->sp + value_size <= vm->stack_size);

        uint64_t* target = (uint64_t*)&vm->stack[vm->sp];

        *target = value;
        vm->sp += value_size;
    }

    uint64_t stack_vm_pop(Stack_VM* vm)
    {
        assert(vm);

        uint64_t value_size = sizeof(uint64_t);
        assert(vm->sp >= value_size);

        vm->sp -= value_size;
        uint64_t* target = (uint64_t*)&vm->stack[vm->sp];

        return *target;
    }
}
