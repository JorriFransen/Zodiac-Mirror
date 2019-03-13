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

        vm->fp = 0;
    }

    void stack_vm_execute_program(Stack_VM* vm, uint64_t* instructions, uint64_t instruction_count)
    {
        assert(vm);
        assert(instructions);
        assert(instruction_count);

        vm->ip = 0;
        vm->sp = 0;
        vm->fp = 0;

        vm->instructions = instructions;
        vm->instruction_count = instruction_count;

        vm->running = true;

        while (vm->ip < vm->instruction_count &&
               vm->running)
        {
            auto instruction = stack_vm_fetch_instruction(vm);
            stack_vm_execute(vm, instruction);
        }

        if (vm->sp > 0)
        {
            int64_t value = stack_vm_pop(vm);
            printf("entry point returned: %ld\n", value);
        }
    }

    void stack_vm_print_program(uint64_t* instructions, uint64_t instruction_count)
    {
        assert(instructions);
        assert(instruction_count > 0);

        uint64_t ip = 0;
        while (ip < instruction_count)
        {
            Stack_VM_Instruction instruction = (Stack_VM_Instruction)instructions[ip];
            printf("%03lu: ", ip);
            ip++;
            switch (instruction)
            {
                case SVMI_INVALID:
                {
                    printf("SVMI_INVALID\n");
                    break;
                }

                case SVMI_NOP:
                {
                    printf("SVMI_NOP_S64\n");
                    break;
                }

                case SVMI_PUSH_S64:
                {
                    int64_t value = (int64_t)instructions[ip++];
                    printf("SVMI_PUSH_S64 %ld\n", value);
                    break;
                }

                case SVMI_POP_S64:
                {
                    printf("SVMI_POP_S64\n");
                    break;
                }

                case SVMI_ALLOCL:
                {
                    int64_t int_count = instructions[ip++];
                    printf("SVMI_ALLOCL %lu\n", int_count);
                    break;
                }

                case SVMI_LOADL_S64:
                {
                    int64_t offset = instructions[ip++];
                    printf("SVMI_LOADL_S64 %ld\n", offset);
                    break;
                }

                case SVMI_STOREL_S64:
                {
                    int64_t offset = instructions[ip++];
                    printf("SVMI_STOREL_S64 %ld\n", offset);
                    break;
                }

                case SVMI_ADD_S64:
                {
                    printf("SVMI_ADD_S64\n");
                    break;
                }

                case SVMI_SUB_S64:
                {
                    printf("SVMI_SUB_S64\n");
                    break;
                }

                case SVMI_MUL_S64:
                {
                    printf("SVMI_MUL_S64\n");
                    break;
                }

                case SVMI_DIV_S64:
                {
                    printf("SVMI_DIV_S64\n");
                    break;
                }

                case SVMI_CALL_IMM:
                {
                    uint64_t addr = instructions[ip++];
                    uint64_t num_args = instructions[ip++];
                    printf("SVMI_CALL_IMM %lu, %lu\n", addr, num_args);
                    break;
                }

                case SVMI_RETURN:
                {
                    printf("SVMI_RETURN\n");
                    break;
                }

                case SVMI_JMP_IMM:
                {
                    uint64_t addr = instructions[ip++];
                    printf("SVMI_JMP_IMM %lu\n", addr);
                    break;
                }

                case SVMI_JMP_COND:
                {
                    uint64_t addr = instructions[ip++];
                    printf("SVMI_JMP_COND %lu\n", addr);
                    break;
                }

                case SVMI_LT_S64:
                {
                    printf("SVMI_LT_S64\n");
                    break;
                }

                case SVMI_LTEQ_S64:
                {
                    printf("SVMI_LTEQ_S64\n");
                    break;
                }

                case SVMI_GT_S64:
                {
                    printf("SVMI_GT_S64\n");
                    break;
                }

                case SVMI_EQ_S64:
                {
                    printf("SVMI_EQ_S64\n");
                    break;
                }

                case SVMI_NEQ_S64:
                {
                    printf("SVMI_NEQ_S64\n");
                    break;
                }

                case SVMI_NOT_BOOL:
                {
                    printf("SVMI_NOT_BOOL\n");
                    break;
                }

                case SVMI_DUP_64:
                {
                    printf("SVMI_DUP_S64\n");
                    break;
                }

                case SVMI_SWP_64:
                {
                    printf("SVMI_SWP_S64\n");
                    break;
                }

                case SVMI_HALT:
                {
                    printf("SVMI_HALT\n");
                    break;
                }

                case SVMI_PRINT_S64:
                {
                    printf("SVMI_PRINT_S64\n");
                    break;
                }

                case SVMI_COUNT:
                {
                    printf("SVMI_COUNT %lu", (uint64_t)SVMI_COUNT);
                    break;
                }

                default: assert(false);

            }
        }

        printf("\n");
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

    uint64_t stack_vm_fetch_u64(Stack_VM* vm)
    {
        assert(vm);
        assert(vm->ip < vm->instruction_count);

        uint64_t result = vm->instructions[vm->ip];
        vm->ip++;
        return result;
    }

    void stack_vm_execute(Stack_VM* vm, Stack_VM_Instruction instruction)
    {
        assert(vm);

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
                stack_vm_pop(vm);
                break;
            }

            case SVMI_ALLOCL:
            {
                uint64_t local_int_count = stack_vm_fetch_s64(vm);
                for (uint64_t i = 0; i < local_int_count; i++)
                {
                    stack_vm_push(vm, 0);
                }
                break;
            };

            case SVMI_LOADL_S64:
            {
                int64_t local_offset = stack_vm_fetch_s64(vm) * sizeof(uint64_t);
                uint64_t value = vm->stack[vm->fp + local_offset];
                stack_vm_push(vm, value);
                break;
            }

            case SVMI_STOREL_S64:
            {
                int64_t local_offset = stack_vm_fetch_s64(vm) * sizeof(uint64_t);
                uint64_t value = stack_vm_pop(vm);
                *(uint64_t*)&vm->stack[vm->fp + local_offset] = value;
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

            case SVMI_MUL_S64: {
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
                uint64_t target_address = stack_vm_fetch_u64(vm);
                uint64_t num_args = stack_vm_fetch_u64(vm);

                stack_vm_push(vm, num_args);
                auto new_fp = vm->sp;
                stack_vm_push(vm, vm->fp);
                vm->fp = new_fp;
                uint64_t return_address = vm->ip;
                vm->ip = target_address;
                stack_vm_push(vm, return_address);
                break;
            }

            case SVMI_RETURN:
            {
                uint64_t return_value = stack_vm_pop(vm);
                uint64_t return_address = *(vm->stack + vm->fp + (1*sizeof(uint64_t)));
                uint64_t previous_fp = *(uint64_t*)(vm->stack + vm->fp);
                uint64_t num_args = *(vm->stack + vm->fp - (1*sizeof(uint64_t)));

                vm->sp = vm->fp - (1*sizeof(uint64_t));

                for (uint64_t i = 0; i < num_args; i++)
                {
                    stack_vm_pop(vm);
                }

                vm->fp = previous_fp;
                vm->ip = return_address;
                stack_vm_push(vm, return_value);
                break;
            }

            case SVMI_JMP_IMM:
            {
                uint64_t target_addr = stack_vm_fetch_u64(vm);
                assert(target_addr < vm->instruction_count);
                vm->ip = target_addr;
                break;
            }

            case SVMI_JMP_COND:
            {
                uint64_t target_addr = stack_vm_fetch_u64(vm);
                bool cond_value = (bool)stack_vm_pop(vm);
                if (cond_value)
                {
                    vm->ip = target_addr;
                }
                break;
            }

            case SVMI_LT_S64:
            {
                uint64_t rhs = stack_vm_pop(vm);
                uint64_t lhs = stack_vm_pop(vm);
                bool result = lhs < rhs;
                stack_vm_push(vm, result);
                break;
            }

            case SVMI_LTEQ_S64:
            {
                uint64_t rhs = stack_vm_pop(vm);
                uint64_t lhs = stack_vm_pop(vm);
                bool result = lhs <= rhs;
                stack_vm_push(vm, result);
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
                uint64_t u64_value = stack_vm_pop(vm);
                bool bool_value = (bool)u64_value;
                bool result = !bool_value;
                stack_vm_push(vm, result);
                break;
            }

            case SVMI_DUP_64:
            {
                auto value = stack_vm_pop(vm);
                stack_vm_push(vm, value);
                stack_vm_push(vm, value);
                break;
            }

            case SVMI_SWP_64:
            {
                auto a = stack_vm_pop(vm);
                auto b = stack_vm_pop(vm);
                stack_vm_push(vm, a);
                stack_vm_push(vm, b);
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
