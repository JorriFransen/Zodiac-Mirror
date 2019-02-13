
#include "stack_vm.h"

#include "common.h"

#include <stdio.h>

using namespace Zodiac;

int main(int argc, char** argv)
{
    printf("vm_test()\n");

    Stack_VM vm;
    stack_vm_init(&vm, MB(1));

    {
        uint64_t instructions[] =
        {
            SVMI_PUSH_S64, 2,
            SVMI_PUSH_S64, 4,
            SVMI_ADD_S64,
            SVMI_DUP_64,
            SVMI_PRINT_S64,
            SVMI_PUSH_S64, 6,
            SVMI_MUL_S64,
            SVMI_DUP_64,
            SVMI_PRINT_S64,
            SVMI_PUSH_S64, 1,
            SVMI_SUB_S64,
            SVMI_DUP_64,
            SVMI_PRINT_S64,
            SVMI_PUSH_S64, 5,
            SVMI_DIV_S64,
            SVMI_PRINT_S64,
            SVMI_HALT,
        };

        auto program_size = sizeof(instructions);
        auto instruction_count = program_size / sizeof(instructions[0]);
        stack_vm_execute_program(&vm, instructions, instruction_count);
    }

    printf("\n");

    {

        // Arg N
        // Arg N -1
        // Arg N -2
        // Arg 0
        // Num args
        // FP
        // Return address
        // Locals

        uint64_t instructions[] =
        {
            SVMI_JMP_IMM, 8,

            // Add function
            SVMI_LOADL_S64, (uint64_t)-2, // Load x
            SVMI_LOADL_S64, (uint64_t)-3, // Load y

            // The  stack should look like this now
            // y
            // x
            // num args
            // fp
            // return address
            // x
            // y <- sp
            SVMI_ADD_S64,
            SVMI_RETURN,

            // Program entry
            SVMI_PUSH_S64, 9,
            SVMI_PUSH_S64, 88,
            SVMI_CALL_IMM, 2, 2,
            SVMI_PRINT_S64,
        };

        auto program_size = sizeof(instructions);
        auto instruction_count = program_size / sizeof(instructions[0]);
        stack_vm_execute_program(&vm, instructions, instruction_count);
    }

    printf("\n");

    {
        uint64_t instructions[] =
        {
            SVMI_JMP_IMM, 31,

            SVMI_LOADL_S64, (uint64_t)-2,                 // Load x
            SVMI_PUSH_S64, 2,                             // constant 2
            SVMI_LT_S64,                                  // if x < 2
            SVMI_NOT_BOOL,
            SVMI_JMP_COND, 13,                            // Jump to else

            // then
            SVMI_LOADL_S64, (uint64_t)-2,                 // Load x
            SVMI_RETURN,                                  // return x

            // else
            SVMI_LOADL_S64, (uint64_t)-2,                 // Load x
            SVMI_DUP_64,
            SVMI_PUSH_S64, 1,
            SVMI_SUB_S64,
            SVMI_CALL_IMM, 2, 1,
            SVMI_SWP_64,
            SVMI_PUSH_S64, 2,
            SVMI_SUB_S64,
            SVMI_CALL_IMM, 2, 1,
            SVMI_ADD_S64,
            SVMI_RETURN,

            SVMI_PUSH_S64, 30,
            SVMI_CALL_IMM, 2, 1,
            SVMI_PRINT_S64,
        };

        auto program_size = sizeof(instructions);
        auto instruction_count = program_size / sizeof(instructions[0]);
        stack_vm_execute_program(&vm, instructions, instruction_count);
    }
    return 0;
}
