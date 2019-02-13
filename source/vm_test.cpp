
#include "stack_vm.h"

#include "common.h"

#include <stdio.h>

using namespace Zodiac;

int main(int argc, char** argv)
{
    printf("vm_test()\n");

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

    // printf("Program size: %lu (%lu bytes)\n",
    //        program_size, program_size / sizeof(instructions[0]));
    auto instruction_count = program_size / sizeof(instructions[0]);

    Stack_VM vm;
    stack_vm_init(&vm, MB(1));

    stack_vm_execute_program(&vm, instructions, instruction_count);

    return 0;
}
