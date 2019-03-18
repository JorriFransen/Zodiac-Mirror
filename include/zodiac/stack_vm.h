#pragma once

#include "common.h"

#include <dyncall/dyncall.h>

#include <stdint.h>

namespace Zodiac
{
    enum Stack_VM_Instruction : uint64_t
    {
        SVMI_INVALID,

        SVMI_NOP,

        SVMI_PUSH_S64,
        SVMI_POP_S64,

        SVMI_ALLOCL,

        SVMI_LOADL_S64,
        SVMI_STOREL_S64,

        SVMI_ADD_S64,
        SVMI_SUB_S64,
        SVMI_MUL_S64,
        SVMI_DIV_S64,

        SVMI_CALL_IMM,
        SVMI_CALL_EX,
        SVMI_RETURN,

        SVMI_JMP_IMM,
        SVMI_JMP_COND,

        SVMI_LT_S64,
        SVMI_LTEQ_S64,
        SVMI_GT_S64,
        SVMI_EQ_S64,
        SVMI_NEQ_S64,

        SVMI_NOT_BOOL,

        SVMI_DUP_64,
        SVMI_SWP_64,

        SVMI_STRING_TABLE,
        SVMI_FOREIGN_TABLE,

        SVMI_LOAD_DYN_LIB,

        SVMI_HALT,
        SVMI_PRINT_S64,

        SVMI_COUNT,
    };

    struct Stack_VM
    {
        uint64_t ip = 0;
        uint64_t sp = 0;
        uint64_t fp = 0;

        uint64_t* instructions = nullptr;
        uint64_t instruction_count = 0;

        uint8_t* stack = nullptr;
        uint64_t stack_size = 0;

        bool running = false;

        DCCallVM* dyn_vm = nullptr;
        BUF(DCpointer) foreign_functions = nullptr;
    };

    void stack_vm_init(Stack_VM* vm, uint64_t stack_byte_size);
    void stack_vm_execute_program(Stack_VM* vm, uint64_t* instructions, uint64_t instruction_count);

    void stack_vm_print_program(uint64_t* instructions, uint64_t instruction_count);

    Stack_VM_Instruction stack_vm_fetch_instruction(Stack_VM* vm);
    int64_t stack_vm_fetch_s64(Stack_VM* vm);
    uint64_t stack_vm_fetch_u64(Stack_VM* vm);
    void stack_vm_execute(Stack_VM* vm, Stack_VM_Instruction instruction);

    void stack_vm_call_foreign(Stack_VM* vm, uint64_t foreign_index, uint64_t num_args);

    void stack_vm_push(Stack_VM* vm, uint64_t value);
    uint64_t stack_vm_pop(Stack_VM* vm);
}
