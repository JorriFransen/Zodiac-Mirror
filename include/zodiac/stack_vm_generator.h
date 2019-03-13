#pragma once

#include "stack_vm.h"
#include "ir.h"

namespace Zodiac
{
    struct Stack_VM_Gen_Result
    {
        BUF(uint64_t) instructions = nullptr;
    };

    struct Stack_VM_Func_Gen_Data
    {
        IR_Function* function = nullptr;
        uint64_t address = 0;
    };

    struct Stack_VM_Generator
    {
        uint64_t entry_address = 0;
        bool found_entry = false;

        BUF(Stack_VM_Func_Gen_Data) func_gen_data = nullptr;

        Stack_VM_Gen_Result result = {};
    };

    void stack_vm_generator_init(Stack_VM_Generator* generator);

    uint64_t stack_vm_generator_get_func_address(Stack_VM_Generator* generator, IR_Function* func);
    Stack_VM_Func_Gen_Data* stack_vm_generator_get_gen_data(Stack_VM_Generator* generator,
                                                            IR_Function* func);
    Stack_VM_Func_Gen_Data* stack_vm_generator_create_gen_data(Stack_VM_Generator* generator,
                                                               IR_Function* func);

    void stack_vm_generator_emit_module(Stack_VM_Generator* generator, IR_Module* ir_module);
    void stack_vm_generator_emit_function(Stack_VM_Generator* generator, IR_Function* ir_function);
    void stack_vm_generator_emit_block(Stack_VM_Generator* generator, IR_Block* ir_block);
    void stack_vm_generator_emit_instruction(Stack_VM_Generator* generator, IR_Instruction* iri);

    void stack_vm_generator_emit_value(Stack_VM_Generator* generator, IR_Value* ir_value);
    void stack_vm_generator_emit_op(Stack_VM_Generator* generator, Stack_VM_Instruction op);
    void stack_vm_generator_emit_address(Stack_VM_Generator* generator, uint64_t address);
    void stack_vm_generator_emit_s64(Stack_VM_Generator* generator, int64_t s64);
    void stack_vm_generator_emit_u64(Stack_VM_Generator* generator, uint64_t u64);
}
