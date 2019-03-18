#pragma once

#include "stack_vm.h"
#include "ir.h"

namespace Zodiac
{
    struct Stack_VM_Gen_Result
    {
        BUF(uint64_t) instructions = nullptr;
    };

    enum Stack_VM_Gen_Data_Kind
    {
        SVMGD_FUNCTION,
        SVMGD_ALLOCL,
        SVMGD_BLOCK,
    };

    struct Stack_VM_Gen_Data
    {
        Stack_VM_Gen_Data_Kind kind;

        struct
        {
            IR_Function* function;
            uint64_t address = 0;
        } function;

        struct
        {
            IR_Value* allocl;
            int64_t function_local_offset;
        } allocl;

        struct
        {
            IR_Block* block;
            uint64_t address = 0;
        } block;
    };

    struct SVMG_Address_Dependency
    {
        uint64_t address_index = 0;
        IR_Value* value = nullptr;
    };

    struct Stack_VM_Generator
    {
        uint64_t entry_address = 0;
        bool found_entry = false;

        IR_Function* current_function = nullptr;

        BUF(Stack_VM_Gen_Data) gen_data = nullptr;
        BUF(SVMG_Address_Dependency) address_dependencies = nullptr;

        Stack_VM_Gen_Result result = {};
    };

    void stack_vm_generator_init(Stack_VM_Generator* generator);

    void stack_vm_generator_emit_strings(Stack_VM_Generator* generator, IR_Module* module);
    void stack_vm_generator_emit_string(Stack_VM_Generator* generator, Atom string);

    uint64_t stack_vm_generator_get_func_address(Stack_VM_Generator* generator, IR_Function* func,
                                                 bool* found);
    uint64_t stack_vm_generator_get_block_address(Stack_VM_Generator* generator, IR_Block* block,
                                                 bool* found);
    Stack_VM_Gen_Data* stack_vm_generator_get_gen_data(Stack_VM_Generator* generator,
                                                            IR_Function* func);
    Stack_VM_Gen_Data* stack_vm_generator_get_gen_data(Stack_VM_Generator* generator,
                                                       IR_Block* block);
    Stack_VM_Gen_Data* stack_vm_generator_get_gen_data(Stack_VM_Generator* generator,
                                                       IR_Value* value);
    Stack_VM_Gen_Data* stack_vm_generator_create_gen_data(Stack_VM_Generator* generator,
                                                               IR_Function* func);
    Stack_VM_Gen_Data* stack_vm_generator_create_gen_data(Stack_VM_Generator* generator,
                                                          IR_Block* block);
    Stack_VM_Gen_Data* stack_vm_generator_create_gen_data(Stack_VM_Generator* generator,
                                                          IR_Value* value);

    void stack_vm_generator_add_address_dependency(Stack_VM_Generator* generator, uint64_t address_index,
                                                   IR_Value* ir_value);
    void stack_vm_generator_satisfy_address_dependencies(Stack_VM_Generator* generator);

    void stack_vm_generator_emit_space_for_locals(Stack_VM_Generator* generator, IR_Function* func);

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
