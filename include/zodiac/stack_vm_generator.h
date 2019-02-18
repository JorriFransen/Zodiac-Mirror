#pragma once

#include "zodiac.h"
#include "ast.h"
#include "stack_vm.h"

namespace Zodiac
{
    struct Stack_VM_Generator_Result
    {
        BUF(uint64_t) instructions = nullptr;
    };

    struct Stack_VM_Generator
    {
        Context* context = nullptr;
        AST_Module* ast_module = nullptr;

        bool done = false;
        bool progressed_on_last_cycle = false;

        bool entry_addr_pos_set = false;
        uint64_t entry_addr_pos = 0;

        bool replacements_done = false;

        Stack_VM_Generator_Result result = {};
    };

    enum Stack_VM_Address_Placeholder_Kind
    {
        SVM_ADDRESS_PLACEHOLDER_ENTRY,
    };

    enum Stack_VM_Gen_Data_Kind
    {
        SVM_GEN_DATA_FUNC,
    };

    struct Stack_VM_Gen_Data
    {
        Stack_VM_Gen_Data_Kind kind;

        union
        {
            struct
            {
                uint64_t address;
            } func;
        };
    };

    void stack_vm_generator_init(Stack_VM_Generator* generator, Context* context, AST_Module* ast_module);
    void stack_vm_generator_do_cycle(Stack_VM_Generator* generator);
    void stack_vm_generator_do_replacements(Stack_VM_Generator* generator);

    static void emit_address_placeholder(Stack_VM_Generator* generator, Stack_VM_Address_Placeholder_Kind kind);

    static void emit_declaration(Stack_VM_Generator* generator, AST_Declaration* decl);
    static void emit_function_declaration(Stack_VM_Generator* generator, AST_Declaration* decl);

    static void emit_statement(Stack_VM_Generator* generator, AST_Statement* stmt);

    static void emit_expression(Stack_VM_Generator* generator, AST_Expression* expr);
    static void emit_binary_expression(Stack_VM_Generator* generator, AST_Expression* expr);
    static void emit_identifier_expression(Stack_VM_Generator* generator, AST_Expression* expr);
    static void emit_literal_expression(Stack_VM_Generator* generator, AST_Expression* expr);

    static void emit_argument_load(Stack_VM_Generator* generator, AST_Declaration* arg_decl);

    static void emit_instruction(Stack_VM_Generator* generator, Stack_VM_Instruction instruction);
    static void emit_address(Stack_VM_Generator* generator, uint64_t address);
    static void emit_s64(Stack_VM_Generator* generator, int64_t s64);
    static void emit_u64(Stack_VM_Generator* generator, uint64_t u64);

    static Stack_VM_Gen_Data* get_gen_data(Stack_VM_Generator* generator, AST_Declaration* declaration);
}
