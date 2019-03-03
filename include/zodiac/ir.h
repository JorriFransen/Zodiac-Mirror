#pragma once

#include "ast.h"

namespace Zodiac
{

    struct IR_Function;
    struct IR_Block;

    enum IR_Value_Kind
    {
        IRV_TEMPORARY,
        IRV_LITERAL,
        IRV_ARGUMENT,
        IRV_FUNCTION,
        IRV_BLOCK,
    };

    struct IR_Value
    {
        IR_Value_Kind kind;
        AST_Type* type = nullptr;

        bool assigned = false;

        union
        {
            struct
            {
                uint64_t index;
            } temp;

            struct
            {
                int64_t s64;
            } literal;

            struct
            {
                const char* name;
                uint64_t index;
            } argument;

            IR_Function* function;
            IR_Block* block;
        };
    };

    enum IR_Operator
    {
        IR_OP_NOP,

        IR_OP_ADD,
        IR_OP_SUB,

        IR_OP_RETURN,

        IR_OP_PUSH_CALL_ARG,
        IR_OP_CALL,
    };

    struct IR_Instruction
    {
        IR_Operator op = IR_OP_NOP;
        IR_Value* arg1 = nullptr;
        IR_Value* arg2 = nullptr;
        IR_Value* result = nullptr;

        IR_Instruction* next = nullptr;
    };

    struct IR_Block
    {
        const char* name = nullptr;
        IR_Instruction* first_instruction = nullptr;
        IR_Instruction* last_instruction = nullptr;

        IR_Block* next = nullptr;
    };

    struct IR_Function
    {
        const char* name = nullptr;
        AST_Type* return_type = nullptr;

        IR_Block* first_block = nullptr;
        IR_Block* last_block = nullptr;

        BUF(IR_Value*) arguments = nullptr;

        uint64_t next_temp_index = 0;
    };

    struct IR_Builder
    {
        Arena arena = {};

        BUF(IR_Function*) functions = nullptr;
        IR_Function* current_function = nullptr;

        IR_Block* insert_block = nullptr;
    };

    void ir_builder_init(IR_Builder* ir_builder);

    IR_Value* ir_builder_begin_function(IR_Builder* ir_builder, const char* name, AST_Type* return_type);
    void ir_builder_end_function(IR_Builder* ir_builder, IR_Value* func_value);
    IR_Value* ir_builder_create_block(IR_Builder* ir_builder, const char* name, IR_Value* function);

    void ir_builder_append_block(IR_Builder* ir_builder, IR_Function* function, IR_Block* block);
    void ir_builder_set_insert_block(IR_Builder* ir_builder, IR_Block* block);
    void ir_builder_set_insert_block(IR_Builder* ir_builder, IR_Value* block_value);

    void ir_builder_emit_instruction(IR_Builder* ir_builder, IR_Instruction* iri);

    IR_Value* ir_builder_emit_function_arg(IR_Builder* ir_builder, const char* name, AST_Type* type);
    IR_Value* ir_builder_emit_add(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_emit_sub(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    void ir_builder_emit_return(IR_Builder* ir_builder, IR_Value* ret_val);
    void ir_builder_emit_call_arg(IR_Builder* ir_builder, IR_Value* arg_value);
    IR_Value* ir_builder_emit_call(IR_Builder* ir_builder, IR_Value* func_value);

    IR_Value* ir_integer_literal(IR_Builder* ir_builder, AST_Type* type, uint64_t s64);

    IR_Function* ir_function_new(IR_Builder* ir_builder, const char* name, AST_Type* return_type);

    IR_Value* ir_value_new(IR_Builder* ir_builder, IR_Value_Kind kind, AST_Type* type);
    IR_Value* ir_value_function_new(IR_Builder* ir_builder, IR_Function* function);
    IR_Value* ir_value_block_new(IR_Builder* ir_builder, IR_Block* block);

    IR_Instruction* ir_instruction_new(IR_Builder* ir_builder, IR_Operator op,
                                       IR_Value* arg1, IR_Value* arg2, IR_Value* result);

    void ir_builder_print_functions(IR_Builder* ir_builder);
    void ir_print_function(IR_Function* function);
    void ir_print_block(IR_Block* block);
    void ir_print_instruction(IR_Instruction* instruction);
    void ir_print_value(IR_Value* value);
}
