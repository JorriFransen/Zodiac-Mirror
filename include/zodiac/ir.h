#pragma once

#include "ast.h"

namespace Zodiac
{
    struct IR_Block;
    struct IR_Function;

    struct IR_Builder
    {
        Arena arena = {};

        BUF(IR_Function*) functions = nullptr;

        uint64_t next_temp_index = 0;
    };

    enum IR_Value_Kind
    {
        IRV_ARGUMENT,
        IRV_TEMPORARY,
        IRV_LITERAL,
        IRV_FUNC,
    };

    struct IR_Value
    {
        IR_Value_Kind kind;

        AST_Type* type;

        union
        {
            struct
            {
                uint64_t index;
                const char* name;
            } argument;

            struct
            {
                uint64_t index;
            } temp;

            struct
            {
                int64_t s64;
            } literal;

            IR_Function* ir_function;
        };
    };

    struct IR_Function
    {
        const char* name = nullptr;
        AST_Type* return_type = nullptr;

        BUF(IR_Value*) arguments = nullptr;
        BUF(IR_Block*) blocks = nullptr;
    };

    enum IR_Instruction_Kind
    {
        IRI_ADD,
        IRI_RETURN,
        IRI_CALL,
        IRI_PUSH_ARG,
    };

    struct IR_Instruction
    {
        IR_Instruction_Kind kind;

        IR_Value* lhs = nullptr;
        IR_Value* rhs = nullptr;
        IR_Value* result = nullptr;
    };

    struct IR_Block
    {
        const char* name = nullptr;
        IR_Function* function = nullptr;
        BUF(IR_Instruction) instructions = nullptr;
    };

    void ir_builder_init(IR_Builder* ir_builder);

    IR_Function* ir_builder_create_function(IR_Builder* ir_builder, const char* name,
                                            AST_Type* return_type);
    IR_Value* ir_function_add_argument(IR_Builder* ir_builder, IR_Function* func,
                                       const char* name, AST_Type* type);
    void ir_function_add_block(IR_Function* func, IR_Block* block);

    IR_Block* ir_builder_create_block(IR_Builder* ir_builder, const char* name);

    IR_Value* ir_builder_insert_add(IR_Builder* ir_builder, IR_Block* block,
                                    IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_insert_call(IR_Builder* ir_builder, IR_Block* block,
                                     IR_Function* func,
                                     IR_Value** args, uint64_t num_args);
    void ir_builder_insert_arg_push(IR_Builder* ir_builder, IR_Block* block,
                                    IR_Value* argument_value);

    IR_Value* ir_value_argument(IR_Builder* ir_builder, uint64_t arg_index,
                                const char* arg_name, AST_Type* arg_type);
    IR_Value* ir_value_temporary(IR_Builder* ir_builder, AST_Type* type);
    IR_Value* ir_value_function(IR_Builder* ir_builder, IR_Function* ir_func);

    void ir_builder_insert_return(IR_Builder* ir_builder, IR_Block* block, IR_Value* result_value);
    void ir_builder_insert_return(IR_Builder* ir_builder, IR_Block* block);

    IR_Value* ir_builder_get_literal(IR_Builder* ir_builder, AST_Type* ir_type, int64_t s64);

    void ir_builder_print(IR_Builder* ir_builder);
    void ir_builder_print_function(IR_Builder* ir_builder, IR_Function* func);
    void ir_builder_print_block(IR_Builder* ir_builder, IR_Block* block);
    void ir_builder_print_instruction(IR_Builder* ir_builder, IR_Instruction iri);
    void ir_builder_print_value(IR_Builder* ir_builder, IR_Value* value);
    void ir_builder_print_type(IR_Builder* ir_builder, AST_Type* ast_type);
}
