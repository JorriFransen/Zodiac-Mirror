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
        IRV_ALLOCL,
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

            struct
            {
                const char* name;
            } allocl;
        };
    };

    enum IR_Operator
    {
        IR_OP_NOP,

        IR_OP_ADD,
        IR_OP_SUB,
        IR_OP_MUL,
        IR_OP_LT,
        IR_OP_LTEQ,

        IR_OP_PUSH_CALL_ARG,
        IR_OP_CALL,
        IR_OP_RETURN,

        IR_OP_JMP,
        IR_OP_JMP_IF,

        IR_OP_ALLOCL,
        IR_OP_STOREL,
        IR_OP_LOADL,

        IR_OP_STOREA,
        IR_OP_LOADA,
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

    struct _IR_Value_To_AST_Decl_Map_Entry_
    {
        IR_Value* ir_value = nullptr;
        AST_Declaration* declaration = nullptr;
    };

#define ir_builder_push_value_and_decl(ir_builder, value, decl)  \
    { _IR_Value_To_AST_Decl_Map_Entry_ entry = {value, decl};    \
      BUF_PUSH(ir_builder->value_to_decl_map, entry); }

    struct IR_Builder
    {
        Arena arena = {};

        BUF(_IR_Value_To_AST_Decl_Map_Entry_) value_to_decl_map = nullptr;

        BUF(IR_Function*) functions = nullptr;
        IR_Function* current_function = nullptr;

        IR_Block* insert_block = nullptr;

        bool expect_arg_or_call = false;
    };

    struct IR_Validation_Result
    {
        BUF(char*) messages = nullptr;
    };

    void ir_builder_init(IR_Builder* ir_builder);

    void ir_builder_emit_module(IR_Builder* ir_builder, AST_Module* module);
    void ir_builder_emit_statement(IR_Builder* ir_builder, AST_Statement* statement);
    IR_Value* ir_builder_emit_expression(IR_Builder* ir_builder, AST_Expression* expression);

    IR_Value* ir_builder_value_for_declaration(IR_Builder* ir_builder, AST_Declaration* declaration);

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
    IR_Value* ir_builder_emit_mul(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_emit_lt(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_emit_lteq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    void ir_builder_emit_return(IR_Builder* ir_builder, IR_Value* ret_val);
    void ir_builder_emit_call_arg(IR_Builder* ir_builder, IR_Value* arg_value);
    IR_Value* ir_builder_emit_call(IR_Builder* ir_builder, IR_Value* func_value);
    void ir_builder_emit_jmp(IR_Builder* ir_builder, IR_Value* block_value);
    void ir_builder_emit_jmp_if(IR_Builder* ir_builder, IR_Value* cond_value, IR_Value* block_value);
    IR_Value* ir_builder_emit_allocl(IR_Builder* ir_builder, AST_Type* type, const char* name);
    void ir_builder_emit_storel(IR_Builder* ir_builder, IR_Value* allocl_value, IR_Value* new_value);
    IR_Value* ir_builder_emit_loadl(IR_Builder* ir_builder, IR_Value* allocl_value);
    IR_Value* ir_builder_emit_loada(IR_Builder* ir_builder, IR_Value* alloca_value);

    IR_Value* ir_integer_literal(IR_Builder* ir_builder, AST_Type* type, uint64_t s64);

    IR_Function* ir_function_new(IR_Builder* ir_builder, const char* name, AST_Type* return_type);

    IR_Value* ir_value_new(IR_Builder* ir_builder, IR_Value_Kind kind, AST_Type* type);
    IR_Value* ir_value_function_new(IR_Builder* ir_builder, IR_Function* function);
    IR_Value* ir_value_block_new(IR_Builder* ir_builder, IR_Block* block);
    IR_Value* ir_value_allocl_new(IR_Builder* ir_builder, AST_Type* type, const char* name);

    IR_Instruction* ir_instruction_new(IR_Builder* ir_builder, IR_Operator op,
                                       IR_Value* arg1, IR_Value* arg2, IR_Value* result);

    bool ir_instruction_is_terminator(IR_Operator op);

    IR_Validation_Result ir_validate(IR_Builder* ir_builder);
    bool ir_validate_function(IR_Function* ir_function, IR_Validation_Result* valres);
    bool ir_validate_block(IR_Block* ir_block, IR_Validation_Result* valres);
    void ir_report_validation_error(IR_Validation_Result* valres, const char* format, ...);

    void ir_builder_print_functions(IR_Builder* ir_builder);
    void ir_print_function(IR_Function* function);
    void ir_print_block(IR_Block* block);
    void ir_print_instruction(IR_Instruction* instruction);
    void ir_print_value(IR_Value* value);
}
