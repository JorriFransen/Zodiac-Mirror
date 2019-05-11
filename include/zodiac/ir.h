#pragma once

#include "ast.h"

namespace Zodiac
{

    struct IR_Function;
    struct IR_Block;

    enum IR_Value_Kind
    {
        IRV_TEMPORARY,
        IRV_ARGUMENT,
        IRV_FUNCTION,
        IRV_BLOCK,
        IRV_ALLOCL,
        IRV_GLOBAL,

        IRV_BOOL_LITERAL,
		IRV_NULL_LITERAL,
        IRV_STRING_LITERAL,
        IRV_INT_LITERAL,
        IRV_FLOAT_LITERAL,
        IRV_CHAR_LITERAL,
        IRV_ARRAY_LITERAL,
    };

    struct IR_Value
    {
        IR_Value_Kind kind;
        AST_Type* type = nullptr;

        bool assigned = false;

        union
        {
            int64_t s64;

            uint8_t u8;
            uint16_t u16;
            uint32_t u32;
            uint64_t u64;

            float r32;
            double r64;

            bool boolean;

            uint8_t* string;
            void* static_array;
            void* struct_pointer;
        } value;

        union
        {
            struct
            {
                uint64_t index;
            } temp;

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
                uint64_t index;
            } allocl;

            struct
            {
                const char* name;
                uint64_t index;
            } global;
        };
    };

    enum IR_Operator
    {
        IR_OP_NOP,

        IR_OP_ADD,
        IR_OP_SUB,
        IR_OP_MUL,
        IR_OP_DIV,
        IR_OP_LT,
        IR_OP_LTEQ,
        IR_OP_GT,
        IR_OP_GTEQ,
        IR_OP_EQ,
        IR_OP_NEQ,
        IR_OP_AND_AND,
        IR_OP_NOT,

        IR_OP_PUSH_CALL_ARG,
        IR_OP_CALL,
        IR_OP_PUSH_EX_CALL_ARG,
        IR_OP_CALL_EX,
		IR_OP_ADDROF_FOREIGN,
		IR_OP_CALL_PTR,
        IR_OP_RETURN,

        IR_OP_SUBSCRIPT,

        IR_OP_JMP,
        IR_OP_JMP_IF,

        IR_OP_ALLOCL,
        IR_OP_STOREL,
        IR_OP_LOADL,

        IR_OP_STOREA,
        IR_OP_LOADA,

        IR_OP_STOREP,
        IR_OP_LOADP,

        IR_OP_STOREG,
        IR_OP_LOADG,

        IR_OP_LOAD_LIT,

        IR_OP_ADDROF,
        IR_OP_DEREF,
        IR_OP_ARRAY_OFFSET_POINTER,
        IR_OP_AGGREGATE_OFFSET_POINTER,

		IR_OP_CAST,
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
        Atom name = {};
        IR_Instruction* first_instruction = nullptr;
        IR_Instruction* last_instruction = nullptr;

        IR_Block* previous = nullptr;
        IR_Block* next = nullptr;
    };

    typedef uint64_t _IR_Function_Flags_;
    enum IR_Function_Flags : _IR_Function_Flags_
    {
        IR_FUNC_FLAG_NONE    = 0x00,
        IR_FUNC_FLAG_FOREIGN = 0x01,
        IR_FUNC_FLAG_VARARG  = 0x02,
    };

    struct IR_Function
    {
        _IR_Function_Flags_ flags = IR_FUNC_FLAG_NONE;

        const char* name = nullptr;
        AST_Type* return_type = nullptr;

        IR_Block* first_block = nullptr;
        IR_Block* last_block = nullptr;

        BUF(IR_Value*) arguments = nullptr; // These are duplicated in local temps
        BUF(IR_Value*) local_temps = nullptr;

        bool is_entry = false;
        uint64_t foreign_index = 0;

        uint64_t next_duplicate_name_index = 1;
    };

    struct IR_Module
    {
        BUF(IR_Function*) functions = nullptr;
        IR_Function* entry_function = nullptr;

        BUF(IR_Value*) global_constants = nullptr;
        BUF(IR_Value*) string_literal_values = nullptr;
        Arena string_literal_arena = {};

        BUF(Atom) dynamic_lib_names = nullptr;

        uint64_t error_count = 0;
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
        Context* context = nullptr;
        AST_Module* ast_module = nullptr;
        Arena arena = {};

        BUF(_IR_Value_To_AST_Decl_Map_Entry_) value_to_decl_map = nullptr;

        IR_Module result = {};
        IR_Function* current_function = nullptr;

        IR_Block* insert_block = nullptr;
    };

    struct IR_Validation_Result
    {
        BUF(char*) messages = nullptr;
    };

    void ir_builder_init(IR_Builder* ir_builder, Context* context);

    IR_Module ir_builder_emit_module(IR_Builder* ir_builder, AST_Module* module);
    void ir_builder_emit_global_declaration(IR_Builder* ir_builder, AST_Declaration* global_decl);
    void ir_builder_emit_statement(IR_Builder* ir_builder, AST_Statement* statement,
                                   IR_Value* break_block);
    void ir_builder_emit_assign_statement(IR_Builder* ir_builder, AST_Statement* statement);
	void ir_builder_emit_switch_statement(IR_Builder* ir_builder, AST_Statement* statement,
                                          IR_Value* break_block);
    IR_Value* ir_builder_emit_expression(IR_Builder* ir_builder, AST_Expression* expression);
    IR_Value* ir_builder_emit_dot_expression(IR_Builder* ir_builder, AST_Expression* expression);
	IR_Value* ir_builder_emit_cast_expression(IR_Builder* ir_builder, AST_Expression* expression);
    IR_Value* ir_builder_emit_load_lit(IR_Builder* ir_builder, IR_Value* literal);
    IR_Value* ir_builder_emit_negate(IR_Builder* ir_builder, AST_Expression* expression);
    IR_Value* ir_builder_emit_addrof(IR_Builder* ir_builder, AST_Expression* expression);
	IR_Value* ir_builder_emit_addrof_foreign(IR_Builder* ir_builder, IR_Value* foreign_func,
		AST_Type* foreign_type);
    IR_Value* ir_builder_emit_deref(IR_Builder* ir_builder, AST_Expression* expression);
    IR_Value* ir_builder_emit_not(IR_Builder* ir_builder, AST_Expression* expression);

    IR_Value* ir_builder_value_for_declaration(IR_Builder* ir_builder, AST_Declaration* declaration);

    IR_Value* ir_builder_begin_function(IR_Builder* ir_builder, const char* name, AST_Type* return_type);
    void ir_builder_end_function(IR_Builder* ir_builder, IR_Value* func_value);
    void ir_builder_patch_empty_block_jumps(IR_Builder* ir_builder, IR_Function* function);
    void ir_builder_patch_block_jumps(IR_Builder* ir_builder, IR_Function* function,
                                     IR_Block* orig_block, IR_Block* target_block);
    IR_Value* ir_builder_create_block(IR_Builder* ir_builder, const char* name,
                                      IR_Function* function = nullptr);
    IR_Value* ir_builder_create_block(IR_Builder* ir_builder, const char* name, IR_Value* function);

    void ir_builder_append_block(IR_Builder* ir_builder, IR_Function* function, IR_Block* block);
    void ir_builder_set_insert_block(IR_Builder* ir_builder, IR_Block* block);
    void ir_builder_set_insert_block(IR_Builder* ir_builder, IR_Value* block_value);

    IR_Value* ir_builder_emit_array_offset_pointer(IR_Builder* ir_builder, IR_Value* array_allocl,
                                                   uint64_t offset);
    IR_Value* ir_builder_emit_array_offset_pointer(IR_Builder* ir_builder, IR_Value* array_allocl,
                                                   IR_Value* offset_value);
    IR_Value* ir_builder_emit_aggregate_offset_pointer(IR_Builder* ir_builder, IR_Value* struct_value,
                                                       uint64_t offset);

    void ir_builder_emit_instruction(IR_Builder* ir_builder, IR_Instruction* iri);

    IR_Value* ir_builder_emit_function_arg(IR_Builder* ir_builder, const char* name, AST_Type* type);
    IR_Value* ir_builder_emit_add(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_emit_sub(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_emit_mul(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_emit_div(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_emit_lt(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_emit_lteq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_emit_gt(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_emit_gteq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_emit_eq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_emit_neq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    IR_Value* ir_builder_emit_and_and(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs);
    void ir_builder_emit_return(IR_Builder* ir_builder, IR_Value* ret_val);
    void ir_builder_emit_call_arg(IR_Builder* ir_builder, IR_Value* arg_value, bool is_vararg = false,
                                  bool is_foreign = false);
    IR_Value* ir_builder_emit_call(IR_Builder* ir_builder, IR_Value* func_value, IR_Value* num_args);
    IR_Value* ir_builder_emit_subscript(IR_Builder* ir_builder, IR_Value* base_value, IR_Value* index_value);
    void ir_builder_emit_jmp(IR_Builder* ir_builder, IR_Value* block_value);
    void ir_builder_emit_jmp_if(IR_Builder* ir_builder, IR_Value* cond_value, IR_Value* block_value);
    IR_Value* ir_builder_emit_allocl(IR_Builder* ir_builder, AST_Type* type, const char* name);
    void ir_builder_emit_storel(IR_Builder* ir_builder, IR_Value* allocl_value, IR_Value* new_value);
    IR_Value* ir_builder_emit_loadl(IR_Builder* ir_builder, IR_Value* allocl_value);
    void ir_builder_emit_storea(IR_Builder* ir_builder, IR_Value* arg_value, IR_Value* new_value);
    IR_Value* ir_builder_emit_loada(IR_Builder* ir_builder, IR_Value* alloca_value);
    void ir_builder_emit_storep(IR_Builder* ir_builder, IR_Value* pointer_allocl, IR_Value* new_value);
    IR_Value* ir_builder_emit_loadp(IR_Builder* ir_builder, IR_Value* pointer, AST_Type* type);
    IR_Value* ir_builder_emit_global(IR_Builder* ir_builder, AST_Declaration* global_decl);
    void ir_builder_emit_storeg(IR_Builder* ir_builder, IR_Value* global_value, IR_Value* new_value);
    IR_Value* ir_builder_emit_loadg(IR_Builder* ir_builder, IR_Value* global_value);

	IR_Value* ir_builder_emit_load(IR_Builder* ir_builder, IR_Value* store);

    IR_Value* ir_builder_emit_zero_literal(IR_Builder* ir_builder, AST_Type* type);
    IR_Value* ir_boolean_literal(IR_Builder* ir_builder, AST_Type* type, bool value);
	IR_Value* ir_null_literal(IR_Builder* ir_builder, AST_Type* type);
    IR_Value* ir_string_literal(IR_Builder* ir_builder, AST_Type* type, Atom string);
    IR_Value* ir_integer_literal(IR_Builder* ir_builder, AST_Type* type, uint64_t s64);
    IR_Value* ir_float_literal(IR_Builder* ir_builder, AST_Type* type, double r64, float r32);
    IR_Value* ir_character_literal(IR_Builder* ir_builder, AST_Type* type, char c);
    uint64_t ir_builder_emit_foreign(IR_Builder* ir_builder, Atom atom);

    IR_Function* ir_function_new(IR_Builder* ir_builder, const char* name, AST_Type* return_type);

    IR_Value* ir_value_new(IR_Builder* ir_builder, IR_Value_Kind kind, AST_Type* type);
    IR_Value* ir_value_function_new(IR_Builder* ir_builder, IR_Function* function);
    IR_Value* ir_value_block_new(IR_Builder* ir_builder, IR_Block* block);
    IR_Value* ir_value_allocl_new(IR_Builder* ir_builder, AST_Type* type, const char* name);
    IR_Value* ir_value_global_new(IR_Builder* ir_builder, AST_Type* type, const char* name);

    IR_Instruction* ir_instruction_new(IR_Builder* ir_builder, IR_Operator op,
                                       IR_Value* arg1, IR_Value* arg2, IR_Value* result);

    bool ir_instruction_is_terminator(IR_Operator op);

    IR_Validation_Result ir_validate(IR_Builder* ir_builder);
    bool ir_validate_function(IR_Function* ir_function, IR_Validation_Result* valres);
    bool ir_validate_block(IR_Block* ir_block, IR_Validation_Result* valres);
    void ir_report_validation_error(IR_Validation_Result* valres, const char* format, ...);

    void ir_builder_print_result(IR_Builder* ir_builder);
    void ir_builder_print_functions(IR_Builder* ir_builder);
    void ir_print_function(IR_Function* function);
    void ir_print_block(IR_Block* block);
    void ir_print_instruction(IR_Instruction* instruction);
    void ir_print_value(IR_Value* value);
    void ir_print_string_literal(const char* string);
	void ir_print_type(AST_Type* type);
    void ir_print_character(char c);
}
