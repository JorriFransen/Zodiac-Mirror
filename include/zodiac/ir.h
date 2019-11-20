#pragma once

#include "ast.h"
#include <dyncall_callback.h>

namespace Zodiac
{

    struct IR_Instruction;
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
        IRV_AGGREGATE_LITERAL,
    };

    enum IR_Value_Flags : uint64_t
    {
        IRV_FLAG_ASSIGNED = 0x01,
        IRV_FLAG_CONST    = 0x02,
    };

    struct IR_Value
    {
        IR_Value_Kind kind;
        AST_Type* type = nullptr;

        uint64_t flags = 0;

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

            void* pointer;
            BUF(IR_Value*) compound_values;
        } value;

        union
        {
            struct
            {
                uint64_t index;
                IR_Instruction* phi;
            } temp;

            struct
            {
                const char* name;
                uint64_t index;
                File_Pos file_pos;
            } argument;

            IR_Function* function;
            IR_Block* block;

            struct
            {
                const char* name;
                uint64_t index;
                File_Pos file_pos;
            } allocl;

            struct
            {
                const char* name;
                uint64_t index;
                IR_Value* init_value;
            } global;
        };
    };

    enum IR_Operator
    {
        IR_OP_NOP,

        IR_OP_ADD,
        IR_OP_SUB,
        IR_OP_MUL,
        IR_OP_MOD,
        IR_OP_DIV,
        IR_OP_LSHIFT,
        IR_OP_RSHIFT,
        IR_OP_LT,
        IR_OP_LTEQ,
        IR_OP_GT,
        IR_OP_GTEQ,
        IR_OP_EQ,
        IR_OP_NEQ,
        IR_OP_AND,
        IR_OP_OR,
        IR_OP_NOT,

        IR_OP_PUSH_CALL_ARG,
        IR_OP_CALL,
        IR_OP_CALL_EX,
		IR_OP_ADDROF_FOREIGN,
        IR_OP_ADDROF_FUNCTION,
		IR_OP_CALL_PTR,
        IR_OP_RETURN,

        IR_OP_JMP,
        IR_OP_JMP_IF,
        IR_OP_SWITCH,

        IR_OP_ALLOCL,
        IR_OP_STOREL,
        IR_OP_LOADL,

        IR_OP_STOREA,
        IR_OP_LOADA,

        IR_OP_STOREP,
        IR_OP_LOADP,

        IR_OP_STOREG,
        IR_OP_LOADG,

        IR_OP_ADDROF,
        IR_OP_DEREF,
        IR_OP_ARRAY_OFFSET_POINTER,
        IR_OP_AGGREGATE_OFFSET_POINTER,

		IR_OP_CAST,
        IR_OP_ASSERT_FAIL,

        IR_OP_PHI,
        IR_OP_GET_TYPE_INFO,

        IR_OP_CREATE_THREAD,
        IR_OP_JOIN_THREAD,
        IR_OP_COMPARE_AND_SWAP,
    };

    struct IR_Phi_Pair
    {
        IR_Block* from_block = nullptr;
        IR_Value* value_to_load = nullptr;
    };

    struct IR_Case_Pair
    {
        IR_Value* value = nullptr;
        IR_Value* dest_block_value = nullptr;
    };

    struct IR_Instruction
    {
        IR_Operator op = IR_OP_NOP;

        union
        {
            struct
            {
                IR_Value* arg1;
                IR_Value* arg2;
            };

            BUF(IR_Phi_Pair) phi_pairs;
        };

        union
        {
            BUF(IR_Case_Pair) case_pairs;
            IR_Value* result = nullptr;
        };

        File_Pos origin;
        AST_Scope* scope = nullptr;

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

    struct IR_Runner;

    struct _IR_DCB_Data
    {
        IR_Runner* runner = nullptr;
        IR_Value* func_value = nullptr;
        DCCallback* callback_address = nullptr;
    };

    struct IR_Function
    {
        _IR_Function_Flags_ flags = IR_FUNC_FLAG_NONE;
        File_Pos file_pos;

        const char* name = nullptr;
        AST_Type* type = nullptr;

        IR_Block* first_block = nullptr;
        IR_Block* last_block = nullptr;

        AST_Scope* body_scope = nullptr;

        BUF(IR_Value*) arguments = nullptr; // These are duplicated in local temps
        BUF(IR_Value*) local_temps = nullptr;

        bool is_entry = false;

        uint64_t foreign_index = 0;
        _IR_DCB_Data dcb_data = {};

        uint64_t next_duplicate_name_index = 1;
    };

    struct IR_Global_Constant
    {
        const char* name = nullptr;
        IR_Value* value = nullptr;
    };

    struct IR_Module
    {
        BUF(IR_Function*) functions = nullptr;
        IR_Function* entry_function = nullptr;

        BUF(IR_Value*) globals = nullptr;
        BUF(IR_Global_Constant) global_constants = nullptr;
        BUF(IR_Value*) string_literal_values = nullptr;
        Arena string_literal_arena = {};

        BUF(IR_Module*) imported_modules = nullptr;

        BUF(Atom) dynamic_lib_names = nullptr;

        const char* name = nullptr;
        const char* file_name = nullptr;
        const char* file_dir = nullptr;

        uint64_t error_count = 0;
    };

    struct IR_Value_And_Decl
    {
        AST_Declaration* decl = nullptr;
        IR_Value* value = nullptr;
    };

    struct IR_Builder
    {
        Context* context = nullptr;
        AST_Module* ast_module = nullptr;
        Arena arena = {};

        IR_Value_And_Decl* value_decl_hash = nullptr;
        uint64_t value_decl_count = 0;

        IR_Module result = {};
        IR_Function* current_function = nullptr;

        IR_Block* insert_block = nullptr;

        Stack<AST_Scope*> scope_stack = {};
    };

    struct IR_Validation_Result
    {
        BUF(char*) messages = nullptr;
    };

    void ir_builder_init(IR_Builder* ir_builder, Context* context);

    IR_Module ir_builder_emit_module(IR_Builder* ir_builder, AST_Module* module);
    void ir_builder_emit_missing_poly_functions(IR_Builder* ir_builder, AST_Module* module);

	void ir_builder_emit_decl_body(IR_Builder* ir_builder, AST_Declaration* func_decl);
    void ir_builder_emit_function_body(IR_Builder* ir_builder, AST_Declaration* decl);
    void ir_builder_emit_global_declaration(IR_Builder* ir_builder, AST_Declaration* global_decl);
    void ir_builder_emit_function_declaration(IR_Builder* ir_builder, AST_Declaration* decl);
    void ir_builder_emit_statement(IR_Builder* ir_builder, AST_Statement* statement,
                                   AST_Scope* scope, IR_Value* break_block);
    void ir_builder_emit_assign_statement(IR_Builder* ir_builder, AST_Statement* statement);
	void ir_builder_emit_switch_statement(IR_Builder* ir_builder, AST_Statement* statement,
                                          AST_Scope* scope, IR_Value* break_block);
    IR_Value* ir_builder_emit_expression(IR_Builder* ir_builder, AST_Expression* expression);
    IR_Value* ir_builder_emit_global_init_expression(IR_Builder* ir_builder,
                                                     AST_Expression* expression);
    IR_Value* ir_builder_emit_pointer_math(IR_Builder* ir_builder, IR_Value* pointer_value,
                                            IR_Value* int_value, AST_Binop_Kind binop,
                                            bool reversed, File_Pos origin);
    IR_Value* ir_builder_emit_dot_expression(IR_Builder* ir_builder, AST_Expression* expression);
	IR_Value* ir_builder_emit_cast_expression(IR_Builder* ir_builder, AST_Expression* expression);
    IR_Value* ir_builder_emit_negate(IR_Builder* ir_builder, AST_Expression* expression,
                                     File_Pos origin);
    IR_Value* ir_builder_emit_addrof(IR_Builder* ir_builder, AST_Expression* expression,
                                     File_Pos origin);
	IR_Value* ir_builder_emit_addrof_foreign(IR_Builder* ir_builder, IR_Value* foreign_func,
                                             AST_Type* foreign_type, File_Pos origin);
    IR_Value* ir_builder_emit_addrof_function(IR_Builder* ir_builder, IR_Value* func,
                                              AST_Type* func_type, File_Pos origin);
    IR_Value* ir_builder_emit_deref(IR_Builder* ir_builder, AST_Expression* expression,
                                    File_Pos origin);
    IR_Value* ir_builder_emit_not(IR_Builder* ir_builder, AST_Expression* expression,
                                  File_Pos origin);

    IR_Value* ir_builder_emit_create_thread(IR_Builder* ir_builder, IR_Value* func_value,
                                            IR_Value* user_data_value, File_Pos origin);
    IR_Value* ir_builder_emit_join_thread(IR_Builder* ir_builder, IR_Value* thread_value,
                                     File_Pos origin);
    IR_Value* ir_builder_emit_compare_and_swap(IR_Builder* ir_builder, IR_Value* pointer_val,
                                               IR_Value* value, IR_Value* new_value,
                                               File_Pos origin);

    void ir_builder_push_value_and_decl(IR_Builder* ir_builder, IR_Value* ir_value,
                                        AST_Declaration* decl);
    void ir_builder_grow_value_decl_hash(IR_Builder* ir_builder);
    IR_Value* ir_builder_value_for_declaration(IR_Builder* ir_builder,
                                               AST_Declaration* declaration);

    IR_Value* ir_builder_begin_function(IR_Builder* ir_builder, File_Pos file_pos,
                                        const char* name, AST_Type* return_type,
                                        AST_Scope* body_scope);
    void ir_builder_end_function(IR_Builder* ir_builder, IR_Value* func_value);
    void ir_builder_patch_empty_block_jumps(IR_Builder* ir_builder, IR_Function* function);
    void ir_builder_patch_block_jumps(IR_Builder* ir_builder, IR_Function* function,
                                     IR_Block* orig_block, IR_Block* target_block);
    IR_Value* ir_builder_create_block(IR_Builder* ir_builder, const char* name,
                                      IR_Function* function = nullptr);
    IR_Value* ir_builder_create_block(IR_Builder* ir_builder, const char* name,
                                      IR_Value* function);

    void ir_builder_append_block(IR_Builder* ir_builder, IR_Function* function, IR_Block* block);
    void ir_builder_set_insert_block(IR_Builder* ir_builder, IR_Block* block);
    void ir_builder_set_insert_block(IR_Builder* ir_builder, IR_Value* block_value);

    IR_Value* ir_builder_emit_array_offset_pointer(IR_Builder* ir_builder, IR_Value* array_allocl,
                                                   uint64_t offset, File_Pos origin);
    IR_Value* ir_builder_emit_array_offset_pointer(IR_Builder* ir_builder, IR_Value* array_allocl,
                                                   IR_Value* offset_value, File_Pos origin);
    IR_Value* ir_builder_emit_aggregate_offset_pointer(IR_Builder* ir_builder,
                                                       IR_Value* struct_value,
                                                       uint64_t offset, File_Pos origin);

    void ir_builder_emit_instruction(IR_Builder* ir_builder, IR_Instruction* iri);

    IR_Value* ir_builder_emit_function_arg(IR_Builder* ir_builder, const char* name,
                                           AST_Type* type, File_Pos file_pos);
    IR_Value* ir_builder_emit_add(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                  File_Pos origin);
    IR_Value* ir_builder_emit_sub(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                  File_Pos origin);
    IR_Value* ir_builder_emit_mul(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                  File_Pos origin);
    IR_Value* ir_builder_emit_mod(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                  File_Pos origin);
    IR_Value* ir_builder_emit_bitshift(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                       AST_Binop_Kind op, File_Pos origin);
    IR_Value* ir_builder_emit_div(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                  File_Pos origin);
    IR_Value* ir_builder_emit_lt(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                 File_Pos origin);
    IR_Value* ir_builder_emit_lteq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                   File_Pos origin);
    IR_Value* ir_builder_emit_gt(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                 File_Pos origin);
    IR_Value* ir_builder_emit_gteq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                   File_Pos origin);
    IR_Value* ir_builder_emit_eq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                 File_Pos origin);
    IR_Value* ir_builder_emit_neq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                  File_Pos origin);
    IR_Value* ir_builder_emit_cond_expr(IR_Builder* ir_builder, AST_Expression* cond_expr,
                                   File_Pos origin);
    IR_Value* ir_builder_emit_and(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                  File_Pos origin);
    IR_Value* ir_builder_emit_or(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                  File_Pos origin);
    void ir_builder_emit_return(IR_Builder* ir_builder, IR_Value* ret_val, File_Pos origin);
    void ir_builder_emit_defer_statements_before_return(IR_Builder* ir_builder, AST_Scope* scope,
                                                        File_Pos return_file_pos);
    void ir_builder_emit_defer_statements_before_break(IR_Builder* ir_builder, AST_Scope* scope,
                                                       File_Pos break_file_pos);
    void ir_builder_emit_if(IR_Builder* ir_builder, AST_Expression* cond_expr,
                            AST_Statement* then_stmt, AST_Statement* else_stmt, AST_Scope* scope,
                            IR_Value* break_block, File_Pos origin);
    void ir_builder_emit_if_cond(IR_Builder* ir_builder, AST_Expression* cond_expr,
                                 IR_Value* then_block_val, IR_Value* else_block_val,
                                 File_Pos origin);
    void ir_builder_emit_short_circuit_if_and(IR_Builder* ir_builder, AST_Expression* and_expr,
                                              File_Pos origin);
    void ir_builder_emit_call_arg(IR_Builder* ir_builder, IR_Value* arg_value,
                                  File_Pos origin, bool is_vararg = false);
    IR_Value* ir_builder_emit_call(IR_Builder* ir_builder, IR_Value* func_value,
                                   IR_Value* num_args, File_Pos origin);
    IR_Value* ir_builder_emit_call(IR_Builder* ir_builder, IR_Value* func_value, int num_args,
                                   File_Pos origin);
    IR_Value* ir_builder_emit_builtin_function_call(IR_Builder* ir_builder,
                                                    AST_Expression* call_expr);
    IR_Value* ir_builder_emit_subscript(IR_Builder* ir_builder, IR_Value* base_value,
                                        IR_Value* index_value, File_Pos origin);
    void ir_builder_emit_jmp(IR_Builder* ir_builder, IR_Value* block_value, File_Pos origin);
    void ir_builder_emit_jmp_if(IR_Builder* ir_builder, IR_Value* cond_value,
                                IR_Value* block_value, File_Pos origin);
    IR_Value* ir_builder_emit_allocl(IR_Builder* ir_builder, AST_Type* type, const char* name,
                                     File_Pos origin);
    void ir_builder_emit_storel(IR_Builder* ir_builder, IR_Value* allocl_value,
                                IR_Value* new_value, File_Pos origin);
    IR_Value* ir_builder_emit_loadl(IR_Builder* ir_builder, IR_Value* allocl_value,
                                    File_Pos origin);
    void ir_builder_emit_storea(IR_Builder* ir_builder, IR_Value* arg_value, IR_Value* new_value,
                                File_Pos origin);
    IR_Value* ir_builder_emit_loada(IR_Builder* ir_builder, IR_Value* alloca_value,
                                    File_Pos origin);
    void ir_builder_emit_storep(IR_Builder* ir_builder, IR_Value* pointer_allocl,
                                IR_Value* new_value, File_Pos origin);
    IR_Value* ir_builder_emit_loadp(IR_Builder* ir_builder, IR_Value* pointer, File_Pos origin);
    IR_Value* ir_builder_emit_global(IR_Builder* ir_builder, AST_Declaration* global_decl);
    void ir_builder_emit_storeg(IR_Builder* ir_builder, IR_Value* global_value,
                                IR_Value* new_value, File_Pos origin);
    IR_Value* ir_builder_emit_loadg(IR_Builder* ir_builder, IR_Value* global_value,
                                    File_Pos origin);

	IR_Value* ir_builder_emit_load(IR_Builder* ir_builder, IR_Value* store, File_Pos origin);
    void ir_builder_emit_store(IR_Builder* ir_builder, IR_Value* store, IR_Value* new_value,
                               File_Pos origin);

    IR_Value* ir_builder_emit_lvalue(IR_Builder* ir_builder, AST_Expression* lvalue_expr,
                                     bool force_pointer = false);

    IR_Value* ir_builder_emit_cast(IR_Builder* ir_builder, IR_Value* value, AST_Type* type,
                                   File_Pos file_pos);

    void ir_builder_emit_assert(IR_Builder* ir_builder, IR_Value* assert_value, File_Pos origin);

    IR_Value* ir_builder_emit_zero_literal(IR_Builder* ir_builder, AST_Type* type);
    IR_Value* ir_boolean_literal(IR_Builder* ir_builder, AST_Type* type, bool value);
	IR_Value* ir_null_literal(IR_Builder* ir_builder, AST_Type* type);
    IR_Value* ir_string_literal(IR_Builder* ir_builder, AST_Type* type, Atom string);
    IR_Value* ir_integer_literal(IR_Builder* ir_builder, AST_Type* type, uint64_t s64);
    IR_Value* ir_float_literal(IR_Builder* ir_builder, AST_Type* type, double r64, float r32);
    IR_Value* ir_character_literal(IR_Builder* ir_builder, AST_Type* type, char c);
    IR_Value* ir_aggregate_literal(IR_Builder* ir_builder, AST_Type* aggregate_type,
                                   BUF(IR_Value*) member_values, bool is_const);
    uint64_t ir_builder_emit_foreign(IR_Builder* ir_builder, Atom atom);

    IR_Value* ir_builder_emit_phi(IR_Builder* ir_builder, AST_Type* type, File_Pos file_pos);
    IR_Value* ir_builder_emit_get_type_info(IR_Builder* ir_builder, uint64_t index,
                                            File_Pos file_pos);

    IR_Function* ir_function_new(IR_Builder* ir_builder, File_Pos file_pos, const char* name,
                                 AST_Type* func_type, AST_Scope* body_scope);
    void phi_node_add_incoming(IR_Value* phi_value, IR_Block* block, IR_Value* value);

    IR_Value* ir_value_new(IR_Builder* ir_builder, IR_Value_Kind kind, AST_Type* type);
    IR_Value* ir_value_function_new(IR_Builder* ir_builder, IR_Function* function);
    IR_Value* ir_value_block_new(IR_Builder* ir_builder, IR_Block* block);
    IR_Value* ir_value_allocl_new(IR_Builder* ir_builder, AST_Type* type, const char* name,
                                  File_Pos file_pos);
    IR_Value* ir_value_global_new(IR_Builder* ir_builder, AST_Type* type, IR_Value* init_value,
                                  const char* name);

    IR_Instruction* ir_instruction_new(IR_Builder* ir_builder, File_Pos origin, IR_Operator op,
                                       IR_Value* arg1, IR_Value* arg2, IR_Value* result);

    bool ir_instruction_is_terminator(IR_Operator op);

    IR_Validation_Result ir_validate(IR_Builder* ir_builder);
    bool ir_validate_function(IR_Function* ir_function, IR_Validation_Result* valres);
    bool ir_validate_block(IR_Block* ir_block, IR_Validation_Result* valres);
    void ir_report_validation_error(IR_Validation_Result* valres, const char* format, ...);

    void ir_builder_print_result(IR_Builder* ir_builder);
    void ir_builder_print_functions(IR_Builder* ir_builder, String_Builder* string_builder);
}
