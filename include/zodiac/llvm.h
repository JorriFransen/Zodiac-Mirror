#pragma once

#include "zodiac.h"
#include "hash.h"
#include "ir.h"

#include <llvm-c/Core.h>

namespace Zodiac
{
    struct LLVM_IR_Function;

    struct LLVM_Registered_Function
    {
        LLVMValueRef func = nullptr;
        IR_Function* zir_func = nullptr;
    };

    struct LLVM_Assigned_Value
    {
        LLVMValueRef llvm_value = nullptr;
        IR_Value* zir_value = nullptr;
    };

    struct LLVM_Block
    {
        LLVMBasicBlockRef block = nullptr;
        IR_Block* zir_block = nullptr;
    };

    struct LLVM_Registered_Type
    {
        LLVMTypeRef type = nullptr;
        AST_Type* zir_type = nullptr;

        bool finalized = false;
    };

    struct LLVM_IR_Builder
    {
        Context* context = nullptr;
        LLVMModuleRef llvm_module = nullptr;
        LLVMBuilderRef llvm_builder = nullptr;

        LLVM_IR_Function* current_function = nullptr;

        BUF(LLVM_Registered_Function) registered_functions = nullptr;

        Stack<IR_Value*> arg_stack = {};
        BUF(LLVM_Assigned_Value) assigned_values = nullptr;
        BUF(LLVM_Registered_Type) registered_aggregates = nullptr;

        BUF(IR_Module*) emitted_modules = nullptr;

        BUF(LLVMValueRef) registered_type_infos = nullptr;
        BUF(LLVMValueRef) casted_type_infos = nullptr;
        LLVMValueRef type_info_global = nullptr;
        LLVMValueRef aggregate_member_info_global = nullptr;
        LLVMValueRef enum_member_info_global = nullptr;

        Hash_Table<Atom, LLVMValueRef> const_c_string_table;
    };

    struct LLVM_IR_Function
    {
        BUF(LLVM_Block) blocks = nullptr;
    };

    void llvm_builder_init(LLVM_IR_Builder* builder);
    void llvm_builder_free(LLVM_IR_Builder* builder);
    void llvm_emit_ir_module(LLVM_IR_Builder* builder, IR_Module* module, bool root = true);
	void llvm_run_linker(LLVM_IR_Builder* builder, IR_Module* module, const char* obj_file_name);
    void llvm_collect_dynamic_lib_names(Context* context, IR_Module* module, BUF(Atom)* dest_arr);
    void llvm_convert_lib_names_to_paths(Context* context, BUF(Atom) lib_names);
    void llvm_emit_type_info(LLVM_IR_Builder* builder);
    void llvm_emit_global_variables_for_type_infos(LLVM_IR_Builder* builder);
    void llvm_emit_type_info_aggregate_members(LLVM_IR_Builder* builder);
    void llvm_emit_type_info_enum_members(LLVM_IR_Builder* builder);
    LLVMValueRef llvm_emit_aggregate_info(LLVM_IR_Builder* builder, Type_Info* type_info);
    LLVMValueRef llvm_emit_enum_info(LLVM_IR_Builder* builder, Type_Info* type_info);
    LLVMValueRef llvm_emit_function_info(LLVM_IR_Builder* builder, Type_Info* type_info);
    LLVMValueRef llvm_emit_constant_string(LLVM_IR_Builder* builder, const char* data,
                                           uint64_t length);
    LLVMValueRef llvm_emit_c_string(LLVM_IR_Builder* builder, const char* data, uint64_t length);
    void llvm_register_ir_function(LLVM_IR_Builder* builder, IR_Function* zir_func);
    void llvm_emit_ir_function(LLVM_IR_Builder* builder, IR_Function* zir_func);
    void llvm_emit_ir_block(LLVM_IR_Builder* builder, IR_Block* zir_block,
                            LLVMBasicBlockRef llvm_block);
    IR_Instruction* llvm_emit_ir_instruction(LLVM_IR_Builder* builder,
                                             IR_Instruction* zir_instruction,
                                             LLVMBasicBlockRef llvm_block);
    LLVMValueRef llvm_emit_ir_value(LLVM_IR_Builder* builder, IR_Value* zir_value);

    void llvm_emit_global(LLVM_IR_Builder* builder, IR_Value* zir_global);

    void llvm_cast_to_bigger_int_type(LLVM_IR_Builder* builder, LLVMValueRef* lhs,
                                      LLVMValueRef* rhs, bool lhs_sign, bool rhs_sign);

    void llvm_assign_result(LLVM_IR_Builder* builder, IR_Value* zir_value,
                            LLVMValueRef llvm_value);
    LLVMValueRef llvm_value_from_zir(LLVM_IR_Builder* builder, IR_Value* zir_value);

    LLVMValueRef llvm_function_from_zir(LLVM_IR_Builder* builder, IR_Value* zir_function_value);
    LLVMValueRef llvm_function_from_zir(LLVM_IR_Builder* builder, IR_Function* zir_function);
    LLVMBasicBlockRef llvm_block_from_zir(LLVM_IR_Builder* builder, IR_Block* zir_block);

    LLVMTypeRef llvm_type_from_ast(LLVM_IR_Builder* builder, AST_Type* zodiac_type);
    LLVMTypeRef llvm_aggregate_type_from_ast(LLVM_IR_Builder* builder, AST_Type* ag_type);

    void llvm_register_aggregate_types(LLVM_IR_Builder* builder, AST_Type* ag_type);
    LLVMTypeRef llvm_finalize_aggregate_types(LLVM_IR_Builder* builder, AST_Type* ag_type);

    LLVM_Registered_Type* llvm_find_registered_type(LLVM_IR_Builder* builder, AST_Type* zir_type);

    uint64_t llvm_int_value_byte_size(LLVMValueRef llvm_value);

}
