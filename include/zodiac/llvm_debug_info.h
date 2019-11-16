#pragma once

#include "ir.h"

#include <llvm/IR/DIBuilder.h>

namespace Zodiac
{
    using namespace llvm;

    struct LLVM_IR_Builder;

    struct Debug_Info
    {
        LLVM_IR_Builder* zir_builder = nullptr;
        DIBuilder* builder = nullptr;
        DIFile* file = nullptr;
        DICompileUnit* compile_unit = nullptr;

        BUF(DIType*) types = nullptr;
        DIFile* current_file = nullptr;

        Stack<DIScope*> scope_stack = {};
    };

    void llvm_debug_info_init(Debug_Info* di, const char* file_name, const char* dir_name,
                              LLVMModuleRef llvm_c_module);
    void llvm_debug_info_finalize(Debug_Info* di);

    void llvm_debug_register_function(Debug_Info* di, IR_Function* zir_func,
                                      LLVMValueRef llvm_func_value);
    void llvm_debug_finalize_function(Debug_Info* di, LLVMValueRef llvm_func_value);

    void llvm_debug_update_location(Debug_Info* di, LLVM_IR_Builder* ir_builder,
                                    IR_Function* zir_func);
    void llvm_debug_update_location(LLVM_IR_Builder* zir_builder, IR_Module* ir_module);
    void llvm_debug_update_location(LLVM_IR_Builder* zir_builder, IR_Instruction* iri);
    void llvm_debug_set_location(LLVM_IR_Builder* ir_builder, DIScope* scope, uint64_t line,
                                 uint64_t col);

    void llvm_debug_enter_scope(LLVM_IR_Builder* zir_builder, IR_Function* zir_function);
    void llvm_debug_exit_scope(LLVM_IR_Builder* zir_builder, IR_Function* zir_function);

    DIFile* llvm_debug_find_or_create_file(Debug_Info* di, const char* file_name,
                                           const char* dir_name);

    DIType* llvm_debug_get_type(Debug_Info* di, AST_Type* ast_type);
}
