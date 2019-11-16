#pragma once

#include "llvm.h"
#include "ir.h"

#include "llvm-c/DebugInfo.h"


namespace Zodiac
{
    struct Debug_Info
    {
        LLVMDIBuilderRef builder = nullptr;
        LLVMMetadataRef file = nullptr;
        LLVMMetadataRef compile_unit = nullptr;

        LLVMMetadataRef global_scope = nullptr;

        LLVMModuleRef llvm_module = nullptr;
        IR_Module* zir_module = nullptr;
    };

    Debug_Info llvm_emit_debug_info(LLVM_IR_Builder* ir_builder, IR_Module* zir_module);
    void llvm_finalize_debug_info(Debug_Info* di);
    void llvm_emit_function_debug_info(Debug_Info* di, const LLVM_Registered_Function& rf);
    void llvm_emit_scope_debug_info(Debug_Info* di);
    LLVMMetadataRef llvm_get_debug_type(Debug_Info* di, AST_Type* ast_type);
}
