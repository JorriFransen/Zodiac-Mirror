
#include <iostream>

#include "llvm_debug_info.h"

#include <llvm/BinaryFormat/Dwarf.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm-c/Comdat.h>
#include <llvm/IR/Comdat.h>
#include <llvm/IR/Module.h>

namespace Zodiac
{
    Debug_Info llvm_emit_debug_info(LLVM_IR_Builder* ir_builder, IR_Module* zir_module)
    {
        auto _m = (llvm::Module*)ir_builder->llvm_module;

        // _m->getOrInsertComdat("dbg_comdat");

        _m->setPICLevel(llvm::PICLevel::BigPIC);
        auto pic_level = _m->getPICLevel();
        printf("pic level: %d\n", (int)pic_level);

        LLVMDIBuilderRef di_builder =
            LLVMCreateDIBuilderDisallowUnresolved(ir_builder->llvm_module);
            // LLVMCreateDIBuilder(ir_builder->llvm_module);

        LLVMMetadataRef di_file = LLVMDIBuilderCreateFile(di_builder, zir_module->file_name,
                                                          strlen(zir_module->file_name),
                                                          zir_module->file_dir,
                                                          strlen(zir_module->file_dir));

        const char* producer = "zodiac_dev";
        bool is_optimized = false;
        const char* flags = "";
        unsigned runtime_version = 0;

        LLVMMetadataRef di_cu = LLVMDIBuilderCreateCompileUnit(di_builder,
                                                               LLVMDWARFSourceLanguageC,
                                                               di_file, producer, strlen(producer),
                                                               is_optimized, flags, strlen(flags),
                                                               runtime_version, "", 0,
                                                               LLVMDWARFEmissionFull, 0, false,
                                                               false);

        Debug_Info di = { di_builder, di_file, di_cu, nullptr, ir_builder->llvm_module,
                          zir_module };

        llvm_emit_scope_debug_info(&di);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->registered_functions); i++)
        {
            const LLVM_Registered_Function& rf = ir_builder->registered_functions[i];
            if (!(rf.zir_func->flags & IR_FUNC_FLAG_FOREIGN))
            // if (strcmp(rf.zir_func->name, "test_func") == 0)
                llvm_emit_function_debug_info(&di, rf);
        }

        return di;
    }

    void llvm_finalize_debug_info(Debug_Info* di)
    {
        LLVMDIBuilderFinalize(di->builder);
    }

    void llvm_emit_function_debug_info(Debug_Info* di, const LLVM_Registered_Function& rf)
    {
        LLVMDIFlags flags = LLVMDIFlagZero;
        unsigned line_no = rf.zir_func->file_pos.line;

        LLVMMetadataRef func_type = llvm_get_debug_type(di, rf.zir_func->type);
        bool is_local = true;

        bool is_definition = true;
        unsigned scope_line = 0;
        if (rf.zir_func->flags & IR_FUNC_FLAG_FOREIGN)
        {
            is_local = false;
            is_definition = false;
            scope_line = line_no;
        }
        else
        {
            scope_line = rf.zir_func->first_block->first_instruction->origin.line;
        }
        bool is_optimized = false;

        LLVMComdatRef comdat = LLVMGetComdat(rf.func);
        if (comdat)
        {
            assert(false);
        }

        LLVMMetadataRef sp =
            LLVMDIBuilderCreateFunction(di->builder,
                                        di->global_scope,
                                        rf.zir_func->name, strlen(rf.zir_func->name),
                                        rf.zir_func->name, strlen(rf.zir_func->name),
                                        di->file,
                                        line_no,
                                        func_type,
                                        is_local,
                                        is_definition,
                                        scope_line,
                                        flags,
                                        is_optimized);

        // LLVMValueRef sp_val = LLVMMetadataAsValue(LLVMGetModuleContext(di->llvm_module), sp);
        // printf("Attaching dbg meta data: %s\n", LLVMPrintValueToString(sp_val));
        // LLVMSetMetadata(rf.func, llvm::LLVMContext::MD_dbg, sp_val);

        auto _func = (llvm::Function*)rf.func;
        _func->setSubprogram((llvm::DISubprogram*)sp);

        comdat = LLVMGetComdat(rf.func);
        if (comdat)
        {
            // LLVMSetComdat(rf.func, nullptr);
            // auto _cd = (llvm::Comdat*)comdat;
            // std::string _str;
            // llvm::raw_string_ostream rso(_str);
            // _cd->print(rso, true);
            // rso.flush();
            // std::cerr << _str;
            // // _cd->dump();
            // assert(false);
            // // LLVMSetComdat(rf.func, nullptr);
            // // printf("removing comdat from function: %s\n", rf.zir_func->name);
        }
    }

    void llvm_emit_scope_debug_info(Debug_Info* di)
    {
        di->global_scope = LLVMDIBuilderCreateLexicalBlock(di->builder, nullptr, di->file, 0, 0);
    }

    LLVMMetadataRef llvm_get_debug_type(Debug_Info* di, AST_Type* ast_type)
    {
        switch (ast_type->kind)
        {
            case AST_TYPE_FUNCTION:
            {

                BUF(LLVMMetadataRef) param_types = nullptr;

                for (uint64_t i = 0; i < BUF_LENGTH(ast_type->function.arg_types); i++)
                {
                    AST_Type* arg_type = ast_type->function.arg_types[i];
                    LLVMMetadataRef di_arg_type = llvm_get_debug_type(di, arg_type);
                    BUF_PUSH(param_types, di_arg_type);
                }

                LLVMDIFlags flags = LLVMDIFlagZero;
                auto result = LLVMDIBuilderCreateSubroutineType(di->builder, di->file, param_types,
                                                                BUF_LENGTH(param_types), flags);
                BUF_FREE(param_types);
                return result;
                break;
            }

            case AST_TYPE_POINTER:
            {
                LLVMMetadataRef base_di_type = llvm_get_debug_type(di, ast_type->pointer.base);
                return LLVMDIBuilderCreatePointerType(di->builder, base_di_type,
                                                      ast_type->bit_size, ast_type->bit_size,
                                                      0, nullptr, 0);
                break;
            }

            case AST_TYPE_BASE:
            {
                if (ast_type->flags & AST_TYPE_FLAG_INT)
                {
                    LLVMDWARFTypeEncoding encoding = 0;
                    if (ast_type->flags & AST_TYPE_FLAG_SIGNED)
                        encoding = llvm::dwarf::DW_ATE_signed;
                    else
                        encoding = llvm::dwarf::DW_ATE_unsigned;

                    LLVMDIFlags flags = LLVMDIFlagZero;

                    return LLVMDIBuilderCreateBasicType(di->builder, ast_type->name,
                                                        strlen(ast_type->name),
                                                        ast_type->bit_size, encoding,
                                                        flags);
                }
                else if (ast_type->flags & AST_TYPE_FLAG_FLOAT)
                {
                    return LLVMDIBuilderCreateBasicType(di->builder, ast_type->name,
                                                        strlen(ast_type->name),
                                                        ast_type->bit_size,
                                                        llvm::dwarf::DW_ATE_float, LLVMDIFlagZero);
                }
                else if (ast_type->flags & AST_TYPE_FLAG_VOID)
                {
                    return nullptr;
                }
                else assert(false);
                break;
            }

            default: assert(false);
        }
    }

}
