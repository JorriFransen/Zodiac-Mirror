
#include "llvm_debug_info.h"

#include "builtin.h"
#include "llvm.h"

#include "llvm/IR/IRBuilder.h"

namespace Zodiac
{
    void llvm_debug_info_init(Debug_Info* di, const char* file_name, const char* dir_name,
                              LLVMModuleRef llvm_c_module)
    {
        Module* llvm_module = (llvm::Module*)llvm_c_module;
        di->builder = new DIBuilder(*llvm_module);
        assert(di->builder);

        di->file = llvm_debug_find_or_create_file(di, file_name, dir_name);
        di->current_file = di->file;
        assert(di->file);

        di->compile_unit = di->builder->createCompileUnit(dwarf::DW_LANG_C, di->file,
                                                          "Zodiac Compiler", 0, "", 0);

        stack_init(&di->scope_stack, 16);

        assert(di->compile_unit);
    }

    void llvm_debug_info_finalize(Debug_Info* di)
    {
        di->builder->finalize();
    }

    void llvm_debug_register_function(Debug_Info* di, IR_Function* zir_func,
                                      LLVMValueRef llvm_func_value)
    {
        Function* func = (Function*)llvm_func_value;
        unsigned line_number = zir_func->file_pos.line;
        unsigned scope_line = zir_func->first_block->first_instruction->origin.line;

        DISubroutineType* function_type =
            (DISubroutineType*)llvm_debug_get_type(di, zir_func->type);

        DISubprogram* sp = di->builder->createFunction(di->current_file, zir_func->name,
                                                       "", di->current_file, line_number,
                                                       function_type, scope_line,
                                                       DINode::FlagPrototyped,
                                                       DISubprogram::SPFlagDefinition);
        func->setSubprogram(sp);

    }

    void llvm_debug_finalize_function(Debug_Info* di, LLVMValueRef llvm_func_value)
    {
        Function* function = (Function*)llvm_func_value;

        DISubprogram* sp = function->getSubprogram();
        di->builder->finalizeSubprogram(sp);
    }

    void llvm_debug_update_location(Debug_Info* di, LLVM_IR_Builder* ir_builder,
                                    IR_Function* zir_func)
    {
        LLVMValueRef llvm_func = llvm_function_from_zir(ir_builder, zir_func);
        Function* function = (Function*)llvm_func;
        DISubprogram* sp = function->getSubprogram();

        llvm_debug_set_location(ir_builder, sp, zir_func->file_pos.line,
                                zir_func->file_pos.line_relative_char_pos);
    }

    void llvm_debug_update_location(LLVM_IR_Builder* zir_builder, IR_Module* ir_module)
    {
        Debug_Info* di = zir_builder->debug_info;
        DIFile* file = llvm_debug_find_or_create_file(di, ir_module->file_name,
                                                      ir_module->file_dir);
        di->current_file = file;
        llvm_debug_set_location(zir_builder, file, 0, 0);
    }

    void llvm_debug_update_location(LLVM_IR_Builder* zir_builder, IR_Instruction* iri)
    {
        Debug_Info* di = zir_builder->debug_info;

        auto fp = iri->origin;
        llvm_debug_set_location(zir_builder, stack_top(di->scope_stack), fp.line,
                                fp.line_relative_char_pos);
    }

    void llvm_debug_set_location(LLVM_IR_Builder* ir_builder, DIScope* scope, uint64_t line,
                                 uint64_t col)
    {
        auto llvm_ir_builder = llvm::unwrap(ir_builder->llvm_builder);
        llvm_ir_builder->SetCurrentDebugLocation(DebugLoc::get(line, col, scope));
    }

    void llvm_debug_enter_scope(LLVM_IR_Builder* zir_builder, IR_Function* zir_function)
    {
        Debug_Info* di = zir_builder->debug_info;
        LLVMValueRef func_val = llvm_function_from_zir(zir_builder, zir_function);
        Function* function = (Function*)func_val;

        DIScope* scope = function->getSubprogram();
        stack_push(di->scope_stack, scope);
    }

    void llvm_debug_exit_scope(LLVM_IR_Builder* zir_builder, IR_Function* zir_function)
    {
        Debug_Info* di = zir_builder->debug_info;
        LLVMValueRef func_val = llvm_function_from_zir(zir_builder, zir_function);
        Function* function = (Function*)func_val;

        DIScope* scope = function->getSubprogram();

        assert(stack_top(di->scope_stack) == scope);
        stack_pop(di->scope_stack);
    }

    DIFile* llvm_debug_find_or_create_file(Debug_Info* di, const char* file_name,
                                           const char* dir_name)
    {
        return di->builder->createFile(file_name, dir_name);
    }

    DIType* llvm_debug_get_type(Debug_Info* di, AST_Type* ast_type)
    {
        switch (ast_type->kind)
        {
            case AST_TYPE_FUNCTION:
            {
                BUF(Metadata*) _param_types = nullptr;

                BUF_PUSH(_param_types, llvm_debug_get_type(di, ast_type->function.return_type));

                for (uint64_t i = 0; i < BUF_LENGTH(ast_type->function.arg_types); i++)
                {
                    AST_Type* arg_type = ast_type->function.arg_types[i];
                    BUF_PUSH(_param_types, llvm_debug_get_type(di, arg_type));
                }

                ArrayRef<Metadata*> param_arr_ref(_param_types, BUF_LENGTH(_param_types));
                DITypeRefArray param_types = di->builder->getOrCreateTypeArray(param_arr_ref);

                DISubroutineType* result = di->builder->createSubroutineType(param_types);

                BUF_FREE(_param_types);

                return result;
                break;
            }

            case AST_TYPE_BASE:
            {
                if (ast_type == Builtin::type_void)
                {
                    return di->builder->createBasicType("void", 0, dwarf::DW_ATE_signed);
                }
                else if (ast_type->flags & AST_TYPE_FLAG_INT)
                {
                    if (ast_type->flags & AST_TYPE_FLAG_SIGNED)
                    {
                        return di->builder->createBasicType(ast_type->name, ast_type->bit_size,
                                                            dwarf::DW_ATE_signed);
                    }
                    else
                    {
                        return di->builder->createBasicType(ast_type->name, ast_type->bit_size,
                                                            dwarf::DW_ATE_unsigned);
                    }
                }
                assert(false);
            }

            case AST_TYPE_POINTER:
            {
                DIType* base_type = llvm_debug_get_type(di, ast_type->pointer.base);
                return di->builder->createPointerType(base_type, ast_type->bit_size,
                                                      ast_type->bit_size);
                break;
            }

            default: assert(false);
        }
    }
}
