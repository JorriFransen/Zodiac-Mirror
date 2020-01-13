
#include "llvm_debug_info.h"

#include "builtin.h"
#include "const_interpreter.h"
#include "llvm.h"

#include "llvm/IR/IRBuilder.h"

namespace Zodiac
{
    void llvm_debug_info_init(Debug_Info* di, Context* context, const char* file_name,
                              const char* dir_name, LLVMModuleRef llvm_c_module)
    {
        di->context = context;

        Module* llvm_module = (llvm::Module*)llvm_c_module;
        di->builder = new DIBuilder(*llvm_module);
        assert(di->builder);

        di->file = llvm_debug_find_or_create_file(di, file_name, dir_name);
        di->current_file = di->file;
        assert(di->file);

        di->compile_unit = di->builder->createCompileUnit(dwarf::DW_LANG_C_plus_plus, di->file,
                                                          "Zodiac Compiler", 0, "", 0);

        stack_init(&di->scope_stack, 16);
        stack_push(di->scope_stack, (DIScope*)di->file);

        di->registered_types = nullptr;

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
        unsigned line_number = (unsigned)zir_func->file_pos.line;
        unsigned scope_line = (unsigned)zir_func->body_scope->line;
        // unsigned scope_line = zir_func->first_block->first_instruction->origin.line;
        // printf("zir_func->body_scope->line = %d\n", zir_func->body_scope->line);

        DISubroutineType* function_type =
            (DISubroutineType*)llvm_debug_get_type(di, zir_func->type);

        // printf("scope_line: %d\n", scope_line);
        // printf("line_number: %d\n", line_number);

        DISubprogram* sp = di->builder->createFunction(di->compile_unit, zir_func->name,
                                                       "", di->current_file, line_number,
                                                       function_type, scope_line,
                                                       DINode::FlagPrototyped,
                                                       DISubprogram::SPFlagDefinition);
        func->setSubprogram(sp);

        di->current_subprogram = sp;
    }

    void llvm_debug_set_current_function(Debug_Info* di, LLVMValueRef llvm_func)
    {
        Function* func = (Function*)llvm_func;

        DISubprogram* sp = func->getSubprogram();
        di->current_subprogram = sp;
    }

    void llvm_debug_finalize_function(Debug_Info* di, LLVMValueRef llvm_func_value)
    {
        Function* function = (Function*)llvm_func_value;

        DISubprogram* sp = function->getSubprogram();
        di->builder->finalizeSubprogram(sp);

        assert(di->current_subprogram == sp);
        di->current_subprogram = nullptr;
    }

    void llvm_debug_register_function_parameter(LLVM_IR_Builder* zir_builder,
                                                LLVMValueRef _llvm_alloca,
                                                IR_Value* zir_arg, unsigned arg_no)
    {
        assert(zir_arg->kind == IRV_ARGUMENT);
        Debug_Info* di = zir_builder->debug_info;

        DIScope* scope = stack_top(di->scope_stack);
        DIType* param_type = llvm_debug_get_type(di, zir_arg->type);
        unsigned line = (unsigned)zir_arg->argument.file_pos.line;

        DILocalVariable* di_var = di->builder->createParameterVariable(scope,
                                                                       zir_arg->argument.name,
                                                                       arg_no, di->current_file,
                                                                       line, param_type, true);

        unsigned col = (unsigned)zir_arg->argument.file_pos.line_relative_char_pos;
        llvm::Value* llvm_alloca = unwrap(_llvm_alloca);
        auto llvm_builder = unwrap(zir_builder->llvm_builder);

        assert(di->current_subprogram);
        di->builder->insertDeclare(llvm_alloca, di_var, di->builder->createExpression(),
                                   DebugLoc::get(line, col, di->current_subprogram),
                                   llvm_builder->GetInsertBlock());
    }

    void llvm_debug_register_function_local_variable(LLVM_IR_Builder* zir_builder,
                                                     LLVMValueRef _llvm_alloca,
                                                     IR_Value* zir_allocl)
    {
        Debug_Info* di = zir_builder->debug_info;

        DIScope* scope = stack_top(di->scope_stack);
        DIFile* file = di->current_file;
        unsigned line = (unsigned)zir_allocl->allocl.file_pos.line;
        unsigned col = (unsigned)zir_allocl->allocl.file_pos.line_relative_char_pos;
        DIType* var_type = llvm_debug_get_type(di, zir_allocl->type);

        auto llvm_alloca = (llvm::Value*)_llvm_alloca;
        auto llvm_builder = unwrap(zir_builder->llvm_builder);

        DILocalVariable* di_var = di->builder->createAutoVariable(scope, zir_allocl->allocl.name,
                                                                  file, line, var_type);
        di->builder->insertDeclare(llvm_alloca, di_var, di->builder->createExpression(),
                                   DebugLoc::get(line, col, di->current_subprogram),
                                   llvm_builder->GetInsertBlock());
    }

    void llvm_debug_register_global(Debug_Info* di, LLVMValueRef _llvm_global,
                                    IR_Value* zir_global)
    {
        unsigned line = (unsigned)zir_global->global.file_pos.line;
        DIType* di_type = llvm_debug_get_type(di, zir_global->type);

        DIGlobalVariableExpression* di_var_expr = di->builder->createGlobalVariableExpression(
            di->compile_unit,
            zir_global->global.name,
            "",
            di->current_file,
            line,
            di_type,
            true);

        GlobalVariable* llvm_global = (GlobalVariable*)unwrap(_llvm_global);
        llvm_global->addDebugInfo(di_var_expr);
    }

    void llvm_debug_update_location(Debug_Info* di, LLVM_IR_Builder* ir_builder,
                                    IR_Function* zir_func)
    {
        LLVMValueRef llvm_func = llvm_function_from_zir(ir_builder, zir_func);
        Function* function = (Function*)llvm_func;
        DISubprogram* sp = function->getSubprogram();

        llvm_debug_set_location(ir_builder, sp, zir_func->file_pos.line,
                                zir_func->file_pos.line_relative_char_pos);
        llvm_debug_set_current_function(di, llvm_func);
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
        llvm_ir_builder->SetCurrentDebugLocation(DebugLoc::get((unsigned)line, (unsigned)col, scope));
    }

    void llvm_debug_unset_location(LLVM_IR_Builder* zir_builder)
    {
        // Debug_Info* di = zir_builder->debug_info;
        auto llvm_builder = llvm::unwrap(zir_builder->llvm_builder);

        llvm_builder->SetCurrentDebugLocation(DebugLoc());

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
                                           const char* _dir_name)
    {
		auto dir_name_len = strlen(_dir_name);
		char* dir_name = (char*)mem_alloc(dir_name_len + 1);
		for (uint64_t i = 0; i < dir_name_len; i++)
		{
			if (_dir_name[i] == '\\')
			{
				dir_name[i] = '/';
			}
			else
			{
				dir_name[i] = _dir_name[i];
			}
		}

		dir_name[dir_name_len] = '\0';

        auto result = di->builder->createFile(file_name, dir_name);
		mem_free(dir_name);
		return result;
    }

    DIType* llvm_debug_get_type(Debug_Info* di, AST_Type* ast_type)
    {
        auto registered_type = llvm_debug_get_registered_type(di, ast_type);
        if (registered_type)
            return registered_type;

        DIType* result = nullptr;
        bool register_type = true;

        switch (ast_type->kind)
        {
            case AST_TYPE_FUNCTION:
            {
                BUF(Metadata*) _param_types = nullptr;

                BUF_PUSH(_param_types, llvm_debug_get_type(di, ast_type->function.return_type));

                for (uint64_t i = 0; i < BUF_LENGTH(ast_type->function.arg_types); i++)
                {
                    AST_Type* arg_type = ast_type->function.arg_types[i]->type;
                    BUF_PUSH(_param_types, llvm_debug_get_type(di, arg_type));
                }

                ArrayRef<Metadata*> param_arr_ref(_param_types, BUF_LENGTH(_param_types));
                DITypeRefArray param_types = di->builder->getOrCreateTypeArray(param_arr_ref);

                result = di->builder->createSubroutineType(param_types);

                BUF_FREE(_param_types);

                break;
            }

            case AST_TYPE_BASE:
            {
                if (ast_type == Builtin::type_void)
                {
                    result = di->builder->createBasicType("void", 0, dwarf::DW_ATE_signed);
                }
                else if (ast_type->flags & AST_TYPE_FLAG_FLOAT)
                {
                    result = di->builder->createBasicType(ast_type->name, ast_type->bit_size,
                                                          dwarf::DW_ATE_float);
                }
                else if (ast_type == Builtin::type_u8)
                {
                    result = di->builder->createBasicType("u8", 8, dwarf::DW_ATE_unsigned_char);
                }
                else if (ast_type->flags & AST_TYPE_FLAG_INT)
                {
                    if (ast_type->flags & AST_TYPE_FLAG_SIGNED)
                    {
                        result = di->builder->createBasicType(ast_type->name, ast_type->bit_size,
                                                            dwarf::DW_ATE_signed);
                    }
                    else
                    {
                        result = di->builder->createBasicType(ast_type->name, ast_type->bit_size,
                                                              dwarf::DW_ATE_unsigned);
                    }
                }
                else assert(false);
                break;
            }

            case AST_TYPE_POINTER:
            {
                DIType* base_type = llvm_debug_get_type(di, ast_type->pointer.base);

                // Check for the pointer type again since the base type may contain
                //  a circular reference to itself via a pointer.
                result = llvm_debug_get_registered_type(di, ast_type);
                if (result)
                {
                    register_type = false;
                }
                else
                {
                    result = di->builder->createPointerType(base_type, ast_type->bit_size,
                                                            (uint32_t)ast_type->bit_size);
                }
                break;
            }

            case AST_TYPE_STATIC_ARRAY:
            {
                AST_Type* base_type = ast_type->static_array.base;
                DIType* di_base_type = llvm_debug_get_type(di, base_type);
                result = di->builder->createArrayType(ast_type->static_array.count,
                                                      (uint32_t)base_type->bit_size, di_base_type, nullptr);
                break;
            }

            case AST_TYPE_STRUCT:
            case AST_TYPE_UNION:
            {
                result = llvm_debug_get_aggregate_type(di, ast_type);
                break;
            }

            case AST_TYPE_ENUM:
            {
                unsigned line = (unsigned)ast_type->aggregate_type.scope->line;

                BUF(Metadata*) members = nullptr;

                auto member_decls = ast_type->aggregate_type.member_declarations;
                for (uint64_t i = 0; i < BUF_LENGTH(member_decls); i++)
                {
                    AST_Declaration* mem_decl = member_decls[i];
                    assert(mem_decl->kind == AST_DECL_CONSTANT_VAR);
                    assert(mem_decl->scope);

                    const char* name = mem_decl->identifier->atom.data;
                    auto init_expr = mem_decl->constant_var.init_expression;
                    int64_t value =
                        const_interpret_int_expression(di->context,
                                                       init_expr, init_expr->type,
                                                       mem_decl->scope);

                    bool is_unsigned = (init_expr->type->flags & AST_TYPE_FLAG_SIGNED) ?
                                            false : true;

                    BUF_PUSH(members, di->builder->createEnumerator(name, value, is_unsigned));
                }

                ArrayRef<Metadata*> _elements(members, BUF_LENGTH(members));
                DINodeArray elements = di->builder->getOrCreateArray(_elements);

                AST_Type* ast_base_type = ast_type->aggregate_type.base_type;
                DIType* underlying_type = llvm_debug_get_type(di, ast_base_type);

                result = di->builder->createEnumerationType(di->compile_unit,
                                                            ast_type->name,
                                                            di->current_file,
                                                            line,
                                                            ast_type->bit_size,
                                                            0,
                                                            elements,
                                                            underlying_type,
                                                            "",
                                                            true);

                BUF_FREE(members);

                assert(result);
                break;
            }

            default: assert(false);
        }

        assert(result);

        if (register_type)
        {
            llvm_debug_register_type(di, ast_type, result);
        }
        return result;
    }

    DIType* llvm_debug_get_aggregate_type(Debug_Info* di, AST_Type* ast_type)
    {
        assert(ast_type->kind == AST_TYPE_STRUCT || ast_type->kind == AST_TYPE_UNION);

        llvm_debug_create_fwd_decl(di, ast_type);

        BUF(Metadata*) members = nullptr;

        auto member_decls = ast_type->aggregate_type.member_declarations;
        uint64_t offset = 0;
        for (uint64_t i = 0; i < BUF_LENGTH(member_decls); i++)
        {
            AST_Declaration* member_decl = member_decls[i];
            AST_Type* ast_member_type = member_decl->mutable_decl.type;

            unsigned mem_line = (unsigned)member_decl->file_pos.line;
            const char* name = nullptr;
            if (member_decl->identifier) name = member_decl->identifier->atom.data;

            DIType* member_type = llvm_debug_get_type(di, ast_member_type);
            member_type = di->builder->createMemberType(di->compile_unit, name, di->current_file, mem_line, ast_member_type->bit_size, 0, offset,
                                                        DINode::FlagZero, member_type);

            offset += ast_member_type->bit_size;

            assert(member_type);
            BUF_PUSH(members, member_type);
        }

        DIType* result = nullptr;

        unsigned line = (unsigned)ast_type->aggregate_type.scope->line;

        ArrayRef<Metadata*> _elements(members, BUF_LENGTH(members));
        DINodeArray elements = di->builder->getOrCreateArray(_elements);

        if (ast_type->kind == AST_TYPE_STRUCT)
        {
            result = di->builder->createStructType(di->compile_unit, ast_type->name,
                                                   di->current_file, line, ast_type->bit_size, 0,
                                                   DINode::FlagZero, nullptr, elements);
        }
        else
        {
            result = di->builder->createUnionType(di->compile_unit, ast_type->name,
                                                  di->current_file, line, ast_type->bit_size,
                                                  0, DINode::FlagZero, elements);
        }

        BUF_FREE(members);

        assert(result);
        return result;
    }

    void llvm_debug_create_fwd_decl(Debug_Info* di, AST_Type* ast_type)
    {
        assert(ast_type->kind == AST_TYPE_STRUCT || ast_type->kind == AST_TYPE_UNION);

        unsigned tag = dwarf::DW_TAG_structure_type;
        if (ast_type->kind == AST_TYPE_UNION) tag = dwarf::DW_TAG_union_type;

        unsigned line = (unsigned)ast_type->aggregate_type.scope->line;

        DIType* di_type = di->builder->createReplaceableCompositeType(tag,
                                                                      ast_type->name,
                                                                      di->compile_unit,
                                                                      di->current_file,
                                                                      line);


        assert(di_type);
        llvm_debug_register_type(di, ast_type, di_type, RDT_FLAG_FWD_DECL);
    }

    DIType* llvm_debug_get_registered_type(Debug_Info* di, AST_Type* ast_type)
    {
        for (uint64_t i = 0; i < BUF_LENGTH(di->registered_types); i++)
        {
            if (di->registered_types[i].ast_type == ast_type)
            {
                return di->registered_types[i].di_type;
            }
        }

        return nullptr;
    }

    void llvm_debug_register_type(Debug_Info* di, AST_Type* ast_type, DIType* di_type,
                                  uint64_t flags/*=RDT_FLAG_NONE*/)
    {
        for (uint64_t i = 0; i < BUF_LENGTH(di->registered_types); i++)
        {
            auto& rt = di->registered_types[i];
            if (rt.ast_type == ast_type)
            {
                assert(rt.flags & RDT_FLAG_FWD_DECL);

                // auto fwd_com = (DICompositeType*)rt.di_type;
                // auto new_com = (DICompositeType*)di_type;
                // fwd_com->replaceElements(new_com->getElements());

                TempMDNode fwd_decl(rt.di_type);
                di->builder->replaceTemporary(std::move(fwd_decl), di_type);
                rt.di_type = di_type;

                rt.flags &= ~RDT_FLAG_FWD_DECL;

                return;
            }
        }

        for (uint64_t i = 0; i < BUF_LENGTH(di->registered_types); i++)
        {
            auto& rt = di->registered_types[i];
            assert(rt.ast_type != ast_type);
            // assert(rt.di_type != di_type);
        }

        Registered_Debug_Type rdt = { ast_type, di_type, flags };
        BUF_PUSH(di->registered_types, rdt);
    }
}
