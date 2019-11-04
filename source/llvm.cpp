#include "llvm.h"

#include "builtin.h"
#include "llvm_types.h"
#include "platform.h"

#include <llvm-c/Analysis.h>
#include <llvm-c/TargetMachine.h>

#include <dynload.h>

#ifdef WIN32
#include <Windows.h>
#endif

namespace Zodiac
{
    uint64_t _get_atom_hash(const Atom& atom)
    {
        return get_atom_hash(atom.data, atom.length);
    }

    void llvm_builder_init(LLVM_IR_Builder* builder)
    {
        builder->registered_functions = nullptr;
        stack_init(&builder->arg_stack, 8);
        builder->assigned_values = nullptr;
        builder->registered_aggregates = nullptr;
        builder->emitted_modules = nullptr;
        builder->registered_type_infos = nullptr;
        builder->type_info_global = nullptr;
        builder->aggregate_member_info_global = nullptr;
        builder->enum_member_info_global = nullptr;

        Atom empty_atom = {};
        hash_table_init(&builder->const_c_string_table, 32, empty_atom, _get_atom_hash);

        // LLVMEnablePrettyStackTrace();
    }

    void llvm_builder_free(LLVM_IR_Builder* builder)
    {
        stack_free(&builder->arg_stack);
        BUF_FREE(builder->assigned_values);
        BUF_FREE(builder->registered_aggregates);
        BUF_FREE(builder->emitted_modules);
    }

    void llvm_emit_ir_module(LLVM_IR_Builder* builder, IR_Module* module, bool root /*=true*/)
    {
        assert(module->name);

        maybe_init_llvm_types(builder);

        if (root)
        {
            LLVMModuleRef llvm_module = LLVMModuleCreateWithName(module->name);
            builder->llvm_module = llvm_module;
            builder->llvm_builder = LLVMCreateBuilder();

            llvm_emit_type_info(builder);

            assert(builder->context->builtin_ir_builder);
            llvm_emit_ir_module(builder, &builder->context->builtin_ir_builder->result, false);
        }

        // Check if this module has been emitted already
        for (uint64_t i = 0; i < BUF_LENGTH(builder->emitted_modules); i++)
        {
            if (builder->emitted_modules[i] == module) return;
        }

        // Emit modules that are imported by this module
        for (uint64_t i = 0; i < BUF_LENGTH(module->imported_modules); i++)
        {
            IR_Module* import_module = module->imported_modules[i];
            llvm_emit_ir_module(builder, import_module, false);
        }

        // Emit global variables
        for (uint64_t i = 0; i < BUF_LENGTH(module->globals); i++)
        {
            IR_Value* zir_global = module->globals[i];
            llvm_emit_global(builder, zir_global);
        }

        // Register functions so we can call them before generating them
        for (uint64_t i = 0; i < BUF_LENGTH(module->functions); i++)
        {
            IR_Function* zir_func = module->functions[i];
            llvm_register_ir_function(builder, zir_func);
        }

        // Emit function bodies
        for (uint64_t i = 0; i < BUF_LENGTH(module->functions); i++)
        {
            IR_Function* zir_func = module->functions[i];
            llvm_emit_ir_function(builder, zir_func);
        }

        if (root)
        {
            char* error = nullptr;
            bool verify_error = LLVMVerifyModule(
                builder->llvm_module, LLVMAbortProcessAction, &error);
            if (verify_error)
            {
                fprintf(stderr, "%s\n", error);
                assert(false);
            }
            LLVMDisposeMessage(error);
            error = nullptr;

            char* llvm_module_string = LLVMPrintModuleToString(builder->llvm_module);
            // printf("%s", llvm_module_string);
            LLVMDisposeMessage(llvm_module_string);

            LLVMInitializeNativeTarget();
            LLVMInitializeNativeAsmPrinter();

            auto target_triple = LLVMGetDefaultTargetTriple();
            // printf("Target triple: %s\n", target_triple);
            LLVMTargetRef llvm_target = nullptr;
            bool target_result = LLVMGetTargetFromTriple(target_triple, &llvm_target, &error);
            if (target_result)
                fprintf(stderr, "%s\n", error);
            LLVMDisposeMessage(error);
            error = nullptr;

            auto cpu = "generic";
            auto features = "";

            LLVMTargetMachineRef llvm_target_machine = LLVMCreateTargetMachine(
                llvm_target,
                target_triple,
                cpu,
                features,
                LLVMCodeGenLevelNone,
                LLVMRelocPIC,
                LLVMCodeModelDefault);

			const char* obj_file_name = string_append(module->name, ".o");

            bool obj_emit_failed = LLVMTargetMachineEmitToFile(
                llvm_target_machine,
                builder->llvm_module,
				(char*)obj_file_name,
                LLVMObjectFile,
                &error);


            if (obj_emit_failed)
                fprintf(stderr, "%s", error);

            LLVMDisposeTargetMachine(llvm_target_machine);
            LLVMDisposeMessage(error);
            error = nullptr;

			llvm_run_linker(builder, module, obj_file_name);

			mem_free(obj_file_name);

        }
        else
        {
            BUF_PUSH(builder->emitted_modules, module);
        }
    }

	void llvm_run_linker(LLVM_IR_Builder* builder, IR_Module* module, const char* obj_file_name)
	{
        auto x64_lib_path = find_linux_x64_lib_path();
        auto gcc_lib_path = find_linux_gcc_lib_path();
        // printf("x64_lib_path: %s\n", x64_lib_path);
        printf("gcc_lib_path: %s\n", gcc_lib_path);

		BUF(Atom) dynamic_lib_names = nullptr;
		llvm_collect_dynamic_lib_names(builder->context, module, &dynamic_lib_names);
		llvm_convert_lib_names_to_paths(builder->context, dynamic_lib_names);

		String_Builder sb;
		string_builder_init(&sb, 2048);

#ifdef __linux__
		string_builder_append(&sb, "ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 ");

        string_builder_append(&sb, x64_lib_path);
        string_builder_append(&sb, "Scrt1.o ");

        string_builder_append(&sb, x64_lib_path);
        string_builder_append(&sb, "crti.o ");

        string_builder_append(&sb, gcc_lib_path);
        string_builder_append(&sb, "crtbeginS.o ");

        string_builder_append(&sb, obj_file_name);
        string_builder_append(&sb, " -lc ");

		for (uint64_t i = 0; i < BUF_LENGTH(dynamic_lib_names); i++)
		{
			string_builder_append(&sb, dynamic_lib_names[i]);
			string_builder_append(&sb, " ");
		}
        string_builder_append(&sb, gcc_lib_path);
		string_builder_append(&sb, "crtendS.o ");
        string_builder_append(&sb, x64_lib_path);
        string_builder_append(&sb, "crtn.o ");
        auto exe_file_name = extract_file_name_from_path(obj_file_name);
        string_builder_append(&sb, "-o ");
        string_builder_append(&sb, exe_file_name);
        string_builder_append(&sb, " ");
        mem_free(exe_file_name);
		string_builder_append(&sb, " 2>&1");
		// string_builder_append(&sb, " /usr/lib64/gcc/x86_64-pc-linux-gnu/9.2.0/crtendS.o /usr/lib64/crtn.o ");



		const char* link_cmd = string_builder_to_string(&sb);
		string_builder_free(&sb);

		fprintf(stderr, "Running linker with command: %s\n", link_cmd);

		char out_buf[1024];
		FILE* link_process_handle = popen(link_cmd, "r");
		assert(link_process_handle);

		while (fgets(out_buf, sizeof(out_buf), link_process_handle) != nullptr)
		{
		   printf("%s", out_buf);
		}
		assert(feof(link_process_handle));
		int close_ret = pclose(link_process_handle);
		close_ret = WEXITSTATUS(close_ret);
		assert(close_ret >= 0);

		if (close_ret != 0)
		{
		   fprintf(stderr, "Link command failed with exit code: %d\n", close_ret);
		}
#elif WIN32

		auto msvc_tools_dir = find_msvc_tools_dir();
		auto windows_kit_lib_dir = find_windows_kit_lib_dir();

		string_builder_append(&sb, msvc_tools_dir);
		string_builder_append(&sb, "\\bin\\Hostx64\\x64\\link.exe ");

		string_builder_append(&sb, "/NOLOGO /WX /SUBSYSTEM:CONSOLE ");

		string_builder_append(&sb, " msvcrtd.lib");
		//string_builder_append(&sb, " ucrtd.lib");
		//string_builder_append(&sb, " vcruntimed.lib");
		string_builder_append(&sb, " user32.lib");
		//string_builder_append(&sb, " kernel32.lib");
		string_builder_append(&sb, " legacy_stdio_definitions.lib");
		//string_builder_append(&sb, " legacy_stdio_definitions.lib");

		string_builder_append(&sb, " ");
		string_builder_append(&sb, obj_file_name);

		string_builder_append(&sb, " /LIBPATH:\"");
		string_builder_append(&sb, windows_kit_lib_dir);
		string_builder_append(&sb, "\\ucrt\\x64\" ");

		string_builder_append(&sb, " /LIBPATH:\"");
		string_builder_append(&sb, msvc_tools_dir);
		string_builder_append(&sb, "\\lib\\x64\" ");

		string_builder_append(&sb, " /LIBPATH:\"");
		string_builder_append(&sb, windows_kit_lib_dir);
		string_builder_append(&sb, "\\um\\x64\" ");

		for (uint64_t i = 0; i < BUF_LENGTH(dynamic_lib_names); i++)
		{
			Atom lib_name = dynamic_lib_names[i];
			string_builder_append(&sb, lib_name);
			string_builder_append(&sb, " ");
		}

		PROCESS_INFORMATION process_info;
		STARTUPINFO startup_info;
		ZeroMemory(&startup_info, sizeof(startup_info));
		ZeroMemory(&process_info, sizeof(process_info));
		startup_info.cb = sizeof(startup_info);

		auto cmd_str = string_builder_to_string(&sb);

		printf("Running link command: %s\n", cmd_str);
		auto result = CreateProcessA(nullptr, cmd_str, nullptr, nullptr, true, 0, nullptr,
                                     nullptr, &startup_info, &process_info);
		mem_free(cmd_str);

		if (!result)
		{
			auto err = GetLastError();
			LPSTR message_buf = nullptr;
			size_t size = FormatMessageA(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                                         FORMAT_MESSAGE_FROM_SYSTEM |
				                         FORMAT_MESSAGE_IGNORE_INSERTS, nullptr, err,
				                         MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
				                         (LPSTR)&message_buf, 0, nullptr);

			fprintf(stderr, "%.*s", (int)size, message_buf);
			LocalFree(message_buf);
			assert(false);
		}

		WaitForSingleObject(process_info.hProcess, INFINITE);
		CloseHandle(process_info.hProcess);
		CloseHandle(process_info.hThread);

#endif


		BUF_FREE(dynamic_lib_names);
	}

    void llvm_collect_dynamic_lib_names(Context* context, IR_Module* module, BUF(Atom)* dest_arr)
    {
        assert(dest_arr);

        for (uint64_t i = 0; i < BUF_LENGTH(module->dynamic_lib_names); i++)
        {
            bool duplicate = false;

            for (uint64_t j = 0; j < BUF_LENGTH(*dest_arr); j++)
            {
                if ((*dest_arr)[j] == module->dynamic_lib_names[i])
                {
                    duplicate = true;
                    break;
                }
            }

            if (!duplicate)
            {
                BUF_PUSH(*dest_arr, module->dynamic_lib_names[i]);
            }
        }

        for (uint64_t i = 0; i < BUF_LENGTH(module->imported_modules); i++)
        {
            llvm_collect_dynamic_lib_names(context, module->imported_modules[i], dest_arr);
        }
    }

    void llvm_convert_lib_names_to_paths(Context* context, BUF(Atom) lib_names)
    {
        for (uint64_t i = 0; i < BUF_LENGTH(lib_names); i++)
        {
            Atom lib_name = lib_names[i];
			const char* lib_path = nullptr;

#ifdef WIN32
			if (!string_ends_with(lib_name.data, ".dll") &&
                !string_ends_with(lib_name.data, ".lib"))
			{
				lib_path = string_append(lib_name.data, ".lib");
			}
			else
			{
				lib_path = string_append(lib_name.data, "");
			}
#else
            lib_path = string_append(lib_name.data, "");
#endif

            bool found = false;

            if (file_exists(lib_path))
            {
                continue;
            }

            for (uint64_t j = 0; j < BUF_LENGTH(context->module_search_path); j++)
            {
                Atom search_path = context->module_search_path[j];
                auto import_atom = atom_append(context->atom_table, search_path, lib_path);
                if (file_exists(import_atom.data))
                {
                    lib_names[i] = import_atom;
                    found = true;
                    break;
                }
            }

            if (!found)
            {
                 // fprintf(stderr, "Did not find dynamic library: %s\n", lib_name.data);
                 DLLib* loaded_lib = dlLoadLibrary(lib_path);
				 if (loaded_lib)
				 {
					 char lib_path_buf[2048];
					 auto ret = dlGetLibraryPath(loaded_lib, lib_path_buf, 2048);
					 assert(ret > 0);
					 lib_names[i] = atom_get(context->atom_table, lib_path_buf);
					 dlFreeLibrary(loaded_lib);
				 }
				 else
				 {
					 lib_names[i] = atom_get(context->atom_table, lib_path);
				 }
            }

			mem_free(lib_path);
        }
    }

    void llvm_emit_type_info(LLVM_IR_Builder* builder)
    {
        Type_Info_Data* tid = &builder->context->type_info_data;

        if (tid->type_info_count <= 0)
        {
            return;
        }

        assert(builder->type_info_global == nullptr);

        LLVMTypeRef llvm_tiam_type = LLVM_Type::Type_Info_Aggregate_Member;
        LLVMTypeRef llvm_tiem_type = LLVM_Type::Type_Info_Enum_Member;

        llvm_emit_global_variables_for_type_infos(builder);

        LLVMValueRef llvm_zero = LLVMConstNull(LLVM_Type::u32);
        builder->type_info_global = LLVMAddGlobal(builder->llvm_module,
                                                  LLVMArrayType(LLVM_Type::ptr_to_Type_Info,
                                                                (unsigned int)tid->type_info_count - 1),
                                                  "type_infos");
        LLVMSetGlobalConstant(builder->type_info_global, true);

        builder->aggregate_member_info_global = LLVMAddGlobal(builder->llvm_module,
                                                              LLVMArrayType(llvm_tiam_type,
                                                                            (unsigned int)tid->agg_count),
                                                              "aggregate_member_infos");
        LLVMSetGlobalConstant(builder->aggregate_member_info_global, true);

        builder->enum_member_info_global = LLVMAddGlobal(builder->llvm_module,
                                                         LLVMArrayType(llvm_tiem_type,
                                                                       (unsigned int)tid->enum_count),
                                                         "enum_member_infos");
        LLVMSetGlobalConstant(builder->enum_member_info_global, true);



        for (uint64_t i = 1; i < tid->type_info_count; i++)
        {
            Type_Info* ti = &tid->type_infos[i];

            LLVMValueRef llvm_name_val = llvm_emit_constant_string(builder, ti->name.data,
                                                                   ti->name.length);

            LLVMValueRef llvm_kind_val = LLVMConstInt(LLVM_Type::Type_Info_Kind, ti->kind, false);
            LLVMValueRef llvm_byte_size_val = LLVMConstInt(LLVM_Type::u64, ti->byte_size, false);

            LLVMValueRef llvm_info_val = LLVMConstNull(LLVM_Type::Type_Info_Info_Union);

            if (ti->kind == POINTER)
            {
                assert(ti->base.id > 0);
                assert(BUF_LENGTH(builder->registered_type_infos) > (ti->base.id - 1));
                llvm_info_val = builder->registered_type_infos[ti->base.id - 1];
                llvm_info_val = LLVMConstPointerCast(llvm_info_val, LLVM_Type::ptr_to_Type_Info);
                llvm_info_val = LLVMConstStruct(&llvm_info_val, 1, false);
            }
            else if (ti->kind == STRUCT ||
                     ti->kind == UNION)
            {
                llvm_info_val = llvm_emit_aggregate_info(builder, ti);
            }
            else if (ti->kind == ENUM)
            {
                llvm_info_val = llvm_emit_enum_info(builder, ti);
            }
            else if (ti->kind == FUNCTION)
            {
                llvm_info_val = llvm_emit_function_info(builder, ti);
            }
            else
            {
                assert(ti->kind == BASE);
            }

            LLVMValueRef ti_mem_vals[] = { llvm_kind_val, llvm_name_val,
                                           llvm_byte_size_val, llvm_info_val };
            unsigned ti_mem_count = STATIC_ARRAY_LENGTH(ti_mem_vals);
            LLVMValueRef llvm_ti = llvm_ti = LLVMConstStruct(ti_mem_vals, ti_mem_count, false);

            LLVMValueRef llvm_ti_global = builder->registered_type_infos[i - 1];
            // printf("llvm_ti_global: %s\n", LLVMPrintTypeToString(LLVMTypeOf(llvm_ti_global)));
            // printf("llvm_ti       : %s\n\n", LLVMPrintTypeToString(LLVMTypeOf(llvm_ti)));
            LLVMSetInitializer(llvm_ti_global, llvm_ti);

            // @TODO: We could cast this right before we push it to the original array
            LLVMValueRef casted_type_info = LLVMConstGEP(llvm_ti_global, &llvm_zero, 1);
            casted_type_info = LLVMConstBitCast(casted_type_info, LLVM_Type::ptr_to_Type_Info);
            BUF_PUSH(builder->casted_type_infos, casted_type_info);
        }

        llvm_emit_type_info_aggregate_members(builder);
        llvm_emit_type_info_enum_members(builder);

        if (builder->registered_type_infos)
        {
            LLVMValueRef type_info_value = LLVMConstArray(LLVM_Type::ptr_to_Type_Info,
                                                          builder->casted_type_infos,
                                                          (unsigned)tid->type_info_count - 1);
            LLVMSetInitializer(builder->type_info_global, type_info_value);
            LLVMSetGlobalConstant(builder->type_info_global, true);
        }
    }

    void llvm_emit_global_variables_for_type_infos(LLVM_IR_Builder* builder)
    {
        Type_Info_Data* tid = &builder->context->type_info_data;
        for (uint64_t i = 1; i < tid->type_info_count; i++)
        {
            Type_Info* ti = &tid->type_infos[i];

            LLVMTypeRef global_var_type = nullptr;

            switch (ti->kind)
            {
                case BASE:
                {
                    global_var_type = LLVM_Type::_instance_Type_Info_Base;
                    break;
                }

                case POINTER:
                {
                    global_var_type = LLVM_Type::_instance_Type_Info_Pointer;
                    break;
                }

                case STRUCT:
                case UNION:
                {
                    global_var_type = LLVM_Type::_instance_Type_Info_Aggregate;
                    break;
                }

                case ENUM:
                {
                    global_var_type = LLVM_Type::_instance_Type_Info_Enum;
                    break;
                }

                case FUNCTION:
                {
                    global_var_type = LLVM_Type::_instance_Type_Info_Function;
                    break;
                }

                default: assert(false);
            }

            assert(global_var_type);

            LLVMValueRef global_var = LLVMAddGlobal(builder->llvm_module, global_var_type,
                                                    "_type_info_global");
            LLVMSetGlobalConstant(global_var, true);

            BUF_PUSH(builder->registered_type_infos, global_var);
        }

    }

    void llvm_emit_type_info_aggregate_members(LLVM_IR_Builder* builder)
    {
        Type_Info_Data* tid = &builder->context->type_info_data;

        if (tid->agg_count <= 0)
        {
            return;
        }

        LLVMValueRef llvm_zero = LLVMConstNull(LLVM_Type::u32);

        BUF(LLVMValueRef) member_infos = nullptr;

        for (uint64_t i = 0; i < tid->agg_count; i++)
        {
            Type_Info_Aggregate_Member* tiam = &tid->aggregate_members[i];

            LLVMValueRef llvm_name_val = llvm_emit_constant_string(builder, tiam->name.data,
                                                                   tiam->name.length);


            LLVMValueRef llvm_ti_ptr_val = builder->registered_type_infos[tiam->type.id - 1];
            llvm_ti_ptr_val = LLVMConstPointerCast(llvm_ti_ptr_val, LLVM_Type::ptr_to_Type_Info);

            LLVMValueRef tiam_mem_vals[] = { llvm_name_val, llvm_ti_ptr_val };
            unsigned tiam_mem_count = STATIC_ARRAY_LENGTH(tiam_mem_vals);

            LLVMValueRef llvm_tiam = LLVMConstNamedStruct(LLVM_Type::Type_Info_Aggregate_Member,
                                                          tiam_mem_vals, tiam_mem_count);
            BUF_PUSH(member_infos, llvm_tiam);
        }

        if (member_infos)
        {
            LLVMValueRef tiams_value = LLVMConstArray(LLVM_Type::Type_Info_Aggregate_Member,
                                                      member_infos, (unsigned)tid->agg_count);
            LLVMSetInitializer(builder->aggregate_member_info_global, tiams_value);
        }

        BUF_FREE(member_infos);

    }

    void llvm_emit_type_info_enum_members(LLVM_IR_Builder* builder)
    {
        Type_Info_Data* tid = &builder->context->type_info_data;

        if (tid->enum_count <= 0)
        {
            return;
        }

        LLVMValueRef llvm_zero = LLVMConstNull(LLVM_Type::u32);

        BUF(LLVMValueRef) enum_infos = nullptr;

        for (uint64_t i = 0; i < tid->enum_count; i++)
        {
            Type_Info_Enum_Member* tiem = &tid->enum_members[i];

            LLVMValueRef llvm_name_val = llvm_emit_constant_string(builder, tiem->name.data,
                                                                   tiem->name.length);
            LLVMValueRef llvm_value_val = LLVMConstInt(LLVM_Type::u64, tiem->value, false);

            assert(BUF_LENGTH(builder->registered_type_infos) > (tiem->type.id - 1));
            LLVMValueRef llvm_ti_ptr_val = builder->casted_type_infos[tiem->type.id - 1];

            LLVMValueRef tiem_mem_vals[] = { llvm_name_val, llvm_value_val, llvm_ti_ptr_val };
            unsigned tiem_mem_count = STATIC_ARRAY_LENGTH(tiem_mem_vals);
            LLVMValueRef llvm_tiem = LLVMConstNamedStruct(LLVM_Type::Type_Info_Enum_Member,
                                                          tiem_mem_vals, tiem_mem_count);
           BUF_PUSH(enum_infos, llvm_tiem);
        }

        if (enum_infos)
        {
            LLVMValueRef tiems_value = LLVMConstArray(LLVM_Type::Type_Info_Enum_Member,
                                                      enum_infos, (unsigned)tid->enum_count);
            LLVMSetInitializer(builder->enum_member_info_global, tiems_value);
        }

        BUF_FREE(enum_infos);
    }

    LLVMValueRef llvm_emit_aggregate_info(LLVM_IR_Builder* builder, Type_Info* type_info)
    {

        LLVMValueRef llvm_member_count = LLVMConstInt(LLVM_Type::u64, type_info->aggregate.count,
                                                      false);
        LLVMValueRef llvm_zero = LLVMConstNull(LLVM_Type::u32);
        LLVMValueRef llvm_agg_idx = LLVMConstInt(LLVM_Type::u32, type_info->aggregate.first.id,
                                                 false);
        LLVMValueRef indices[] = { llvm_zero, llvm_agg_idx };
        unsigned index_count = STATIC_ARRAY_LENGTH(indices);
        LLVMValueRef llvm_agg_info_ptr = LLVMConstGEP(builder->aggregate_member_info_global,
                                                      indices, index_count);
        LLVMValueRef mem_vals[] = { llvm_member_count, llvm_agg_info_ptr };
        unsigned mem_count = STATIC_ARRAY_LENGTH(mem_vals);

        LLVMValueRef result = LLVMConstStruct(mem_vals, mem_count, false);
        result = LLVMConstStruct(&result, 1, false);

        return result;
    }

    LLVMValueRef llvm_emit_enum_info(LLVM_IR_Builder* builder, Type_Info* type_info)
    {
        LLVMValueRef llvm_zero = LLVMConstNull(LLVM_Type::u32);

        assert(BUF_LENGTH(builder->registered_type_infos) > (type_info->enum_info.base.id - 1));
        LLVMValueRef llvm_base_ti_ptr =
            builder->casted_type_infos[type_info->enum_info.base.id - 1];
        assert(llvm_base_ti_ptr);
        LLVMValueRef llvm_member_count = LLVMConstInt(LLVM_Type::u64,
                                                      type_info->enum_info.member_count, false);

        LLVMValueRef llvm_ei_index = LLVMConstInt(LLVM_Type::u32, type_info->enum_info.first.id,
                                                  false);
        LLVMValueRef info_idxs[] = { llvm_zero, llvm_ei_index };
        unsigned info_idx_count= STATIC_ARRAY_LENGTH(info_idxs);
        LLVMValueRef llvm_enum_info_ptr = LLVMConstGEP(builder->enum_member_info_global,
                                                       info_idxs, info_idx_count);

        LLVMValueRef mem_vals[] = { llvm_base_ti_ptr, llvm_member_count, llvm_enum_info_ptr };
        unsigned mem_count = STATIC_ARRAY_LENGTH(mem_vals);


        AST_Type* zir_ti_type = Builtin::type_Type_Info;
        AST_Declaration* ti_info_decl = zir_ti_type->aggregate_type.member_declarations[3];
        AST_Type* zir_ti_info_type = ti_info_decl->mutable_decl.type;
        AST_Declaration* enum_info_decl = zir_ti_info_type->aggregate_type.member_declarations[2];
        AST_Type* zir_enum_info_type = enum_info_decl->mutable_decl.type;
        LLVMTypeRef llvm_enum_info_type = llvm_type_from_ast(builder, zir_enum_info_type);

        LLVMValueRef result = LLVMConstNamedStruct(llvm_enum_info_type, mem_vals, mem_count);
        result = LLVMConstStruct(&result, 1, false);

        return result;
    }

    LLVMValueRef llvm_emit_function_info(LLVM_IR_Builder* builder, Type_Info* type_info)
    {
        auto return_idx = type_info->function.return_type.id - 1;
        assert(BUF_LENGTH(builder->registered_type_infos) > return_idx);
        LLVMValueRef llvm_return_ti_ptr = builder->casted_type_infos[return_idx];

        LLVMValueRef llvm_arg_count = LLVMConstInt(LLVM_Type::u64, type_info->function.arg_count,
                                                   false);

        LLVMValueRef llvm_zero = LLVMConstNull(LLVM_Type::u32);
        LLVMValueRef llvm_agg_idx = LLVMConstInt(LLVM_Type::u32, type_info->function.first_arg.id,
                                                 false);
        LLVMValueRef indices[] = { llvm_zero, llvm_agg_idx };
        unsigned index_count = STATIC_ARRAY_LENGTH(indices);
        LLVMValueRef llvm_agg_ptr = LLVMConstGEP(builder->aggregate_member_info_global,
                                                      indices, index_count);

        LLVMValueRef mem_vals[] = { llvm_return_ti_ptr, llvm_arg_count, llvm_agg_ptr };
        unsigned mem_count = STATIC_ARRAY_LENGTH(mem_vals);

        AST_Type* zir_ti_type = Builtin::type_Type_Info;
        AST_Declaration* ti_info_decl = zir_ti_type->aggregate_type.member_declarations[3];
        AST_Type* zir_ti_info_type = ti_info_decl->mutable_decl.type;
        AST_Declaration* function_info_decl =
            zir_ti_info_type->aggregate_type.member_declarations[3];
        AST_Type* zir_function_info_type = function_info_decl->mutable_decl.type;
        LLVMTypeRef llvm_function_info_type = llvm_type_from_ast(builder, zir_function_info_type);

        LLVMValueRef result = LLVMConstNamedStruct(llvm_function_info_type, mem_vals, mem_count);
        result = LLVMConstStruct(&result, 1, false);
        return result;
    }

    LLVMValueRef llvm_emit_constant_string(LLVM_IR_Builder* builder, const char* data,
                                           uint64_t length)
    {
        LLVMValueRef llvm_str_ptr = llvm_emit_c_string(builder, data, length);
        LLVMValueRef llvm_str_length = LLVMConstInt(LLVM_Type::u64, length, false);
        LLVMValueRef str_mem_vals[] = { llvm_str_ptr, llvm_str_length };
        unsigned str_mem_count = STATIC_ARRAY_LENGTH(str_mem_vals);

        LLVMTypeRef llvm_string_type = llvm_type_from_ast(builder, Builtin::type_String);
        LLVMValueRef llvm_str_val = LLVMConstNamedStruct(llvm_string_type, str_mem_vals,
                                                         str_mem_count);

        return llvm_str_val;
    }

    LLVMValueRef llvm_emit_c_string(LLVM_IR_Builder* builder, const char* data, uint64_t length)
    {
        if (!data || length == 0)
        {
            return LLVMConstNull(LLVM_Type::ptr_to_u8);
        }

        LLVMValueRef existing_string;
        Atom str_atom = atom_get(builder->context->atom_table, data, length);
        if (hash_table_find(&builder->const_c_string_table, str_atom, &existing_string))
        {
            return existing_string;
        }

        LLVMValueRef llvm_str = LLVMConstString(data, (unsigned)length, false);
        LLVMValueRef llvm_str_glob = LLVMAddGlobal(builder->llvm_module, LLVMTypeOf(llvm_str),
                                                   "_string_const");
        LLVMSetInitializer(llvm_str_glob, llvm_str);
        LLVMSetLinkage(llvm_str_glob, LLVMPrivateLinkage);
        LLVMSetUnnamedAddress(llvm_str_glob, LLVMGlobalUnnamedAddr);
        LLVMSetAlignment(llvm_str_glob, 1);
        LLVMSetGlobalConstant(llvm_str_glob, true);

        LLVMValueRef llvm_str_ptr = LLVMConstPointerCast(llvm_str_glob, LLVM_Type::ptr_to_u8);

        hash_table_push(&builder->const_c_string_table, str_atom, llvm_str_ptr);

        return llvm_str_ptr;
    }

    void llvm_register_ir_function(LLVM_IR_Builder* builder, IR_Function* zir_func)
    {
        LLVMTypeRef llvm_func_type = llvm_type_from_ast(builder, zir_func->type);

        const char* func_name = zir_func->name;
        LLVMValueRef llvm_func_value = LLVMAddFunction(builder->llvm_module, func_name,
                                                       llvm_func_type);

        LLVM_Registered_Function rf = { llvm_func_value, zir_func };
        BUF_PUSH(builder->registered_functions, rf);
    }

    void llvm_emit_ir_function(LLVM_IR_Builder* builder, IR_Function* zir_func)
    {
        LLVMValueRef llvm_func_value = llvm_function_from_zir(builder, zir_func);

        assert(stack_count(builder->arg_stack) == 0);

        assert(!builder->current_function);
        LLVM_IR_Function llvm_ir_function = {};
        builder->current_function = &llvm_ir_function;


        bool is_foreign = zir_func->flags & IR_FUNC_FLAG_FOREIGN;

        if (!is_foreign)
        {
            IR_Block* zir_block = zir_func->first_block;
            while (zir_block)
            {
                LLVMBasicBlockRef llvm_block = LLVMAppendBasicBlock(llvm_func_value,
                                                                    zir_block->name.data);
                LLVM_Block b = { llvm_block, zir_block };
                BUF_PUSH(llvm_ir_function.blocks, b);

                zir_block = zir_block->next;
            }

			assert(llvm_ir_function.blocks);
            LLVMPositionBuilderAtEnd(builder->llvm_builder, llvm_ir_function.blocks[0].block);

            for (uint64_t i = 0; i < BUF_LENGTH(zir_func->arguments); i++)
            {
                IR_Value* zir_arg = zir_func->arguments[i];
                LLVMTypeRef llvm_type = llvm_type_from_ast(builder, zir_arg->type);
                LLVMValueRef llvm_arg_alloca = LLVMBuildAlloca(builder->llvm_builder, llvm_type,
                                                            zir_arg->argument.name);
                LLVMValueRef llvm_arg_value = LLVMGetParam(llvm_func_value,
                                                        (unsigned)zir_arg->argument.index);
                LLVMBuildStore(builder->llvm_builder, llvm_arg_value, llvm_arg_alloca);
                llvm_assign_result(builder, zir_arg, llvm_arg_alloca);
            }

            zir_block = zir_func->first_block;
            for (uint64_t i = 0; i < BUF_LENGTH(llvm_ir_function.blocks); i++)
            {
                assert(zir_block);
                LLVMBasicBlockRef llvm_block = llvm_ir_function.blocks[i].block;

                llvm_emit_ir_block(builder, zir_block, llvm_block);

                zir_block = zir_block->next;
            }
        }
        else
        {
            // assert(false);
        }


        BUF_FREE(llvm_ir_function.blocks);

        builder->current_function = nullptr;
        assert(stack_count(builder->arg_stack) == 0);

        if (LLVMVerifyFunction(llvm_func_value, LLVMPrintMessageAction) == 1)
        {
            printf("\n%s", LLVMPrintValueToString(llvm_func_value));
            exit(-1);
        }
        else
        {
            // printf("\n%s\n", LLVMPrintValueToString(llvm_func_value));
        }
    }

    void llvm_emit_ir_block(LLVM_IR_Builder* builder, IR_Block* zir_block,
                            LLVMBasicBlockRef llvm_block)
    {
        LLVMPositionBuilderAtEnd(builder->llvm_builder, llvm_block);

        IR_Instruction* zir_instruction = zir_block->first_instruction;
        while (zir_instruction)
        {
            zir_instruction = llvm_emit_ir_instruction(builder, zir_instruction, llvm_block);
        }
    }

#define ASSERT_INT(a) {if (a) assert(a->type->flags & AST_TYPE_FLAG_INT || a->type->kind == AST_TYPE_ENUM); }

    IR_Instruction* llvm_emit_ir_instruction(LLVM_IR_Builder* builder,
                                             IR_Instruction* zir_instruction,
                                             LLVMBasicBlockRef llvm_block)
    {
        auto cf = builder->current_function;

        auto next = zir_instruction->next;

        auto a1 = zir_instruction->arg1;
        auto a2 = zir_instruction->arg2;
        auto r = zir_instruction->result;

        bool both_int = false;
        bool both_float = false;
        bool both_pointer = false;

        if (a1 && a2 && a1->type && a2->type)
        {
            if ((a1->type->flags & AST_TYPE_FLAG_INT) &&
                (a2->type->flags & AST_TYPE_FLAG_INT))
            {
                both_int = true;
            }
            else if ((a1->type->flags & AST_TYPE_FLAG_FLOAT) &&
                     (a2->type->flags & AST_TYPE_FLAG_FLOAT))
            {
                both_float = true;
            }
            else if (a1->type->kind == AST_TYPE_ENUM &&
                     a2->type->kind == AST_TYPE_ENUM)
            {
                assert(a1->type->aggregate_type.base_type->flags & AST_TYPE_FLAG_INT);
                assert(a2->type->aggregate_type.base_type->flags & AST_TYPE_FLAG_INT);
                both_int = true;
            }
            else if (a1->type->kind == AST_TYPE_POINTER &&
                     a2->type->kind == AST_TYPE_POINTER)
            {
                both_pointer = true;
            }
        }


        switch (zir_instruction->op)
        {

            case IR_OP_NOP:
            {
                assert(false);
                break;
            }

            case IR_OP_ADD:
            {
                LLVMValueRef lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                LLVMValueRef result = nullptr;

                if (both_int || both_pointer)
                {
                    result = LLVMBuildAdd(builder->llvm_builder, lhs, rhs, "");
                }
                else if (both_float)
                {
                    result = LLVMBuildFAdd(builder->llvm_builder, lhs, rhs, "");
                }
                else assert(false);

                assert(result);

                llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_SUB:
            {
                LLVMValueRef lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                LLVMValueRef result = nullptr;

                if (both_int || both_pointer)
                {
                    result = LLVMBuildSub(builder->llvm_builder, lhs, rhs, "");
                }
                else if (both_float)
                {
                    result = LLVMBuildFSub(builder->llvm_builder, lhs, rhs, "");
                }
                else assert(false);

                assert(result);

                llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_MUL:
            {
                LLVMValueRef lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                LLVMValueRef result = nullptr;
                if (both_int)
                {
                    result = LLVMBuildMul(builder->llvm_builder, lhs, rhs, "");
                }
                else if (both_float)
                {
                    result = LLVMBuildFMul(builder->llvm_builder, lhs, rhs, "");
                }
                else assert(false);

                assert(result);

                llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_MOD:
            {
				assert(a1);
				assert(a2);

                ASSERT_INT(a1);
                ASSERT_INT(a2);

                LLVMValueRef lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                bool is_signed = (zir_instruction->arg1->type->flags & AST_TYPE_FLAG_SIGNED) ||
                    (zir_instruction->arg2->type->flags & AST_TYPE_FLAG_SIGNED);

                LLVMValueRef result = nullptr;
                if (is_signed)
                {
                    result = LLVMBuildSRem(builder->llvm_builder, lhs, rhs, "");
                }
                else
                {
                    result = LLVMBuildURem(builder->llvm_builder, lhs, rhs, "");
                }

                llvm_assign_result(builder, r, result);

                break;
            }

            case IR_OP_DIV:
            {
                LLVMValueRef lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                LLVMValueRef result = nullptr;
                if (both_int)
                {
                    bool lhs_sign = a1->type->flags & AST_TYPE_FLAG_SIGNED;
                    bool rhs_sign = a2->type->flags & AST_TYPE_FLAG_SIGNED;
                    assert(lhs_sign == rhs_sign);

                    if (lhs_sign)
                    {
                        result = LLVMBuildSDiv(builder->llvm_builder, lhs, rhs, "");
                    }
                    else
                    {
                        result = LLVMBuildUDiv(builder->llvm_builder, lhs, rhs, "");
                    }
                }
                else if (both_float)
                {
                    result = LLVMBuildFDiv(builder->llvm_builder, lhs, rhs, "");
                }
                else assert(false);


                llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_LSHIFT:
            {
                assert(both_int);

                auto lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                auto rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                LLVMValueRef result = LLVMBuildShl(builder->llvm_builder, lhs, rhs, "");

                llvm_assign_result(builder, r, result);
                break;
            }

            case IR_OP_RSHIFT:
            {
                assert(both_int);

                auto lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                auto rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                LLVMValueRef result = LLVMBuildLShr(builder->llvm_builder, lhs, rhs, "");

                llvm_assign_result(builder, r, result);
                break;
            }

            case IR_OP_LT:
            {
                LLVMValueRef lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                LLVMValueRef result = nullptr;
                if (both_int)
                {
                    bool is_signed =
                        (zir_instruction->arg1->type->flags & AST_TYPE_FLAG_SIGNED) ||
                        (zir_instruction->arg2->type->flags & AST_TYPE_FLAG_SIGNED);
                    auto op = is_signed ? LLVMIntSLT : LLVMIntULT;
                    result = LLVMBuildICmp(builder->llvm_builder, op, lhs, rhs, "");
                }
                else if (both_float)
                {
                    result = LLVMBuildFCmp(builder->llvm_builder, LLVMRealOLT, lhs, rhs, "");
                }
                else assert(false);

                assert(result);

                auto bool_type = llvm_type_from_ast(builder, Builtin::type_bool);
                result = LLVMBuildCast(builder->llvm_builder, LLVMZExt, result, bool_type, "");
                llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_LTEQ:
            {
                LLVMValueRef lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                LLVMValueRef result = nullptr;
                if (both_int)
                {
                    bool is_signed =
                        (zir_instruction->arg1->type->flags & AST_TYPE_FLAG_SIGNED) ||
                        (zir_instruction->arg2->type->flags & AST_TYPE_FLAG_SIGNED);
                    auto op = is_signed ? LLVMIntSLE : LLVMIntULE;
                    result = LLVMBuildICmp(builder->llvm_builder, op, lhs, rhs, "");
                }
                else if (both_float)
                {
                    result = LLVMBuildFCmp(builder->llvm_builder, LLVMRealOLE, lhs, rhs, "");
                }
                else assert(false);

                auto bool_type = llvm_type_from_ast(builder, Builtin::type_bool);
                result = LLVMBuildCast(builder->llvm_builder, LLVMZExt, result, bool_type, ""); llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_GT:
            {
                LLVMValueRef lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                LLVMValueRef result = nullptr;
                if (both_int)
                {
                    bool is_signed =
                        (zir_instruction->arg1->type->flags & AST_TYPE_FLAG_SIGNED) ||
                        (zir_instruction->arg2->type->flags & AST_TYPE_FLAG_SIGNED);
                    auto op = is_signed ? LLVMIntSGT : LLVMIntUGT;
                   result = LLVMBuildICmp(builder->llvm_builder, op, lhs, rhs, "");
                }
                else if (both_float)
                {
                    result = LLVMBuildFCmp(builder->llvm_builder, LLVMRealOGT, lhs, rhs, "");
                }
                else assert(false);

                auto bool_type = llvm_type_from_ast(builder, Builtin::type_bool);
                result = LLVMBuildCast(builder->llvm_builder, LLVMZExt, result, bool_type, "");
                llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_GTEQ:
            {
                LLVMValueRef lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                LLVMValueRef result = nullptr;
                if (both_int)
                {
                    bool is_signed =
                        (zir_instruction->arg1->type->flags & AST_TYPE_FLAG_SIGNED) ||
                        (zir_instruction->arg2->type->flags & AST_TYPE_FLAG_SIGNED);
                    auto op = is_signed ? LLVMIntSGE : LLVMIntUGE;
                    result = LLVMBuildICmp(builder->llvm_builder, op, lhs, rhs, "");
                }
                else if (both_float)
                {
                    result = LLVMBuildFCmp(builder->llvm_builder, LLVMRealOGE, lhs, rhs, "");
                }
                else assert(false);

                assert(result);

                auto bool_type = llvm_type_from_ast(builder, Builtin::type_bool);
                result = LLVMBuildCast(builder->llvm_builder, LLVMZExt, result, bool_type, "");
                llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_EQ:
            {
				assert(a1);
				assert(a2);

                LLVMValueRef lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);
                LLVMValueRef result = nullptr;

                bool lhs_sign = a1->type->flags & AST_TYPE_FLAG_SIGNED;
                bool rhs_sign = a2->type->flags & AST_TYPE_FLAG_SIGNED;
                assert(lhs_sign == rhs_sign);

                if (both_int)
                {
                    llvm_cast_to_bigger_int_type(builder, &lhs, &rhs, lhs_sign, rhs_sign);
                    result = LLVMBuildICmp(builder->llvm_builder, LLVMIntEQ, lhs, rhs, "");
                }
                else if (both_float)
                {
                    result = LLVMBuildFCmp(builder->llvm_builder, LLVMRealOEQ, lhs, rhs, "");
                }
                else assert(false);

                auto bool_type = llvm_type_from_ast(builder, Builtin::type_bool);
                result = LLVMBuildCast(builder->llvm_builder, LLVMZExt, result, bool_type, "");

                llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_NEQ:
            {
                LLVMValueRef lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                LLVMValueRef result = nullptr;
                if (both_int || both_pointer)
                {
                    result = LLVMBuildICmp(builder->llvm_builder, LLVMIntNE, lhs, rhs, "");
                }
                else if (both_float)
                {
                    result = LLVMBuildFCmp(builder->llvm_builder, LLVMRealONE, lhs, rhs, "");
                }
                else assert(false);

                assert(result);

                auto bool_type = llvm_type_from_ast(builder, Builtin::type_bool);
                result = LLVMBuildCast(builder->llvm_builder, LLVMZExt, result, bool_type, "");

                llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_AND:
            {
				assert(a1);
				assert(a2);

                ASSERT_INT(a1);

                assert((a2->type->flags & AST_TYPE_FLAG_INT) ||
                       a2->type->kind == AST_TYPE_ENUM &&
                       a1->type == a2->type->aggregate_type.base_type);

                LLVMValueRef lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                if (both_int)
                {
                    bool lhs_sign = a1->type->flags & AST_TYPE_FLAG_SIGNED;
                    bool rhs_sign = a2->type->flags & AST_TYPE_FLAG_SIGNED;
                    llvm_cast_to_bigger_int_type(builder, &lhs, &rhs, lhs_sign, rhs_sign);
                }

                LLVMValueRef result = LLVMBuildAnd(builder->llvm_builder, lhs, rhs, "");

                llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_OR:
            {
                ASSERT_INT(a1);
                ASSERT_INT(a2);

                LLVMValueRef lhs = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef rhs = llvm_emit_ir_value(builder, zir_instruction->arg2);

                if (both_int)
                {
                    bool lhs_sign = a1->type->flags & AST_TYPE_FLAG_SIGNED;
                    bool rhs_sign = a2->type->flags & AST_TYPE_FLAG_SIGNED;
                    llvm_cast_to_bigger_int_type(builder, &lhs, &rhs, lhs_sign, rhs_sign);
                }

                LLVMValueRef result = LLVMBuildOr(builder->llvm_builder, lhs, rhs, "");

                llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_NOT: // This is boolean !false == true, !true == false
            {
                LLVMValueRef llvm_value = llvm_emit_ir_value(builder, a1);
                LLVMTypeRef llvm_value_type = LLVMTypeOf(llvm_value);
                auto type_size = LLVMGetIntTypeWidth(llvm_value_type);
                if (type_size != 1)
                {
                    llvm_value = LLVMBuildICmp(builder->llvm_builder, LLVMIntNE, llvm_value,
                                               LLVMConstNull(llvm_value_type), "");
                }
                LLVMValueRef llvm_result = LLVMBuildNot(builder->llvm_builder, llvm_value, "");

                auto bool_ty = llvm_type_from_ast(builder, Builtin::type_bool);
                llvm_result = LLVMBuildCast(builder->llvm_builder, LLVMZExt, llvm_result, bool_ty,
                                            "");
                llvm_assign_result(builder, r, llvm_result);
                break;
            }

            case IR_OP_PUSH_CALL_ARG:
            {
                assert(zir_instruction->arg1);
                stack_push(builder->arg_stack, zir_instruction->arg1);
                break;
            }

            case IR_OP_CALL_EX:
            case IR_OP_CALL:
            case IR_OP_CALL_PTR:
            {
				assert(a1);
				assert(a2);

                IR_Value* zir_func_value = zir_instruction->arg1;
                IR_Value* zir_arg_count = zir_instruction->arg2;

                LLVMValueRef llvm_func_value = llvm_emit_ir_value(builder, zir_func_value);

                bool is_vararg;
                bool is_foreign;

                if (zir_func_value->kind == IRV_FUNCTION)
                {
                    is_vararg = zir_func_value->function->flags & IR_FUNC_FLAG_VARARG;
                    is_foreign = zir_func_value->function->flags & IR_FUNC_FLAG_FOREIGN;
                }
                else
                {
                    assert(zir_func_value->type->kind == AST_TYPE_POINTER);
                    assert(zir_func_value->type->pointer.base->kind == AST_TYPE_FUNCTION);

                    AST_Type* func_type = zir_func_value->type->pointer.base;
                    is_vararg = func_type->flags & AST_TYPE_FLAG_FUNC_VARARG;
                    is_foreign = true;
                }

                BUF(LLVMValueRef) args = nullptr;

                assert(zir_arg_count->kind == IRV_INT_LITERAL);
                assert(zir_arg_count->type->flags & AST_TYPE_FLAG_INT);
                uint64_t arg_count = zir_arg_count->value.u64;

                for (uint64_t i = 0; i < arg_count; i++)
                {
                    uint64_t peek_offset = (arg_count - 1) - i;
                    IR_Value* zir_arg_value = stack_peek(builder->arg_stack, peek_offset);
                    LLVMValueRef llvm_arg_value = llvm_emit_ir_value(builder, zir_arg_value);

                    if (zir_arg_value->kind == IRV_ALLOCL)
                    {
                        llvm_arg_value = LLVMBuildLoad(builder->llvm_builder, llvm_arg_value, "");
                    }
                    if (is_vararg && is_foreign && zir_arg_value->type == Builtin::type_float)
                    {
                        llvm_arg_value = LLVMBuildFPCast(builder->llvm_builder, llvm_arg_value,
                                                         LLVMDoubleType(), "");
                    }

                    BUF_PUSH(args, llvm_arg_value);
                }
                for (uint64_t i = 0; i < arg_count; i++)
                {
                    stack_pop(builder->arg_stack);
                }

                LLVMValueRef llvm_result_value = LLVMBuildCall(builder->llvm_builder,
                                                               llvm_func_value, args, (unsigned)arg_count,
                                                               "");

                llvm_assign_result(builder, zir_instruction->result, llvm_result_value);
                BUF_FREE(args);
                break;
            }

            case IR_OP_ADDROF_FOREIGN:
            {
                LLVMValueRef llvm_func_value = llvm_emit_ir_value(builder, a1);
                llvm_assign_result(builder, r, llvm_func_value);
                break;
            }

            case IR_OP_ADDROF_FUNCTION:
            {
                LLVMValueRef llvm_func_value = llvm_emit_ir_value(builder, a1);
                llvm_assign_result(builder, r, llvm_func_value);
                break;
            }

            // case IR_OP_CALL_PTR:
            // {
            //     assert(false);
            //     break;
            // }

            case IR_OP_RETURN:
            {
                if (zir_instruction->arg1)
                {
                    LLVMValueRef llvm_return_value = llvm_emit_ir_value(builder,
                                                                        zir_instruction->arg1);
                    if (a1->type == Builtin::type_bool)
                    {
                        auto llvm_ret_type = llvm_type_from_ast(builder, Builtin::type_bool);
                        llvm_return_value = LLVMBuildIntCast(builder->llvm_builder,
                                                             llvm_return_value, llvm_ret_type,
                                                             "");
                    }
                    LLVMBuildRet(builder->llvm_builder, llvm_return_value);
                }
                else
                {
                    LLVMBuildRetVoid(builder->llvm_builder);
                }
                break;
            }

            case IR_OP_JMP:
            {
				assert(a1);

                IR_Block* zir_dest_block = zir_instruction->arg1->block;
                LLVMBasicBlockRef llvm_dest_block = llvm_block_from_zir(builder, zir_dest_block);
                LLVMBuildBr(builder->llvm_builder, llvm_dest_block);
                break;
            }

            case IR_OP_JMP_IF:
            {
				assert(a1);
				assert(a2);

                LLVMValueRef llvm_cond_value = llvm_emit_ir_value(builder,
                                                                  zir_instruction->arg1);

                auto ty = llvm_type_from_ast(builder, a1->type);
                if (a1->type->kind == AST_TYPE_POINTER)
                {
                    llvm_cond_value = LLVMBuildICmp(builder->llvm_builder, LLVMIntNE,
                                                    llvm_cond_value, LLVMConstNull(ty), "");
                }
                else
                {
                    LLVMTypeRef llvm_cond_type = LLVMTypeOf(llvm_cond_value);
                    auto cond_type_size = LLVMGetIntTypeWidth(llvm_cond_type);

                    if (cond_type_size != 1)
                    {
                        llvm_cond_value = LLVMBuildICmp(builder->llvm_builder, LLVMIntNE,
                                                        llvm_cond_value, LLVMConstNull(ty), "");
                    }
                }
                IR_Block* zir_then_block = zir_instruction->arg2->block;
                LLVMBasicBlockRef llvm_then_block = llvm_block_from_zir(builder, zir_then_block);

                auto next_ziri = zir_instruction->next;
                assert(next_ziri);
                if (next_ziri->op == IR_OP_JMP)
                {
                    IR_Block* zir_else_block = next_ziri->arg1->block;
                    LLVMBasicBlockRef llvm_else_block = llvm_block_from_zir(builder,
                                                                            zir_else_block);
                    LLVMBuildCondBr(builder->llvm_builder, llvm_cond_value, llvm_then_block,
                                    llvm_else_block);

                    next = next_ziri->next;
                }

                break;
            }

            case IR_OP_SWITCH:
            {
				assert(a1);
				assert(a2);

                unsigned num_cases = (unsigned)BUF_LENGTH(zir_instruction->case_pairs);
                LLVMValueRef llvm_switch_value = llvm_emit_ir_value(builder, a1);
                LLVMBasicBlockRef llvm_else_block = llvm_block_from_zir(builder, a2->block);
                LLVMValueRef llvm_switch = LLVMBuildSwitch(builder->llvm_builder,
                                                           llvm_switch_value, llvm_else_block,
                                                           num_cases);

                for (uint64_t i = 0; i < BUF_LENGTH(zir_instruction->case_pairs); i++)
                {
                    const auto& case_pair = zir_instruction->case_pairs[i];
                    LLVMValueRef llvm_on_val = llvm_emit_ir_value(builder, case_pair.value);
                    IR_Block* zir_dest_block = case_pair.dest_block_value->block;
                    LLVMBasicBlockRef llvm_dest_block = llvm_block_from_zir(builder,
                                                                            zir_dest_block);
                    LLVMAddCase(llvm_switch, llvm_on_val, llvm_dest_block);
                }
                break;
            }

            case IR_OP_ALLOCL:
            {
				assert(r);

                IR_Value* zir_allocl = zir_instruction->result;
                const char* name = zir_allocl->allocl.name;
                LLVMTypeRef llvm_type = llvm_type_from_ast(builder, zir_allocl->type);
                LLVMValueRef llvm_alloca = LLVMBuildAlloca(builder->llvm_builder, llvm_type,
                                                           name);

                llvm_assign_result(builder, zir_allocl, llvm_alloca);
                break;
            }

            case IR_OP_STOREL:
            {
				assert(a1);
				assert(a2);

                IR_Value* zir_allocl = zir_instruction->arg1;
                IR_Value* zir_new_value = zir_instruction->arg2;
                LLVMValueRef llvm_dest_value = llvm_value_from_zir(builder, zir_allocl);
                LLVMValueRef llvm_new_value = llvm_emit_ir_value(builder, zir_new_value);
                assert(llvm_new_value);

                if (a2->kind == IRV_ALLOCL)
                {
                    llvm_new_value = LLVMBuildLoad(builder->llvm_builder, llvm_new_value, "");
                }
                LLVMBuildStore(builder->llvm_builder, llvm_new_value, llvm_dest_value);
                break;
            }

            case IR_OP_LOADL:
            {
                IR_Value* zir_allocl = zir_instruction->arg1;
                IR_Value* zir_result = zir_instruction->result;
                LLVMValueRef llvm_source_value = llvm_value_from_zir(builder, zir_allocl);
                LLVMValueRef llvm_result_value = LLVMBuildLoad(builder->llvm_builder,
                                                               llvm_source_value, "");
                llvm_assign_result(builder, zir_result, llvm_result_value);
                break;
            }

            case IR_OP_STOREA:
            {
                LLVMValueRef llvm_dest = llvm_emit_ir_value(builder, a1);
                LLVMValueRef llvm_new_value = llvm_emit_ir_value(builder, a2);

                LLVMBuildStore(builder->llvm_builder, llvm_new_value, llvm_dest);
                break;
            }

            case IR_OP_LOADA:
            {
                assert(zir_instruction->arg1);
                IR_Value* zir_arg = zir_instruction->arg1;
                assert(zir_arg->kind == IRV_ARGUMENT);

                // LLVMValueRef llvm_arg_value = cf->locals[zir_arg->argument.index];
                LLVMValueRef llvm_arg_value = llvm_value_from_zir(builder, zir_arg);
                LLVMValueRef llvm_tmp = LLVMBuildLoad(builder->llvm_builder, llvm_arg_value,
                                                      zir_arg->argument.name);

                llvm_assign_result(builder, zir_instruction->result, llvm_tmp);
                break;
            }

            case IR_OP_STOREP:
            {
				assert(a1);
				assert(a2);

                LLVMValueRef llvm_dest = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef llvm_new_value = llvm_emit_ir_value(builder, zir_instruction->arg2);

                if (a2->kind == IRV_ALLOCL)
                {
                    llvm_new_value = LLVMBuildLoad(builder->llvm_builder, llvm_new_value, "");
                }
                LLVMBuildStore(builder->llvm_builder, llvm_new_value, llvm_dest);
                break;
            }

            case IR_OP_LOADP:
            {
                LLVMValueRef llvm_source = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef llvm_result = LLVMBuildLoad(builder->llvm_builder, llvm_source, "");

                llvm_assign_result(builder, zir_instruction->result, llvm_result);
                break;
            }

            case IR_OP_STOREG:
            {
                LLVMValueRef llvm_dest = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef llvm_new_value = llvm_emit_ir_value(builder, zir_instruction->arg2);

                LLVMBuildStore(builder->llvm_builder, llvm_new_value, llvm_dest);
                break;
            }

            case IR_OP_LOADG:
            {
                LLVMValueRef llvm_source = llvm_emit_ir_value(builder, zir_instruction->arg1);
                LLVMValueRef llvm_result = LLVMBuildLoad(builder->llvm_builder, llvm_source, "");
                llvm_assign_result(builder, zir_instruction->result, llvm_result);
                break;
            }

            case IR_OP_ADDROF:
            {
				assert(a1);

                switch (a1->kind)
                {
                    case IRV_ALLOCL:
                    case IRV_GLOBAL:
					case IRV_ARGUMENT:
                    {
                        LLVMValueRef llvm_allocl = llvm_emit_ir_value(builder, a1);
                        llvm_assign_result(builder, r, llvm_allocl);
                        break;
                    }

                    default: assert(false);
                }
                break;
            }

            case IR_OP_DEREF:
            {
				assert(a1);

                LLVMValueRef llvm_source = llvm_emit_ir_value(builder, a1);
                LLVMTypeRef llvm_index_type = llvm_type_from_ast(builder, Builtin::type_u32);
                if (a1->kind == IRV_ARGUMENT || a1->kind == IRV_ALLOCL)
                {
                    llvm_source = LLVMBuildLoad(builder->llvm_builder, llvm_source, "");
                }
                LLVMValueRef result = LLVMBuildLoad(builder->llvm_builder, llvm_source, "");
                llvm_assign_result(builder, r, result);
                break;
            }

            case IR_OP_ARRAY_OFFSET_POINTER:
            {
				assert(a1);
				assert(a2);

                LLVMValueRef llvm_source = llvm_emit_ir_value(builder, a1);
                LLVMValueRef llvm_index = llvm_emit_ir_value(builder, a2);
                LLVMValueRef indices[] = { llvm_index };
                uint64_t index_count = STATIC_ARRAY_LENGTH(indices);
                assert(r->type->kind == AST_TYPE_POINTER);
                LLVMTypeRef llvm_dest_type = llvm_type_from_ast(builder, r->type->pointer.base);
                LLVMTypeRef llvm_pointer_dest_type = LLVMPointerType(llvm_dest_type, 0);
                if (a1->type->kind == AST_TYPE_STATIC_ARRAY ||
                    (a1->type->kind == AST_TYPE_POINTER &&
                     a1->type->pointer.base->kind == AST_TYPE_STATIC_ARRAY))
                {
                    LLVMTypeRef llvm_index_type = llvm_type_from_ast(builder, Builtin::type_u32);
                    LLVMValueRef llvm_zero = LLVMConstNull(llvm_index_type);
                    llvm_source = LLVMBuildInBoundsGEP(builder->llvm_builder, llvm_source,
                                                       &llvm_zero, 1, "");

                    llvm_source = LLVMBuildPointerCast(builder->llvm_builder, llvm_source,
                                                       llvm_pointer_dest_type, "");
                }
                LLVMValueRef result = LLVMBuildInBoundsGEP(builder->llvm_builder, llvm_source,
                                                           indices, (unsigned)index_count, "");
                llvm_assign_result(builder, r, result);
                break;
            }

            case IR_OP_AGGREGATE_OFFSET_POINTER:
            {
				assert(a1);
				assert(a2);

                IR_Value* zir_source = zir_instruction->arg1;
                LLVMValueRef llvm_source = llvm_emit_ir_value(builder, zir_source);

                bool is_union = false;
                if (zir_source->type->kind == AST_TYPE_POINTER)
                {
                    if (zir_source->type->pointer.base->kind == AST_TYPE_UNION)
                    {
                        is_union = true;
                    }
                }
                else
                {
                    assert(zir_source->kind == IRV_ALLOCL || zir_source->kind == IRV_ARGUMENT);
                    assert(zir_source->type->kind == AST_TYPE_STRUCT ||
                           zir_source->type->kind == AST_TYPE_UNION);

                    is_union = true;
                }

                IR_Value* zir_index = zir_instruction->arg2;
                LLVMTypeRef llvm_index_type = llvm_type_from_ast(builder, Builtin::type_u32);
                assert(zir_index->kind == IRV_INT_LITERAL);
                LLVMValueRef llvm_index = LLVMConstInt(llvm_index_type, zir_index->value.u32,
                                                       false);
                LLVMValueRef llvm_zero_value = LLVMConstInt(llvm_index_type, 0, false);
                LLVMValueRef indices[] = { llvm_zero_value, llvm_index };
                uint64_t index_count = 2;

                 // char* llvm_source_value_str = LLVMPrintValueToString(llvm_source);
                 // char* llvm_source_type_str = LLVMPrintTypeToString(LLVMTypeOf(llvm_source));
                 // printf("----> llvm_source: %s\n", llvm_source_value_str);
                 // printf("----> llvm_source_type_str: %s\n", llvm_source_type_str);

                 // char* llvm_index_type_str = LLVMPrintTypeToString(llvm_index_type);
                 // printf("--> llvm_index_type: %s\n", llvm_index_type_str);

                LLVMValueRef result = LLVMBuildGEP(builder->llvm_builder, llvm_source, indices,
                                                    (unsigned)index_count, "");

                 // char* llvm_result_type_str = LLVMPrintTypeToString(LLVMTypeOf(result));
                 // printf("--> llvm_result_type_str: %s\n", llvm_result_type_str);

                 // LLVMDisposeMessage(llvm_source_type_str);
                 // LLVMDisposeMessage(llvm_index_type_str);
                 // LLVMDisposeMessage(llvm_result_type_str);

                 if (is_union)
                 {
                     LLVMTypeRef llvm_dest_ptr_ty = llvm_type_from_ast(builder, r->type);
                     result = LLVMBuildPointerCast(builder->llvm_builder, result,
                                                   llvm_dest_ptr_ty, "");
                 }

                llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_CAST:
            {
				assert(a1);
				assert(r);

                AST_Type* target_ast_type = zir_instruction->result->type;
                LLVMTypeRef llvm_dest_type = llvm_type_from_ast(builder, target_ast_type);
                bool is_integer = target_ast_type->flags & AST_TYPE_FLAG_INT;
                bool is_float = target_ast_type->flags & AST_TYPE_FLAG_FLOAT;
                bool is_pointer = target_ast_type->kind == AST_TYPE_POINTER;
                bool target_sign = target_ast_type->flags & AST_TYPE_FLAG_SIGNED;
                bool is_enum = target_ast_type->kind == AST_TYPE_ENUM;

                IR_Value* zir_value = zir_instruction->arg1;
                LLVMValueRef llvm_value = llvm_emit_ir_value(builder, zir_value);

                LLVMValueRef result = nullptr;

                if (target_ast_type == Builtin::type_bool)
                {
                    LLVMValueRef llvm_zero = LLVMConstNull(LLVMTypeOf(llvm_value));
                    result = LLVMBuildICmp(builder->llvm_builder, LLVMIntNE, llvm_value,
                                           llvm_zero, "");
                    LLVMTypeRef llvm_bool_type = llvm_type_from_ast(builder, Builtin::type_bool);
                    result = LLVMBuildCast(builder->llvm_builder, LLVMZExt, result,
                                           llvm_bool_type, "");
                }
                else if (is_integer || is_enum)
                {
                    if (zir_value->type->flags & AST_TYPE_FLAG_FLOAT)
                    {
                        LLVMOpcode op = target_sign ? LLVMFPToSI : LLVMFPToUI;
                        result = LLVMBuildCast(builder->llvm_builder, op, llvm_value,
                                               llvm_dest_type, "");
                    }
                    else if (zir_value->type->kind == AST_TYPE_POINTER)
                    {
                        result = LLVMBuildPtrToInt(builder->llvm_builder, llvm_value,
                                                   llvm_dest_type, "");
                    }
                    else if (zir_value->type->kind == AST_TYPE_STRUCT ||
                             zir_value->type->kind == AST_TYPE_UNION)
                    {
                        result = LLVMBuildBitCast(builder->llvm_builder, llvm_value,
                                                  llvm_dest_type, "");
                    }
                    else
                    {
                        result = LLVMBuildIntCast2(builder->llvm_builder, llvm_value,
                                                   llvm_dest_type, target_sign, "");
                    }
                }
                else if (is_float)
                {
                    if (zir_value->type->flags & AST_TYPE_FLAG_INT)
                    {
                        LLVMOpcode op = target_sign ? LLVMSIToFP : LLVMUIToFP;
                        result = LLVMBuildCast(builder->llvm_builder, op, llvm_value,
                                               llvm_dest_type, "");
                    }
                    else
                    {
                        result = LLVMBuildFPCast(builder->llvm_builder, llvm_value,
                                                 llvm_dest_type, "");
                    }
                }
                else if (is_pointer)
                {
                    if (zir_value->type->flags & AST_TYPE_FLAG_INT)
                    {
                        result = LLVMBuildIntToPtr(builder->llvm_builder, llvm_value,
                                                   llvm_dest_type, "");
                    }
                    else
                    {
                        result = LLVMBuildPointerCast(builder->llvm_builder, llvm_value,
                                                      llvm_dest_type, "");
                    }
                }
                else
                {
                    assert(false);
                }

                assert(result);

                llvm_assign_result(builder, zir_instruction->result, result);
                break;
            }

            case IR_OP_ASSERT_FAIL:
            {
                assert(false);
                // LLVMTypeRef llvm_int_ty = llvm_type_from_ast(builder, Builtin::type_int);
                // LLVMTypeRef llvm_int_ptr_ty = LLVMPointerType(llvm_int_ty, 0);
                // LLVMValueRef llvm_null_ptr = LLVMConstNull(llvm_int_ptr_ty);
                // LLVMValueRef llvm_one_val = LLVMConstInt(llvm_int_ty, 1, false);
                // LLVMBuildStore(builder->llvm_builder, llvm_one_val, llvm_null_ptr);
                break;
            }

            case IR_OP_PHI:
            {
                LLVMTypeRef llvm_type = llvm_type_from_ast(builder, r->type);
                bool is_int = r->type->flags & AST_TYPE_FLAG_INT;

                BUF(LLVMValueRef) pair_llvm_values = nullptr;

                for (uint64_t i = 0; i < BUF_LENGTH(zir_instruction->phi_pairs); i++)
                {
                    IR_Value* zir_value = zir_instruction->phi_pairs[i].value_to_load;
                    LLVMValueRef llvm_value = llvm_emit_ir_value(builder, zir_value);

                    BUF_PUSH(pair_llvm_values, llvm_value);
                }

                LLVMValueRef llvm_phi = LLVMBuildPhi(builder->llvm_builder, llvm_type, "");


                for (uint64_t i = 0; i < BUF_LENGTH(zir_instruction->phi_pairs); i++)
                {
                    auto& pair = zir_instruction->phi_pairs[i];
                    LLVMValueRef llvm_value = pair_llvm_values[i];
                    LLVMBasicBlockRef llvm_block = llvm_block_from_zir(builder, pair.from_block);
                    LLVMAddIncoming(llvm_phi, &llvm_value, &llvm_block, 1);
                }

                BUF_FREE(pair_llvm_values);

                llvm_assign_result(builder, r, llvm_phi);
                break;
            }

            case IR_OP_GET_TYPE_INFO:
            {
                assert(builder->type_info_global);
				assert(a1);

                LLVMTypeRef llvm_target_type = llvm_type_from_ast(builder, r->type);
                LLVMTypeRef idx_type = llvm_type_from_ast(builder, Builtin::type_u32);
                LLVMValueRef llvm_idx = LLVMConstInt(idx_type, a1->value.u32 - 1, false);
                LLVMValueRef llvm_zero = LLVMConstNull(idx_type);

                LLVMValueRef result = LLVMBuildInBoundsGEP(builder->llvm_builder,
                                                           builder->type_info_global,
                                                           &llvm_zero, 1, "");
                result = LLVMBuildPointerCast(builder->llvm_builder, result,
                                              LLVMPointerType(llvm_target_type, 0), "");
                result = LLVMBuildInBoundsGEP(builder->llvm_builder, result, &llvm_idx, 1, "");
                result = LLVMBuildLoad(builder->llvm_builder, result, "");

                llvm_assign_result(builder, r, result);
                break;
            }

            case IR_OP_CREATE_THREAD:
            {
                assert(false);
                break;
            }

            case IR_OP_JOIN_THREAD:
            {
                assert(false);
                break;
            }

            case IR_OP_COMPARE_AND_SWAP:
            {
                assert(stack_count(builder->arg_stack));
                IR_Value* zir_pointer = stack_pop(builder->arg_stack);
                LLVMValueRef llvm_pointer = llvm_emit_ir_value(builder, zir_pointer);

                LLVMValueRef llvm_old_value = llvm_emit_ir_value(builder, a1);
                LLVMValueRef llvm_new_value = llvm_emit_ir_value(builder, a2);

                LLVMValueRef result = LLVMBuildAtomicCmpXchg(builder->llvm_builder, llvm_pointer,
                                                             llvm_old_value, llvm_new_value,
                                                             LLVMAtomicOrderingMonotonic,
                                                             LLVMAtomicOrderingMonotonic, false);
                result = LLVMBuildExtractValue(builder->llvm_builder, result, 1, "");

                llvm_assign_result(builder, r, result);
                break;
            }

            default: assert(false);
        }

        return next;
    }

    LLVMValueRef llvm_emit_ir_value(LLVM_IR_Builder* builder, IR_Value* zir_value)
    {
        switch (zir_value->kind)
        {
            case IRV_ALLOCL:
            case IRV_ARGUMENT:
            case IRV_TEMPORARY:
            case IRV_GLOBAL:
            {
                return llvm_value_from_zir(builder, zir_value);
                break;
            }

            case IRV_FUNCTION:
            {
                if (zir_value->kind == IRV_FUNCTION)
                {
                    return llvm_function_from_zir(builder, zir_value);
                }
                else if (zir_value->type->kind == AST_TYPE_POINTER &&
                         zir_value->type->pointer.base->kind == AST_TYPE_FUNCTION)
                {
                    return llvm_value_from_zir(builder, zir_value);
                }
                else assert(false);
                break;
            }

            case IRV_BLOCK:
            {
                assert(false);
                break;
            }

            case IRV_BOOL_LITERAL:
            {
                auto llvm_type = llvm_type_from_ast(builder, zir_value->type);
                return LLVMConstInt(llvm_type, zir_value->value.boolean, false);
                break;
            }

            case IRV_NULL_LITERAL:
            {
                auto llvm_type = llvm_type_from_ast(builder, zir_value->type);
                return LLVMConstNull(llvm_type);
                break;
            }

            case IRV_STRING_LITERAL:
            {
                char* str = (char*)zir_value->value.pointer;
                return llvm_emit_c_string(builder, str, strlen(str));

                break;
            }

            case IRV_INT_LITERAL:
            {
                LLVMTypeRef llvm_int_type = llvm_type_from_ast(builder, zir_value->type);
                return LLVMConstInt(llvm_int_type, zir_value->value.u64, false);
                break;
            }

            case IRV_FLOAT_LITERAL:
            {
                auto zir_type = zir_value->type;
                LLVMTypeRef llvm_real_type = llvm_type_from_ast(builder, zir_type);
                if (zir_type == Builtin::type_float)
                    return LLVMConstReal(llvm_real_type, zir_value->value.r32);
                else if (zir_type == Builtin::type_double)
                    return LLVMConstReal(llvm_real_type, zir_value->value.r64);
                else assert(false);
                break;
            }

            case IRV_CHAR_LITERAL:
            {
                LLVMTypeRef llvm_char_type = llvm_type_from_ast(builder, zir_value->type);
                return LLVMConstInt(llvm_char_type, zir_value->value.u8, false);
                break;
            }

            case IRV_ARRAY_LITERAL:
            {
                assert(false);
                break;
            }

            case IRV_AGGREGATE_LITERAL:
            {
                LLVMTypeRef llvm_agg_type = llvm_type_from_ast(builder, zir_value->type);

                if (zir_value->flags & IRV_FLAG_CONST)
                {
                    BUF(LLVMValueRef) member_vals = nullptr;
                    for (uint64_t i = 0; i < BUF_LENGTH(zir_value->value.compound_values); i++)
                    {
                        IR_Value* zir_compound_member = zir_value->value.compound_values[i];
                        LLVMValueRef llvm_compound_member =
                            llvm_emit_ir_value(builder, zir_compound_member);
                        BUF_PUSH(member_vals, llvm_compound_member);
                    }

                    auto result = LLVMConstNamedStruct(llvm_agg_type, member_vals,
                                                    (unsigned)BUF_LENGTH(member_vals));
                    BUF_FREE(member_vals);
                    return result;
                }
                else
                {
                    LLVMValueRef agg_value = LLVMConstNull(llvm_agg_type);

                    for (uint64_t i = 0; i < BUF_LENGTH(zir_value->value.compound_values); i++)
                    {
                        IR_Value* zir_compound_member = zir_value->value.compound_values[i];
                        LLVMValueRef llvm_compound_member =
                            llvm_emit_ir_value(builder, zir_compound_member);

                        agg_value = LLVMBuildInsertValue(builder->llvm_builder, agg_value,
                                             llvm_compound_member, (unsigned)i, "");
                    }

                    return agg_value;
                }
                break;
            }

            default: assert(false);
        }

		assert(false);
		return nullptr;
    }

    void llvm_emit_global(LLVM_IR_Builder* builder, IR_Value* zir_global)
    {
        assert(zir_global->kind == IRV_GLOBAL);

        auto llvm_type = llvm_type_from_ast(builder, zir_global->type);
        auto name = zir_global->global.name;

        LLVMValueRef llvm_global = LLVMAddGlobal(builder->llvm_module, llvm_type, name);
        LLVMSetExternallyInitialized(llvm_global, false);

        LLVMValueRef llvm_init_value = nullptr;
        if (zir_global->global.init_value)
        {
            llvm_init_value = llvm_emit_ir_value(
                builder, zir_global->global.init_value);
        }
        else
        {
            llvm_init_value = LLVMConstNull(llvm_type);
        }
        assert(llvm_init_value);
        LLVMSetInitializer(llvm_global, llvm_init_value);

        llvm_assign_result(builder, zir_global, llvm_global);
    }

    void llvm_cast_to_bigger_int_type(LLVM_IR_Builder* builder, LLVMValueRef* lhs,
                                      LLVMValueRef* rhs, bool lhs_sign, bool rhs_sign)
    {
        LLVMTypeRef lhs_type = LLVMTypeOf(*lhs);
        LLVMTypeRef rhs_type = LLVMTypeOf(*rhs);

        auto lhs_size = LLVMGetIntTypeWidth(lhs_type);
        auto rhs_size = LLVMGetIntTypeWidth(rhs_type);

        assert(lhs_sign == rhs_sign);
        LLVMOpcode op = lhs_sign ? LLVMSExt : LLVMZExt;

        if (lhs_size > rhs_size)
        {
            *rhs = LLVMBuildCast(builder->llvm_builder, op, *rhs, lhs_type, "");
        }
        else if (rhs_size > lhs_size)
        {
            *lhs = LLVMBuildCast(builder->llvm_builder, op, *lhs, rhs_type, "");
        }
    }

    void llvm_assign_result(LLVM_IR_Builder* builder, IR_Value* zir_value,
                            LLVMValueRef llvm_value)
    {
        for (uint64_t i = 0; i < BUF_LENGTH(builder->assigned_values); i++)
        {
            LLVM_Assigned_Value& av = builder->assigned_values[i];
            assert(av.zir_value != zir_value);
            // assert(av.llvm_value != llvm_value);
        }

        LLVM_Assigned_Value av = { llvm_value, zir_value };
        BUF_PUSH(builder->assigned_values, av);
    }

    LLVMValueRef llvm_value_from_zir(LLVM_IR_Builder* builder, IR_Value* zir_value)
    {
        for (uint64_t i = 0; i < BUF_LENGTH(builder->assigned_values); i++)
        {
            auto& av = builder->assigned_values[i];
            if (av.zir_value == zir_value) return av.llvm_value;
        }

        assert(false);
        return nullptr;
    }

    LLVMValueRef llvm_function_from_zir(LLVM_IR_Builder* builder, IR_Value* zir_function_value)
    {
        assert(zir_function_value->kind == IRV_FUNCTION);

        IR_Function* zir_function = zir_function_value->function;
        return llvm_function_from_zir(builder, zir_function);
    }

    LLVMValueRef llvm_function_from_zir(LLVM_IR_Builder* builder, IR_Function* zir_function)
    {
        for (uint64_t i = 0; i < BUF_LENGTH(builder->registered_functions); i++)
        {
            auto& rf = builder->registered_functions[i];
            if (rf.zir_func == zir_function)
            {
                return rf.func;
            }
        }

        fprintf(stderr, "Did not find function: %s\n", zir_function->name);
        assert(false);
        return nullptr;
    }

    LLVMBasicBlockRef llvm_block_from_zir(LLVM_IR_Builder* builder, IR_Block* zir_block)
    {
        for (uint64_t i = 0; i < BUF_LENGTH(builder->current_function->blocks); i++)
        {
            auto& lb = builder->current_function->blocks[i];
            if (lb.zir_block == zir_block) return lb.block;
        }

        assert(false);
        return nullptr;
    }

    LLVMTypeRef llvm_type_from_ast(LLVM_IR_Builder* builder, AST_Type* zir_type)
    {
        assert(zir_type);

        switch (zir_type->kind)
        {
            case AST_TYPE_BASE:
            {
                bool is_integer = zir_type->flags & AST_TYPE_FLAG_INT;
                bool is_signed = zir_type->flags & AST_TYPE_FLAG_SIGNED;
                bool is_void = zir_type->flags & AST_TYPE_FLAG_VOID;
                bool is_float = zir_type->flags & AST_TYPE_FLAG_FLOAT;

                if (is_integer)
                {
                    return LLVMIntType((unsigned)zir_type->bit_size);
                }
                else if (is_void)
                {
                    return LLVMVoidType();
                }
                else if (is_float)
                {
                    if (zir_type->bit_size == 32) return LLVMFloatType();
                    else if (zir_type->bit_size == 64) return LLVMDoubleType();
                    else assert(false);
                }
                else assert(false);
                break;
            }

            case AST_TYPE_POINTER:
            {
                if (zir_type->pointer.base == Builtin::type_void)
                {
                    return llvm_type_from_ast(builder, Builtin::type_pointer_to_u8);
                }
                else
                {
                    LLVMTypeRef base = llvm_type_from_ast(builder, zir_type->pointer.base);
                    return LLVMPointerType(base, 0);
                }
                break;
            }

            case AST_TYPE_STATIC_ARRAY:
            {
                LLVMTypeRef llvm_elem_type = llvm_type_from_ast(builder,
                                                                zir_type->static_array.base);
                uint64_t elem_count = zir_type->static_array.count;
                return LLVMArrayType(llvm_elem_type, (unsigned)elem_count);
                break;
            }

            case AST_TYPE_STRUCT:
            {
                auto registered_type = llvm_find_registered_type(builder, zir_type);
                if (registered_type) return registered_type->type;

                return llvm_aggregate_type_from_ast(builder, zir_type);
                break;
            }

            case AST_TYPE_UNION:
            {
                auto registered_type = llvm_find_registered_type(builder, zir_type);
                if (registered_type) return registered_type->type;

                return llvm_aggregate_type_from_ast(builder, zir_type);
                break;
            }

            case AST_TYPE_ENUM:
            {
                return llvm_type_from_ast(builder, zir_type->aggregate_type.base_type);
                break;
            }

            case AST_TYPE_FUNCTION:
            {
                LLVMTypeRef llvm_return_type = llvm_type_from_ast(builder,
                                                                  zir_type->function.return_type);
                BUF(LLVMTypeRef) llvm_arg_types = nullptr;
                for (uint64_t i = 0; i < BUF_LENGTH(zir_type->function.arg_types); i++)
                {
                    AST_Type* zodiac_arg_type = zir_type->function.arg_types[i];
                    LLVMTypeRef llvm_arg_type = llvm_type_from_ast(builder, zodiac_arg_type);
                    BUF_PUSH(llvm_arg_types, llvm_arg_type);
                }
                bool is_vararg = (zir_type->flags & AST_TYPE_FLAG_FUNC_VARARG);

                LLVMTypeRef result = LLVMFunctionType(llvm_return_type, llvm_arg_types,
                                                      (unsigned)BUF_LENGTH(llvm_arg_types), is_vararg);
                BUF_FREE(llvm_arg_types);

                return result;
                break;
            }

            default: assert(false);
        }

		assert(false);
		return	nullptr;
    }

    LLVMTypeRef llvm_aggregate_type_from_ast(LLVM_IR_Builder* builder, AST_Type* ag_type)
    {
        assert(ag_type->kind == AST_TYPE_STRUCT ||
               ag_type->kind == AST_TYPE_UNION);

        llvm_register_aggregate_types(builder, ag_type);
        LLVMTypeRef result = llvm_finalize_aggregate_types(builder, ag_type);

        return result;
    }

    void llvm_register_aggregate_types(LLVM_IR_Builder* builder, AST_Type* ag_type)
    {
        assert(ag_type->kind == AST_TYPE_STRUCT ||
               ag_type->kind == AST_TYPE_UNION);

        for (uint64_t i = 0; i < BUF_LENGTH(builder->registered_aggregates); i++)
        {
            if (builder->registered_aggregates[i].zir_type == ag_type)
            {
                return;
            }
        }

        auto members = ag_type->aggregate_type.member_declarations;
        for (uint64_t i = 0; i < BUF_LENGTH(members); i++)
        {
            AST_Declaration* member_decl = members[i];
            AST_Type* member_type = member_decl->mutable_decl.type;

            if (member_type->kind == AST_TYPE_STRUCT || member_type->kind == AST_TYPE_UNION)
            {
                llvm_register_aggregate_types(builder, member_type);
            }
        }

        LLVMTypeRef llvm_type = nullptr;
        if (ag_type->name)
        {
            llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), ag_type->name);
        }
        else
        {
            llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), "anon_aggregate");
            // llvm_type = LLVMStructType(nullptr, 0, false);
        }

        LLVM_Registered_Type rt = { llvm_type, ag_type };
        BUF_PUSH(builder->registered_aggregates, rt);
    }

    LLVMTypeRef llvm_finalize_aggregate_types(LLVM_IR_Builder* builder, AST_Type* ag_type)
    {
        assert(ag_type->kind == AST_TYPE_STRUCT ||
               ag_type->kind == AST_TYPE_UNION);

        auto member_decls = ag_type->aggregate_type.member_declarations;

        BUF(LLVMTypeRef) llvm_member_types = nullptr;
        uint64_t biggest_member_index = 0;
        uint64_t biggest_member_size = 0;

        for (uint64_t i = 0; i < BUF_LENGTH(member_decls); i++)
        {
            AST_Declaration* member_decl = member_decls[i];
            AST_Type* member_type = member_decl->mutable_decl.type;

            if (member_type->bit_size > biggest_member_size)
            {
                biggest_member_size = member_type->bit_size;
                biggest_member_index = i;
            }

            LLVMTypeRef llvm_mem_type = nullptr;

            if(member_type->kind == AST_TYPE_STRUCT || member_type->kind == AST_TYPE_UNION)
            {
                llvm_mem_type = llvm_finalize_aggregate_types(builder, member_type);
                // printf("\t\tfinalized member %lu: %s\n", i, LLVMPrintTypeToString(llvm_mem_type));
            }
            else
            {
                llvm_mem_type = llvm_type_from_ast(builder, member_type);
            }

            assert(llvm_mem_type);
            BUF_PUSH(llvm_member_types, llvm_mem_type);
        }

        LLVM_Registered_Type* reg_type = llvm_find_registered_type(builder, ag_type);
        LLVMTypeRef llvm_ag_type = reg_type->type;
        if (!reg_type->finalized)
        {
            auto llvm_member_count = BUF_LENGTH(llvm_member_types);

            if (ag_type->kind == AST_TYPE_STRUCT)
            {
                LLVMStructSetBody(llvm_ag_type, llvm_member_types, (unsigned)llvm_member_count, false);
                // printf("Finalized struct type: %s\n", ag_type->name);
            }
            else if (ag_type->kind == AST_TYPE_UNION)
            {
                // auto union_ty = llvm_member_types[biggest_member_index];
                // unsigned mem_count = LLVMCountStructElementTypes(union_ty);
                // LLVMTypeRef* mem_types = (LLVMTypeRef*)mem_alloc(sizeof(LLVMTypeRef) * mem_count);

                // LLVMGetStructElementTypes(union_ty, mem_types);

                // LLVMStructSetBody(llvm_ag_type, mem_types, mem_count, false);

                // mem_free(mem_types);
                LLVMStructSetBody(llvm_ag_type, &llvm_member_types[biggest_member_index], 1,
                                  false);
                // printf("Finalized union type: %s\n", ag_type->name);
            }
            else assert(false);

            reg_type->finalized = true;
        }

        BUF_FREE(llvm_member_types);
        // printf("\t%s\n\n", LLVMPrintTypeToString(llvm_ag_type));
        return llvm_ag_type;
    }

    LLVM_Registered_Type* llvm_find_registered_type(LLVM_IR_Builder* builder, AST_Type* zir_type)
    {
        for (uint64_t i = 0; i < BUF_LENGTH(builder->registered_aggregates); i++)
        {
            if (builder->registered_aggregates[i].zir_type == zir_type)
            {
                return &builder->registered_aggregates[i];
            }
        }

        return nullptr;
    }

    uint64_t llvm_int_value_byte_size(LLVMValueRef llvm_value)
    {
        LLVMTypeRef llvm_type = LLVMTypeOf(llvm_value);
        return LLVMGetIntTypeWidth(llvm_type);
    }
}
