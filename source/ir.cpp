#include "ir.h"

#include "builtin.h"

#include <inttypes.h>
#include <stdarg.h>

namespace Zodiac
{
    void ir_builder_init(IR_Builder* ir_builder, Context* context)
    {
        assert(ir_builder);

        ir_builder->context = context;
        ir_builder->arena = arena_create(MB(1));
        ir_builder->value_to_decl_map = nullptr;
        ir_builder->result = {};
        ir_builder->result.string_literal_arena = arena_create(MB(1));
        ir_builder->current_function = nullptr;
        ir_builder->insert_block = nullptr;

        if (!ir_builder->context->global_init_block)
        {
            ir_builder->context->global_init_block = ir_builder_create_block(ir_builder,
                                                                             "global_init_block");
        }
    }

    IR_Module ir_builder_emit_module(IR_Builder* ir_builder, AST_Module* module)
    {
        assert(ir_builder);
        assert(module);

        assert(ir_builder->ast_module == nullptr);
        ir_builder->ast_module = module;

		// Emit builtin declarations
		auto builtin_decls = ir_builder->context->builtin_decls;
		for (uint64_t i = 0; i < BUF_LENGTH(builtin_decls); i++)
		{
			ir_builder_emit_global_declaration(ir_builder, builtin_decls[i]);
            if (ir_builder->result.error_count)
            {
                return ir_builder->result;
            }
		}

        // Emit global declarations
        for (uint64_t i = 0; i < BUF_LENGTH(module->global_declarations); i++)
        {
            ir_builder_emit_global_declaration(ir_builder, module->global_declarations[i]);
            if (ir_builder->result.error_count)
            {
                return ir_builder->result;
            }
        }

        // Emit function bodies
        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->value_to_decl_map); i++)
        {
            AST_Declaration* decl = ir_builder->value_to_decl_map[i].declaration;
            IR_Value* ir_value = ir_builder->value_to_decl_map[i].ir_value;
            if (ir_value->kind == IRV_FUNCTION)
            {
                IR_Function* func = ir_value->function;
                IR_Block* entry_block = func->first_block;

                if (decl->function.body_block)
                {
                    ir_builder->current_function = func;
                    ir_builder_set_insert_block(ir_builder, entry_block);

                    ir_builder_emit_statement(ir_builder, decl->function.body_block, nullptr);

                    auto insert_block = ir_builder->insert_block;
                    auto first_instruction = insert_block->first_instruction;
                    auto last_instruction = insert_block->last_instruction;
                    //if (!first_instruction && insert_block->previous)
                    //{
                    //    //@TODO: Free list
                    //    insert_block->previous->next = nullptr;
                    //    insert_block->previous = nullptr;
                    //}
                    //else if (!last_instruction ||
                    //        !ir_instruction_is_terminator(last_instruction->op))
                    if (!last_instruction ||
                        !ir_instruction_is_terminator(last_instruction->op))
                    {
                        ir_builder_emit_return(ir_builder, nullptr);
                    }

                    ir_builder->current_function = nullptr;

					ir_builder_patch_empty_block_jumps(ir_builder, func);
                }
                else
                {
                    assert(decl->directive);
                    assert(decl->directive->kind == AST_DIREC_FOREIGN);
                }
            }
        }

        assert(module->gen_data == nullptr);
        module->gen_data = ir_builder;
        return ir_builder->result;
    }

    void ir_builder_emit_global_declaration(IR_Builder* ir_builder, AST_Declaration* global_decl)
    {
        assert(ir_builder);
        assert(global_decl);

        assert(global_decl->location == AST_DECL_LOC_GLOBAL);

        switch (global_decl->kind)
        {
            case AST_DECL_FUNC:
            {
                AST_Identifier* ident = global_decl->identifier;
                AST_Type* return_type = global_decl->function.return_type;
                IR_Value* func_value = ir_builder_begin_function(ir_builder,
                                                                    ident->atom.data,
                                                                    global_decl->function.type);
                if (global_decl->function.body_block)
                {
                    IR_Value* entry_block = ir_builder_create_block(ir_builder, "entry",
                                                                    func_value);
                    ir_builder_set_insert_block(ir_builder, entry_block);

                    for (uint64_t i = 0; i < BUF_LENGTH(global_decl->function.args); i++)
                    {
                        AST_Declaration* arg_decl = global_decl->function.args[i];
                        assert(arg_decl->kind == AST_DECL_MUTABLE);
                        assert(arg_decl->location == AST_DECL_LOC_ARGUMENT);

                        AST_Identifier* arg_ident = arg_decl->identifier;
                        AST_Type* arg_type = arg_decl->mutable_decl.type;
                        IR_Value* arg_value = ir_builder_emit_function_arg(ir_builder,
                                                                        arg_ident->atom.data,
                                                                        arg_type);
                        ir_builder_push_value_and_decl(ir_builder, arg_value, arg_decl);
                    }
                }
                else
                {
                    func_value->function->flags |= IR_FUNC_FLAG_FOREIGN;
                    if (global_decl->function.is_vararg)
                    {
                        func_value->function->flags |= IR_FUNC_FLAG_VARARG;
                    }
                    func_value->function->foreign_index =
                        ir_builder_emit_foreign(ir_builder, global_decl->identifier->atom);
                }

                ir_builder_end_function(ir_builder, func_value);

                ir_builder_push_value_and_decl(ir_builder, func_value, global_decl);
                break;
            }

            case AST_DECL_MUTABLE:
            {
                IR_Value* value = ir_builder_emit_global(ir_builder, global_decl);
                ir_builder_push_value_and_decl(ir_builder, value, global_decl);
                break;
            }

            case AST_DECL_CONSTANT_VAR:
            {
                AST_Expression* init_expr = global_decl->constant_var.init_expression;
                IR_Value* value = ir_builder_emit_expression(ir_builder, init_expr);
                BUF_PUSH(ir_builder->result.global_constants, value);
                ir_builder_push_value_and_decl(ir_builder, value, global_decl);
                break;
            }

            case AST_DECL_TYPE:
            {
                break;
            }

            case AST_DECL_DYN_LINK:
            {
                bool found = false;
                for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->result.dynamic_lib_names); i++)
                {
                    Atom ex_lib = ir_builder->result.dynamic_lib_names[i];
                    if (ex_lib == global_decl->dyn_link_name)
                    {
                        found = true;
                        break;
                    }
                }

                if (!found)
                {
                    BUF_PUSH(ir_builder->result.dynamic_lib_names, global_decl->dyn_link_name);
                }
                break;
            }

            case AST_DECL_STATIC_IF:
            {
                AST_Expression* cond_expr = global_decl->static_if.cond_expr;
                IR_Value* cond_value = ir_builder_emit_expression(ir_builder, cond_expr);
                assert(cond_value->type == Builtin::type_bool);
                if (cond_value->value.boolean)
                {
                    ir_builder_emit_global_declaration(ir_builder,
                                                       global_decl->static_if.then_declaration);
                }
                else if (global_decl->static_if.else_declaration)
                {
                    ir_builder_emit_global_declaration(ir_builder,
                                                       global_decl->static_if.else_declaration);
                }
                break;
            }

            case AST_DECL_BLOCK:
            {
                for (uint64_t i = 0; i < BUF_LENGTH(global_decl->block.decls); i++)
                {
                    ir_builder_emit_global_declaration(ir_builder, global_decl->block.decls[i]);
                }
                break;
            }

            case AST_DECL_STATIC_ASSERT:
            {
                AST_Expression* assert_expr = global_decl->static_assert_expression;
                IR_Value* cond_value = ir_builder_emit_expression(ir_builder, assert_expr);
                assert(cond_value->type == Builtin::type_bool);
                if (!cond_value->value.boolean)
                {
                    auto fp = global_decl->static_assert_expression->file_pos;
                    fprintf(stderr,
                            "Error:%s:%" PRIu64 ":%" PRIu64 ": Static assertion failed!\n\n",
                            fp.file_name, fp.line, fp.line_relative_char_pos);
                    ir_builder->result.error_count++;
                }
                break;
            }

            case AST_DECL_IMPORT:
            {
                // assert(false);
                // Do nothing for now?
                break;
            }

            case AST_DECL_AGGREGATE_TYPE:
            {
                // Do nothing for now?
                break;
            }

            case AST_DECL_ENUM_TYPE:
            {
                break;
            }

			case AST_DECL_TYPEDEF:
			{
				break;
			}

            default: assert(false);
        }
    }

    void ir_builder_emit_statement(IR_Builder* ir_builder, AST_Statement* statement,
                                   IR_Value* break_block)
    {
        assert(ir_builder);
        assert(statement);

        switch (statement->kind)
        {
            case AST_STMT_DECLARATION:
            {
                AST_Declaration* decl = statement->declaration;
                assert(decl->kind == AST_DECL_MUTABLE);
                assert(decl->location == AST_DECL_LOC_LOCAL);

                IR_Value* allocl = ir_builder_emit_allocl(ir_builder, decl->mutable_decl.type,
                                                          decl->identifier->atom.data);

                ir_builder_push_value_and_decl(ir_builder, allocl, decl);

                AST_Expression* init_expr = decl->mutable_decl.init_expression;
                if (init_expr)
                {
                    IR_Value* init_value = ir_builder_emit_expression(ir_builder, init_expr);
                    ir_builder_emit_storel(ir_builder, allocl, init_value);
                }
                break;
            }

            case AST_STMT_RETURN:
            {
                assert(statement->return_expression);
                IR_Value* return_value = ir_builder_emit_expression(ir_builder,
                                                                    statement->return_expression);
                ir_builder_emit_return(ir_builder, return_value);
                break;
            }

            case AST_STMT_BLOCK:
            {
                for (uint64_t i = 0; i < BUF_LENGTH(statement->block.statements); i++)
                {
                    AST_Statement* block_member_stmt = statement->block.statements[i];
                    ir_builder_emit_statement(ir_builder, block_member_stmt, break_block);
                }
                break;
            }

            case AST_STMT_IF:
            {
                IR_Value* cond_value = ir_builder_emit_expression(ir_builder,
                                                                  statement->if_stmt.if_expression);

                IR_Function* cur_func = ir_builder->current_function;
                IR_Value* then_block_val = ir_builder_create_block(ir_builder, "then", cur_func);
                IR_Value* else_block_val = nullptr;
                IR_Value* post_if_block_val = ir_builder_create_block(ir_builder, "post_if");
                if (statement->if_stmt.else_statement)
                {
                    else_block_val = ir_builder_create_block(ir_builder, "else");
                }
                else
                {
                    else_block_val = post_if_block_val;
                }

                ir_builder_emit_jmp_if(ir_builder, cond_value, then_block_val);
                ir_builder_emit_jmp(ir_builder, else_block_val);

                ir_builder_set_insert_block(ir_builder, then_block_val);
                assert(statement->if_stmt.then_statement);
                ir_builder_emit_statement(ir_builder, statement->if_stmt.then_statement, break_block);
                if (!ir_instruction_is_terminator(then_block_val->block->last_instruction->op))
                {
                    ir_builder_emit_jmp(ir_builder, post_if_block_val);
                }

                if (statement->if_stmt.else_statement)
                {
                    ir_builder_append_block(ir_builder, cur_func, else_block_val->block);
                    ir_builder_set_insert_block(ir_builder, else_block_val);
                    ir_builder_emit_statement(ir_builder, statement->if_stmt.else_statement, break_block);
                    if (!ir_instruction_is_terminator(else_block_val->block->last_instruction->op))
                    {
                        ir_builder_emit_jmp(ir_builder, post_if_block_val);
                    }
                }

                // if (ir_builder->insert_block == else_block_val->block)
                // {
                //     ir_builder_append_block(ir_builder, cur_func, post_if_block_val->block);
                //     ir_builder_set_insert_block(ir_builder, post_if_block_val);
                // }
                ir_builder_append_block(ir_builder, cur_func, post_if_block_val->block);
                ir_builder_set_insert_block(ir_builder, post_if_block_val);

                break;
            }

            case AST_STMT_ASSIGN:
            {
                ir_builder_emit_assign_statement(ir_builder, statement);
                break;
            }

            case AST_STMT_CALL:
            {
                ir_builder_emit_expression(ir_builder, statement->call_expression);
                break;
            }

            case AST_STMT_WHILE:
            {
                IR_Function* cur_func = ir_builder->current_function;
                IR_Value* while_cond_block_value = ir_builder_create_block(ir_builder,
                                                                           "while_cond", cur_func);
                IR_Value* while_body_block_value = ir_builder_create_block(ir_builder,
                                                                           "while_body", cur_func);
                IR_Value* post_while_block_value = ir_builder_create_block(ir_builder,
                                                                           "post_while");

                ir_builder_emit_jmp(ir_builder, while_cond_block_value);

                ir_builder_set_insert_block(ir_builder, while_cond_block_value);
                IR_Value* cond_value = ir_builder_emit_expression(ir_builder,
                                                                  statement->while_stmt.cond_expr);
                ir_builder_emit_jmp_if(ir_builder, cond_value, while_body_block_value);
                ir_builder_emit_jmp(ir_builder, post_while_block_value);

                ir_builder_set_insert_block(ir_builder, while_body_block_value);
                ir_builder_emit_statement(ir_builder, statement->while_stmt.body_stmt,
                                          post_while_block_value);
                ir_builder_emit_jmp(ir_builder, while_cond_block_value);

                ir_builder_append_block(ir_builder, cur_func, post_while_block_value->block);
                ir_builder_set_insert_block(ir_builder, post_while_block_value);
                break;
            }

            case AST_STMT_FOR:
            {
                IR_Function* cur_func = ir_builder->current_function;
                IR_Value* for_cond_block_value = ir_builder_create_block(ir_builder, "for_cond",
                                                                         cur_func);
                IR_Value* for_body_block_value = ir_builder_create_block(ir_builder, "for_body",
                                                                         cur_func);
                IR_Value* post_for_block_value = ir_builder_create_block(ir_builder, "post_for");


                ir_builder_emit_statement(ir_builder, statement->for_stmt.init_stmt, break_block);
                ir_builder_emit_jmp(ir_builder, for_cond_block_value);

                ir_builder_set_insert_block(ir_builder, for_cond_block_value);
                IR_Value* cond_value = ir_builder_emit_expression(ir_builder, statement->for_stmt.cond_expr);
                ir_builder_emit_jmp_if(ir_builder, cond_value, for_body_block_value);
                ir_builder_emit_jmp(ir_builder, post_for_block_value);

                ir_builder_set_insert_block(ir_builder, for_body_block_value);
                ir_builder_emit_statement(ir_builder, statement->for_stmt.body_stmt,
                                          post_for_block_value);
                ir_builder_emit_statement(ir_builder, statement->for_stmt.step_stmt, break_block);
                ir_builder_emit_jmp(ir_builder, for_cond_block_value);

                ir_builder_append_block(ir_builder, cur_func, post_for_block_value->block);
                ir_builder_set_insert_block(ir_builder, post_for_block_value);
                break;
            }

			case AST_STMT_SWITCH:
			{
				ir_builder_emit_switch_statement(ir_builder, statement, break_block);
				break;
			}

            case AST_STMT_BREAK:
            {
                assert(break_block);
                ir_builder_emit_jmp(ir_builder, break_block);
                break;
            }

            default: assert(false);
        }
    }

    void ir_builder_emit_assign_statement(IR_Builder* ir_builder, AST_Statement* statement)
    {
        assert(ir_builder);
        assert(statement);
        assert(statement->kind == AST_STMT_ASSIGN);

        AST_Expression* lvalue_expr = statement->assign.lvalue_expression;

        if (lvalue_expr->kind == AST_EXPR_IDENTIFIER)
        {
            AST_Declaration* lvalue_decl = lvalue_expr->identifier->declaration;
            IR_Value* target_alloc = ir_builder_value_for_declaration(ir_builder, lvalue_decl);
            assert(target_alloc);
            IR_Value* new_value = ir_builder_emit_expression(ir_builder, statement->assign.expression);

            switch (target_alloc->kind)
            {
                case IRV_ALLOCL:
                {
                    ir_builder_emit_storel(ir_builder, target_alloc, new_value);
                    break;
                }

                case IRV_ARGUMENT:
                {
                    ir_builder_emit_storea(ir_builder, target_alloc, new_value);
                    break;
                }

                case IRV_GLOBAL:
                {
                    ir_builder_emit_storeg(ir_builder, target_alloc, new_value);
                    break;
                }

                default: assert(false);
            }
        }
        else if (lvalue_expr->kind == AST_EXPR_UNARY)
        {
            assert(lvalue_expr->unary.op == AST_UNOP_DEREF);
            AST_Expression* operand_expr = lvalue_expr->unary.operand;
            assert(operand_expr->type->kind == AST_TYPE_POINTER);
            assert(operand_expr->kind == AST_EXPR_IDENTIFIER);
            IR_Value* pointer_alloc = ir_builder_value_for_declaration(ir_builder,
                                                                       operand_expr->identifier->declaration);
            IR_Value* new_value = ir_builder_emit_expression(ir_builder, statement->assign.expression);
            ir_builder_emit_storep(ir_builder, pointer_alloc, new_value);
        }
        else if (lvalue_expr->kind == AST_EXPR_SUBSCRIPT)
        {
            AST_Expression* base_expression = lvalue_expr->subscript.base_expression;
            AST_Expression* index_expression = lvalue_expr->subscript.index_expression;
            assert(base_expression->kind == AST_EXPR_IDENTIFIER);
            IR_Value* index_value = ir_builder_emit_expression(ir_builder, index_expression);
            IR_Value* new_value = ir_builder_emit_expression(ir_builder, statement->assign.expression);

            if (base_expression->type->kind == AST_TYPE_STATIC_ARRAY)
            {
                AST_Type* element_type = base_expression->type->static_array.base;
                IR_Value* array_alloc = ir_builder_value_for_declaration(ir_builder,
                                                                         base_expression->identifier->declaration);
                assert(array_alloc->kind == IRV_ALLOCL);
                IR_Value* dest_pointer_value = ir_builder_emit_array_offset_pointer(ir_builder, array_alloc,
                                                                                    index_value);
                ir_builder_emit_storep(ir_builder, dest_pointer_value, new_value);
            }
            else if (base_expression->type->kind == AST_TYPE_POINTER)
            {
                AST_Type* element_type = base_expression->type->pointer.base;

                IR_Value* pointer_alloc = ir_builder_value_for_declaration(ir_builder,
                    base_expression->identifier->declaration);
                assert(pointer_alloc->kind == IRV_ALLOCL);
                IR_Value* base_pointer_value = ir_builder_emit_loadl(ir_builder, pointer_alloc);
                IR_Value* target_addr = ir_builder_emit_array_offset_pointer(ir_builder,
                                                                             base_pointer_value,
                                                                             index_value);

                ir_builder_emit_storep(ir_builder, target_addr, new_value);
            }
            else assert(false);
        }
        else if (lvalue_expr->kind == AST_EXPR_DOT)
        {
            AST_Expression* base_expression = lvalue_expr->dot.base_expression;
            AST_Expression* member_expression = lvalue_expr->dot.member_expression;

            auto base_decl = base_expression->identifier->declaration;
            IR_Value* struct_value = ir_builder_value_for_declaration(ir_builder,
                                                                      base_decl);
            AST_Type* struct_type = nullptr;
            bool is_pointer = false;
            if (base_expression->type->kind == AST_TYPE_STRUCT)
            {
                struct_type = base_expression->type;
            }
            else if (base_expression->type->kind == AST_TYPE_POINTER)
            {
                struct_type = base_expression->type->pointer.base;
                assert(struct_type->kind == AST_TYPE_STRUCT);
                is_pointer = true;
            }
            else assert(false);

            uint64_t member_index = 0;
            bool found = false;

            for (uint64_t i = 0; i < BUF_LENGTH(struct_type->aggregate_type.member_declarations); i++)
            {
                AST_Declaration* member_decl = struct_type->aggregate_type.member_declarations[i];
                if (member_expression->identifier->atom == member_decl->identifier->atom)
                {
                    member_index = i;
                    found = true;
                    break;
                }
            }

            assert(found);

            if (is_pointer)
            {
                if (struct_value->kind == IRV_ALLOCL)
                {
                    struct_value = ir_builder_emit_loadl(ir_builder, struct_value);
                }
                struct_value = ir_builder_emit_loadp(ir_builder, struct_value, struct_type);
            }

            IR_Value* target_pointer = ir_builder_emit_aggregate_offset_pointer(ir_builder,
                                                                                struct_value,
                                                                                member_index);
            IR_Value* new_value = ir_builder_emit_expression(ir_builder,
                                                             statement->assign.expression);
            ir_builder_emit_storep(ir_builder, target_pointer, new_value);
        }
        else assert(false);
    }

	struct _IR_Case
	{
		IR_Value* case_block = nullptr;
	};

	void ir_builder_emit_switch_statement(IR_Builder* ir_builder, AST_Statement* statement,
                                          IR_Value* break_block)
	{
		assert(ir_builder);
		assert(statement);
		assert(statement->kind == AST_STMT_SWITCH);


		IR_Function* cur_func = ir_builder->current_function;

		IR_Value* cond_value = ir_builder_emit_expression(ir_builder,
			statement->switch_stmt.switch_expression);

		IR_Block* switch_block = ir_builder->insert_block;
		IR_Value* post_switch_block_val = ir_builder_create_block(ir_builder, "post_switch");

		BUF(_IR_Case) cases = nullptr;

		bool found_default = false;
		IR_Value* default_block_val = nullptr;

		for (uint64_t i = 0; i < BUF_LENGTH(statement->switch_stmt.cases); i++)
		{
			const AST_Switch_Case& switch_case = statement->switch_stmt.cases[i];
			_IR_Case ir_case = {};
			if (switch_case.is_default)
			{
				found_default = true;
				ir_case.case_block = ir_builder_create_block(ir_builder, "default_case");
				default_block_val = ir_case.case_block;
			}
			else
			{
				assert(switch_case.case_expressions || switch_case.range_expressions);

				ir_builder_set_insert_block(ir_builder, switch_block);

				ir_case.case_block = ir_builder_create_block(ir_builder, "case");

                for (uint64_t i = 0; i < BUF_LENGTH(switch_case.case_expressions); i++)
                {
                    auto case_expression = switch_case.case_expressions[i];
                    auto case_value = ir_builder_emit_expression(ir_builder, case_expression);
                    IR_Value* case_cond_val = ir_builder_emit_eq(ir_builder, cond_value, case_value);
                    ir_builder_emit_jmp_if(ir_builder, case_cond_val, ir_case.case_block);
                }

                for (uint64_t i = 0; i < BUF_LENGTH(switch_case.range_expressions); i += 2)
                {
                    auto min_expr = switch_case.range_expressions[i];
                    auto max_expr = switch_case.range_expressions[i + 1];

                    assert(min_expr->type->flags & AST_TYPE_FLAG_INT);
                    assert(max_expr->type->flags & AST_TYPE_FLAG_INT);

                    IR_Value* min_value = ir_builder_emit_expression(ir_builder, min_expr);
                    IR_Value* max_value = ir_builder_emit_expression(ir_builder, max_expr);

                    IR_Value* lhs_value = ir_builder_emit_gteq(ir_builder, cond_value, min_value);
                    IR_Value* rhs_value = ir_builder_emit_lteq(ir_builder, cond_value, max_value);

                    IR_Value* case_cond_val = ir_builder_emit_and_and(ir_builder, lhs_value, rhs_value);
                    ir_builder_emit_jmp_if(ir_builder, case_cond_val, ir_case.case_block);
                }
			}

			ir_builder_set_insert_block(ir_builder, ir_case.case_block);
			ir_builder_emit_statement(ir_builder, switch_case.stmt, break_block);
			ir_builder_emit_jmp(ir_builder, post_switch_block_val);

			BUF_PUSH(cases, ir_case);
		}

		ir_builder_set_insert_block(ir_builder, switch_block);

		if (found_default)
		{
			assert(default_block_val);
			ir_builder_emit_jmp(ir_builder, default_block_val);
		}
		else
		{
			ir_builder_emit_jmp(ir_builder, post_switch_block_val);
		}

		for (uint64_t i = 0; i < BUF_LENGTH(cases); i++)
		{
			const _IR_Case& ir_case = cases[i];

			ir_builder_append_block(ir_builder, cur_func, ir_case.case_block->block);
		}

		// TODO: temp memory
		BUF_FREE(cases);

		ir_builder_append_block(ir_builder, cur_func, post_switch_block_val->block);
		ir_builder_set_insert_block(ir_builder, post_switch_block_val);
	}

    IR_Value* ir_builder_emit_expression(IR_Builder* ir_builder, AST_Expression* expression)
    {
        assert(ir_builder);
        assert(expression);

        switch (expression->kind)
        {
            case AST_EXPR_BINARY:
            {
                IR_Value* lhs_value = ir_builder_emit_expression(ir_builder,
                                                                 expression->binary.lhs);
                IR_Value* rhs_value = ir_builder_emit_expression(ir_builder,
                                                                 expression->binary.rhs);

                switch (expression->binary.op)
                {
                    case AST_BINOP_ADD:
                        return ir_builder_emit_add(ir_builder, lhs_value, rhs_value);

                    case AST_BINOP_SUB:
                        return ir_builder_emit_sub(ir_builder, lhs_value, rhs_value);

                    case AST_BINOP_DIV:
                        return ir_builder_emit_div(ir_builder, lhs_value, rhs_value);

                    case AST_BINOP_MUL:
                        return ir_builder_emit_mul(ir_builder, lhs_value, rhs_value);

                    case AST_BINOP_LT:
                        return ir_builder_emit_lt(ir_builder, lhs_value, rhs_value);

                    case AST_BINOP_LTEQ:
                        return ir_builder_emit_lteq(ir_builder, lhs_value, rhs_value);

                    case AST_BINOP_GT:
                        return ir_builder_emit_gt(ir_builder, lhs_value, rhs_value);

                    case AST_BINOP_GTEQ:
                        return ir_builder_emit_gteq(ir_builder, lhs_value, rhs_value);

                    case AST_BINOP_EQ:
                        return ir_builder_emit_eq(ir_builder, lhs_value, rhs_value);

                    case AST_BINOP_NEQ:
                        return ir_builder_emit_neq(ir_builder, lhs_value, rhs_value);

                    case AST_BINOP_AND_AND:
                        return ir_builder_emit_and_and(ir_builder, lhs_value, rhs_value);

                    default: assert(false);
                }
            }

            case AST_EXPR_UNARY:
            {
                switch (expression->unary.op)
                {
                    case AST_UNOP_MINUS:
                    {
                        return ir_builder_emit_negate(ir_builder, expression->unary.operand);
                        break;
                    }

                    case AST_UNOP_ADDROF:
                    {
                        return ir_builder_emit_addrof(ir_builder, expression->unary.operand);
                        break;
                    }

                    case AST_UNOP_DEREF:
                    {
                        return ir_builder_emit_deref(ir_builder, expression->unary.operand);
                        break;
                    }

                    case AST_UNOP_NOT:
                    {
                        return ir_builder_emit_not(ir_builder, expression->unary.operand);
                        break;
                    }

                    default: assert(false);
                }
                break;
            }

            case AST_EXPR_BOOL_LITERAL:
            {
                return ir_boolean_literal(ir_builder, expression->type,
                                          expression->bool_literal.boolean);
                break;
            }

			case AST_EXPR_NULL_LITERAL:
			{
				return ir_null_literal(ir_builder, expression->type);
			}

            case AST_EXPR_STRING_LITERAL:
            {
                return ir_string_literal(ir_builder, expression->type,
                                         expression->string_literal.atom);
                break;
            }

            case AST_EXPR_INTEGER_LITERAL:
            {
                return ir_integer_literal(ir_builder, expression->type,
                                          (int64_t)expression->integer_literal.u64);
                break;
            }

            case AST_EXPR_FLOAT_LITERAL:
            {
                return ir_float_literal(ir_builder, expression->type,
                                        expression->float_literal.r64,
                                        expression->float_literal.r32);
                break;
            }

            case AST_EXPR_CHAR_LITERAL:
            {
                return ir_character_literal(ir_builder, expression->type,
                                            expression->character_literal.c);
                break;
            }

            case AST_EXPR_IDENTIFIER:
            {
                AST_Declaration* ident_decl = expression->identifier->declaration;
                IR_Value* value = ir_builder_value_for_declaration(ir_builder, ident_decl);
                assert(value);
                if (value->kind == IRV_TEMPORARY ||
                    value->kind == IRV_INT_LITERAL ||
                    value->kind == IRV_BOOL_LITERAL)
                {
                    // Do nothing else for now
                }
                else if (value->kind == IRV_ALLOCL)
                {
                    value = ir_builder_emit_loadl(ir_builder, value);
                }
                else if (value->kind == IRV_ARGUMENT)
                {
                    value = ir_builder_emit_loada(ir_builder, value);
                }
                else if (value->kind == IRV_GLOBAL)
                {
                    value = ir_builder_emit_loadg(ir_builder, value);
                }
                else if (value->kind == IRV_FUNCTION)
                {
                    value = ir_builder_emit_addrof_function(ir_builder, value, expression->type);
                }
                else assert(false);

                return  value;

                break;
            }

            case AST_EXPR_CALL:
            {
                AST_Declaration* callee_decl = expression->call.callee_declaration;
                assert(callee_decl);
				if (callee_decl->kind == AST_DECL_FUNC)
				{
					IR_Value* callee_value = ir_builder_value_for_declaration(ir_builder, callee_decl);
					assert(callee_value);
					assert(callee_value->kind == IRV_FUNCTION);

					bool is_foreign = callee_value->function->flags & IR_FUNC_FLAG_FOREIGN;

                    AST_Type* func_type = callee_decl->function.type;
                    assert(func_type);
                    assert(func_type->kind == AST_TYPE_FUNCTION);

					for (uint64_t i = 0; i < BUF_LENGTH(expression->call.arg_expressions); i++)
					{
                        bool is_vararg = false;
                        if (i >= BUF_LENGTH(func_type->function.arg_types))
                        {
                            assert(func_type->function.is_vararg);
                            is_vararg = true;
                        }
						AST_Expression* arg_expr = expression->call.arg_expressions[i];
						IR_Value* arg_value = ir_builder_emit_expression(ir_builder, arg_expr);
						ir_builder_emit_call_arg(ir_builder, arg_value, is_vararg, is_foreign);
					}

					uint64_t num_args = BUF_LENGTH(expression->call.arg_expressions);
					IR_Value* num_args_lit = ir_integer_literal(ir_builder, Builtin::type_int,
						num_args);
					return ir_builder_emit_call(ir_builder, callee_value, num_args_lit);
				}
				else if (callee_decl->kind == AST_DECL_MUTABLE)
				{
					assert(callee_decl->mutable_decl.type->kind == AST_TYPE_POINTER);
					assert(callee_decl->mutable_decl.type->pointer.base->kind == AST_TYPE_FUNCTION);

					IR_Value* callee_value = ir_builder_value_for_declaration(ir_builder, callee_decl);
					IR_Value* func_ptr_value = ir_builder_emit_load(ir_builder, callee_value);

                    AST_Type* func_type = callee_decl->mutable_decl.type->pointer.base;

					for (uint64_t i = 0; i < BUF_LENGTH(expression->call.arg_expressions); i++)
					{
                        bool is_vararg = false;
                        if (i >= BUF_LENGTH(func_type->function.arg_types))
                        {
                            assert(func_type->function.is_vararg);
                            is_vararg = true;
                        }
						AST_Expression* arg_expr = expression->call.arg_expressions[i];
						IR_Value* arg_value = ir_builder_emit_expression(ir_builder, arg_expr);
						ir_builder_emit_call_arg(ir_builder, arg_value, is_vararg, true);
					}
					uint64_t num_args = BUF_LENGTH(expression->call.arg_expressions);
					IR_Value* num_args_lit = ir_integer_literal(ir_builder, Builtin::type_int,
						num_args);
					return ir_builder_emit_call(ir_builder, func_ptr_value, num_args_lit);
				}
				else assert(false);
                break;
            }

            case AST_EXPR_SUBSCRIPT:
            {
                AST_Expression* base_expr = expression->subscript.base_expression;
                AST_Expression* index_expr = expression->subscript.index_expression;
                IR_Value* index_value = ir_builder_emit_expression(ir_builder,
                                                                   index_expr);
                IR_Value* base_value = ir_builder_emit_expression(ir_builder,
                                                                  base_expr);
                return ir_builder_emit_subscript(ir_builder, base_value, index_value);
                break;
            }

            case AST_EXPR_COMPOUND_LITERAL:
            {
                if (expression->type->kind == AST_TYPE_STATIC_ARRAY)
                {
                    AST_Type* array_type = expression->type;
                    IR_Value* result_value = ir_builder_emit_allocl(ir_builder, array_type,
                                                                    "array_compound_lit");

                    auto compound_exprs = expression->compound_literal.expressions;
                    for (uint64_t i = 0; i < BUF_LENGTH(compound_exprs); i++)
                    {
                        AST_Expression* element_expression = compound_exprs[i];
                        IR_Value* element_value = ir_builder_emit_expression(ir_builder,
                                                                             element_expression);

                        IR_Value* pointer_value =
                            ir_builder_emit_array_offset_pointer(ir_builder, result_value, i);

                        ir_builder_emit_storep(ir_builder, pointer_value, element_value);
                    }

                    return result_value;
                }
                else if (expression->type->kind == AST_TYPE_STRUCT)
                {
                    AST_Type* struct_type = expression->type;
                    IR_Value* result_value = ir_builder_emit_allocl(ir_builder, struct_type,
                                                                    "struct_compound_lit");

                    auto compound_exprs = expression->compound_literal.expressions;

                    for (uint64_t i = 0; i < BUF_LENGTH(compound_exprs); i++)
                    {
                        AST_Expression* member_expression = compound_exprs[i];
                        IR_Value* member_value = ir_builder_emit_expression(ir_builder,
                                                                            member_expression);

                        IR_Value* pointer_value =
                            ir_builder_emit_aggregate_offset_pointer(ir_builder, result_value, i);
                        ir_builder_emit_storep(ir_builder, pointer_value, member_value);
                    }

                    return result_value;
                }
                else assert(false);

                break;
            }

            case AST_EXPR_ARRAY_LENGTH:
            {
                AST_Type* array_type = expression->array_length.ident_expr->type;
                assert(array_type->kind == AST_TYPE_STATIC_ARRAY);
                IR_Value* count_literal = ir_integer_literal(ir_builder, Builtin::type_int,
                                                             array_type->static_array.count);
                IR_Value* count_value = ir_builder_emit_load_lit(ir_builder, count_literal);
                return count_value;
                break;
            }

            case AST_EXPR_DOT:
            {
                return ir_builder_emit_dot_expression(ir_builder, expression);
                break;
            }

			case AST_EXPR_CAST:
			{
				return ir_builder_emit_cast_expression(ir_builder, expression);
				break;
			}

            default: assert(false);
        }

        return nullptr;
    }

    IR_Value* ir_builder_emit_dot_expression(IR_Builder* ir_builder, AST_Expression* expression)
    {
        assert(ir_builder);
        assert(expression);
        assert(expression->kind == AST_EXPR_DOT);

        AST_Expression* base_expression = expression->dot.base_expression;
        assert(base_expression->kind == AST_EXPR_IDENTIFIER);
        AST_Expression* member_expression = expression->dot.member_expression;
        assert(member_expression->kind == AST_EXPR_IDENTIFIER);

        if (!base_expression->type)
        {
            assert(base_expression->identifier->declaration);
            auto base_decl = base_expression->identifier->declaration;
            assert(base_decl->kind == AST_DECL_IMPORT);

            return ir_builder_emit_expression(ir_builder, member_expression);
        }
        else if (base_expression->type->kind == AST_TYPE_ENUM)
        {
            AST_Type* enum_type = base_expression->type;
            AST_Enum_Member_Decl* member = nullptr;

            for (uint64_t i = 0; i < BUF_LENGTH(enum_type->enum_type.member_declarations); i++)
            {
                AST_Enum_Member_Decl* member_decl = enum_type->enum_type.member_declarations[i];
                if (member_decl->identifier->atom == member_expression->identifier->atom)
                {
                    member = member_decl;
                    break;
                }
            }

            assert(member);
            return ir_integer_literal(ir_builder, enum_type->enum_type.base_type,
                                      member->index_value);
        }
        else
        {
            AST_Type* struct_type = nullptr;
            bool is_pointer = false;
            if (base_expression->type->kind == AST_TYPE_STRUCT)
            {
                struct_type = base_expression->type;
            }
            else if (base_expression->type->kind == AST_TYPE_POINTER)
            {
                struct_type = base_expression->type->pointer.base;
                is_pointer = true;
            }
            else assert(false);

            assert(struct_type->kind == AST_TYPE_STRUCT);

            uint64_t member_index = 0;
            AST_Type* member_type = nullptr;
            auto member_decls = struct_type->aggregate_type.member_declarations;
            for (uint64_t i = 0; i < BUF_LENGTH(member_decls); i++)
            {
                AST_Declaration* member_decl = member_decls[i];
                if (member_decl->identifier->atom == member_expression->identifier->atom)
                {
                    member_index = i;
                    member_type = member_decl->mutable_decl.type;
                    break;
                }
            }

            assert(member_type);

            auto base_decl = base_expression->identifier->declaration;
            IR_Value* base_pointer = ir_builder_value_for_declaration(ir_builder,
                                                                        base_decl);
            if (is_pointer)
            {
                if (base_pointer->kind == IRV_ALLOCL)
                {
                    base_pointer = ir_builder_emit_loadl(ir_builder, base_pointer);
                }
                else if (base_pointer->kind == IRV_ARGUMENT)
                {
                    base_pointer = ir_builder_emit_loada(ir_builder, base_pointer);
                }
                base_pointer = ir_builder_emit_loadp(ir_builder, base_pointer, struct_type);
            }
            IR_Value* value_pointer = ir_builder_emit_aggregate_offset_pointer(ir_builder,
                                                                                base_pointer,
                                                                                member_index);
            return ir_builder_emit_loadp(ir_builder, value_pointer, member_type);
        }
    }

	IR_Value* ir_builder_emit_cast_expression(IR_Builder* ir_builder, AST_Expression* expression)
	{
		assert(ir_builder);
		assert(expression);
		assert(expression->kind == AST_EXPR_CAST);

		IR_Value* expr_value = ir_builder_emit_expression(ir_builder, expression->cast_expr.expr);
		IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, expression->type);
		IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_CAST, expr_value, nullptr,
			result);

		ir_builder_emit_instruction(ir_builder, iri);

		return result;
	}

    IR_Value* ir_builder_emit_load_lit(IR_Builder* ir_builder, IR_Value* literal)
    {
        assert(ir_builder);
        assert(literal);
        assert(literal->kind == IRV_INT_LITERAL);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, literal->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_LOAD_LIT, literal, nullptr,
                                                 result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_negate(IR_Builder* ir_builder, AST_Expression* expression)
    {
        assert(ir_builder);
        assert(expression);

        IR_Value* zero_val = ir_builder_emit_zero_literal(ir_builder, expression->type);
        IR_Value* expression_value = ir_builder_emit_expression(ir_builder, expression);
        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, expression->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_SUB, zero_val, expression_value,
                                                 result_value);

        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_addrof(IR_Builder* ir_builder, AST_Expression* expression)
    {
        assert(ir_builder);
        assert(expression);

        if (expression->kind == AST_EXPR_IDENTIFIER)
        {
            auto decl = expression->identifier->declaration;
            IR_Value* expression_value = ir_builder_value_for_declaration(ir_builder,
                                                                          decl);
            assert(expression_value);

            if (expression_value->kind == IRV_FUNCTION)
            {
                assert(expression_value->function->flags & IR_FUNC_FLAG_FOREIGN);
                return ir_builder_emit_addrof_foreign(ir_builder, expression_value,
                                                      expression->type);
            }
            else
            {
                AST_Type* result_type = ast_find_or_create_pointer_type(ir_builder->context,
                                                                        expression->type);
                IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, result_type);

                IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_ADDROF,
                                                        expression_value, nullptr,
                                                        result_value);
                ir_builder_emit_instruction(ir_builder, iri);

                return result_value;
            }
        }
        else if (expression->kind == AST_EXPR_SUBSCRIPT)
        {
            AST_Expression* base_expr = expression->subscript.base_expression;
            AST_Expression* index_expr = expression->subscript.index_expression;
            if (base_expr->type->kind == AST_TYPE_POINTER)
            {
                IR_Value* base_pointer = ir_builder_emit_expression(ir_builder,
                                                                    base_expr);
                IR_Value* index_value = ir_builder_emit_expression(ir_builder,
                                                                   index_expr);
                return ir_builder_emit_array_offset_pointer(ir_builder, base_pointer,
                                                            index_value);
            }
            else
            {
                assert(base_expr->kind == AST_EXPR_IDENTIFIER);
                AST_Declaration* ident_decl = base_expr->identifier->declaration;
                IR_Value* array_allocl = ir_builder_value_for_declaration(ir_builder,
                                                                          ident_decl);
                assert(base_expr->type->kind == AST_TYPE_STATIC_ARRAY);
                IR_Value* index_value = ir_builder_emit_expression(ir_builder,
                                                                   index_expr);
                return ir_builder_emit_array_offset_pointer(ir_builder, array_allocl,
                                                            index_value);
            }
        }
        else if (expression->kind == AST_EXPR_DOT)
        {
            AST_Expression* base_expression = expression->dot.base_expression;
            AST_Expression* member_expression = expression->dot.member_expression;

            assert(base_expression->kind == AST_EXPR_IDENTIFIER);
			if (base_expression->type)
			{
				assert(base_expression->type->kind == AST_TYPE_STRUCT);

				assert(member_expression->kind == AST_EXPR_IDENTIFIER);

				AST_Type * struct_type = base_expression->type;

				uint64_t member_index = 0;
				bool found = false;
				auto member_decls = struct_type->aggregate_type.member_declarations;
				for (uint64_t i = 0; i < BUF_LENGTH(member_decls); i++)
				{
					AST_Declaration* member_decl = member_decls[i];
					if (member_expression->identifier->atom == member_decl->identifier->atom)
					{
						member_index = i;
						found = true;
						break;
					}
				}

				assert(found);

				auto base_decl = base_expression->identifier->declaration;
				IR_Value* struct_allocl = ir_builder_value_for_declaration(ir_builder,
					base_decl);

				return ir_builder_emit_aggregate_offset_pointer(ir_builder, struct_allocl,
					member_index);
			}
			else if (expression->type->kind == AST_TYPE_FUNCTION)
			{
				IR_Value* func_value = ir_builder_value_for_declaration(ir_builder,
					member_expression->identifier->declaration);
				assert(func_value);
				assert(func_value->kind == IRV_FUNCTION);
				assert(func_value->function->flags & IR_FUNC_FLAG_FOREIGN);

				return ir_builder_emit_addrof_foreign(ir_builder, func_value, expression->type);

			}
			else assert(false);
        }
        else
        {
            assert(false);
        }

        assert(false);
        return nullptr;
    }

	IR_Value* ir_builder_emit_addrof_foreign(IR_Builder* ir_builder, IR_Value* foreign_func,
		AST_Type* foreign_type)
	{
		assert(ir_builder);
		assert(foreign_func);
		assert(foreign_func->kind == IRV_FUNCTION);
		assert(foreign_func->function->flags & IR_FUNC_FLAG_FOREIGN);
		assert(foreign_type);

		AST_Type* pointer_type = ast_find_or_create_pointer_type(ir_builder->context, foreign_type);

		IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, pointer_type);
		IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_ADDROF_FOREIGN, foreign_func,
			nullptr, result_value);

		ir_builder_emit_instruction(ir_builder, iri);

		return result_value;
	}

    IR_Value* ir_builder_emit_addrof_function(IR_Builder* ir_builder, IR_Value* func, AST_Type* func_type)
    {
        assert(ir_builder);
        assert(func);
        assert(func->kind == IRV_FUNCTION);
        assert(func_type);
        assert(func_type->kind == AST_TYPE_FUNCTION);

        AST_Type* pointer_type = ast_find_or_create_pointer_type(ir_builder->context, func_type);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, pointer_type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_ADDROF_FUNCTION, func, nullptr,
                                                 result_value);

        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_deref(IR_Builder* ir_builder, AST_Expression* expression)
    {
        assert(ir_builder);
        assert(expression);
        assert(expression->kind == AST_EXPR_IDENTIFIER);

        AST_Declaration* ident_decl = expression->identifier->declaration;
        IR_Value* expression_value = ir_builder_value_for_declaration(ir_builder,
                                                                      ident_decl);
        assert(expression_value);

        AST_Type* operand_type = expression->type;
        assert(operand_type->kind == AST_TYPE_POINTER);

        AST_Type* result_type = operand_type->pointer.base;

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, result_type);

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_DEREF, expression_value,
                                                 nullptr, result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_not(IR_Builder* ir_builder, AST_Expression* expression)
    {
        assert(ir_builder);
        assert(expression);
        assert(expression->type == Builtin::type_bool ||
               (expression->type->flags & AST_TYPE_FLAG_INT) ||
               expression->type->kind == AST_TYPE_POINTER);

        IR_Value* operand_val = ir_builder_emit_expression(ir_builder, expression);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_NOT, operand_val,
                                                 nullptr, result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_value_for_declaration(IR_Builder* ir_builder,
                                               AST_Declaration* declaration)
    {
        assert(ir_builder);
        assert(declaration);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->value_to_decl_map); i++)
        {
            auto entry = ir_builder->value_to_decl_map[i];
            if (entry.declaration == declaration)
            {
                return entry.ir_value;
            }
        }

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->ast_module->import_modules); i++)
        {
            AST_Module* import_module = ir_builder->ast_module->import_modules[i];
            assert(import_module->gen_data);

            IR_Builder* import_ir_builder = (IR_Builder*)import_module->gen_data;

            for (uint64_t j = 0; j < BUF_LENGTH(import_ir_builder->value_to_decl_map); j++)
            {
                auto entry = import_ir_builder->value_to_decl_map[j];
                if (entry.declaration == declaration)
                {
                    return entry.ir_value;
                }
            }
        }

        return nullptr;
    }

    IR_Value* ir_builder_begin_function(IR_Builder* ir_builder, const char* name,
                                        AST_Type* func_type)
    {
        assert(ir_builder);
        assert(name);
        assert(func_type);

        assert(ir_builder->current_function == nullptr);

        //TODO: Assert we don't have a function with the same name
        IR_Function* function = ir_function_new(ir_builder, name, func_type);
        ir_builder->current_function = function;
        BUF_PUSH(ir_builder->result.functions, function);

        if (strcmp("main", name) == 0)
        {
            function->is_entry = true;
            assert(!ir_builder->result.entry_function);
            ir_builder->result.entry_function = function;
        }

        return ir_value_function_new(ir_builder, function);
    }

    void ir_builder_end_function(IR_Builder* ir_builder, IR_Value* func_value)
    {
        assert(ir_builder);
        assert(func_value);
        assert(func_value->kind == IRV_FUNCTION);

        IR_Function* function = func_value->function;

        assert(ir_builder->current_function == function);
        ir_builder->current_function = nullptr;

        ir_builder->insert_block = nullptr;
    }

    void ir_builder_patch_empty_block_jumps(IR_Builder* ir_builder, IR_Function* function)
    {
        assert(ir_builder);
        assert(function);

        auto block = function->first_block;
        IR_Block* previous_block = nullptr;
        while (block)
        {
            auto next_block = block->next;
            auto pb = block;

            if (!block->first_instruction)
            {
                assert(!block->last_instruction);
                assert(block != function->last_block);

                IR_Block* target_block = block->next;
                while (target_block && !target_block->first_instruction)
                {
                    target_block = target_block->next;
                }
                assert(target_block && target_block->first_instruction);

                ir_builder_patch_block_jumps(ir_builder, function, block, target_block);

                // TODO: Freelist the empty block
                if (previous_block)
                {
                    previous_block->next = block->next;
                    block->next->previous = previous_block;
                    pb = previous_block;
                }
                else
                {
                    function->first_block = next_block;
                    next_block->previous = nullptr;
                    pb = nullptr;
                }
            }

            previous_block = pb;
            block = next_block;
        }
    }

    void ir_builder_patch_block_jumps(IR_Builder* ir_builder, IR_Function* function,
                                     IR_Block* orig_block, IR_Block* target_block)
    {
        assert(ir_builder);
        assert(function);
        assert(orig_block);
        assert(target_block);

        auto block = function->first_block;
        while (block)
        {
            if (block != orig_block)
            {
                auto iri = block->first_instruction;
                while (iri)
                {
                    IR_Value* dest_block_val = nullptr;
                    if (iri->op == IR_OP_JMP)
                    {
                        dest_block_val = iri->arg1;
                    }
                    else if (iri->op == IR_OP_JMP_IF)
                    {
                        dest_block_val = iri->arg2;
                    }

                    if (dest_block_val && dest_block_val->block == orig_block)
                    {
                        dest_block_val->block = target_block;
                    }

                    iri = iri->next;
                }
            }
            else
            {
                assert(!block->first_instruction);
            }
            block = block->next;
        }
    }

    IR_Value* ir_builder_create_block(IR_Builder* ir_builder, const char* name, IR_Function* function/*= nullptr*/)
    {
        assert(ir_builder);
        assert(name);

        IR_Block* block = arena_alloc(&ir_builder->arena, IR_Block);
        block->name = atom_get(ir_builder->context->atom_table, name);
        block->first_instruction = nullptr;
        block->last_instruction = nullptr;

        if (function)
        {
            ir_builder_append_block(ir_builder, function, block);
        }

        return ir_value_block_new(ir_builder, block);
    }

    IR_Value* ir_builder_create_block(IR_Builder* ir_builder, const char* name, IR_Value* function_value)
    {
        assert(ir_builder);
        assert(name);

        if (function_value)
        {
            assert(function_value->kind == IRV_FUNCTION);
        }

        return ir_builder_create_block(ir_builder, name, function_value->function);
    }

    void ir_builder_append_block(IR_Builder* ir_builder, IR_Function* function, IR_Block* block)
    {
        assert(ir_builder);
        assert(function);
        assert(block);

        assert(block->next == nullptr);

        auto ib = function->first_block;
        while (ib)
        {
            if (ib == block)
            {
                assert(false);
            }

            if (ib->name == block->name)
            {
                Atom new_name = atom_append(ir_builder->context->atom_table, block->name,
                                            function->next_duplicate_name_index++);
                block->name = new_name;
                break;
            }

            ib = ib->next;
        }

        if (function->first_block)
        {
            assert(function->last_block);
            assert(function->last_block->next == nullptr);

            function->last_block->next = block;
            block->previous = function->last_block;
            function->last_block = block;
        }
        else
        {
            assert(function->last_block == nullptr);

            function->first_block = block;
            function->last_block = block;
            block->previous = nullptr;
        }
    }

    void ir_builder_set_insert_block(IR_Builder* ir_builder, IR_Block* block)
    {
        assert(ir_builder);
        assert(block);

        ir_builder->insert_block = block;
    }

    void ir_builder_set_insert_block(IR_Builder* ir_builder, IR_Value* block_value)
    {
        assert(ir_builder);
        assert(block_value);
        assert(block_value->kind == IRV_BLOCK);

        ir_builder_set_insert_block(ir_builder, block_value->block);
    }

    IR_Value* ir_builder_emit_array_offset_pointer(IR_Builder* ir_builder, IR_Value* array_allocl,
                                                   uint64_t offset)
    {
        assert(ir_builder);
        assert(array_allocl);
        assert(array_allocl->type->kind == AST_TYPE_STATIC_ARRAY);
        assert(offset < array_allocl->type->static_array.count);

        IR_Value* offset_value_literal = ir_integer_literal(ir_builder, Builtin::type_int, offset);
        AST_Type* result_type = ast_find_or_create_pointer_type(ir_builder->context,
                                                                array_allocl->type->static_array.base);
        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, result_type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_ARRAY_OFFSET_POINTER, array_allocl,
                                                         offset_value_literal, result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_array_offset_pointer(IR_Builder* ir_builder, IR_Value* array_allocl,
                                                   IR_Value* offset_value)
    {
        assert(ir_builder);
        assert(array_allocl);
        assert(array_allocl->type->kind == AST_TYPE_STATIC_ARRAY ||
               array_allocl->type->kind == AST_TYPE_POINTER);
        assert(offset_value);
        assert(offset_value->kind == IRV_TEMPORARY || IRV_INT_LITERAL);
        assert(offset_value->type == Builtin::type_int);

        AST_Type* result_type = nullptr;
        if (array_allocl->type->kind == AST_TYPE_STATIC_ARRAY)
        {
            result_type = ast_find_or_create_pointer_type(ir_builder->context,
                                                          array_allocl->type->static_array.base);
        }
        else
        {
            result_type = array_allocl->type;
        }

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, result_type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_ARRAY_OFFSET_POINTER, array_allocl,
                                                 offset_value, result_value);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_value;
    }

    IR_Value* ir_builder_emit_aggregate_offset_pointer(IR_Builder* ir_builder, IR_Value* struct_value,
                                                       uint64_t offset)
    {
        assert(ir_builder);
        assert(struct_value);
        assert(struct_value->kind == IRV_ALLOCL ||
               struct_value->kind == IRV_ARGUMENT ||
               struct_value->kind == IRV_TEMPORARY);
        assert(struct_value->type->kind == AST_TYPE_STRUCT);
        AST_Type* struct_type = struct_value->type;
        assert(struct_type->kind == AST_TYPE_STRUCT);

        assert(BUF_LENGTH(struct_type->aggregate_type.member_declarations) > offset);
        AST_Declaration* member_decl = struct_type->aggregate_type.member_declarations[offset];
        assert(member_decl->kind == AST_DECL_MUTABLE);
        assert(member_decl->location == AST_DECL_LOC_AGGREGATE_MEMBER);

        IR_Value* offset_value_literal = ir_integer_literal(ir_builder, Builtin::type_int, offset);
        AST_Type* result_type = ast_find_or_create_pointer_type(ir_builder->context,
                                                                member_decl->mutable_decl.type);
        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, result_type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_AGGREGATE_OFFSET_POINTER,
                                                 struct_value,
                                                 offset_value_literal, result_value);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_value;
    }

    void ir_builder_emit_instruction(IR_Builder* ir_builder, IR_Instruction* iri)
    {
        assert(ir_builder);
        assert(iri);

        // assert(ir_builder->current_function);
        assert(ir_builder->insert_block);

        assert(iri->next == nullptr);

        auto block = ir_builder->insert_block;
        if (block->first_instruction)
        {
            assert(block->last_instruction);
            assert(block->last_instruction->next == nullptr);

            block->last_instruction->next = iri;
            block->last_instruction = iri;
        }
        else
        {
            block->first_instruction = iri;
            block->last_instruction = iri;
        }
    }

    IR_Value* ir_builder_emit_function_arg(IR_Builder* ir_builder, const char* name, AST_Type* type)
    {
        assert(ir_builder);
        assert(name);
        assert(type);

        assert(ir_builder->current_function);

        IR_Value* arg_value = ir_value_new(ir_builder, IRV_ARGUMENT, type);
        arg_value->argument.name = name;
        arg_value->argument.index = BUF_LENGTH(ir_builder->current_function->local_temps);
        arg_value->assigned = true;

        BUF_PUSH(ir_builder->current_function->local_temps, arg_value);
        BUF_PUSH(ir_builder->current_function->arguments, arg_value);
        return arg_value;
    }

    IR_Value* ir_builder_emit_add(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, lhs->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_ADD, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_sub(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, lhs->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_SUB, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_mul(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, lhs->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_MUL, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_div(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, lhs->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_DIV, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_lt(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_LT, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_lteq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_LTEQ, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_gt(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_GT, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_gteq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_GTEQ, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_eq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        if (lhs->type == rhs->type ||
            (lhs->type->kind == AST_TYPE_ENUM && (lhs->type->enum_type.base_type == rhs->type)))
        {
            IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
            IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_EQ, lhs, rhs, result);

            ir_builder_emit_instruction(ir_builder, iri);

            return result;
        }
        else assert(false);

		return nullptr;
    }

    IR_Value* ir_builder_emit_neq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        if (lhs->type == rhs->type ||
            (lhs->type->kind == AST_TYPE_ENUM && (lhs->type->enum_type.base_type == rhs->type)))
        {
            IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
            IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_NEQ, lhs, rhs, result);

            ir_builder_emit_instruction(ir_builder, iri);

            return result;
        }
        else assert(false);

		return nullptr;
    }

    IR_Value* ir_builder_emit_and_and(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_AND_AND, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    void ir_builder_emit_return(IR_Builder* ir_builder, IR_Value* ret_val)
    {
        assert(ir_builder);

        // TODO: Check return type agains current functions return type

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_RETURN, ret_val,
                                                 nullptr, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    void ir_builder_emit_call_arg(IR_Builder* ir_builder, IR_Value* arg_value,
                                  bool is_vararg, bool is_foreign/*=false*/)
    {
        assert(ir_builder);
        assert(arg_value);

        auto op = IR_OP_PUSH_CALL_ARG;
        if (is_foreign)
        {
            op = IR_OP_PUSH_EX_CALL_ARG;
        }
        IR_Value* is_vararg_value = nullptr;
        if (is_vararg)
        {
            is_vararg_value = ir_boolean_literal(ir_builder, Builtin::type_bool, true);
        }
        IR_Instruction* iri = ir_instruction_new(ir_builder, op, arg_value,
                                                 is_vararg_value, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_call(IR_Builder* ir_builder, IR_Value* func_value,
                                   IR_Value* num_args)
    {
        assert(ir_builder);
        assert(func_value);
        assert(num_args);
        assert(num_args->kind == IRV_INT_LITERAL);

		if (func_value->kind == IRV_FUNCTION)
		{
			IR_Function* function = func_value->function;
			assert(function->type);

			IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY,
                                                  function->type->function.return_type);
			auto op = IR_OP_CALL;
			IR_Value* arg_1 = func_value;
			if (func_value->function->flags & IR_FUNC_FLAG_FOREIGN)
			{
				// arg_1 = ir_integer_literal(ir_builder, Builtin::type_int, func_value->function->foreign_index);
				op = IR_OP_CALL_EX;
			}
			IR_Instruction* iri = ir_instruction_new(ir_builder, op, arg_1,
				num_args, result_value);
			ir_builder_emit_instruction(ir_builder, iri);
			return result_value;
		}
		else if (func_value->type->kind == AST_TYPE_POINTER)
		{
			assert(func_value->type->pointer.base->kind == AST_TYPE_FUNCTION);
			AST_Type* func_type = func_value->type->pointer.base;
			IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, func_type->function.return_type);
			IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_CALL_PTR, func_value,
				num_args, result_value);
			ir_builder_emit_instruction(ir_builder, iri);
			return result_value;
		}
		else assert(false);

		assert(false);
		return nullptr;
    }

    IR_Value* ir_builder_emit_subscript(IR_Builder* ir_builder, IR_Value* base_value, IR_Value* index_value)
    {
        assert(ir_builder);
        assert(base_value);
        assert(index_value);

        switch (base_value->type->kind)
        {
            case AST_TYPE_POINTER:
            {
                IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, base_value->type->pointer.base);

                IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_SUBSCRIPT, base_value, index_value,
                                                        result_value);
                ir_builder_emit_instruction(ir_builder, iri);

                return result_value;
                break;
            }

            case AST_TYPE_STATIC_ARRAY:
            {
                IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, base_value->type->static_array.base);
                IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_SUBSCRIPT, base_value, index_value,
                                                         result_value);
                ir_builder_emit_instruction(ir_builder, iri);
                return result_value;
                break;
            }

            default: assert(false);
        }

        assert(false);
        return nullptr;
    }

    void ir_builder_emit_jmp(IR_Builder* ir_builder, IR_Value* block_value)
    {
        assert(ir_builder);
        assert(block_value);
        assert(block_value->kind == IRV_BLOCK);

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_JMP, block_value,
                                                 nullptr, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    void ir_builder_emit_jmp_if(IR_Builder* ir_builder, IR_Value* cond_value, IR_Value* block_value)
    {
        assert(ir_builder);
        assert(cond_value);
        assert(block_value);
        assert(block_value->kind == IRV_BLOCK);

        assert(cond_value->type == Builtin::type_bool);

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_JMP_IF, cond_value,
                                                 block_value, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_allocl(IR_Builder* ir_builder, AST_Type* type, const char* name)
    {
        assert(ir_builder);
        assert(type);
        assert(name);

        // assert(type->kind == AST_TYPE_POINTER ||
        //        type->flags & AST_TYPE_FLAG_INT);

        IR_Value* allocl_value = ir_value_allocl_new(ir_builder, type, name);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_ALLOCL, nullptr, nullptr,
                                                 allocl_value);
        ir_builder_emit_instruction(ir_builder, iri);

        assert(ir_builder->current_function);
        BUF_PUSH(ir_builder->current_function->local_temps, allocl_value);

        return allocl_value;
    }

    void ir_builder_emit_storel(IR_Builder* ir_builder, IR_Value* allocl_value, IR_Value* new_value)
    {
        assert(ir_builder);
        assert(allocl_value);
        assert(new_value);

        assert(allocl_value->kind == IRV_ALLOCL);
        assert(new_value->kind == IRV_TEMPORARY ||
               new_value->kind == IRV_ARGUMENT ||
               new_value->kind == IRV_ALLOCL ||
               new_value->kind == IRV_INT_LITERAL ||
               new_value->kind == IRV_STRING_LITERAL ||
               new_value->kind == IRV_CHAR_LITERAL ||
               new_value->kind == IRV_FLOAT_LITERAL ||
               new_value->kind == IRV_NULL_LITERAL);

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_STOREL, allocl_value, new_value,
                                                 nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_loadl(IR_Builder* ir_builder, IR_Value* allocl_value)
    {
        assert(ir_builder);
        assert(allocl_value);
        assert(allocl_value->kind == IRV_ALLOCL);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, allocl_value->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_LOADL, allocl_value, nullptr,
                                                result_value);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_value;
    }

    void ir_builder_emit_storea(IR_Builder* ir_builder, IR_Value* arg_value, IR_Value* new_value)
    {
        assert(ir_builder);
        assert(arg_value);
        assert(new_value);

        assert(arg_value->kind == IRV_ARGUMENT);
        assert(new_value->kind == IRV_TEMPORARY ||
               new_value->kind == IRV_ARGUMENT ||
               new_value->kind == IRV_INT_LITERAL);

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_STOREA, arg_value, new_value,
                                                 nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_loada(IR_Builder* ir_builder, IR_Value* alloca_value)
    {
        assert(ir_builder);
        assert(alloca_value);
        assert(alloca_value->kind == IRV_ARGUMENT);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, alloca_value->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_LOADA, alloca_value, nullptr,
                                                 result_value);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_value;
    }

    void ir_builder_emit_storep(IR_Builder* ir_builder, IR_Value* pointer_allocl, IR_Value* new_value)
    {
        assert(ir_builder);
        assert(pointer_allocl);
        assert(new_value);

        assert(pointer_allocl->kind == IRV_ALLOCL ||
               (pointer_allocl->kind == IRV_TEMPORARY) && pointer_allocl->type->kind == AST_TYPE_POINTER);
        assert(pointer_allocl->type);
        assert(pointer_allocl->type->kind == AST_TYPE_POINTER);
        assert(new_value->kind == IRV_TEMPORARY ||
               new_value->kind == IRV_ARGUMENT ||
               new_value->kind == IRV_INT_LITERAL ||
               new_value->kind == IRV_ALLOCL ||
               new_value->kind == IRV_CHAR_LITERAL ||
               new_value->kind == IRV_FLOAT_LITERAL);

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_STOREP, pointer_allocl, new_value, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_loadp(IR_Builder* ir_builder, IR_Value* pointer, AST_Type* type)
    {
        assert(ir_builder);
        assert(pointer);
        assert(pointer->type);
        assert(pointer->type->kind == AST_TYPE_POINTER);
        assert(type);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, type);

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_LOADP, pointer, nullptr, result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_global(IR_Builder* ir_builder, AST_Declaration* global_decl)
    {
        assert(ir_builder);
        assert(global_decl);
        assert(global_decl->kind == AST_DECL_MUTABLE);

        IR_Value* global_value = ir_value_global_new(ir_builder, global_decl->mutable_decl.type,
                                                     global_decl->identifier->atom.data);
        global_value->assigned = true;

        if (global_decl->mutable_decl.init_expression)
        {
            assert(!ir_builder->insert_block);
            ir_builder->insert_block = ir_builder->context->global_init_block->block;
            IR_Value* init_value = ir_builder_emit_expression(ir_builder, global_decl->mutable_decl.init_expression);
            ir_builder_emit_storeg(ir_builder, global_value, init_value);
            ir_builder->insert_block = nullptr;
        }
        return global_value;
    }

    void ir_builder_emit_storeg(IR_Builder* ir_builder, IR_Value* global_value, IR_Value* new_value)
    {
        assert(ir_builder);
        assert(global_value);
        assert(global_value->kind == IRV_GLOBAL);
        assert(new_value);

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_STOREG, global_value, new_value,
                                                 nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_loadg(IR_Builder* ir_builder, IR_Value* global_value)
    {
        assert(ir_builder);
        assert(global_value);
        assert(global_value->kind == IRV_GLOBAL);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, global_value->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_LOADG, global_value, nullptr,
                                                 result_value);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_value;
    }

	IR_Value* ir_builder_emit_load(IR_Builder* ir_builder, IR_Value* store)
	{
		assert(ir_builder);
		assert(store);

		switch (store->kind)
		{
			case IRV_ARGUMENT:
			{
				return ir_builder_emit_loada(ir_builder, store);
				break;
			}

			case IRV_GLOBAL:
			{
				return ir_builder_emit_loadg(ir_builder, store);
				break;
			}

            case IRV_ALLOCL:
            {
                return ir_builder_emit_loadl(ir_builder, store);
                break;
            }

			default: assert(false);
		}

		assert(false);
		return nullptr;
	}

    IR_Value* ir_builder_emit_zero_literal(IR_Builder* ir_builder, AST_Type* type)
    {
        assert(ir_builder);
        assert(type);

        if (type->flags & AST_TYPE_FLAG_FLOAT)
        {
            return ir_float_literal(ir_builder, type, 0, 0);
        }
        else if (type->flags & AST_TYPE_FLAG_INT)
        {
            return ir_integer_literal(ir_builder, type, 0);
        }
        else assert(false);

        assert(false);
        return nullptr;
    }

    IR_Value* ir_boolean_literal(IR_Builder* ir_builder, AST_Type* type, bool value)
    {
        assert(ir_builder);
        assert(type);

        IR_Value* result = ir_value_new(ir_builder, IRV_BOOL_LITERAL, type);
        result->value.boolean = value;
        result->assigned = true;
        return result;
    }

	IR_Value* ir_null_literal(IR_Builder* ir_builder, AST_Type* type)
	{
		assert(ir_builder);
		assert(type);

		IR_Value* result = ir_value_new(ir_builder, IRV_NULL_LITERAL, type);
		result->value.string = nullptr;
		result->assigned = true;
		return result;
	}

    IR_Value* ir_string_literal(IR_Builder* ir_builder, AST_Type* type, Atom string)
    {
        assert(ir_builder);
        assert(type);

        IR_Value* result = ir_value_new(ir_builder, IRV_STRING_LITERAL, type);
        result->value.string = arena_alloc_array(&ir_builder->result.string_literal_arena, uint8_t, string.length + 1);
        memcpy(result->value.string, string.data, string.length + 1);
        result->assigned = true;

        return result;
    }

    IR_Value* ir_integer_literal(IR_Builder* ir_builder, AST_Type* type, uint64_t s64)
    {
        assert(ir_builder);
        assert(type);

        IR_Value* result = ir_value_new(ir_builder, IRV_INT_LITERAL, type);
        result->value.s64 = s64;
        result->assigned = true;

        return result;
    }

    IR_Value* ir_float_literal(IR_Builder* ir_builder, AST_Type* type, double r64, float r32)
    {
        assert(ir_builder);
        assert(type);

        IR_Value* result = ir_value_new(ir_builder, IRV_FLOAT_LITERAL, type);
        if (type == Builtin::type_double)
        {
            result->value.r64 = r64;
        }
        else if (type == Builtin::type_float)
        {
            result->value.r32 = r32;
        }
        else assert(false);
        result->assigned = true;

        return result;
    }

    IR_Value* ir_character_literal(IR_Builder* ir_builder, AST_Type* type, char c)
    {
        assert(ir_builder);
        assert(type);

        IR_Value* result = ir_value_new(ir_builder, IRV_CHAR_LITERAL, type);
        result->value.u8 = c;
        result->assigned = true;

        return result;
    }

    uint64_t ir_builder_emit_foreign(IR_Builder* ir_builder, Atom atom)
    {
        assert(ir_builder);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->context->foreign_table); i++)
        {
            if (ir_builder->context->foreign_table[i] == atom)
            {
                return i;
            }
        }

        BUF_PUSH(ir_builder->context->foreign_table, atom);
        return BUF_LENGTH(ir_builder->context->foreign_table) - 1;
    }

    IR_Function* ir_function_new(IR_Builder* ir_builder, const char* name, AST_Type* func_type)
    {
        assert(ir_builder);
        assert(name);
        assert(func_type);

        IR_Function* result = arena_alloc(&ir_builder->arena, IR_Function);
        result->flags = IR_FUNC_FLAG_NONE;
        result->name = name;
        result->type = func_type;
        result->first_block = nullptr;
        result->last_block = nullptr;
        result->arguments = nullptr;
        result->local_temps = nullptr;
        result->is_entry = false;
        result->dcb_data = {};

        return result;
    }

    IR_Value* ir_value_new(IR_Builder* ir_builder, IR_Value_Kind kind, AST_Type* type)
    {
        assert(ir_builder);

        IR_Value* result = arena_alloc(&ir_builder->arena, IR_Value);
        result->kind = kind;
        result->type = type;
        result->assigned = false;

        if (kind == IRV_TEMPORARY)
        {
            if (ir_builder->current_function)
            {
                result->temp.index = BUF_LENGTH(ir_builder->current_function->local_temps);
                BUF_PUSH(ir_builder->current_function->local_temps, result);
            }
        }

        return result;
    }

    IR_Value* ir_value_function_new(IR_Builder* ir_builder, IR_Function* function)
    {
        assert(ir_builder);
        assert(function);

        IR_Value* result = ir_value_new(ir_builder, IRV_FUNCTION, nullptr);
        result->function = function;
        result->assigned = true;

        return result;
    }

    IR_Value* ir_value_block_new(IR_Builder* ir_builder, IR_Block* block)
    {
        assert(ir_builder);
        assert(block);

        IR_Value* result = ir_value_new(ir_builder, IRV_BLOCK, nullptr);
        result->block = block;
        result->assigned = true;

        return result;
    }

    IR_Value* ir_value_allocl_new(IR_Builder* ir_builder, AST_Type* type, const char* name)
    {
        assert(ir_builder);
        assert(type);
        assert(name);

        IR_Value* result = ir_value_new(ir_builder, IRV_ALLOCL, type);
        result->allocl.name = name;

        assert(ir_builder->current_function);
        result->allocl.index = BUF_LENGTH(ir_builder->current_function->local_temps);
        BUF_PUSH(ir_builder->current_function->local_temps, result);
        return result;
    }

    IR_Value* ir_value_global_new(IR_Builder* ir_builder, AST_Type* type, const char* name)
    {
        assert(ir_builder);
        assert(type);
        assert(name);

        IR_Value* result = ir_value_new(ir_builder, IRV_GLOBAL, type);
        result->global.name = name;

        result->global.index = BUF_LENGTH(ir_builder->context->global_table);
        Global_Variable global_var = { &ir_builder->result, result };
        BUF_PUSH(ir_builder->context->global_table, global_var);
        return result;
    }

    IR_Instruction* ir_instruction_new(IR_Builder* ir_builder, IR_Operator op,
                                       IR_Value* arg1, IR_Value* arg2, IR_Value* result_value)
    {
        assert(ir_builder);

        if (arg1)
        {
            assert(arg1->assigned);
        }

        if (arg2)
        {
            assert(arg2->assigned);
        }

        if (result_value)
        {
            assert(!result_value->assigned);
            result_value->assigned = true;
        }

        IR_Instruction* result = arena_alloc(&ir_builder->arena, IR_Instruction);
        result->op = op;
        result->arg1 = arg1;
        result->arg2 = arg2;
        result->result = result_value;

        result->next = nullptr;

        return result;
    }

    bool ir_instruction_is_terminator(IR_Operator op)
    {
        return (op == IR_OP_JMP ||
                op == IR_OP_RETURN);
    }

    IR_Validation_Result ir_validate(IR_Builder* ir_builder)
    {
        assert(ir_builder);

        IR_Validation_Result valres = {};

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->result.functions); i++)
        {
            ir_validate_function(ir_builder->result.functions[i], &valres);
        }

        return valres;
    }

    bool ir_validate_function(IR_Function* ir_function, IR_Validation_Result* valres)
    {
        assert(ir_function);
        assert(valres);

        bool result = true;

        //@TODO: Validate arguments and return type

        IR_Block* block = ir_function->first_block;
        while (block)
        {
            result &= ir_validate_block(block, valres);
            block = block->next;
        }

        return result;
    }

    bool ir_validate_block(IR_Block* ir_block, IR_Validation_Result* valres)
    {
        assert(ir_block);
        assert(valres);

        bool result = true;

        if (!ir_block->first_instruction)
        {
            assert(!ir_block->last_instruction);
            result = false;
            ir_report_validation_error(valres, "Block is empty: %s", ir_block->name);
        }
        else
        {
            assert(ir_block->last_instruction);

            bool ends_with_term = ir_instruction_is_terminator(ir_block->last_instruction->op);
            result &= ends_with_term;

            if (!ends_with_term)
            {
                ir_report_validation_error(valres, "Block does not end with a terminator: %s",
                                           ir_block->name);
            }
        }

        return result;
    }

    void ir_report_validation_error(IR_Validation_Result* valres, const char* format, ...)
    {
        assert(valres);
        assert(format);

        va_list va_args;
        va_start(va_args, format);
        uint64_t length = (uint64_t)vsnprintf(nullptr, 0, format, va_args) + 1;
        va_end(va_args);

        char* msg_buf = (char*)mem_alloc(length + 1);
        va_start(va_args, format);
        int written = vsnprintf(msg_buf, length, format, va_args);
        va_end(va_args);
        assert(written == length - 1);

        BUF_PUSH(valres->messages, msg_buf);
    }

    void ir_builder_print_result(IR_Builder* ir_builder)
    {
        assert(ir_builder);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->result.dynamic_lib_names); i++)
        {
            auto lib_atom = ir_builder->result.dynamic_lib_names[i];
            printf("#dynamic_link %s\n", lib_atom.data);
        }

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->result.global_constants); i++)
        {
            IR_Value* constant = ir_builder->result.global_constants[i];
            ir_print_value(constant);
        }

        printf("\n");

        ir_builder_print_functions(ir_builder);
    }

    void ir_builder_print_functions(IR_Builder* ir_builder)
    {
        assert(ir_builder);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->result.functions); i++)
        {
            IR_Function* func = ir_builder->result.functions[i];
            ir_print_function(func);
        }
    }

    void ir_print_function(IR_Function* function)
    {
        assert(function);

        bool is_foreign = function->flags & IR_FUNC_FLAG_FOREIGN;

        if (is_foreign)
        {
            printf("#foreign ");
        }

        printf("%s(", function->name);
        for (uint64_t i = 0; i < BUF_LENGTH(function->arguments); i++)
        {
            if (i > 0)
            {
                printf(", ");
            }
            ir_print_value(function->arguments[i]);
            printf(":(");
            ir_print_type(function->arguments[i]->type);
            printf(")");
        }
        printf(")");

        printf(" -> ");
        ir_print_type(function->type->function.return_type);
        printf("\n");

        if (!is_foreign)
        {
            printf("{\n");

            IR_Block* block = function->first_block;

            while (block)
            {
                ir_print_block(block);
                block = block->next;
            }

            printf("}\n");
        }

        printf("\n");
    }

    void ir_print_block(IR_Block* block)
    {
        assert(block);

        printf("\t%s:\n", block->name.data);

        IR_Instruction* instruction = block->first_instruction;

        while (instruction)
        {
            ir_print_instruction(instruction);
            instruction = instruction->next;
        }
    }

    void ir_print_instruction(IR_Instruction* instruction)
    {
        assert(instruction);

        printf("\t\t");

        if (instruction->result)
        {
            ir_print_value(instruction->result);
            printf(":(");
            ir_print_type(instruction->result->type);
            printf(") = ");
        }

        switch (instruction->op)
        {
            case IR_OP_NOP:
            {
                assert(false);
                break;
            }

            case IR_OP_ADD:
            {
                ir_print_value(instruction->arg1);
                printf(" + ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_SUB:
            {
                ir_print_value(instruction->arg1);
                printf(" - ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_MUL:
            {
                ir_print_value(instruction->arg1);
                printf(" * ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_DIV:
            {
                ir_print_value(instruction->arg1);
                printf(" / ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_LT:
            {
                ir_print_value(instruction->arg1);
                printf(" < ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_LTEQ:
            {
                ir_print_value(instruction->arg1);
                printf(" <= ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_GT:
            {
                ir_print_value(instruction->arg1);
                printf(" > ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_GTEQ:
            {
                ir_print_value(instruction->arg1);
                printf(" >= ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_EQ:
            {
                ir_print_value(instruction->arg1);
                printf(" == ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_NEQ:
            {
                ir_print_value(instruction->arg1);
                printf(" != ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_AND_AND:
            {
                ir_print_value(instruction->arg1);
                printf(" && ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_NOT:
            {
                printf("!");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_RETURN:
            {
                printf("RETURN ");
                if (instruction->arg1)
                {
                    ir_print_value(instruction->arg1);
                }
                break;
            }

            case IR_OP_SUBSCRIPT:
            {
                ir_print_value(instruction->arg1);
                printf(" SUBSCRIPT ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_PUSH_CALL_ARG:
            {
                printf("PUSH_ARG ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_CALL:
            {
                printf("CALL ");
                ir_print_value(instruction->arg1);
                printf(", ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_PUSH_EX_CALL_ARG:
            {
                printf("PUSH_EX_ARG ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_CALL_EX:
            {
                printf("CALL_EX ");
                ir_print_value(instruction->arg1);
                printf(", ");
                ir_print_value(instruction->arg2);
                break;
            }

			case IR_OP_ADDROF_FOREIGN:
			{
				printf("ADDROF_FOREIGN ");
				ir_print_value(instruction->arg1);
				break;
			}

            case IR_OP_ADDROF_FUNCTION:
            {
                printf("ADDROF_FUNCTION ");
                ir_print_value(instruction->arg1);
                break;
            }

			case IR_OP_CALL_PTR:
			{
				printf("CALL_PTR ");
				ir_print_value(instruction->arg1);
				printf(", ");
				ir_print_value(instruction->arg2);
				break;
			}

            case IR_OP_JMP:
            {
                printf("JMP ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_JMP_IF:
            {
                printf("IF ");
                ir_print_value(instruction->arg1);
                printf(" JMP ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_ALLOCL:
            {
                printf("ALLOCL ");
                break;
            }

            case IR_OP_STOREL:
            {
                printf("STOREL ");
                ir_print_value(instruction->arg2);
                printf(" INTO ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_LOADL:
            {
                printf("LOADL ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_STOREA:
            {
                printf("STOREA ");
                ir_print_value(instruction->arg2);
                printf(" INTO ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_LOADA:
            {
                printf("LOADA ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_STOREP:
            {
                printf("STOREP ");
                ir_print_value(instruction->arg2);
                printf(" INTO ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_LOADP:
            {
                printf("LOADP ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_STOREG:
            {
                printf("STOREG ");
                ir_print_value(instruction->arg2);
                printf(" INTO ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_LOADG:
            {
                printf("LOADG ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_LOAD_LIT:
            {
                printf("LOAD_LIT ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_ADDROF:
            {
                printf("ADDROF ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_DEREF:
            {
                printf("DEREF ");
                ir_print_value(instruction->arg1);
                break;
            }

            case IR_OP_ARRAY_OFFSET_POINTER:
            {
                printf("ARRAY_OFFSET_POINTER ");
                ir_print_value(instruction->arg1);
                printf(" ");
                ir_print_value(instruction->arg2);
                break;
            }

            case IR_OP_AGGREGATE_OFFSET_POINTER:
            {
                printf("AGGREGATE_OFFSET_POINTER ");
                ir_print_value(instruction->arg1);
                printf(" ");
                ir_print_value(instruction->arg2);
                break;
            }

			case IR_OP_CAST:
			{
				printf("CAST ");
				ir_print_value(instruction->arg1);
				printf(" TO ");
				ir_print_type(instruction->result->type);
				break;
			}

            default: assert(false);

        }

        printf("\n");
    }

    void ir_print_value(IR_Value* value)
    {
        assert(value);

        switch (value->kind)
        {
            case IRV_TEMPORARY:
            {
                printf("t(%" PRIu64 ")", value->temp.index);
                break;
            }

            case IRV_BOOL_LITERAL:
            {
                printf("lit(%s)", value->value.boolean ? "true" : "false");
                break;
            }

			case IRV_NULL_LITERAL:
			{
				printf("lit(null)");
				break;
			}

            case IRV_STRING_LITERAL:
            {
                ir_print_string_literal((const char*)value->value.string);
                break;
            }

            case IRV_INT_LITERAL:
            {
                printf("lit(%" PRId64 ")", value->value.s64);
                break;
            }

            case IRV_FLOAT_LITERAL:
            {
                if (value->type == Builtin::type_double)
                {
                    printf("lit(%f)", value->value.r64);
                }
                else if (value->type == Builtin::type_float)
                {
                    printf("lit(%f)", value->value.r32);
                }
                else assert(false);
                break;
            }

            case IRV_CHAR_LITERAL:
            {
                printf("lit('");
                ir_print_character(value->value.u8);
                printf("')");
                break;
            }

            case IRV_ARGUMENT:
            {
                printf("arg(%s)", value->argument.name);
                break;
            }

            case IRV_FUNCTION:
            {
                printf("func(%s)", value->function->name);
                break;
            }

            case IRV_BLOCK:
            {
                printf("block(%s)", value->block->name.data);
                break;
            }

            case IRV_ALLOCL:
            {
                printf("allocl(%s)", value->allocl.name);
                break;
            }

            case IRV_GLOBAL:
            {
                printf("global(%s)", value->global.name);
                break;
            }

            default: assert(false);
        }
    }

    void ir_print_string_literal(const char* string)
    {
        assert(string);

        printf("lit(\"");

        auto string_length = strlen(string);

        for (uint64_t i = 0; i < string_length; i++)
        {
            ir_print_character(string[i]);
        }

        printf("\")");
    }

	void ir_print_type(AST_Type* type)
	{
		assert(type);

		switch (type->kind)
		{
			case AST_TYPE_POINTER:
			{
				printf("*");
				ir_print_type(type->pointer.base);
				break;
			}

			case AST_TYPE_FUNCTION:
			{
				printf("(");
				for (uint64_t i = 0; i < BUF_LENGTH(type->function.arg_types); i++)
				{
					ir_print_type(type->function.arg_types[i]);
					if (i + 1 < BUF_LENGTH(type->function.arg_types))
						printf(", ");
				}
				printf(") -> ");

				ir_print_type(type->function.return_type);
				break;
			}

			case AST_TYPE_BASE:
			{
				bool sign = type->flags & AST_TYPE_FLAG_SIGNED;
				if (type->flags & AST_TYPE_FLAG_INT)
				{
					printf("%c%" PRIu64, sign ? 's' : 'u', type->bit_size);
				}
                else if (type->flags & AST_TYPE_FLAG_VOID)
                {
                    printf("void");
                }
                else if (type->flags & AST_TYPE_FLAG_FLOAT)
                {
                    printf("r%" PRIu64, type->bit_size);
                }
				else assert(false);
				break;
			}

            case AST_TYPE_ENUM:
            {
                bool sign = type->flags & AST_TYPE_FLAG_SIGNED;
                printf("enum(%c%" PRIu64 ")", sign ? 's' : 'u', type->bit_size);
                break;
            }

            case AST_TYPE_STRUCT:
            {
                if (type->name)
                {
                    printf("struct(%s)", type->name);
                }
                else
                {
                    printf("struct { ");
                    for (uint64_t i = 0; i < BUF_LENGTH(type->aggregate_type.member_declarations); i++)
                    {
                        if (i > 0)
                        {
                            printf(", ");
                        }

                        AST_Declaration* member_decl = type->aggregate_type.member_declarations[i];
                        assert(member_decl->kind == AST_DECL_MUTABLE);
                        ir_print_type(member_decl->mutable_decl.type);
                    }
                    printf(" }");
                }
                break;
            }

            case AST_TYPE_STATIC_ARRAY:
            {
                printf("[%" PRIu64 "]", type->static_array.count);
                ir_print_type(type->static_array.base);
                break;
            }

			default: assert(false);
		}
	}

    void ir_print_character(char c)
    {
        switch (c)
        {
            case '\n':
            {
                printf("\\n");
                break;
            }

            default:
            {
                printf("%c", c);
                break;
            }
        }
    }
}
