#include "ir.h"

#include "builtin.h"
#include "const_interpreter.h"
#include "ir_printer.h"

#include <inttypes.h>
#include <stdarg.h>

namespace Zodiac
{
    void ir_builder_init(IR_Builder* ir_builder, Context* context)
    {
        assert(ir_builder);

        ir_builder->context = context;
        ir_builder->arena = arena_create(MB(1));
        ir_builder->result = {};

        ir_builder->value_decl_hash =
            (IR_Value_And_Decl*)mem_alloc(sizeof(IR_Value_And_Decl) * 512);
        ir_builder->value_decl_count = 512;

        ir_builder->current_function = nullptr;
        ir_builder->insert_block = nullptr;

        stack_init(&ir_builder->scope_stack, 64);

    }

    IR_Module ir_builder_emit_module(IR_Builder* ir_builder, AST_Module* module)
    {
        assert(ir_builder);
        assert(module);

        assert(ir_builder->ast_module == nullptr);
        ir_builder->ast_module = module;

        stack_push(ir_builder->scope_stack, module->module_scope);

        for (uint64_t i = 0; i < BUF_LENGTH(module->import_modules); i++)
        {
            AST_Module* import_module = module->import_modules[i];
            assert(import_module->gen_data);
            if (import_module->poly_dirty)
            {
                ir_builder_emit_missing_poly_functions(ir_builder, import_module);
            }

            auto import_ir_module = &((IR_Builder*)import_module->gen_data)->result;
            BUF_PUSH(ir_builder->result.imported_modules, import_ir_module);
        }

        // Emit global declarations
        for (uint64_t i = 0; i < BUF_LENGTH(module->global_declarations); i++)
        {
            AST_Declaration* global_decl = module->global_declarations[i];
            ir_builder_emit_global_declaration(ir_builder, global_decl);
            if (ir_builder->result.error_count)
            {
                return ir_builder->result;
            }
        }

        // Emit function bodies
        for (uint64_t i = 0; i < BUF_LENGTH(module->global_declarations); i++)
        {
            AST_Declaration* decl = module->global_declarations[i];
            if (decl->flags & AST_DECL_FLAG_RESOLVED)
            {
                ir_builder_emit_decl_body(ir_builder, decl);
            }
            else
            {
                // TODO: Assert we are doing an insert, or some other pass where
                //        it's allowed to have unresolved declarations when emitting.
                // assert(false);
            }
        }

        module->gen_data = ir_builder;

        ir_builder->result.name = module->module_name;
        ir_builder->result.file_name = module->module_file_name;
        ir_builder->result.file_dir = module->module_file_dir;

        stack_pop(ir_builder->scope_stack);

        return ir_builder->result;
    }

    struct _Dirty_Overload
    {
        AST_Declaration* decl = nullptr;
        IR_Builder* ir_builder = nullptr;
    };

    void ir_builder_emit_missing_poly_functions(IR_Builder* ir_builder, AST_Module* module)
    {
        assert(ir_builder);
        assert(module);

        for (uint64_t i = 0; i < BUF_LENGTH(module->import_modules); i++)
        {
            AST_Module* import_module = module->import_modules[i];
            assert(import_module->gen_data);
            if (import_module->poly_dirty)
            {
                ir_builder_emit_missing_poly_functions(ir_builder, import_module);
            }
        }

        BUF(_Dirty_Overload) dirty_overloads = nullptr;

        for (uint64_t i = 0; i < BUF_LENGTH(module->global_declarations); i++)
        {
            AST_Declaration* global_decl = module->global_declarations[i];

            AST_Module* overload_module = nullptr;
            IR_Builder* overload_ir_builder = nullptr;

            bool dirty = false;
            if (global_decl->kind == AST_DECL_FUNC_OVERLOAD)
            {
                overload_module = global_decl->scope->module;
                overload_ir_builder = (IR_Builder*)overload_module->gen_data;

                for (uint64_t j = 0; j < BUF_LENGTH(global_decl->function_overload.overloads); j++)
                {
                    AST_Declaration* overload = global_decl->function_overload.overloads[j];

                    if (overload->flags & AST_DECL_FLAG_FUNC_POLY)
                    {
                        IR_Value* value = ir_builder_value_for_declaration(ir_builder, overload);
                        if (!value)
                        {
                            dirty = true;
                        }
                    }
                }
            }

            if (dirty)
            {
                ir_builder_emit_global_declaration(overload_ir_builder, global_decl);
                _Dirty_Overload _do = { global_decl, overload_ir_builder };
                BUF_PUSH(dirty_overloads, _do);
            }
        }

        for (uint64_t i = 0; i < BUF_LENGTH(dirty_overloads); i++)
        {
            auto _do = dirty_overloads[i];
            ir_builder_emit_decl_body(_do.ir_builder, _do.decl);
        }

        BUF_FREE(dirty_overloads);

        module->poly_dirty = false;
    }

    void ir_builder_emit_decl_body(IR_Builder* ir_builder, AST_Declaration* decl)
    {
        assert(ir_builder);
        assert(decl);

        switch (decl->kind)
        {
            case AST_DECL_FUNC:
            {
                if (!(decl->flags & AST_DECL_FLAG_FUNC_OVERLOAD))
                {
                    ir_builder_emit_function_body(ir_builder, decl);
                }
                break;
            }

            case AST_DECL_STATIC_IF:
            {
                bool cond = const_interpret_bool_expression(ir_builder->context,
                                                            decl->static_if.cond_expr,
                                                            ir_builder->ast_module->module_scope);
                if (cond)
                {
                    ir_builder_emit_decl_body(ir_builder, decl->static_if.then_declaration);
                }
                else if (decl->static_if.else_declaration)
                {
                    ir_builder_emit_decl_body(ir_builder, decl->static_if.else_declaration);
                }
                break;
            }

            case AST_DECL_BLOCK:
            {
                for (uint64_t i = 0; i < BUF_LENGTH(decl->block.decls); i++)
                {
                    ir_builder_emit_decl_body(ir_builder, decl->block.decls[i]);
                }
                break;
            }

            case AST_DECL_FUNC_OVERLOAD:
            {
                auto overloads = decl->function_overload.overloads;
                for (uint64_t i = 0; i < BUF_LENGTH(overloads); i++)
                {
                    auto overload_decl = overloads[i];
                    assert(overload_decl->kind == AST_DECL_FUNC);
                    if (!overload_decl->function.body_generated)
                    {
                        ir_builder_emit_function_body(ir_builder, overload_decl);
                    }
                }

                break;
            }

            default:
            {
                break;
            }
        }
    }

    void ir_builder_emit_function_body(IR_Builder* ir_builder, AST_Declaration* decl)
    {
        assert(ir_builder);
        assert(decl);
        assert(decl->kind == AST_DECL_FUNC);

        if (decl->function.body_generated) return;
        if (decl->flags & AST_DECL_FLAG_FUNC_POLY_TEMPLATE) return;

        IR_Value* ir_value = ir_builder_value_for_declaration(ir_builder, decl);
        assert(ir_value);
        assert(ir_value->kind == IRV_FUNCTION);

        IR_Function* func = ir_value->function;
        IR_Block* entry_block = func->first_block;

        if (decl->function.body_block)
        {
            stack_push(ir_builder->scope_stack, decl->function.argument_scope);

            ir_builder->current_function = func;
            ir_builder_set_insert_block(ir_builder, entry_block);

            ir_builder_emit_statement(ir_builder, decl->function.body_block,
                                        decl->function.body_block->block.scope, nullptr);

            auto insert_block = ir_builder->insert_block;
            auto first_instruction = insert_block->first_instruction;
            auto last_instruction = insert_block->last_instruction;

            if (!last_instruction ||
                !ir_instruction_is_terminator(last_instruction->op))
            {
                File_Pos fp;
                fp.file_name = "<generated return>";
                IR_Value* ret_value = ir_builder_emit_zero_literal(ir_builder,
                                                                decl->function.return_type);
                ir_builder_emit_return(ir_builder, ret_value, fp);
            }

            ir_builder->current_function = nullptr;

            ir_builder_patch_empty_block_jumps(ir_builder, func);

            decl->function.body_generated = true;

            stack_pop(ir_builder->scope_stack); // Argument scope
        }
        else
        {
            assert(decl->directive);
            assert(decl->directive->kind == AST_DIREC_FOREIGN);
        }
    }

    void ir_builder_emit_global_declaration(IR_Builder* ir_builder, AST_Declaration* global_decl)
    {
        assert(ir_builder);
        assert(global_decl);

        if (!(global_decl->flags & AST_DECL_FLAG_RESOLVED))
        {
            return;
        }

        assert(global_decl->location == AST_DECL_LOC_GLOBAL);

        switch (global_decl->kind)
        {
            case AST_DECL_FUNC:
            {
                if (global_decl->flags & AST_DECL_FLAG_FUNC_POLY_TEMPLATE)
                {
                    return;
                }

                bool is_overload = global_decl->flags & AST_DECL_FLAG_FUNC_OVERLOAD;
                bool is_poly = global_decl->flags & AST_DECL_FLAG_FUNC_POLY;

                if (!is_overload && !is_poly)
                {
                    ir_builder_emit_function_declaration(ir_builder, global_decl);
                }
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
                IR_Value* value = ir_builder_emit_global(ir_builder, global_decl);
                value->flags |= IRV_FLAG_CONST;
                ir_builder_push_value_and_decl(ir_builder, value, global_decl);
                IR_Global_Constant gc = { global_decl->identifier->atom.data,
                                          value->global.init_value };
                BUF_PUSH(ir_builder->result.global_constants, gc);
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
                auto module_scope = ir_builder->ast_module->module_scope;
                AST_Expression* cond_expr = global_decl->static_if.cond_expr;
                // IR_Value* cond_value = ir_builder_emit_expression(ir_builder, cond_expr);
                bool cond_value = const_interpret_bool_expression(ir_builder->context, cond_expr,
                                                                module_scope);
                // assert(cond_value->type == Builtin::type_bool);
                if (cond_value)
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
                // IR_Value* cond_value = ir_builder_emit_expression(ir_builder, assert_expr);
                assert(assert_expr->type == Builtin::type_bool);

                bool cond_value = const_interpret_bool_expression(ir_builder->context, assert_expr,
                                                                  ir_builder->ast_module->module_scope);

                if (!cond_value)
                // if (!cond_value->value.boolean)
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
                // TODO: FIXME: These should be in the enums scope instead of global scope
                if (global_decl->aggregate_type.kind == AST_AGG_DECL_ENUM)
                {
                    auto agg_members = global_decl->aggregate_type.aggregate_decl->members;
                    for (uint64_t i = 0; i < BUF_LENGTH(agg_members); i++)
                    {
                        AST_Declaration* enum_mem = agg_members[i];
                        AST_Expression* init_expr = enum_mem->constant_var.init_expression;
                        IR_Value* value = ir_builder_emit_expression(ir_builder, init_expr);
                        ir_builder_push_value_and_decl(ir_builder, value, enum_mem);
                    }
                }
                break;
            }

            case AST_DECL_TYPEDEF:
            {
                break;
            }

            case AST_DECL_USING:
            {
                break;
            }

            case AST_DECL_INSERT:
            {
                break;
            }

            case AST_DECL_FUNC_OVERLOAD:
            {
                auto overloads = global_decl->function_overload.overloads;
                for (uint64_t i = 0; i < BUF_LENGTH(overloads); i++)
                {
                    auto overload_decl = overloads[i];

                    auto value = ir_builder_value_for_declaration(ir_builder, overload_decl);
                    if (!value)
                    {
                        ir_builder_emit_function_declaration(ir_builder, overload_decl);
                    }
                }

                break;
            }

            default: assert(false);
        }
    }

    void ir_builder_emit_function_declaration(IR_Builder* ir_builder, AST_Declaration* decl)
    {
        assert(ir_builder);
        assert(decl);
        assert(decl->kind == AST_DECL_FUNC);

        AST_Identifier* ident = decl->identifier;
        AST_Type* return_type = decl->function.return_type;

        AST_Scope* body_scope = nullptr;
        if (!(decl->flags & AST_DECL_FLAG_FOREIGN))
        {
            body_scope = decl->function.body_block->block.scope;
        }

        IR_Value* func_value = ir_builder_begin_function(ir_builder, decl->file_pos,
                                                        ident->atom.data,
                                                        decl->function.type,
                                                        body_scope);

        if (decl->function.body_block)
        {
            IR_Value* entry_block = ir_builder_create_block(ir_builder, "entry",
                                                            func_value);
            ir_builder_set_insert_block(ir_builder, entry_block);

            for (uint64_t i = 0; i < BUF_LENGTH(decl->function.args); i++)
            {
                AST_Declaration* arg_decl = decl->function.args[i];
                assert(arg_decl->kind == AST_DECL_MUTABLE);
                assert(arg_decl->location == AST_DECL_LOC_ARGUMENT);

                AST_Identifier* arg_ident = arg_decl->identifier;
                AST_Type* arg_type = arg_decl->mutable_decl.type;
                if (arg_type->kind == AST_TYPE_STATIC_ARRAY)
                {
                    arg_type = ast_find_or_create_pointer_type(ir_builder->context, arg_type);
                }
                IR_Value* arg_value = ir_builder_emit_function_arg(ir_builder,
                                                                arg_ident->atom.data,
                                                                arg_type, arg_ident->file_pos);
                ir_builder_push_value_and_decl(ir_builder, arg_value, arg_decl);
            }
        }
        else
        {
            func_value->function->flags |= IR_FUNC_FLAG_FOREIGN;
            if (decl->flags & AST_DECL_FLAG_FUNC_VARARG)
            {
                func_value->function->flags |= IR_FUNC_FLAG_VARARG;
            }
            func_value->function->foreign_index = ir_builder_emit_foreign(ir_builder, decl);
        }

        ir_builder_end_function(ir_builder, func_value);

        // printf("emitted function: %s\n", decl->identifier->atom.data);
        ir_builder_push_value_and_decl(ir_builder, func_value, decl);
    }

    void ir_builder_emit_statement(IR_Builder* ir_builder, AST_Statement* statement,
                                AST_Scope* scope, IR_Value* break_block)
    {
        assert(ir_builder);
        assert(statement);

        switch (statement->kind)
        {
            case AST_STMT_DECLARATION:
            {
                AST_Declaration* decl = statement->declaration;
                if (decl->kind == AST_DECL_MUTABLE)
                {
                    assert(decl->location == AST_DECL_LOC_LOCAL);

                    IR_Value* allocl = ir_builder_emit_allocl(ir_builder, decl->mutable_decl.type,
                                                            decl->identifier->atom.data,
                                                            decl->file_pos);

                    ir_builder_push_value_and_decl(ir_builder, allocl, decl);

                    AST_Expression* init_expr = decl->mutable_decl.init_expression;
                    if (init_expr)
                    {
                        IR_Value* init_value = ir_builder_emit_expression(ir_builder, init_expr);
                        AST_Type* init_type = init_value->type;

                        if (init_type->kind == AST_TYPE_STRUCT &&
                            (init_type->flags & AST_TYPE_FLAG_FROM_MRV))
                        {
                            IR_Value* mrv_value = init_value;
                            assert(BUF_LENGTH(init_type->aggregate_type.member_declarations));
                            auto member_decls = init_type->aggregate_type.member_declarations;
                            assert(member_decls[0]->mutable_decl.type == allocl->type);
                            auto init_fp = init_expr->file_pos;

                            init_value = ir_builder_emit_extract_value(ir_builder, mrv_value,
                                                                       0, init_fp);
                        }

                        ir_builder_emit_storel(ir_builder, allocl, init_value, decl->file_pos);
                    }
                }
                else if (decl->kind == AST_DECL_USING)
                {
                    // Do nothing
                }
                else if (decl->kind == AST_DECL_CONSTANT_VAR)
                {
                    AST_Expression* init_expr = decl->constant_var.init_expression;
                    assert(init_expr->flags & AST_EXPR_FLAG_CONST);
                    assert(init_expr->flags & AST_EXPR_FLAG_LITERAL);
                    IR_Value* constant_value = ir_builder_emit_expression(ir_builder, init_expr);

                    ir_builder_push_value_and_decl(ir_builder, constant_value, decl);

                    // // TODO: Emit intit expression and store in global init block
                    // assert(false);
                }
                else if (decl->kind == AST_DECL_LIST)
                {
                    AST_Type* mrv_struct_type = decl->list.init_expression->type->mrv.struct_type;
                    File_Pos init_fp = decl->list.init_expression->file_pos;
                    IR_Value* mrv_struct = ir_builder_emit_expression(ir_builder,
                                                                      decl->list.init_expression);

                    AST_Expression* list_expr = decl->list.list_expression;
                    uint64_t decl_idx = 0;
                    for (uint64_t i = 0; i < BUF_LENGTH(list_expr->list.expressions); i++)
                    {
                        AST_Expression* expr = list_expr->list.expressions[i];
                        if (expr->kind != AST_EXPR_IGNORED_VALUE)
                        {
                            AST_Declaration* list_decl = decl->list.declarations[decl_idx++];
                            assert(list_decl->location == AST_DECL_LOC_LOCAL);

                            auto name = list_decl->identifier->atom.data;

                            IR_Value* allocl = ir_builder_emit_allocl(ir_builder,
                                                                    list_decl->mutable_decl.type,
                                                                    name,
                                                                    list_decl->file_pos);
                            ir_builder_push_value_and_decl(ir_builder, allocl, list_decl);

                            IR_Value* init_value =
                                ir_builder_emit_extract_value(ir_builder, mrv_struct, i,
                                                              list_decl->file_pos);
                            ir_builder_emit_storel(ir_builder, allocl, init_value,
                                                list_decl->file_pos);
                        }
                    }
                }
                else assert(false);
                break;
            }

            case AST_STMT_RETURN:
            {
                auto return_file_pos = statement->file_pos;

                IR_Value* return_value = nullptr;
                if (statement->return_expression)
                {
                    if (statement->return_expression->kind == AST_EXPR_EXPRESSION_LIST)
                    {
                        return_value = ir_builder_emit_mrv(ir_builder,
                                                           statement->return_expression);
                    }
                    else
                    {
                        return_value = ir_builder_emit_expression(ir_builder,
                                                                  statement->return_expression);
                    }
                    ir_builder_emit_defer_statements_before_return(ir_builder, scope,
                                                                    return_file_pos);
                }
                ir_builder_emit_return(ir_builder, return_value, return_file_pos);
                break;
            }

            case AST_STMT_BLOCK:
            {
                auto block_scope = statement->block.scope;
                stack_push(ir_builder->scope_stack, block_scope);
                for (uint64_t i = 0; i < BUF_LENGTH(statement->block.statements); i++)
                {
                    AST_Statement* block_member_stmt = statement->block.statements[i];
                    ir_builder_emit_statement(ir_builder, block_member_stmt, block_scope,
                                            break_block);
                }
                stack_pop(ir_builder->scope_stack); // Block scope

                auto last_iri = ir_builder->insert_block->last_instruction;

                if (last_iri)
                {
                    if (last_iri->op == IR_OP_RETURN ||
                        last_iri->op == IR_OP_JMP)
                        break;
                }

                auto defer_statements = statement->block.scope->defer_statements;
                if (defer_statements)
                {
                    // We don't have to check the order here since this is always
                    //  the end of the block.
                    for (uint64_t i = 0; i < BUF_LENGTH(defer_statements); i++)
                    {
                        uint64_t index = BUF_LENGTH(defer_statements) - 1 - i;
                        AST_Statement* defer_stmt = defer_statements[index];
                        ir_builder_emit_statement(ir_builder, defer_stmt, scope, nullptr);
                    }
                }
                break;
            }

            case AST_STMT_IF:
            {
                ir_builder_emit_if(ir_builder, statement->if_stmt.if_expression,
                                statement->if_stmt.then_statement,
                                statement->if_stmt.else_statement,
                                scope, break_block,
                                statement->file_pos);
                break;
            }

            case AST_STMT_STATIC_IF:
            {
                bool cond = const_interpret_bool_expression(
                    ir_builder->context, statement->static_if_stmt.if_expression, scope);

                if (cond)
                {
                    ir_builder_emit_statement(ir_builder,
                                            statement->static_if_stmt.then_statement, scope,
                                            break_block);
                }
                else if (statement->static_if_stmt.else_statement)
                {
                    ir_builder_emit_statement(ir_builder,
                                            statement->static_if_stmt.else_statement, scope,
                                            break_block);
                }
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
                                                                        "while_cond",
                                                                        cur_func);
                IR_Value* while_body_block_value = ir_builder_create_block(ir_builder,
                                                                        "while_body",
                                                                        cur_func);
                IR_Value* post_while_block_value = ir_builder_create_block(ir_builder,
                                                                        "post_while");

                ir_builder_emit_jmp(ir_builder, while_cond_block_value, statement->file_pos);

                ir_builder_set_insert_block(ir_builder, while_cond_block_value);
                ir_builder_emit_if_cond(ir_builder, statement->while_stmt.cond_expr,
                                        while_body_block_value, post_while_block_value,
                                        statement->file_pos);
                // IR_Value* cond_value =
                //     ir_builder_emit_expression(ir_builder, statement->while_stmt.cond_expr);
                // ir_builder_emit_jmp_if(ir_builder, cond_value, while_body_block_value,
                //                        statement->file_pos);
                // ir_builder_emit_jmp(ir_builder, post_while_block_value, statement->file_pos);

                ir_builder_set_insert_block(ir_builder, while_body_block_value);
                ir_builder_emit_statement(ir_builder, statement->while_stmt.body_stmt,
                                        scope, post_while_block_value);
                ir_builder_emit_jmp(ir_builder, while_cond_block_value, statement->file_pos);

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


                ir_builder_emit_statement(ir_builder, statement->for_stmt.init_stmt, scope,
                                        break_block);
                ir_builder_emit_jmp(ir_builder, for_cond_block_value, statement->file_pos);

                ir_builder_set_insert_block(ir_builder, for_cond_block_value);
                IR_Value* cond_value = ir_builder_emit_expression(ir_builder,
                                                                statement->for_stmt.cond_expr);
                ir_builder_emit_jmp_if(ir_builder, cond_value, for_body_block_value,
                                    statement->file_pos);
                ir_builder_emit_jmp(ir_builder, post_for_block_value, statement->file_pos);

                ir_builder_set_insert_block(ir_builder, for_body_block_value);
                ir_builder_emit_statement(ir_builder, statement->for_stmt.body_stmt, scope,
                                        post_for_block_value);
                ir_builder_emit_statement(ir_builder, statement->for_stmt.step_stmt, scope,
                                        break_block);
                ir_builder_emit_jmp(ir_builder, for_cond_block_value, statement->file_pos);

                ir_builder_append_block(ir_builder, cur_func, post_for_block_value->block);
                ir_builder_set_insert_block(ir_builder, post_for_block_value);
                break;
            }

            case AST_STMT_SWITCH:
            {
                ir_builder_emit_switch_statement(ir_builder, statement, scope, break_block);
                break;
            }

            case AST_STMT_BREAK:
            {
                assert(break_block);
                ir_builder_emit_defer_statements_before_break(ir_builder, scope,
                                                            statement->file_pos);
                ir_builder_emit_jmp(ir_builder, break_block, statement->file_pos);
                break;
            }

            case AST_STMT_INSERT:
            {
                assert(statement->insert.gen_statement);
                ir_builder_emit_statement(ir_builder, statement->insert.gen_statement,
                                        scope, break_block);
                break;
            }

            case AST_STMT_ASSERT:
            {
                IR_Value* assert_value = ir_builder_emit_expression(ir_builder,
                                                                    statement->assert_expression);
                ir_builder_emit_assert(ir_builder, assert_value, statement->file_pos);

                break;
            }

            case AST_STMT_ASSERT_FAIL:
            {
                IR_Instruction* iri = ir_instruction_new(ir_builder, statement->file_pos,
                                                        IR_OP_ASSERT_FAIL,
                                                        nullptr, nullptr, nullptr);
                ir_builder_emit_instruction(ir_builder, iri);
                break;
            }

            case AST_STMT_DEFER:
            {
                // These are handled at the end of blocks and functions
                break;
            }

            case AST_STMT_POST_INCREMENT:
            {
                AST_Expression* base_expression = statement->post_increment->base_expression;
                IR_Value* lvalue = ir_builder_emit_lvalue(ir_builder, base_expression);
                IR_Value* old_value = ir_builder_emit_expression(ir_builder, base_expression);

                IR_Value* new_value = nullptr;
                if (statement->post_increment->flags & AST_EXPR_FLAG_POINTER_MATH)
                {
                    IR_Value* one_value = ir_integer_literal(ir_builder, Builtin::type_u64, 1);
                    new_value = ir_builder_emit_pointer_math(ir_builder, old_value, one_value,
                                                            AST_BINOP_ADD, false,
                                                            statement->file_pos);
                }
                else
                {
                    IR_Value* one_value = ir_integer_literal(ir_builder,
                                                            statement->post_increment->type, 1);
                    new_value = ir_builder_emit_add(ir_builder, old_value, one_value,
                                                    statement->file_pos);
                }
                assert(new_value);

                ir_builder_emit_store(ir_builder, lvalue, new_value, statement->file_pos);
                break;
            }

            case AST_STMT_POST_DECREMENT:
            {
                AST_Expression* base_expression = statement->post_decrement->base_expression;
                IR_Value* lvalue = ir_builder_emit_lvalue(ir_builder, base_expression);
                IR_Value* old_value = ir_builder_emit_expression(ir_builder, base_expression);


                IR_Value* new_value = nullptr;
                if (statement->post_decrement->flags & AST_EXPR_FLAG_POINTER_MATH)
                {
                    IR_Value* one_value = ir_integer_literal(ir_builder, Builtin::type_u64, 1);
                    new_value = ir_builder_emit_pointer_math(ir_builder, old_value, one_value,
                                                            AST_BINOP_SUB, false,
                                                            statement->file_pos);
                }
                else
                {
                    IR_Value* one_value = ir_integer_literal(ir_builder,
                                                            statement->post_decrement->type, 1);
                    new_value = ir_builder_emit_sub(ir_builder, old_value, one_value,
                                        statement->file_pos);
                }
                assert(new_value);

                ir_builder_emit_store(ir_builder, lvalue, new_value, statement->file_pos);
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
        AST_Expression* expr = statement->assign.expression;

        if (lvalue_expr->kind == AST_EXPR_EXPRESSION_LIST)
        {
            assert(expr->kind == AST_EXPR_CALL);
            assert(expr->type->kind == AST_TYPE_MRV);

            BUF(IR_Value*) lvalues = nullptr;
            for (uint64_t i = 0; i < BUF_LENGTH(lvalue_expr->list.expressions); i++)
            {
                auto list_expr = lvalue_expr->list.expressions[i];
                if (list_expr->kind != AST_EXPR_IGNORED_VALUE)
                {
                    IR_Value* lvalue = ir_builder_emit_lvalue(ir_builder, list_expr);
                    BUF_PUSH(lvalues, lvalue);
                }
                else
                {
                    BUF_PUSH(lvalues, nullptr);
                }
            }

            IR_Value* ret_value = ir_builder_emit_expression(ir_builder, expr);

            auto fp = statement->file_pos;
            for (uint64_t i = 0; i < BUF_LENGTH(lvalues); i++)
            {
                IR_Value* lvalue = lvalues[i];
                if (lvalue)
                {
                    IR_Value* new_value = ir_builder_emit_extract_value(ir_builder, ret_value,
                                                                        i, fp);
                    ir_builder_emit_store(ir_builder, lvalue, new_value, fp);
                }
            }

            BUF_FREE(lvalues);

        }
        else
        {
            IR_Value* lvalue = ir_builder_emit_lvalue(ir_builder, lvalue_expr);
            IR_Value* new_value = ir_builder_emit_expression(ir_builder,
                                                             statement->assign.expression);
            ir_builder_emit_store(ir_builder, lvalue, new_value, statement->file_pos);
        }
    }

    struct _IR_Case
    {
        IR_Value* case_block = nullptr;
    };

    void ir_builder_emit_switch_statement(IR_Builder* ir_builder, AST_Statement* statement,
                                        AST_Scope* scope, IR_Value* break_block)
    {
        assert(ir_builder);
        assert(statement);
        assert(statement->kind == AST_STMT_SWITCH);


        IR_Function* cur_func = ir_builder->current_function;

        auto switch_expr = statement->switch_stmt.switch_expression;
        IR_Value* switch_value = ir_builder_emit_expression(ir_builder, switch_expr);

        IR_Block* origin_block = ir_builder->insert_block;
        IR_Value* first_range_case_block_val = nullptr;
        IR_Value* last_range_case_block_val = nullptr;
        IR_Value* post_switch_block_val = ir_builder_create_block(ir_builder, "post_switch");
        IR_Value* default_or_post_switch_block_val = post_switch_block_val;

        BUF(IR_Case_Pair) case_pairs = nullptr;
        BUF(IR_Value*) case_blocks = nullptr;
        BUF(IR_Value*) range_blocks = nullptr;
        IR_Value* default_block_value = nullptr;

        for (uint64_t i = 0; i < BUF_LENGTH(statement->switch_stmt.cases); i++)
        {
            const auto& switch_case = statement->switch_stmt.cases[i];

            auto block_name = switch_case.is_default ? "default" : "case";
            IR_Value* case_block_val = ir_builder_create_block(ir_builder, block_name, cur_func);
            ir_builder_set_insert_block(ir_builder, case_block_val);
            ir_builder_emit_statement(ir_builder, switch_case.stmt, scope,
                                    post_switch_block_val);
            ir_builder_emit_jmp(ir_builder, post_switch_block_val, switch_case.file_pos);

            BUF_PUSH(case_blocks, case_block_val);

            if (switch_case.is_default)
            {
                default_block_value = case_block_val;
            }
        }

        if (default_block_value)
        {
            default_or_post_switch_block_val = default_block_value;
        }

        for (uint64_t i = 0; i < BUF_LENGTH(statement->switch_stmt.cases); i++)
        {
            const auto& switch_case = statement->switch_stmt.cases[i];

            auto case_block_val = case_blocks[i];

            if (case_block_val != default_block_value)
            {
                IR_Case_Pair pair;
                pair.dest_block_value = case_block_val;

                for (uint64_t j = 0; j < BUF_LENGTH(switch_case.case_expressions); j++)
                {
                    AST_Expression* case_expr = switch_case.case_expressions[j];
                    assert((case_expr->type->flags & AST_TYPE_FLAG_INT) ||
                        case_expr->type->kind == AST_TYPE_ENUM);
                    assert(case_expr->flags & AST_EXPR_FLAG_CONST);

                    IR_Value* case_expr_val = ir_builder_emit_expression(ir_builder, case_expr);

                    assert(case_expr_val->kind == IRV_INT_LITERAL);

                    pair.value = case_expr_val;
                    BUF_PUSH(case_pairs, pair);
                }

                for (uint64_t j = 0; j < BUF_LENGTH(switch_case.range_expressions); j+=2)
                {
                    AST_Expression* min_expr = switch_case.range_expressions[j];
                    AST_Expression* max_expr = switch_case.range_expressions[j + 1];

                    IR_Value* range_case_block = ir_builder_create_block(ir_builder,
                                                                        "range_case");
                    if (!first_range_case_block_val)
                    {
                        first_range_case_block_val = range_case_block;
                    }

                    ir_builder_set_insert_block(ir_builder, range_case_block);
                    IR_Value* min_value = ir_builder_emit_expression(ir_builder, min_expr);
                    IR_Value* max_value = ir_builder_emit_expression(ir_builder, max_expr);

                    IR_Value* gteq_min_val = ir_builder_emit_gteq(ir_builder, switch_value,
                                                                min_value,
                                                                switch_case.file_pos);
                    IR_Value* lteq_max_val = ir_builder_emit_lteq(ir_builder, switch_value,
                                                                max_value,
                                                                switch_case.file_pos);
                    IR_Value* range_cond_value = ir_builder_emit_and(ir_builder, gteq_min_val,
                                                                    lteq_max_val,
                                                                    switch_case.file_pos);

                    ir_builder_emit_jmp_if(ir_builder, range_cond_value, pair.dest_block_value,
                                        switch_case.file_pos);
                    auto jmp_target = last_range_case_block_val ? last_range_case_block_val :
                        default_or_post_switch_block_val;
                    ir_builder_emit_jmp(ir_builder, jmp_target, switch_case.file_pos);

                    BUF_PUSH(range_blocks, range_case_block);
                    last_range_case_block_val = range_case_block;
                }
            }
        }

        for (uint64_t i = 0; i < BUF_LENGTH(range_blocks); i++)
        {
            IR_Value* range_block_val = range_blocks[i];
            ir_builder_append_block(ir_builder, cur_func, range_block_val->block);
        }

        if (last_range_case_block_val)
        {
            ir_builder_set_insert_block(ir_builder, last_range_case_block_val);
            ir_builder_emit_jmp(ir_builder, default_or_post_switch_block_val,
                                statement->file_pos);
        }

        ir_builder_set_insert_block(ir_builder, origin_block);

        IR_Value* default_or_last_range_case_block_val = last_range_case_block_val ?
            last_range_case_block_val : default_or_post_switch_block_val;
        IR_Instruction* iri = ir_instruction_new(ir_builder, statement->file_pos, IR_OP_SWITCH,
                                                switch_value,
                                                default_or_last_range_case_block_val, nullptr);
        iri->case_pairs = case_pairs;
        ir_builder_emit_instruction(ir_builder, iri);

        ir_builder_append_block(ir_builder, cur_func, post_switch_block_val->block);
        ir_builder_set_insert_block(ir_builder, post_switch_block_val);

        BUF_FREE(case_blocks);
        BUF_FREE(range_blocks);
    }

    IR_Value* ir_builder_emit_expression(IR_Builder* ir_builder, AST_Expression* expression)
    {
        assert(ir_builder);
        assert(expression);

        switch (expression->kind)
        {
            case AST_EXPR_BINARY:
            {
                if (expression->binary.call_expression)
                {
                    return ir_builder_emit_expression(ir_builder,
                                                    expression->binary.call_expression);
                }
                else if (expression->binary.op == AST_BINOP_AND_AND ||
                        expression->binary.op == AST_BINOP_OR_OR)
                {
                    return ir_builder_emit_cond_expr(ir_builder, expression,
                                                    expression->file_pos);
                }
                else
                {
                    IR_Value* lhs_value = ir_builder_emit_expression(ir_builder,
                                                                    expression->binary.lhs);
                    IR_Value* rhs_value = ir_builder_emit_expression(ir_builder,
                                                                    expression->binary.rhs);

                    if (expression->flags & AST_EXPR_FLAG_POINTER_MATH)
                    {
                        IR_Value* pointer_value = nullptr;
                        IR_Value* int_value = nullptr;
                        bool reversed = false;
                        if (lhs_value->type->kind == AST_TYPE_POINTER)
                        {
                            assert(rhs_value->type->flags & AST_TYPE_FLAG_INT);
                            pointer_value = lhs_value;
                            int_value = rhs_value;
                        }
                        else
                        {
                            assert(lhs_value->type->flags & AST_TYPE_FLAG_INT);
                            assert(rhs_value->type->kind == AST_TYPE_POINTER);

                            pointer_value = rhs_value;
                            int_value = lhs_value;
                            reversed = true;
                        }
                        return ir_builder_emit_pointer_math(ir_builder, pointer_value, int_value,
                                                            expression->binary.op, reversed,
                                                            expression->file_pos);

                    }

                    switch (expression->binary.op)
                    {
                        case AST_BINOP_ADD:
                            return ir_builder_emit_add(ir_builder, lhs_value, rhs_value,
                                                    expression->file_pos);

                        case AST_BINOP_SUB:
                            return ir_builder_emit_sub(ir_builder, lhs_value, rhs_value,
                                                    expression->file_pos);

                        case AST_BINOP_DIV:
                            return ir_builder_emit_div(ir_builder, lhs_value, rhs_value,
                                                    expression->file_pos);

                        case AST_BINOP_MUL:
                            return ir_builder_emit_mul(ir_builder, lhs_value, rhs_value,
                                                    expression->file_pos);

                        case AST_BINOP_MOD:
                            return ir_builder_emit_mod(ir_builder, lhs_value, rhs_value,
                                                    expression->file_pos);

                        case AST_BINOP_LSHIFT:
                        case AST_BINOP_RSHIFT:
                            return ir_builder_emit_bitshift(ir_builder, lhs_value, rhs_value,
                                                            expression->binary.op,
                                                            expression->file_pos);
                        case AST_BINOP_LT:
                            return ir_builder_emit_lt(ir_builder, lhs_value, rhs_value,
                                                    expression->file_pos);

                        case AST_BINOP_LTEQ:
                            return ir_builder_emit_lteq(ir_builder, lhs_value, rhs_value,
                                                        expression->file_pos);

                        case AST_BINOP_GT:
                            return ir_builder_emit_gt(ir_builder, lhs_value, rhs_value,
                                                    expression->file_pos);

                        case AST_BINOP_GTEQ:
                            return ir_builder_emit_gteq(ir_builder, lhs_value, rhs_value,
                                                        expression->file_pos);

                        case AST_BINOP_EQ:
                            return ir_builder_emit_eq(ir_builder, lhs_value, rhs_value,
                                                    expression->file_pos);

                        case AST_BINOP_NEQ:
                            return ir_builder_emit_neq(ir_builder, lhs_value, rhs_value,
                                                    expression->file_pos);

                        case AST_BINOP_AND:
                            return ir_builder_emit_and(ir_builder, lhs_value, rhs_value,
                                                    expression->file_pos);

                        case AST_BINOP_OR:
                            return ir_builder_emit_or(ir_builder, lhs_value, rhs_value,
                                                    expression->file_pos);

                        default: assert(false);
                    }
                }
            }

            case AST_EXPR_UNARY:
            {
                switch (expression->unary.op)
                {
                    case AST_UNOP_MINUS:
                    {
                        return ir_builder_emit_negate(ir_builder, expression->unary.operand,
                                                    expression->file_pos);
                        break;
                    }

                    case AST_UNOP_ADDROF:
                    {
                        if (expression->type->kind == AST_TYPE_POINTER &&
                            expression->type->pointer.base->kind == AST_TYPE_STATIC_ARRAY &&
                            expression->unary.operand->type->kind == AST_TYPE_STATIC_ARRAY)
                        {
                            AST_Type* expected_array_type = expression->type->pointer.base;
                            AST_Type* operand_array_type = expression->unary.operand->type;
                            assert(expected_array_type == operand_array_type);

                           IR_Value* operand_lvalue =
                               ir_builder_emit_lvalue(ir_builder, expression->unary.operand);

                           return operand_lvalue;
                        }
                        else
                        {
                            return ir_builder_emit_addrof(ir_builder, expression->unary.operand,
                                                          expression->file_pos);
                        }
                        break;
                    }

                    case AST_UNOP_DEREF:
                    {
                        return ir_builder_emit_deref(ir_builder, expression->unary.operand,
                                                    expression->file_pos);
                        break;
                    }

                    case AST_UNOP_NOT:
                    {
                        return ir_builder_emit_not(ir_builder, expression->unary.operand,
                                                expression->file_pos);
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

                if (!value)
                {
                    fprintf(stderr, "Expected to find ir value for identifier: %s\n",
                            expression->identifier->atom.data);
                    fprintf(stderr, "at: %s:%d:%d\n", expression->file_pos.file_name,
                            (int)expression->file_pos.line,
                            (int)expression->file_pos.line_relative_char_pos);
                    assert(value);
                }

                if (value->kind == IRV_TEMPORARY ||
                    value->kind == IRV_INT_LITERAL ||
                    value->kind == IRV_BOOL_LITERAL)
                {
                    // Do nothing else for now
                }
                else if (value->kind == IRV_ALLOCL)
                {
                    value = ir_builder_emit_loadl(ir_builder, value, expression->file_pos);
                }
                else if (value->kind == IRV_ARGUMENT)
                {
                    value = ir_builder_emit_loada(ir_builder, value, expression->file_pos);
                }
                else if (value->kind == IRV_GLOBAL)
                {
                    if (value->flags & IRV_FLAG_CONST)
                    {
                        value = value->global.init_value;
                    }
                    else
                    {
                        value = ir_builder_emit_loadg(ir_builder, value, expression->file_pos);
                    }
                }
                else if (value->kind == IRV_FUNCTION)
                {
                    value = ir_builder_emit_addrof_function(ir_builder, value, expression->type,
                                                            expression->file_pos);
                }
                else assert(false);

                return  value;

                break;
            }

            case AST_EXPR_CALL:
            {
                AST_Declaration* callee_decl = expression->call.callee_declaration;

                if (!callee_decl)
                {
                    assert(expression->call.builtin_function != AST_BUILTIN_FUNC_INVALID);

                    return ir_builder_emit_builtin_function_call(ir_builder, expression);
                }

                assert(callee_decl);
                if (callee_decl->kind == AST_DECL_FUNC)
                {
                    IR_Value* callee_value = ir_builder_value_for_declaration(ir_builder,
                                                                            callee_decl);
                    assert(callee_value);
                    assert(callee_value->kind == IRV_FUNCTION);

                    AST_Type* func_type = callee_decl->function.type;
                    assert(func_type);
                    assert(func_type->kind == AST_TYPE_FUNCTION);

                    uint64_t arg_count = 0;

                    for (uint64_t i = 0; i < BUF_LENGTH(expression->call.arg_expressions); i++)
                    {
                        arg_count++;
                        bool is_vararg = false;

                        AST_Expression* arg_expr = expression->call.arg_expressions[i];

                        IR_Value* arg_value = nullptr;

                        if (i >= BUF_LENGTH(func_type->function.arg_types))
                        {
                            assert(func_type->flags & AST_TYPE_FLAG_FUNC_VARARG);
                            is_vararg = true;
                        }

                        if (i < BUF_LENGTH(func_type->function.arg_types) &&
                            func_type->function.arg_types[i]->kind == AST_TYPE_STATIC_ARRAY)
                        {
                            arg_value = ir_builder_emit_lvalue(ir_builder, arg_expr);
                        }
                        else
                        {
                            arg_value = ir_builder_emit_expression(ir_builder, arg_expr);
                        }

                        assert(arg_value);

                        ir_builder_emit_call_arg(ir_builder, arg_value, arg_expr->file_pos,
                                                is_vararg);

                        if (arg_expr->flags & AST_EXPR_FLAG_FIRST_VARARG)
                        {
                            break;
                        }
                    }

                    IR_Value* num_args_lit = ir_integer_literal(ir_builder, Builtin::type_s64,
                                                                arg_count);
                    return ir_builder_emit_call(ir_builder, callee_value, num_args_lit,
                                                expression->file_pos);
                }
                else if (callee_decl->kind == AST_DECL_MUTABLE)
                {
                    assert(callee_decl->mutable_decl.type->kind == AST_TYPE_POINTER);
                    assert(callee_decl->mutable_decl.type->pointer.base->kind ==
                        AST_TYPE_FUNCTION);

                    IR_Value* callee_value = ir_builder_value_for_declaration(ir_builder,
                                                                            callee_decl);
                    IR_Value* func_ptr_value = ir_builder_emit_load(ir_builder, callee_value,
                                                                    expression->file_pos);

                    AST_Type* func_type = callee_decl->mutable_decl.type->pointer.base;

                    for (uint64_t i = 0; i < BUF_LENGTH(expression->call.arg_expressions); i++)
                    {
                        bool is_vararg = false;
                        if (i >= BUF_LENGTH(func_type->function.arg_types))
                        {
                            assert(func_type->flags & AST_TYPE_FLAG_FUNC_VARARG);
                            is_vararg = true;
                        }
                        AST_Expression* arg_expr = expression->call.arg_expressions[i];
                        IR_Value* arg_value = ir_builder_emit_expression(ir_builder, arg_expr);
                        ir_builder_emit_call_arg(ir_builder, arg_value, arg_expr->file_pos,
                                                is_vararg);
                    }
                    uint64_t num_args = BUF_LENGTH(expression->call.arg_expressions);
                    IR_Value* num_args_lit = ir_integer_literal(ir_builder, Builtin::type_s64,
                        num_args);
                    return ir_builder_emit_call(ir_builder, func_ptr_value, num_args_lit,
                                                expression->file_pos);
                }
                else assert(false);
                break;
            }

            case AST_EXPR_SUBSCRIPT:
            {
                if (expression->subscript.call_expression)
                {
                    return ir_builder_emit_expression(ir_builder,
                                                    expression->subscript.call_expression);
                }
                else
                {
                    AST_Expression* base_expr = expression->subscript.base_expression;
                    AST_Expression* index_expr = expression->subscript.index_expression;
                    IR_Value* index_value = ir_builder_emit_expression(ir_builder,
                                                                    index_expr);
                    IR_Value* base_value = ir_builder_emit_lvalue(ir_builder,
                                                                    base_expr);
                    if (!(base_value->type->kind == AST_TYPE_STATIC_ARRAY) &&
                            !(base_value->type->kind == AST_TYPE_POINTER &&
                            base_value->type->pointer.base->kind == AST_TYPE_STATIC_ARRAY))
                    {
                        base_value = ir_builder_emit_load(ir_builder, base_value, expression->file_pos);
                    }
                    return ir_builder_emit_subscript(ir_builder, base_value, index_value,
                                                    expression->file_pos);
                }
                break;
            }

            case AST_EXPR_COMPOUND_LITERAL:
            {
                if (expression->type->kind == AST_TYPE_STATIC_ARRAY)
                {
                    // AST_Type* array_type = expression->type;
                    // IR_Value* result_value = ir_builder_emit_allocl(ir_builder, array_type,
                    //                                                 "array_compound_lit",
                    //                                                 expression->file_pos);

                    // auto compound_exprs = expression->compound_literal.expressions;
                    // for (uint64_t i = 0; i < BUF_LENGTH(compound_exprs); i++)
                    // {
                    //     AST_Expression* element_expression = compound_exprs[i];
                    //     IR_Value* element_value = ir_builder_emit_expression(ir_builder,
                    //                                                         element_expression);

                    //     IR_Value* pointer_value =
                    //         ir_builder_emit_array_offset_pointer(ir_builder, result_value, i,
                    //                                             element_expression->file_pos);

                    //     ir_builder_emit_storep(ir_builder, pointer_value, element_value,
                    //                         element_expression->file_pos);
                    // }

                    // return result_value;

                    AST_Type* array_type = expression->type;

                    BUF(IR_Value*) compound_values = nullptr;
                    auto compound_exprs = expression->compound_literal.expressions;

                    bool all_const = true;

                    for (uint64_t i = 0; i < BUF_LENGTH(compound_exprs); i++)
                    {
                        AST_Expression* member_expression = compound_exprs[i];
                        IR_Value* member_value = ir_builder_emit_expression(ir_builder,
                                                                            member_expression);

                        if (!(member_value->flags & IRV_FLAG_CONST))
                        {
                            all_const = false;
                        }

                        BUF_PUSH(compound_values, member_value);
                    }

                    return ir_array_literal(ir_builder, array_type, compound_values, all_const);
                }
                else if (expression->type->kind == AST_TYPE_STRUCT)
                {
                    AST_Type* struct_type = expression->type;

                    BUF(IR_Value*) compound_values = nullptr;
                    auto compound_exprs = expression->compound_literal.expressions;

                    bool all_const = true;

                    for (uint64_t i = 0; i < BUF_LENGTH(compound_exprs); i++)
                    {
                        AST_Expression* member_expression = compound_exprs[i];
                        IR_Value* member_value = ir_builder_emit_expression(ir_builder,
                                                                            member_expression);

                        if (!(member_value->flags & IRV_FLAG_CONST))
                        {
                            all_const = false;
                        }

                        BUF_PUSH(compound_values, member_value);
                    }

                    return ir_aggregate_literal(ir_builder, struct_type, compound_values,
                                                all_const);
                }
                else assert(false);

                break;
            }

            case AST_EXPR_ARRAY_LENGTH:
            {
                AST_Type* array_type = expression->array_length.ident_expr->type;
                assert(array_type->kind == AST_TYPE_STATIC_ARRAY);
                IR_Value* count_literal = ir_integer_literal(ir_builder, Builtin::type_s64,
                                                            array_type->static_array.count);
                return count_literal;
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

            case AST_EXPR_SIZEOF:
            {
                return ir_integer_literal(ir_builder, expression->type,
                                        expression->sizeof_expr.byte_size);
                break;
            }

            case AST_EXPR_POST_INCREMENT:
            {
                IR_Value* result_value = ir_builder_emit_expression(ir_builder,
                                                                    expression->base_expression);

                IR_Value* lvalue = ir_builder_emit_lvalue(ir_builder,
                                                        expression->base_expression);

                IR_Value* new_value = nullptr;
                if (expression->flags & AST_EXPR_FLAG_POINTER_MATH)
                {
                    IR_Value* one_value = ir_integer_literal(ir_builder, Builtin::type_u64, 1);
                    new_value = ir_builder_emit_pointer_math(ir_builder, lvalue, one_value,
                                                            AST_BINOP_ADD, false,
                                                            expression->file_pos);
                }
                else
                {
                    IR_Value* one_value =
                        ir_integer_literal(ir_builder, expression->base_expression->type, 1);
                    new_value = ir_builder_emit_add(ir_builder, result_value, one_value,
                                                    expression->file_pos);
                }
                assert(new_value);

                ir_builder_emit_store(ir_builder, lvalue, new_value, expression->file_pos);
                return result_value;
                break;
            }

            case AST_EXPR_POST_DECREMENT:
            {
                IR_Value* result_value = ir_builder_emit_expression(ir_builder,
                                                                    expression->base_expression);

                IR_Value* lvalue = ir_builder_emit_lvalue(ir_builder,
                                                        expression->base_expression);
                IR_Value* new_value = nullptr;
                if (expression->flags & AST_EXPR_FLAG_POINTER_MATH)
                {
                    IR_Value* one_value = ir_integer_literal(ir_builder, Builtin::type_u64, 1);
                    new_value = ir_builder_emit_pointer_math(ir_builder, lvalue, one_value,
                                                            AST_BINOP_SUB, false,
                                                            expression->file_pos);
                }
                else
                {
                    IR_Value* one_value =
                        ir_integer_literal(ir_builder, expression->base_expression->type, 1);
                    new_value = ir_builder_emit_sub(ir_builder, result_value, one_value,
                                                    expression->file_pos);
                }
                assert(new_value);

                ir_builder_emit_store(ir_builder, lvalue, new_value, expression->file_pos);
                return result_value;
                break;
            }

            case AST_EXPR_GET_TYPE_INFO:
            {
                assert(expression->get_type_info_expr.type);
                AST_Type* type = expression->get_type_info_expr.type;
                assert(type->info_index);

                return ir_builder_emit_get_type_info(ir_builder, type->info_index,
                                                    expression->file_pos);
                break;
            }

            case AST_EXPR_MAKE_LVALUE:
            {
                assert(false);
                IR_Value* allocl = ir_builder_emit_allocl(ir_builder, expression->type, "",
                                                          expression->file_pos);
                IR_Value* init_val =
                    ir_builder_emit_expression(ir_builder, expression->make_lvalue.expression);
                ir_builder_emit_store(ir_builder, allocl, init_val, expression->file_pos);
                return allocl;
                break;
            }

            default: assert(false);
        }

        return nullptr;
    }

    IR_Value* ir_builder_emit_pointer_math(IR_Builder* ir_builder, IR_Value* pointer_value,
                                            IR_Value* int_value, AST_Binop_Kind binop,
                                            bool reversed, File_Pos origin)
    {
        assert(pointer_value->type->kind == AST_TYPE_POINTER);
        assert(int_value->type->flags & AST_TYPE_FLAG_INT);

        AST_Type* pointer_type = pointer_value->type;

        pointer_value = ir_builder_emit_cast(ir_builder, pointer_value, int_value->type, origin);

        if (binop == AST_BINOP_ADD || binop == AST_BINOP_SUB)
        {
            uint64_t byte_size = pointer_type->pointer.base->bit_size / 8;
            IR_Value* size_mult = ir_integer_literal(ir_builder, int_value->type, byte_size);
            int_value = ir_builder_emit_mul(ir_builder, int_value, size_mult, origin);
        }

        IR_Value* lhs = pointer_value;
        IR_Value* rhs = int_value;
        if (reversed)
        {
            lhs = int_value;
            rhs = pointer_value;
        }

        IR_Value* result = nullptr;

        switch (binop)
        {
            case AST_BINOP_ADD:
            {
                result = ir_builder_emit_add(ir_builder, lhs, rhs, origin);
                break;
            }

            case AST_BINOP_SUB:
            {
                result = ir_builder_emit_sub(ir_builder, lhs, rhs, origin);
                break;
            }

            case AST_BINOP_NEQ:
            {
                result = ir_builder_emit_neq(ir_builder, lhs, rhs, origin);
                break;
            }

            case AST_BINOP_EQ:
            {
                result = ir_builder_emit_eq(ir_builder, lhs, rhs, origin);
                break;
            }

            default: assert(false);
        }

        assert(result);


        result = ir_builder_emit_cast(ir_builder, result, pointer_type, origin);

        return result;
    }

    IR_Value* ir_builder_emit_dot_expression(IR_Builder* ir_builder, AST_Expression* expression)
    {
        assert(ir_builder);
        assert(expression);
        assert(expression->kind == AST_EXPR_DOT);

        AST_Expression* base_expression = expression->dot.base_expression;
        AST_Expression* member_expression = expression->dot.member_expression;
        assert(member_expression->kind == AST_EXPR_IDENTIFIER);

        if (!base_expression->type)
        {
            AST_Declaration* base_decl = nullptr;
            if (base_expression->kind == AST_EXPR_IDENTIFIER)
            {
                assert(base_expression->identifier->declaration);
                base_decl = base_expression->identifier->declaration;
            }
            else if (base_expression->kind == AST_EXPR_DOT)
            {
                assert(base_expression->dot.declaration);
                base_decl = base_expression->dot.declaration;
            }
            else assert(false);

            if (base_decl->kind == AST_DECL_IMPORT)
            {
                return ir_builder_emit_expression(ir_builder, member_expression);
            }
            else if (base_decl->kind == AST_DECL_AGGREGATE_TYPE &&
                    base_decl->aggregate_type.kind == AST_AGG_DECL_ENUM)
            {
                AST_Declaration* member = nullptr;
                auto agg_decls = base_decl->aggregate_type.aggregate_decl->members;
                for (uint64_t i = 0; i < BUF_LENGTH(agg_decls); i++)
                {
                    AST_Declaration* member_decl = agg_decls[i];
                    if (member_decl->identifier->atom == member_expression->identifier->atom)
                    {
                        member = member_decl;
                        break;
                    }
                }

                assert(member);
                assert(member->kind == AST_DECL_CONSTANT_VAR);
                assert(member->constant_var.init_expression);
                IR_Value* value = ir_builder_emit_expression(ir_builder,
                                                member->constant_var.init_expression);
                return value;
                // return ir_builder_emit_cast(ir_builder, value, base_decl->aggregate_type.type,
                //                             expression->file_pos);
            }
            else assert(false);

        }
        else
        {
            AST_Type* aggregate_type = nullptr;
            IR_Value* base_value = nullptr;
            AST_Declaration* base_decl = nullptr;

            if (base_expression->kind == AST_EXPR_IDENTIFIER)
            {
                base_decl = base_expression->identifier->declaration;
                base_value = ir_builder_value_for_declaration(ir_builder, base_decl);

                if (base_expression->type->kind == AST_TYPE_ENUM)
                {
                    assert(base_value == nullptr);
                    return ir_builder_emit_expression(ir_builder, member_expression);
                }

                if (base_expression->type->kind == AST_TYPE_STRUCT ||
                    base_expression->type->kind == AST_TYPE_UNION)
                {
                    aggregate_type = base_expression->type;
                }
                else if (base_expression->type->kind == AST_TYPE_POINTER)
                {
                    aggregate_type = base_expression->type->pointer.base;
                    base_value = ir_builder_emit_load(ir_builder, base_value,
                                                    expression->file_pos);
                }
                else assert(false);
            }
            else if (base_expression->kind == AST_EXPR_DOT)
            {
                if (base_expression->dot.declaration->kind == AST_DECL_AGGREGATE_TYPE &&
                    base_expression->dot.declaration->aggregate_type.kind == AST_AGG_DECL_ENUM)
                {
                    assert(member_expression->identifier->declaration);
                    return ir_builder_value_for_declaration(ir_builder, member_expression->identifier->declaration);
                }
                base_value = ir_builder_emit_lvalue(ir_builder, base_expression);
                //base_value = ir_builder_emit_dot_expression(ir_builder, base_expression);
                bool need_load = true;
                if (base_value->kind == IRV_ALLOCL &&
                        (base_value->type->kind == AST_TYPE_STRUCT ||
                        base_value->type->kind == AST_TYPE_UNION))
                {
                    need_load = false;
                }
                if (base_value->type->kind == AST_TYPE_POINTER &&
                        (base_value->type->pointer.base->kind == AST_TYPE_STRUCT ||
                        base_value->type->pointer.base->kind == AST_TYPE_UNION))
                {
                    need_load = false;
                }
                if (need_load)
                {
                    base_value = ir_builder_emit_load(ir_builder, base_value, expression->file_pos);
                }
                aggregate_type = base_value->type;
                base_decl = base_expression->dot.declaration;
            }

            while (aggregate_type->kind == AST_TYPE_POINTER)
            {
                aggregate_type = aggregate_type->pointer.base;
            }

            assert(aggregate_type->kind == AST_TYPE_STRUCT ||
                aggregate_type->kind == AST_TYPE_UNION);

            // while (base_value->type->kind == AST_TYPE_POINTER)
            // {
            //     base_value = ir_builder_emit_load(ir_builder, base_value, expression->file_pos);
            // }

            uint64_t member_index = 0;

            auto member_decls = aggregate_type->aggregate_type.member_declarations;
            for (uint64_t i = 0; i < BUF_LENGTH(member_decls); i++)
            {
                AST_Declaration* member_decl = member_decls[i];
                if (!member_decl->identifier)
                {
                    AST_Type* anon_type = member_decl->mutable_decl.type;
                    assert(anon_type->kind == AST_TYPE_STRUCT ||
                        anon_type->kind == AST_TYPE_UNION);
                    auto anon_members = anon_type->aggregate_type.member_declarations;
                    for (uint64_t j = 0; j < BUF_LENGTH(anon_members); j++)
                    {
                        AST_Declaration* anon_member = anon_members[j];
                        if (anon_member->identifier->atom == member_expression->identifier->atom)
                        {
                            base_value =
                                ir_builder_emit_aggregate_offset_pointer(ir_builder, base_value,
                                                                        i,
                                                                        member_expression->file_pos);
                            member_index = j;
                            break;
                        }
                    }
                }
                else if (member_decl->identifier->atom == member_expression->identifier->atom)
                {
                    member_index = i;
                    break;
                }
            }

            assert(base_decl);
            assert(base_value);

            // while (base_value->type->kind != AST_TYPE_STRUCT &&
            //        base_value->type->kind != AST_TYPE_UNION)
            // {
            //     base_value = ir_builder_emit_load(ir_builder, base_value,
            //                                       member_expression->file_pos);
            // }
            IR_Value* value_pointer = ir_builder_emit_aggregate_offset_pointer(ir_builder,
                                                                                base_value,
                                                                            member_index,
                member_expression->file_pos);
            return ir_builder_emit_loadp(ir_builder, value_pointer, expression->file_pos);
        }

        assert(false);
        return nullptr;
    }

    IR_Value* ir_builder_emit_cast_expression(IR_Builder* ir_builder, AST_Expression* expression)
    {
        assert(ir_builder);
        assert(expression);
        assert(expression->kind == AST_EXPR_CAST);

        AST_Type* source_type = expression->cast_expr.expr->type;
        AST_Type* dest_type = expression->type;

        if (source_type == dest_type)
        {
            return ir_builder_emit_expression(ir_builder, expression->cast_expr.expr);
        }

        AST_Type* dest_ptr_type = ast_find_or_create_pointer_type(ir_builder->context, dest_type);

        bool source_struct = source_type->kind == AST_TYPE_STRUCT;
        bool source_union = source_type->kind == AST_TYPE_UNION;
        bool dest_int = dest_type->flags & AST_TYPE_FLAG_INT;
        bool dest_float = dest_type->flags & AST_TYPE_FLAG_FLOAT;

        if ((dest_int || dest_float) && (source_struct || source_union))
        {
            IR_Value* expr_value_ptr = ir_builder_emit_lvalue(ir_builder,
                                                            expression->cast_expr.expr);
            expr_value_ptr = ir_builder_emit_cast(ir_builder, expr_value_ptr, dest_ptr_type,
                                                expression->file_pos);
            return ir_builder_emit_load(ir_builder, expr_value_ptr, expression->file_pos);
        }
        else
        {
            IR_Value* expr_value = ir_builder_emit_expression(ir_builder,
                                                            expression->cast_expr.expr);
            return ir_builder_emit_cast(ir_builder, expr_value, dest_type, expression->file_pos);
        }
    }

    IR_Value* ir_builder_emit_negate(IR_Builder* ir_builder, AST_Expression* expression,
                                    File_Pos origin)
    {
        assert(ir_builder);
        assert(expression);

        IR_Value* zero_val = ir_builder_emit_zero_literal(ir_builder, expression->type);
        IR_Value* expression_value = ir_builder_emit_expression(ir_builder, expression);
        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, expression->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_SUB, zero_val,
                                                expression_value, result_value);

        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_addrof(IR_Builder* ir_builder, AST_Expression* expression,
                                    File_Pos origin)
    {
        assert(ir_builder);
        assert(expression);

        if (expression->kind == AST_EXPR_IDENTIFIER)
        {
            auto decl = expression->identifier->declaration;
            IR_Value* expression_value = ir_builder_value_for_declaration(ir_builder,
                                                                        decl);
            assert(expression_value);

            if (expression_value->kind != IRV_FUNCTION)
            {
                return ir_builder_emit_lvalue(ir_builder, expression, true);
            }
        }

        auto res = ir_builder_emit_lvalue(ir_builder, expression);
        assert(res->type->kind == AST_TYPE_POINTER);
        return res;

        assert(false);
        return nullptr;
    }

    IR_Value* ir_builder_emit_addrof_foreign(IR_Builder* ir_builder, IR_Value* foreign_func,
                                            AST_Type* foreign_type, File_Pos origin)
    {
        assert(ir_builder);
        assert(foreign_func);
        assert(foreign_func->kind == IRV_FUNCTION);
        assert(foreign_func->function->flags & IR_FUNC_FLAG_FOREIGN);
        assert(foreign_type);

        AST_Type* pointer_type = ast_find_or_create_pointer_type(ir_builder->context,
                                                                foreign_type);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, pointer_type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_ADDROF_FOREIGN,
                                                foreign_func, nullptr, result_value);

        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_addrof_function(IR_Builder* ir_builder, IR_Value* func,
                                            AST_Type* func_type, File_Pos origin)
    {
        assert(ir_builder);
        assert(func);
        assert(func->kind == IRV_FUNCTION);
        assert(func_type);
        assert(func_type->kind == AST_TYPE_FUNCTION);

        AST_Type* pointer_type = ast_find_or_create_pointer_type(ir_builder->context, func_type);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, pointer_type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_ADDROF_FUNCTION, func,
                                                nullptr, result_value);

        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_deref(IR_Builder* ir_builder, AST_Expression* expression,
                                    File_Pos origin)
    {
        assert(ir_builder);
        assert(expression);

        IR_Value* expression_value = nullptr;

        switch (expression->kind)
        {
            case AST_EXPR_IDENTIFIER:
            {
                AST_Declaration* ident_decl = expression->identifier->declaration;
                expression_value = ir_builder_value_for_declaration(ir_builder, ident_decl);
                break;
            }

            case AST_EXPR_CALL:
            case AST_EXPR_CAST:
            {
                expression_value = ir_builder_emit_expression(ir_builder, expression);
                break;
            }

            case AST_EXPR_UNARY:
            {
                auto op = expression->unary.op;
                if (op == AST_UNOP_ADDROF)
                {
                    expression_value = ir_builder_emit_expression(ir_builder, expression);
                }
                else assert(false);

                break;
            }

            default: assert(false);
        }


        assert(expression_value);

        AST_Type* operand_type = expression->type;
        assert(operand_type->kind == AST_TYPE_POINTER);

        AST_Type* result_type = operand_type->pointer.base;

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, result_type);

        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_DEREF,
                                                 expression_value, nullptr, result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_not(IR_Builder* ir_builder, AST_Expression* expression,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(expression);
        assert(expression->type == Builtin::type_bool ||
            (expression->type->flags & AST_TYPE_FLAG_INT) ||
            expression->type->kind == AST_TYPE_POINTER);

        IR_Value* operand_val = ir_builder_emit_expression(ir_builder, expression);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_NOT, operand_val,
                                                nullptr, result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_create_thread(IR_Builder* ir_builder, IR_Value* func_value,
                                            IR_Value* user_data_value, File_Pos origin)
    {
        assert(ir_builder);
        assert(func_value);
        assert(user_data_value);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_Thread);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_CREATE_THREAD,
                                                func_value, user_data_value, result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_join_thread(IR_Builder* ir_builder, IR_Value* thread_value,
                                    File_Pos origin)
    {
        assert(ir_builder);
        assert(thread_value);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_pointer_to_void);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_JOIN_THREAD,
                                                thread_value, nullptr, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_compare_and_swap(IR_Builder* ir_builder, IR_Value* pointer_val,
                                            IR_Value* value, IR_Value* new_value,
                                            File_Pos origin)
    {
        assert(ir_builder);
        assert(pointer_val);
        assert(value);
        assert(new_value);

        ir_builder_emit_call_arg(ir_builder, pointer_val, origin);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_COMPARE_AND_SWAP,
                                                value, new_value, result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    void ir_builder_push_value_and_decl(IR_Builder* ir_builder, IR_Value* ir_value,
                                        AST_Declaration* decl)
    {
        assert(ir_builder);
        assert(ir_value);
        assert(decl);

        uint64_t hash = hash_pointer(decl);
        uint64_t hash_index = hash & (ir_builder->value_decl_count - 1);

        uint64_t iterations = 0;
        while (iterations < ir_builder->value_decl_count)
        {
            IR_Value_And_Decl* entry = &ir_builder->value_decl_hash[hash_index];

            if (entry->decl)
            {
                if (decl->flags & AST_DECL_FLAG_BUILTIN)
                {
                    return;
                }
                assert(entry->decl != decl);
            }
            else
            {
                IR_Value_And_Decl new_entry;
                new_entry.decl = decl;
                new_entry.value = ir_value;

                ir_builder->value_decl_hash[hash_index] = new_entry;
                return;
            }

            iterations++;
            hash_index++;
            if (ir_builder->value_decl_count <= hash_index)
            {
                hash_index = 0;
            }
        }

        ir_builder_grow_value_decl_hash(ir_builder);
        ir_builder_push_value_and_decl(ir_builder, ir_value, decl);
    }

    void ir_builder_grow_value_decl_hash(IR_Builder* ir_builder)
    {
        assert(ir_builder);
        assert(ir_builder->value_decl_hash);
        assert(ir_builder->value_decl_count);


        uint64_t old_count = ir_builder->value_decl_count;
        IR_Value_And_Decl* old_data = ir_builder->value_decl_hash;
        uint64_t new_count = old_count * 2;
        IR_Value_And_Decl* new_data = (IR_Value_And_Decl*)mem_alloc(sizeof(IR_Value_And_Decl) * new_count);

        ir_builder->value_decl_hash = new_data;
        ir_builder->value_decl_count = new_count;

        for (uint64_t i = 0; i < old_count; i++)
        {
            const IR_Value_And_Decl& old_entry = old_data[i];
            if (old_entry.decl)
            {
                ir_builder_push_value_and_decl(ir_builder, old_entry.value, old_entry.decl);
            }
        }

        mem_free(old_data);
    }

    IR_Value* ir_builder_value_for_declaration(IR_Builder* ir_builder,
                                            AST_Declaration* declaration)
    {
        assert(ir_builder);
        assert(declaration);

        IR_Value* result = nullptr;

        uint64_t hash = hash_pointer(declaration);
        uint64_t hash_index = hash & (ir_builder->value_decl_count - 1);

        uint64_t iterations = 0;
        while (iterations < ir_builder->value_decl_count)
        {
            IR_Value_And_Decl* entry = &ir_builder->value_decl_hash[hash_index];

            if (entry->decl)
            {
                if (entry->decl == declaration)
                {
                    result = entry->value;
                    break;
                }
            }
            else
            {
                break;
            }

            iterations++;
            hash_index++;
            if (ir_builder->value_decl_count <= hash_index)
            {
                hash_index = 0;
            }
        }

        if (!result)
        {
            for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->ast_module->import_modules); i++)
            {
                AST_Module* import_module = ir_builder->ast_module->import_modules[i];
                assert(import_module->gen_data);

                IR_Builder* import_ir_builder = (IR_Builder*)import_module->gen_data;

                result = ir_builder_value_for_declaration(import_ir_builder, declaration);
                if (result)
                {
                    break;
                }
            }
        }

        if (result && declaration->kind == AST_DECL_CONSTANT_VAR && declaration->constant_var.type != result->type &&
            (declaration->constant_var.type->flags & AST_TYPE_FLAG_INT))
        {
            result->type = declaration->constant_var.type;
        }

        return result;
    }

    IR_Value* ir_builder_begin_function(IR_Builder* ir_builder, File_Pos file_pos,
                                        const char* name, AST_Type* func_type,
                                        AST_Scope* body_scope)
    {
        assert(ir_builder);
        assert(name);
        assert(func_type);

        assert(ir_builder->current_function == nullptr);

        //TODO: Assert we don't have a function with the same name
        IR_Function* function = ir_function_new(ir_builder, file_pos, name, func_type,
                                                body_scope);
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
                                                uint64_t offset, File_Pos origin)
    {
        assert(ir_builder);
        assert(array_allocl);
        assert(array_allocl->type->kind == AST_TYPE_STATIC_ARRAY);
        assert(offset < array_allocl->type->static_array.count);

        IR_Value* offset_value_literal = ir_integer_literal(ir_builder, Builtin::type_s64,
                                                            offset);
        AST_Type* result_type =
            ast_find_or_create_pointer_type(ir_builder->context,
                                            array_allocl->type->static_array.base);
        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, result_type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_ARRAY_OFFSET_POINTER,
                                                array_allocl,
                                                        offset_value_literal, result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_array_offset_pointer(IR_Builder* ir_builder, IR_Value* array_allocl,
                                                IR_Value* offset_value, File_Pos origin)
    {
        assert(ir_builder);
        assert(array_allocl);
        assert(array_allocl->type->kind == AST_TYPE_POINTER ||
               array_allocl->type->kind == AST_TYPE_STATIC_ARRAY);
        assert(offset_value);
        assert(offset_value->kind == IRV_TEMPORARY || IRV_INT_LITERAL);
        assert(offset_value->type == Builtin::type_s64 ||
            offset_value->type == Builtin::type_u64);

        AST_Type* result_type = nullptr;
        if (array_allocl->type->kind == AST_TYPE_STATIC_ARRAY)
        {
            result_type = ast_find_or_create_pointer_type(ir_builder->context,
                                                        array_allocl->type->static_array.base);
        }
        else if (array_allocl->type->kind == AST_TYPE_POINTER &&
                array_allocl->type->pointer.base->kind == AST_TYPE_STATIC_ARRAY)
        {
            result_type = ast_find_or_create_pointer_type(ir_builder->context,
                                                        array_allocl->type->pointer.base->static_array.base);
        }
        else
        {
            result_type = array_allocl->type;
        }

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, result_type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_ARRAY_OFFSET_POINTER,
                                                array_allocl,
                                                offset_value, result_value);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_value;
    }

    IR_Value* ir_builder_emit_aggregate_offset_pointer(IR_Builder* ir_builder,
                                                    IR_Value* struct_value,
                                                    uint64_t offset, File_Pos origin)
    {
        assert(ir_builder);
        assert(struct_value);
        assert(struct_value->kind == IRV_ALLOCL ||
            struct_value->kind == IRV_ARGUMENT ||
            struct_value->kind == IRV_GLOBAL ||
               struct_value->kind == IRV_TEMPORARY ||
            struct_value->type->kind == AST_TYPE_POINTER);
        assert(struct_value->type->kind == AST_TYPE_STRUCT ||
            struct_value->type->kind == AST_TYPE_UNION ||
            (struct_value->type->kind == AST_TYPE_POINTER &&
                (struct_value->type->pointer.base->kind == AST_TYPE_STRUCT ||
                struct_value->type->pointer.base->kind == AST_TYPE_UNION)));

        if ((struct_value->kind == IRV_ARGUMENT || struct_value->kind == IRV_ALLOCL)
            && struct_value->type->kind == AST_TYPE_POINTER)
        {
            struct_value = ir_builder_emit_load(ir_builder, struct_value, origin);
        }

        AST_Type* struct_type = struct_value->type;
        if (struct_value->type->kind == AST_TYPE_POINTER)
        {
            struct_type = struct_type->pointer.base;
        }

        assert(BUF_LENGTH(struct_type->aggregate_type.member_declarations) > offset);
        AST_Declaration* member_decl = struct_type->aggregate_type.member_declarations[offset];
        assert(member_decl->kind == AST_DECL_MUTABLE);
        assert(member_decl->location == AST_DECL_LOC_AGGREGATE_MEMBER);

        IR_Value* offset_value_literal = nullptr;
        if (struct_type->kind == AST_TYPE_STRUCT)
        {
            offset_value_literal = ir_integer_literal(ir_builder, Builtin::type_s64, offset);
        }
        else if (struct_type->kind == AST_TYPE_UNION)
        {
            offset_value_literal = ir_integer_literal(ir_builder, Builtin::type_s64, 0);
        }

        AST_Type* result_type = ast_find_or_create_pointer_type(ir_builder->context,
                                                                member_decl->mutable_decl.type);
        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, result_type);
        assert(struct_value);
        assert(offset_value_literal);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin,
                                                IR_OP_AGGREGATE_OFFSET_POINTER, struct_value,
                                                offset_value_literal, result_value);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_value;
    }

    IR_Value* ir_builder_emit_extract_value(IR_Builder* ir_builder, IR_Value* aggregate_val,
                                            uint64_t member_idx, File_Pos origin)
    {
        assert(aggregate_val->kind == IRV_TEMPORARY);
        assert(aggregate_val->type->kind == AST_TYPE_STRUCT ||
               aggregate_val->type->kind == AST_TYPE_UNION);

        AST_Type* agg_type = aggregate_val->type;
        assert(agg_type->kind == AST_TYPE_STRUCT ||
               agg_type->kind == AST_TYPE_UNION);

        assert(member_idx < BUF_LENGTH(agg_type->aggregate_type.member_declarations));

        AST_Declaration* member_decl = agg_type->aggregate_type.member_declarations[member_idx];
        assert(member_decl->kind == AST_DECL_MUTABLE);
        AST_Type* member_type = member_decl->mutable_decl.type;

        IR_Value* index_val = ir_integer_literal(ir_builder, Builtin::type_u64, member_idx);
        IR_Value* result_val = ir_value_new(ir_builder, IRV_TEMPORARY, member_type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_EXTRACT_VALUE,
                                                 aggregate_val, index_val, result_val);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_val;
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

    IR_Value* ir_builder_emit_function_arg(IR_Builder* ir_builder, const char* name,
                                        AST_Type* type, File_Pos file_pos)
    {
        assert(ir_builder);
        assert(name);
        assert(type);

        assert(ir_builder->current_function);

        IR_Value* arg_value = ir_value_new(ir_builder, IRV_ARGUMENT, type);
        arg_value->argument.name = name;
        arg_value->argument.index = BUF_LENGTH(ir_builder->current_function->local_temps);
        arg_value->flags |= IRV_FLAG_ASSIGNED;
        arg_value->argument.file_pos = file_pos;

        BUF_PUSH(ir_builder->current_function->local_temps, arg_value);
        BUF_PUSH(ir_builder->current_function->arguments, arg_value);
        return arg_value;
    }

    IR_Value* ir_builder_emit_add(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, lhs->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_ADD, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_sub(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, lhs->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_SUB, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_mul(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, lhs->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_MUL, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_mod(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, lhs->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_MOD, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_bitshift(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                    AST_Binop_Kind op, File_Pos origin)
    {
        assert(op == AST_BINOP_LSHIFT || op == AST_BINOP_RSHIFT);
        assert(lhs->type == rhs->type);

        auto ir_op = (op == AST_BINOP_LSHIFT) ? IR_OP_LSHIFT : IR_OP_RSHIFT;

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, lhs->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, ir_op, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);
        return result;
    }

    IR_Value* ir_builder_emit_div(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, lhs->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_DIV, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_lt(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_LT, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_lteq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_LTEQ, lhs, rhs,
                                                result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_gt(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_GT, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_gteq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_GTEQ, lhs, rhs, result);

        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_eq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        if (lhs->type == rhs->type ||
            (lhs->type->kind == AST_TYPE_ENUM &&
            (lhs->type->aggregate_type.base_type == rhs->type)) ||
            (lhs->type->kind == AST_TYPE_POINTER && rhs->type == Builtin::type_pointer_to_void) ||
            (rhs->type->kind == AST_TYPE_POINTER && lhs->type == Builtin::type_pointer_to_void))
        {
            IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
            IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_EQ, lhs, rhs,
                                                    result);

            ir_builder_emit_instruction(ir_builder, iri);

            return result;
        }
        else assert(false);

        return nullptr;
    }

    IR_Value* ir_builder_emit_neq(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        if (lhs->type == rhs->type ||
            (lhs->type->kind == AST_TYPE_ENUM &&
            (lhs->type->aggregate_type.base_type == rhs->type)))
        {
            IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, Builtin::type_bool);
            IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_NEQ, lhs, rhs,
                                                    result);

            ir_builder_emit_instruction(ir_builder, iri);

            return result;
        }
        else assert(false);

        return nullptr;
    }

    IR_Value* ir_builder_emit_cond_expr(IR_Builder* ir_builder, AST_Expression* cond_expr,
                                        File_Pos origin)
    {
        switch (cond_expr->kind)
        {
            case AST_EXPR_BINARY:
            {
                auto cur_func = ir_builder->current_function;
                auto lhs_expr = cond_expr->binary.lhs;
                auto rhs_expr = cond_expr->binary.rhs;

                IR_Block* origin_block = ir_builder->insert_block;
                IR_Value* true_value = ir_boolean_literal(ir_builder, Builtin::type_bool,
                                                        true);
                IR_Value* false_value = ir_boolean_literal(ir_builder, Builtin::type_bool,
                                                        false);

                switch (cond_expr->binary.op)
                {
                    case AST_BINOP_AND_AND:
                    {
                        IR_Value* and_check_rhs_block_val =
                            ir_builder_create_block(ir_builder, "sc_and_check_rhs");
                        IR_Value* post_sc_and_block_val =
                            ir_builder_create_block(ir_builder, "sc_post_and");


                        ir_builder_set_insert_block(ir_builder, origin_block);
                        IR_Value* lhs_val = ir_builder_emit_expression(ir_builder, lhs_expr);
                        origin_block = ir_builder->insert_block;
                        ir_builder_emit_jmp_if(ir_builder, lhs_val, and_check_rhs_block_val,
                                            origin);
                        ir_builder_emit_jmp(ir_builder, post_sc_and_block_val, origin);

                        ir_builder_append_block(ir_builder, cur_func,
                                                and_check_rhs_block_val->block);

                        ir_builder_set_insert_block(ir_builder, and_check_rhs_block_val);
                        IR_Value* rhs_val = ir_builder_emit_expression(ir_builder, rhs_expr);
                        auto rhs_check_block = ir_builder->insert_block;
                        ir_builder_emit_jmp(ir_builder, post_sc_and_block_val, origin);

                        ir_builder_set_insert_block(ir_builder, post_sc_and_block_val);
                        IR_Value* result_value = ir_builder_emit_phi(ir_builder,
                                                                    Builtin::type_bool,
                                                                    origin);
                        phi_node_add_incoming(result_value, origin_block, lhs_val);
                        phi_node_add_incoming(result_value, rhs_check_block, rhs_val);

                        ir_builder_append_block(ir_builder, cur_func,
                                                post_sc_and_block_val->block);
                        return result_value;
                        break;
                    }

                    case AST_BINOP_OR_OR:
                    {
                        IR_Value* or_check_rhs_block_val =
                            ir_builder_create_block(ir_builder, "sc_or_check_rhs");
                        IR_Value* post_sc_or_block_val =
                            ir_builder_create_block(ir_builder, "sc_post_or");

                        ir_builder_set_insert_block(ir_builder, origin_block);
                        IR_Value* lhs_val = ir_builder_emit_expression(ir_builder, lhs_expr);
                        origin_block = ir_builder->insert_block;
                        ir_builder_emit_jmp_if(ir_builder, lhs_val, post_sc_or_block_val, origin);
                        ir_builder_emit_jmp(ir_builder, or_check_rhs_block_val, origin);

                        ir_builder_append_block(ir_builder, cur_func,
                                                or_check_rhs_block_val->block);

                        ir_builder_set_insert_block(ir_builder, or_check_rhs_block_val);
                        IR_Value* rhs_val = ir_builder_emit_expression(ir_builder, rhs_expr);
                        auto rhs_check_block = ir_builder->insert_block;
                        ir_builder_emit_jmp(ir_builder, post_sc_or_block_val, origin);

                        ir_builder_set_insert_block(ir_builder, post_sc_or_block_val);
                        IR_Value* result_value = ir_builder_emit_phi(ir_builder,
                                                                    Builtin::type_bool,
                                                                    origin);
                        phi_node_add_incoming(result_value, origin_block, lhs_val);
                        phi_node_add_incoming(result_value, rhs_check_block, rhs_val);

                        ir_builder_append_block(ir_builder, cur_func,
                                                post_sc_or_block_val->block);

                        return result_value;
                        break;
                    }

                    default: assert(false);
                }
                break;
            }

            default: assert(false);
        }

        assert(false);
        return nullptr;
    }

    IR_Value* ir_builder_emit_and(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type ||
            ((lhs->type->flags & AST_TYPE_FLAG_INT) &&
                rhs->type->kind == AST_TYPE_ENUM &&
                lhs->type == rhs->type->aggregate_type.base_type));

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, lhs->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_AND, lhs, rhs, result);
        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    IR_Value* ir_builder_emit_or(IR_Builder* ir_builder, IR_Value* lhs, IR_Value* rhs,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type ||
            ((lhs->type->flags & AST_TYPE_FLAG_INT) &&
                rhs->type->kind == AST_TYPE_ENUM &&
                lhs->type == rhs->type->aggregate_type.base_type));

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, lhs->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_OR, lhs, rhs, result);
        ir_builder_emit_instruction(ir_builder, iri);

        return result;
    }

    void ir_builder_emit_return(IR_Builder* ir_builder, IR_Value* ret_val, File_Pos origin)
    {
        assert(ir_builder);

        // TODO: Check return type agains current functions return type

        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_RETURN, ret_val,
                                                nullptr, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    void ir_builder_emit_defer_statements_before_return(IR_Builder* ir_builder, AST_Scope* scope,
                                                        File_Pos return_file_pos)
    {
        assert(ir_builder);
        assert(scope);

        while (!(scope->flags & AST_SCOPE_FLAG_IS_MODULE_SCOPE))
        {
            for (uint64_t i = 0; i < BUF_LENGTH(scope->defer_statements); i++)
            {
                uint64_t index = BUF_LENGTH(scope->defer_statements) - 1 - i;
                AST_Statement* defer_statement = scope->defer_statements[index];
                bool emit = true;
                if (defer_statement->file_pos.line > return_file_pos.line)
                {
                    emit = false;
                }
                else if (defer_statement->file_pos.line == return_file_pos.line)
                {
                    if (defer_statement->file_pos.line_relative_char_pos >
                        return_file_pos.line_relative_char_pos)
                    {
                        emit = false;
                    }
                }

                if (emit)
                {
                    ir_builder_emit_statement(ir_builder, defer_statement, scope, nullptr);
                }
            }
            scope = scope->parent;
        }

    }

    void ir_builder_emit_defer_statements_before_break(IR_Builder* ir_builder, AST_Scope* scope,
                                                    File_Pos break_file_pos)
    {
        assert(ir_builder);
        assert(scope);

        while (!(scope->flags & AST_SCOPE_FLAG_IS_MODULE_SCOPE) &&
               !(scope->flags & AST_SCOPE_FLAG_BREAK_SCOPE))
        {
            for (uint64_t i = 0; i < BUF_LENGTH(scope->defer_statements); i++)
            {
                uint64_t index = BUF_LENGTH(scope->defer_statements) - 1 - i;
                AST_Statement* defer_stmt = scope->defer_statements[index];

                bool emit = true;

                if (defer_stmt->file_pos.line > break_file_pos.line)
                {
                    emit = false;
                }
                else if (defer_stmt->file_pos.line == break_file_pos.line)
                {
                    if (defer_stmt->file_pos.line_relative_char_pos >
                        break_file_pos.line_relative_char_pos)
                    {
                        emit = false;
                    }
                }

                if (emit)
                {
                    ir_builder_emit_statement(ir_builder, defer_stmt, scope, nullptr);
                }
            }

            scope = scope->parent;
        }
    }

    void ir_builder_emit_if(IR_Builder* ir_builder, AST_Expression* cond_expr,
                            AST_Statement* then_stmt, AST_Statement* else_stmt, AST_Scope* scope,
                            IR_Value* break_block, File_Pos origin)
    {

        IR_Function* cur_func = ir_builder->current_function;

        IR_Value* then_block_val = ir_builder_create_block(ir_builder, "then");
        IR_Value* else_block_val = nullptr;
        IR_Value* post_if_block_val = ir_builder_create_block(ir_builder, "post_if");

        if (else_stmt)
        {
            else_block_val = ir_builder_create_block(ir_builder, "else");
        }
        else
        {
            else_block_val = post_if_block_val;
        }

        ir_builder_emit_if_cond(ir_builder, cond_expr, then_block_val, else_block_val, origin);

        ir_builder_append_block(ir_builder, cur_func, then_block_val->block);
        ir_builder_set_insert_block(ir_builder, then_block_val);
        ir_builder_emit_statement(ir_builder, then_stmt, scope, break_block);
        ir_builder_emit_jmp(ir_builder, post_if_block_val, origin);

        ir_builder_append_block(ir_builder, cur_func, else_block_val->block);
        if (else_stmt)
        {
            ir_builder_append_block(ir_builder, cur_func, post_if_block_val->block);

            ir_builder_set_insert_block(ir_builder, else_block_val);
            ir_builder_emit_statement(ir_builder, else_stmt, scope, break_block);
            ir_builder_emit_jmp(ir_builder, post_if_block_val, origin);
        }

        ir_builder_set_insert_block(ir_builder, post_if_block_val);

        // IR_Value* post_if_block_val = ir_builder_create_block(ir_builder, "post_if");
        // IR_Value* cond_value = ir_builder_emit_expression(ir_builder, cond_expr);
        // if (cond_value->type != Builtin::type_bool)
        // {
        //     assert(cond_value->type->kind == AST_TYPE_POINTER ||
        //             (cond_value->type->flags & AST_TYPE_FLAG_INT));
        // }

        // IR_Function* cur_func = ir_builder->current_function;
        // IR_Value* then_block_val = ir_builder_create_block(ir_builder, "then", cur_func);
        // IR_Value* else_block_val = nullptr;
        // IR_Value* post_if_block_val = ir_builder_create_block(ir_builder, "post_if");
        // if (else_stmt)
        // {
        //     else_block_val = ir_builder_create_block(ir_builder, "else");
        // }
        // else
        // {
        //     else_block_val = post_if_block_val;
        // }

        // ir_builder_emit_jmp_if(ir_builder, cond_value, then_block_val, origin);
        // ir_builder_emit_jmp(ir_builder, else_block_val, origin);

        // ir_builder_set_insert_block(ir_builder, then_block_val);
        // IR_Block* then_block = ir_builder->insert_block;
        // assert(then_stmt);
        // ir_builder_emit_statement(ir_builder, then_stmt, scope, break_block);
        // ir_builder_emit_jmp(ir_builder, post_if_block_val, origin);

        // if (else_stmt)
        // {
        //     ir_builder_append_block(ir_builder, cur_func, else_block_val->block);
        //     ir_builder_set_insert_block(ir_builder, else_block_val);
        //     ir_builder_emit_statement(ir_builder, else_stmt, scope, break_block);
        //     ir_builder_emit_jmp(ir_builder, post_if_block_val, origin);
        // }

        // ir_builder_append_block(ir_builder, cur_func, post_if_block_val->block);
        // ir_builder_set_insert_block(ir_builder, post_if_block_val);
    }

    void ir_builder_emit_if_cond(IR_Builder* ir_builder, AST_Expression* cond_expr,
                                IR_Value* then_block_val, IR_Value* else_block_val,
                                File_Pos origin)
    {
        IR_Function* cur_func = ir_builder->current_function;

        switch (cond_expr->kind)
        {
            case AST_EXPR_BINARY:
            {
                auto lhs_expr = cond_expr->binary.lhs;
                auto rhs_expr = cond_expr->binary.rhs;

                switch (cond_expr->binary.op)
                {
                    case AST_BINOP_AND_AND:
                    {
                        IR_Value* check_rhs_block_val =
                            ir_builder_create_block(ir_builder, "sc_and_check_rhs", cur_func);

                        ir_builder_emit_if_cond(ir_builder, lhs_expr, check_rhs_block_val,
                                                else_block_val, origin);

                        ir_builder_set_insert_block(ir_builder, check_rhs_block_val);
                        ir_builder_emit_if_cond(ir_builder, rhs_expr, then_block_val,
                                                else_block_val, origin);
                        ir_builder_set_insert_block(ir_builder, check_rhs_block_val);
                        break;
                    }

                    case AST_BINOP_OR_OR:
                    {
                        IR_Value* check_rhs_block_val =
                            ir_builder_create_block(ir_builder, "sc_or_check_rhs", cur_func);

                        ir_builder_emit_if_cond(ir_builder, lhs_expr, then_block_val,
                                                check_rhs_block_val, origin);

                        ir_builder_set_insert_block(ir_builder, check_rhs_block_val);
                        ir_builder_emit_if_cond(ir_builder, rhs_expr, then_block_val,
                                                else_block_val, origin);
                        ir_builder_set_insert_block(ir_builder, check_rhs_block_val);
                        break;
                    }

                    case AST_BINOP_EQ:
                    case AST_BINOP_LTEQ:
                    case AST_BINOP_GT:
                    case AST_BINOP_LT:
                    case AST_BINOP_GTEQ:
                    case AST_BINOP_NEQ:
                    case AST_BINOP_AND:
                    case AST_BINOP_OR:
                    case AST_BINOP_MOD:
                    {
                        IR_Value* cond_value = ir_builder_emit_expression(ir_builder, cond_expr);
                        ir_builder_emit_jmp_if(ir_builder, cond_value, then_block_val, origin);
                        ir_builder_emit_jmp(ir_builder, else_block_val, origin);
                        break;
                    }

                    default: assert(false);
                }


                break;
            }

            case AST_EXPR_UNARY:
            {
                AST_Expression* operand_expr = cond_expr->unary.operand;
                switch (cond_expr->unary.op)
                {
                    case AST_UNOP_NOT:
                    {
                        ir_builder_emit_if_cond(ir_builder, operand_expr, else_block_val,
                                                then_block_val, origin);
                        break;
                    }

                    default: assert(false);
                }

                break;
            }

            case AST_EXPR_CAST:
            case AST_EXPR_DOT:
            case AST_EXPR_IDENTIFIER:
            case AST_EXPR_BOOL_LITERAL:
            case AST_EXPR_CALL:
            case AST_EXPR_SUBSCRIPT:
            {
                IR_Value* cond_value = ir_builder_emit_expression(ir_builder, cond_expr);
                ir_builder_emit_jmp_if(ir_builder, cond_value, then_block_val, origin);
                ir_builder_emit_jmp(ir_builder, else_block_val, origin);
                break;
            }

            default: assert(false);
        }
    }

    void ir_builder_emit_call_arg(IR_Builder* ir_builder, IR_Value* arg_value,
                                File_Pos origin, bool is_vararg /*= false*/)
    {
        assert(ir_builder);
        assert(arg_value);

        if (arg_value->kind == IRV_AGGREGATE_LITERAL)
        {
            IR_Value* allocl = ir_builder_emit_allocl(ir_builder, arg_value->type,
                                                    "compound_lit", origin);
            ir_builder_emit_store(ir_builder, allocl, arg_value, origin);
            // arg_value = ir_builder_emit_load(ir_builder, allocl, origin);
            arg_value = allocl;
        }

        IR_Value* is_vararg_value = nullptr;
        if (is_vararg)
        {
            is_vararg_value = ir_boolean_literal(ir_builder, Builtin::type_bool, true);
        }
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_PUSH_CALL_ARG,
                                                arg_value, is_vararg_value, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_call(IR_Builder* ir_builder, IR_Value* func_value,
                                IR_Value* num_args, File_Pos origin)
    {
        assert(ir_builder);
        assert(func_value);
        assert(num_args);
        assert(num_args->kind == IRV_INT_LITERAL);

        if (func_value->kind == IRV_FUNCTION)
        {
            IR_Function* function = func_value->function;
            assert(function->type);

            auto ret_type = function->type->function.return_type;
            if (ret_type->kind == AST_TYPE_MRV) ret_type = ret_type->mrv.struct_type;

            IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, ret_type);
            auto op = IR_OP_CALL;
            IR_Value* arg_1 = func_value;
            if (func_value->function->flags & IR_FUNC_FLAG_FOREIGN)
            {
                op = IR_OP_CALL_EX;
            }
            IR_Instruction* iri = ir_instruction_new(ir_builder, origin, op, arg_1,
                num_args, result_value);
            ir_builder_emit_instruction(ir_builder, iri);
            return result_value;
        }
        else if (func_value->type->kind == AST_TYPE_POINTER)
        {
            assert(func_value->type->pointer.base->kind == AST_TYPE_FUNCTION);
            AST_Type* func_type = func_value->type->pointer.base;
            IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY,
                                                func_type->function.return_type);
            IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_CALL_PTR,
                                                    func_value,
                num_args, result_value);
            ir_builder_emit_instruction(ir_builder, iri);
            return result_value;
        }
        else assert(false);

        assert(false);
        return nullptr;
    }

    IR_Value* ir_builder_emit_call(IR_Builder* ir_builder, IR_Value* func_value, int num_args,
                                File_Pos origin)
    {
        IR_Value* num_args_val = ir_integer_literal(ir_builder, Builtin::type_s64, num_args);
        return ir_builder_emit_call(ir_builder, func_value, num_args_val, origin);
    }

    IR_Value* ir_builder_emit_builtin_function_call(IR_Builder* ir_builder,
                                                    AST_Expression* call_expr)
    {
        assert(ir_builder);
        assert(call_expr);
        assert(call_expr->kind == AST_EXPR_CALL);
        assert(call_expr->call.builtin_function != AST_BUILTIN_FUNC_INVALID);

        switch (call_expr->call.builtin_function)
        {
            case AST_BUILTIN_FUNC_CREATE_THREAD:
            {
                assert(BUF_LENGTH(call_expr->call.arg_expressions) == 2);
                AST_Expression* func_expr = call_expr->call.arg_expressions[0];
                AST_Expression* user_data_expr = call_expr->call.arg_expressions[1];
                IR_Value* func_value = ir_builder_emit_expression(ir_builder, func_expr);
                IR_Value* user_data_val = ir_builder_emit_expression(ir_builder, user_data_expr);
                return ir_builder_emit_create_thread(ir_builder, func_value, user_data_val,
                                                    call_expr->file_pos);
            }

            case AST_BUILTIN_FUNC_JOIN_THREAD:
            {
                assert(BUF_LENGTH(call_expr->call.arg_expressions) == 1);
                AST_Expression* thread_expr = call_expr->call.arg_expressions[0];
                IR_Value* thread_value = ir_builder_emit_expression(ir_builder, thread_expr);
                return ir_builder_emit_join_thread(ir_builder, thread_value, call_expr->file_pos);
            }

            case AST_BUILTIN_FUNC_COMPARE_AND_SWAP:
            {
                assert(BUF_LENGTH(call_expr->call.arg_expressions) == 3);
                AST_Expression* pointer_expr = call_expr->call.arg_expressions[0];
                AST_Expression* val_expr = call_expr->call.arg_expressions[1];
                AST_Expression* new_val_expr = call_expr->call.arg_expressions[2];
                IR_Value* pointer_val = ir_builder_emit_expression(ir_builder, pointer_expr);
                IR_Value* val = ir_builder_emit_expression(ir_builder, val_expr);
                IR_Value* new_val = ir_builder_emit_expression(ir_builder, new_val_expr);
                return ir_builder_emit_compare_and_swap(ir_builder, pointer_val, val, new_val,
                                                        call_expr->file_pos);
            }

            default: assert(false);
        }

        assert(false);
        return nullptr;
    }

    IR_Value* ir_builder_emit_subscript(IR_Builder* ir_builder, IR_Value* base_value,
                                        IR_Value* index_value, File_Pos origin)
    {
        assert(ir_builder);
        assert(base_value);
        assert(index_value);

        switch (base_value->type->kind)
        {
            case AST_TYPE_POINTER:
            {
                IR_Value* result_value = nullptr;
                if (base_value->type->pointer.base->kind == AST_TYPE_STATIC_ARRAY)
                {
                    AST_Type* arr_base_type = base_value->type->pointer.base->static_array.base;
                    result_value =
                        ir_value_new(ir_builder, IRV_TEMPORARY,
                                    ast_find_or_create_pointer_type(ir_builder->context,
                                                                    arr_base_type));
                }
                else
                {
                    result_value = ir_value_new(ir_builder, IRV_TEMPORARY,
                                                        base_value->type);
                }

                IR_Instruction* iri = ir_instruction_new(ir_builder, origin,
                                                        IR_OP_ARRAY_OFFSET_POINTER,
                                                        base_value, index_value,
                                                        result_value);
                ir_builder_emit_instruction(ir_builder, iri);
                result_value = ir_builder_emit_load(ir_builder, result_value, origin);

                return result_value;
                break;
            }

            case AST_TYPE_STATIC_ARRAY:
            {
                AST_Type* type = ast_find_or_create_pointer_type(ir_builder->context,
                                                            base_value->type->static_array.base);
                IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, type);
                IR_Instruction* iri = ir_instruction_new(ir_builder, origin,
                                                        IR_OP_ARRAY_OFFSET_POINTER,
                                                        base_value, index_value,
                                                        result_value);
                ir_builder_emit_instruction(ir_builder, iri);
                result_value = ir_builder_emit_load(ir_builder, result_value, origin);
                return result_value;
                break;
            }

            default: assert(false);
        }

        assert(false);
        return nullptr;
    }

    void ir_builder_emit_jmp(IR_Builder* ir_builder, IR_Value* block_value, File_Pos origin)
    {
        assert(ir_builder);
        assert(block_value);
        assert(block_value->kind == IRV_BLOCK);

        auto last_iri = ir_builder->insert_block->last_instruction;
        if (last_iri)
        {
            if (last_iri->op == IR_OP_RETURN ||
                last_iri->op == IR_OP_JMP)
                return;
        }

        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_JMP, block_value,
                                                nullptr, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);

    }

    void ir_builder_emit_jmp_if(IR_Builder* ir_builder, IR_Value* cond_value,
                                IR_Value* block_value, File_Pos origin)
    {
        assert(ir_builder);
        assert(cond_value);
        assert(block_value);
        assert(block_value->kind == IRV_BLOCK);

        assert(cond_value->type == Builtin::type_bool ||
               cond_value->type->kind == AST_TYPE_POINTER ||
               cond_value->type->kind == AST_TYPE_ENUM ||
               (cond_value->type->flags & AST_TYPE_FLAG_INT));

        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_JMP_IF, cond_value,
                                                block_value, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_allocl(IR_Builder* ir_builder, AST_Type* type, const char* name,
                                    File_Pos origin)
    {
        assert(ir_builder);
        assert(type);
        assert(name);

        // assert(type->kind == AST_TYPE_POINTER ||
        //        type->flags & AST_TYPE_FLAG_INT);

        IR_Value* allocl_value = ir_value_allocl_new(ir_builder, type, name, origin);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_ALLOCL, nullptr,
                                                nullptr, allocl_value);
        allocl_value->flags |= IRV_FLAG_ASSIGNED;
        ir_builder_emit_instruction(ir_builder, iri);

        assert(ir_builder->current_function);
        BUF_PUSH(ir_builder->current_function->local_temps, allocl_value);

        return allocl_value;
    }

    void ir_builder_emit_storel(IR_Builder* ir_builder, IR_Value* allocl_value,
                                IR_Value* new_value, File_Pos origin)
    {
        assert(ir_builder);
        assert(allocl_value);
        assert(new_value);

        assert(allocl_value->type == new_value->type);

        assert(allocl_value->kind == IRV_ALLOCL);
        assert(new_value->kind == IRV_TEMPORARY ||
               new_value->kind == IRV_ARGUMENT ||
               new_value->kind == IRV_ALLOCL ||
               new_value->kind == IRV_INT_LITERAL ||
               new_value->kind == IRV_STRING_LITERAL ||
               new_value->kind == IRV_CHAR_LITERAL ||
               new_value->kind == IRV_FLOAT_LITERAL ||
               new_value->kind == IRV_NULL_LITERAL ||
               new_value->kind == IRV_BOOL_LITERAL ||
               new_value->kind == IRV_AGGREGATE_LITERAL ||
               new_value->kind == IRV_ARRAY_LITERAL);

        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_STOREL, allocl_value,
                                                new_value, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_loadl(IR_Builder* ir_builder, IR_Value* allocl_value,
                                    File_Pos origin)
    {
        assert(ir_builder);
        assert(allocl_value);
        assert(allocl_value->kind == IRV_ALLOCL);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, allocl_value->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_LOADL, allocl_value,
                                                nullptr, result_value);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_value;
    }

    void ir_builder_emit_storea(IR_Builder* ir_builder, IR_Value* arg_value, IR_Value* new_value,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(arg_value);
        assert(new_value);

        assert(arg_value->kind == IRV_ARGUMENT);
        assert(new_value->kind == IRV_TEMPORARY ||
            new_value->kind == IRV_ARGUMENT ||
            new_value->kind == IRV_INT_LITERAL ||
            new_value->kind == IRV_BOOL_LITERAL);

        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_STOREA, arg_value,
                                                new_value, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_loada(IR_Builder* ir_builder, IR_Value* alloca_value,
                                    File_Pos origin)
    {
        assert(ir_builder);
        assert(alloca_value);
        assert(alloca_value->kind == IRV_ARGUMENT);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, alloca_value->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_LOADA, alloca_value,
                                                nullptr,
                                                result_value);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_value;
    }

    void ir_builder_emit_storep(IR_Builder* ir_builder, IR_Value* pointer_allocl,
                                IR_Value* new_value, File_Pos origin)
    {
        assert(ir_builder);
        assert(pointer_allocl);
        assert(new_value);

        assert(pointer_allocl->kind == IRV_ALLOCL ||
            (pointer_allocl->kind == IRV_TEMPORARY) &&
            pointer_allocl->type->kind == AST_TYPE_POINTER);
        assert(pointer_allocl->type);
        assert(pointer_allocl->type->kind == AST_TYPE_POINTER);
        assert(new_value->kind == IRV_TEMPORARY ||
            new_value->kind == IRV_ARGUMENT ||
            new_value->kind == IRV_INT_LITERAL ||
            new_value->kind == IRV_ALLOCL ||
            new_value->kind == IRV_CHAR_LITERAL ||
            new_value->kind == IRV_FLOAT_LITERAL ||
            new_value->kind == IRV_BOOL_LITERAL ||
            new_value->kind == IRV_NULL_LITERAL ||
            new_value->kind == IRV_STRING_LITERAL ||
            new_value->kind == IRV_AGGREGATE_LITERAL ||
            new_value->kind == IRV_ARRAY_LITERAL);

        assert(pointer_allocl->type->pointer.base == new_value->type);

        IR_Instruction* iri = ir_instruction_new(ir_builder, origin,
                                                IR_OP_STOREP, pointer_allocl,
                                                new_value, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_loadp(IR_Builder* ir_builder, IR_Value* pointer,
                                    File_Pos origin)
    {
        assert(ir_builder);
        assert(pointer);
        assert(pointer->type);
        assert(pointer->type->kind == AST_TYPE_POINTER);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY,
                                            pointer->type->pointer.base);

        IR_Instruction* iri = ir_instruction_new(ir_builder, origin,
                                                IR_OP_LOADP, pointer, nullptr,
                                                result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_global(IR_Builder* ir_builder, AST_Declaration* global_decl)
    {
        assert(ir_builder);
        assert(global_decl);
        assert(global_decl->kind == AST_DECL_MUTABLE ||
               global_decl->kind == AST_DECL_CONSTANT_VAR);

        AST_Type* type = nullptr;
        AST_Expression* init_expr = nullptr;
        if (global_decl->kind == AST_DECL_MUTABLE)
        {
            type = global_decl->mutable_decl.type;
            init_expr = global_decl->mutable_decl.init_expression;
        }
        else
        {
            type = global_decl->constant_var.type;
            init_expr = global_decl->constant_var.init_expression;
            assert(init_expr);
        }

        IR_Value* init_value = nullptr;

        if (init_expr)
        {
            init_value = ir_builder_emit_expression(ir_builder, init_expr);
        }

        IR_Value* global_value = ir_value_global_new(ir_builder,
                                                        type,
                                                        init_value,
                                                        global_decl->identifier->atom.data,
                                                        global_decl->file_pos);
        global_value->flags |= IRV_FLAG_ASSIGNED;

        BUF_PUSH(ir_builder->result.globals, global_value);
        return global_value;
    }

    void ir_builder_emit_storeg(IR_Builder* ir_builder, IR_Value* global_value,
                                IR_Value* new_value, File_Pos origin)
    {
        assert(ir_builder);
        assert(global_value);
        assert(global_value->kind == IRV_GLOBAL);
        assert(new_value);

        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_STOREG, global_value,
                                                new_value, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_loadg(IR_Builder* ir_builder, IR_Value* global_value,
                                    File_Pos origin)
    {
        assert(ir_builder);
        assert(global_value);
        assert(global_value->kind == IRV_GLOBAL);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, global_value->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_LOADG, global_value,
                                                nullptr, result_value);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_value;
    }

    IR_Value* ir_builder_emit_load(IR_Builder* ir_builder, IR_Value* store, File_Pos origin)
    {
        assert(ir_builder);
        assert(store);

        switch (store->kind)
        {
            case IRV_ARGUMENT:
            {
                return ir_builder_emit_loada(ir_builder, store, origin);
                break;
            }

            case IRV_GLOBAL:
            {
                return ir_builder_emit_loadg(ir_builder, store, origin);
                break;
            }

            case IRV_ALLOCL:
            {
                return ir_builder_emit_loadl(ir_builder, store, origin);
                break;
            }

            default:
            {
                assert(store->type->kind == AST_TYPE_POINTER);
                return ir_builder_emit_loadp(ir_builder, store, origin);
                break;
            }
        }

        assert(false);
        return nullptr;
    }

    void ir_builder_emit_store(IR_Builder* ir_builder, IR_Value* store, IR_Value* new_value,
                            File_Pos origin)
    {
        assert(ir_builder);
        assert(store);
        assert(new_value);

        switch (store->kind)
        {
            case IRV_ARGUMENT:
            {
                ir_builder_emit_storea(ir_builder, store, new_value, origin);
                break;
            }

            case IRV_GLOBAL:
            {
                ir_builder_emit_storeg(ir_builder, store, new_value, origin);
                break;
            }

            case IRV_ALLOCL:
            {
                ir_builder_emit_storel(ir_builder, store, new_value, origin);
                break;
            }

            default:
            {
                assert(store->type->kind == AST_TYPE_POINTER);
                ir_builder_emit_storep(ir_builder, store, new_value, origin);
                break;
            }
        }
    }

    // @TODO: FIXME: @:CLEANUP: The result value should always be a pointer, so force pointer
    //    should not be necessary. The problem is that only emit_addrof calls calls this function
    //    expecting a pointer, so other call sites will take the address of this pointer instead
    //    of using the pointer.
    IR_Value* ir_builder_emit_lvalue(IR_Builder* ir_builder, AST_Expression* lvalue_expr,
                                    bool force_pointer /*=false*/)
    {
        assert(ir_builder);
        assert(lvalue_expr);

        if (lvalue_expr->kind == AST_EXPR_IDENTIFIER)
        {
            AST_Declaration* lvalue_decl = lvalue_expr->identifier->declaration;
            IR_Value* target_alloc = ir_builder_value_for_declaration(ir_builder, lvalue_decl);
            assert(target_alloc);

            if (lvalue_expr->type->kind == AST_TYPE_FUNCTION)
            {
                IR_Value* func_value =
                    ir_builder_value_for_declaration(ir_builder,
                                                    lvalue_expr->identifier->declaration);
                assert(func_value);
                if (func_value->function->flags & IR_FUNC_FLAG_FOREIGN)
                {
                    return ir_builder_emit_addrof_foreign(ir_builder, func_value,
                                                        lvalue_expr->type,
                                                        lvalue_expr->file_pos);
                }
                else
                {
                    return ir_builder_emit_addrof_function(ir_builder, func_value, func_value->type,
                                                        lvalue_expr->file_pos);
                }
            }

            if (target_alloc->kind == IRV_FUNCTION)
            {
                return ir_builder_emit_addrof_function(ir_builder, target_alloc, lvalue_decl->function.type, lvalue_expr->file_pos);
            }
            else if (force_pointer &&
                (target_alloc->kind == IRV_ALLOCL ||
                 target_alloc->kind == IRV_ARGUMENT ||
                 target_alloc->kind == IRV_GLOBAL))
            {
                AST_Type* result_type = ast_find_or_create_pointer_type(ir_builder->context,
                                                                        lvalue_expr->type);
                IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, result_type);
                IR_Instruction* iri = ir_instruction_new(ir_builder, lvalue_expr->file_pos,
                                                        IR_OP_ADDROF, target_alloc, nullptr,
                                                        result_value);
                ir_builder_emit_instruction(ir_builder, iri);
                target_alloc = result_value;
            }
            return target_alloc;
        }
        else if (lvalue_expr->kind == AST_EXPR_DOT)
        {
            AST_Expression* member_expression = lvalue_expr->dot.member_expression;

            IR_Value* base_value = nullptr;

            if (lvalue_expr->type->kind == AST_TYPE_FUNCTION)
            {
                IR_Value* func_value = ir_builder_value_for_declaration(
                    ir_builder, member_expression->identifier->declaration);
                assert(func_value);
                assert(func_value->function->flags & IR_FUNC_FLAG_FOREIGN);
                return ir_builder_emit_addrof_foreign(ir_builder, func_value, lvalue_expr->type,
                                                    lvalue_expr->file_pos);
            }

            base_value = ir_builder_emit_lvalue(ir_builder, lvalue_expr->dot.base_expression);

            bool need_load = true;

            if (base_value->kind == IRV_ALLOCL &&
                (base_value->type->kind == AST_TYPE_STRUCT ||
                base_value->type->kind == AST_TYPE_UNION))
            {
                need_load = false;
            }

            if (base_value->type->kind == AST_TYPE_POINTER &&
                    (base_value->type->pointer.base->kind == AST_TYPE_STRUCT ||
                    base_value->type->pointer.base->kind == AST_TYPE_UNION))
            {
                need_load = false;
            }

            if (base_value->kind == IRV_ARGUMENT &&
                    (base_value->type->kind == AST_TYPE_STRUCT ||
                    base_value->type->kind == AST_TYPE_UNION))
            {
                need_load = false;
            }

            if (need_load)
            {
                base_value = ir_builder_emit_load(ir_builder, base_value, lvalue_expr->file_pos);
            }

            assert(base_value->type->kind == AST_TYPE_STRUCT ||
                base_value->type->kind == AST_TYPE_UNION ||
                (base_value->type->kind == AST_TYPE_POINTER &&
                    (base_value->type->pointer.base->kind == AST_TYPE_STRUCT ||
                    base_value->type->pointer.base->kind == AST_TYPE_UNION)));

            AST_Type* base_type = base_value->type;
            AST_Type* aggregate_type = nullptr;

            if (base_type->kind == AST_TYPE_STRUCT ||
                base_type->kind == AST_TYPE_UNION)
            {
                aggregate_type = base_type;
            }
            else if (base_type->kind == AST_TYPE_POINTER)
            {
                assert(base_type->pointer.base->kind == AST_TYPE_STRUCT ||
                    base_type->pointer.base->kind == AST_TYPE_UNION);
                aggregate_type = base_type->pointer.base;
            }
            else assert(false);

            auto agg_members = aggregate_type->aggregate_type.member_declarations;
            uint64_t member_index = 0;
            bool found = false;
            for (uint64_t i = 0; i < BUF_LENGTH(agg_members); i++)
            {
                AST_Declaration* member_decl = agg_members[i];
                if (!member_decl->identifier)
                {
                    AST_Type* anon_agg_type = member_decl->mutable_decl.type;
                    assert(anon_agg_type->kind == AST_TYPE_STRUCT ||
                        anon_agg_type->kind == AST_TYPE_UNION);
                    auto anon_members = anon_agg_type->aggregate_type.member_declarations;
                    for (uint64_t j = 0; j < BUF_LENGTH(anon_members); j++)
                    {
                        AST_Declaration* anon_member = anon_members[j];
                        if (member_expression->identifier->atom == anon_member->identifier->atom)
                        {
                            IR_Value* anon_value =
                                ir_builder_emit_aggregate_offset_pointer(ir_builder,
                                                                        base_value, i,
                                                                        member_expression->file_pos);
                            return ir_builder_emit_aggregate_offset_pointer(ir_builder,
                                                                            anon_value, j,
                                                                            member_expression->file_pos);
                        }
                    }
                }
                else if (member_expression->identifier->atom == member_decl->identifier->atom)
                {
                    return ir_builder_emit_aggregate_offset_pointer(ir_builder, base_value, i,
                                                                    member_expression->file_pos);
                }
            }

            assert(false);
        }
        else if (lvalue_expr->kind == AST_EXPR_SUBSCRIPT)
        {
            if (lvalue_expr->subscript.call_expression)
            {
                return ir_builder_emit_expression(ir_builder,
                                                  lvalue_expr->subscript.call_expression);
            }

            IR_Value* base_value = ir_builder_emit_lvalue(ir_builder,
                                                        lvalue_expr->subscript.base_expression);
            if (!(base_value->type->kind == AST_TYPE_STATIC_ARRAY) &&
                    !(base_value->type->kind == AST_TYPE_POINTER &&
                    base_value->type->pointer.base->kind == AST_TYPE_STATIC_ARRAY))
            {
                base_value = ir_builder_emit_load(ir_builder, base_value, lvalue_expr->file_pos);
            }

            IR_Value* index_value =
                ir_builder_emit_expression(ir_builder, lvalue_expr->subscript.index_expression);

            return ir_builder_emit_array_offset_pointer(ir_builder, base_value, index_value,
                                                        lvalue_expr->file_pos);
        }
        else if (lvalue_expr->kind == AST_EXPR_UNARY)
        {
            assert(lvalue_expr->unary.op == AST_UNOP_DEREF);
            IR_Value* result = ir_builder_emit_lvalue(ir_builder, lvalue_expr->unary.operand);
            return ir_builder_emit_load(ir_builder, result, lvalue_expr->file_pos);
        }
        else if (lvalue_expr->kind == AST_EXPR_MAKE_LVALUE)
        {
            IR_Value* allocl = ir_builder_emit_allocl(ir_builder, lvalue_expr->type, "",
                                                      lvalue_expr->file_pos);
            IR_Value* init_val = ir_builder_emit_expression(ir_builder,
                                                            lvalue_expr->make_lvalue.expression);
            ir_builder_emit_store(ir_builder, allocl, init_val, lvalue_expr->file_pos);

            AST_Type* ptr_type = ast_find_or_create_pointer_type(ir_builder->context,
                                                                 lvalue_expr->type);
            IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, ptr_type);
            IR_Instruction* iri = ir_instruction_new(ir_builder, lvalue_expr->file_pos,
                                                     IR_OP_ADDROF, allocl, nullptr, result_value);
            ir_builder_emit_instruction(ir_builder, iri);
            return result_value;
        }
        else if (lvalue_expr->kind == AST_EXPR_COMPOUND_LITERAL &&
                 lvalue_expr->type->kind == AST_TYPE_STATIC_ARRAY)
        {
            assert(false);
        }
        else
        {
            assert(false);
        }

        assert(false);
        return nullptr;
    }

    IR_Value* ir_builder_emit_cast(IR_Builder* ir_builder, IR_Value* value, AST_Type* type,
                                File_Pos origin)
    {
        assert(ir_builder);
        assert(value);
        assert(value->kind == IRV_TEMPORARY ||
               value->kind == IRV_INT_LITERAL ||
               value->kind == IRV_CHAR_LITERAL ||
               value->kind == IRV_FLOAT_LITERAL ||
               value->kind == IRV_ARGUMENT ||
               (value->kind == IRV_ALLOCL && value->type->kind == AST_TYPE_STATIC_ARRAY));
        assert(type);

        if (value->type == type)
        {
            return value;
        }

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, origin, IR_OP_CAST, value, nullptr,
                                                result);

        ir_builder_emit_instruction(ir_builder, iri);
        return result;
    }

    void ir_builder_emit_assert(IR_Builder* ir_builder, IR_Value* assert_value, File_Pos origin)
    {
        assert(ir_builder);
        assert(assert_value);

        assert(Builtin::decl_default_assert_handler);
        assert(ir_builder->context->builtin_ir_builder);
        IR_Value* assert_func = ir_builder_value_for_declaration(
            ir_builder->context->builtin_ir_builder, Builtin::decl_default_assert_handler);
        assert(assert_func);

        Atom file_name_atom = atom_get(ir_builder->context->atom_table, origin.file_name);
        Atom func_name_atom = atom_get(ir_builder->context->atom_table,
                                    ir_builder->current_function->name);
        IR_Value* func_name_val = ir_string_literal(ir_builder, Builtin::type_pointer_to_u8,
                                                    func_name_atom);
        IR_Value* file_name_val = ir_string_literal(ir_builder, Builtin::type_pointer_to_u8,
                                                    file_name_atom);
        IR_Value* line_val = ir_integer_literal(ir_builder, Builtin::type_s64, origin.line);


        if (assert_value->type != Builtin::type_bool)
        {
            assert_value = ir_builder_emit_cast(ir_builder, assert_value, Builtin::type_bool,
                                                origin);
        }

        ir_builder_emit_call_arg(ir_builder, assert_value, origin, false);
        ir_builder_emit_call_arg(ir_builder, func_name_val, origin, false);
        ir_builder_emit_call_arg(ir_builder, file_name_val, origin, false);
        ir_builder_emit_call_arg(ir_builder, line_val, origin, false);

        ir_builder_emit_call(ir_builder, assert_func, 4, origin);
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
        else if (type->kind == AST_TYPE_POINTER)
        {
            return ir_null_literal(ir_builder, type);
        }
        else if (type == Builtin::type_void)
        {
            return nullptr;
        }
        else if (type->kind == AST_TYPE_STRUCT)
        {
            BUF(IR_Value*) member_values = nullptr;
            for (uint64_t i = 0; i < BUF_LENGTH(type->aggregate_type.member_declarations); i++)
            {
                AST_Declaration* member_decl = type->aggregate_type.member_declarations[i];
                AST_Type* member_type = member_decl->mutable_decl.type;

                IR_Value* member_value = ir_builder_emit_zero_literal(ir_builder, member_type);
                BUF_PUSH(member_values, member_value);
            }

            return ir_aggregate_literal(ir_builder, type, member_values, true);
        }
        else if (type->kind == AST_TYPE_ENUM)
        {
            return ir_builder_emit_zero_literal(ir_builder, type->aggregate_type.base_type);
        }
        else if (type->kind == AST_TYPE_MRV)
        {
            return ir_builder_emit_zero_literal(ir_builder, type->mrv.struct_type);
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
        result->flags |= (IRV_FLAG_ASSIGNED | IRV_FLAG_CONST);
        return result;
    }

    IR_Value* ir_null_literal(IR_Builder* ir_builder, AST_Type* type)
    {
        assert(ir_builder);
        assert(type);

        IR_Value* result = ir_value_new(ir_builder, IRV_NULL_LITERAL, type);
        result->value.pointer = nullptr;
        result->flags |= (IRV_FLAG_ASSIGNED | IRV_FLAG_CONST);
        return result;
    }

    IR_Value* ir_string_literal(IR_Builder* ir_builder, AST_Type* type, Atom string)
    {
        assert(ir_builder);
        assert(type);

        IR_Value* result = ir_value_new(ir_builder, IRV_STRING_LITERAL, type);
        result->value.pointer = arena_alloc_array(&ir_builder->result.string_literal_arena, uint8_t, string.length + 1);
        memcpy(result->value.pointer, string.data, string.length + 1);
        result->flags |= (IRV_FLAG_ASSIGNED | IRV_FLAG_CONST);

        return result;
    }

    IR_Value* ir_integer_literal(IR_Builder* ir_builder, AST_Type* type, uint64_t u64)
    {
        assert(ir_builder);
        assert(type);

        IR_Value* result = ir_value_new(ir_builder, IRV_INT_LITERAL, type);
        result->value.u64 = u64;
        result->flags |= (IRV_FLAG_ASSIGNED | IRV_FLAG_CONST);

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
        result->flags |= (IRV_FLAG_ASSIGNED | IRV_FLAG_CONST);

        return result;
    }

    IR_Value* ir_character_literal(IR_Builder* ir_builder, AST_Type* type, char c)
    {
        assert(ir_builder);
        assert(type);

        IR_Value* result = ir_value_new(ir_builder, IRV_CHAR_LITERAL, type);
        result->value.u8 = c;
        result->flags |= (IRV_FLAG_ASSIGNED | IRV_FLAG_CONST);

        return result;
    }

    IR_Value* ir_aggregate_literal(IR_Builder* ir_builder, AST_Type* aggregate_type,
                                BUF(IR_Value*) member_values, bool is_const)
    {
        IR_Value* result = ir_value_new(ir_builder, IRV_AGGREGATE_LITERAL, aggregate_type);
        result->value.compound_values = member_values;
        result->flags |= IRV_FLAG_ASSIGNED;
        if (is_const)
        {
            result->flags |= IRV_FLAG_CONST;
        }

        return result;
    }

    IR_Value* ir_array_literal(IR_Builder* ir_builder, AST_Type* array_type,
                               BUF(IR_Value*) member_values, bool is_const)
    {
        IR_Value* result = ir_value_new(ir_builder, IRV_ARRAY_LITERAL, array_type);
        result->value.compound_values = member_values;
        result->flags |= IRV_FLAG_ASSIGNED;
        if (is_const)
        {
            result->flags |= IRV_FLAG_CONST;
        }

        return result;
    }

    uint64_t ir_builder_emit_foreign(IR_Builder* ir_builder, AST_Declaration* decl)
    {
        assert(ir_builder);
        assert(decl->kind == AST_DECL_FUNC);

        assert(decl->identifier);
        auto atom = decl->identifier->atom;

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->context->foreign_table); i++)
        {
            if (ir_builder->context->foreign_table[i].name == atom)
            {
                return i;
            }
        }

        Foreign_Function ff = { atom, decl->file_pos };

        BUF_PUSH(ir_builder->context->foreign_table, ff);
        return BUF_LENGTH(ir_builder->context->foreign_table) - 1;
    }

    IR_Value* ir_builder_emit_phi(IR_Builder* ir_builder, AST_Type* type, File_Pos file_pos)
    {
        assert(ir_builder);
        assert(type);

        IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, file_pos, IR_OP_PHI,
                                                nullptr, nullptr, result);
        ir_builder_emit_instruction(ir_builder, iri);

        result->temp.phi = iri;

        return result;
    }

    IR_Value* ir_builder_emit_get_type_info(IR_Builder* ir_builder, uint64_t index,
                                            File_Pos file_pos)
    {
        assert(ir_builder);
        assert(index);


        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY,
                                            Builtin::type_pointer_to_Type_Info);
        IR_Value* index_value = ir_integer_literal(ir_builder, Builtin::type_u64, index);
        IR_Instruction* iri = ir_instruction_new(ir_builder, file_pos, IR_OP_GET_TYPE_INFO,
                                                index_value, nullptr, result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    void phi_node_add_incoming(IR_Value* phi_value, IR_Block* block, IR_Value* value)
    {
        assert(phi_value);
        assert(phi_value->kind == IRV_TEMPORARY);
        assert(phi_value->temp.phi);
        assert(block);
        assert(value);

        IR_Phi_Pair pair = { block, value };

        IR_Instruction* iri = phi_value->temp.phi;
        assert(iri);
        assert(iri->op == IR_OP_PHI);

        BUF_PUSH(iri->phi_pairs, pair);
    }

    IR_Value* ir_builder_emit_mrv(IR_Builder* ir_builder, AST_Expression* list_expr)
    {
        AST_Type* mrv_type = ir_builder->current_function->type->function.return_type;
        assert(mrv_type->kind == AST_TYPE_MRV);

        BUF(IR_Value*) member_values = nullptr;
        bool is_const = true;
        for (uint64_t i = 0; i < BUF_LENGTH(list_expr->list.expressions); i++)
        {
            auto expr = list_expr->list.expressions[i];
            is_const &= (bool)(expr->flags & AST_EXPR_FLAG_CONST);

            IR_Value* mem_value = ir_builder_emit_expression(ir_builder, expr);
            BUF_PUSH(member_values, mem_value);
        }

        return ir_aggregate_literal(ir_builder, mrv_type->mrv.struct_type, member_values,
                                    is_const);
    }

    IR_Function* ir_function_new(IR_Builder* ir_builder, File_Pos file_pos, const char* name,
                                AST_Type* func_type, AST_Scope* body_scope)
    {
        assert(ir_builder);
        assert(name);
        assert(func_type);

        IR_Function* result = arena_alloc(&ir_builder->arena, IR_Function);
        result->flags = IR_FUNC_FLAG_NONE;
        result->file_pos = file_pos;
        result->name = name;
        result->type = func_type;
        result->first_block = nullptr;
        result->last_block = nullptr;
        result->arguments = nullptr;
        result->local_temps = nullptr;
        result->is_entry = false;
        result->dcb_data = {};
        result->body_scope = body_scope;

        return result;
    }

    IR_Value* ir_value_new(IR_Builder* ir_builder, IR_Value_Kind kind, AST_Type* type)
    {
        assert(ir_builder);

        IR_Value* result = arena_alloc(&ir_builder->arena, IR_Value);
        result->kind = kind;
        result->type = type;
        result->flags = 0;
        result->value.pointer = nullptr;

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
        assert(function->type);

        IR_Value* result = ir_value_new(ir_builder, IRV_FUNCTION, function->type);
        result->function = function;
        result->flags |= (IRV_FLAG_ASSIGNED | IRV_FLAG_CONST);

        return result;
    }

    IR_Value* ir_value_block_new(IR_Builder* ir_builder, IR_Block* block)
    {
        assert(ir_builder);
        assert(block);

        IR_Value* result = ir_value_new(ir_builder, IRV_BLOCK, nullptr);
        result->block = block;
        result->flags |= (IRV_FLAG_ASSIGNED | IRV_FLAG_CONST);

        return result;
    }

    IR_Value* ir_value_allocl_new(IR_Builder* ir_builder, AST_Type* type, const char* name,
                                File_Pos file_pos)
    {
        assert(ir_builder);
        assert(type);
        assert(name);

        IR_Value* result = ir_value_new(ir_builder, IRV_ALLOCL, type);
        result->allocl.name = name;
        result->allocl.file_pos = file_pos;

        assert(ir_builder->current_function);
        result->allocl.index = BUF_LENGTH(ir_builder->current_function->local_temps);
        BUF_PUSH(ir_builder->current_function->local_temps, result);
        return result;
    }

    IR_Value* ir_value_global_new(IR_Builder* ir_builder, AST_Type* type,
                                    IR_Value* init_value, const char* name,
                                    File_Pos file_pos)
    {
        assert(ir_builder);
        assert(type);
        assert(name);

        IR_Value* result = ir_value_new(ir_builder, IRV_GLOBAL, type);
        result->global.name = name;
        result->global.init_value = init_value;
        result->global.file_pos = file_pos;
        result->value.pointer = nullptr;

        result->global.index = BUF_LENGTH(ir_builder->context->global_table);
        Global_Variable global_var = { &ir_builder->result, result };
        BUF_PUSH(ir_builder->context->global_table, global_var);
        return result;
    }

    IR_Instruction* ir_instruction_new(IR_Builder* ir_builder, File_Pos origin, IR_Operator op,
                                    IR_Value* arg1, IR_Value* arg2, IR_Value* result_value)
    {
        assert(ir_builder);

        if (arg1)
        {
            assert(arg1->flags & IRV_FLAG_ASSIGNED);
        }

        if (arg2)
        {
            assert(arg2->flags & IRV_FLAG_ASSIGNED);
        }

        if (result_value)
        {
            assert(!(result_value->flags & IRV_FLAG_ASSIGNED));
            result_value->flags |= IRV_FLAG_ASSIGNED;
        }

        IR_Instruction* result = arena_alloc(&ir_builder->arena, IR_Instruction);
        result->origin = origin;
        result->op = op;
        result->arg1 = arg1;
        result->arg2 = arg2;
        result->result = result_value;

        assert(stack_count(ir_builder->scope_stack));
        result->scope = stack_top(ir_builder->scope_stack);

        result->next = nullptr;

        return result;
    }

    bool ir_instruction_is_terminator(IR_Operator op)
    {
        return (op == IR_OP_JMP ||
                op == IR_OP_RETURN ||
                op == IR_OP_SWITCH);
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
            result &= ir_validate_block(ir_function, block, valres);
            block = block->next;
        }

        return result;
    }

    bool ir_validate_block(IR_Function* ir_func, IR_Block* ir_block, IR_Validation_Result* valres)
    {
        assert(ir_block);
        assert(valres);

        bool result = true;

        if (!ir_block->first_instruction)
        {
            assert(!ir_block->last_instruction);
            result = false;
            ir_report_validation_error(valres, "Block is empty: %s:%s", ir_func->name,
                                       ir_block->name.data);
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

        String_Builder sb;
        string_builder_init(&sb, 2048);

        string_builder_appendf(&sb, "IR module: %s\n", ir_builder->ast_module->module_name);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->result.dynamic_lib_names); i++)
        {
            auto lib_atom = ir_builder->result.dynamic_lib_names[i];
            string_builder_appendf(&sb, "#dynamic_link %s\n", lib_atom.data);
        }

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->result.global_constants); i++)
        {
            auto gc = ir_builder->result.global_constants[i];
            string_builder_appendf(&sb, "global(%s) = ", gc.name);
            ir_print_value(gc.value, &sb);
            string_builder_append(&sb, "\n");
        }

        ir_builder_print_functions(ir_builder, &sb);

        char* result = string_builder_to_string(&sb);
        string_builder_free(&sb);

        if (result)
        {
            printf("%s", result);
        }
    }

    void ir_builder_print_functions(IR_Builder* ir_builder, String_Builder* string_builder)
    {
        assert(ir_builder);
        assert(string_builder);

        auto func_count = BUF_LENGTH(ir_builder->result.functions);

        if (func_count)
        {
            string_builder_append(string_builder, "\n");
        }

        for (uint64_t i = 0; i < func_count; i++)
        {
            IR_Function* func = ir_builder->result.functions[i];
            ir_print_function(func, string_builder);
        }
    }
}
