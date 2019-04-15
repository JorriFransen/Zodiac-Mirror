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
    }

    struct _IR_Decl_To_Func_
    {
        AST_Declaration* decl = nullptr;
        IR_Function* func = nullptr;
    };

    IR_Module ir_builder_emit_module(IR_Builder* ir_builder, AST_Module* module)
    {
        assert(ir_builder);
        assert(module);

        assert(ir_builder->ast_module == nullptr);
        ir_builder->ast_module = module;

        // Emit global declarations
        for (uint64_t i = 0; i < BUF_LENGTH(module->global_declarations); i++)
        {
            AST_Declaration* global_decl = module->global_declarations[i];
            assert(global_decl->location == AST_DECL_LOC_GLOBAL);

            switch (global_decl->kind)
            {
                case AST_DECL_FUNC:
                {
                    IR_Value* func_value = ir_builder_begin_function(ir_builder,
                                                                     global_decl->identifier->atom.data,
                                                                     global_decl->function.return_type);
                    if (global_decl->function.body_block)
                    {
                        IR_Value* entry_block = ir_builder_create_block(ir_builder, "entry", func_value);
                        ir_builder_set_insert_block(ir_builder, entry_block);

                        for (uint64_t i = 0; i < BUF_LENGTH(global_decl->function.args); i++)
                        {
                            AST_Declaration* arg_decl = global_decl->function.args[i];
                            assert(arg_decl->kind == AST_DECL_MUTABLE);
                            assert(arg_decl->location == AST_DECL_LOC_ARGUMENT);

                            IR_Value* arg_value = ir_builder_emit_function_arg(ir_builder,
                                                                            arg_decl->identifier->atom.data,
                                                                            arg_decl->mutable_decl.type);
                            ir_builder_push_value_and_decl(ir_builder, arg_value, arg_decl);
                        }
                    }
                    else
                    {
                        func_value->function->flags |= IR_FUNC_FLAG_FOREIGN;
                        func_value->function->foreign_index =
                            ir_builder_emit_foreign(ir_builder, global_decl->identifier->atom);
                    }
                    ir_builder_end_function(ir_builder, func_value);

                    _IR_Decl_To_Func_ decl_to_func_entry = { global_decl, func_value->function };
                    ir_builder_push_value_and_decl(ir_builder, func_value, global_decl);
                    break;
                }

                case AST_DECL_MUTABLE:
                {
                    assert(false);
                    break;
                }

                case AST_DECL_TYPE:
                {
                    assert(false);
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

                default: assert(false);
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

                    ir_builder_emit_statement(ir_builder, decl->function.body_block);

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
                }
                else
                {
                    assert(decl->directive);
                    assert(decl->directive->kind == AST_DIREC_FOREIGN);
                }
            }
        }

        return ir_builder->result;
    }

    void ir_builder_emit_statement(IR_Builder* ir_builder, AST_Statement* statement)
    {
        assert(ir_builder);
        assert(statement);

        switch (statement->kind)
        {
            case AST_STMT_DECLARATION:
            {
                AST_Declaration* decl = statement->declaration;
                assert(decl->kind == AST_DECL_MUTABLE);
                assert(decl->location = AST_DECL_LOC_LOCAL);

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
                    ir_builder_emit_statement(ir_builder, block_member_stmt);
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
                ir_builder_emit_statement(ir_builder, statement->if_stmt.then_statement);
                if (!ir_instruction_is_terminator(then_block_val->block->last_instruction->op))
                {
                    ir_builder_emit_jmp(ir_builder, post_if_block_val);
                }

                if (statement->if_stmt.else_statement)
                {
                    ir_builder_append_block(ir_builder, cur_func, else_block_val->block);
                    ir_builder_set_insert_block(ir_builder, else_block_val);
                    ir_builder_emit_statement(ir_builder, statement->if_stmt.else_statement);
                    if (!ir_instruction_is_terminator(else_block_val->block->last_instruction->op))
                    {
                        ir_builder_emit_jmp(ir_builder, post_if_block_val);
                    }
                }

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
                IR_Value* while_cond_block_value = ir_builder_create_block(ir_builder, "while_cond", cur_func);
                IR_Value* while_body_block_value = ir_builder_create_block(ir_builder, "while_body", cur_func);
                IR_Value* post_while_block_value = ir_builder_create_block(ir_builder, "post_while");

                ir_builder_emit_jmp(ir_builder, while_cond_block_value);

                ir_builder_set_insert_block(ir_builder, while_cond_block_value);
                IR_Value* cond_value = ir_builder_emit_expression(ir_builder, statement->while_stmt.cond_expr);
                ir_builder_emit_jmp_if(ir_builder, cond_value, while_body_block_value);
                ir_builder_emit_jmp(ir_builder, post_while_block_value);

                ir_builder_set_insert_block(ir_builder, while_body_block_value);
                ir_builder_emit_statement(ir_builder, statement->while_stmt.body_stmt);
                ir_builder_emit_jmp(ir_builder, while_cond_block_value);

                ir_builder_append_block(ir_builder, cur_func, post_while_block_value->block);
                ir_builder_set_insert_block(ir_builder, post_while_block_value);
                break;
            }

            case AST_STMT_FOR:
            {
                IR_Function* cur_func = ir_builder->current_function;
                IR_Value* for_cond_block_value = ir_builder_create_block(ir_builder, "for_cond", cur_func);
                IR_Value* for_body_block_value = ir_builder_create_block(ir_builder, "for_body", cur_func);
                IR_Value* post_for_block_value = ir_builder_create_block(ir_builder, "post_for");

                ir_builder_emit_statement(ir_builder, statement->for_stmt.init_stmt);
                ir_builder_emit_jmp(ir_builder, for_cond_block_value);

                ir_builder_set_insert_block(ir_builder, for_cond_block_value);
                IR_Value* cond_value = ir_builder_emit_expression(ir_builder, statement->for_stmt.cond_expr);
                ir_builder_emit_jmp_if(ir_builder, cond_value, for_body_block_value);
                ir_builder_emit_jmp(ir_builder, post_for_block_value);

                ir_builder_set_insert_block(ir_builder, for_body_block_value);
                ir_builder_emit_statement(ir_builder, statement->for_stmt.body_stmt);
                ir_builder_emit_statement(ir_builder, statement->for_stmt.step_stmt);
                ir_builder_emit_jmp(ir_builder, for_cond_block_value);

                ir_builder_append_block(ir_builder, cur_func, post_for_block_value->block);
                ir_builder_set_insert_block(ir_builder, post_for_block_value);
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


                IR_Value* element_byte_size_lit = ir_integer_literal(ir_builder, Builtin::type_int,
                                                                    element_type->base.bit_size / 8);
                IR_Value* element_byte_size = ir_builder_emit_load_lit(ir_builder, element_byte_size_lit);

                IR_Value* offset_value = ir_builder_emit_mul(ir_builder, element_byte_size, index_value);
                offset_value->type = base_pointer_value->type;

                IR_Value* target_addr = ir_builder_emit_add(ir_builder, base_pointer_value, offset_value);

                ir_builder_emit_storep(ir_builder, target_addr, new_value);
            }
            else assert(false);
        }
        else assert(false);
    }

    IR_Value* ir_builder_emit_expression(IR_Builder* ir_builder, AST_Expression* expression)
    {
        assert(ir_builder);
        assert(expression);

        switch (expression->kind)
        {
            case AST_EXPR_BINARY:
            {
                IR_Value* lhs_value = ir_builder_emit_expression(ir_builder, expression->binary.lhs);
                IR_Value* rhs_value = ir_builder_emit_expression(ir_builder, expression->binary.rhs);

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

                    default: assert(false);
                }
            }

            case AST_EXPR_UNARY:
            {
                switch (expression->unary.op)
                {
                    case AST_UNOP_MINUS:
                    {
                        assert(false);
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

                    default: assert(false);
                }
                break;
            }

            case AST_EXPR_STRING_LITERAL:
            {
                IR_Value* literal = ir_string_literal(ir_builder, expression->type, expression->string_literal.atom);

                IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, expression->type);
                IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_LOAD_LIT, literal, nullptr, result);
                ir_builder_emit_instruction(ir_builder, iri);

                return result;
                break;
            }

            case AST_EXPR_INTEGER_LITERAL:
            {
                IR_Value* literal = ir_integer_literal(ir_builder, expression->type,
                                                       (int64_t)expression->integer_literal.u64);

                IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, expression->type);
                IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_LOAD_LIT, literal, nullptr, result);
                ir_builder_emit_instruction(ir_builder, iri);

                return result;
                break;
            }

            case AST_EXPR_CHAR_LITERAL:
            {
                IR_Value* literal = ir_character_literal(ir_builder, expression->type, expression->character_literal.c);

                IR_Value* result = ir_value_new(ir_builder, IRV_TEMPORARY, expression->type);
                IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_LOAD_LIT, literal, nullptr, result);
                ir_builder_emit_instruction(ir_builder, iri);

                return result;
                break;
            }

            case AST_EXPR_IDENTIFIER:
            {
                AST_Declaration* ident_decl = expression->identifier->declaration;
                IR_Value* value = ir_builder_value_for_declaration(ir_builder, ident_decl);
                if (value->kind == IRV_TEMPORARY ||
                    value->kind == IRV_INT_LITERAL)
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
                else assert(false);

                return  value;

                break;
            }

            case AST_EXPR_CALL:
            {
                AST_Declaration* callee_decl = expression->call.callee_declaration;
                assert(callee_decl);
                assert(callee_decl->kind == AST_DECL_FUNC);

                IR_Value* callee_value = ir_builder_value_for_declaration(ir_builder, callee_decl);
                assert(callee_value);
                assert(callee_value->kind == IRV_FUNCTION);

                bool is_foreign = callee_value->function->flags & IR_FUNC_FLAG_FOREIGN;

                for (uint64_t i = 0; i < BUF_LENGTH(expression->call.arg_expressions); i++)
                {
                    AST_Expression* arg_expr = expression->call.arg_expressions[i];
                    IR_Value* arg_value = ir_builder_emit_expression(ir_builder, arg_expr);
                    ir_builder_emit_call_arg(ir_builder, arg_value, is_foreign);
                }

                IR_Value* num_args_lit = ir_integer_literal(ir_builder, Builtin::type_int,
                                                            BUF_LENGTH(expression->call.arg_expressions));
                return ir_builder_emit_call(ir_builder, callee_value, num_args_lit);
                break;
            }

            case AST_EXPR_SUBSCRIPT:
            {
                IR_Value* index_value = ir_builder_emit_expression(ir_builder,
                                                                   expression->subscript.index_expression);
                IR_Value* base_value = ir_builder_emit_expression(ir_builder,
                                                                  expression->subscript.base_expression);
                return ir_builder_emit_subscript(ir_builder, base_value, index_value);
                assert(false);
                break;
            }

            case AST_EXPR_COMPOUND_LITERAL:
            {
                AST_Type* array_type = expression->type;
                assert(array_type->kind == AST_TYPE_STATIC_ARRAY);

                IR_Value* result_value = ir_builder_emit_allocl(ir_builder, array_type, "array_compound_lit");

                for (uint64_t i = 0; i < BUF_LENGTH(expression->compound_literal.expressions); i++)
                {
                    AST_Expression* element_expression = expression->compound_literal.expressions[i];
                    IR_Value* element_value = ir_builder_emit_expression(ir_builder, element_expression);

                    IR_Value* pointer_value = ir_builder_emit_array_offset_pointer(ir_builder, result_value, i);

                    ir_builder_emit_storep(ir_builder, pointer_value, element_value);
                }

                return result_value;
                break;
            }

            case AST_EXPR_ARRAY_LENGTH:
            {
                AST_Type* array_type = expression->array_length.ident_expr->type;
                assert(array_type->kind == AST_TYPE_STATIC_ARRAY);
                IR_Value* count_literal = ir_integer_literal(ir_builder, Builtin::type_int, array_type->static_array.count);
                IR_Value* count_value = ir_builder_emit_load_lit(ir_builder, count_literal);
                return count_value;
                break;
            }

            default: assert(false);
        }

        return nullptr;
    }

    IR_Value* ir_builder_emit_load_lit(IR_Builder* ir_builder, IR_Value* literal)
    {
        assert(ir_builder);
        assert(literal);
        assert(literal->kind == IRV_INT_LITERAL);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, literal->type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_LOAD_LIT, literal, nullptr, result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_emit_addrof(IR_Builder* ir_builder, AST_Expression* expression)
    {
        assert(ir_builder);
        assert(expression);

        if (expression->kind == AST_EXPR_IDENTIFIER)
        {
            IR_Value* expression_value = ir_builder_value_for_declaration(ir_builder, expression->identifier->declaration);
            assert(expression_value);

            AST_Type* result_type = ast_find_or_create_pointer_type(ir_builder->context, ir_builder->ast_module,
                                                                    expression->type);
            IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, result_type);

            IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_ADDROF, expression_value, nullptr, result_value);
            ir_builder_emit_instruction(ir_builder, iri);

            return result_value;
        }
        else if (expression->kind == AST_EXPR_SUBSCRIPT)
        {
            AST_Expression* base_expr = expression->subscript.base_expression;
            if (base_expr->type->kind == AST_TYPE_POINTER)
            {
                AST_Type* element_type = base_expr->type->pointer.base;
                assert(base_expr->kind == AST_EXPR_IDENTIFIER);
                IR_Value* base_pointer_allocl = ir_builder_value_for_declaration(ir_builder,
                                                                                base_expr->identifier->declaration);
                IR_Value* base_pointer_value = ir_builder_emit_loadl(ir_builder, base_pointer_allocl);

                IR_Value* index_value = ir_builder_emit_expression(ir_builder, expression->subscript.index_expression);
                IR_Value* element_byte_size_lit = ir_integer_literal(ir_builder, Builtin::type_int,
                                                                    element_type->base.bit_size / 8);
                IR_Value* element_byte_size = ir_builder_emit_load_lit(ir_builder, element_byte_size_lit);
                IR_Value* offset_value = ir_builder_emit_mul(ir_builder, index_value, element_byte_size);
                offset_value->type = base_pointer_value->type;

                IR_Value* result_value = ir_builder_emit_add(ir_builder, base_pointer_value, offset_value);
                return result_value;
            }
            else
            {
                assert(base_expr->kind == AST_EXPR_IDENTIFIER);
                IR_Value* array_allocl = ir_builder_value_for_declaration(ir_builder,
                                                                          base_expr->identifier->declaration);
                assert(base_expr->type->kind == AST_TYPE_STATIC_ARRAY);
                IR_Value* index_value = ir_builder_emit_expression(ir_builder, expression->subscript.index_expression);
                return ir_builder_emit_array_offset_pointer(ir_builder, array_allocl, index_value);
            }
        }

        assert(false);
        return nullptr;
    }

    IR_Value* ir_builder_emit_deref(IR_Builder* ir_builder, AST_Expression* expression)
    {
        assert(ir_builder);
        assert(expression);
        assert(expression->kind == AST_EXPR_IDENTIFIER);

        IR_Value* expression_value = ir_builder_value_for_declaration(ir_builder, expression->identifier->declaration);
        assert(expression_value);

        AST_Type* operand_type = expression->type;
        assert(operand_type->kind == AST_TYPE_POINTER);

        AST_Type* result_type = operand_type->pointer.base;

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, result_type);

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_DEREF, expression_value, nullptr, result_value);
        ir_builder_emit_instruction(ir_builder, iri);

        return result_value;
    }

    IR_Value* ir_builder_value_for_declaration(IR_Builder* ir_builder, AST_Declaration* declaration)
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

        return nullptr;
    }

    IR_Value* ir_builder_begin_function(IR_Builder* ir_builder, const char* name, AST_Type* return_type)
    {
        assert(ir_builder);
        assert(name);
        assert(return_type);

        assert(ir_builder->current_function == nullptr);

        //TODO: Assert we don't have a function with the same name
        IR_Function* function = ir_function_new(ir_builder, name, return_type);
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

    IR_Value* ir_builder_create_block(IR_Builder* ir_builder, const char* name, IR_Function* function/*= nullptr*/)
    {
        assert(ir_builder);
        assert(name);

        IR_Block* block = arena_alloc(&ir_builder->arena, IR_Block);
        block->name = name;
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

    IR_Value* ir_builder_emit_array_offset_pointer(IR_Builder* ir_builder, IR_Value* array_allocl, uint64_t offset)
    {
        assert(ir_builder);
        assert(array_allocl);
        assert(array_allocl->type->kind == AST_TYPE_STATIC_ARRAY);
        assert(offset < array_allocl->type->static_array.count);

        IR_Value* offset_value_literal = ir_integer_literal(ir_builder, Builtin::type_int, offset);
        AST_Type* result_type = ast_find_or_create_pointer_type(ir_builder->context, ir_builder->ast_module,
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
        assert(array_allocl->type->kind == AST_TYPE_STATIC_ARRAY);
        assert(offset_value);
        assert(offset_value->kind == IRV_TEMPORARY);
        assert(offset_value->type == Builtin::type_int);

        AST_Type* result_type = ast_find_or_create_pointer_type(ir_builder->context, ir_builder->ast_module,
                                                                array_allocl->type->static_array.base);
        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, result_type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_ARRAY_OFFSET_POINTER, array_allocl,
                                                 offset_value, result_value);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_value;
    }

    void ir_builder_emit_instruction(IR_Builder* ir_builder, IR_Instruction* iri)
    {
        assert(ir_builder);
        assert(iri);

        assert(ir_builder->current_function);
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

    void ir_builder_emit_return(IR_Builder* ir_builder, IR_Value* ret_val)
    {
        assert(ir_builder);

        // TODO: Check return type agains current functions return type

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_RETURN, ret_val,
                                                 nullptr, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    void ir_builder_emit_call_arg(IR_Builder* ir_builder, IR_Value* arg_value, bool is_foreign/*=false*/)
    {
        assert(ir_builder);
        assert(arg_value);

        auto op = IR_OP_PUSH_CALL_ARG;
        if (is_foreign)
        {
            op = IR_OP_PUSH_EX_CALL_ARG;
        }
        IR_Instruction* iri = ir_instruction_new(ir_builder, op, arg_value,
                                                 nullptr, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_call(IR_Builder* ir_builder, IR_Value* func_value, IR_Value* num_args)
    {
        assert(ir_builder);
        assert(func_value);
        assert(func_value->kind == IRV_FUNCTION);
        assert(num_args);
        assert(num_args->kind == IRV_INT_LITERAL);

        IR_Function* function = func_value->function;
        assert(function->return_type);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, function->return_type);
        auto op = IR_OP_CALL;
        IR_Value* arg_1 = func_value;
        if (func_value->function->flags & IR_FUNC_FLAG_FOREIGN)
        {
            arg_1 = ir_integer_literal(ir_builder, Builtin::type_int, func_value->function->foreign_index);
            op = IR_OP_CALL_EX;
        }
        IR_Instruction* iri = ir_instruction_new(ir_builder, op, arg_1,
                                                 num_args, result_value);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_value;
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

        assert(type->kind == AST_TYPE_POINTER ||
               type->flags | AST_TYPE_FLAG_INT);

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
               new_value->kind == IRV_ALLOCL);

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
               new_value->kind == IRV_INT_LITERAL);

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_STOREP, pointer_allocl, new_value, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
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

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->result.foreign_table); i++)
        {
            if (ir_builder->result.foreign_table[i] == atom)
            {
                return i;
            }
        }

        BUF_PUSH(ir_builder->result.foreign_table, atom);
        return BUF_LENGTH(ir_builder->result.foreign_table) - 1;
    }

    IR_Function* ir_function_new(IR_Builder* ir_builder, const char* name, AST_Type* return_type)
    {
        assert(ir_builder);
        assert(name);
        assert(return_type);

        IR_Function* result = arena_alloc(&ir_builder->arena, IR_Function);
        result->flags = IR_FUNC_FLAG_NONE;
        result->name = name;
        result->return_type = return_type;
        result->first_block = nullptr;
        result->last_block = nullptr;
        result->arguments = nullptr;
        result->local_temps = nullptr;
        result->is_entry = false;

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
            assert(ir_builder->current_function);
            result->temp.index = BUF_LENGTH(ir_builder->current_function->local_temps);
            BUF_PUSH(ir_builder->current_function->local_temps, result);
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
        int length = vsnprintf(nullptr, 0, format, va_args) + 1;
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
        }
        printf(")\n");

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

        printf("\t%s:\n", block->name);

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
            printf(" = ");
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

            case IRV_STRING_LITERAL:
            {
                printf("lit(\"%s\")", value->value.string);
                break;
            }

            case IRV_INT_LITERAL:
            {
                printf("lit(%" PRId64 ")", value->value.s64);
                break;
            }

            case IRV_CHAR_LITERAL:
            {
                printf("lit('%c')", value->value.u8);
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
                printf("block(%s)", value->block->name);
                break;
            }

            case IRV_ALLOCL:
            {
                printf("allocl(%s)", value->allocl.name);
                break;
            }

            default: assert(false);
        }
    }
}
