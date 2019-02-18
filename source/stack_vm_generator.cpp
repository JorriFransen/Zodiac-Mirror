#include "stack_vm_generator.h"

#include "builtin.h"

namespace Zodiac
{
    void stack_vm_generator_init(Stack_VM_Generator* generator, Context* context, AST_Module* ast_module)
    {
        assert(generator);
        assert(context);
        assert(ast_module);


        generator->context = context;
        generator->ast_module = ast_module;
        generator->done = false;
        generator->progressed_on_last_cycle = false;
        generator->entry_addr_pos_set = false;
        generator->replacements_done = false;

        generator->result = {};

        emit_instruction(generator, SVMI_CALL_IMM);
        emit_address_placeholder(generator, SVM_ADDRESS_PLACEHOLDER_ENTRY);
        emit_u64(generator, 0); // num args;

        emit_instruction(generator, SVMI_HALT);
    }

    void stack_vm_generator_do_cycle(Stack_VM_Generator* generator)
    {
        assert(generator);

        generator->progressed_on_last_cycle = false;
        generator->done = true;

        AST_Module* module = generator->ast_module;
        for (uint64_t i = 0; i < BUF_LENGTH(module->global_declarations); i++)
        {
            AST_Declaration* global_decl = module->global_declarations[i];
            if (global_decl->flags & AST_DECL_FLAG_RESOLVED &&
                !(global_decl->flags & AST_DECL_FLAG_GENERATED))
            {
                emit_declaration(generator, global_decl);
                generator->progressed_on_last_cycle = true;
            }
            if (!(global_decl->flags & AST_DECL_FLAG_GENERATED))
            {
                generator->done = false;
            }
        }

        if (generator->done)
        {
            assert(!generator->replacements_done);
            stack_vm_generator_do_replacements(generator);
            assert(generator->replacements_done);
        }
    }

    void stack_vm_generator_do_replacements(Stack_VM_Generator* generator)
    {
        assert(generator);

        assert(generator->entry_addr_pos_set);
        AST_Declaration* entry_point_decl = generator->ast_module->entry_point;
        assert(entry_point_decl);
        assert(entry_point_decl->kind == AST_DECL_FUNC);

        assert(entry_point_decl->gen_data);
        auto gen_data = get_gen_data(generator, entry_point_decl);
        assert(gen_data);
        assert(gen_data->kind == SVM_GEN_DATA_FUNC);
        uint64_t entry_addr = gen_data->func.address;
        assert(BUF_LENGTH(generator->result.instructions) > generator->entry_addr_pos);
        generator->result.instructions[generator->entry_addr_pos] = entry_addr;

        generator->replacements_done = true;
    }

    static void emit_address_placeholder(Stack_VM_Generator* generator, Stack_VM_Address_Placeholder_Kind kind)
    {
        assert(generator);

        switch (kind)
        {
            case SVM_ADDRESS_PLACEHOLDER_ENTRY:
            {
                assert(!generator->entry_addr_pos_set);
                generator->entry_addr_pos = BUF_LENGTH(generator->result.instructions);
                emit_address(generator, 0);
                generator->entry_addr_pos_set = true;
                break;
            }

            default: assert(false);
        }
    }

    static void emit_declaration(Stack_VM_Generator* generator, AST_Declaration* decl)
    {
        assert(generator);
        assert(decl);
        assert(decl->flags & AST_DECL_FLAG_RESOLVED);
        assert(!(decl->flags & AST_DECL_FLAG_GENERATED));

        switch (decl->kind)
        {
            case AST_DECL_FUNC:
                emit_function_declaration(generator, decl);
                break;

            default: assert(false);
        }

        decl->flags |= AST_DECL_FLAG_GENERATED;
    }

    static void emit_function_declaration(Stack_VM_Generator* generator, AST_Declaration* decl)
    {
        assert(generator);
        assert(decl);
        assert(decl->kind == AST_DECL_FUNC);
        assert(decl->flags & AST_DECL_FLAG_RESOLVED);
        assert(!(decl->flags & AST_DECL_FLAG_GENERATED));

        uint64_t entry_addr = BUF_LENGTH(generator->result.instructions);
        assert(!decl->gen_data);
        auto gen_data = get_gen_data(generator, decl);
        gen_data->func.address = entry_addr;

        assert(decl->function.body_block);
        emit_statement(generator, decl->function.body_block);
    }

    static void emit_statement(Stack_VM_Generator* generator, AST_Statement* stmt)
    {
        assert(generator);
        assert(stmt);
        assert(!(stmt->flags & AST_STMT_FLAG_GENERATED));

        switch (stmt->kind)
        {
            case AST_STMT_BLOCK:
            {
                for (uint64_t i = 0; i < BUF_LENGTH(stmt->block.statements); i++)
                {
                    AST_Statement* block_stmt = stmt->block.statements[i];
                    emit_statement(generator, block_stmt);
                }
                break;
            }

            case AST_STMT_RETURN:
            {
                assert(stmt->return_expression);
                emit_expression(generator, stmt->return_expression);
                emit_instruction(generator, SVMI_RETURN);
                break;
            }

            default: assert(false);
        }

        stmt->flags |= AST_STMT_FLAG_GENERATED;
    }

    static void emit_expression(Stack_VM_Generator* generator, AST_Expression* expr)
    {
        assert(generator);
        assert(expr);
        assert(!(expr->flags & AST_EXPR_FLAG_GENERATED));

        switch (expr->kind)
        {
            case AST_EXPR_BINARY:
            {
                emit_binary_expression(generator, expr);
                break;
            }

            case AST_EXPR_IDENTIFIER:
            {
                emit_identifier_expression(generator, expr);
                break;
            }

            case AST_EXPR_LITERAL:
            {
                emit_literal_expression(generator, expr);
                break;
            }

            default: assert(false);
        }

        expr->flags |= AST_EXPR_FLAG_GENERATED;
    }

    static void emit_binary_expression(Stack_VM_Generator* generator, AST_Expression* expr)
    {
        assert(generator);
        assert(expr);
        assert(expr->kind == AST_EXPR_BINARY);
        assert(!(expr->flags & AST_EXPR_FLAG_GENERATED));

        emit_expression(generator, expr->binary.lhs);
        emit_expression(generator, expr->binary.rhs);

        assert(expr->type == Builtin::type_int);

        switch (expr->binary.op)
        {
            case AST_BINOP_ADD:
            {
                emit_instruction(generator, SVMI_ADD_S64);
                break;
            }
            default: assert(false);
        }
    }

    static void emit_identifier_expression(Stack_VM_Generator* generator, AST_Expression* expr)
    {
        assert(generator);
        assert(expr);
        assert(expr->kind == AST_EXPR_IDENTIFIER);

        assert(!(expr->flags & AST_EXPR_FLAG_GENERATED));

        auto decl = expr->identifier->declaration;
        assert(decl);
        assert(decl->kind == AST_DECL_MUTABLE);

        switch (decl->location)
        {
            case AST_DECL_LOC_INVALID:
            {
                assert(false);
                break;
            }

            case AST_DECL_LOC_ARGUMENT:
            {
                emit_argument_load(generator, decl);
                break;
            }

            case AST_DECL_LOC_LOCAL:
            {
                assert(false);
                break;
            }

            case AST_DECL_LOC_GLOBAL:
            {
                assert(false);
                break;
            }

            default: assert(false);
        }

    }

    static void emit_literal_expression(Stack_VM_Generator* generator, AST_Expression* expr)
    {
        assert(generator);
        assert(expr);
        assert(expr->kind == AST_EXPR_LITERAL);

        assert(expr->type == Builtin::type_int);

        emit_instruction(generator, SVMI_PUSH_S64);
        emit_s64(generator, expr->literal.u64);
    }

    static void emit_argument_load(Stack_VM_Generator* generator, AST_Declaration* arg_decl)
    {
        assert(generator);
        assert(arg_decl);
        assert(arg_decl->location == AST_DECL_LOC_ARGUMENT);

        assert(arg_decl->mutable_decl.type);
        assert(arg_decl->mutable_decl.type == Builtin::type_int);

        int64_t offset = -2 - arg_decl->mutable_decl.argument_index;
        emit_instruction(generator, SVMI_LOADL_S64);
        emit_s64(generator, offset);
    }

    static void emit_instruction(Stack_VM_Generator* generator, Stack_VM_Instruction instruction)
    {
        assert(generator);
        emit_u64(generator, instruction);
    }

    static void emit_address(Stack_VM_Generator* generator, uint64_t address)
    {
        assert(generator);
        emit_u64(generator, address);
    }


    static void emit_s64(Stack_VM_Generator* generator, int64_t s64)
    {
        assert(generator);
        emit_u64(generator, s64);
    }

    static void emit_u64(Stack_VM_Generator* generator, uint64_t u64)
    {
        assert(generator);
        BUF_PUSH(generator->result.instructions, u64);
    }

    static Stack_VM_Gen_Data* get_gen_data(Stack_VM_Generator* generator, AST_Declaration* declaration)
    {
        assert(generator);
        assert(declaration);

        if (declaration->gen_data)
        {
            return (Stack_VM_Gen_Data*)declaration->gen_data;
        }
        else
        {
            Stack_VM_Gen_Data* result = arena_alloc(generator->context->arena, Stack_VM_Gen_Data);

            switch (declaration->kind)
            {
                case AST_DECL_FUNC:
                {
                    result->kind = SVM_GEN_DATA_FUNC;
                    break;
                }

                default: assert(false);
            }

            declaration->gen_data = result;
            return result;
        }
    }
}
