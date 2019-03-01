#include "ir.h"

#include "builtin.h"

namespace Zodiac
{

    void ir_builder_init(IR_Builder* ir_builder)
    {
        assert(ir_builder);

        ir_builder->arena = arena_create(MB(1));
        ir_builder->functions = nullptr;
        ir_builder->next_temp_index = 0;
    }

    IR_Function* ir_builder_create_function(IR_Builder* ir_builder, const char* name,
                                            AST_Type* return_type)
    {
        assert(ir_builder);
        assert(name);
        assert(return_type);

        IR_Function* result = arena_alloc(&ir_builder->arena, IR_Function);
        result->name = name;
        result->return_type = return_type;
        result->arguments = nullptr;
        result->blocks = nullptr;

        BUF_PUSH(ir_builder->functions, result);

        return result;
    }

    IR_Value* ir_function_add_argument(IR_Builder* ir_builder, IR_Function* func,
                                       const char* name, AST_Type* type)
    {
        assert(ir_builder);
        assert(func);
        assert(name);
        assert(type);

        uint64_t arg_index = BUF_LENGTH(func->arguments);
        IR_Value* arg_value = ir_value_argument(ir_builder, arg_index, name, type);
        BUF_PUSH(func->arguments, arg_value);

        return arg_value;
    }

    void ir_function_add_block(IR_Function* func, IR_Block* block)
    {
        assert(func);
        assert(block);

        assert(block->function == nullptr);

        for (uint64_t i = 0; i < BUF_LENGTH(func->blocks); i++)
        {
            IR_Block* existing_block = func->blocks[i];
            assert(existing_block != block);
        }

        block->function = func;
        BUF_PUSH(func->blocks, block);
    }

    IR_Block* ir_builder_create_block(IR_Builder* ir_builder, const char* name)
    {
        assert(ir_builder);
        assert(name);

        IR_Block* result = arena_alloc(&ir_builder->arena, IR_Block);
        result->name = name;
        result->instructions = nullptr;

        return result;
    }

    IR_Value* ir_builder_insert_add(IR_Builder* ir_builder, IR_Block* block,
                                    IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(block);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result_value = ir_value_temporary(ir_builder, lhs->type);
        IR_Instruction iri = { IRI_ADD, lhs, rhs, result_value };

        BUF_PUSH(block->instructions, iri);

        return result_value;
    }

    IR_Value* ir_builder_insert_sub(IR_Builder* ir_builder, IR_Block* block,
                                    IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(block);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result_value = ir_value_temporary(ir_builder, lhs->type);
        IR_Instruction iri = { IRI_SUB, lhs, rhs, result_value };

        BUF_PUSH(block->instructions, iri);

        return result_value;
    }

    IR_Value* ir_builder_insert_bool_not(IR_Builder* ir_builder, IR_Block* block,
                                         IR_Value* bool_value)
    {
        assert(ir_builder);
        assert(block);
        assert(bool_value);

        assert(bool_value->type == Builtin::type_bool);

        IR_Value* result_value = ir_value_temporary(ir_builder, Builtin::type_bool);
        IR_Instruction iri = { IRI_NOT_BOOL, bool_value, nullptr, result_value };

        BUF_PUSH(block->instructions, iri);

        return result_value;
    }

    IR_Value* ir_builder_insert_call(IR_Builder* ir_builder, IR_Block* block,
                                     IR_Function* func,
                                     IR_Value** args, uint64_t num_args)
    {
        assert(ir_builder);
        assert(block);
        assert(func);

        for (uint64_t i = 0; i < num_args; i++)
        {
            ir_builder_insert_arg_push(ir_builder, block, args[i]);
        }

        IR_Value* num_args_lit = ir_builder_get_literal(ir_builder, Builtin::type_int,
                                                        num_args);
        IR_Value* return_value = ir_value_temporary(ir_builder, func->return_type);
        IR_Value* func_value = ir_value_function(ir_builder, func);
        IR_Instruction iri = { IRI_CALL, num_args_lit, func_value, return_value };
        BUF_PUSH(block->instructions, iri);

        return return_value;
    }

    void ir_builder_insert_arg_push(IR_Builder* ir_builder, IR_Block* block,
                                    IR_Value* argument_value)
    {
        assert(ir_builder);
        assert(block);
        assert(argument_value);

        IR_Instruction iri =  { IRI_PUSH_ARG, argument_value, nullptr, nullptr };
        BUF_PUSH(block->instructions, iri);
    }

    IR_Value* ir_builder_insert_lt(IR_Builder* ir_builder, IR_Block* block,
                                   IR_Value* lhs, IR_Value* rhs)
    {
        assert(ir_builder);
        assert(block);
        assert(lhs);
        assert(rhs);

        assert(lhs->type == rhs->type);

        IR_Value* result_value = ir_value_temporary(ir_builder, Builtin::type_bool);
        IR_Instruction iri = { IRI_LT, lhs, rhs, result_value };

        BUF_PUSH(block->instructions, iri);

        return result_value;
    }

    void ir_builder_insert_cond_jump(IR_Builder* ir_builder, IR_Block* block, IR_Value* cond_value,
                                          IR_Block* then_block, IR_Block* else_block)
    {
        assert(ir_builder);
        assert(block);
        assert(cond_value);
        assert(then_block);
        assert(else_block);

        IR_Value* not_cond_value = ir_builder_insert_bool_not(ir_builder, block, cond_value);
        IR_Value* else_block_value = ir_value_block(ir_builder, else_block);
        IR_Instruction iri = { IRI_COND_JMP, not_cond_value, else_block_value };
        BUF_PUSH(block->instructions, iri);

        IR_Value* then_block_value = ir_value_block(ir_builder, then_block);
        iri = { IRI_JMP, else_block_value, nullptr };
        BUF_PUSH(block->instructions, iri);
    }

    IR_Value* ir_value_argument(IR_Builder* ir_builder, uint64_t arg_index,
                                const char* arg_name, AST_Type* arg_type)
    {
        assert(ir_builder);
        assert(arg_name);
        assert(arg_type);

        IR_Value* result = arena_alloc(&ir_builder->arena, IR_Value);
        result->kind = IRV_ARGUMENT;
        result->argument.index = arg_index;
        result->argument.name = arg_name;
        result->type = arg_type;

        return result;
    }

    IR_Value* ir_value_temporary(IR_Builder* ir_builder, AST_Type* type)
    {
        assert(ir_builder);
        assert(type);

        IR_Value* result = arena_alloc(&ir_builder->arena, IR_Value);
        result->kind = IRV_TEMPORARY;
        result->type = type;
        result->temp.index = ir_builder->next_temp_index++;

        return result;
    }

    IR_Value* ir_value_function(IR_Builder* ir_builder, IR_Function* ir_func)
    {
        assert(ir_builder);
        assert(ir_func);

        IR_Value* result = arena_alloc(&ir_builder->arena, IR_Value);
        result->kind = IRV_FUNC;
        result->ir_function = ir_func;

        return result;
    }

    IR_Value* ir_value_block(IR_Builder* ir_builder, IR_Block* ir_block)
    {
        assert(ir_builder);
        assert(ir_block);

        IR_Value* result = arena_alloc(&ir_builder->arena, IR_Value);
        result->kind = IRV_BLOCK;
        result->ir_block = ir_block;

        return result;
    }

    void ir_builder_insert_return(IR_Builder* ir_builder, IR_Block* block, IR_Value* result_value)
    {
        assert(ir_builder);
        assert(block);
        assert(result_value);

        IR_Instruction iri = { IRI_RETURN, result_value, nullptr, nullptr };
        BUF_PUSH(block->instructions, iri);
    }

    void ir_builder_insert_return(IR_Builder* ir_builder, IR_Block* block)
    {
        assert(ir_builder);
        assert(block);

        IR_Instruction iri = { IRI_RETURN, nullptr, nullptr, nullptr };
        BUF_PUSH(block->instructions, iri);
    }

    IR_Value* ir_builder_get_literal(IR_Builder* ir_builder, AST_Type* ast_type, int64_t s64)
    {
        assert(ir_builder);
        assert(ast_type);

        assert(ast_type == Builtin::type_int);

        IR_Value* result = arena_alloc(&ir_builder->arena, IR_Value);
        result->kind = IRV_LITERAL;
        result->type = ast_type;

        result->literal.s64 = s64;

        return result;
    }

    void ir_builder_print(IR_Builder* ir_builder)
    {
        assert(ir_builder);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->functions); i++)
        {
            IR_Function* func = ir_builder->functions[i];
            ir_builder_print_function(ir_builder, func);
        }
    }

    void ir_builder_print_function(IR_Builder* ir_builder, IR_Function* func)
    {
        assert(ir_builder);
        assert(func);

        printf("%s(", func->name);

        for (uint64_t ai = 0; ai < BUF_LENGTH(func->arguments); ai++)
        {
            if (ai > 0)
            {
                printf(", ");
            }
            IR_Value* arg_val = func->arguments[ai];
            printf("%s: ", arg_val->argument.name);
            ir_builder_print_type(ir_builder, arg_val->type);
        }

        printf(") -> ");
        ir_builder_print_type(ir_builder, func->return_type);
        printf("\n");

        if (func->blocks)
        {
            printf("{\n");

            for (uint64_t bi = 0; bi < BUF_LENGTH(func->blocks); bi++)
            {
                IR_Block* block = func->blocks[bi];
                ir_builder_print_block(ir_builder, block);
            }

            printf("}\n");
        }

        printf("\n");
    }

    void ir_builder_print_block(IR_Builder* ir_builder, IR_Block* block)
    {
        assert(ir_builder);
        assert(block);

        printf("%s:\n", block->name);

        for (uint64_t i = 0; i < BUF_LENGTH(block->instructions); i++)
        {
            IR_Instruction iri = block->instructions[i];
            ir_builder_print_instruction(ir_builder, iri);
        }
    }

    void ir_builder_print_instruction(IR_Builder* ir_builder, IR_Instruction iri)
    {
        assert(ir_builder);

        printf("\t");

        if (iri.result)
        {
            ir_builder_print_value(ir_builder, iri.result);
            printf(" = ");
        }

        switch (iri.kind)
        {
            case IRI_ADD:
                printf("IRI_ADD");
                break;

            case IRI_SUB:
                printf("IRI_SUB");
                break;

            case IRI_NOT_BOOL:
                printf("IRI_NOT_BOOL");
                break;

            case IRI_RETURN:
                printf("IRI_RETURN");
                break;

            case IRI_CALL:
                printf("IRI_CALL");
                break;

            case IRI_PUSH_ARG:
                printf("IRI_PUSH_ARG");
                break;

            case IRI_LT:
                printf("IRI_LT");
                break;

            case IRI_JMP:
                printf("IRI_JMP");
                break;

            case IRI_COND_JMP:
                printf("IRI_JMP_COND");
                break;

            default: assert(false);
        }

        if (iri.lhs)
        {
            printf("  ");

            ir_builder_print_value(ir_builder, iri.lhs);
        }

        if (iri.rhs)
        {
            printf(", ");
            ir_builder_print_value(ir_builder, iri.rhs);
        }

        printf("\n");
    }

    void ir_builder_print_value(IR_Builder* ir_builder, IR_Value* value)
    {
        assert(ir_builder);
        assert(value);

        switch (value->kind)
        {
            case IRV_ARGUMENT:
            {
                printf("arg(%s)", value->argument.name);
                break;
            }

            case IRV_TEMPORARY:
            {
                printf("temp(%lu)", value->temp.index);
                break;
            }

            case IRV_LITERAL:
            {
                printf("lit(%ld)", value->literal.s64);
                break;
            }

            case IRV_FUNC:
            {
                printf("func(%s)", value->ir_function->name);
                break;
            }

            case IRV_BLOCK:
            {
                printf("block(%s)", value->ir_block->name);
                break;
            }

            default: assert(false);
        }
    }

    void ir_builder_print_type(IR_Builder* ir_builder, AST_Type* ast_type)
    {
        assert(ir_builder);
        assert(ast_type);

        auto flags = ast_type->flags;

        if (flags & AST_TYPE_FLAG_VOID)
        {
            printf("void");
        }
        else if (flags & AST_TYPE_FLAG_INT)
        {
            if (flags & AST_TYPE_FLAG_SIGNED)
            {
                printf("s");
            }
            else
            {
                printf("u");
            }

            printf("%lu", ast_type->bit_size);
        }
        else
        {
            assert(false);
        }
    }
}
