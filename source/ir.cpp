#include "ir.h"

#include "builtin.h"

namespace Zodiac
{

    void ir_builder_init(IR_Builder* ir_builder)
    {
        assert(ir_builder);

        ir_builder->arena = arena_create(MB(1));
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
        IR_Instruction iri = { IRI_CALL, num_args_lit, nullptr, return_value };
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

        assert(false);
    }
}
