#include "ir.h"

#include "builtin.h"

namespace Zodiac
{
    void ir_builder_init(IR_Builder* ir_builder)
    {
        assert(ir_builder);

        ir_builder->arena = arena_create(MB(1));
        ir_builder->functions = nullptr;
        ir_builder->current_function = nullptr;
        ir_builder->insert_block = nullptr;
        ir_builder->expect_arg_or_call = false;
    }

    struct _IR_Decl_To_Func_
    {
        AST_Declaration* decl = nullptr;
        IR_Function* func = nullptr;
    };

    void ir_builder_emit_module(IR_Builder* ir_builder, AST_Module* module)
    {
        assert(ir_builder);
        assert(module);

        //@TODO: Stack allocator
        BUF(_IR_Decl_To_Func_) decl_to_func = nullptr;

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
                    IR_Value* entry_block = ir_builder_create_block(ir_builder, "entry", func_value);
                    ir_builder_set_insert_block(ir_builder, entry_block);

                    for (uint64_t i = 0; i < BUF_LENGTH(global_decl->function.args); i++)
                    {
                        AST_Declaration* arg_decl = global_decl->function.args[i];
                        assert(arg_decl->kind == AST_DECL_MUTABLE);
                        assert(arg_decl->location == AST_DECL_LOC_ARGUMENT);

                        ir_builder_emit_function_arg(ir_builder, arg_decl->identifier->atom.data,
                                                     arg_decl->mutable_decl.type);
                    }
                    ir_builder_end_function(ir_builder, func_value);

                    _IR_Decl_To_Func_ decl_to_func_entry = { global_decl, func_value->function };
                    BUF_PUSH(decl_to_func, decl_to_func_entry);
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

                default: assert(false);
            }
        }

        // Emit function bodies
        for (uint64_t i = 0; i < BUF_LENGTH(decl_to_func); i++)
        {
            AST_Declaration* decl = decl_to_func[i].decl;
            IR_Function* func = decl_to_func[i].func;
            IR_Block* entry_block = func->first_block;

            ir_builder->current_function = func;
            ir_builder_set_insert_block(ir_builder, entry_block);

            ir_builder_emit_statement(ir_builder, decl->function.body_block);

            ir_builder->current_function = nullptr;
        }

        BUF_FREE(decl_to_func);
    }

    void ir_builder_emit_statement(IR_Builder* ir_builder, AST_Statement* statement)
    {
        assert(ir_builder);
        assert(statement);

        switch (statement->kind)
        {
            case AST_STMT_DECLARATION:
            {
                assert(false);
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
                assert(false);
                break;
            }

            default: assert(false);
        }
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

                    default: assert(false);
                }
            }

            case AST_EXPR_LITERAL:
            {
                return ir_integer_literal(ir_builder, expression->type, (int64_t)expression->literal.u64);
                break;
            }

            default: assert(false);
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
        BUF_PUSH(ir_builder->functions, function);

        ir_builder->expect_arg_or_call = false;

        return ir_value_function_new(ir_builder, function);
    }

    void ir_builder_end_function(IR_Builder* ir_builder, IR_Value* func_value)
    {
        assert(ir_builder);
        assert(func_value);
        assert(func_value->kind == IRV_FUNCTION);

        IR_Function* function = func_value->function;

        assert(ir_builder->current_function == function);
        assert(ir_builder->expect_arg_or_call == false);
        ir_builder->current_function = nullptr;

        ir_builder->insert_block = nullptr;
    }

    IR_Value* ir_builder_create_block(IR_Builder* ir_builder, const char* name, IR_Value* function_value)
    {
        assert(ir_builder);
        assert(name);
        assert(function_value);

        IR_Block* block = arena_alloc(&ir_builder->arena, IR_Block);
        block->name = name;
        block->first_instruction = nullptr;
        block->last_instruction = nullptr;

        if (function_value)
        {
            ir_builder_append_block(ir_builder, function_value->function, block);
        }

        return ir_value_block_new(ir_builder, block);
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
            function->last_block = block;
        }
        else
        {
            assert(function->last_block == nullptr);

            function->first_block = block;
            function->last_block = block;
        }
    }

    void ir_builder_set_insert_block(IR_Builder* ir_builder, IR_Block* block)
    {
        assert(ir_builder);
        assert(block);

        ir_builder->insert_block = block;

        ir_builder->expect_arg_or_call = false;

        auto li = block->last_instruction;
        if (li)
        {
            if (li->op == IR_OP_PUSH_CALL_ARG)
            {
                ir_builder->expect_arg_or_call = true;
            }
        }
    }

    void ir_builder_set_insert_block(IR_Builder* ir_builder, IR_Value* block_value)
    {
        assert(ir_builder);
        assert(block_value);
        assert(block_value->kind == IRV_BLOCK);

        ir_builder_set_insert_block(ir_builder, block_value->block);
    }

    void ir_builder_emit_instruction(IR_Builder* ir_builder, IR_Instruction* iri)
    {
        assert(ir_builder);
        assert(iri);

        assert(ir_builder->current_function);
        assert(ir_builder->insert_block);

        assert(iri->next == nullptr);

        if (ir_builder->expect_arg_or_call)
        {
            assert(iri->op == IR_OP_PUSH_CALL_ARG ||
                   iri->op == IR_OP_CALL);

            if (iri->op == IR_OP_CALL)
            {
                ir_builder->expect_arg_or_call = false;
            }
        }

        if (iri->op == IR_OP_PUSH_CALL_ARG)
        {
            ir_builder->expect_arg_or_call = true;
        }

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
        arg_value->argument.index = BUF_LENGTH(ir_builder->current_function->arguments);
        arg_value->assigned = true;

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

    void ir_builder_emit_return(IR_Builder* ir_builder, IR_Value* ret_val)
    {
        assert(ir_builder);

        // TODO: Check return type agains current functions return type

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_RETURN, ret_val,
                                                 nullptr, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    void ir_builder_emit_call_arg(IR_Builder* ir_builder, IR_Value* arg_value)
    {
        assert(ir_builder);
        assert(arg_value);

        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_PUSH_CALL_ARG, arg_value,
                                                 nullptr, nullptr);
        ir_builder_emit_instruction(ir_builder, iri);
    }

    IR_Value* ir_builder_emit_call(IR_Builder* ir_builder, IR_Value* func_value)
    {
        assert(ir_builder);
        assert(func_value);
        assert(func_value->kind == IRV_FUNCTION);

        IR_Function* function = func_value->function;
        assert(function->return_type);

        IR_Value* result_value = ir_value_new(ir_builder, IRV_TEMPORARY, function->return_type);
        IR_Instruction* iri = ir_instruction_new(ir_builder, IR_OP_CALL, func_value,
                                                 nullptr, result_value);
        ir_builder_emit_instruction(ir_builder, iri);
        return result_value;
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

    IR_Value* ir_integer_literal(IR_Builder* ir_builder, AST_Type* type, uint64_t s64)
    {
        assert(ir_builder);
        assert(type);

        IR_Value* result = ir_value_new(ir_builder, IRV_LITERAL, Builtin::type_int);
        result->literal.s64 = s64;
        result->assigned = true;

        return result;
    }

    IR_Function* ir_function_new(IR_Builder* ir_builder, const char* name, AST_Type* return_type)
    {
        assert(ir_builder);
        assert(name);
        assert(return_type);

        IR_Function* result = arena_alloc(&ir_builder->arena, IR_Function);
        result->name = name;
        result->return_type = return_type;
        result->first_block = nullptr;
        result->last_block = nullptr;
        result->arguments = nullptr;
        result->next_temp_index = 0;

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
            result->temp.index = ir_builder->current_function->next_temp_index++;
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

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->functions); i++)
        {
            ir_validate_function(ir_builder->functions[i], &valres);
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

    void ir_builder_print_functions(IR_Builder* ir_builder)
    {
        assert(ir_builder);

        for (uint64_t i = 0; i < BUF_LENGTH(ir_builder->functions); i++)
        {
            IR_Function* func = ir_builder->functions[i];
            ir_print_function(func);
        }
    }

    void ir_print_function(IR_Function* function)
    {
        assert(function);

        printf("%s(", function->name);
        for (uint64_t i = 0; i < BUF_LENGTH(function->arguments); i++)
        {
            if (i > 0)
            {
                printf(", ");
            }
            ir_print_value(function->arguments[i]);
        }
        printf(")\n{\n");

        IR_Block* block = function->first_block;

        while (block)
        {
            ir_print_block(block);
            block = block->next;
        }

        printf("}\n\n");
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

            case IR_OP_RETURN:
            {
                printf("RETURN ");
                if (instruction->arg1)
                {
                    ir_print_value(instruction->arg1);
                }
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
                printf("t(%lu)", value->temp.index);
                break;
            }

            case IRV_LITERAL:
            {
                printf("lit(%ld)", value->literal.s64);
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

            default: assert(false);
        }
    }
}