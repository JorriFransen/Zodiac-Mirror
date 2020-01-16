#include "ir_printer.h"

#include "builtin.h"

#include <inttypes.h>

namespace Zodiac
{
    void ir_print_function(IR_Function* function, String_Builder* sb)
    {
        assert(function);
        assert(sb);

        bool is_foreign = function->flags & IR_FUNC_FLAG_FOREIGN;

        if (is_foreign)
        {
            string_builder_append(sb, "#foreign ");
        }

        string_builder_append(sb, function->name);
        string_builder_append(sb, "(");
        bool is_sret = false;
        for (uint64_t i = 0; i < BUF_LENGTH(function->arguments); i++)
        {
            if (i == 0)
            {
                if (function->arguments[i]->flags & IRV_FLAG_SRET_ARG)
                {
                    is_sret = true;
                }
            }
            else if (i > 0)
            {
                string_builder_append(sb, ", ");
            }

            ir_print_value(function->arguments[i], sb);
            string_builder_append(sb, ":(");
            ir_print_type(function->arguments[i]->type, sb);
            string_builder_append(sb, ")");
        }
        string_builder_append(sb, ") -> ");
        if (is_sret)
        {
            string_builder_append(sb, "void");
        }
        else
        {
            ir_print_type(function->type->function.return_type, sb);
        }
        string_builder_append(sb, "\n");

        if (!is_foreign)
        {
            string_builder_append(sb, "{\n");

            IR_Block* block = function->first_block;

            while (block)
            {
                ir_print_block(block, sb);
                block = block->next;
            }

            string_builder_append(sb, "}\n");
        }

        string_builder_append(sb, "\n");
    }

    void ir_print_block(IR_Block* block, String_Builder* sb)
    {
        assert(block);
        assert(sb);

        string_builder_appendf(sb, "\t%s:\n", block->name.data);

        IR_Instruction* instruction = block->first_instruction;

        while (instruction)
        {
            ir_print_instruction(instruction, sb);
            instruction = instruction->next;
        }
    }

    void ir_print_instruction(IR_Instruction* instruction, String_Builder* sb)
    {
        assert(instruction);
        assert(sb);

        string_builder_append(sb, "\t\t");

        if (instruction->result && instruction->op != IR_OP_SWITCH)
        {
            ir_print_value(instruction->result, sb);
            string_builder_append(sb, ":(");
            ir_print_type(instruction->result->type, sb);
            string_builder_append(sb, ") = ");
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
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " + ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_SUB:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " - ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_MUL:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " * ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_MOD:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " %% ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_DIV:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " / ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_LSHIFT:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " << ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_RSHIFT:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " << ");
                ir_print_value(instruction->arg2, sb);
            }

            case IR_OP_LT:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " < ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_LTEQ:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " <= ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_GT:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " > ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_GTEQ:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " >= ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_EQ:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " == ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_NEQ:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " != ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_AND:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " & ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_OR:
            {
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " | ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_NOT:
            {
                string_builder_append(sb, "!");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_RETURN:
            {
                string_builder_append(sb, "RETURN ");
                if (instruction->arg1)
                {
                    ir_print_value(instruction->arg1, sb);
                }
                break;
            }

            case IR_OP_PUSH_CALL_ARG:
            {
                string_builder_append(sb, "PUSH_ARG ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_CALL:
            {
                string_builder_append(sb, "CALL ");
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, ", ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_CALL_EX:
            {
                string_builder_append(sb, "CALL_EX ");
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, ", ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

			case IR_OP_ADDROF_FOREIGN:
			{
				string_builder_append(sb, "ADDROF_FOREIGN ");
				ir_print_value(instruction->arg1, sb);
				break;
			}

            case IR_OP_ADDROF_FUNCTION:
            {
                string_builder_append(sb, "ADDROF_FUNCTION ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

			case IR_OP_CALL_PTR:
			{
				string_builder_append(sb, "CALL_PTR ");
				ir_print_value(instruction->arg1, sb);
				string_builder_append(sb, ", ");
				ir_print_value(instruction->arg2, sb);
				break;
			}

            case IR_OP_JMP:
            {
                string_builder_append(sb, "JMP ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_JMP_IF:
            {
                string_builder_append(sb, "IF ");
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " JMP ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_SWITCH:
            {
                string_builder_append(sb, "SWITCH ");
                ir_print_value(instruction->arg1, sb);
                if (instruction->arg2)
                {
                    string_builder_append(sb, ", ");
                    ir_print_value(instruction->arg2, sb);
                }
                if (instruction->result)
                {
                    string_builder_append(sb, ", (");
                    for (uint64_t i = 0; i < BUF_LENGTH(instruction->case_pairs); i++)
                    {
                        auto cp = instruction->case_pairs[i];
                        if (i > 0)
                        {
                            string_builder_append(sb, ", ");
                        }
                        string_builder_append(sb, "<");
                        string_builder_append(sb, cp.dest_block_value->block->name.data,
                                              cp.dest_block_value->block->name.length);
                        string_builder_append(sb, ", ");
                        ir_print_value(cp.value, sb);
                        string_builder_append(sb, ">");

                    }
                    string_builder_append(sb, ")");
                }
                break;
            }

            case IR_OP_ALLOCL:
            {
                string_builder_append(sb, "ALLOCL ");
                break;
            }

            case IR_OP_STOREL:
            {
                string_builder_append(sb, "STOREL ");
                ir_print_value(instruction->arg2, sb);
                string_builder_append(sb, " INTO ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_LOADL:
            {
                string_builder_append(sb, "LOADL ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_STOREA:
            {
                string_builder_append(sb, "STOREA ");
                ir_print_value(instruction->arg2, sb);
                string_builder_append(sb, " INTO ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_LOADA:
            {
                string_builder_append(sb, "LOADA ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_STOREP:
            {
                string_builder_append(sb, "STOREP ");
                ir_print_value(instruction->arg2, sb);
                string_builder_append(sb, " INTO ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_LOADP:
            {
                string_builder_append(sb, "LOADP ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_STOREG:
            {
                string_builder_append(sb, "STOREG ");
                ir_print_value(instruction->arg2, sb);
                string_builder_append(sb, " INTO ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_LOADG:
            {
                string_builder_append(sb, "LOADG ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_ADDROF:
            {
                string_builder_append(sb, "ADDROF ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_DEREF:
            {
                string_builder_append(sb, "DEREF ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_ARRAY_OFFSET_POINTER:
            {
                string_builder_append(sb, "ARRAY_OFFSET_POINTER ");
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_AGGREGATE_OFFSET_POINTER:
            {
                string_builder_append(sb, "AGGREGATE_OFFSET_POINTER ");
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, " ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

			case IR_OP_CAST:
			{
				string_builder_append(sb, "CAST ");
				ir_print_value(instruction->arg1, sb);
				string_builder_append(sb, " TO ");
				ir_print_type(instruction->result->type, sb);
				break;
			}

            case IR_OP_ASSERT_FAIL:
            {
                string_builder_append(sb, "ASSERT_FAIL ");
                break;
            }

            case IR_OP_CREATE_THREAD:
            {
                string_builder_append(sb, "CREATE_THREAD ");
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, ", ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_JOIN_THREAD:
            {
                string_builder_append(sb, "JOIN_THREAD ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_COMPARE_AND_SWAP:
            {
                string_builder_append(sb, "COMPARE_AND_SWAP ");
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, ", ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_PHI:
            {
                string_builder_append(sb, "PHI ");
                for (uint64_t i = 0; i < BUF_LENGTH(instruction->phi_pairs); i++)
                {
                    auto pair = instruction->phi_pairs[i];
                    string_builder_appendf(sb, "[%s, ", pair.from_block->name);
                    ir_print_value(pair.value_to_load, sb);
                    string_builder_append(sb, "]");

                    if (i < BUF_LENGTH(instruction->phi_pairs) - 1)
                    {
                        string_builder_append(sb, ", ");
                    }
                }
                break;
            }

            case IR_OP_GET_TYPE_INFO:
            {
                string_builder_append(sb, "GET_TYPE_INFO ");
                ir_print_value(instruction->arg1, sb);
                break;
            }

            case IR_OP_GET_TYPE_INFO_BASE_PTR:
            {
                string_builder_append(sb, "GET_TYPE_INFO_BASE_PTR");
                break;
            }

            case IR_OP_EXTRACT_VALUE:
            {
                string_builder_append(sb, "EXTRACT_VALUE ");
                ir_print_value(instruction->arg1, sb);
                string_builder_append(sb, ", ");
                ir_print_value(instruction->arg2, sb);
                break;
            }

            case IR_OP_SCOPE_ENTRY:
            {
                string_builder_append(sb, "SCOPE_ENTRY ");
                break;
            }

            case IR_OP_SCOPE_EXIT:
            {
                string_builder_append(sb, "SCOPE_EXIT ");
                break;
            }

            default: assert(false);

        }

        string_builder_append(sb, "\n");
    }

    void ir_print_value(IR_Value* value, String_Builder* sb)
    {
        assert(value);
        assert(sb);

        switch (value->kind)
        {
            case IRV_TEMPORARY:
            {
                string_builder_appendf(sb, "t(%" PRIu64 ")", value->temp.index);
                break;
            }

            case IRV_BOOL_LITERAL:
            {
                string_builder_appendf(sb, "lit(%s)", value->value.boolean ? "true" : "false");
                break;
            }

			case IRV_NULL_LITERAL:
			{
				string_builder_append(sb, "lit(null)");
				break;
			}

            case IRV_STRING_LITERAL:
            {
                ir_print_string_literal((const char*)value->value.pointer, sb);
                break;
            }

            case IRV_INT_LITERAL:
            {
                string_builder_appendf(sb, "lit(%" PRId64 "):", value->value.s64);
                ir_print_type(value->type, sb);
                break;
            }

            case IRV_FLOAT_LITERAL:
            {
                if (value->type == Builtin::type_double)
                {
                    string_builder_appendf(sb, "lit(%f):", value->value.r64);
                }
                else if (value->type == Builtin::type_float)
                {
                    string_builder_appendf(sb, "lit(%f):", value->value.r32);
                }
                else assert(false);
                ir_print_type(value->type, sb);
                break;
            }

            case IRV_CHAR_LITERAL:
            {
                string_builder_append(sb, "lit('");
                ir_print_character(value->value.u8, sb);
                string_builder_append(sb, "')");
                break;
            }

            case IRV_AGGREGATE_LITERAL:
            case IRV_ARRAY_LITERAL:
            {
                string_builder_append(sb, "{ ");
                for (uint64_t i = 0; i < BUF_LENGTH(value->value.compound_values); i++)
                {
                    if (i > 0)
                    {
                        string_builder_append(sb, ", ");
                    }
                    IR_Value* compound_member = value->value.compound_values[i];
                    ir_print_value(compound_member, sb);
                }
                string_builder_append(sb, " }");
                break;
            }

            case IRV_ARGUMENT:
            {
                string_builder_appendf(sb, "arg(%s)", value->argument.name);
                break;
            }

            case IRV_FUNCTION:
            {
                string_builder_appendf(sb, "func(%s)", value->function->name);
                break;
            }

            case IRV_BLOCK:
            {
                string_builder_appendf(sb, "block(%s)", value->block->name.data);
                break;
            }

            case IRV_ALLOCL:
            {
                string_builder_appendf(sb, "allocl(%s)", value->allocl.name);
                break;
            }

            case IRV_GLOBAL:
            {
                string_builder_appendf(sb, "global(%s)", value->global.name);
                break;
            }

            default: assert(false);
        }
    }

    void ir_print_string_literal(const char* string, String_Builder* sb)
    {
        assert(string);
        assert(sb);

        string_builder_append(sb, "lit(\"");

        auto string_length = strlen(string);

        for (uint64_t i = 0; i < string_length; i++)
        {
            ir_print_character(string[i], sb);
        }

        string_builder_append(sb, "\")");
    }

	void ir_print_type(AST_Type* type, String_Builder* sb)
	{
		assert(type);
        assert(sb);

        ast_type_to_string(type, sb);
	}

    void ir_print_character(char c, String_Builder* sb)
    {
        assert(sb);

        switch (c)
        {
            case '\n':
            {
                string_builder_append(sb, "\\n");
                break;
            }

            case '\033':
            {
                string_builder_append(sb, "\\033");
                break;
            }

            case '\0':
            {
                string_builder_append(sb, "\\0");
                break;
            }

            default:
            {
                string_builder_appendf(sb, "%c", c);
                break;
            }
        }
    }
}
