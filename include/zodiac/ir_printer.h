#pragma once

#include "ir.h"

namespace Zodiac
{
    void ir_print_function(IR_Function* function, String_Builder* string_builder);
    void ir_print_block(IR_Block* block, String_Builder* string_builder);
    void ir_print_instruction(IR_Instruction* instruction, String_Builder* string_builder);
    void ir_print_value(IR_Value* value, String_Builder* string_builder);
    void ir_print_string_literal(const char* string, String_Builder* string_builder);
	void ir_print_type(AST_Type* type, String_Builder* string_builder);
    void ir_print_character(char c, String_Builder* string_builder);
}
