#pragma once

#include "ast.h"
#include "zodiac.h"

namespace Zodiac
{
    enum Copy_Flag
    {
        COPY_FLAG_NONE           = 0,
        COPY_FLAG_DONT_COPY_POLY = (1 << 0),
    };

    AST_Declaration* copy_declaration(Context* context, AST_Declaration* declaration,
                                      Copy_Flag flags = COPY_FLAG_NONE);
    AST_Statement* copy_statement(Context* context, AST_Statement* statement,
                                  AST_Scope* parent_scope, Copy_Flag flags = COPY_FLAG_NONE);
    AST_Expression* copy_expression(Context* context, AST_Expression* expression,
                                    Copy_Flag flags = COPY_FLAG_NONE);
    AST_Type_Spec* copy_type_spec(Context* context, AST_Type_Spec* type_spec,
                                Copy_Flag flags = COPY_FLAG_NONE);
    AST_Identifier* copy_identifier(Context* context, AST_Identifier* identifier,
                                    Copy_Flag flags = COPY_FLAG_NONE);
    AST_Overload_Directive copy_overload_directive(Context* context, AST_Overload_Directive od,
                                                Copy_Flag flags = COPY_FLAG_NONE);
    AST_Scope* copy_scope(Context* context, AST_Scope* scope,
                        Copy_Flag flags = COPY_FLAG_NONE);
}
