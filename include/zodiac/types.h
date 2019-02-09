#pragma once

#include "zodiac.h"
#include "ast.h"

namespace Zodiac
{
    AST_Type* find_type(Context* context, AST_Module* module, AST_Identifier* identifier);
}
