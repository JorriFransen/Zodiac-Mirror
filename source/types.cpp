#include "types.h"

namespace Zodiac
{
    AST_Type* find_type(Context* context, AST_Module* module, AST_Identifier* identifier)
    {
        assert(context);
        assert(module);
        assert(identifier);

        for (uint64_t i = 0; i < BUF_LENGTH(context->global_type_table); i++)
        {
            AST_Declaration* global_type_decl = context->global_type_table[i];
            assert(global_type_decl->kind == AST_DECL_TYPE);
            assert(global_type_decl->type.type);
            AST_Type* global_type = global_type_decl->type.type;
            if (global_type_decl->identifier &&
                global_type_decl->identifier->atom == identifier->atom)
            {
                return global_type;
            }
        }

        return nullptr;
    }
}
