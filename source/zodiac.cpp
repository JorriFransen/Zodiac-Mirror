#include "zodiac.h"

#include "builtin.h"

namespace Zodiac
{
    void context_init(Context* context, Arena* arena)
    {
        assert(context);
        assert(arena);

        context->arena = arena;

        context->atom_table = (Atom_Table*)mem_alloc(sizeof(Atom_Table));
        atom_table_init(context->atom_table);

        context->keywords =  nullptr;
        context_init_keywords(context);

        init_builtin_types(context);
        init_builtin_decls(context);
    }

#define DEFINE_KW(string, kw_kind) \
    { \
        Atom atom_for_string = atom_get(context->atom_table, string); \
        Registered_Keyword entry = { atom_for_string, kw_kind }; \
        BUF_PUSH(context->keywords, entry); \
    }

    static void context_init_keywords(Context* context)
    {
        assert(context);

        DEFINE_KW("true", TOK_KW_TRUE);
        DEFINE_KW("false", TOK_KW_FALSE);
        DEFINE_KW("return", TOK_KW_RETURN);
        DEFINE_KW("static_if", TOK_KW_STATIC_IF);
        DEFINE_KW("if", TOK_KW_IF);
        DEFINE_KW("else", TOK_KW_ELSE);
        DEFINE_KW("foreign", TOK_KW_FOREIGN);
        DEFINE_KW("dynamic_link", TOK_KW_DYN_LINK);
        DEFINE_KW("while", TOK_KW_WHILE);
        DEFINE_KW("for", TOK_KW_FOR);
        DEFINE_KW("array_length", TOK_KW_ARRAY_LENGTH);
        DEFINE_KW("static_assert", TOK_KW_STATIC_ASSERT);
    }

#undef DEFINE_KW
}
