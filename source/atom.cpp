#include "atom.h"

#include <cassert>
#include <string.h>

namespace Zodiac
{
    void atom_table_init(Atom_Table* atom_table)
    {
        assert(atom_table);

        atom_table->atoms = nullptr;
        atom_table->string_arena = arena_create(MB(1));
    }

    const Atom& atom_get(Atom_Table* atom_table, const char* string, uint64_t string_length)
    {
        assert(atom_table);
        assert(string);
        assert(string_length > 0);

        for (uint64_t i = 0; i < BUF_LENGTH(atom_table->atoms); i++)
        {
            const Atom& atom = atom_table->atoms[i];
            if (string_length == atom.length &&
                strncmp(string, atom.data, string_length) == 0)
            {
                return atom;
            }
        }

        Atom result = {};
        result.data = arena_alloc_array(&atom_table->string_arena, char, string_length + 1);
        memcpy((void*)result.data, string, string_length);
        result.data[result.length - 1] = '\0';
        result.length = string_length;

        BUF_PUSH(atom_table->atoms, result);

        return atom_table->atoms[BUF_LENGTH(atom_table->atoms) - 1];
    }

    const Atom& atom_get(Atom_Table* atom_table, const char* string)
    {
        return atom_get(atom_table, string, strlen(string));
    }

    uint64_t atom_to_u64(const Atom& atom)
    {
        uint64_t base = 10;
        uint64_t result = 0;

        for (uint64_t i = 0; i < atom.length; i++)
        {
            result *= base;
            uint64_t digit_value = atom.data[i] - '0';
            result += digit_value;
        }

        return result;
    }

    bool operator==(const Atom& lhs, const Atom& rhs)
    {
        return lhs.length == rhs.length && lhs.data == rhs.data;
    }
}
