#include "atom.h"

#include <cassert>
#include <inttypes.h>
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

    const Atom& atom_append(Atom_Table* atom_table, const Atom& lhs, const char* rhs, uint64_t rhs_length)
    {
        assert(atom_table);
        assert(rhs);

        uint64_t new_length = lhs.length + rhs_length;
        char* temp_result = (char*)mem_alloc(new_length * sizeof(char) + 1);
        memcpy(temp_result, lhs.data, lhs.length);
        memcpy(temp_result + lhs.length, rhs, rhs_length);
        temp_result[new_length] = '\0';

        const Atom& result = atom_get(atom_table, temp_result);
        mem_free(temp_result);
        return result;
    }

    const Atom& atom_append(Atom_Table* atom_table, const Atom& lhs, const char* rhs)
    {
        return atom_append(atom_table, lhs, rhs, strlen(rhs));
    }

    const Atom& atom_append(Atom_Table* atom_table, const Atom& lhs, const Atom& rhs)
    {
        return atom_append(atom_table, lhs, rhs.data, rhs.length);
    }

    const Atom& atom_append(Atom_Table* atom_table, const Atom& lhs, uint64_t u64)
    {
        assert(atom_table);

        uint64_t new_length = snprintf(nullptr, 0, "%s%" PRIu64, lhs.data, u64);
        char* tmp_result = (char*)mem_alloc(new_length + 1);
        snprintf(tmp_result, new_length + 1, "%s%" PRIu64, lhs.data, u64);

        const Atom& result = atom_get(atom_table, tmp_result);
        mem_free(tmp_result);
        return result;
    }

    static uint64_t _digit_value(char c)
    {
        if (c >= '0' && c <= '9')
        {
            return c - '0';
        }
        else if (c >= 'a' && c <= 'f')
        {
            return c - 'a' + 10;
        }
        else if (c >= 'A' && c <= 'F')
            return c - 'A' + 10;
        else
        {
            assert(false);
        }
    }

    uint64_t atom_to_u64(const Atom& atom, uint64_t base/* = 10 */)
    {
        uint64_t result = 0;

        for (uint64_t i = 0; i < atom.length; i++)
        {
            result *= base;
            uint64_t digit_value = atom.data[i] - '0';
            result += digit_value;
        }

        return result;
    }

    double atom_to_double(const Atom& atom)
    {
        assert(atom.data);
        assert(atom.length > 0);

        char* end_ptr;
        double result = strtod(atom.data, &end_ptr);
        if (result == 0.0 && end_ptr != atom.data + (atom.length))
        {
            assert(false);
        }

        return result; 
    }

    bool operator==(const Atom& lhs, const Atom& rhs)
    {
        return lhs.length == rhs.length && lhs.data == rhs.data;
    }

    bool operator!=(const Atom& lhs, const Atom& rhs)
    {
        return !operator==(lhs, rhs);
    }

}
