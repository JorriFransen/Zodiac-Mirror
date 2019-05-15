#include "atom.h"

#include <cassert>
#include <inttypes.h>
#include <string.h>

namespace Zodiac
{
	void atom_test()
	{
		Atom_Table at;
		atom_table_init(&at);

		Atom test1 = atom_get(&at, "test");
		Atom test2 = atom_get(&at, "test");
	}

    void atom_table_init(Atom_Table* atom_table)
    {
        assert(atom_table);

        atom_table->atoms = nullptr;
        atom_table->string_arena = arena_create(MB(1));

		atom_table->atoms = (Atom*)mem_alloc(sizeof(Atom) * 1024);
		atom_table->atom_count = 1024;
    }

    const Atom& atom_get(Atom_Table* atom_table, const char* string, uint64_t string_length,
		bool copy_string/*=true*/)
    {
        assert(atom_table);
        assert(string);
        assert(string_length > 0);

		uint64_t hash = get_atom_hash(string, string_length);
		uint64_t hash_index = hash % atom_table->atom_count;

		uint64_t index = hash_index;
		uint64_t iterations = 0;
		while (iterations < atom_table->atom_count)
		{
			Atom* atom = &atom_table->atoms[index];
			if (atom->data)
			{
				if (atom->length == string_length &&
					strncmp(atom->data, string, string_length) == 0)
				{
					// Match
					return *atom;
				}
				// Collision
			}
			else
			{
				// Free slot
				if (copy_string)
				{
					atom->data = arena_alloc_array(&atom_table->string_arena, char, string_length + 1);
					memcpy((void*)atom->data, string, string_length);
					((char*)atom->data)[string_length] = '\0';
				}
				else
				{
					atom->data = string;
				}
				atom->length = string_length;
				return *atom;
			}

			iterations++;
			index++;
			if (atom_table->atom_count <= index)
			{
				index = 0;
			}
		}

		atom_table_grow(atom_table);
		return atom_get(atom_table, string, string_length);
    }

    const Atom& atom_get(Atom_Table* atom_table, const char* string)
    {
        return atom_get(atom_table, string, strlen(string));
    }

	static void atom_table_grow(Atom_Table* atom_table)
	{
		assert(atom_table);
		assert(atom_table->atoms);
		assert(atom_table->atom_count);

		uint64_t new_count = atom_table->atom_count * 2;
		Atom* old_atoms = atom_table->atoms;
		uint64_t old_atom_count = atom_table->atom_count;
		Atom* new_atoms = (Atom*)mem_alloc(sizeof(Atom) * new_count);

		atom_table->atoms = new_atoms;
		atom_table->atom_count = new_count;

		for (uint64_t i = 0; i < old_atom_count; i++)
		{
			Atom& old_atom = old_atoms[i];
			atom_get(atom_table, old_atom.data, old_atom.length, false);
		}

		mem_free(old_atoms);
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
            return ((uint64_t)c - '0');
        }
        else if (c >= 'a' && c <= 'f')
        {
            return ((uint64_t)c - 'a') + 10;
        }
        else if (c >= 'A' && c <= 'F')
            return ((uint64_t)c - 'A') + 10;
        else
        {
            assert(false);
        }

		assert(false);
		return 0;
    }

    uint64_t atom_to_u64(const Atom& atom, uint64_t base/* = 10 */)
    {
        uint64_t result = 0;

        for (uint64_t i = 0; i < atom.length; i++)
        {
            result *= base;
            uint64_t digit_value = _digit_value(atom.data[i]);
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

    float atom_to_float(const Atom& atom)
    {
        assert(atom.data);
        assert(atom.length > 0);

        char* end_ptr;
        float result = strtof(atom.data, &end_ptr);
        if (result == 0.0 && end_ptr != atom.data + (atom.length))
        {
            assert(false);
        }

        return result;
    }

	static uint64_t get_atom_hash(const char* string, uint64_t string_length)
	{
		assert(string);
		assert(string_length > 0);

		uint64_t result = 55943;

		for (uint64_t i = 0; i < string_length; i++)
		{
			result = result * string[i] * 7537;
		}

		return result;
	}

    bool operator==(const Atom& lhs, const Atom& rhs)
    {
        return lhs.data == rhs.data;
    }

    bool operator!=(const Atom& lhs, const Atom& rhs)
    {
        return !operator==(lhs, rhs);
    }

}
