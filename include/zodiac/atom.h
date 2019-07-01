#pragma once

#include "common.h"

namespace Zodiac
{
    struct Atom
    {
        const char* data = nullptr;
        uint64_t length = 0;
    };

    struct Atom_Table
    {
        Arena string_arena;
		Atom* atoms = nullptr;
		uint64_t atom_count = 0;
    };

	void atom_test();

    void atom_table_init(Atom_Table* atom_table);
    const Atom& atom_get(Atom_Table* atom_table, const char* string, uint64_t string_length,
		                 bool copy_string = true);
    const Atom& atom_get(Atom_Table* atom_table, const char* string);

	static void atom_table_grow(Atom_Table* atom_table);

    const Atom& atom_append(Atom_Table* atom_table, const Atom& lhs, const char* rhs,
                            uint64_t rhs_lenght);
    const Atom& atom_append(Atom_Table* atom_table, const Atom& lhs, const char* rhs);
    const Atom& atom_append(Atom_Table* atom_table, const Atom& lhs, const Atom& rhs);

    const Atom& atom_append(Atom_Table* atom_table, const Atom& lhs, uint64_t u64);
    const Atom& atom_append(Atom_Table* atom_table, const char* lhs, uint64_t u64);

    uint64_t atom_to_u64(const Atom& atom, uint64_t base = 10);
    double atom_to_double(const Atom& atom);
    float atom_to_float(const Atom& atom);

	static uint64_t get_atom_hash(const char* string, uint64_t string_length);

    bool operator==(const Atom& lhs, const Atom& rhs);
    bool operator!=(const Atom& lhs, const Atom& rhs);

}
