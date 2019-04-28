#pragma once

#include "common.h"

namespace Zodiac
{
    struct Atom
    {
        char* data = nullptr;
        uint64_t length = 0;
    };

    struct Atom_Table
    {
        Arena string_arena;
        BUF(Atom) atoms = nullptr;
    };

    void atom_table_init(Atom_Table* atom_table);
    const Atom& atom_get(Atom_Table* atom_table, const char* string, uint64_t string_length);
    const Atom& atom_get(Atom_Table* atom_table, const char* string);
    const Atom& atom_append(Atom_Table* atom_table, const Atom& lhs, const char* rhs,
                            uint64_t rhs_lenght);
    const Atom& atom_append(Atom_Table* atom_table, const Atom& lhs, const char* rhs);
    const Atom& atom_append(Atom_Table* atom_table, const Atom& lhs, const Atom& rhs);

    uint64_t atom_to_u64(const Atom& atom);
    double atom_to_double(const Atom& atom);

    bool operator==(const Atom& lhs, const Atom& rhs);
    bool operator!=(const Atom& lhs, const Atom& rhs);

}
