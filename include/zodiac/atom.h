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
        BUF(Atom) atoms = nullptr;
    };

    void atom_table_init(Atom_Table* atom_table);
    const Atom& atom_get(Atom_Table* atom_table, const char* string, uint64_t string_length);
    const Atom& atom_get(Atom_Table* atom_table, const char* string);

    uint64_t atom_to_u64(const Atom& atom);

    bool operator==(const Atom& lhs, const Atom& rhs);

}
