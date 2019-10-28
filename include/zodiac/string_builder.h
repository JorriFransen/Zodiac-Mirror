#pragma once

#include "common.h"
#include "atom.h"

namespace Zodiac
{
    struct String_Builder_Bucket;
    struct String_Builder
    {
        Arena arena;
        String_Builder_Bucket* first_bucket = nullptr;
        String_Builder_Bucket* current_bucket = nullptr;
    };

    struct String_Builder_Bucket
    {
        char* data;
        uint64_t cursor = 0;
        uint64_t capacity = 0;
        String_Builder_Bucket* next_bucket = nullptr;
    };

    void string_builder_init(String_Builder* string_builder, uint64_t initial_bucket_size);
    void string_builder_free(String_Builder* string_builder);
    String_Builder_Bucket* string_builder_create_bucket(String_Builder* string_builder, uint64_t bucket_cap);

    void string_builder_append(String_Builder* string_builder, const char* string, uint64_t string_length);
    void string_builder_append(String_Builder* string_builder, const char* string);
    void string_builder_append(String_Builder* string_builder, const Atom& atom);
    void string_builder_append(String_Builder* string_builder, uint64_t integer);

    void string_builder_appendf(String_Builder* string_builder, const char* format, ...);

    void string_builder_ensure_capacity(String_Builder* string_builder, uint64_t capacity);

    void string_builder_bucket_append(String_Builder_Bucket* bucket, const char* string,
                                      uint64_t string_length);

    char* string_builder_to_string(String_Builder* string_builder);

}
