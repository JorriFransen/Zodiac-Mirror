#include "string_builder.h"

#include <inttypes.h>
#include <stdarg.h>

namespace Zodiac
{
    void string_builder_init(String_Builder* string_builder, uint64_t initial_bucket_size)
    {
        assert(string_builder);
        assert(string_builder->current_bucket == nullptr);

        uint64_t arena_block_size = initial_bucket_size;
        uint64_t string_builder_bucket_size = arena_block_size - sizeof(String_Builder_Bucket);

        string_builder->arena = arena_create(arena_block_size);
        string_builder->current_bucket = string_builder_create_bucket(string_builder,
                                                                      string_builder_bucket_size);
        string_builder->first_bucket = string_builder->current_bucket;
    }

    void string_builder_free(String_Builder* string_builder)
    {
        assert(string_builder);
        assert(string_builder->first_bucket);

        arena_free(&string_builder->arena);
    }

    String_Builder_Bucket* string_builder_create_bucket(String_Builder* string_builder, uint64_t bucket_cap)
    {
        assert(string_builder);
        assert(bucket_cap);

        uint8_t* bucket_mem = arena_alloc_array(&string_builder->arena, uint8_t,
                                                bucket_cap + sizeof(String_Builder_Bucket));

        String_Builder_Bucket* result = (String_Builder_Bucket*)bucket_mem;
        result->data = (char*)(bucket_mem + sizeof(String_Builder_Bucket));
        result->cursor = 0;
        result->capacity = bucket_cap;
        result->next_bucket = nullptr;

        return result;
    }

    void string_builder_append(String_Builder* string_builder, const char* string, uint64_t string_length)
    {
        assert(string_builder);
        assert(string);
        assert(string_length);

        string_builder_ensure_capacity(string_builder, string_length);
        string_builder_bucket_append(string_builder->current_bucket, string, string_length);
    }

    void string_builder_append(String_Builder* string_builder, const char* string)
    {
        string_builder_append(string_builder, string, strlen(string));
    }

    void string_builder_append(String_Builder* string_builder, const Atom& atom)
    {
        string_builder_append(string_builder, atom.data, atom.length);
    }

    void string_builder_append(String_Builder* string_builder, uint64_t integer)
    {
        assert(string_builder);

        char str_buf[128];
        auto sprf_res = sprintf(str_buf, "%" PRIu64, integer);
        assert(sprf_res);

        string_builder_append(string_builder, str_buf);
    }

    void string_builder_appendf(String_Builder* string_builder, const char* format, ...)
    {
        assert(string_builder);
        assert(format);

        va_list args;
        va_start(args, format);

        auto length = vsnprintf(nullptr, 0, format, args);
        va_end(args);
        va_start(args, format);

        char* str_buf = (char*)mem_alloc(length + 1);
        auto written = vsnprintf(str_buf, length + 1, format, args);
        assert(written == length);


        string_builder_append(string_builder, str_buf, length);

        mem_free(str_buf);
    }

    void string_builder_ensure_capacity(String_Builder* string_builder, uint64_t capacity)
    {
        assert(string_builder);
        assert(capacity);

        auto current_bucket = string_builder->current_bucket;
        assert(current_bucket);

        auto capacity_remaining = current_bucket->capacity - current_bucket->cursor;

        if (capacity_remaining < capacity)
        {
            uint64_t new_bucket_cap = current_bucket->capacity;
            while (new_bucket_cap < capacity) new_bucket_cap *= 2;

            auto new_bucket = string_builder_create_bucket(string_builder, new_bucket_cap);

            current_bucket->next_bucket = new_bucket;
            string_builder->current_bucket = new_bucket;
        }
    }

    void string_builder_bucket_append(String_Builder_Bucket* bucket, const char* string,
                                      uint64_t string_length)
    {
        assert(bucket);
        assert(string);
        assert(string_length);

        assert(bucket->capacity - bucket->cursor >= string_length);

        for (uint64_t i = 0; i < string_length; i++)
        {
            bucket->data[bucket->cursor++] = string[i];
        }
    }

    char* string_builder_to_string(String_Builder* string_builder)
    {
        assert(string_builder);

        uint64_t length = 0;

        String_Builder_Bucket* bucket = string_builder->first_bucket;
        while (bucket)
        {
            length += bucket->cursor;
            bucket = bucket->next_bucket;
        }

        if (!length)
        {
            return nullptr;
        }

        char* result = (char*)mem_alloc(length + 1);
		assert(result);
        uint64_t result_index = 0;


        bucket = string_builder->first_bucket;
        while (bucket)
        {
            for (uint64_t i = 0; i < bucket->cursor; i++)
            {
                result[result_index++] = bucket->data[i];
            }

            bucket = bucket->next_bucket;
        }

        result[result_index] = '\0';

        return result;
    }
}
