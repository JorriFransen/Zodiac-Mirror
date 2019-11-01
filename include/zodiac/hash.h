#pragma once

#include "common.h"

namespace Zodiac
{
    template <typename K>
    using Hash_Function = uint64_t (*)(const K&);

    template <typename K, typename V>
    struct Hash_Table
    {
        K* keys = nullptr;
        V* values = nullptr;

        uint64_t capacity = 0;
        uint64_t used_capacity = 0;

        K empty_key;

        Hash_Function<K> get_hash;
    };

    template <typename K, typename V>
    void hash_table_init(Hash_Table<K, V>* table, uint64_t capacity, K empty_key,
                         Hash_Function<K> hash_function)
    {
        assert(table);

        table->keys = (K*)mem_alloc(sizeof(K) * capacity);
		assert(table->keys);
        table->values = (V*)mem_alloc(sizeof(V) * capacity);
		assert(table->values);
        table->empty_key = empty_key;
        table->get_hash = hash_function;

        for (uint64_t i = 0; i < capacity; i++)
        {
            table->keys[i] = empty_key;
        }

        memset(table->values, 0, sizeof(V) * capacity);

        table->capacity = capacity;
        table->used_capacity = 0;
    }

    template <typename K, typename V>
    void hash_table_grow_if_needed(Hash_Table<K, V>* table)
    {
        if (table->used_capacity == 0)
        {
            return;
        }

        float filled_percent = (float)table->used_capacity / table->capacity;
        if (filled_percent >= 0.7)
        {
            uint64_t new_cap = table->capacity * 2;
            // printf("Growing hash table %p to %u\n", table, new_cap);
            // printf("\tfilled_percent: %f\n", filled_percent);
            K* new_keys = (K*)mem_alloc(sizeof(K) * new_cap);
            V* new_values = (V*)mem_alloc(sizeof(V) * new_cap);

            uint64_t old_cap = table->capacity;
            K* old_keys = table->keys;
            V* old_values = table->values;

            table->keys = new_keys;
            table->values = new_values;
            table->capacity = new_cap;

            for (uint64_t i = 0; i < old_cap; i++)
            {
                if (old_keys[i] != table->empty_key)
                {
                    _hash_table_push(table, old_keys[i], old_values[i]);
                }
            }
        }
    }

    template <typename K, typename V>
    void _hash_table_push(Hash_Table<K, V>* table, K new_key, V value)
    {
        uint64_t hash = table->get_hash(new_key);
        uint64_t hash_index = hash % table->capacity;
        uint64_t iterations = 0;

        while (iterations < table->capacity)
        {
            const K& key = table->keys[hash_index];

            if (key == table->empty_key)
            {
                table->used_capacity++;
                table->keys[hash_index] = new_key;
                table->values[hash_index] = value;
                return;
            }
            else if (key == new_key)
            {
                assert(false); // duplicate key
            }
            else
            {
                iterations++;
                hash_index++;
                if (hash_index >= table->capacity) hash_index = 0;
            }
        }

        assert(false); // There should have been a free slot
    }

    template <typename K, typename V>
    void hash_table_push(Hash_Table<K, V>* table, K new_key, V value)
    {
        hash_table_grow_if_needed(table);
        _hash_table_push(table, new_key, value);
    }

    template <typename K, typename V>
    V* hash_table_find_pointer(Hash_Table<K, V>* table, K key)
    {
        uint64_t hash = table->get_hash(key);
        uint64_t hash_index = hash % table->capacity;
        uint64_t iterations = 0;

        while (iterations < table->capacity)
        {
            if (table->keys[hash_index] == key)
            {
                return &table->values[hash_index];
            }

            iterations++;
            hash_index++;
            if (hash_index >= table->capacity) hash_index = 0;
        }

        return nullptr;
    }

    template <typename K, typename V>
    bool hash_table_find(Hash_Table<K, V>* table, K key, V* value_dest)
    {
        V* result = hash_table_find_pointer(table, key);
        if (result)
        {
            *value_dest = *result;
            return  true;
        }

        return false;
    }
}
