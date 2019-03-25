#pragma once

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Common macros
#define KB(n) ((n) * 1024)
#define MB(n) (KB((n)) * 1024)
#define GB(n) (MB((n)) * 1024)

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

// Use these memory management macros for now, so we can add support for custom
//   allocators later.
#define mem_alloc(size) (calloc(1, (size)))
#define mem_free(ptr) (free((void*)(ptr)))

// Stretchy buffer
struct BUF_Header
{
    size_t capacity;
    size_t length;
};

template <class T>
T* _buf_grow(T* buf, size_t elem_size);

#define BUF(type) type*
#define _BUF_HDR(buf) ((BUF_Header*)((char*)(buf) - sizeof(BUF_Header)))

#define BUF_LENGTH(buf) ((buf) ? _BUF_HDR(buf)->length : 0)
#define BUF_CAPACITY(buf) ((buf) ? _BUF_HDR(buf)->capacity : 0)
#define BUF_END(buf) ((buf) + BUF_LENGTH(buf))

#define _BUF_FULL(buf) (BUF_LENGTH(buf) >= BUF_CAPACITY(buf))

#define BUF_PUSH(buf, elem) \
    (_BUF_FULL(buf) ? ((buf) = _buf_grow((buf), sizeof(*(buf)))) : 0), \
    ((buf)[_BUF_HDR(buf)->length++] = (elem)) \

#define BUF_FREE(buf) ((buf) ? mem_free(_BUF_HDR(buf)), (buf) = NULL : 0)

template <class T>
T* _buf_grow(T* buf, size_t elem_size)
{
    assert(BUF_CAPACITY(buf) <= (SIZE_MAX - 1) / 2);
    size_t new_cap = BUF_CAPACITY(buf) > 0 ? 2 * BUF_CAPACITY(buf) : 8;
    assert(new_cap <= (SIZE_MAX - sizeof(BUF_Header)) / elem_size);
    size_t new_size = sizeof(BUF_Header) + (new_cap * elem_size);

    BUF_Header* new_hdr;
    if (buf)
    {
        new_hdr = (BUF_Header*)realloc(_BUF_HDR(buf), new_size);
    }
    else
    {
        new_hdr = (BUF_Header*)mem_alloc(new_size);
        new_hdr->length = 0;
    }

    new_hdr->capacity = new_cap;
    return ((T*)(((char*)new_hdr) + sizeof(BUF_Header)));
}

// Memory arenas
typedef struct _Arena_Block
{
    void* data;
    uint64_t first_free_index;
    uint64_t data_length;

    struct _Arena_Block* next_block;
} Arena_Block;

typedef struct _Arena
{
    size_t default_block_size = 0;
    Arena_Block* blocks = nullptr;
} Arena;

Arena arena_create(size_t block_size);
void arena_free(Arena* arena);
void* _arena_alloc(Arena* arena, size_t size);
Arena_Block* _arena_alloc_new_block(Arena* arena, size_t min_block_size);
bool _arena_block_fits(Arena_Block* block, size_t size);
void* _arena_alloc_from_block(Arena_Block* block, size_t size);

#define ARENA_DEFAULT_BLOCK_SIZE (MB(1))

#define arena_alloc(arena, type) ((type*)_arena_alloc((arena), sizeof(type)))
#define arena_alloc_array(arena, type, length) ((type*)_arena_alloc((arena), sizeof(type) * (length)))

// Stack
template <typename T>
struct Stack
{
    T* data = nullptr;
    uint64_t sp = 0;
    uint64_t capacity = 0;
};

template <typename T>
void stack_init(Stack<T>* stack, uint64_t capacity)
{
    assert(stack);
    assert(capacity > 0);

    assert(stack->data == nullptr);
    assert(stack->sp == 0);
    assert(stack->capacity == 0);

    stack->data = (T*)mem_alloc(capacity * sizeof(T));
    stack->capacity = capacity;
}

template <typename T>
void stack_free(Stack<T>* stack)
{
    assert(stack);
    assert(stack->data);
    assert(stack->capacity);

    mem_free(stack->data);
    stack->sp = 0;
    stack->capacity = 0;
}

template <typename T>
void stack_clear(Stack<T>& stack)
{
    assert(stack.data);
    assert(stack.capacity > 0);
    assert(stack.sp <= stack.capacity);

    stack.sp = 0;
}

template <typename T>
void stack_grow(Stack<T>& stack, uint64_t new_capacity)
{
    assert(new_capacity > stack.capacity);

    T* new_data = (T*)mem_alloc(new_capacity * sizeof(T));
    memcpy(new_data, stack.data, stack.capacity);
    mem_free(stack.data);
    stack.data = new_data;
    stack.capacity = new_capacity;

}

template <typename T>
void stack_push(Stack<T>& stack, T elem)
{
    if (stack.sp >= stack.capacity)
    {
        stack_grow(stack, stack.capacity * 2);
    }

    stack.data[stack.sp] = elem;
    stack.sp++;
}

template <typename T>
T stack_pop(Stack<T>& stack)
{
    assert(stack.data);
    assert(stack.sp);
    assert(stack.sp <= stack.capacity);

    stack.sp--;
    return stack.data[stack.sp];
}

template <typename T>
T stack_top(Stack<T>& stack)
{
    assert(stack.data);
    assert(stack.sp);
    assert(stack.sp <= stack.capacity);

    return stack.data[stack.sp - 1];
}

template <typename T>
T stack_peek(Stack<T>& stack, uint64_t offset = 0)
{
    assert(stack.data);
    assert(stack.sp);

    return stack.data[(stack.sp - 1) - offset];
}

template <typename T>
void stack_copy(Stack<T>& dest, Stack<T>& source)
{
    assert(dest.data == nullptr);
    assert(dest.sp == 0);
    assert(dest.capacity == 0);

    assert(source.data != nullptr);
    assert(source.sp > 0);
    assert(source.capacity >= source.sp);

    auto size_in_bytes = source.capacity * sizeof(T);
    dest.data = (T*)mem_alloc(size_in_bytes);
    memcpy(dest.data, source.data, size_in_bytes);
    dest.capacity = source.capacity;
    dest.sp = source.sp;
}

template <typename T>
uint64_t stack_count(Stack<T>& stack)
{
    return stack.sp;
}

// File IO
bool path_exists(const char* path);
bool file_exists(const char* file_path);
bool dir_exists(const char* dir_path);
bool file_is_child_of_directory(const char* file_name, const char* dir_name);
const char* read_file_string(const char* file_path);

void fatal(const char* format, ...);
void fatal(const char* format, va_list args);

// Strings
const char* string_append(const char* str_a, const char* str_b);
bool string_contains(const char* str, char c);

// Paths
const char* extract_directory_from_path(const char* path);
const char* extract_file_name_from_path(const char* path);
const char* full_path_from_cwd(const char* rel_path);
const char* normalize_path(const char* path);
