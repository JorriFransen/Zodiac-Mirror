#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif // !_CRT_SECURE_NO_WARNINGS

#include "common.h"

#include <assert.h>
#include <string.h>
#include <stdarg.h>

#ifdef WIN32
    #include <windows.h>
    #include <io.h>
    #include <direct.h>
    #include "Shlwapi.h"

    //#define F_OK 0
#else
    #include <sys/types.h>
    #include <sys/stat.h>
    #include <unistd.h>
    #include <limits.h>

    #define MAX_PATH PATH_MAX
#endif

Arena arena_create(size_t block_size)
{
    Arena result;
    result.default_block_size = block_size;
    result.blocks = _arena_alloc_new_block(&result, block_size);
    return result;
}

void arena_free(Arena* arena)
{
    assert(arena);

    Arena_Block* block = arena->blocks;
    while (block)
    {
        Arena_Block* next_block = block->next_block;

        assert(block->data_length == 0);
        mem_free(block->data);

        mem_free(block);
    }

    arena->blocks = NULL;
}

void* _arena_alloc(Arena* arena, size_t size)
{
    assert(arena);

    if (arena->blocks == NULL ||
        _arena_block_fits(arena->blocks, size)  == false)
    {
        Arena_Block* new_block = _arena_alloc_new_block(arena, size);

        new_block->next_block = arena->blocks;
        arena->blocks = new_block;

        assert(_arena_block_fits(arena->blocks, size));
    }

    return _arena_alloc_from_block(arena->blocks, size);
}

Arena_Block* _arena_alloc_new_block(Arena* arena, size_t min_block_size)
{
    assert(arena);

    size_t byte_size = MAX(ARENA_DEFAULT_BLOCK_SIZE, min_block_size);

    Arena_Block* block = (Arena_Block*)mem_alloc(sizeof(Arena_Block));
    block->first_free_index = 0;
    block->data_length = byte_size / sizeof(void*);
    if ((byte_size % sizeof(void*)) != 0)
    {
        block->data_length++;
    }
    block->data = mem_alloc(block->data_length * sizeof(void*));
    block->next_block = NULL;

    assert((block->data_length * sizeof(void*)) >= min_block_size);

    return block;
}

bool _arena_block_fits(Arena_Block* block, size_t size)
{
    assert(block);

    size_t free_length = block->data_length - block->first_free_index;
    return free_length * sizeof(void*) >= size;
}

void* _arena_alloc_from_block(Arena_Block* block, size_t size)
{
    assert(block);

    void* result = (void*)((uintptr_t*)block->data + block->first_free_index);
    size_t p_size = size / sizeof(void*);
    if (size % sizeof(void*) != 0)
    {
        p_size++;
    }
    block->first_free_index += p_size;

    return result;
}

bool path_exists(const char* path)
{
    if (file_exists(path))
    {
        return true;
    }
    else if (dir_exists(path))
    {
        return true;
    }

    return false;
}

bool file_exists(const char* file_path)
{
#ifdef WIN32

    auto result = GetFileAttributesA(file_path);
    if (result == INVALID_FILE_ATTRIBUTES)
    {
        return false;
    }

    return !(result & FILE_ATTRIBUTE_DIRECTORY);
#else

    struct stat statbuf;
    if (stat(file_path, &statbuf) != 0)
    {
        return false;
    }
    return S_ISREG(statbuf.st_mode);

#endif

    assert(false);
}

bool dir_exists(const char* dir_path)
{
    assert(dir_path);

#ifdef WIN32

    auto result = GetFileAttributesA(dir_path);
    if (result == INVALID_FILE_ATTRIBUTES)
    {
        return false;
    }
    return result & FILE_ATTRIBUTE_DIRECTORY;
#else // Linux

    struct stat statbuf;
    if (stat(dir_path, &statbuf) != 0)
    {
        return false;
    }
    return S_ISDIR(statbuf.st_mode);

#endif

    assert(false);
}

bool file_is_child_of_directory(const char* file_name, const char* dir_name)
{
    assert(file_name);
    assert(dir_name);

    assert(file_exists(file_name));
    assert(dir_exists(dir_name));

    const char* norm_file = normalize_path(file_name);
    const char* norm_dir = normalize_path(dir_name);

    size_t dir_len = strlen(norm_dir);

    bool result = strncmp(norm_file, norm_dir, dir_len) == 0;

    mem_free(norm_file);
    mem_free(norm_dir);

    return result;
}

const char* read_file_string(const char* file_path)
{
    assert(file_path);

    FILE* f_handle = fopen(file_path, "rb");
    if (f_handle == NULL)
    {
        fatal("Unable to open file: %s\n", file_path);
    }

    fseek(f_handle, 0, SEEK_END);
    long size = ftell(f_handle);
    fseek(f_handle, 0, SEEK_SET);

    char* result = (char*)mem_alloc(size + 1);
    size_t read_result = fread(result, size, 1, f_handle);
    assert(read_result == 1);
    result[size] = '\0';

    fclose(f_handle);

    return result;
}

void fatal(const char* format, ...)
{
    va_list va_args;
    va_start(va_args, format);
    fatal(format, va_args);
    va_end(va_args);
}

void fatal(const char* format, va_list args)
{
    fprintf(stderr, "error: ");
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");

    exit(1);
}

const char* string_append(const char* str_a, const char* str_b)
{
    assert(str_a);
    assert(str_b);

    auto a_len = strlen(str_a);
    auto b_len = strlen(str_b);
    auto len = a_len + b_len;

    char* new_str = (char*)mem_alloc(sizeof(char) * (len + 1));
    memcpy((void*)new_str, str_a, a_len);
    memcpy((void*)(new_str + a_len), str_b, b_len);
    new_str[len] = '\0';

    return new_str;
}

bool string_contains(const char* str, char c)
{
    for (size_t i = 0; i < strlen(str); i++)
    {
        if (c == str[i])
        {
            return true;
        }
    }
    return false;
}

const char* extract_directory_from_path(const char* path)
{
    assert(path);

    uint64_t path_length = strlen(path);
    assert(path_length >= 1);
    uint64_t last_sep_index = 0;

    bool sep_found = false;

    for (uint64_t i = path_length - 1; i != 0; i--)
    {
        if (path[i] == '/' || path[i] == '\\')
        {
            last_sep_index = i;
            sep_found = true;
            break;
        }
    }

    char* result = nullptr;

    if (sep_found)
    {
        size_t result_len = last_sep_index + 1;
        result = (char*)mem_alloc(sizeof(char) * (result_len + 1));
        memcpy(result, path, result_len);
        result[result_len] = '\0';
    }
    else
    {

    #ifdef WIN32
        assert(false);
    #endif

        const char* result_str = "/./";
        size_t result_len = strlen(result_str);
        result = (char*)mem_alloc(sizeof(char) * (result_len + 1));
        memcpy(result, result_str, result_len);
        result[result_len] = '\0';
    }

    assert(result);
    return result;
}

const char* extract_file_name_from_path(const char* path)
{
    assert(path);

    size_t path_len = strlen(path);
    size_t last_sep_idx = 0;
    size_t last_dot_idx = path_len;
    bool sep_found = false;
    bool dot_found = false;

    for (size_t i = path_len - 1; i < path_len; i--)
    {
        if (path[i] == '.')
        {
            last_dot_idx = i;
            dot_found = true;
        }
        if (path[i] == '/' || path[i] == '\\')
        {
            last_sep_idx = i;
            sep_found = true;
            break;
        }
    }

    size_t cpy_offset = 0;
    size_t new_len = path_len;

    if (sep_found)
    {
        new_len -= (last_sep_idx + 1);
        cpy_offset = last_sep_idx + 1;
    }
    if (dot_found) new_len -= (path_len - last_dot_idx);

    char* result = (char*)mem_alloc(sizeof(char) * (new_len + 1));
    memcpy(result, path + cpy_offset, new_len);
    result[new_len] = '\0';

    return result;
}

const char* full_path_from_cwd(const char* rel_path)
{
    assert(rel_path);

    char cwd[1024 * 5];

#ifdef WIN32
    _getcwd(cwd, sizeof(cwd));
#else
    getcwd(cwd, sizeof(cwd));
#endif

    assert(strlen(cwd) < (sizeof(cwd) - 1));
    size_t cwd_len = strlen(cwd);

#ifdef WIN32
    cwd[cwd_len] = '\\';
#else
    cwd[cwd_len] = '/';
#endif

    cwd[cwd_len + 1] = '\0';
    const char* full_path = string_append(cwd, rel_path);
    assert(file_exists(full_path) || dir_exists(full_path));

    const char* result = normalize_path(full_path);

    assert(result);
    mem_free(full_path);

    return result;
}

const char* normalize_path(const char* path)
{
    assert(path);

    char _result[MAX_PATH];
    size_t result_len = 0;

#ifdef WIN32
    if (!PathCanonicalizeA((LPSTR)_result, path))
    {
        assert(false);
    }

    result_len = strlen(_result);

    if (dir_exists(_result) && _result[result_len - 1] != '\\')
    {
        assert(result_len + 2 <= MAX_PATH);
        _result[result_len] = '\\';
        result_len++;
        _result[result_len] = 0;
    }

#else
    if (!realpath(path, _result))
    {
        assert(false);
    }

    result_len = strlen(_result);

    if (dir_exists(_result) && _result[result_len - 1] != '/')
    {
        assert(result_len + 2 <= MAX_PATH);
        _result[result_len] = '/';
        result_len++;
        _result[result_len] = 0;
    }

#endif

    char* result = (char*)mem_alloc(sizeof(char) * (result_len + 1));
    memcpy(result, _result, result_len + 1);
    return result;
}
