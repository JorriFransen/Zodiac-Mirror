#pragma once

#include "common.h"

#include <stdint.h>
#include <cassert>

typedef void *(*_Thread_Routine)(void *);

#ifdef _WIN32

#include <Windows.h>

typedef HANDLE Thread_Handle;

static bool _create_thread(LPTHREAD_START_ROUTINE func, void* user_data, Thread_Handle* handle)
{
	Thread_Handle new_handle = CreateThread(nullptr, 0, func, user_data, 0, nullptr);

	if (new_handle)
	{
		*handle = new_handle;
		return true;
	}

	return false;
}

static bool _join_thread(Thread_Handle handle)
{
	WaitForSingleObject(handle, INFINITE);
	return CloseHandle(handle);
}

static bool _compare_and_swap(uint64_t* pointer, uint64_t old_value, uint64_t new_value)
{
	auto ret = InterlockedCompareExchange64((LONG64*)pointer, new_value, old_value);
	return ret == old_value;
}

#define CREATE_THREAD(func, user_data, handle) _create_thread((LPTHREAD_START_ROUTINE)func, user_data, handle);
#define JOIN_THREAD(handle) _join_thread(handle)
#define COMPARE_AND_SWAP(pointer, old_value, new_value) \
	_compare_and_swap(pointer, old_value, new_value)

static const char* _get_environment_variable(const char* name)
{
	assert(name);

	char* result = nullptr;
	auto var_length = GetEnvironmentVariable(name, nullptr, 0);
	if (var_length)
	{
		char* buffer = (char*)malloc(var_length);
		auto length_without_null = GetEnvironmentVariable(name, buffer, var_length);
		assert(length_without_null == var_length - 1);
		result = (char*)normalize_path(buffer);
		free(buffer);
	}

	return result;
}


#elif __linux__

#include <cstdlib>
#include <pthread.h>

typedef pthread_t Thread_Handle;

static bool _create_thread(_Thread_Routine func, void* user_data, Thread_Handle* handle)
{
    auto result = pthread_create(handle, nullptr, func, user_data);
    if (result == 0)
    {
        assert(*handle != 0);
        return true;
    }
    return false;
}

static bool _join_thread(Thread_Handle handle)
{
    auto result = pthread_join(handle, nullptr);
    return result == 0;
}

#define CREATE_THREAD(func, user_data, handle) _create_thread(func, user_data, handle)
#define JOIN_THREAD(handle) _join_thread(handle)
#define COMPARE_AND_SWAP(pointer, old_value, new_value) \
	__sync_bool_compare_and_swap(pointer, old_value, new_value)

static const char* _get_environment_variable(const char* name)
{
	assert(name);
	char* result = getenv(name);
	if (result)
	{
		result = (char*)normalize_path(result);
	}

	return result;
}

#endif

#define _GET_ENVIRONMENT_VARIABLE(name) _get_environment_variable(name)

namespace Zodiac
{
    const char* platform_get_environment_var(const char* name);
}
