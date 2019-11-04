
#include "platform.h"
#include "atom.h"

namespace Zodiac
{
#ifdef _WIN32

	bool _folder_name_is_version_number(const char* name)
	{
		bool valid_version = true;
		uint32_t dot_count = 0;

		for (uint64_t i = 0; i < strlen(name); i++)
		{
			char c = name[i];
			if (IsCharAlphaNumericA(c))
			{
				continue;
			}
			else if (c == '.')
			{
				dot_count++;
				if (dot_count > 2)
				{
					valid_version = false;
					break;
				}
			}
			else
			{
				valid_version = false;
				break;
			}
		}

		return valid_version;
	}

	struct _Version
	{
		uint64_t major = 0;
		uint64_t minor = 0;
		uint64_t patch = 0;
	};

	_Version _parse_version(const char* version_str)
	{
		Atom major_atom = { version_str, 0 };
		Atom minor_atom = { nullptr, 0 };
		Atom patch_atom = { nullptr, 0 };

		uint32_t dot_count = 0;

		for (uint64_t i = 0; i < strlen(version_str); i++)
		{
			if (version_str[i] == '.')
			{
				if (dot_count == 0)
				{
					minor_atom.data = &version_str[i + 1];
				}
				else if (dot_count == 1)
				{
					patch_atom.data = &version_str[i + 1];
				}

				dot_count++;
			}
			else
			{
				if (dot_count == 0)
				{
					major_atom.length++;
				}
				else if (dot_count == 1)
				{
					minor_atom.length++;
				}
				else if (dot_count == 2)
				{
					patch_atom.length++;
				}
				else assert(false);
			}
		}

		return
		{
			atom_to_u64(major_atom),
			atom_to_u64(minor_atom),
			atom_to_u64(patch_atom),
		};
	}

	uint64_t _find_best_version(BUF(const char*) versions)
	{
		assert(versions);
		assert(BUF_LENGTH(versions));

		uint64_t best_major = 0;
		uint64_t best_minor = 0;
		uint64_t best_patch = 0;

		uint64_t best_index = 0;

		for (uint64_t i = 0; i < BUF_LENGTH(versions); i++)
		{
			_Version version = _parse_version(versions[i]);
			if (version.major > best_major)
			{
				best_major = version.major;
				best_minor = version.minor;
				best_patch = version.patch;
				best_index = i;
			}
			else if (version.major == best_major && version.minor > best_minor)
			{
				best_minor = version.minor;
				best_patch = version.patch;
				best_index = i;
			}
			else if (version.major == best_major && version.minor == best_minor &&
				version.patch > best_patch)
			{
				best_patch = version.patch;
				best_index = i;
			}
		}

		return best_index;
	}

	const char* find_msvc_tools_dir()
	{
		auto base_dir = "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC";
		assert(dir_exists(base_dir));

		auto search_pattern = string_append(base_dir, "\\*");

		WIN32_FIND_DATA find_data;
		HANDLE file_handle = FindFirstFileA(search_pattern, &find_data);
		assert(file_handle);

		BUF(const char*) candidates = nullptr;

		while (file_handle)
		{
			auto file_name = find_data.cFileName;
			if (!((strcmp(file_name, ".") == 0) || (strcmp(file_name, "..") == 0)))
			{
				if ((find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0 &&
					_folder_name_is_version_number(file_name))
				{
					auto _file_name = string_append(file_name, "");
					BUF_PUSH(candidates, _file_name);
				}
			}

			BOOL result = FindNextFileA(file_handle, &find_data);
			if (!result) break;
		}

		auto highest_version_index = _find_best_version(candidates);

		auto _tools_dir = string_append(base_dir, "\\");
		auto tools_dir = string_append(_tools_dir, candidates[highest_version_index]);

		mem_free(_tools_dir);
		for (uint64_t i = 0; i < BUF_LENGTH(candidates); i++) { mem_free(candidates[i]); }
		BUF_FREE(candidates);
		mem_free(search_pattern);

		return tools_dir;
	}

	const char* find_windows_kit_lib_dir()
	{
		auto base_dir = "C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.18362.0";
		assert(dir_exists(base_dir));
		return string_append(base_dir, "");
	}

#elif __linux__

    const char* find_linux_x64_lib_path()
    {
        if (file_exists("/usr/lib64/libc.so.6"))
            return "/usr/lib64/";
        else if (file_exists("/lib/x86_64-linux-gnu/libc.so.6"))
            return "/lib/x86_64-linux-gnu/";

        assert(false);
    }

    const char* find_linux_gcc_lib_path()
    {
        if (file_exists("/usr/lib64/libc.so.6"))
            return "/usr/lib64/gcc/x86_64-pc-linux-gnu/9.2.0/";
        else if (file_exists("/lib/x86_64-linux-gnu/libc.so.6"))
            return "/usr/lib/gcc/x86_64-linux-gnu/9/";

        assert(false);
    }

#endif


    const char* platform_get_environment_var(const char* name)
    {
		return _GET_ENVIRONMENT_VARIABLE(name);
    }
}
