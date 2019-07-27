cmake_minimum_required(VERSION 3.4.3)

# find_package(LLVM REQUIRED CONFIG)
# message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
# message(STATUS "Using LLVMConfig.cmake.in: ${LLVM_DIR}")
# include_directories(${LLVM_INCLUDE_DIRS})
# add_definitions(${LLVM_DEFINITIONS})

# if (WIN32)
#   llvm_map_components_to_libnames(llvm_libs
#     support core irreader linker native target)
# else()
#   llvm_map_components_to_libnames(llvm_libs support core irreader linker native target)
# endif()
project(zodiac)

if(MSVC)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
endif()

set (ZODIAC_COMPILER_SOURCE
    source/common.cpp

    source/zodiac.cpp

    source/ast.cpp
    source/atom.cpp
    source/builtin.cpp
    source/const_interpreter.cpp
    source/ir.cpp
    source/ir_runner.cpp
    source/lexer.cpp
    source/parser.cpp
    source/platform.cpp
    source/polymorph.cpp
    source/resolver.cpp
    source/string_builder.cpp
    source/token.cpp
)

add_subdirectory(lib/dyncall-1.0/)

if (WIN32)
    set(ZODIAC_LIBS user32.lib kernel32.lib Shlwapi.lib)
elseif(UNIX)
    set(ZODIAC_LIBS dl pthread)
endif()

add_executable(zodiac source/main.cpp ${ZODIAC_COMPILER_SOURCE})
target_include_directories(zodiac PRIVATE include/zodiac)
target_include_directories(zodiac PRIVATE lib/dyncall-1.0/dyncall)
target_include_directories(zodiac PRIVATE lib/dyncall-1.0/dyncallback)
target_include_directories(zodiac PRIVATE lib/dyncall-1.0/dynload)
target_link_libraries(zodiac dyncall_s dynload_s dyncallback_s ${ZODIAC_LIBS})
