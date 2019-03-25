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


set (ZODIAC_COMPILER_SOURCE
    source/common.cpp

    source/zodiac.cpp

    source/ast.cpp
    source/atom.cpp
    source/builtin.cpp
    source/ir.cpp
    source/lexer.cpp
    source/parser.cpp
    source/resolver.cpp
    source/token.cpp
    source/types.cpp
)

add_subdirectory(lib/dyncall-1.0/)

if (WIN32)
    set(ZODIAC_LIBS user32.lib kernel32.lib Shlwapi.lib)
elseif(UNIX)
    set(ZODIAC_LIBS dl)
endif()

add_executable(zodiac source/main.cpp ${ZODIAC_COMPILER_SOURCE})
target_include_directories(zodiac PRIVATE include/zodiac)
target_include_directories(zodiac PRIVATE lib/dyncall-1.0)
target_link_libraries(zodiac dyncall_s dynload_s ${ZODIAC_LIBS})

add_executable(vm_test source/vm_test.cpp ${ZODIAC_COMPILER_SOURCE})
target_include_directories(vm_test PRIVATE include/zodiac)
target_include_directories(vm_test PRIVATE lib/dyncall-1.0)
target_link_libraries(vm_test dyncall_s dynload_s ${ZODIAC_LIBS})

add_executable(ir_test source/ir_test.cpp ${ZODIAC_COMPILER_SOURCE})
target_include_directories(ir_test PRIVATE include/zodiac)
target_include_directories(ir_test PRIVATE lib/dyncall-1.0)
target_link_libraries(ir_test dyncall_s dynload_s ${ZODIAC_LIBS})
