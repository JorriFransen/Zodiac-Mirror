cmake_minimum_required(VERSION 3.13.5)

project(zodiac)

set (ZODIAC_COMPILER_SOURCE
    source/common.cpp

    source/zodiac.cpp

    source/ast.cpp
    source/atom.cpp
    source/builtin.cpp
    source/const_interpreter.cpp
    source/copier.cpp
    source/hash.cpp
    source/ir.cpp
    source/ir_printer.cpp
    source/ir_runner.cpp
    source/lexer.cpp
    source/llvm.cpp
    source/llvm_debug_info.cpp
    source/llvm_types.cpp
    source/parser.cpp
    source/platform.cpp
    source/polymorph.cpp
    source/resolver.cpp
    source/string_builder.cpp
    source/token.cpp
    source/type_info.cpp
)

add_executable(zodiac source/main.cpp ${ZODIAC_COMPILER_SOURCE})
target_include_directories(zodiac PRIVATE include/zodiac)
target_include_directories(zodiac PRIVATE lib/include)

if(MSVC)
    add_compile_definitions(NOMINMAX)

    if(CMAKE_BUILD_TYPE STREQUAL Debug)
        target_link_directories(zodiac PRIVATE lib/windows/lib/debug)
    else()
        target_link_directories(zodiac PRIVATE lib/windows/lib)
    endif()

    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
    set(ZODIAC_LIBS user32.lib kernel32.lib Shlwapi.lib)

    set (DYNCALL_LIBS
        dyncall_s.lib
        dynload_s.lib
        dyncallback_s.lib
    )

    set (LLVM_LIBS
        LLVMX86CodeGen.lib
        LLVMGlobalISel.lib
        LLVMX86Desc.lib
        LLVMX86Utils.lib
        LLVMX86Info.lib
        LLVMMCDisassembler.lib
        LLVMSelectionDAG.lib
        LLVMAsmPrinter.lib
        LLVMDebugInfoDWARF.lib
        LLVMCodeGen.lib
        LLVMTarget.lib
        LLVMScalarOpts.lib
        LLVMInstCombine.lib
        LLVMAggressiveInstCombine.lib
        LLVMTransformUtils.lib
        LLVMBitWriter.lib
        LLVMAnalysis.lib
        LLVMProfileData.lib
        LLVMObject.lib
        LLVMMCParser.lib
        LLVMBitReader.lib
        LLVMBitstreamReader.lib
        LLVMMC.lib
        LLVMDebugInfoCodeView.lib
        LLVMDebugInfoMSF.lib
        LLVMCore.lib
        LLVMRemarks.lib
        LLVMBinaryFormat.lib
        LLVMSupport.lib
        LLVMDemangle.lib
    )

elseif(UNIX)
    target_link_directories(zodiac PRIVATE lib/linux/lib)

    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fuse-ld=gold")
    set(ZODIAC_LIBS dl pthread)

    SET (DYNCALL_LIBS
        dyncall_s.a
        dynload_s.a
        dyncallback_s.a
    )
    
    set (LLVM_LIBS
        LLVMX86CodeGen.a
        LLVMGlobalISel.a
        LLVMX86Desc.a
        LLVMX86Utils.a
        LLVMX86Info.a
        LLVMMCDisassembler.a
        LLVMSelectionDAG.a
        LLVMAsmPrinter.a
        LLVMDebugInfoDWARF.a
        LLVMDebugInfoCodeView.a
        LLVMCodeGen.a
        LLVMTarget.a
        LLVMScalarOpts.a
        LLVMInstCombine.a
        LLVMAggressiveInstCombine.a
        LLVMTransformUtils.a
        LLVMBitWriter.a
        LLVMAnalysis.a
        LLVMProfileData.a
        LLVMObject.a
        LLVMMCParser.a
        LLVMBitReader.a
        LLVMBitstreamReader.a
        LLVMMC.a
        LLVMDebugInfoMSF.a
        LLVMCore.a
        LLVMRemarks.a
        LLVMBinaryFormat.a
        LLVMSupport.a
        LLVMDemangle.a
        LLVMWindowsManifest.a
        LLVMTableGen.a
        LLVMTransformUtils.a
        LLVMInstrumentation.a
        LLVMOption.a

        tinfo
        z
    )

endif()

target_link_libraries(zodiac ${DYNCALL_LIBS} ${ZODIAC_LIBS} ${LLVM_LIBS})
