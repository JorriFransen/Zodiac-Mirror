#! /usr/bin/env bash

pushd lib
    unzip dyncall-1.0.zip
popd

mkdir -p build

pushd build
    cmake .. -G "Ninja" -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
popd

