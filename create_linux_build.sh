#! /usr/bin/env bash

pushd lib
    unzip -u dyncall-1.0.zip
popd

mkdir -p build

pushd build
    cmake .. -G "Ninja" -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON

    mkdir -p release_deb
    pushd release_deb
    cmake ../.. -G "Ninja" -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_BUILD_TYPE=RelWithDepInfo -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
popd

