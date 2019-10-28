
IF NOT EXIST "build_release" mkdir build_release
IF NOT EXIST "build_debug" mkdir build_debug

pushd build_release
   cmake .. -G "Visual Studio 16 2019" -Thost=x64 -DCMAKE_BUILD_TYPE=Release
popd

pushd build_debug
    cmake .. -G "Visual Studio 16 2019" -Thost=x64 -DCMAKE_BUILD_TYPE=Debug
popd
