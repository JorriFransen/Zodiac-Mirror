
pushd lib
   IF NOT EXIST "dyncall-1.0" powershell Expand-Archive -Path dyncall-1.0.zip -DestinationPath .
popd

IF NOT EXIST "build" mkdir build

pushd build
   cmake .. -G "Visual Studio 15 2017 Win64" -Thost=x64
popd