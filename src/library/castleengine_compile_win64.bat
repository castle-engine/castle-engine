:: Call this script from this directory
@echo off
setlocal

:: You can pass additional FPC parameters as this script parameters
:: So you can call, for example, 'castleengine_compile_win64.bat -dSOME_SYMBOL'
set FPC_CONFIG=%*

:: First auto-detect fpc.exe location
for /f "tokens=*" %%i in ('..\..\tools\fpc_path_detect.bat') do set FPC_EXE_DIR=%%i
echo fpc folder: %FPC_EXE_DIR%fpc.exe

:: Supposing you have cross-compilers for win32 or win64 in that folder, let's compile the library
set OUT_DIR=build-win64
if not exist %OUT_DIR% mkdir %OUT_DIR%

:: Change to the directory where castle-fpc.cfg is.
cd ..\..

%FPC_EXE_DIR%fpc.exe -fPIC -dRELEASE @castle-fpc.cfg -dCASTLE_WINDOW_LIBRARY src/library/castleengine.lpr -Px86_64 -FEsrc/library/%OUT_DIR% %FPC_CONFIG%
