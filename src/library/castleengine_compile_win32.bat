:: Call this script from this directory
@echo off

:: First auto-detect fpc.exe location

:: Find FPC version folder under Lazarus installation, e.g. c:\lazarus\fpc\3.0.2 
for /d %%D in (c:\lazarus\fpc\*.*) do set FPC_VER_DIR=%%D
echo fpc version folder:  %FPC_VER_DIR%

:: Find installed FPC platform (win32 or win64), e.g. c:\lazarus\fpc\3.0.2\bin\x86_64-win64 
for /d %%D in (%FPC_VER_DIR%\bin\*.*) do set FPC_EXE_DIR=%%D
echo fpc platform folder: %FPC_EXE_DIR%

:: Supposing you have cross-compilers for win32 or win64 in that folder, let's compile the library

:: Change to the directory where castle-fpc.cfg is.
cd ..\..

%FPC_EXE_DIR%\fpc.exe -fPIC -dRELEASE @castle-fpc.cfg -dCASTLE_WINDOW_LIBRARY src/library/castleengine.lpr -Pi386 -ocastleengine32.dll
