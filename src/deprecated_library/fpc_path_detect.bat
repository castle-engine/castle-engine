:: Auto-detect fpc.exe location
@echo off

:: Test if fpc is in %PATH%, return empty string
where /q fpc.exe
if not ERRORLEVEL 1 (
  echo.
  exit /B
)

setlocal

:: see if environment variable exists
if defined FPCDIR (
  set FPC_MAIN=%FPCDIR%
) else (
:: Check if standalone FPC installation exists
  if exist c:\fpc (
    set FPC_MAIN=c:\fpc
  ) else (
:: See if is under Lazarus
    if exist c:\lazarus\fpc ( 
      set FPC_MAIN=c:\lazarus\fpc
    ) else (
:: See if is under fpcupdeluxe
      if exist c:\fpcupdeluxe\fpc set FPC_MAIN=c:\fpcupdeluxe\fpc
    )
  )
)

::echo fpc main folder: %FPC_MAIN%

:: If FPC not found, let's just silently suppose it is in PATH as we have to return something 
if not defined FPC_MAIN (
  echo.
  exit /B
)

if "%FPC_MAIN%" == "c:\fpcupdeluxe\fpc" (
:: fpcdeluxe doesn't use version subfolder 
  set FPC_VER_DIR=%FPC_MAIN% 
) else (
:: Find FPC version folder under fpc folder, e.g. c:\lazarus\fpc\3.0.2 
  for /d %%D in (%FPC_MAIN%\*.*) do set FPC_VER_DIR=%%D
)

::echo fpc version folder: %FPC_VER_DIR%

:: Find installed FPC platform (win32 or win64), e.g. c:\lazarus\fpc\3.0.2\bin\x86_64-win64 
for /d %%D in (%FPC_VER_DIR%\bin\*.*) do set FPC_EXE_DIR=%%D
if not defined FPC_EXE_DIR set FPC_EXE_DIR=%FPC_VER_DIR%\bin 

:: Return path including the ending slash (to be able to pass empty string too when fpc.exe is in path) 
echo %FPC_EXE_DIR%\
