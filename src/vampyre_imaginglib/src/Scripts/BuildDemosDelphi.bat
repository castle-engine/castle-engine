@echo OFF
echo Building Demos using Delphi
echo.

set ROOTDIR=..
set DEMOPATH=%ROOTDIR%\Demos\ObjectPascal
set OUTPUT=-E%ROOTDIR%\Demos\Bin
set INCLUDE=-I%ROOTDIR%\Source 
set UNITOUT=-N%ROOTDIR%\Demos\Bin\Dcu\
rem -NS Unit scopes are needed for newer Delphi and argument is happily ignored by D7
set OPTIONS=-B -$D- -$L- -$Y- -Q -DRELEASE -NSSystem;Winapi;Vcl;Vcl.Shell

set DEFINES=-DFULL_FEATURE_SET
set UNITS=-U%ROOTDIR%\Source -U%ROOTDIR%\Source\JpegLib -U%ROOTDIR%\Source\ZLib -U%ROOTDIR%\Extensions -U%ROOTDIR%\Extensions\LibTiff
set UNITS=%UNITS% -U%ROOTDIR%\Extensions\LibTiff -U%DEMOPATH%\Common

set DEMOSBUILD=0
set DEMOCOUNT=3

call :BUILD Benchmark\Bench.dpr
call :BUILD VampConvert\VampConvert.dpr
call :BUILD ImageBrowser\ImgBrowser.dpr

goto END

:BUILD
  dcc32 %OPTIONS% %DEMOPATH%\%1 %OUTPUT% %UNITS% %INCLUDE% %DEFINES% %UNITOUT%
  if errorlevel 1 (echo Error when building %1) else (set /a DEMOSBUILD+=1)
  echo.
goto :EOF

:END
if "%DEMOSBUILD%"=="%DEMOCOUNT%" (
  echo [92mBuild Successful - all %DEMOSBUILD% of %DEMOCOUNT% build in Demos/Bin directory[0m
) else (
  echo [91mErrors during building - only %DEMOSBUILD% of %DEMOCOUNT% demos build[0m
)


