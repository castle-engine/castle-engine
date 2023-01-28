@echo OFF
echo Building Demos using Free Pascal
echo.

set ROOTDIR=..
set DEMOPATH=%ROOTDIR%\Demos\ObjectPascal
set BINPATH=%ROOTDIR%\Demos\Bin
set UNITPATH=%ROOTDIR%\Demos\Bin\Dcu\
set OUTPUT=-FE%BINPATH% -FU%UNITPATH%
rem This is how you suppress -vn set in fpc.cfg
set OPTIONS=-B -O3 -Xs -vn-
rem set TARGET=-Twin32

rem FPC does not like creating any new directories passed by -FE -FU
mkdir %BINPATH%\ %UNITPATH%\  2>nul

set DEFINES=-dFULL_FEATURE_SET
set UNITS=-Fu%ROOTDIR%\Source -Fu%ROOTDIR%\Source\JpegLib -Fu%ROOTDIR%\Source\ZLib -Fu%ROOTDIR%\Extensions -Fu%ROOTDIR%\Extensions\LibTiff -Fu%DEMOPATH%\Common
set INCLUDE=-Fi%ROOTDIR%\Source 
set LIBS=-Fl%ROOTDIR%\Extensions\J2KObjects -Fl%ROOTDIR%\Extensions\LibTiff\Compiled

set DEMOSBUILD=0
set DEMOCOUNT=2

call :BUILD Benchmark\Bench.dpr 
call :BUILD VampConvert\VampConvert.dpr 

goto END

:BUILD
  fpc %TARGET% %OPTIONS% %OUTPUT% %DEFINES% %UNITS% %INCLUDE% %LIBS% "%DEMOPATH%\%1" 
  if errorlevel 1 (echo Error when building %1) else (set /a DEMOSBUILD+=1)
  echo.
goto :EOF

:END
if "%DEMOSBUILD%"=="%DEMOCOUNT%" (
  echo [92mBuild Successful - all %DEMOSBUILD% of %DEMOCOUNT% build in Demos/Bin directory[0m
) else (
  echo [91mErrors during building - only %DEMOSBUILD% of %DEMOCOUNT% demos build[0m
)

