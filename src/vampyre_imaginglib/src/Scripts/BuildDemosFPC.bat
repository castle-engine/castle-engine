@echo OFF
echo Building Demos using Free Pascal

set ROOTDIR=..
set DEMOPATH=%ROOTDIR%\Demos\ObjectPascal
set UNITS=-Fu%ROOTDIR%\Source -Fu%ROOTDIR%\Source\JpegLib -Fu%ROOTDIR%\Source\ZLib -Fu%ROOTDIR%\Extras\Extensions -Fu%ROOTDIR%\Extras\Extensions\LibTiff -Fu%DEMOPATH%\Common
set INCLUDE=-Fi%ROOTDIR%\Source 
set LIBS=-Fl%ROOTDIR%\Extras\Extensions\J2KObjects -Fu%ROOTDIR%\Extras\Extensions\LibTiff\Compiled
set OUTPUT=-FE%ROOTDIR%\Demos\Bin
set OPTIONS=-Sgi -O2 -Xs
rem set TARGET=-Twin32

set DEMOSBUILD=0
set DEMOCOUNT=2

call :BUILD Benchmark\Bench.dpr -oBench.exe
call :BUILD VampConvert\VampConvert.dpr -oVampConvert.exe

goto END

:BUILD
  fpc %TARGET% %OPTIONS% %OUTPUT% "%DEMOPATH%\%1" %UNITS% %INCLUDE% %LIBS% %2
  if errorlevel 1 (echo Error when building %1) else (set /a DEMOSBUILD+=1)
goto :EOF

:END
if "%DEMOSBUILD%"=="%DEMOCOUNT%" (
  echo [92mBuild Successful - all %DEMOSBUILD% of %DEMOCOUNT% build[0m
) else (
  echo [91mErrors during building - only %DEMOSBUILD% of %DEMOCOUNT% demos build[0m
)

call Clean.bat
