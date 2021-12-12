@echo OFF
echo Building Extension Demos using Free Pascal

rem Important! Set this dirs on your system for the demos to compile!
set SDLDIR=..\Demos\ObjectPascal\Common
set OPENGLDIR=..\Demos\ObjectPascal\Common
set D3DDIR=..\Demos\ObjectPascal\Common

set ROOTDIR=..
set DEMOPATH=%ROOTDIR%\Demos\ObjectPascal
set UNITS=-Fu%ROOTDIR%\Source -Fu%ROOTDIR%\Source\JpegLib -Fu%ROOTDIR%\Source\ZLib -Fu%DEMOPATH%\Common
set UNITS=%UNITS% -Fu%ROOTDIR%\Source\Extensions -Fu%ROOTDIR%\Extras\Extensions -Fu%ROOTDIR%\Extras\Extensions\LibTiff -Fu"%SDLDIR%" -Fu"%OPENGLDIR%" -Fu"%D3DDIR%"
set INCLUDE=-Fi%ROOTDIR%\Source -Fi"%SDLDIR%" -Fi"%OPENGLDIR%" -Fi"%D3DDIR%"
set LIBS=-Fl%ROOTDIR%\Extras\Extensions\J2KObjects -Fu%ROOTDIR%\Extras\Extensions\LibTiff\Compiled
set OUTPUT=-FE%ROOTDIR%\Demos\Bin
set OPTIONS=-Sgi -O2 -Xs -dDONT_LINK_EXTRAS

set DEMOSBUILD=0
set DEMOCOUNT=3

set CURRDEMO=SDLDemo\SDLDemo.dpr
if "%SDLDIR%"=="" (echo SDL search directory not set - skipping %CURRDEMO%) else (call :BUILD %CURRDEMO% -oSDLDemo.exe)

set CURRDEMO=OpenGLDemo\OpenGLDemo.dpr
if "%OPENGLDIR%"=="" (echo OpenGL search directory not set - skipping %CURRDEMO%) else (call :BUILD %CURRDEMO% -oOpenGLDemo.exe)

set CURRDEMO=D3DDemo\D3DDemo.dpr
if "%D3DDIR%"=="" (echo D3D search directory not set - skipping %CURRDEMO%) else (call :BUILD %CURRDEMO% -oD3DDemo.exe)

goto END

:BUILD
  fpc %OPTIONS% %OUTPUT% "%DEMOPATH%\%1" %UNITS% %INCLUDE% %LIBS% %2
  if errorlevel 1 (echo Error when building %1) else (set /a DEMOSBUILD+=1)
goto :EOF

:END
if "%DEMOSBUILD%"=="%DEMOCOUNT%" (
  echo [92mBuild Successful - all %DEMOSBUILD% of %DEMOCOUNT% build[0m
) else (
  echo [91mErrors during building - only %DEMOSBUILD% of %DEMOCOUNT% demos build[0m
)

call Clean.bat
