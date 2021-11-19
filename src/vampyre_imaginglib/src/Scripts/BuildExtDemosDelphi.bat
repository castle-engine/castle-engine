@echo OFF
echo Building Extension Demos using Delphi

rem Important! Set this dirs on your system for the demos to compile! 
set SDLDIR=..\Demos\ObjectPascal\Common
set OPENGLDIR=..\Demos\ObjectPascal\Common
set D3DDIR=..\Demos\ObjectPascal\Common

set ROOTDIR=..
set DEMOPATH=%ROOTDIR%\Demos\ObjectPascal
set OUTPUT=-E%ROOTDIR%\Demos\Bin
set UNITS=-U%ROOTDIR%\Source -U%ROOTDIR%\Source\JpegLib -U%ROOTDIR%\Source\ZLib -U%DEMOPATH%\Common -U%ROOTDIR%\Extras\Extensions\LibTiff
set UNITS=%UNITS% -U%ROOTDIR%\Source\Extensions -U%ROOTDIR%\Extras\Extensions -U"%SDLDIR%" -U"%OPENGLDIR%" -U"%D3DDIR%"
set INCLUDE=-I%ROOTDIR%\Source -I"%SDLDIR%" -I"%OPENGLDIR%" -I"%D3DDIR%"
set OPTIONS=-B -$D- -$L- -$Y- -DRELEASE

set DEMOSBUILD=0
set DEMOCOUNT=3

set CURRDEMO=SDLDemo\SDLDemo.dpr
if "%SDLDIR%"=="" (echo SDL search directory not set - skipping %CURRDEMO%) else (call :BUILD %CURRDEMO%)

set CURRDEMO=OpenGLDemo\OpenGLDemo.dpr
if "%OPENGLDIR%"=="" (echo OpenGL search directory not set - skipping %CURRDEMO%) else (call :BUILD %CURRDEMO%)

set CURRDEMO=D3DDemo\D3DDemo.dpr
if "%D3DDIR%"=="" (echo D3D search directory not set - skipping %CURRDEMO%) else (call :BUILD %CURRDEMO%)

goto END

:BUILD
  dcc32 %OPTIONS% %DEMOPATH%\%1 %OUTPUT% %UNITS% %INCLUDE%
  if errorlevel 1 (echo Error when building %1) else (set /a DEMOSBUILD+=1)
goto :EOF

:END
if "%DEMOSBUILD%"=="%DEMOCOUNT%" (
  echo [92mBuild Successful - all %DEMOSBUILD% of %DEMOCOUNT% build[0m
) else (
  echo [91mErrors during building - only %DEMOSBUILD% of %DEMOCOUNT% demos build[0m
)

call Clean.bat
