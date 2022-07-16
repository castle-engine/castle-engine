@echo OFF
rem Ext. demos have some build and/or runtime dependencies like SDL or GL.
echo Building Extended Demos using Free Pascal
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

set DEFINES=-dDONT_LINK_EXTRAS
set UNITS=-Fu%ROOTDIR%\Source -Fu%ROOTDIR%\Source\JpegLib -Fu%ROOTDIR%\Source\ZLib -Fu%ROOTDIR%\Extensions -Fu%DEMOPATH%\Common
set INCLUDE=-Fi%ROOTDIR%\Source 

set DEMOSBUILD=0
set DEMOCOUNT=3

call :BUILD SDLDemo\SDLDemo.dpr 
call :BUILD OpenGLDemo\OpenGLDemo.dpr 
call :BUILD D3DDemo\D3DDemo.dpr 

goto END

:BUILD
  fpc %TARGET% %OPTIONS% %OUTPUT% %DEFINES% %UNITS% %INCLUDE% %LIBS% "%DEMOPATH%\%1" 
  if errorlevel 1 (echo Error when building %1) else (set /a DEMOSBUILD+=1)
  echo.
goto :EOF

:END
if "%DEMOSBUILD%"=="%DEMOCOUNT%" (
  echo [92mBuild Successful -  all %DEMOSBUILD% of %DEMOCOUNT% build in Demos/Bin directory[0m
) else (
  echo [91mErrors during building - only %DEMOSBUILD% of %DEMOCOUNT% demos build[0m
)

