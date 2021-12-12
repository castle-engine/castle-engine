@echo OFF
echo Deleting ugly files...

set ROOTDIR=..
set EXTS=*.dcu *.ppu *.a *.dpu *.o *.rst *.bak *.bk? *.~* *.*~ *.or *.obj
set EXTS=%EXTS% *.tgs *.tgw *.identcache *.local *.stat *.dcpil

del /q /f %ROOTDIR%\Demos\Bin\_BenchOut.* 2>nul 1>nul

call :DELINTREE %ROOTDIR%\Bin
call :DELINTREE %ROOTDIR%\Demos 
call :DELINTREE %ROOTDIR%\Scripts 
call :DELINTREE %ROOTDIR%\Source\JpegLib
call :DELINTREE %ROOTDIR%\Source\ZLib
call :DELINDIR %ROOTDIR%\Source
call :DELINDIR %ROOTDIR%\Extras\Extensions
call :DELINDIR %ROOTDIR%\Extras\DynamicLib
call :DELINDIR %ROOTDIR%\Extras\IdePackages
call :DELINDIR %ROOTDIR%\Extras\Extensions\LibTiff
call :DELINTREE %ROOTDIR%\Extras\Demos
call :DELINTREE %ROOTDIR%\Extras\Tools
call :DELINTREE %ROOTDIR%\Extras\Contrib

goto :END

:DELINDIR
  pushd %1
  echo Processing dir: %1
  del /q /f %EXTS% 2>nul 1>nul
  popd
goto :EOF 

:DELINTREE
  pushd %1
  echo Processing dir tree: %1
  del /q /f /s %EXTS% 2>nul 1>nul
  popd
goto :EOF


:END
echo Clean finished
