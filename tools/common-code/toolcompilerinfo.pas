{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".
  Parts of this file are based on FPC packages/fcl-process/src/process.pp ,
  which conveniently uses *exactly* the same license as Castle Game Engine.

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Information about compiler and IDE. }
unit ToolCompilerInfo;

interface

uses SysUtils,
  CastleUtils;

var
  { If set, use this path to search for FPC and related utils. }
  FpcCustomPath: String;
  { If set, use this path to search for Lazarus and related utils. }
  LazarusCustomPath: String;

{ Executable path of a tool ExeName, that is part of FPC.
  For example 'fpc', or 'fppkg'.
  Returns '' if not found, just like vanilla FindExe. }
function FindExeFpc(const ExeName: String): String;

{ Executable path of a tool ExeName, that is part of Lazarus.
  For example 'lazarus', or 'lazbuild'.
  Returns '' if not found, just like vanilla FindExe. }
function FindExeLazarus(const ExeName: String): String;

{ Extend $PATH environment variable as necessary to add FpcCustomPath
  and LazarusCustomPath. }
function PathExtendForFpcLazarus(const PathList: String): String;

type
  EExecutableNotFound = class(EShortErrorMessage);

{ Find the executable of FPC compiler.
  It uses FindExeFpc, searching for 'fpc.sh' (script set by fpcupdeluxe
  that should be used to run FPC)
  or just 'fpc' (normal way to run FPC).
  It raises EExecutableNotFound in case it failed.
  @raises EExecutableNotFound }
function FindExeFpcCompiler: String;

{ Find the main executable of Lazarus IDE.
  This uses FindExeLazarus for common executable names like 'lazarus' or 'lazarus-ide'.
  @raises EExecutableNotFound When the exe is not found. }
function FindExeLazarusIDE: String;

implementation

uses CastleFilesUtils, CastleStringUtils,
  ToolArchitectures;

function FindExeFpc(const ExeName: String): String;
begin
  if FpcCustomPath <> '' then
  begin
    Result := InclPathDelim(FpcCustomPath) + ExeName + ExeExtension;
    if RegularFileExists(Result) then
      Exit;
    Result := InclPathDelim(FpcCustomPath) +
      'bin' + PathDelim +
      CPUToString(DefaultCPU) + '-' + OSToString(DefaultOS) + PathDelim +
      ExeName + ExeExtension;
    if RegularFileExists(Result) then
      Exit;
  end;
  Result := FindExe(ExeName);
end;

function FindExeLazarus(const ExeName: String): String;
begin
  if LazarusCustomPath <> '' then
  begin
    Result := InclPathDelim(LazarusCustomPath) + ExeName + ExeExtension;
    if RegularFileExists(Result) then
      Exit;
  end;
  Result := FindExe(ExeName);
end;

function PathExtendForFpcLazarus(const PathList: String): String;

  function PathAppend(const P, NewPart: String): String;
  begin
    if P = '' then
      Result := NewPart
    else
      { Add NewPart at the beginning, this way FPC explicitly specified
        in CGE editor "Preferences" should override FPC on PATH. }
      Result := NewPart + PathSeparator + P;
  end;

var
  FpcBin: String;
begin
  Result := PathList;

  if FpcCustomPath <> '' then
  begin
    FpcBin := InclPathDelim(FpcCustomPath) +
      'bin' + PathDelim +
      CPUToString(DefaultCPU) + '-' + OSToString(DefaultOS);
    if DirectoryExists(FpcBin) then
      Result := PathAppend(Result, FpcBin)
    else
      Result := PathAppend(Result, FpcCustomPath);
  end;

  if LazarusCustomPath <> '' then
    Result := PathAppend(Result, LazarusCustomPath);
end;

function FindExeFpcCompiler: String;
begin
  Result := FindExeFpc('fpc.sh');
  if Result = '' then
    Result := FindExeFpc('fpc');
  if Result = '' then
    raise EExecutableNotFound.Create('Cannot find "fpc" program. Make sure it is installed, and available on environment variable $PATH. If you use the CGE editor, you can also set FPC location in "Preferences".');
end;

function FindExeLazarusIDE: String;
begin
  Result := FindExeLazarus('lazarus');
  if Result = '' then
  begin
    { Alternative possible Lazarus executable name on non-Windows,
      if installed from deb files or by "make install". }
    Result := FindExeLazarus('lazarus-ide');
    if Result = '' then
      // Note: FormProject using this message also for ErrorBox, so make sure it looks sensible.
      raise EExecutableNotFound.Create('Cannot find "lazarus" or "lazarus-ide" program. Make sure it is installed, and available on environment variable $PATH. If you use the CGE editor, you can also set Lazarus location in "Preferences".');
  end;
end;

end.
