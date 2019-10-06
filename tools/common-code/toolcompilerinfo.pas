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

uses SysUtils;

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

type
  EExecutableNotFound = class(Exception);

{ Find the executable of FPC compiler.
  This is just like FindExeFpc('fpc')
  but raises EExecutableNotFound in case it failed.
  @raises EExecutableNotFound }
function FindExeFpcCompiler: String;

{ Find the executable of Lazarus IDE.
  This is just like FindExeLazarus('lazarus')
  but raises EExecutableNotFound in case it failed.
  @raises EExecutableNotFound }
function FindExeLazarusIDE: String;

implementation

uses CastleUtils, CastleFilesUtils,
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

function FindExeFpcCompiler: String;
begin
  Result := FindExeFpc('fpc');
  if Result = '' then
    raise EExecutableNotFound.Create('Cannot find "fpc" program. Make sure it is installed, and available on environment variable $PATH. If you use the CGE editor, you can also set FPC location in "Preferences".');
end;

function FindExeLazarusIDE: String;
begin
  Result := FindExeLazarus('lazarus');
  if Result = '' then
    // Note: FormProject using this message also for ErrorBox, so make sure it looks sensible.
    raise EExecutableNotFound.Create('Cannot find "lazarus" program. Make sure it is installed, and available on environment variable $PATH. If you use the CGE editor, you can also set Lazarus location in "Preferences".');
end;


initialization
  // This way CGE editor passes the FPC/Lazarus locations to the build tool
  FpcCustomPath := GetEnvironmentVariable('CASTLE_FPC_CUSTOM_PATH');
  LazarusCustomPath := GetEnvironmentVariable('CASTLE_LAZARUS_CUSTOM_PATH');
end.
