{
  Copyright 2019-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

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
  It can also find and return a "bundled" FPC version with CGE.

  The FpcCfgLocation, if returned non-empty, indicates that you should
  pass "-n @FpcCfgLocation" to FPC call.
  This is right now used to make "bundled" FPC version work.
  Internally, in this case, this routine will generate (or update) proper FPC config file.

  @raises EExecutableNotFound When the exe is not found and ExceptionWhenMissing. }
function FindExeFpcCompiler(const ExceptionWhenMissing: Boolean; out FpcCfgLocation: String): String;
function FindExeFpcCompiler(const ExceptionWhenMissing: Boolean = true): String;

{ Find the main executable of Lazarus IDE.
  This uses FindExeLazarus for common executable names like 'lazarus' or 'lazarus-ide'.

  @raises EExecutableNotFound When the exe is not found and ExceptionWhenMissing. }
function FindExeLazarusIDE(const ExceptionWhenMissing: Boolean = true): String;

{ Find lazbuild.
  @raises EExecutableNotFound When the exe is not found and ExceptionWhenMissing. }
function FindExeLazbuild(const ExceptionWhenMissing: Boolean = true): String;

{ Find the path of Delphi installation.
  Ends with PathDelim.

  Overloaded version with AppExe argument returns the IDE exe (that should be used
  to open pas files), which is practically always inside "bin/bds.exe" under Delphi path.

  When missing: ExceptionWhenMissing -> raise EExecutableNotFound,
  otherwise return ''. }
function FindDelphiPath(const ExceptionWhenMissing: Boolean): String;
function FindDelphiPath(const ExceptionWhenMissing: Boolean; out AppExe: String): String;

implementation

uses CastleFilesUtils, CastleStringUtils, CastleLog,
  {$ifdef MSWINDOWS} Registry, {$endif}
  ToolArchitectures, ToolCommonUtils;

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

function FindExeFpcCompiler(const ExceptionWhenMissing: Boolean; out FpcCfgLocation: String): String;

  procedure UpdateBundledFpcCfg(const FpcCfgLocation: String; const FpcPath: String);
  const
    SAutoGenerated = 'AUTO-GENERATED BY CASTLE GAME ENGINE.';
    SContents =
      '# ' + SAutoGenerated + NL +
      '#' + NL +
      '# This file may be overwritten by Castle Game Engine to make sure it contains proper' + NL +
      '# FPC configuration (in particular, paths to FPC standard units).' + NL +
      '#' + NL +
      '# Advanced: to edit this file manually, just remove the above line "AUTO-GENERATED BY...",' + NL +
      '# then CGE will never modify this file. And then you can safely edit this file yourself.' + NL +
      '# But: in this case you are responsible to update this file.' + NL +
      '# E.g. if you move CGE location, along with FPC location.' + NL +
      '# Or if CGE will require here anything specific in future versions (though we try to avoid any requirements' + NL +
      '# in regards to options in this file).' + NL +
      '#' + NL +
      '# Note: you can always remove this file to make it auto-created at next invocation' + NL +
      '# of this FPC from CGE build tool / editor.' + NL +
      NL;
  var
    Contents, CurrentContents: String;
  begin
    Contents := SContents + '-Fu' + InclPathDelim(FpcPath) + 'units' + PathDelim + '$fpctarget' + PathDelim + '*' + NL;
    if not FileExists(FpcCfgLocation) then
    begin
      WritelnVerbose('Creating FPC config in "' + FpcCfgLocation + '"');
      StringToFile(FpcCfgLocation, Contents);
    end else
    begin
      CurrentContents := FileToString(FpcCfgLocation);
      if CurrentContents <> Contents then
      begin
        if Pos(SAutoGenerated, CurrentContents) <> 0 then
        begin
          WritelnVerbose('Updating FPC config in "' + FpcCfgLocation + '"');
          StringToFile(FpcCfgLocation, Contents);
        end else
          WritelnVerbose('Not updating FPC config, as it does not have "AUTO-GENERATED..." string, in "' + FpcCfgLocation + '"');
      end;
    end;
  end;

var
  BundledFpcPath, BundledFpcExe: String;
begin
  FpcCfgLocation := '';

  Result := FindExeFpc('fpc.sh');
  if Result = '' then
    Result := FindExeFpc('fpc');
  if (Result = '') and (CastleEnginePath <> '') then
  begin
    BundledFpcPath := CastleEnginePath + 'tools' + PathDelim +
      'contrib' + PathDelim +
      'fpc' + PathDelim;
    BundledFpcExe := BundledFpcPath + 'bin' + PathDelim + 'fpc' + ExeExtension;
    if FileExists(BundledFpcExe) then
    begin
      FpcCfgLocation := BundledFpcPath + 'fpc-cge.cfg';
      UpdateBundledFpcCfg(FpcCfgLocation, BundledFpcPath);
      Result := BundledFpcExe;
    end;
  end;

  if (Result = '') and ExceptionWhenMissing then
    raise EExecutableNotFound.Create('Cannot find "fpc" program. Make sure it is installed, and available on environment variable $PATH. If you use the CGE editor, you can also set FPC location in "Preferences".');
end;

function FindExeFpcCompiler(const ExceptionWhenMissing: Boolean): String;
var
  FpcCfgLocation: String;
begin
  Result := FindExeFpcCompiler(ExceptionWhenMissing, FpcCfgLocation);
end;

function FindExeLazarusIDE(const ExceptionWhenMissing: Boolean): String;
begin
  Result := FindExeLazarus('lazarus');
  if Result = '' then
  begin
    { Alternative possible Lazarus executable name on non-Windows,
      if installed from deb files or by "make install". }
    Result := FindExeLazarus('lazarus-ide');
  end;

  if (Result = '') and ExceptionWhenMissing then
    // Note: FormProject using this message also for ErrorBox, so make sure it looks sensible.
    raise EExecutableNotFound.Create('Cannot find "lazarus" or "lazarus-ide" program. Make sure it is installed, and available on environment variable $PATH. If you use the CGE editor, you can also set Lazarus location in "Preferences".');
end;

function FindExeLazbuild(const ExceptionWhenMissing: Boolean): String;
begin
  Result := FindExeLazarus('lazbuild');
  if (Result = '') and ExceptionWhenMissing then
    // Note: FormProject using this message also for ErrorBox, so make sure it looks sensible.
    raise EExecutableNotFound.Create('Cannot find "lazbuild" program. Make sure it is installed, and available on environment variable $PATH. If you use the CGE editor, you can also set Lazarus location in "Preferences".');
end;

function FindDelphiPath(const ExceptionWhenMissing: Boolean; out AppExe: String): String;
{$ifdef MSWINDOWS}

{ Our algorithm to find Delphi location in the registry (using some guesswork and regedit searching, confirmed by
  https://docwiki.embarcadero.com/RADStudio/Sydney/en/System_Registry_Keys_for_IDE_Visual_Settings
  https://stackoverflow.com/questions/6870282/how-are-delphi-environment-variables-such-as-bds-evaluated ) :

  - Read subdirs from current user: HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\
  - Read subdirs from machine: HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Embarcadero\BDS\
  - Each subdirectory is the version number. Pick the latest.
    Pick user-specific latest, if both HKEY_CURRENT_USER and HKEY_LOCAL_MACHINE will have the same versions.
    To compare them, treat as floats (as they look like "22.0").
  - Inside the chosen subdir, use:
    - key "App" which is like "c:\program files (x86)\embarcadero\studio\22.0\bin\bds.exe".
    - key "RootDir", like "c:\program files (x86)\embarcadero\studio\22.0\"
      (without "bin" or exe inside).

  Things inside Delphi path:

    bin/bds.exe - Delphi IDE
    bin/dcc[xxx].exe - Delphi compiler, name determines target OS/arch.
      See CompileDelphi for a list.

  See also
  https://docwiki.embarcadero.com/RADStudio/Sydney/en/Delphi_Compiler
  https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Delphi_Toolchains
  https://docwiki.embarcadero.com/RADStudio/Alexandria/en/IDE_Command_Line_Switches_and_Options

}

var
  R: TRegistry;
  LargestKeyAsFloat: Double;
  LargestKeyDelphiRootDir: String;
  LargestKeyDelphiApp: String;

  procedure ScanRegistry(const RootKey: HKEY; const BasePath: UnicodeString);
  var
    KeyName: UnicodeString;
    KeyNames: TUnicodeStringArray;
    DelphiRootDir, DelphiApp: String;
    KeyFloat: Double;
  begin
    R.RootKey := RootKey;

    R.OpenKeyReadOnly(BasePath);
    KeyNames := R.GetKeyNames;
    R.CloseKey;

    for KeyName in KeyNames do
      if TryStrToFloatDot(UTF8Encode(KeyName), KeyFloat) and
         (KeyFloat >= LargestKeyAsFloat) then
      begin
        R.OpenKeyReadOnly(BasePath + KeyName);
        DelphiRootDir := R.ReadString('RootDir');
        R.CloseKey;

        R.OpenKeyReadOnly(BasePath + KeyName);
        DelphiApp := R.ReadString('App');
        R.CloseKey;

        if (DelphiRootDir <> '') and (DelphiApp <> '') then
        begin
          LargestKeyAsFloat := KeyFloat;
          LargestKeyDelphiRootDir := DelphiRootDir;
          LargestKeyDelphiApp := DelphiApp;
          //WritelnLog('Delphi %f found in %s', [LargestKeyAsFloat, LargestKeyDelphiRootDir]);
        end;
      end;
  end;

begin
  R := TRegistry.Create(KEY_READ);
  try
    LargestKeyAsFloat := 0;
    ScanRegistry(HKEY_LOCAL_MACHINE, 'SOFTWARE\WOW6432Node\Embarcadero\BDS\');
    ScanRegistry(HKEY_CURRENT_USER, 'SOFTWARE\Embarcadero\BDS\');

    Result := LargestKeyDelphiRootDir;
    if Result <> '' then
    begin
      Result := InclPathDelim(Result);
      AppExe := LargestKeyDelphiApp;
    end;
  finally FreeAndNil(R)end;
{$else}
begin
  // just in case Delphi IDE will be available on non-Windows some day
  Result := FindExe('dcc32');
  if Result <> '' then
    Result := ExtractFilePath(Result);
{$endif}

  if (Result = '') and ExceptionWhenMissing then
    raise EExecutableNotFound.Create('Cannot find Delphi installation in the registry. Make sure Delphi is installed correctly.');
end;

function FindDelphiPath(const ExceptionWhenMissing: Boolean): String;
var
  IgnoreAppExe: String;
begin
  Result := FindDelphiPath(ExceptionWhenMissing, IgnoreAppExe);
end;

end.
