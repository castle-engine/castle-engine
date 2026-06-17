{
  Copyright 2019-2025 Michalis Kamburelis.

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

{ Executable path of a tool with given ExeName (one of ExeNameList items),
  that is part of FPC.
  For example 'fpc' or 'fppkg'.
  ExeNameList is a list, may contain multiple allowed names
  (in the order from most preferred to least preferred),
  must have at least one item.

  Searches first in FpcCustomPath, then in $PATH.

  Returns '' if not found, just like vanilla FindExe. }
function FindExeFpc(const ExeNameList: array of String): String;

{ Executable path of a tool ExeName, that is part of Lazarus.
  For example 'lazarus', or 'lazbuild'.
  Returns '' if not found, just like vanilla FindExe. }
function FindExeLazarus(const ExeName: String): String;

{ Extend $PATH environment variable as necessary to add FpcCustomPath
  and LazarusCustomPath. }
function PathExtendForFpcLazarus(const PathList: String): String;

{ Write environment information to the standard output.
  See "castle-engine --help" for valid arguments after "castle-engine output-environment",
  these are values allowed for OutputKey. }
procedure DoOutputEnvironment(const OutputKey: String);

type
  EExecutableNotFound = class(EShortErrorMessage);

{ Find the executable of FPC compiler.

  It uses FindExeFpc, searching for 'fpc.sh' (script set by fpcupdeluxe
  that should be used to run FPC)
  or just 'fpc' (normal way to run FPC).
  It can also find and return a "bundled" FPC version with CGE.

  The FpcStandardUnitsPath, if returned non-empty, indicates that you should
  pass "-Fu<FpcStandardUnitsPath>" to FPC call.
  This is right now used to make "bundled" FPC version work.

  @raises EExecutableNotFound When the exe is not found and ExceptionWhenMissing. }
function FindExeFpcCompiler(const ExceptionWhenMissing: Boolean; out FpcStandardUnitsPath: String): String;
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

  When global @link(DelphiVersion) is set, we validate and return information
  about the given Delphi version.

  @raises EExecutableNotFound(
    If we cannot find Delphi installation, and ExceptionWhenMissing is @true,
    we raise EExecutableNotFound.

    This happens if any Delphi version is missing (when DelphiVersion is empty),
    or if the specific Delphi version is missing (when DelphiVersion is set).
    We deliberately @italic(do not fall back) from the requested DelphiVersion
    -> largest Delphi version, instead this should be signalled to user
    as an issue that DelphiVersion is missing.

    If ExceptionWhenMissing is @false, we return '' in these situations,
    and AppExe is also set to ''.
  ) }
function FindDelphiPath(const ExceptionWhenMissing: Boolean): String;
function FindDelphiPath(const ExceptionWhenMissing: Boolean; out AppExe: String): String;

var
  { Delphi version.

    This is like
    - 23.0 for Delphi 12
    - 37.0 for Delphi 13

    This corresponds to the version number in the registry, under
    - HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Embarcadero\BDS\
    - HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\

    This also corresponds to the version number in the Delphi path, underneath
    C:\Program Files (x86)\Embarcadero\Studio
    (if you installed to the default location).

    See "Product Version" on
    https://docwiki.embarcadero.com/RADStudio/Florence/en/Compiler_Versions . }
  DelphiVersion: String = '';

implementation

uses CastleFilesUtils, CastleStringUtils, CastleLog,
  CastleInternalArchitectures,
  {$ifdef MSWINDOWS} Registry, {$endif}
  ToolCommonUtils;

function FindExeFpc(const ExeNameList: array of String): String;
var
  ExeName: String;
begin
  { First check FpcCustomPath, if not empty. }
  if FpcCustomPath <> '' then
  begin
    for ExeName in ExeNameList do
    begin
      // look for ExeName in FpcCustomPath, without bin/CPU-OS/ subdirectory
      Result := InclPathDelim(FpcCustomPath) + ExeName + ExeExtension;
      if RegularFileExists(Result) then
        Exit;

      // look for ExeName in FpcCustomPath, with bin/CPU-OS/ subdirectory
      Result := InclPathDelim(FpcCustomPath) +
        'bin' + PathDelim +
        CPUToString(DefaultCPU) + '-' + OSToString(DefaultOS) + PathDelim +
        ExeName + ExeExtension;
      if RegularFileExists(Result) then
        Exit;
    end;
  end;

  { Last, check on $PATH. }
  for ExeName in ExeNameList do
  begin
    Result := FindExe(ExeName);
    if Result <> '' then
      Exit;
  end;
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

procedure DoOutputEnvironment(const OutputKey: String);
var
  StandardUnitsPath: String;
begin
  case OutputKey of
    'fpc-exe':
      Writeln(FindExeFpcCompiler(false));
    'fpc-standard-units-path':
    begin
      FindExeFpcCompiler(false, StandardUnitsPath);
      Writeln(StandardUnitsPath);
    end
    else raise Exception.CreateFmt('Unsupported output key: "%s"', [OutputKey]);
  end;
end;

function FindExeFpcCompiler(const ExceptionWhenMissing: Boolean; out FpcStandardUnitsPath: String): String;
var
  BundledFpcPath, BundledFpcExe: String;
begin
  FpcStandardUnitsPath := '';

  // find FPC on FpcCustomPath or $PATH
  Result := FindExeFpc([
    // FPC script from fpcupdeluxe
    'fpc.sh',

    { In case "bundled FPC" is on $PATH, call it using 'fpc-cfg', not just 'fpc'
      (the latter case would mean it cannot find standard units).
      And it's actually normal that "bundled FPC" is on $PATH, when calling
      build tool from
      - VS Code extension, which prepends the FPC location on PATH
        of the called process.
      - CGE editor, that also prepends the FPC location on PATH,
        see PathExtendForFpcLazarus. }
    'fpc-cge',

    // regular FPC executable
    'fpc'
  ]);

  // fallback to use bundled FPC
  if (Result = '') and (CastleEnginePath <> '') then
  begin
    BundledFpcPath := CastleEnginePath + 'tools' + PathDelim +
      'contrib' + PathDelim +
      'fpc' + PathDelim;
    BundledFpcExe := BundledFpcPath + 'bin' + PathDelim + 'fpc' + ExeExtension;
    if FileExists(BundledFpcExe) then
    begin
      FpcStandardUnitsPath := BundledFpcPath + 'units' + PathDelim + '$fpctarget' + PathDelim + '*';
      Result := BundledFpcExe;
    end;
  end;

  if (Result = '') and ExceptionWhenMissing then
    raise EExecutableNotFound.Create('Cannot find "fpc" program. Make sure it is installed, and available on environment variable $PATH. If you use the CGE editor, you can also set FPC location in "Preferences".');
end;

function FindExeFpcCompiler(const ExceptionWhenMissing: Boolean): String;
var
  FpcStandardUnitsPath: String;
begin
  Result := FindExeFpcCompiler(ExceptionWhenMissing, FpcStandardUnitsPath);
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
const
  RegistryRoot1 = HKEY_LOCAL_MACHINE;
  RegistryPath1 = 'SOFTWARE\WOW6432Node\Embarcadero\BDS\';

  RegistryRoot2 = HKEY_CURRENT_USER;
  RegistryPath2 = 'SOFTWARE\Embarcadero\BDS\';

var
  R: TRegistry;

  { Read Delphi RootDir and App from registry at PathWithVersion.
    Updates DelphiRootDir, DelphiApp and returns @true only
    if both values are non-empty. }
  function RegistryReadDelphiVersion(const PathWithVersion: UnicodeString;
    var DelphiRootDir, DelphiApp: String): Boolean;
  var
    NewDelphiRootDir, NewDelphiApp: String;
  begin
    R.OpenKeyReadOnly(PathWithVersion);
    NewDelphiRootDir := R.ReadString('RootDir');
    NewDelphiApp := R.ReadString('App');
    R.CloseKey;

    Result := (NewDelphiRootDir <> '') and (NewDelphiApp <> '');
    if Result then
    begin
      DelphiRootDir := NewDelphiRootDir;
      DelphiApp := NewDelphiApp;
    end;
  end;

  { Calculate FindDelphiPath result and AppExe, by scanning the registry
    for the largest Delphi version. }
  function FindLargestDelphiVersion(out AppExe: String): String;
  var
    LargestKeyAsFloat: Double;
    LargestKeyDelphiRootDir: String;
    LargestKeyDelphiApp: String;

    { Scan subdirectories of registry in RootKey + BasePath,
      looking for the largest Delphi version.
      Updates LargestKeyAsFloat, LargestKeyDelphiRootDir, LargestKeyDelphiApp. }
    procedure ScanRegistryForLargest(const RootKey: HKEY; const BasePath: UnicodeString);
    var
      KeyName: UnicodeString;
      KeyNames: TUnicodeStringArray;
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
          if RegistryReadDelphiVersion(BasePath + KeyName,
            LargestKeyDelphiRootDir, LargestKeyDelphiApp) then
          begin
            LargestKeyAsFloat := KeyFloat;
            //WritelnLog('Delphi %f found in %s', [LargestKeyAsFloat, LargestKeyDelphiRootDir]);
          end;
        end;
    end;

  begin { FindLargestDelphiVersion }
    // default out value
    AppExe := '';

    LargestKeyAsFloat := 0;
    ScanRegistryForLargest(RegistryRoot1, RegistryPath1);
    ScanRegistryForLargest(RegistryRoot2, RegistryPath2);

    Result := LargestKeyDelphiRootDir;
    if Result <> '' then
    begin
      Result := InclPathDelim(Result);
      AppExe := LargestKeyDelphiApp;
    end;

    if (Result = '') and ExceptionWhenMissing then
      raise EExecutableNotFound.Create('Cannot find Delphi installation in the registry. Make sure Delphi is installed correctly.');
  end;

  { Calculate FindDelphiPath result and AppExe, by checking the registry
    for a specific Delphi version. }
  function UseDelphiVersion(out AppExe: String): String;
  var
    DelphiRootDir, DelphiApp: String;
  begin
    Result := '';
    // default out value
    AppExe := '';
    DelphiRootDir := '';
    DelphiApp := '';

    R.RootKey := RegistryRoot1;
    if R.KeyExists(RegistryPath1 + DelphiVersion) and
      RegistryReadDelphiVersion(RegistryPath1 + UnicodeString(DelphiVersion),
        DelphiRootDir, DelphiApp) then
    begin
      Result := InclPathDelim(DelphiRootDir);
      AppExe := DelphiApp;
      Exit;
    end;

    R.RootKey := RegistryRoot2;
    if R.KeyExists(RegistryPath2 + DelphiVersion) and
      RegistryReadDelphiVersion(RegistryPath2 + UnicodeString(DelphiVersion),
        DelphiRootDir, DelphiApp) then
    begin
      Result := InclPathDelim(DelphiRootDir);
      AppExe := DelphiApp;
      Exit;
    end;

    if ExceptionWhenMissing then
      raise EExecutableNotFound.CreateFmt('Delphi version "%s" was requested, but not found in the registry. Make sure it is installed correctly. ' +
        'To let the tool automatically detect the latest Delphi version, just do not use the --delphi-version parameter.', [
        DelphiVersion
      ]);
  end;

begin
  R := TRegistry.Create(KEY_READ);
  try
    if DelphiVersion = '' then
      Result := FindLargestDelphiVersion(AppExe)
    else
      Result := UseDelphiVersion(AppExe);
  finally FreeAndNil(R) end;
end;

{$else}

begin
  // just in case Delphi IDE will be available on non-Windows some day
  Result := FindExe('dcc32');
  if Result <> '' then
    Result := ExtractFilePath(Result);
  if (Result = '') and ExceptionWhenMissing then
    raise EExecutableNotFound.Create('Cannot find Delphi installation in the registry. Make sure Delphi is installed correctly.');
end;

{$endif}

function FindDelphiPath(const ExceptionWhenMissing: Boolean): String;
var
  IgnoreAppExe: String;
begin
  Result := FindDelphiPath(ExceptionWhenMissing, IgnoreAppExe);
end;

end.
