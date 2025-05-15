{
  Copyright 2014-2025 Michalis Kamburelis.

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

{ Common utilities shared by various tools. }
unit ToolCommonUtils;

interface

uses Classes,
  CastleStringUtils;

var
  { Trivial verbosity global setting. }
  Verbose: boolean = false;

{ In case of console application with Verbose, writes to stduot.
  Otherwise, only to WritelnLog. }
procedure WritelnVerbose(const S: String); overload;
procedure WritelnVerbose(const SFormat: String; const Args: array of const); overload;

{ Like @link(FindExe), but additionally look for the exe in
  Castle Game Engine bin/ subdirectory. }
function FindExeCastleTool(const ExeName: String): String;

var
  { When non-empty, determines the CastleEnginePath result unconditionally. }
  CastleEngineOverridePath: String;

{ Path to CGE main directory.
  Autodetected or obtained from $CASTLE_ENGINE_PATH environment variable.

  Returns empty String if it wasn't possible to get a valid value.
  Otherwise, the returned path always ends with path delimiter,
  and always exists. }
function CastleEnginePath: String;

{ Determine and create a new (unique, with random number in the name) temp directory. }
function CreateTemporaryDir: String;

var
  { CGE manifest filename, designating CGE project root.
    Can be adjusted using `castle-engine --manifest-name=xxx`. }
  ManifestName: String = 'CastleEngineManifest.xml';

implementation

uses SysUtils,
  CastleFilesUtils, CastleUtils, CastleUriUtils, CastleLog,
  CastleInternalArchitectures;

procedure WritelnVerbose(const S: String);
begin
  if Verbose and IsConsole then
    WriteLn(S)
  else
    WriteLnLog(S);
end;

procedure WritelnVerbose(const SFormat: String; const Args: array of const);
begin
  WritelnVerbose(Format(SFormat, Args));
end;

function FindExeCastleTool(const ExeName: String): String;
begin
  if CastleEnginePath <> '' then
  begin
    Result := CastleEnginePath + 'bin' + PathDelim + ExeName + ExeExtension;
    if RegularFileExists(Result) then
      Exit;

    { Look for exe wrapped in macOS application bundle,
      necessary to find castle-model-viewer, castle-image-viewer in CGE bin. }
    {$ifdef DARWIN}
    Result := CastleEnginePath + 'bin' + PathDelim +
      ExeName + '.app' + PathDelim +
      'Contents' + PathDelim +
      'MacOS' + PathDelim +
      ExeName + ExeExtension;
    if RegularFileExists(Result) then
      Exit;
    {$endif}

    Result := CastleEnginePath + 'tools' + PathDelim + 'contrib' + PathDelim +
      CPUToString(DefaultCPU) + '-' + OSToString(DefaultOS) + PathDelim +
      ExeName + ExeExtension;
    if RegularFileExists(Result) then
      Exit;
  end;
  Result := FindExe(ExeName);
end;

function GetCastleEnginePathFromExeName: String; forward;

function GetCastleEnginePathFromEnv: String;

  { Do everything possible to make paths that actually point to the same
    location be the same string. }
  function PathCanonical(const S: String): String;
  begin
    Result := S;
    if Length(Result) <> 1 then // do not change root directory '/'
      Result := ExclPathDelim(Result);
    Result := ExpandFileName(Result);
    Result := SReplaceChars(Result, '\', '/');
  end;

var
  EnginePathFromExe: String;
begin
  Result := GetEnvironmentVariable('CASTLE_ENGINE_PATH');
  if Result = '' then
    Exit;

  Result := InclPathDelim(Result);

  if not DirectoryExists(Result) then
  begin
    WritelnWarning('$CASTLE_ENGINE_PATH environment variable points to a non-existing directory "%s", ignoring',
      [Result]);
    Exit('');
  end;

  { $CASTLE_ENGINE_PATH environment variable may point to the directory
    - containing castle_game_engine/ as subdirectory (deprecated but allowed)
    - or containing castle-engine/ as subdirectory (deprecated but allowed)
    - or pointing straight to castle_game_engine/ or castle-engine/ directory. }
  if DirectoryExists(Result + 'castle_game_engine') then
  begin
    WritelnWarning('$CASTLE_ENGINE_PATH environment variable points to a parent of "castle_game_engine" directory: "%s". This is deprecated, better change $CASTLE_ENGINE_PATH to include the "castle_game_engine" suffix.',
      [Result]);
    Result := Result + 'castle_game_engine' + PathDelim
  end else
  if DirectoryExists(Result + 'castle-engine') then
  begin
    WritelnWarning('$CASTLE_ENGINE_PATH environment variable points to a parent of "castle-engine" directory: "%s". This is deprecated, better change $CASTLE_ENGINE_PATH to include the "castle-engine" suffix.',
      [Result]);
    Result := Result + 'castle-engine' + PathDelim;
  end;

  if not DirectoryExists(Result + 'src') then
    WritelnWarning('$CASTLE_ENGINE_PATH environment variable defined, but we cannot find Castle Game Engine sources inside: "%s". We try to continue, assuming that engine unit paths are already specified within fpc.cfg file, otherwise compilation will fail.',
      [Result]);
  if not DirectoryExists(Result + 'tools' + PathDelim + 'build-tool' + PathDelim + 'data') then
    WritelnWarning('$CASTLE_ENGINE_PATH environment variable defined, but we cannot find build tool data inside: "%s". We try to continue, but some packaging operations will fail.',
      [Result]);

  if Result <> '' then
  begin
    EnginePathFromExe := GetCastleEnginePathFromExeName;
    if (EnginePathFromExe <> '') and
       (not SameFileName(PathCanonical(Result), PathCanonical(EnginePathFromExe))) then
    begin
      WritelnWarning('$CASTLE_ENGINE_PATH environment variable points to a different directory than the one detected from the executable path: "%s" vs "%s". This may be a mistake, possibly you have two (different) engine versions installed. Please check your environment, likely remove one of the engine versions or undefine CASTLE_ENGINE_PATH to not point to the engine that shall be unused.', [
        Result,
        EnginePathFromExe
      ]);
    end;
  end;
end;

{ Check is Path a sensible CGE sources path.
  Requires Path to end with PathDelim. }
function CheckCastlePath(const Path: String): Boolean;
begin
  Result :=
    DirectoryExists(Path + 'src') and
    DirectoryExists(Path + 'tools' + PathDelim + 'build-tool' + PathDelim + 'data');
end;

function GetCastleEnginePathFromExeName: String;
var
  ToolDir: String;
begin
  try
    // knowingly using deprecated ExeName, that should be non-deprecated and internal here
    {$warnings off}
    ToolDir := ExtractFileDir(ExeName);
    {$warnings on}
    { in case we're inside macOS bundle, use bundle path.
      This makes detection in case of CGE editor work OK. }
    {$ifdef DARWIN}
    if BundlePath <> '' then
      ToolDir := ExtractFileDir(ExclPathDelim(BundlePath));
    {$endif}

    { Check ../ of current exe, makes sense in released CGE version when
      tools are precompiled in bin/ subdirectory. }
    Result := InclPathDelim(ExtractFileDir(ToolDir));
    if CheckCastlePath(Result) then
      Exit;
    { Check ../../ of current exe, makes sense in development when
      each tool is compiled by various scripts in tools/xxx/ subdirectory. }
    Result := InclPathDelim(ExtractFileDir(ExtractFileDir(ToolDir)));
    if CheckCastlePath(Result) then
      Exit;

    Result := '';
  except
    on EExeNameNotAvailable do
      WritelnVerbose('Cannot detect CGE path because ExeName not available on this platform, and $CASTLE_ENGINE_PATH not defined');
  end;
end;

function GetCastleEnginePathSystemWide: String;
begin
  {$ifdef UNIX}
  Result := '/usr/src/castle-engine/';
  if CheckCastlePath(Result) then
    Exit;

  Result := '/usr/local/src/castle-engine/';
  if CheckCastlePath(Result) then
    Exit;
  {$endif}

  Result := '';
end;

var
  CastleEnginePathIsCached: Boolean;
  CastleEnginePathCached: String;

function CastleEnginePath: String;
begin
  { In case of CastleEngineOverridePath, ignore CastleEnginePathCached.
    This avoids clearing this cache when CastleEngineOverridePath changes
    at runtime, like in editor. }
  if CastleEngineOverridePath <> '' then
    Result := InclPathDelim(CastleEngineOverridePath)
  else
  if CastleEnginePathIsCached then
    Result := CastleEnginePathCached
  else
  begin
    // try to find CGE on $CASTLE_ENGINE_PATH
    Result := GetCastleEnginePathFromEnv;
    // try to find CGE on path relative to current exe
    if Result = '' then
      Result := GetCastleEnginePathFromExeName;
    // try to find CGE on system-wide paths
    if Result = '' then
      Result := GetCastleEnginePathSystemWide;

    if Result <> '' then
      WritelnVerbose('Castle Game Engine directory detected: ' + Result)
    else
      WritelnWarning('Castle Game Engine directory cannot be detected:' + NL +
        '- $CASTLE_ENGINE_PATH environment variable not defined, or points to an incorrect directory.' + NL +
        '- Moreover we cannot find Castle Game Engine looking at parent directory of this program exe.');

    CastleEnginePathIsCached := true;
    CastleEnginePathCached := Result;
  end;
end;

function CreateTemporaryDir: String;
begin
  Result := InclPathDelim(GetTempDir(false)) +
    ApplicationName + IntToStr(Random(1000000));
  CheckForceDirectories(Result);
  WritelnVerbose('Created temporary dir for package: ' + Result);
end;

end.
