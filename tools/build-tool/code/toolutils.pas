{
  Copyright 2014-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities. }
unit ToolUtils;

interface

uses DOM,
  CastleImages, CastleStringUtils, CastleUtils;

{ Copy file, making sure the destination directory exists
  (eventually creating it), and checking result. }
procedure SmartCopyFile(const Source, Dest: string);

function FileSize(const FileName: string): Int64;

var
  { Output path base directory. Empty to use working project directory. }
  OutputPathBase: string = '';

{ Calculate the final location of temporary output files
  (including the castle-engine-output subdir part),
  as an absolute path ending with path delimiter.
  Makes sure the dir exists, if CreateIfNecessary. }
function TempOutputPath(const WorkingDirectory: string;
  const CreateIfNecessary: boolean = true): string;

type
  TReplaceMacros = function (const Source: string): string of object;

const
  MaxAndroidTagLength = 23;

const
  { Interpolation to scale images with highest quality.

    Latest FPC breaks alpha channel at resizing using riLanczos.
    TODO: Submit a patch to FPC to restore previous behavior.

    Index: packages/fcl-image/src/fpinterpolation.inc
    ===================================================================
    --- packages/fcl-image/src/fpinterpolation.inc      (wersja 40746)
    +++ packages/fcl-image/src/fpinterpolation.inc      (kopia robocza)
    @@ -223,7 +223,8 @@
               NewCol.blue:=Min(NewCol.blue+round(Col.blue*f),$ffff);
               NewCol.alpha:=Min(NewCol.alpha+round(Col.alpha*f),$ffff);
             end;
    -        Canvas.Colors[x+dx,y+dy]:=AlphaBlend(Canvas.Colors[x+dx,y+dy], NewCol);
    +        //Canvas.Colors[x+dx,y+dy]:=AlphaBlend(Canvas.Colors[x+dx,y+dy], NewCol);
    +        Canvas.Colors[x+dx,y+dy]:=NewCol;
           end;
         end;
       finally
  }
  BestInterpolation = {$ifdef VER3_0} riLanczos {$else} riBilinear {$endif};

{ Add all parameters in Parameters to Macros.
  Each parameter key is prefixed by ParameterMacroPrefix
  when it is added to the Macros list. }
procedure ParametersAddMacros(const Macros, Parameters: TStringStringMap;
  const ParameterMacroPrefix: String);

{ Find the filename of linker input produced by FPC when called with -Cn .
  Path must contain a final path delimiter.
  Raises exception if not found (or found too many, and it is ambiguous what to use).

  For old FPC, it is just link.res.

  During FPC 3.2.0 -> 3.2.2 development:
  it may be any link<id>.res and unfortunately we don't know the <id>
  (it's not the TProcess.ProcessID of "fpc" process, at least under Windows).

  Since FPC 3.2.2 it is linkfiles<id>.res (and we ignore link<id>.res and linksyms<id>.res). }
function FindLinkRes(const Path: String): String;

{ Set Unix executable bit.
  It will not be able to perform the CHMOD operation on non-Unix OS
  and will log a corresponding warning instead. }
procedure DoMakeExecutable(const PathAndName: String);

{ Simple GUI error box.

  Implemented without depending on GTK, LCL or any other GUI library
  on Unix, as build tool should remain command-line only, to be easy to use
  on servers without GUI libraries installed. We depend on "zenity".

  This is only used when --gui-errors was used. }
procedure ErrorBox(const Message: String);

const
  SCannotFindCgePath = 'Cannot find Castle Game Engine path.' + NL +
    'Solutions:' + NL +
    '1. Run from CGE editor (where you can configure CGE path in "Preferences",' + NL +
    '2. Or place the engine tools (exe) inside the bin/ subdirectory of the engine,' + NL +
    '3. Or set the environment variable $CASTLE_ENGINE_PATH.';

function CachePath: String;

{ Generate GUID using Seed to determine it, instead of using Random.
  This is useful when you want to generate GUID that is stable between runs
  (for example, when you want to have regenerate_auto_files_in_all_examples.sh
  have stable output).

  This is not a cryptographically secure hash, it's just a simple hash
  to make GUID stable. }
function CreateGUIDFromHash(const Seed: String): TGuid;

implementation

uses {$ifdef UNIX} BaseUnix, {$endif}
  {$ifdef MSWINDOWS} Windows, {$endif}
  Classes, Process, SysUtils,
  CastleFilesUtils, CastleURIUtils, CastleLog, CastleXMLUtils, CastleFindFiles,
  ToolCommonUtils;

procedure SmartCopyFile(const Source, Dest: string);
begin
  CheckForceDirectories(ExtractFileDir(Dest));
  CheckCopyFile(Source, Dest);
end;

function FileSize(const FileName: string): Int64;
var
  SourceFile: TFileStream;
begin
  SourceFile := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := SourceFile.Size;
  finally FreeAndNil(SourceFile) end;
end;

var
  FOutputPath: String;
  FOutputPathForWorkingDirectory: String;

function TempOutputPath(const WorkingDirectory: String; const CreateIfNecessary: Boolean): String;
const
  OutputNoteContents = {$I ../embedded_templates/template-castle-engine-output-warning.txt.inc};
var
  OutputNote: String;
begin
  if (FOutputPath = '') or
     { Do not reuse cached FOutputPath for different WorkingDirectory,
       otherwise doing DoCompile for different projects in same run would accidentally
       reuse output.
       Testcase: "castle-engine cache", each mode (release valgrind debug)
       must build new files. }
     (FOutputPathForWorkingDirectory <> WorkingDirectory) then
  begin
    if OutputPathBase = '' then
      FOutputPath := InclPathDelim(WorkingDirectory)
    else
      FOutputPath := InclPathDelim(OutputPathBase);
    FOutputPath += 'castle-engine-output' + PathDelim;

    if CreateIfNecessary then
    begin
      CheckForceDirectories(FOutputPath);

      OutputNote := FOutputPath + 'DO-NOT-COMMIT-THIS-DIRECTORY.txt';
      if not RegularFileExists(OutputNote) then
        StringToFile(OutputNote, OutputNoteContents);
    end;

    FOutputPathForWorkingDirectory := WorkingDirectory;
  end;

  Result := FOutputPath;
end;

procedure ParametersAddMacros(const Macros, Parameters: TStringStringMap;
  const ParameterMacroPrefix: String);
var
  Pair: TStringStringMap.TDictionaryPair;
begin
  for Pair in Parameters do
    Macros.Add(UpperCase(ParameterMacroPrefix + Pair.Key), Pair.Value);
end;

type
  TFindLinkResHandler = class
    FileName: String;
    procedure FoundFile(const FileInfo: TFileInfo; var StopSearch: boolean);
  end;

procedure TFindLinkResHandler.FoundFile(const FileInfo: TFileInfo; var StopSearch: boolean);
begin
  if FileName <> '' then
    raise Exception.CreateFmt('Multiple linker input files in the same directory: "%s" and "%s". Delete all "link*.res" here and run the process again', [
      FileName,
      FileInfo.AbsoluteName
    ]);
  FileName := FileInfo.AbsoluteName;
end;

function FindLinkRes(const Path: String): String;
var
  Handler: TFindLinkResHandler;
  LinkFilesRes: String;
begin
  LinkFilesRes := CombinePaths(Path, 'linkfiles.res');
  if FileExists(LinkFilesRes) then
  begin
    { Latest FPC 3.3.1 introduced linkfiles.res file
      (see FPC sources compiler/systems/t_bsd.pas , started in commit
      https://github.com/graemeg/freepascal/commit/36d634bd87427e480d4f82344c5f7e5c7d6b57eb
      it seems ).
      It contains what we need: the list of .o files.
      The link<some-process-id>.res is also generated, but it is no longer useful for us.
      So exit with "linkfiles.res", without causing "Multiple linker input files..." error. }
    Exit(LinkFilesRes);
  end;

  { First try to match linkfiles*.res, ignoring other link*.res.
    This is good for FPC >= 3.2.2. }
  Handler := TFindLinkResHandler.Create;
  try
    FindFiles(Path, 'linkfiles*.res', false, @Handler.FoundFile, []);
    Result := Handler.FileName;
    if Result <> '' then
      Exit;
  finally FreeAndNil(Handler) end;

  { If no linkfiles*.res found, try to match any link*.res.
    This is good for FPC development between 3.2.0 and 3.2.2. }
  Handler := TFindLinkResHandler.Create;
  try
    FindFiles(Path, 'link*.res', false, @Handler.FoundFile, []);
    Result := Handler.FileName;
    if Result <> '' then
      Exit;
  finally FreeAndNil(Handler) end;

  raise Exception.CreateFmt('Cannot find any linker input file in the directory "%s"', [
    Path
  ]);
end;

procedure DoMakeExecutable(const PathAndName: String);
{$ifdef UNIX}
var
  ChmodResult: CInt;
begin
  ChmodResult := FpChmod(PathAndName,
    S_IRUSR or S_IWUSR or S_IXUSR or
    S_IRGRP or            S_IXGRP or
    S_IROTH or            S_IXOTH);
  if ChmodResult <> 0 then
    WritelnWarning('Package', Format('Error setting executable bit on "%s": %s', [
      PathAndName,
      SysErrorMessage(ChmodResult)
    ]));
{$else}
begin
  WritelnWarning('Package', 'Packaging for a platform where UNIX permissions matter, but we cannot set "chmod" on this platform. This usually means that you package for Unix from Windows, and means that "executable" bit inside binary in tar.gz archive may not be set --- archive may not be 100% comfortable for Unix users');
{$endif}
end;

procedure ErrorBox(const Message: String);

  {$ifdef MSWINDOWS}
  procedure WindowsErrorBox(const Text: String; const Caption: String = 'Error'; const Parent: HWND = 0);
  begin
    MessageBox(Parent, PChar(Text), PChar(Caption), MB_OK or MB_ICONERROR or MB_TASKMODAL);
  end;
  {$endif}

begin
  {$ifdef MSWINDOWS}
  WindowsErrorBox(Message);
  {$else}
  RunCommandSimple('zenity', ['--error', '--no-markup', '--text=' + Message]);
  {$endif}
end;

function CachePath: String;
begin
  Result := InclPathDelim(GetAppConfigDir(false)) + 'cache' + PathDelim;
end;

{ Hash of arbitrary data. Always 0 for DataSize = 0. }
function HashData(const Data; const DataSize: SizeInt): UInt32;
var
  DataBytes: TByteArray absolute Data;
  I: SizeInt;
begin
  Result := 0;
  for I := 0 to DataSize - 1 do
  begin
    Result := Result xor DataBytes[I];
    {$I norqcheckbegin.inc}
    Result := Result * 16777619;
    {$I norqcheckend.inc}
  end;
end;

{ Hash of a String. Always 0 for empty string. }
function HashString(const S: String): UInt32;
begin
  if S = '' then
    Result := 0
  else
    Result := HashData(S[1], Length(S) * SizeOf(Char));
end;

function CreateGUIDFromHash(const Seed: String): TGuid;

(*
var
  { FPC: https://www.freepascal.org/docs-html/rtl/system/randseed.html
    Delphi: https://docwiki.embarcadero.com/Libraries/Sydney/en/System.RandSeed }
  SavedRandSeed: {$ifdef FPC} Cardinal {$else} Integer {$endif}};
begin
  SavedRandSeed := RandSeed;
  try
    RandSeed := HashString(Seed);
    Result := CreateGUID;
  finally
    RandSeed := SavedRandSeed;
  end;
*)

{ Use approach that doesn't depend on Random algorithm, which may differ between FPC, Delphi
  and even their particular versions. }

var
  I: Integer;
begin
  Result.D1 := HashString(Seed);
  { Below we assign UInt32 values (HashString) to smaller types, just ignore the overflows. }
  {$I norqcheckbegin.inc}
  Result.D2 := HashString(Seed + 'D2');
  Result.D3 := HashString(Seed + 'D3');
  for I := Low(Result.D4) to High(Result.D4) do
    Result.D4[I] := HashString(Seed + 'D4' + IntToStr(I));
  {$I norqcheckend.inc}
end;

end.
