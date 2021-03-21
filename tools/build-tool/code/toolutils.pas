{
  Copyright 2014-2019 Michalis Kamburelis.

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

{ Utilities. }
unit ToolUtils;

interface

uses DOM,
  CastleImages, CastleStringUtils;

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
    TODO: Submit a patch to FPC to restore previous behaviour.

    Index: packages/fcl-image/src/fpinterpolation.inc
    ===================================================================
    --- packages/fcl-image/src/fpinterpolation.inc	(wersja 40746)
    +++ packages/fcl-image/src/fpinterpolation.inc	(kopia robocza)
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

  For old FPC, it is just link.res.
  For new FPC 3.3.1 it may be any link<id>.res and unfortunately we don't know the <id>
  (it's not the TProcess.ProcessID of "fpc" process, at least under Windows). }
function FindLinkRes(const Path: String): String;

implementation

uses Classes, Process, SysUtils,
  CastleFilesUtils, CastleUtils, CastleURIUtils, CastleLog, CastleXMLUtils,
  CastleFindFiles,
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
  FOutputPath: string;

function TempOutputPath(const WorkingDirectory: string; const CreateIfNecessary: boolean): string;
const
  OutputNoteContents = {$I ../embedded_templates/template-castle-engine-output-warning.txt.inc};
var
  OutputNote: string;
begin
  if FOutputPath = '' then
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

  Handler := TFindLinkResHandler.Create;
  try
    FindFiles(Path, 'link*.res', false, @Handler.FoundFile, []);
    Result := Handler.FileName;
  finally FreeAndNil(Handler) end;
end;

end.
