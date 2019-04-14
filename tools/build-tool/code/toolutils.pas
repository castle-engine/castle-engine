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

uses CastleImages, CastleStringUtils;

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

function CreateTemporaryDir: string;

{ Run the command, and return immediately, without waiting for finish. }
procedure RunCommandNoWait(
  const ProjectPath: string;
  const ExeName: string; const Options: array of string);

type
  TImageFileNames = class(TCastleStringList)
  private
    FBaseUrl: string;
  public
    property BaseUrl: string read FBaseUrl write FBaseUrl;
    { Find image with given extension, or '' if not found. }
    function FindExtension(const Extensions: array of string): string;
    { Find and read an image format that we can process with our CastleImages.
      Try to read it to a class that supports nice-quality resizing (TResizeInterpolationFpImage).
      @nil if not found. }
    function FindReadable: TCastleImage;
  end;

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

implementation

uses Classes, Process, SysUtils,
  CastleFilesUtils, CastleUtils, CastleURIUtils, CastleLog,
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
      if not FileExists(OutputNote) then
        StringToFile(OutputNote, OutputNoteContents);
    end;
  end;

  Result := FOutputPath;
end;

function CreateTemporaryDir: string;
begin
  Result := InclPathDelim(GetTempDir(false)) +
    ApplicationName + IntToStr(Random(1000000));
  CheckForceDirectories(Result);
  WritelnVerbose('Created temporary dir for package: ' + Result);
end;

procedure RunCommandNoWait(
  const ProjectPath: string;
  const ExeName: string; const Options: array of string);
var
  P: TProcess;
  I: Integer;
  {$ifdef UNIX} NoHupExe: String; {$endif}
begin
  P := TProcess.Create(nil);
  try
    P.Executable := ExeName;
    // this is useful on Unix, to place nohup.out inside temp directory
    P.CurrentDirectory := TempOutputPath(ProjectPath);

    { Under Unix, execute using nohup.
      This way the parent process can die (and destroy child's IO handles)
      and the new process will keep running OK.
      This is important when you execute in "castle-editor" the option
      to "Restart and Rebuild" editor, then "castle-editor" calls "castle-engine editor",
      and both "castle-editor" and "castle-engine" processes die
      (while the new CGE editor should continue running). }
    {$ifdef UNIX}
    NoHupExe := FindExe('nohup');
    if NoHupExe <> '' then
    begin
      P.Executable := NoHupExe;
      P.Parameters.Add(ExeName);
    end;
    {$endif}

    for I := Low(Options) to High(Options) do
      P.Parameters.Add(Options[I]);

    { Under Windows, these options should make a process execute OK.
      Following http://wiki.lazarus.freepascal.org/Executing_External_Programs . }
    P.InheritHandles := false;
    P.ShowWindow := swoShow;

    WritelnVerbose('Calling ' + ExeName);
    WritelnVerbose(P.Parameters.Text);

    P.Execute;
  finally FreeAndNil(P) end;
end;

{ TImageFileNames ------------------------------------------------------------- }

function TImageFileNames.FindExtension(const Extensions: array of string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    if AnsiSameText(ExtractFileExt(Strings[I]), '.ico') then
      Exit(Strings[I]);
end;

function TImageFileNames.FindReadable: TCastleImage;
var
  I: Integer;
  MimeType, URL: string;
begin
  for I := 0 to Count - 1 do
  begin
    URL := CombineURI(BaseUrl, Strings[I]);
    MimeType := URIMimeType(URL);
    if (MimeType <> '') and IsImageMimeType(MimeType, true, false) then
      Exit(LoadImage(URL, [TRGBImage, TRGBAlphaImage]));
  end;
  Result := nil;
end;

end.
