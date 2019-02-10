{
  Copyright 2014-2018 Michalis Kamburelis.

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

{ Run command in given directory with given arguments,
  gathering output and status to string.
  Also gathers error output to the same string.
  Like Process.RunCommandIndir in FPC >= 2.6.4, but also captures error output.

  @param(ExeName Executable name, should be an absolute filename
    e.g. found by FindExe. This will avoid FPC errors (the default FPC
    algorithm searching $PATH may mistake binary for a directory with
    the same name).)

  @param(OutputString Standard output (stdout) and standard error output
    (stderr) of the command.)
}
procedure MyRunCommandIndir(
  const CurDir: string; const ExeName: string;
  const Options: array of string;
  out OutputString: string; out ExitStatus: integer);

{ Run command in given directory with given arguments,
  gathering output and status to string, and also letting output
  to go to our output.

  @param(ExeName Executable name, should be an absolute filename
    e.g. found by FindExe. Like for MyRunCommandIndir.)

  @param(OutputString Stdout and stderr of the process.
    Like by MyRunCommandIndir.)

  @param(OverrideEnvironmentName
    When not empty, this environment variable has set
    value OverrideEnvironmentValue in the process.) }
procedure RunCommandIndirPassthrough(
  const CurDir: string; const ExeName: string;
  const Options: array of string;
  var OutputString:string; var ExitStatus:integer;
  const OverrideEnvironmentName: string = '';
  const OverrideEnvironmentValue: string = '');

{ Run command in given (or current) directory with given arguments,
  letting output (stdout and stderr) to go to our stdout.
  Command is searched on $PATH following standard OS conventions,
  if it's not already an absolute filename.
  Raises exception if command fails (detected by exit code <> 0). }
procedure RunCommandSimple(
  const ExeName: string; const Options: array of string); overload;
procedure RunCommandSimple(
  const CurDir: string; const ExeName: string; const Options: array of string;
  const OverrideEnvironmentName: string = '';
  const OverrideEnvironmentValue: string = ''); overload;

{ Run the command, and return immediately, without waiting for finish. }
procedure RunCommandNoWait(
  const ProjectPath: string;
  const ExeName: string; const Options: array of string);

var
  { Trivial verbosity global setting. }
  Verbose: boolean = false;

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

{ Like @link(FindExe), but additionally look for the exe in
  Castle Game Engine bin/ subdirectory. }
function FindCgeExe(const ExeName: String): String;

{ Path to CGE main directory,
  obtained from $CASTLE_ENGINE_PATH environment variable.

  Returns empty String if it wasn't possible to get a valid value.
  Otherwise, the returned path always ends with path delimiter,
  and always exists. }
function CastleEnginePath: String;

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
  CastleFilesUtils, CastleUtils, CastleURIUtils;

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

procedure MyRunCommandIndir(const CurDir: string;
  const ExeName: string;const Options: array of string;
  out OutputString: string; out ExitStatus: integer);
{ Adjusted from fpc/trunk/packages/fcl-process/src/process.pp }
Const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.
var
  p : TProcess;
  i : integer;
  numbytes,bytesread : integer;
begin
  // default out values
  OutputString := '';
  ExitStatus := 0;

  p:=TProcess.create(nil);
  p.Executable:=exename;
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  if high(Options)>=0 then
   for i:=low(Options) to high(Options) do
     p.Parameters.add(Options[i]);
  if Verbose then
  begin
    Writeln('Calling ' + ExeName);
    Writeln(P.Parameters.Text);
  end;

  try
    try
      p.Options := [poUsePipes, poStderrToOutPut];
      bytesread := 0;
      p.Execute;
      while p.Running do
      begin
        Setlength(OutputString,BytesRead + READ_BYTES);
        NumBytes := p.Output.Read(OutputString[1+bytesread], READ_BYTES);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes) else
          Sleep(100);
      end;
      repeat
        Setlength(OutputString,BytesRead + READ_BYTES);
        NumBytes := p.Output.Read(OutputString[1+bytesread], READ_BYTES);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
      until NumBytes <= 0;
      setlength(OutputString,BytesRead);
      ExitStatus:=p.ExitStatus;
    except
      on e : Exception do
      begin
        setlength(OutputString,BytesRead);
        raise;
      end;
    end;
  finally p.free end;
end;

procedure RunCommandIndirPassthrough(const CurDir: string;const ExeName: string;const Options: array of string;var OutputString:string;var ExitStatus:integer;
  const OverrideEnvironmentName: string = '';
  const OverrideEnvironmentValue: string = '');
{ Adjusted from fpc/trunk/packages/fcl-process/src/process.pp }
Const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.
var
  p : TProcess;
  i : integer;
  numbytes,bytesread : integer;
  NewEnvironment: TStringList;
begin
  p:=TProcess.create(nil);
  p.Executable:=exename;
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  if high(Options)>=0 then
   for i:=low(Options) to high(Options) do
     p.Parameters.add(Options[i]);
  if Verbose then
  begin
    Writeln('Calling ' + ExeName);
    Writeln(P.Parameters.Text);
  end;

  NewEnvironment := nil;
  try
    if OverrideEnvironmentName <> '' then
    begin
      NewEnvironment := TStringList.Create;
      for I := 1 to GetEnvironmentVariableCount do
        NewEnvironment.Add(GetEnvironmentString(I));
      NewEnvironment.Values[OverrideEnvironmentName] := OverrideEnvironmentValue;
      P.Environment := NewEnvironment;
      // Writeln('Environment: ' + P.Environment.Text);
    end;

    try
      p.Options := [poUsePipes, poStderrToOutPut];
      bytesread := 0;
      p.Execute;
      while p.Running do
      begin
        Setlength(OutputString,BytesRead + READ_BYTES);
        NumBytes := p.Output.Read(OutputString[1+bytesread], READ_BYTES);
        Write(Copy(OutputString, 1+bytesread, NumBytes)); // passthrough
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes) else
          Sleep(100);
      end;
      repeat
        Setlength(OutputString,BytesRead + READ_BYTES);
        NumBytes := p.Output.Read(OutputString[1+bytesread], READ_BYTES);
        Write(Copy(OutputString, 1+bytesread, NumBytes)); // passthrough
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
      until NumBytes <= 0;
      setlength(OutputString,BytesRead);
      ExitStatus:=p.ExitStatus;
    except
      on e : Exception do
      begin
        setlength(OutputString,BytesRead);
        raise;
      end;
    end;
  finally
    FreeAndNil(p);
    FreeAndNil(NewEnvironment);
  end;
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

    if Verbose then
    begin
      Writeln('Calling ' + ExeName);
      Writeln(P.Parameters.Text);
    end;

    P.Execute;
  finally FreeAndNil(P) end;
end;

procedure RunCommandSimple(
  const ExeName: string; const Options: array of string);
begin
  RunCommandSimple(GetCurrentDir, ExeName, Options);
end;

procedure RunCommandSimple(
  const CurDir: string; const ExeName: string; const Options: array of string;
  const OverrideEnvironmentName: string = '';
  const OverrideEnvironmentValue: string = '');
var
  ProcessOutput: string;
  ProcessStatus: Integer;
  AbsoluteExeName: string;
begin
  { use FindExe to use our fixed PathFileSearch that does not accidentaly find
    "ant" directory as "ant" executable }
  if IsPathAbsolute(ExeName) then
    AbsoluteExeName := ExeName
  else
  begin
    AbsoluteExeName := FindExe(ExeName);
    if AbsoluteExeName = '' then
      raise Exception.CreateFmt('Cannot find "%s" on environment variable $PATH. Make sure "%s" is installed and $PATH is configured correctly',
        [ExeName, ExeName]);
  end;

  RunCommandIndirPassthrough(CurDir, AbsoluteExeName, Options,
    ProcessOutput, ProcessStatus, OverrideEnvironmentName, OverrideEnvironmentValue);
  if ProcessStatus <> 0 then
    raise Exception.CreateFmt('"%s" (on $PATH as "%s") call failed with exit status %d',
      [ExeName, AbsoluteExeName, ProcessStatus]);
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
  if Verbose then
    Writeln('Created temporary dir for package: ' + Result);
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

function FindCgeExe(const ExeName: String): String;
var
  CgePath: String;
begin
  CgePath := CastleEnginePath;
  if CgePath <> '' then
  begin
    Result := CgePath + 'bin' + PathDelim + ExeName + ExeExtension;
    if FileExists(Result) then
      Exit;
  end;

  Result := FindExe(ExeName);
end;

function CastleEnginePath: String;
begin
  Result := GetEnvironmentVariable('CASTLE_ENGINE_PATH');
  if Result = '' then Exit;

  Result := InclPathDelim(Result);

  if not DirectoryExists(Result) then
    Exit('');

  { $CASTLE_ENGINE_PATH environment variable may point to the directory
    - containing castle_game_engine/ as subdirectory
    - or containing castle-engine/ as subdirectory
    - or pointing straight to castle_game_engine/ or castle-engine/
      directory. }
  if DirectoryExists(Result + 'castle_game_engine') then
    Result := Result + 'castle_game_engine' + PathDelim
  else
  if DirectoryExists(Result + 'castle-engine') then
    Result := Result + 'castle-engine' + PathDelim;
end;

end.
