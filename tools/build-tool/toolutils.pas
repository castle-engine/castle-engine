{
  Copyright 2014-2017 Michalis Kamburelis.

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
  var OutputString:string; var ExitStatus:integer);

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
  const ExeName: string; const Options: array of string);
procedure RunCommandSimple(
  const CurDir: string; const ExeName: string; const Options: array of string;
  const OverrideEnvironmentName: string = '';
  const OverrideEnvironmentValue: string = '');

var
  { Trivial verbosity global setting. }
  Verbose: boolean = false;
  { Leave created temporary files. }
  LeaveTemp: boolean = false;

type
  TReplaceMacros = function (const Source: string): string of object;

function CreateTemporaryDir: string;

type
  TIconFileNames = class(TCastleStringList)
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

procedure MyRunCommandIndir(const CurDir: string;const ExeName: string;const Options: array of string;var OutputString:string;var ExitStatus:integer);
{ Adjusted from fpc/trunk/packages/fcl-process/src/process.pp }
Const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.
var
  p : TProcess;
  i : integer;
  numbytes,bytesread : integer;
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
    AbsoluteExeName := ExeName else
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

function CreateTemporaryDir: string;
begin
  Result := InclPathDelim(GetTempDir(false)) +
    ApplicationName + IntToStr(Random(1000000));
  CheckForceDirectories(Result);
  if Verbose then
    Writeln('Created temporary dir for package: ' + Result);
end;

{ TIconFileNames ------------------------------------------------------------- }

function TIconFileNames.FindExtension(const Extensions: array of string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    if AnsiSameText(ExtractFileExt(Strings[I]), '.ico') then
      Exit(Strings[I]);
end;

function TIconFileNames.FindReadable: TCastleImage;
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
