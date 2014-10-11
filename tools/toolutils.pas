{
  Copyright 2014-2014 Michalis Kamburelis and FPC team.

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

{ Copy file, making sure the destination directory exists
  (eventually creating it), and checking result. }
procedure SmartCopyFile(const Source, Dest: string);

function FileSize(const FileName: string): Int64;

{ Run command in given directory with given arguments,
  gathering output and status to string.
  Also gathers error output to the same string.
  Like Process.RunCommandIndir in FPC >= 2.6.4, but also captures error output. }
procedure MyRunCommandIndir(
  const curdir:string; const exename:string;
  const commands:array of string;
  var outputstring:string; var exitstatus:integer);

{ Run command in given directory with given arguments,
  gathering output and status to string, and also letting output
  to go to our output. }
procedure RunCommandIndirPassthrough(
  const curdir:string; const exename:string;
  const commands:array of string;
  var outputstring:string; var exitstatus:integer);

var
  { Trivial verbosity global setting. }
  Verbose: boolean;

implementation

uses Classes, Process, SysUtils,
  CastleFilesUtils;

procedure SmartCopyFile(const Source, Dest: string);
var
  SourceFile, DestFile: TFileStream;
begin
  CheckForceDirectories(ExtractFileDir(Dest));

  SourceFile := TFileStream.Create(Source, fmOpenRead);
  try
    DestFile := TFileStream.Create(Dest, fmCreate);
    try
      DestFile.CopyFrom(SourceFile, SourceFile.Size);
    finally FreeAndNil(SourceFile) end;
  finally FreeAndNil(DestFile) end;

{  if not CopyFile(Source, Dest) then
    raise Exception.CreateFmt('Cannot copy file from "%s" to "%s"', [Source, Dest]);}
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

procedure MyRunCommandIndir(const curdir:string;const exename:string;const commands:array of string;var outputstring:string;var exitstatus:integer);
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
  if high(commands)>=0 then
   for i:=low(commands) to high(commands) do
     p.Parameters.add(commands[i]);

  try
    try
      p.Options := [poUsePipes, poStderrToOutPut];
      bytesread := 0;
      p.Execute;
      while p.Running do
      begin
        Setlength(outputstring,BytesRead + READ_BYTES);
        NumBytes := p.Output.Read(outputstring[1+bytesread], READ_BYTES);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes) else
          Sleep(100);
      end;
      repeat
        Setlength(outputstring,BytesRead + READ_BYTES);
        NumBytes := p.Output.Read(outputstring[1+bytesread], READ_BYTES);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
      until NumBytes <= 0;
      setlength(outputstring,BytesRead);
      exitstatus:=p.exitstatus;
    except
      on e : Exception do
      begin
        setlength(outputstring,BytesRead);
        raise;
      end;
    end;
  finally p.free end;
end;

procedure RunCommandIndirPassthrough(const curdir:string;const exename:string;const commands:array of string;var outputstring:string;var exitstatus:integer);
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
  if high(commands)>=0 then
   for i:=low(commands) to high(commands) do
     p.Parameters.add(commands[i]);

  try
    try
      p.Options := [poUsePipes, poStderrToOutPut];
      bytesread := 0;
      p.Execute;
      while p.Running do
      begin
        Setlength(outputstring,BytesRead + READ_BYTES);
        NumBytes := p.Output.Read(outputstring[1+bytesread], READ_BYTES);
        Write(Copy(outputstring, 1+bytesread, NumBytes)); // passthrough
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes) else
          Sleep(100);
      end;
      repeat
        Setlength(outputstring,BytesRead + READ_BYTES);
        NumBytes := p.Output.Read(outputstring[1+bytesread], READ_BYTES);
        Write(Copy(outputstring, 1+bytesread, NumBytes)); // passthrough
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
      until NumBytes <= 0;
      setlength(outputstring,BytesRead);
      exitstatus:=p.exitstatus;
    except
      on e : Exception do
      begin
        setlength(outputstring,BytesRead);
        raise;
      end;
    end;
  finally p.free end;
end;

end.
