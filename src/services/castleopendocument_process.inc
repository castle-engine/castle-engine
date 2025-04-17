{%MainUnit castleopendocument.pas}
{
  Copyright 2012-2024 Michalis Kamburelis and Lazarus developers.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.
  Parts of this file are based on Lazarus LCL code, which has
  exactly the same license as our "Castle Game Engine":
  LGPL with static linking exception, see COPYING.txt for details.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ RunCmdFromPath (using TProcess on FPC, or Unix-specific System call on Delphi).
  This is used by OpenUrl and/or OpenDocument on some platforms. }

{$ifdef read_uses}
  // With Delphi, this will compile only on platforms that have Posix.Stdlib, and that's good.
  {$ifdef FPC} BaseUnix, Process, {$else} Posix.Stdlib, {$endif}
{$endif}

{$ifdef read_implementation}

{ Based on Lazarus LCL:
  lcl/lclstrconsts.pas
  lcl/include/unixfileutil.inc
  lcl/utf8process.pp
  lcl/include/sysenvapis_unix.inc
}

function FileIsExecutable(const AFilename: string): boolean;
{$ifdef FPC}
var
  Info: Stat;
{$endif}
begin
  {$ifdef FPC}
  // First check AFilename is not a directory and then check if executable
  Result:= (FpStat(AFilename,info)<>-1) and FPS_ISREG(info.st_mode) and
           (BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)=0);
  {$else}
  // For now, in Delphi, just assume it's executable.
  Result := true;
  {$endif}
end;

{$ifdef FPC}

resourcestring
  lisProgramFileNotFound = 'program file not found %s';
  lisCanNotExecute = 'can not execute %s';

{ Runs a short command which should point to an executable in
  the environment PATH.

  CGE: simplified this to use TProcess.Executable, TProcess.Parameters
  instead of TProcess.CommandLine (Lazarus TProcessUTF8 didn't have these
  improvements). This removes the need for paranoid quoting of strings
  everywhere.
  This always takes exactly 1 parameter now --- which is actually Ok
  for usage in this unit. }
procedure RunCmdFromPath(ProgramFilename, Parameter: string);
var
  OldProgramFilename: String;
  BrowserProcess: TProcess;
begin
  OldProgramFilename:=ProgramFilename;
  ProgramFilename:=PathFileSearch(ProgramFilename);

  if ProgramFilename='' then
    raise EFOpenError.Create(Format(lisProgramFileNotFound, [OldProgramFilename]));
  if not FileIsExecutable(ProgramFilename) then
    raise EFOpenError.Create(Format(lisCanNotExecute, [ProgramFilename]));

  // run
  BrowserProcess := TProcess.Create(nil);
  try
    BrowserProcess.Executable := ProgramFilename;
    BrowserProcess.Parameters.Add(Parameter);

    WritelnLog('Executing', 'Executable: "' + ProgramFilename +
      '", Parameter: "' + Parameter + '"');

    BrowserProcess.Execute;
  finally
    BrowserProcess.Free;
  end;
end;

{$else}

procedure RunCmdFromPath(const ProgramFilename, Parameter: string);
var
  Command: AnsiString;
begin
  if (Pos('"', ProgramFilename) <> 0) or
     (Pos('"', Parameter) <> 0) then
  begin
    WritelnWarning('RunCmdFromPath', 'Not implemented for the case when ProgramFilename or Parameter contain a double quote');
    Exit;
  end;

  Command := Format('"%s" "%s"', [ProgramFilename, Parameter]);
  _System(PAnsiChar(Command));
end;

{$endif}

{$endif read_implementation}
