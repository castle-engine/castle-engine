{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleInternalProcess;

{$I ../../../src/common_includes/castleconf.inc}

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCastleInternalProcess = class(TCastleTestCase)
  published
    procedure TestEnvironmentStrings;
    procedure TestProcessExecute;
  end;

implementation

uses CastleInternalProcess, CastleUtils, CastleLog, CastleFilesUtils;

procedure TTestCastleInternalProcess.TestEnvironmentStrings;
var
  Env: TStrings;
  I: Integer;
begin
  Env := EnvironmentStrings;
  try
    WritelnLog('Environment strings count: %d', [Env.Count]);
    //WritelnLog('Environment strings:' + NL + Env.Text);
    for I := 0 to Env.Count - 1 do
      AssertTrue(Env.Names[I] <> '');
  finally FreeAndNil(Env) end;
end;

procedure TTestCastleInternalProcess.TestProcessExecute;

{$ifndef CASTLE_PROCESS_AVAILABLE}
begin
  AbortTest; // cannot use ExecuteCommandCheckStatus
  Exit;
end;
{$else}

{ If the standard Unix tools touch, ls are available -- have fun
  testing their usage with ExecuteCommandCheckStatus. }

var
  TouchExe, LsExe, RmExe, TempDir, TempFileBaseName: String;
begin
  if not CanUseFileSystem then // needed for GetTempDirectory
  begin
    AbortTest;
    Exit;
  end;

  TouchExe := FindExe('touch');
  LsExe := FindExe('ls');
  RmExe := FindExe('rm');

  if (TouchExe = '') or (LsExe = '') or (RmExe = '') then
  begin
    WritelnLog('touch, ls or rm not found, skipping TestProcessExecute');
    AbortTest;
    Exit;
  end;

  WritelnLog('Using Unix tools:' + NL +
    '  touch: "%s"' + NL +
    '  ls: "%s"' + NL +
    '  rm: "%s"', [
    TouchExe,
    LsExe,
    RmExe
  ]);

  { Run the tools with CurrentDirectory = TempDir, so the relative
    TempFileBaseName is created/listed/removed there. This also exercises
    TCastleProcess.CurrentDirectory and the ExitStatus checking. }
  TempDir := GetTempDirectory;
  TempFileBaseName := 'castle_internal_process_testfile' + IntToStr(Random(1000000)) + '.txt';
  ExecuteCommandCheckStatus(TempDir, TouchExe, [TempFileBaseName]);
  ExecuteCommandCheckStatus(TempDir, LsExe, ['-l', TempFileBaseName]);
  ExecuteCommandCheckStatus(TempDir, RmExe, ['-f', TempFileBaseName]);
end;

{$endif}

initialization
  RegisterTest(TTestCastleInternalProcess);
end.
