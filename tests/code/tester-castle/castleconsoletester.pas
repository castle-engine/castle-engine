{
  Copyright 2022-2025 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Running tests using CastleTester with console output (just using CastleLog). }
unit CastleConsoleTester;

interface

uses SysUtils, Classes, CastleTester;

type
  TCastleConsoleTester = class
  strict private
    FTester: TCastleTester;
    procedure TestExecuted(const AName: String);
    procedure TestFailed(const TestName, Msg: String);
  public
    constructor Create;
    destructor Destroy; override;

    { Runs all tests (allowed by current ParamFilter, if any). }
    procedure Run(const ATestCaseToRun: String = '');
  end;

implementation

uses CastleLog, CastleTesterParameters, CastleUtils, CastleStringUtils;

procedure TCastleConsoleTester.TestFailed(const TestName, Msg: String);
begin
  WritelnLog(TestName, 'Failed: ' + Msg);
end;

constructor TCastleConsoleTester.Create;
begin
  inherited;
  FTester := TCastleTester.Create(nil);
  FTester.NotifyTestCaseExecuted := {$ifdef FPC}@{$endif}TestExecuted;
  FTester.NotifyTestFail := {$ifdef FPC}@{$endif}TestFailed;
end;

destructor TCastleConsoleTester.Destroy;
begin
  FreeAndNil(FTester);
  inherited;
end;

procedure TCastleConsoleTester.Run(const ATestCaseToRun: String);
begin
  FTester.AddRegisteredTestCases;
  WritelnLog('Scaning tests...');
  FTester.Scan;
  WritelnLog('Found ' + IntToStr(FTester.EnabledTestCount) + ' tests.');
  if ParamFilter <> '' then
  begin
    FTester.EnableFilter(ParamFilter);
    WritelnLog('Applying filter: Enabled ' + IntToStr(FTester.EnabledTestCount) + ' tests.');
  end;
  WritelnLog('Preparing tests...');
  FTester.PrepareTestListToRun(ATestCaseToRun);
  WritelnLog('Running tests...');
  FTester.Run;
  if FTester.TestFailedCount = 0 then
    WritelnLog('All %d tests passed (included in this: %d aborted).', [
      FTester.TestPassedCount,
      FTester.TestAbortedCount
    ]);

  //WritelnLog('Press <enter> to quit...');
  //Readln;
end;

procedure TCastleConsoleTester.TestExecuted(const AName: String);
begin
  WritelnLog('Processing: ' + AName);
end;

end.
