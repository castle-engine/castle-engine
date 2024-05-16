{
  Copyright 2022-2022 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Running tests using CastleTester with console output (just using CGE WritelnLog). }
unit CastleConsoleTester;

interface

uses SysUtils, Classes, CastleTester;

type
  TCastleConsoleTester = class
  strict private
    FTester: TCastleTester;
    procedure TestExecuted(const AName: String);
    procedure TestFailed(const TestName, Msg: String);

    procedure Log(const AMessage: String);
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
  Log(TestName + ': Failed: ' + Msg);
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

procedure TCastleConsoleTester.Log(const AMessage: String);
begin
  WritelnLog(AMessage);
end;

procedure TCastleConsoleTester.Run(const ATestCaseToRun: String);
begin
  FTester.AddRegisteredTestCases;
  Log('Scaning tests...');
  FTester.Scan;
  Log('Found ' + IntToStr(FTester.EnabledTestCount) + ' tests.');
  if ParamFilter <> '' then
  begin
    FTester.EnableFilter(ParamFilter);
    Log('Applying filter: Enabled ' + IntToStr(FTester.EnabledTestCount) + ' tests.');
  end;
  Log('Preparing tests...');
  FTester.PrepareTestListToRun(ATestCaseToRun);
  Log('Running tests...');
  FTester.Run;
  if FTester.TestFailedCount = 0 then
    Log('All tests passed.');

  //Log('Press <enter> to quit...');
  //Readln;
end;

procedure TCastleConsoleTester.TestExecuted(const AName: String);
begin
  Log('Processing: ' + AName);
end;

end.
