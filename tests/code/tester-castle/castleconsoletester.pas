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
    procedure AssertFailed(const TestName, Msg: String);

    procedure Log(const AMessage: String);
  public
    constructor Create;
    destructor Destroy; override;

    { Runs all tests or only one test case when ATestCaseToRun <> '' }
    procedure Run(const ATestCaseToRun: String = '');

    { Parsing --suite=xx parameter and returns only value (xx) }
    class function GetTestCaseNameFromParameters: String;
  end;

implementation

uses CastleLog, CastleParameters;

procedure TCastleConsoleTester.AssertFailed(const TestName, Msg: String);
begin
  Log(TestName + ': Failed: ' + Msg);
end;

constructor TCastleConsoleTester.Create;
begin
  FTester := TCastleTester.Create(nil);
  FTester.NotifyTestCaseExecuted := {$ifdef FPC}@{$endif}TestExecuted;
  FTester.NotifyAssertFail := {$ifdef FPC}@{$endif}AssertFailed;
end;

destructor TCastleConsoleTester.Destroy;
begin
  FreeAndNil(FTester);
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
  Log('Found ' + IntToStr(Ftester.EnabledTestCount) + ' tests.');
  Log('Preparing tests...');
  FTester.PrepareTestListToRun(ATestCaseToRun);
  Log('Running tests...');
  FTester.Run;
  if FTester.TestFailedCount = 0 then
    Log('All tests passed.');

  //Log('Press <enter> to quit...');
  //Readln;
end;

class function TCastleConsoleTester.GetTestCaseNameFromParameters: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Parameters.Count - 1 do
  begin
    if Pos('--suite=', Parameters[I]) = 1 then
    begin
      Result := Parameters[I];
      Delete(Result, 1, 8);
      Exit(trim(Result));
    end;
  end;
end;

procedure TCastleConsoleTester.TestExecuted(const AName: String);
begin
  Log('Processing: ' + AName);
end;

end.
