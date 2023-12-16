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
  private
    FilterTests: String;
  public
    constructor Create;
    destructor Destroy; override;

    { Parse --filter command-line parameter. }
    procedure ParseParameters;

    { Runs all tests (allowed by current filter, if any). }
    procedure Run(const ATestCaseToRun: String = '');
  end;

implementation

uses CastleLog, CastleParameters, CastleUtils;

procedure TCastleConsoleTester.TestFailed(const TestName, Msg: String);
begin
  Log(TestName + ': Failed: ' + Msg);
end;

constructor TCastleConsoleTester.Create;
begin
  FTester := TCastleTester.Create(nil);
  FTester.NotifyTestCaseExecuted := {$ifdef FPC}@{$endif}TestExecuted;
  FTester.NotifyTestFail := {$ifdef FPC}@{$endif}TestFailed;
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
  Log('Found ' + IntToStr(FTester.EnabledTestCount) + ' tests.');
  if FilterTests <> '' then
  begin
    FTester.EnableFilter(FilterTests);
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

{ Handle --filter command-line option.
  This is a callback for Parameters.Parse. }
procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var
  Sender: TCastleConsoleTester;
begin
  Sender := TCastleConsoleTester(Data);
  case OptionNum of
    0: Sender.FilterTests := Argument;
    else raise EInternalError.Create('OptionProc: OptionNum = ' + IntToStr(OptionNum));
  end;
end;

procedure TCastleConsoleTester.ParseParameters;
const
  Options: array [0..0] of TOption = (
    (Short:'f'; Long:'filter'; Argument: oaRequired)
  );
begin
  Parameters.Parse(Options, @OptionProc, Self, true);
end;

end.
