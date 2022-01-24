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
      procedure Run;
  end;


implementation

uses CastleLog;

{ TCastleConsoleTester }

procedure TCastleConsoleTester.AssertFailed(const TestName, Msg: String);
begin
  Log(TestName + 'Failed : ' + Msg);
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

procedure TCastleConsoleTester.Run;
begin
  FTester.AddRegisteredTestCases;
  Log('Scaning tests...');
  FTester.Scan;
  Log('Found ' + IntToStr(Ftester.EnabledTestCount) + ' tests.');
  Log('Preparing tests...');
  FTester.PrepareTestListToRun;
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
