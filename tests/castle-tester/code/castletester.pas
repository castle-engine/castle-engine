unit CastleTester;

interface

uses SysUtils, Classes, System.Generics.Collections, Rtti;

type

  EAssertionFailedError = class(Exception);

  TNotifyAssertFail = procedure (const Msg: String) of object;
  TNotifyTestExecuted = procedure (const Name: String) of object;
  TNotifyTestCaseExecuted = procedure (const Name: String) of object;

  TCastleTest = class

  end;

  TCastleTestCase = class
  private
    FName: String;
    FNotifyAssertFail: TNotifyAssertFail;
  public
    procedure TestTestCase;
    constructor Create;

    procedure Fail(const Msg: String);
    procedure AssertTrue(const ACondition: Boolean); overload;
    procedure AssertTrue(const Msg: String; const ACondition: Boolean); overload;
    procedure AssertFalse(const ACondition: Boolean); overload;
    procedure AssertFalse(const Msg: String; const ACondition: Boolean); overload;
    procedure AssertFilenamesEqual(Expected, Actual: String);
    procedure AssertEquals(Expected, Actual: String);

    function CompareFileName(Expected, Actual: String): Boolean;

  end;


  TCastleTester = class (TComponent)
  private
    FRttiContext: TRttiContext;
    FTestCaseList: {$ifdef FPC}specialize{$endif} TObjectList<TCastleTestCase>;
    FNotifyTestExecuted: TNotifyTestExecuted;
    FNotifyTestCaseExecuted: TNotifyTestCaseExecuted;
    FNotifyAssertFail: TNotifyAssertFail;
    procedure SetNotifyAssertFail(const ANotifyAssertFail: TNotifyAssertFail);


    procedure RunTestCase(TestCase: TCastleTestCase);

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure AddTestCase(const TestCase: TCastleTestCase);

    procedure Run;

    property NotifyTestExecuted: TNotifyTestExecuted read FNotifyTestExecuted
      write FNotifyTestExecuted;
    property NotifyTestCaseExecuted: TNotifyTestCaseExecuted
      read FNotifyTestCaseExecuted write FNotifyTestCaseExecuted;
    property NotifyAssertFail: TNotifyAssertFail read FNotifyAssertFail
      write SetNotifyAssertFail;

  end;

implementation

{ TCastleTester }

uses CastleLog, TypInfo;

procedure TCastleTester.AddTestCase(const TestCase: TCastleTestCase);
begin
  FTestCaseList.Add(TestCase);
  TestCase.FNotifyAssertFail := FNotifyAssertFail;
end;

constructor TCastleTester.Create(AOwner: TComponent);
begin
  inherited;

  FTestCaseList := TObjectList<TCastleTestCase>.Create;
  FRttiContext := TRttiContext.Create;
end;

destructor TCastleTester.Destroy;
begin
  FRttiContext.Free;
  FreeAndNil(FTestCaseList);
  inherited;
end;

procedure TCastleTester.Run;
var
  I: Integer;
  TestCase: TCastleTestCase;
begin
  for I := 0 to FTestCaseList.Count -1 do
  begin
    TestCase := FTestCaseList[I];
    RunTestCase(TestCase);
  end;
end;

procedure TCastleTester.RunTestCase(TestCase: TCastleTestCase);
var
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
begin
  RttiType := FRttiContext.GetType(TestCase.ClassInfo);

  for RttiMethod in RttiType.GetMethods do
  begin
    if (RttiMethod.MethodKind in [mkProcedure, mkFunction]) and
      (Length(RttiMethod.GetParameters) = 0) and
      (pos('TEST', UpperCase(RttiMethod.Name)) = 1) then
    begin
      RttiMethod.Invoke(TestCase, []);
    end;

  end;

end;

procedure TCastleTester.SetNotifyAssertFail(
  const ANotifyAssertFail: TNotifyAssertFail);
var
  TestCase: TCastleTestCase;
begin
  if @FNotifyAssertFail = @ANotifyAssertFail then
    Exit;

  FNotifyAssertFail := ANotifyAssertFail;
  for TestCase in FTestCaseList do
  begin
    TestCase.FNotifyAssertFail := ANotifyAssertFail;
  end;
end;

{ TCastleTestCase }

procedure TCastleTestCase.AssertEquals(Expected, Actual: String);
begin
  AssertTrue('Expected: ' + Expected + ' Actual: ' + Actual, Expected = Actual);
end;

procedure TCastleTestCase.AssertFalse(const Msg: String;
  const ACondition: Boolean);
begin
  AssertTrue(Msg, not ACondition);
end;

procedure TCastleTestCase.AssertFalse(const ACondition: Boolean);
begin
  AssertFalse('', ACondition);
end;

procedure TCastleTestCase.AssertFilenamesEqual(Expected, Actual: String);
begin
  AssertTrue('Expected: ' + Expected + ' Actual: ' + Actual, CompareFileName(Expected, Actual));
end;

procedure TCastleTestCase.AssertTrue(const Msg: String; const ACondition: Boolean);
begin
  if not ACondition then
    Fail(Msg);
end;

procedure TCastleTestCase.AssertTrue(const ACondition: Boolean);
begin
  AssertTrue('', ACondition);
end;

function TCastleTestCase.CompareFileName(Expected, Actual: String): Boolean;
begin
  {$ifdef MSWINDOWS}
  Result := UpperCase(Expected) = UpperCase(Actual);
  {$else}
  Result := Expected = Actual;
  {$endif}
end;

constructor TCastleTestCase.Create;
begin
  FName := ClassName;
end;

procedure TCastleTestCase.Fail(const Msg: String);
begin
  if Assigned(FNotifyAssertFail) then
    FNotifyAssertFail(Msg);

  raise EAssertionFailedError.Create(Msg);
end;

procedure TCastleTestCase.TestTestCase;
begin
  WritelnLog('TestTestCase');
end;



end.
