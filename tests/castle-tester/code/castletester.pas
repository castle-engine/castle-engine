unit CastleTester;

interface

uses SysUtils, Classes, Generics.Collections, Rtti;

type

  EAssertionFailedError = class(Exception);

  TNotifyAssertFail = procedure (const TestName, Msg: String) of object;
  TNotifyTestExecuted = procedure (const Name: String) of object;
  TNotifyTestCaseExecuted = procedure (const Name: String) of object;

  TCastleTest = class

  end;

  TCastleTestCase = class
  strict private
    FName: String;
  private
    FNotifyAssertFail: TNotifyAssertFail;
    FCurrentTestName: String;
  public
    procedure TestTestCase;
    constructor Create;

    procedure Fail(const Msg: String; ErrorAddr: Pointer = nil);
    procedure AssertTrue(const ACondition: Boolean); overload;
    procedure AssertTrue(const Msg: String; const ACondition: Boolean;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertFalse(const ACondition: Boolean); overload;
    procedure AssertFalse(const Msg: String; const ACondition: Boolean;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertFilenamesEqual(Expected, Actual: String);
    procedure AssertEquals(Expected, Actual: String);

    function CompareFileName(Expected, Actual: String): Boolean;

    property CurrentTestName: String read FCurrentTestName;
  end;


  TCastleTester = class (TComponent)
  strict private
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

  FTestCaseList := {$ifdef FPC}specialize{$endif} TObjectList<TCastleTestCase>.Create;
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
  // raise Exception.Create('Test Return Adress') at {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  RttiType := FRttiContext.GetType(TestCase.ClassInfo);

  for RttiMethod in RttiType.GetMethods do
  begin
    if (RttiMethod.MethodKind in [mkProcedure, mkFunction]) and
      (Length(RttiMethod.GetParameters) = 0) and
      (pos('TEST', UpperCase(RttiMethod.Name)) = 1) then
    begin
      TestCase.FCurrentTestName := RttiMethod.Name;
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
  AssertTrue('Expected: ' + Expected + ' Actual: ' + Actual, Expected = Actual,
    {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif});
end;

procedure TCastleTestCase.AssertFalse(const Msg: String;
  const ACondition: Boolean; ErrorAddr: Pointer);
begin
  if not Assigned(ErrorAddr) then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif});


  AssertTrue(Msg, not ACondition, ErrorAddr);
end;

procedure TCastleTestCase.AssertFalse(const ACondition: Boolean);
begin
  AssertFalse('', ACondition,
    {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif});
end;

procedure TCastleTestCase.AssertFilenamesEqual(Expected, Actual: String);
begin
  AssertTrue('Expected: ' + Expected + ' Actual: ' + Actual,
    CompareFileName(Expected, Actual),
    {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif});
end;

procedure TCastleTestCase.AssertTrue(const Msg: String;
  const ACondition: Boolean;  ErrorAddr: Pointer);
begin
  if not Assigned(ErrorAddr) then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not ACondition then
    Fail(Msg, ErrorAddr);
end;

procedure TCastleTestCase.AssertTrue(const ACondition: Boolean);
begin
  AssertTrue('', ACondition, {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif});
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

procedure TCastleTestCase.Fail(const Msg: String; ErrorAddr: Pointer);
begin
  if Assigned(FNotifyAssertFail) then
    FNotifyAssertFail(CurrentTestName, Msg);

  if Assigned(ErrorAddr) then
    raise EAssertionFailedError.Create(Msg) at ErrorAddr
  else
  begin
    raise EAssertionFailedError.Create(Msg) at
      {$ifdef FPC}
      // FPC https://www.freepascal.org/docs-html/ref/refse118.html#x244-26800017.1
      get_caller_addr(get_frame)
      {$else}
      // Delphi: https://stackoverflow.com/questions/8950513/what-does-at-returnaddress-mean-in-delphi
      ReturnAddress
      {$endif};
  end;
end;

procedure TCastleTestCase.TestTestCase;
begin
  WritelnLog('TestTestCase');
  //AssertTrue(false);
end;



end.
