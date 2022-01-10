unit CastleTester;

interface

uses SysUtils, Classes, Generics.Collections, Rtti, CastleVectors, CastleBoxes,
  CastleFrustum, CastleImages, CastleRectangles, CastleWindow, CastleViewport;

const
  { Epsilon used by default when compating Single (Single-precision float values).
    Compatible with Math unit value, used by standard routines like Math.SameValue
    and Math.IsZero. }
  SingleEpsilon = 1E-4;

  { Epsilon used by default when compating Double (Double-precision float values).
    Compatible with Math unit value, used by standard routines like Math.SameValue
    and Math.IsZero. }
  DoubleEpsilon = 1E-12;

type

  EAssertionFailedError = class(Exception);

  TNotifyAssertFail = procedure (const TestName, Msg: String) of object;
  TNotifyTestExecuted = procedure (const Name: String) of object;
  TNotifyTestCaseExecuted = procedure (const Name: String) of object;
  TNotifyTestCountChanged = procedure (const TestCount: Integer) of object;

  TCastleTesterMode = (
    ctmDesktop, // tester runs in UI application on desktop (Linux, Windows, MacOS)
    ctmConsole, // tester runs in console
    ctmMobile,  // tester runs on Android or iOs
    ctmWeb // for testing
  );

  TCastleTester = class;
  TCastleTestCase = class;

  TCastleTest = class
  strict private
    FTestCase: TCastleTestCase;
    FRttiMethod: TRttiMethod;
    FEnabled: Boolean;

    procedure SetEnabled(NewValue: Boolean);
  public
    Name: String;

    constructor Create(const ATestCase: TCastleTestCase; const AName: String;
      const ARttiMethod: TRttiMethod);

    procedure Run;

    function GetFullName: String;

    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  TCastleTestCase = class
  strict private
    FName: String;

    { Object list for tests }
    FTestList: {$ifdef FPC}specialize{$endif} TObjectList<TCastleTest>;

    function GetTest(const Index: Integer): TCastleTest;
  private
    FNotifyAssertFail: TNotifyAssertFail;
    FCurrentTestName: String;

    FWindowForTest: TCastleWindowBase;
    { Viewport from MainWindow in Desktop or Mobile mode or viewport from
      FWindowForViewportTest when Console Mode }
    FViewportForTest: TCastleViewport;
    { Window for Viewport when tester in Console mode }
    FWindowForViewportTest: TCastleWindowBase;
    { TCastleTester that runs test case }
    FCastleTester: TCastleTester;

    function PrepareCustomMsg(const Msg: String): String;
  public
    Enabled: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Setup; virtual;
    procedure TearDown; virtual;

    procedure Fail(const Msg: String; const ErrorAddr: Pointer = nil);
    procedure AssertTrue(const ACondition: Boolean); overload;
    procedure AssertTrue(const Msg: String; const ACondition: Boolean;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertFalse(const ACondition: Boolean); overload;
    procedure AssertFalse(const Msg: String; const ACondition: Boolean;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertFilenamesEqual(const Expected, Actual: String);
    procedure AssertEquals(const Expected, Actual: String); overload;
    procedure AssertEquals(const Expected, Actual: Boolean); overload;
    procedure AssertEquals(const Expected, Actual: Single); overload;
    procedure AssertEquals(const Expected, Actual: Integer); overload;
    procedure AssertEquals(const Msg: String; const Expected, Actual: Integer;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertEquals(const Msg, Expected, Actual: String;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertEquals(const Msg: String; const Expected, Actual: Boolean;
      ErrorAddr: Pointer = nil); overload;

    procedure AssertSameValue(const Expected, Actual: Single;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertSameValue(const Expected, Actual: Single;
      const Epsilon: Single; ErrorAddr: Pointer = nil); overload;

    procedure AssertMatrixEquals(const Expected, Actual: TMatrix4;
      const Epsilon: Single; ErrorAddr: Pointer = nil);

    procedure AssertVectorEquals(const Expected, Actual: TVector2Byte;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector3Byte;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector4Byte;
      ErrorAddr: Pointer = nil); overload;

    procedure AssertVectorEquals(const Expected, Actual: TVector2;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector3;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector4;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector2;
      const Epsilon: Single; ErrorAddr: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector3;
      const Epsilon: Single; ErrorAddr: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector4;
      const Epsilon: Single; ErrorAddr: Pointer = nil); overload;

    { Check that 3D planes (defined by equation Ax+By+Cz+D=0) are equal.
      The vectors must be a component-wise multiplication of each other. }
    procedure AssertPlaneEquals(const Expected, Actual: TVector4;
      const Epsilon: Single; ErrorAddr: Pointer = nil); overload;
    procedure AssertPlaneEquals(const Expected, Actual: TVector4;
      ErrorAddr: Pointer = nil); overload;


    procedure AssertBoxesEqual(const Expected, Actual: TBox3D;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertBoxesEqual(const Expected, Actual: TBox3D;
      const Epsilon: Double; ErrorAddr: Pointer = nil); overload;

    procedure AssertImagesEqual(const Expected, Actual: TRGBAlphaImage;
      ErrorAddr: Pointer = nil);

    procedure AssertRectsEqual(const Expected, Actual: TRectangle;
      ErrorAddr: Pointer = nil); overload;
    procedure AssertRectsEqual(const Expected, Actual: TFloatRectangle;
      ErrorAddrs: Pointer = nil); overload;

    procedure AssertFrustumEquals(const Expected, Actual: TFrustum;
      const Epsilon: Single; ErrorAddr: Pointer = nil); overload;
    procedure AssertFrustumEquals(const Expected, Actual: TFrustum;
      ErrorAddr: Pointer = nil); overload;


    function CompareFileName(Expected, Actual: String): Boolean;
    function GetTempDirectory: String;

    procedure TestLog(Text: String);

    procedure OnWarningRaiseException(const Category, S: string);

    { Method that facilitates the creation of the window for the test
      it will be freed when test method ends but if you need test
      window destroying call DestroyWindowForTest }
    function CreateWindowForTest: TCastleWindowBase;
    procedure DestroyWindowForTest;

    { If you don't need window for testing you can simply get a viewport.
      This view port is automatically cleaned when test method ends.
      In UI mode it's in main window }
    function GetTestingViewport: TCastleViewport;

    { Used by TCastleTester.Scan to add tests }
    function AddTest(const AName: String;
      const ARttiMethod: TRttiMethod): TCastleTest;

    function IsConsoleMode: Boolean;

    { Clears test list }
    procedure ClearTests;

    function TestCount: Integer;
    function EnabledTestCount: Integer;

    property Test[const Index: Integer]: TCastleTest read GetTest;

    property CurrentTestName: String read FCurrentTestName;

  published
    procedure TestTestCase;

  end;


  TCastleTester = class (TComponent)
  strict private
    { Rtti context to run all test methods }
    FRttiContext: TRttiContext;

    { Object list for test cases }
    FTestCaseList: {$ifdef FPC}specialize{$endif} TObjectList<TCastleTestCase>;

    { Object list for test that should be executed }
    FTestsToRun: {$ifdef FPC}specialize{$endif} TList<TCastleTest>;
    FLastRunningTestIndex: Integer;

    { Test app window when tester runs in UI mode or nil in console mode }
    FUIWindow: TCastleWindowBase;

    { Flag to stop on first test fail }
    FStopOnFirstFail: Boolean;

    FTestsCount: Integer;
    FTestPassedCount: Integer;
    FTestFailedCount: Integer;

    procedure SetNotifyAssertFail(const ANotifyAssertFail: TNotifyAssertFail);

    { Scans test case using RTTI }
    procedure ScanTestCase(TestCase: TCastleTestCase);

    procedure SetTestCount(const NewTestCount: Integer);

    procedure RunTest(Test: TCastleTest);

    procedure SetTestPassedCount(const NewTestCount: Integer);
    procedure SetTestFailedCount(const NewTestCount: Integer);

  private
    { Callbacks to change UI }
    FNotifyTestExecuted: TNotifyTestExecuted;
    FNotifyTestCaseExecuted: TNotifyTestCaseExecuted;
    FNotifyAssertFail: TNotifyAssertFail;
    FNotifyTestCountChanged: TNotifyTestCountChanged;
    FNotifyTestPassedChanged: TNotifyTestCountChanged;
    FNotifyTestFailedChanged: TNotifyTestCountChanged;
    FNotifyEnabledTestCountChanged: TNotifyEvent;

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    { Adds testcase to tester }
    procedure AddTestCase(const TestCase: TCastleTestCase);
    { Scans added test cases }
    procedure Scan;
    { Prepares list of tests to run }
    procedure PrepareTestListToRun;

    { Running all test in one loop - good for console because blocks UI }
    procedure Run;

    { Returns true when there is next test to run }
    function IsNextTestToRun: Boolean;
    { Runs next test, good for UI apps, can raise exceptions }
    procedure RunNextTest;

    { Returns tester mode (UI or Console) }
    function Mode: TCastleTesterMode;

    function EnabledTestCount: Integer;

    { Stop testing on first fail or run all tests }
    property StopOnFirstFail: Boolean read FStopOnFirstFail
      write FStopOnFirstFail default true;

    property TestsCount: Integer read FTestsCount;
    property TestPassedCount: Integer read FTestPassedCount
      write SetTestPassedCount;
    property TestFailedCount: Integer read FTestFailedCount
      write SetTestFailedCount;

    property NotifyTestExecuted: TNotifyTestExecuted read FNotifyTestExecuted
      write FNotifyTestExecuted;

    property NotifyTestCaseExecuted: TNotifyTestCaseExecuted
      read FNotifyTestCaseExecuted write FNotifyTestCaseExecuted;

    { Callback after test fail }
    property NotifyAssertFail: TNotifyAssertFail read FNotifyAssertFail
      write SetNotifyAssertFail;

    { Callback after test count changed }
    property NotifyTestCountChanged: TNotifyTestCountChanged
      read FNotifyTestCountChanged write FNotifyTestCountChanged;

    { Callback after passed test count changed }
    property NotifyTestPassedChanged: TNotifyTestCountChanged
      read FNotifyTestPassedChanged write FNotifyTestPassedChanged;

    { Callback after failed test count changed }
    property NotifyTestFailedChanged: TNotifyTestCountChanged
      read FNotifyTestFailedChanged write FNotifyTestFailedChanged;

    { Callback after enabled test count changed }
    property NotifyEnabledTestCountChanged: TNotifyEvent
      read FNotifyEnabledTestCountChanged write FNotifyEnabledTestCountChanged;
  end;

implementation

{ TCastleTester }

uses CastleLog, TypInfo, Math, {$ifndef FPC}IOUtils,{$endif} CastleUtils;


procedure TCastleTester.AddTestCase(const TestCase: TCastleTestCase);
begin
  FTestCaseList.Add(TestCase);
  TestCase.FNotifyAssertFail := FNotifyAssertFail;
  TestCase.FCastleTester := Self;
end;

constructor TCastleTester.Create(AOwner: TComponent);
begin
  inherited;

  StopOnFirstFail := true;

  FTestCaseList := {$ifdef FPC}specialize{$endif} TObjectList<TCastleTestCase>.Create;
  FRttiContext := TRttiContext.Create;

  FTestsToRun := {$ifdef FPC}specialize{$endif} TList<TCastleTest>.Create;
end;

destructor TCastleTester.Destroy;
begin
  FRttiContext.Free;
  FreeAndNil(FTestsToRun);
  FreeAndNil(FTestCaseList);
  inherited;
end;

function TCastleTester.IsNextTestToRun: Boolean;
begin
  Result := (FLastRunningTestIndex < FTestsToRun.Count);
end;

function TCastleTester.Mode: TCastleTesterMode;
begin
  if FUIWindow = nil then
    Result := ctmConsole
  else
    {$if defined(ANDROID) or defined(iPHONESIM) or defined(iOS)}
    Result := ctmMobile;
    {$else}
    Result := ctmDesktop;
    {$endif}
end;

procedure TCastleTester.PrepareTestListToRun;
var
  I, J: Integer;
  TestCase: TCastleTestCase;
  Test: TCastleTest;
begin
  FLastRunningTestIndex := 0;
  FUIWindow := Application.MainWindow;
  FTestsToRun.Clear;

  for I := 0 to FTestCaseList.Count -1 do
  begin
    TestCase := FTestCaseList[I];
    if TestCase.Enabled then
    begin
      for J := 0 to TestCase.TestCount -1 do
      begin
        Test := TestCase.Test[J];
        if Test.Enabled then
          FTestsToRun.Add(Test);
      end;
    end;
  end;

  TestPassedCount := 0;
  TestFailedCount := 0;
  if Assigned(FNotifyEnabledTestCountChanged) then
    NotifyEnabledTestCountChanged(Self);
end;

procedure TCastleTester.Run;
var
  I: Integer;
begin
  FUIWindow := Application.MainWindow;
  FLastRunningTestIndex := 0;

  for I := 0 to FTestsToRun.Count -1 do
    RunTest(FTestsToRun[I]);
end;

procedure TCastleTester.RunNextTest;
begin
  RunTest(FTestsToRun[FLastRunningTestIndex]);
  Inc(FLastRunningTestIndex);
end;

procedure TCastleTester.RunTest(Test: TCastleTest);
begin
  if not Test.Enabled then
    Exit;

  if Assigned(FNotifyTestCaseExecuted) then
    FNotifyTestCaseExecuted(Test.GetFullName);

  try
    Test.Run;
    TestPassedCount := TestPassedCount + 1;
  except
    on E: EAssertionFailedError do
    begin
      TestFailedCount := TestFailedCount + 1;
      if FStopOnFirstFail then
        raise;
    end;
    on E: Exception do
    begin
      // TODO: warnning here when we don't stop on first Exception.
      if FStopOnFirstFail then
        raise;
    end;
  end;
end;

procedure TCastleTester.Scan;
var
  I: Integer;
  TestCase: TCastleTestCase;
begin
  FTestsCount := 0;

  for I := 0 to FTestCaseList.Count -1 do
  begin
    TestCase := FTestCaseList[I];
    TestCase.ClearTests;
    ScanTestCase(TestCase);
    SetTestCount(TestsCount + TestCase.TestCount);
  end;
end;

procedure TCastleTester.ScanTestCase(TestCase: TCastleTestCase);
var
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
  Test: TCastleTest;
begin
  // raise Exception.Create('Test Return Adress') at {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  TestCase.Setup;
  try
    RttiType := FRttiContext.GetType(TestCase.ClassInfo);

    for RttiMethod in RttiType.GetMethods do
    begin
      if (RttiMethod.MethodKind in [mkProcedure, mkFunction]) and
        (Length(RttiMethod.GetParameters) = 0) and
        (pos('TEST', UpperCase(RttiMethod.Name)) = 1) then
      begin
        //TestCase.FCurrentTestName := RttiMethod.Name;
        //RttiMethod.Invoke(TestCase, []);
        TestCase.AddTest(RttiMethod.Name, RttiMethod);
      end;

    end;
  finally
    TestCase.TearDown;
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

procedure TCastleTester.SetTestCount(const NewTestCount: Integer);
begin
  FTestsCount := NewTestCount;

  if Assigned(FNotifyTestCountChanged) then
    FNotifyTestCountChanged(FTestsCount);
end;

procedure TCastleTester.SetTestFailedCount(const NewTestCount: Integer);
begin
  FTestFailedCount := NewTestCount;

  if Assigned(FNotifyTestFailedChanged) then
    FNotifyTestFailedChanged(FTestFailedCount);
end;

procedure TCastleTester.SetTestPassedCount(const NewTestCount: Integer);
begin
  FTestPassedCount := NewTestCount;

  if Assigned(FNotifyTestPassedChanged) then
    FNotifyTestPassedChanged(FTestPassedCount);
end;

function TCastleTester.EnabledTestCount: Integer;
var
  I: Integer;
  TestCase: TCastleTestCase;
begin
  Result := 0;

  for I := 0 to FTestCaseList.Count -1 do
  begin
    TestCase := FTestCaseList[I];
    if TestCase.Enabled then
      Inc(Result, TestCase.EnabledTestCount);
  end;
end;

{ TCastleTestCase }

procedure TCastleTestCase.AssertEquals(const Expected, Actual: String);
begin
  AssertEquals('', Expected, Actual,
    {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif});
end;

procedure TCastleTestCase.AssertEquals(const Expected, Actual: Boolean);
begin
  AssertEquals('', Expected, Actual,
    {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif});
end;

function TCastleTestCase.AddTest(const AName: String;
  const ARttiMethod: TRttiMethod): TCastleTest;
begin
  Result := TCastleTest.Create(Self, AName, ARttiMethod);
  FTestList.Add(Result);
end;

procedure TCastleTestCase.AssertBoxesEqual(const Expected, Actual: TBox3D;
  const Epsilon: Double; ErrorAddr: Pointer);
var
  I: Integer;
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  if Expected.IsEmpty and Actual.IsEmpty then
    Exit; // OK

  if Expected.IsEmpty then
    Fail(Format('Expected empty box, actual box is NOT empty (%s)',
      [Actual.ToRawString]));

  if Actual.IsEmpty then
    Fail(Format('Expected NOT empty box (%s), actual box is empty',
      [Expected.ToRawString]), ErrorAddr);

  for I := 0 to 2 do
    if (not SameValue(Expected.Data[0][I], Actual.Data[0][I], Epsilon)) or
       (not SameValue(Expected.Data[1][I], Actual.Data[1][I], Epsilon)) then
      Fail(Format('Boxes are not equal: expected: %s, actual: %s',
        [Expected.ToRawString, Actual.ToRawString]), ErrorAddr);
end;

procedure TCastleTestCase.AssertBoxesEqual(const Expected, Actual: TBox3D;
  ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  AssertBoxesEqual(Expected, Actual, SingleEpsilon, ErrorAddr);
end;

procedure TCastleTestCase.AssertEquals(const Expected, Actual: Single);
begin
  AssertTrue('Expected: ' + FloatToStr(Expected) + ' Actual: ' +
    FloatToStr(Actual), SameValue(Expected, Actual, SingleEpsilon),
    {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif});
end;

procedure TCastleTestCase.AssertFalse(const Msg: String;
  const ACondition: Boolean; ErrorAddr: Pointer);
begin
  if not Assigned(ErrorAddr) then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};


  AssertTrue(Msg, not ACondition, ErrorAddr);
end;

procedure TCastleTestCase.AssertFalse(const ACondition: Boolean);
begin
  AssertFalse('', ACondition,
    {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif});
end;

procedure TCastleTestCase.AssertFilenamesEqual(const Expected, Actual: String);
begin
  AssertTrue('Expected: ' + Expected + ' Actual: ' + Actual,
    CompareFileName(Expected, Actual),
    {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif});
end;

procedure TCastleTestCase.AssertFrustumEquals(const Expected, Actual: TFrustum;
  ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};
  AssertFrustumEquals(Expected, Actual, SingleEpsilon, ErrorAddr);
end;

procedure TCastleTestCase.AssertImagesEqual(const Expected,
  Actual: TRGBAlphaImage; ErrorAddr: Pointer);
var
  ExpectedPtr, ActualPtr: PVector4Byte;
  I: Integer;
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  // Overloaded version with AErrorAddrs is missing for fpcunit AssertEquals
  AssertEquals(Expected.Width, Actual.Width{, AErrorAddrs});
  AssertEquals(Expected.Height, Actual.Height{, AErrorAddrs});
  AssertEquals(Expected.Depth, Actual.Depth{, AErrorAddrs});
  ExpectedPtr := Expected.Pixels;
  ActualPtr := Actual.Pixels;
  for I := 1 to Actual.Width * Actual.Height * Actual.Depth do
  begin
    AssertVectorEquals(ExpectedPtr^, ActualPtr^, ErrorAddr);
    Inc(ExpectedPtr);
    Inc(ActualPtr);
  end;
end;

procedure TCastleTestCase.AssertFrustumEquals(const Expected, Actual: TFrustum;
  const Epsilon: Single; ErrorAddr: Pointer);
var
  I: TFrustumPlane;
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  try
    AssertEquals(Expected.ZFarInfinity, Actual.ZFarInfinity);

    if Expected.ZFarInfinity then
    begin
      for I := Low(I) to Pred(High(I)) do
        AssertPlaneEquals(Expected.Planes[I], Actual.Planes[I], Epsilon);
    end else
    begin
      for I := Low(I) to High(I) do
        AssertPlaneEquals(Expected.Planes[I], Actual.Planes[I], Epsilon);
    end;
  except
    on E: Exception do
    begin
      Fail(Format('Expected frustum (%s) does not equal actual (%s). The underlying difference exception: %s', [
        Expected.ToString('  '),
        Actual.ToString('  '),
        E.Message
      ]), ErrorAddr);
    end;
  end;
end;

procedure TCastleTestCase.AssertMatrixEquals(const Expected, Actual: TMatrix4;
  const Epsilon: Single; ErrorAddr: Pointer);
var
  DifferenceEpsilon: Single;
  I, J: TMatrix4.TIndex;
begin
  if not Assigned(ErrorAddr) then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  if not TMatrix4.Equals(Expected, Actual, Epsilon) then
  begin
    DifferenceEpsilon := 0;
    for I := Low(TMatrix4.TIndex) to High(TMatrix4.TIndex) do
      for J := Low(TMatrix4.TIndex) to High(TMatrix4.TIndex) do
        MaxVar(DifferenceEpsilon, Abs(Expected[I, J] - Actual[I, J]));

    Fail(Format('Matrices (TMatrix4) are not equal:' + LineEnding +
      '  Expected:' + LineEnding +
      '%s' + LineEnding +
      '  Actual:' + LineEnding +
      '%s' + LineEnding +
      '  The epsilon to ignore the difference would need to be >= %.10f, but is %.10f',
      [Expected.ToRawString('    '),
       Actual.ToRawString('    '),
       DifferenceEpsilon,
       Epsilon
      ]), ErrorAddr);
  end;
end;

procedure TCastleTestCase.AssertPlaneEquals(const Expected, Actual: TVector4;
  ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};
  AssertPlaneEquals(Expected, Actual, SingleEpsilon, ErrorAddr);
end;

procedure TCastleTestCase.AssertRectsEqual(const Expected,
  Actual: TFloatRectangle; ErrorAddrs: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  if not Expected.Equals(Actual) then
    Fail(Format('Expected rect (%s) does not equal actual (%s)',
      [Expected.ToString, Actual.ToString]), ErrorAddr);
end;

procedure TCastleTestCase.AssertRectsEqual(const Expected, Actual: TRectangle;
  ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  if not Expected.Equals(Actual) then
    Fail(Format('Expected rect (%s) does not equal actual (%s)',
      [Expected.ToString, Actual.ToString]), ErrorAddr);
end;

procedure TCastleTestCase.AssertPlaneEquals(const Expected, Actual: TVector4;
  const Epsilon: Single; ErrorAddr: Pointer);
var
  MaxE, MaxA: Integer;
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  MaxE := MaxAbsVectorCoord(Expected);
  MaxA := MaxAbsVectorCoord(Actual);

  if MaxE <> MaxA then
    Fail(Format('Planes (TVector4) are not equal, their maximum component index differs. Expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), ErrorAddr);

  if IsZero(Expected[MaxE], Epsilon) and
     IsZero(  Actual[MaxA], Epsilon) then
  begin
    if not (Expected.IsZero(Epsilon) and Actual.IsZero(Epsilon)) then
      Fail(Format('Planes (TVector4) are not equal, they should be both zero since maximum component is zero. Expected: %s, actual: %s',
        [Expected.ToRawString, Actual.ToRawString]), ErrorAddr);
  end else
  if IsZero(Expected[MaxE], Epsilon) or
     IsZero(  Actual[MaxA], Epsilon) then
  begin
    Fail(Format('Planes (TVector4) are not equal, one of them has zero maximum component, the other not. Expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), ErrorAddr);
  end else
  begin
    if not TVector4.Equals(
      Expected,
      Actual * (Expected[MaxE] / Actual[MaxA]),
      Epsilon
    ) then
      Fail(Format('Planes (TVector4) are not equal, they are not multiplied version of each other. Expected: %s, actual: %s. After trying to bring them closer, actual is %s', [
        Expected.ToRawString,
        Actual.ToRawString,
        (Actual * (Expected[MaxE] / Actual[MaxA])).ToRawString
      ]), ErrorAddr);
  end;
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual,
  Epsilon: Single; ErrorAddr: Pointer);
begin
  if not Assigned(ErrorAddr) then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  if not SameValue(Expected, Actual, Epsilon) then
    Fail(Format('Floats (Single) are not equal: expected: %g, actual: %g',
      [Expected, Actual]), ErrorAddr);
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual: Single;
  ErrorAddr: Pointer);
begin
  if not Assigned(ErrorAddr) then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  AssertSameValue(Expected, Actual, SingleEpsilon, ErrorAddr);
end;

procedure TCastleTestCase.AssertTrue(const Msg: String;
  const ACondition: Boolean; ErrorAddr: Pointer);
begin
  if not Assigned(ErrorAddr) then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not ACondition then
    Fail(Msg, ErrorAddr);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected,
  Actual: TVector4Byte; ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector4Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector4Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]), ErrorAddr);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected,
  Actual: TVector3Byte; ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector3Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector3Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]), ErrorAddr);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected,
  Actual: TVector2Byte; ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector2Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector2Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]), ErrorAddr);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected, Actual: TVector3;
  ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertVectorEquals(Expected, Actual, SingleEpsilon, ErrorAddr);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected, Actual: TVector4;
  const Epsilon: Single; ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector4.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector4) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), ErrorAddr);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected, Actual: TVector3;
  const Epsilon: Single; ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector3.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector3) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), ErrorAddr);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected, Actual: TVector2;
  const Epsilon: Single; ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector2.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector2) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), ErrorAddr);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected, Actual: TVector4;
  ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertVectorEquals(Expected, Actual, SingleEpsilon, ErrorAddr);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected, Actual: TVector2;
  ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertVectorEquals(Expected, Actual, SingleEpsilon, ErrorAddr);
end;

procedure TCastleTestCase.AssertTrue(const ACondition: Boolean);
begin
  AssertTrue('', ACondition, {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif});
end;

procedure TCastleTestCase.ClearTests;
begin
  FTestList.Clear;
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
  FTestList := {$ifdef FPC}specialize{$endif} TObjectList<TCastleTest>.Create;
  Enabled := true;
  FWindowForTest := nil;
end;

function TCastleTestCase.CreateWindowForTest: TCastleWindowBase;
begin
  FWindowForTest := TCastleWindowBase.Create(nil);
  if FCastleTester.Mode = ctmConsole then
  begin
    Application.MainWindow := FWindowForTest;
  end;
end;

destructor TCastleTestCase.Destroy;
begin
  FreeAndNil(FTestList);
  inherited;
end;

procedure TCastleTestCase.DestroyWindowForTest;
begin
  FreeAndNil(FWindowForTest);
  if FCastleTester.Mode = ctmConsole then
  begin
    Application.MainWindow := nil;
  end;
end;

function TCastleTestCase.EnabledTestCount: Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to FTestList.Count -1 do
  begin
    if FTestList[I].Enabled then
      Inc(Result);
  end;
end;

procedure TCastleTestCase.Fail(const Msg: String; const ErrorAddr: Pointer);
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

function TCastleTestCase.GetTempDirectory: String;
begin
  {$ifdef FPC}
  Result := GetTempDir;
  {$else}
  Result := TPath.GetTempPath;
  {$endif}
end;

function TCastleTestCase.GetTest(const Index: Integer): TCastleTest;
begin
  Result := FTestList[Index];
end;

function TCastleTestCase.GetTestingViewport: TCastleViewport;
begin

end;

function TCastleTestCase.IsConsoleMode: Boolean;
begin
  Result := (FCastleTester.Mode = ctmConsole);
end;

procedure TCastleTestCase.OnWarningRaiseException(const Category, S: string);
begin
  raise Exception.CreateFmt(ClassName +
    ': received a warning, and any warning here is an error: %s: %s',
    [Category,
    S]
  );
end;

function TCastleTestCase.PrepareCustomMsg(const Msg: String): String;
begin
  Result := Trim(Msg);
  if Length(Result) > 0 then
    Result := '"' + Result + '" ';
end;

procedure TCastleTestCase.Setup;
begin

end;

procedure TCastleTestCase.TearDown;
begin

end;

function TCastleTestCase.TestCount: Integer;
begin
  Result := FTestList.Count;
end;

procedure TCastleTestCase.TestLog(Text: String);
begin
  WritelnLog(Text);
end;

procedure TCastleTestCase.TestTestCase;
begin
  TestLog('TestTestCase');
  //AssertTrue('Just testing failing', false);
end;



procedure TCastleTestCase.AssertEquals(const Expected, Actual: Integer);
begin
  AssertEquals('', Expected, Actual,
    {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif});
end;

procedure TCastleTestCase.AssertEquals(const Msg: String; const Expected,
  Actual: Integer; ErrorAddr: Pointer = nil);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertTrue(PrepareCustomMsg(Msg) + 'Expected: ' + IntToStr(Expected) +
    ' Actual: ' + IntToStr(Actual), Expected = Actual, ErrorAddr);
end;

procedure TCastleTestCase.AssertEquals(const Msg, Expected, Actual: String;
  ErrorAddr: Pointer = nil);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertTrue(PrepareCustomMsg(Msg) + 'Expected: ' + Expected +
    ' Actual: ' + Actual, Expected = Actual, ErrorAddr);
end;

procedure TCastleTestCase.AssertEquals(const Msg: String; const Expected,
  Actual: Boolean; ErrorAddr: Pointer);
begin
  if ErrorAddr = nil then
    ErrorAddr := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertTrue(PrepareCustomMsg(Msg) + 'Expected: ' + BoolToStr(Expected, true) + ' Actual: ' +
    BoolToStr(Actual, true), Expected = Actual, ErrorAddr);
end;

{ TCastleTest }

constructor TCastleTest.Create(const ATestCase: TCastleTestCase;
  const AName: String; const ARttiMethod: TRttiMethod);
begin
  FTestCase := ATestCase;
  Name := AName;
  FEnabled := true;
  FRttiMethod := ARttiMethod;
end;

function TCastleTest.GetFullName: String;
begin
  Result:= FTestCase.ClassName + '.' + Name;
end;

procedure TCastleTest.Run;
begin
  FTestCase.FCurrentTestName := GetFullName;
  try
    FRttiMethod.Invoke(FTestCase, []);
  finally
    { Some tests need a new window after running test we check should it be
      freed }
    FTestCase.DestroyWindowForTest;
  end;
end;

procedure TCastleTest.SetEnabled(NewValue: Boolean);
begin
  if FEnabled = NewValue then
    Exit;

  FEnabled := NewValue;

  if Assigned(FTestCase.FCastleTester.FNotifyEnabledTestCountChanged) then
    FTestCase.FCastleTester.FNotifyEnabledTestCountChanged(Self);
end;

end.
