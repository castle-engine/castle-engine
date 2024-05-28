﻿{
  Copyright 2022-2024 Andrzej Kilijański, Dean Zobec, Michael Van Canneyt, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Running tests, compatible with fpcunit, using some code from fpcunit
  (part of Free Component Library (FCL)) by Dean Zobec, Michael Van Canneyt.
  See https://wiki.lazarus.freepascal.org/fpcunit for more info. }
unit CastleTester;

interface

// FPC: Do not warn that Rtti is experimental
{$ifdef FPC} {$warnings off} {$endif}

uses SysUtils, Classes, Generics.Collections, Rtti, CastleVectors, CastleBoxes,
  CastleFrustum, CastleImages, CastleRectangles, CastleWindow, CastleViewport;

{$ifdef FPC} {$warnings on} {$endif}

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

  TNotifyTestFail = procedure (const TestName, Msg: String) of object;
  TNotifyTestExecuted = procedure (const Name: String) of object;
  TNotifyTestCaseExecuted = procedure (const Name: String) of object;
  TNotifyTestCountChanged = procedure (const TestCount: Integer) of object;

  TCastleTester = class;

  { Generate type info for TCastleTestCase and descendants.
    Delphi expects the forward class declaration to have the same $M state
    as an actual class declaration. }
  {$M+}
  TCastleTestCase = class;
  {$M-}

  TCastleTest = class
  strict private
    FTestCase: TCastleTestCase;
    {$ifdef FPC}
    FMethodPointer: CodePointer;
    {$else}
    FRttiMethod: TRttiMethod;
    {$endif}
    FEnabled: Boolean;

    procedure SetEnabled(NewValue: Boolean);
  public
    Name: String;

    constructor Create(const ATestCase: TCastleTestCase; const AName: String;
      {$ifdef FPC}const AMethodPointer: CodePointer{$else}
      const ARttiMethod: TRttiMethod{$endif});

    procedure Run;
    { Runs TestCase.Setup }
    procedure Setup;
    { Runs TestCase.TearDown }
    procedure TearDown;

    { Test case name with test name }
    function GetFullName: String;

    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  {$M+} // Generate type info for TCastleTestCase and descendants
  TCastleTestCase = class
  strict private
    FName: String;
    SavedApplicationMainWindow: TCastleWindow;

    { Object list for tests }
    FTestList: {$ifdef FPC}specialize{$endif} TObjectList<TCastleTest>;

    function GetTest(const Index: Integer): TCastleTest;
  private
    FNotifyTestFail: TNotifyTestFail;
    FCurrentTestName: String;

    FWindowForTest: TCastleWindow;
    { Viewport from Application.MainWindow in non-console mode,
      or viewport from FWindowForViewportTest when Console Mode. }
    FViewportForTest: TCastleViewport;
    { Window for Viewport when tester in Console mode }
    FWindowForViewportTest: TCastleWindow;
    { TCastleTester that runs test case }
    FCastleTester: TCastleTester;

    function PrepareCustomMsg(const Msg: String): String;
  protected
    procedure Setup; virtual;
    procedure TearDown; virtual;
  public
    Enabled: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Fail(const Msg: String; const AddrOfError: Pointer = nil);
    procedure AssertTrue(const ACondition: Boolean); overload;
    procedure AssertTrue(const Msg: String; const ACondition: Boolean;
      AddrOfError: Pointer = nil); overload;
    procedure AssertFalse(const ACondition: Boolean); overload;
    procedure AssertFalse(const Msg: String; const ACondition: Boolean;
      AddrOfError: Pointer = nil); overload;
    procedure AssertFilenamesEqual(const Expected, Actual: String);
    procedure AssertEquals(const Expected, Actual: String); overload;
    procedure AssertEquals(const Expected, Actual: Boolean); overload;
    procedure AssertEquals(const Expected, Actual: Single); overload;
    procedure AssertEquals(const Expected, Actual: Integer); overload;
    procedure AssertEquals(const Expected, Actual: Cardinal); overload;
    procedure AssertEquals(const Expected, Actual: Int64); overload;
    procedure AssertEquals(const Msg: String; const Expected, Actual: Integer;
      AddrOfError: Pointer = nil); overload;
    procedure AssertEquals(const Msg, Expected, Actual: String;
      AddrOfError: Pointer = nil); overload;
    procedure AssertEquals(const Msg: String; const Expected, Actual: Boolean;
      AddrOfError: Pointer = nil); overload;
    procedure AssertEquals(const Msg: String; const Expected, Actual: Cardinal;
      AddrOfError: Pointer = nil); overload;
    procedure AssertEquals(const Msg: String; const Expected, Actual: Int64;
      AddrOfError: Pointer = nil); overload;

    procedure AssertSameValue(const Expected, Actual: Single;
      AddrOfError: Pointer = nil); overload;
    procedure AssertSameValue(const Expected, Actual: Single;
      const Epsilon: Single; AddrOfError: Pointer = nil); overload;

    procedure AssertMatrixEquals(const Expected, Actual: TMatrix4;
      const Epsilon: Single; AddrOfError: Pointer = nil); overload;
    procedure AssertMatrixEquals(const Expected, Actual: TMatrix4;
      AddrOfError: Pointer = nil); overload;

    procedure AssertVectorEquals(const Expected, Actual: TVector2Byte;
      AddrOfError: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector3Byte;
      AddrOfError: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector4Byte;
      AddrOfError: Pointer = nil); overload;

    procedure AssertVectorEquals(const Expected, Actual: TVector2;
      AddrOfError: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector3;
      AddrOfError: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector4;
      AddrOfError: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector2;
      const Epsilon: Single; AddrOfError: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector3;
      const Epsilon: Single; AddrOfError: Pointer = nil); overload;
    procedure AssertVectorEquals(const Expected, Actual: TVector4;
      const Epsilon: Single; AddrOfError: Pointer = nil); overload;

    { Check that 3D planes (defined by equation Ax+By+Cz+D=0) are equal.
      The vectors must be a component-wise multiplication of each other. }
    procedure AssertPlaneEquals(const Expected, Actual: TVector4;
      const Epsilon: Single; AddrOfError: Pointer = nil); overload;
    procedure AssertPlaneEquals(const Expected, Actual: TVector4;
      AddrOfError: Pointer = nil); overload;

    { TODO: Need to have different names to avoid FPC errors "duplicate ASM label",
      see https://bugs.freepascal.org/view.php?id=32188 }
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector2Double;
      AddrOfError: Pointer = nil); overload;
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector3Double;
      AddrOfError: Pointer = nil); overload;
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector4Double;
      AddrOfError: Pointer = nil); overload;
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector2Double;
      const Epsilon: Single; AddrOfError: Pointer = nil); overload;
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector3Double;
      const Epsilon: Single; AddrOfError: Pointer = nil); overload;
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector4Double;
      const Epsilon: Single; AddrOfError: Pointer = nil); overload;
    procedure AssertSameValue(const Expected, Actual: Double;
      AddrOfError: Pointer = nil); overload;
    procedure AssertSameValue(const Expected, Actual: Double;
      const Epsilon: Double; AddrOfError: Pointer = nil); overload;

    procedure AssertBoxesEqual(const Expected, Actual: TBox3D;
      AddrOfError: Pointer = nil); overload;
    procedure AssertBoxesEqual(const Expected, Actual: TBox3D;
      const Epsilon: Double; AddrOfError: Pointer = nil); overload;
    procedure AssertBoxesEqual(const Msg: String; const Expected, Actual: TBox3D;
      AddrOfError: Pointer = nil); overload;
    procedure AssertBoxesEqual(const Msg: String; const Expected, Actual: TBox3D;
      const Epsilon: Double; AddrOfError: Pointer = nil); overload;

    procedure AssertImagesEqual(const Expected, Actual: TCastleImage;
      AddrOfError: Pointer = nil);

    procedure AssertRectsEqual(const Expected, Actual: TRectangle;
      AddrOfError: Pointer = nil); overload;
    procedure AssertRectsEqual(const Expected, Actual: TFloatRectangle;
      AddrOfError: Pointer = nil); overload;

    procedure AssertFrustumEquals(const Expected, Actual: TFrustum;
      const Epsilon: Single; AddrOfError: Pointer = nil); overload;
    procedure AssertFrustumEquals(const Expected, Actual: TFrustum;
      AddrOfError: Pointer = nil); overload;

    function CompareFileName(Expected, Actual: String): Boolean;

    { Get temporary directory, implementation that works for both Delphi and FPC. }
    function GetTempDirectory: String;

    procedure TestLog(Text: String);

    procedure OnWarningRaiseException(const Category, S: string);

    { Create TCastleWindow for test purposes.

      Only one such window may exist at given time,
      it is also automatically set as Application.MainWindow. }
    function CreateWindowForTest: TCastleWindow;

    { Destroy TCastleWindow created by CreateWindowForTest.
      This method also checks Window value and sets it to @nil.

      It is often not necessary to actually use it,
      because the window will be automatically freed when test method ends anyway. }
    procedure DestroyWindowForTest(var Window: TCastleWindow);

    { If you need a TCastleViewport for testing, you can use this one.
      This viewport is automatically cleaned when test method ends. }
    //function GetTestingViewport: TCastleViewport;

    { Used by TCastleTester.Scan to add tests }
    function AddTest(const AName: String;
      {$ifdef FPC}const AMethodPointer: CodePointer{$else}
      const ARttiMethod: TRttiMethod{$endif}): TCastleTest;

    { Does the current platform allow to create new TCastleWindow during tests.

      In case you do manually TCastleWindow.Create call,
      you should also honour this method, do not create TCastleWindow instance
      when this is @false. Abort the test (without any failure) in this case.

      All test windows should be created using CreateWindowForTest now,
      and CreateWindowForTest will actually raise exception if this is true.

      This is @false on mobile or when run with --no-window-create . }
    function CanCreateWindowForTest: Boolean;

    { Clears test list }
    procedure ClearTests;

    function TestCount: Integer;
    function EnabledTestCount: Integer;

    property Test[const Index: Integer]: TCastleTest read GetTest;

    property CurrentTestName: String read FCurrentTestName;

  published

  end;
  {$M-}

  TCastleTestCaseClass = class of TCastleTestCase;

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
    FUIWindow: TCastleWindow;

    { Flag to stop on first test fail }
    FStopOnFirstFail: Boolean;

    FTestsCount: Integer;
    FTestPassedCount: Integer;
    FTestFailedCount: Integer;

    procedure SetNotifyTestFail(const ANotifyTestFail: TNotifyTestFail);

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
    FNotifyTestFail: TNotifyTestFail;
    FNotifyTestCountChanged: TNotifyTestCountChanged;
    FNotifyTestPassedChanged: TNotifyTestCountChanged;
    FNotifyTestFailedChanged: TNotifyTestCountChanged;
    FNotifyEnabledTestCountChanged: TNotifyEvent;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Adds testcase to tester }
    procedure AddTestCase(const TestCase: TCastleTestCase);
    { Adds registered testcases to tester }
    procedure AddRegisteredTestCases;

    { Scans added test cases }
    procedure Scan;

    { Prepares list of tests to run - choosen test case or all for '' }
    procedure PrepareTestListToRun(const ATestCaseName: String = '');

    { Running all test in one loop - good for console because blocks UI }
    procedure Run;

    { Returns true when there is next test to run }
    function IsNextTestToRun: Boolean;
    { Runs next test, good for UI apps, can raise exceptions }
    procedure RunNextTest;

    function EnabledTestCount: Integer;

    { Stop testing on first fail or run all tests.

      This also means that unhandled exception is just "let through",
      which is nice to let FPC print backtrace of exception to console,
      if outside code will just let unhandled exception to break the program.

      Default @true is suitable for console version. }
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
    property NotifyTestFail: TNotifyTestFail read FNotifyTestFail
      write SetNotifyTestFail;

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

    { Set Enabled of all tests, to reflect whether they match Filter.
      Filter can use wildcards (* and ?) and is compared with
      the test GetFullName.
      See ../../README.md for detailed description and examples. }
    procedure EnableFilter(const Filter: String);
  end;

procedure RegisterTest(CastleTestCaseClass: TCastleTestCaseClass);

{ Like CompareMem, but slower,
  and when the memory is different, log the difference:
  position and the 2 different bytes.

  Note: Size and I are Integer, not Int64.
  This is good enough for current purposes, and it's easier for implementation
  (iterating with In64 doesn't compile with FPC 3.2.2 on Linux/Arm (32-bit)
  now (Raspberry Pi).) }
function CompareMemDebug(const P1, P2: Pointer; const Size: Integer): Boolean;

implementation

uses TypInfo, Math, {$ifdef FPC}testutils,{$else}IOUtils,{$endif} StrUtils,
  CastleLog, CastleUtils, CastleStringUtils, CastleTesterParameters;

{ routines ------------------------------------------------------------------- }

var
  FRegisteredTestCaseList: {$ifdef FPC}specialize{$endif} TList<TCastleTestCaseClass>;

procedure RegisterTest(CastleTestCaseClass: TCastleTestCaseClass);
begin
  FRegisteredTestCaseList.Add(CastleTestCaseClass);
end;

function CompareMemDebug(const P1, P2: Pointer; const Size: Integer): Boolean;
var
  I: Integer;
  P1B, P2B: PByte;
begin
  Result := true;
  P1B := P1;
  P2B := P2;
  for I := 0 to Size - 1 do
  begin
    if P1B^ <> P2B^ then
    begin
      WritelnLog('Difference at %d: %d <> %d', [I, P1B^, P2B^]);
      Exit(false);
    end;
    Inc(P1B);
    Inc(P2B);
  end;
end;

{ TCastleTester -------------------------------------------------------------- }

procedure TCastleTester.AddRegisteredTestCases;
var
  I: Integer;
begin
  for I := 0 to FRegisteredTestCaseList.Count - 1 do
  begin
    AddTestCase(FRegisteredTestCaseList[I].Create);
  end;
end;

procedure TCastleTester.AddTestCase(const TestCase: TCastleTestCase);
begin
  FTestCaseList.Add(TestCase);
  TestCase.FNotifyTestFail := FNotifyTestFail;
  TestCase.FCastleTester := Self;
end;

constructor TCastleTester.Create(AOwner: TComponent);
begin
  inherited;

  StopOnFirstFail := true;
  FRttiContext := TRttiContext.Create;
  FTestCaseList := {$ifdef FPC}specialize{$endif} TObjectList<TCastleTestCase>.Create;
  FTestsToRun := {$ifdef FPC}specialize{$endif} TList<TCastleTest>.Create;
end;

destructor TCastleTester.Destroy;
begin
  FreeAndNil(FTestsToRun);
  FreeAndNil(FTestCaseList);
  FRttiContext.Free;
  inherited;
end;

function TCastleTester.IsNextTestToRun: Boolean;
begin
  Result := (FLastRunningTestIndex < FTestsToRun.Count);
end;

procedure TCastleTester.PrepareTestListToRun(const ATestCaseName: String);
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

    if ATestCaseName <> '' then
      if UpperCase(TestCase.ClassName) <> UpperCase(ATestCaseName) then
        continue;

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
var
  FailMsg: String;
begin
  // raise Exception.Create('Test Return Adress') at {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};
  if not Test.Enabled then
    Exit;

  if Assigned(FNotifyTestCaseExecuted) then
    FNotifyTestCaseExecuted(Test.GetFullName);

  try
    Test.Setup;
    try
      Test.Run;
      TestPassedCount := TestPassedCount + 1;
    finally
      Test.TearDown;
    end;
  except
    { Notes:

      - We don't treat EAssertionFailedError
        any differently from other exceptions here.

        (EAssertionFailedError is raised by our Fail
        and our AssertXxx methods; *not* raised by standard Pascal Assert.)

        Reason: When the test raises (and doesn't catch) any exception
        (explicitly like "raise ..", or because of check failed
        e.g. range check error, or because of standard Pascal Assert failed)
        it should be reported as a failure, just like a failure in our Fail
        or AssertXxx.

        Trying to differentiate between EAssertionFailedError and others
        actually caused bugs in display of castle-tester in the past.
        It seems simpler and more consistent to treat all exceptions the same.

      - We capture all TObject exceptions here, not just Exception,
        to be sure to really catch everything. Though I know of no real-life
        cases when something in standard Pascal libraries raises an exception
        that is not Exception.
    }
    on E: TObject do
    begin
      // call FNotifyTestFail
      if Assigned(FNotifyTestFail) then
      begin
        if E is Exception then
          FailMsg := Exception(E).Message
        else
          FailMsg := ''; // exception class not Exception, no better message
        FNotifyTestFail(Test.GetFullName, FailMsg);
      end;

      // handle TestFailedCount
      TestFailedCount := TestFailedCount + 1;

      // handle FStopOnFirstFail
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

  function TestMethod(const MethodName: String): Boolean;
  begin
    Result := IsPrefix('test', MethodName, true) and
      { Avoid adding internal method TestCount }
      not SameText(MethodName, 'TestCount');
  end;

var
  {$ifdef FPC}
  MethodList: TStringList;
  AMethodName: String;
  {$else}
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
  {$endif}
begin
  {$ifdef FPC}
  MethodList := TStringList.Create;
  try
    GetMethodList(TestCase, MethodList);

    for AMethodName in MethodList do
    begin
      if TestMethod(AMethodName) then
        TestCase.AddTest(AMethodName, TestCase.MethodAddress(AMethodName));
    end;

  finally
    FreeAndNil(MethodList);
  end;
  {$else}
  RttiType := FRttiContext.GetType(TestCase.ClassInfo);

  for RttiMethod in RttiType.GetMethods do
  begin
    if (RttiMethod.MethodKind in [mkProcedure, mkFunction]) and
      (Length(RttiMethod.GetParameters) = 0) and
      TestMethod(RttiMethod.Name) then
    begin
      TestCase.AddTest(RttiMethod.Name, RttiMethod);
    end;
  end;
  {$endif}
end;

procedure TCastleTester.SetNotifyTestFail(
  const ANotifyTestFail: TNotifyTestFail);
var
  TestCase: TCastleTestCase;
begin
  if @FNotifyTestFail = @ANotifyTestFail then
    Exit;

  FNotifyTestFail := ANotifyTestFail;
  for TestCase in FTestCaseList do
  begin
    TestCase.FNotifyTestFail := ANotifyTestFail;
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

procedure TCastleTester.EnableFilter(const Filter: String);
var
  TestCase: TCastleTestCase;
  Test: TCastleTest;
  I: Integer;
begin
  for TestCase in FTestCaseList do
    for I := 0 to TestCase.TestCount - 1 do
    begin
      Test := TestCase.Test[I];
      Test.Enabled := IsWild(Test.GetFullName, Filter, true);
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
  {$ifdef FPC}const AMethodPointer: CodePointer{$else}
      const ARttiMethod: TRttiMethod{$endif}): TCastleTest;
begin
  Result := TCastleTest.Create(Self, AName, {$ifdef FPC}AMethodPointer{$else}
  ARttiMethod{$endif});
  FTestList.Add(Result);
end;

procedure TCastleTestCase.AssertBoxesEqual(const Expected, Actual: TBox3D;
  const Epsilon: Double; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  AssertBoxesEqual('', Expected, Actual, Epsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertBoxesEqual(const Expected, Actual: TBox3D;
  AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  AssertBoxesEqual('', Expected, Actual, AddrOfError);
end;

procedure TCastleTestCase.AssertBoxesEqual(const Msg: String;
  const Expected, Actual: TBox3D;
  const Epsilon: Double; AddrOfError: Pointer);
var
  I: Integer;
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  if Expected.IsEmpty and Actual.IsEmpty then
    Exit; // OK

  if Expected.IsEmpty then
    Fail(Format('Expected empty box, actual box is NOT empty (%s). ' + Msg,
      [Actual.ToRawString]));

  if Actual.IsEmpty then
    Fail(Format('Expected NOT empty box (%s), actual box is empty. ' + Msg,
      [Expected.ToRawString]), AddrOfError);

  for I := 0 to 2 do
    if (not SameValue(Expected.Data[0][I], Actual.Data[0][I], Epsilon)) or
       (not SameValue(Expected.Data[1][I], Actual.Data[1][I], Epsilon)) then
      Fail(Format('Boxes are not equal: expected: %s, actual: %s. ' + Msg,
        [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertBoxesEqual(const Msg: String;
  const Expected, Actual: TBox3D;
  AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  AssertBoxesEqual(Msg, Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertEquals(const Expected, Actual: Single);
begin
  AssertTrue('Expected: ' + FloatToStr(Expected) + ' Actual: ' +
    FloatToStr(Actual), SameValue(Expected, Actual, SingleEpsilon),
    {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif});
end;

procedure TCastleTestCase.AssertFalse(const Msg: String;
  const ACondition: Boolean; AddrOfError: Pointer);
begin
  if not Assigned(AddrOfError) then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};


  AssertTrue(Msg, not ACondition, AddrOfError);
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
  AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};
  AssertFrustumEquals(Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertImagesEqual(
  const Expected, Actual: TCastleImage; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertEquals(Expected.Width, Actual.Width);
  AssertEquals(Expected.Height, Actual.Height);
  AssertEquals(Expected.Depth, Actual.Depth);
  AssertEquals(Expected.Size, Actual.Size);

  AssertTrue(CompareMemDebug(Expected.RawPixels, Actual.RawPixels, Expected.Size));
  AssertTrue(CompareMem     (Expected.RawPixels, Actual.RawPixels, Expected.Size));
end;

procedure TCastleTestCase.AssertFrustumEquals(const Expected, Actual: TFrustum;
  const Epsilon: Single; AddrOfError: Pointer);
var
  I: TFrustumPlane;
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  try
    AssertEquals(Expected.FarInfinity, Actual.FarInfinity);

    if Expected.FarInfinity then
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
      ]), AddrOfError);
    end;
  end;
end;

procedure TCastleTestCase.AssertMatrixEquals(const Expected, Actual: TMatrix4;
  const Epsilon: Single; AddrOfError: Pointer);
var
  DifferenceEpsilon: Single;
  I, J: TMatrix4.TIndex;
begin
  if not Assigned(AddrOfError) then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

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
      ]), AddrOfError);
  end;
end;

procedure TCastleTestCase.AssertMatrixEquals(const Expected, Actual: TMatrix4;
  AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  AssertMatrixEquals(Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertPlaneEquals(const Expected, Actual: TVector4;
  AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};
  AssertPlaneEquals(Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(const Expected,
  Actual: TVector2Double; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(const Expected,
  Actual: TVector3Double; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(const Expected,
  Actual: TVector4Double; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(const Expected,
  Actual: TVector2Double; const Epsilon: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector2Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector2Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(const Expected,
  Actual: TVector3Double; const Epsilon: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector3Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector3Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(const Expected,
  Actual: TVector4Double; const Epsilon: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector4Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector4Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual: Double;
  AddrOfError: Pointer);
begin
  if not Assigned(AddrOfError) then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  AssertSameValue(Expected, Actual, DoubleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual: Double;
  const Epsilon: Double; AddrOfError: Pointer);
begin
  if not Assigned(AddrOfError) then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  if not SameValue(Expected, Actual, Epsilon) then
    Fail(Format('Floats (Double) are not equal: expected: %g, actual: %g',
      [Expected, Actual]), AddrOfError);
end;

procedure TCastleTestCase.AssertRectsEqual(const Expected,
  Actual: TFloatRectangle; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  if not Expected.Equals(Actual) then
    Fail(Format('Expected rect (%s) does not equal actual (%s)',
      [Expected.ToString, Actual.ToString]), AddrOfError);
end;

procedure TCastleTestCase.AssertRectsEqual(const Expected, Actual: TRectangle;
  AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  if not Expected.Equals(Actual) then
    Fail(Format('Expected rect (%s) does not equal actual (%s)',
      [Expected.ToString, Actual.ToString]), AddrOfError);
end;

procedure TCastleTestCase.AssertPlaneEquals(const Expected, Actual: TVector4;
  const Epsilon: Single; AddrOfError: Pointer);
var
  MaxE, MaxA: Integer;
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  MaxE := MaxAbsVectorCoord(Expected);
  MaxA := MaxAbsVectorCoord(Actual);

  if MaxE <> MaxA then
    Fail(Format('Planes (TVector4) are not equal, their maximum component index differs. Expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);

  if IsZero(Expected[MaxE], Epsilon) and
     IsZero(  Actual[MaxA], Epsilon) then
  begin
    if not (Expected.IsZero(Epsilon) and Actual.IsZero(Epsilon)) then
      Fail(Format('Planes (TVector4) are not equal, they should be both zero since maximum component is zero. Expected: %s, actual: %s',
        [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
  end else
  if IsZero(Expected[MaxE], Epsilon) or
     IsZero(  Actual[MaxA], Epsilon) then
  begin
    Fail(Format('Planes (TVector4) are not equal, one of them has zero maximum component, the other not. Expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
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
      ]), AddrOfError);
  end;
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual: Single;
  const Epsilon: Single; AddrOfError: Pointer);
begin
  if not Assigned(AddrOfError) then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  if not SameValue(Expected, Actual, Epsilon) then
    Fail(Format('Floats (Single) are not equal: expected: %g, actual: %g',
      [Expected, Actual]), AddrOfError);
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual: Single;
  AddrOfError: Pointer);
begin
  if not Assigned(AddrOfError) then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif};

  AssertSameValue(Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertTrue(const Msg: String;
  const ACondition: Boolean; AddrOfError: Pointer);
begin
  if not Assigned(AddrOfError) then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not ACondition then
    Fail(Msg, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected,
  Actual: TVector4Byte; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector4Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector4Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected,
  Actual: TVector3Byte; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector3Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector3Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected,
  Actual: TVector2Byte; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector2Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector2Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected, Actual: TVector3;
  AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertVectorEquals(Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected, Actual: TVector4;
  const Epsilon: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector4.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector4) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected, Actual: TVector3;
  const Epsilon: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector3.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector3) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected, Actual: TVector2;
  const Epsilon: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  if not TVector2.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector2) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected, Actual: TVector4;
  AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertVectorEquals(Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(const Expected, Actual: TVector2;
  AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertVectorEquals(Expected, Actual, SingleEpsilon, AddrOfError);
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

destructor TCastleTestCase.Destroy;
begin
  FreeAndNil(FTestList);
  inherited;
end;

function TCastleTestCase.CreateWindowForTest: TCastleWindow;
begin
  if FWindowForTest <> nil then
    raise Exception.Create('CreateWindowForTest called twice without DestroyWindowForTest');
  if not CanCreateWindowForTest then
    raise Exception.Create('CreateWindowForTest called when CanCreateWindowForTest = false');

  FWindowForTest := TCastleWindow.Create(nil);
  SavedApplicationMainWindow := Application.MainWindow;
  Application.MainWindow := FWindowForTest;
  Result := FWindowForTest;
end;

procedure TCastleTestCase.DestroyWindowForTest(var Window: TCastleWindow);
begin
  if FWindowForTest = nil then
    raise Exception.Create('DestroyWindowForTest called without CreateWindowForTest');

  Assert(FWindowForTest = Window);
  Window := nil;

  FreeAndNil(FWindowForTest);
  Application.MainWindow := SavedApplicationMainWindow;
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

procedure TCastleTestCase.Fail(const Msg: String; const AddrOfError: Pointer);
begin
  if Assigned(AddrOfError) then
    raise EAssertionFailedError.Create(Msg) at AddrOfError
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

{ TODO:
function TCastleTestCase.GetTestingViewport: TCastleViewport;
begin
  raise Exception.Create('Not implemented');
end;
}

function TCastleTestCase.CanCreateWindowForTest: Boolean;
begin
  Result :=
    {$if defined(ANDROID) or
         defined(iPHONESIM) or
         defined(iOS) or
         defined(CASTLE_NINTENDO_SWITCH)}
      // On these platforms, we cannot create a window, so we cannot test
      false
    {$else}
      not ParamNoWindowCreate
    {$endif};
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

procedure TCastleTestCase.AssertEquals(const Expected, Actual: Integer);
begin
  AssertEquals('', Expected, Actual,
    {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif});
end;

procedure TCastleTestCase.AssertEquals(const Expected, Actual: Cardinal);
begin
  AssertEquals('', Expected, Actual,
    {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif});
end;

procedure TCastleTestCase.AssertEquals(const Expected, Actual: Int64);
begin
  AssertEquals('', Expected, Actual,
    {$ifdef FPC}get_caller_addr(get_frame){$else}ReturnAddress{$endif});
end;

procedure TCastleTestCase.AssertEquals(const Msg: String; const Expected,
  Actual: Integer; AddrOfError: Pointer = nil);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertTrue(PrepareCustomMsg(Msg) + 'Expected: ' + IntToStr(Expected) +
    ' Actual: ' + IntToStr(Actual), Expected = Actual, AddrOfError);
end;

procedure TCastleTestCase.AssertEquals(const Msg, Expected, Actual: String;
  AddrOfError: Pointer = nil);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertTrue(PrepareCustomMsg(Msg) + 'Expected: ' + Expected +
    ' Actual: ' + Actual, Expected = Actual, AddrOfError);
end;

procedure TCastleTestCase.AssertEquals(const Msg: String; const Expected,
  Actual: Boolean; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertTrue(PrepareCustomMsg(Msg) + 'Expected: ' + BoolToStr(Expected, true) + ' Actual: ' +
    BoolToStr(Actual, true), Expected = Actual, AddrOfError);
end;

procedure TCastleTestCase.AssertEquals(const Msg: String; const Expected,
  Actual: Cardinal; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertTrue(PrepareCustomMsg(Msg) + 'Expected: ' + IntToStr(Expected) +
    ' Actual: ' + IntToStr(Actual), Expected = Actual, AddrOfError);
end;

procedure TCastleTestCase.AssertEquals(const Msg: String; const Expected,
  Actual: Int64; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := {$ifdef FPC}get_caller_addr(get_frame){$else}System.ReturnAddress{$endif};

  AssertTrue(PrepareCustomMsg(Msg) + 'Expected: ' + IntToStr(Expected) +
    ' Actual: ' + IntToStr(Actual), Expected = Actual, AddrOfError);
end;

{ TCastleTest }

constructor TCastleTest.Create(const ATestCase: TCastleTestCase;
  const AName: String; {$ifdef FPC}const AMethodPointer: CodePointer{$else}
      const ARttiMethod: TRttiMethod{$endif});
begin
  FTestCase := ATestCase;
  Name := AName;
  FEnabled := true;
  {$ifdef FPC}
  FMethodPointer := AMethodPointer;
  {$else}
  FRttiMethod := ARttiMethod;
  {$endif}
end;

function TCastleTest.GetFullName: String;
begin
  Result:= FTestCase.ClassName + '.' + Name;
end;

procedure TCastleTest.Run;
{$ifdef FPC}
var
  Method: TMethod;
type
  TCastleTestFunc = procedure() of object;
{$endif}
var
  DummyWindowForTest: TCastleWindow;
begin
  FTestCase.FCurrentTestName := GetFullName;
  try
    {$ifdef FPC}
    Method.Code := FMethodPointer;
    Method.Data := Pointer(FTestCase);
    TCastleTestFunc(Method);
    {$else}
    FRttiMethod.Invoke(FTestCase, []);
    {$endif}
  finally
    { Automatically free window created by CreateWindowForTest. }
    if FTestCase.FWindowForTest <> nil then
    begin
      { Use DummyWindowForTest that will be set to nil by DestroyWindowForTest. }
      DummyWindowForTest := FTestCase.FWindowForTest;
      FTestCase.DestroyWindowForTest(DummyWindowForTest);
    end;
  end;
end;

procedure TCastleTest.Setup;
begin
  FTestCase.Setup;
end;

procedure TCastleTest.TearDown;
begin
  FTestCase.TearDown;
end;

procedure TCastleTest.SetEnabled(NewValue: Boolean);
begin
  if FEnabled = NewValue then
    Exit;

  FEnabled := NewValue;

  if Assigned(FTestCase.FCastleTester.FNotifyEnabledTestCountChanged) then
    FTestCase.FCastleTester.FNotifyEnabledTestCountChanged(Self);
end;

initialization
  FRegisteredTestCaseList := {$ifdef FPC}specialize{$endif} TList<TCastleTestCaseClass>.Create;
finalization
  FreeAndNil(FRegisteredTestCaseList);
end.
