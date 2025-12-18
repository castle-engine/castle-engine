// -*- compile-command: "./test_single_testcase.sh TTestCastleUIControls" -*-

{
  Copyright 2018-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test of CastleUIControls unit. }
unit TestCastleUIControls;

interface

uses
  Classes, SysUtils,
  CastleTester, CastleUIControls;

type
  TTestCastleUIControls = class(TCastleTestCase)
  published
    procedure TestRectEffective;
    procedure TestRecursiveSize;
    procedure TestForIn;
    procedure TestDetachParent;
    procedure TestContainerSettingsNoWindow;
    procedure TestContainerSettingsClosedWindow;
    procedure TestContainerSettingsOpenWindow;
    procedure TestControlsParentDoesNotOwn;
    procedure TestMoveToFront;
    procedure TestViewsLifecycle;
  end;

implementation

uses CastleRectangles, CastleVectors, CastleUtils, CastleWindow;

{ TTestContainer ------------------------------------------------------------- }

type
  { Dummy container class with minimal overrides (to not be abstract),
    just for testing. }
  TTestContainer = class(TCastleContainer)
  public
    function PixelsWidth: Integer; override;
    function PixelsHeight: Integer; override;
  end;

function TTestContainer.PixelsWidth: Integer;
begin
  Result := 100;
end;

function TTestContainer.PixelsHeight: Integer;
begin
  Result := 100;
end;

{ TTestCastleUIControls ------------------------------------------------------ }

procedure TTestCastleUIControls.TestRectEffective;
var
  N: TCastleUserInterface;
  F: TCastleUserInterface;
begin
  N := TCastleUserInterface.Create(nil);
  try
    AssertEquals(100, N.EffectiveWidth);
    AssertEquals(100, N.EffectiveHeight);

    N.Width := 0;
    N.Height := 0;
    AssertEquals(0, N.EffectiveWidth);
    AssertEquals(0, N.EffectiveHeight);
  finally FreeAndNil(N); end;

  F := TCastleUserInterface.Create(nil);
  try
    F.Width := 300;
    F.Height := 400;
    AssertEquals(300, F.EffectiveWidth);
    AssertEquals(400, F.EffectiveHeight);
  finally FreeAndNil(F); end;
end;

type
  TParentAdjustsToChildren = class(TCastleUserInterface)
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  end;

procedure TParentAdjustsToChildren.PreferredSize(var PreferredWidth, PreferredHeight: Single);
var
  I: Integer;
  C: TCastleUserInterface;
begin
  for I := 0 to ControlsCount - 1 do
  begin
    C := Controls[I];
    MaxVar(PreferredWidth, C.EffectiveWidth * UIScale);
    MaxVar(PreferredHeight, C.EffectiveHeight * UIScale);
  end;
end;

procedure TTestCastleUIControls.TestRecursiveSize;
var
  UiParent: TParentAdjustsToChildren;
  UiChild: TCastleUserInterface;
begin
  UiParent := TParentAdjustsToChildren.Create(nil);
  UiChild := TCastleUserInterface.Create(nil);
  try
    UiParent.InsertFront(UiChild);

    // check that calculating rects doesn't cause infinite loop
    UiParent.RenderRect;
    UiChild.RenderRect;

    UiChild.FullSize := true;

    // check that calculating rects doesn't cause infinite loop
    UiParent.RenderRect;
    UiChild.RenderRect;
  finally
    FreeAndNil(UiParent);
    FreeAndNil(UiChild);
  end;
end;

procedure TTestCastleUIControls.TestForIn;
var
  Owner: TComponent;
  T, T1, C: TCastleUserInterface;
  Y: Single;
begin
  Owner := TComponent.Create(nil);
  try
    T := TCastleUserInterface.Create(Owner);
    T.AnchorDelta := Vector2(0, 0);

    T1 := TCastleUserInterface.Create(Owner);
    T1.AnchorDelta := Vector2(1, 0);
    T.InsertFront(T1);

    T1 := TCastleUserInterface.Create(Owner);
    T1.AnchorDelta := Vector2(2, 0);
    T.InsertFront(T1);

    T1 := TCastleUserInterface.Create(Owner);
    T1.AnchorDelta := Vector2(3, 0);
    T.InsertFront(T1);

    T1 := TCastleUserInterface.Create(Owner);
    T1.AnchorDelta := Vector2(1, 1);
    T.Controls[0].InsertFront(T1);

    T1 := TCastleUserInterface.Create(Owner);
    T1.AnchorDelta := Vector2(1, 2);
    T.Controls[0].InsertFront(T1);

    T1 := TCastleUserInterface.Create(Owner);
    T1.AnchorDelta := Vector2(1, 3);
    T.Controls[0].InsertFront(T1);

    Y := 1;
    for C in T do
    begin
      AssertVectorEquals(C.AnchorDelta, Vector2(Y, 0));
      Y := Y + 1;
    end;
    AssertSameValue(Y, 4);
  finally FreeAndNil(Owner) end;
end;

procedure TTestCastleUIControls.TestDetachParent;
var
  Container: TTestContainer;
  U1, U2, U3: TCastleUserInterface;
begin
  Container := nil;
  U1 := nil;
  U2 := nil;
  U3 := nil;
  try
    Container := TTestContainer.Create(nil);
    U1 := TCastleUserInterface.Create(nil);
    U2 := TCastleUserInterface.Create(nil);
    U3 := TCastleUserInterface.Create(nil);

    AssertTrue(U1.Parent = nil);
    AssertTrue(U1.Container = nil);
    AssertTrue(U2.Parent = nil);
    AssertTrue(U2.Container = nil);
    AssertTrue(U3.Parent = nil);
    AssertTrue(U3.Container = nil);

    Container.Controls.InsertFront(U1);
    AssertTrue(U1.Parent = nil);
    AssertTrue(U1.Container = Container);
    AssertTrue(U2.Parent = nil);
    AssertTrue(U2.Container = nil);
    AssertTrue(U3.Parent = nil);
    AssertTrue(U3.Container = nil);

    U1.InsertFront(U2);
    AssertTrue(U1.Parent = nil);
    AssertTrue(U1.Container = Container);
    AssertTrue(U2.Parent = U1);
    AssertTrue(U2.Container = Container);
    AssertTrue(U3.Parent = nil);
    AssertTrue(U3.Container = nil);

    U2.InsertFront(U3);
    AssertTrue(U1.Parent = nil);
    AssertTrue(U1.Container = Container);
    AssertTrue(U2.Parent = U1);
    AssertTrue(U2.Container = Container);
    AssertTrue(U3.Parent = U2);
    AssertTrue(U3.Container = Container);

    FreeAndNil(U2);
    AssertTrue(U1.Parent = nil);
    AssertTrue(U1.Container = Container);
    AssertTrue(U2 = nil);
    AssertTrue(U3.Parent = nil);
    AssertTrue(U3.Container = nil);
  finally
    FreeAndNil(Container);
    FreeAndNil(U1);
    FreeAndNil(U2);
    FreeAndNil(U3);
  end;
end;

procedure TTestCastleUIControls.TestContainerSettingsNoWindow;
var
  Container: TCastleContainer;
begin
  Container := TTestContainer.Create(nil);
  try
    Container.LoadSettings('castle-data:/settings_test/test1.xml');
    Container.LoadSettings('castle-data:/settings_test/test2.xml');
    Container.LoadSettings('castle-data:/settings_test/test3.xml');
  finally FreeAndNil(Container); end;
end;

procedure TTestCastleUIControls.TestContainerSettingsClosedWindow;
var
  Window: TCastleWindow;
  Container: TCastleContainer;
begin
  if not CanCreateWindowForTest then
  begin
    AbortTest;
    Exit;
  end;

  Window := CreateWindowForTest;
  try
    Container := Window.Container;
    Container.LoadSettings('castle-data:/settings_test/test1.xml');
    Container.LoadSettings('castle-data:/settings_test/test2.xml');
    Container.LoadSettings('castle-data:/settings_test/test3.xml');
  finally DestroyWindowForTest(Window); end;
end;

procedure TTestCastleUIControls.TestContainerSettingsOpenWindow;
var
  Window: TCastleWindow;
  Container: TCastleContainer;
begin
  if not CanCreateWindowForTest then
  begin
    AbortTest;
    Exit;
  end;

  Window := CreateWindowForTest;
  try
    Window.Open;
    Container := Window.Container;
    Container.LoadSettings('castle-data:/settings_test/test1.xml');
    Container.LoadSettings('castle-data:/settings_test/test2.xml');
    Container.LoadSettings('castle-data:/settings_test/test3.xml');
  finally DestroyWindowForTest(Window); end;
end;

type
  TMyUi = class(TCastleUserInterface)
  public
    FreeingUnexpected: Boolean;
    destructor Destroy; override;
  end;

destructor TMyUi.Destroy;
begin
  { Note that in general, one should avoid raising exceptions from the destructor.
    That's because such exception can easily obscure the real reason while
    the destructor is called -- because something else failed,
    and we're in "finally" clause because of other exception.
    In general, destructor should just gracefully accept half-created instance,
    because there maybe was exception during constructor.

    But in this case it is justified.
    The exception should never happen if test goes OK.
    If the test fails, we'll investigate why. }
  if FreeingUnexpected then
    raise Exception.Create('Unexpected freeing, this means something owns it but should not');
  inherited;
end;

procedure TTestCastleUIControls.TestControlsParentDoesNotOwn;
var
  MyChild: TMyUi;
  Container: TTestContainer;
  SomeParent: TCastleUserInterface;
begin
  // make it safe to free stuff by a single finally clause
  Container := nil;
  SomeParent := nil;
  MyChild := TMyUi.Create(nil);
  try
    MyChild.FreeingUnexpected := true;

    Container := TTestContainer.Create(nil);

    Container.Controls.InsertFront(MyChild);
    Container.Controls.Remove(MyChild);

    SomeParent := TCastleUserInterface.Create(nil);
    SomeParent.InsertFront(MyChild);
    SomeParent.RemoveControl(MyChild);

    Container.Controls.InsertFront(SomeParent);
    SomeParent.InsertFront(MyChild);
    SomeParent.RemoveControl(MyChild);

    MyChild.FreeingUnexpected := false;
  finally
    FreeAndNil(MyChild);
    FreeAndNil(SomeParent);
    FreeAndNil(Container);
  end;
end;

type
  TMyUiTestMoveToFront = class(TCastleUserInterface)
  public
    ChangeContainerUnexpected: Boolean;
    procedure InternalSetContainer(const NewContainer: TCastleContainer); override;
  end;

procedure TMyUiTestMoveToFront.InternalSetContainer(const NewContainer: TCastleContainer);
begin
  if ChangeContainerUnexpected then
    raise Exception.Create('Unexpected change of container');
  inherited;
end;

procedure TTestCastleUIControls.TestMoveToFront;
var
  Container: TTestContainer;
  U1, U2: TCastleUserInterface;
  UTest: TMyUiTestMoveToFront;
begin
  U1 := nil;
  U2 := nil;
  Container := nil;
  try
    Container := TTestContainer.Create(nil);
    U1 := TCastleUserInterface.Create(nil);
    U1.Name := 'U1';
    U2 := TCastleUserInterface.Create(nil);
    U2.Name := 'U2';
    UTest := TMyUiTestMoveToFront.Create(nil);
    UTest.Name := 'UTest';

    UTest.ChangeContainerUnexpected := true;
    UTest.MoveToFront; // does nothing now
    UTest.ChangeContainerUnexpected := false;

    Container.Controls.InsertFront(UTest);
    Container.Controls.InsertFront(U1);
    AssertEquals(2, Container.Controls.Count);
    AssertTrue(Container.Controls[0] = UTest);
    AssertTrue(Container.Controls[1] = U1);

    UTest.ChangeContainerUnexpected := true;
    UTest.MoveToFront;
    UTest.ChangeContainerUnexpected := false;

    // order changed now
    AssertEquals(2, Container.Controls.Count);
    AssertTrue(Container.Controls[0] = U1);
    AssertTrue(Container.Controls[1] = UTest);

    // now test if it's also good when UTest is a child of U1

    Container.Controls.Remove(UTest);
    U1.InsertFront(UTest);
    U1.InsertFront(U2);
    AssertEquals(2, U1.ControlsCount);
    AssertTrue(U1.Controls[0] = UTest);
    AssertTrue(U1.Controls[1] = U2);

    UTest.ChangeContainerUnexpected := true;
    UTest.MoveToFront;
    UTest.ChangeContainerUnexpected := false;

    // order changed now
    AssertEquals(2, U1.ControlsCount);
    AssertTrue(U1.Controls[0] = U2);
    AssertTrue(U1.Controls[1] = UTest);
  finally
    FreeAndNil(U1);
    FreeAndNil(U2);
    FreeAndNil(UTest);
    FreeAndNil(Container);
  end;
end;

type
  TLoggingView = class(TCastleView)
  public
    EventLog: TStringList;
    procedure Start; override;
    procedure Stop; override;
    procedure Pause; override;
    procedure Resume; override;
  end;

procedure TLoggingView.Start;
begin
  inherited;
  EventLog.Add(ClassName + '.Start');
end;

procedure TLoggingView.Stop;
begin
  inherited;
  EventLog.Add(ClassName + '.Stop');
end;

procedure TLoggingView.Pause;
begin
  inherited;
  EventLog.Add(ClassName + '.Pause');
end;

procedure TLoggingView.Resume;
begin
  inherited;
  EventLog.Add(ClassName + '.Resume');
end;

type
  TView1 = class(TLoggingView)
  end;

  TView2 = class(TLoggingView)
  end;

  TView3 = class(TLoggingView)
  end;

procedure TTestCastleUIControls.TestViewsLifecycle;
var
  Container: TTestContainer;
  V1, V2, V3: TLoggingView;
  EventLog: TStringList;
begin
  V1 := nil;
  V2 := nil;
  V3 := nil;
  Container := nil;
  EventLog := nil;
  try
    Container := TTestContainer.Create(nil);

    // we will collect all view lifecycle events in EventLog
    EventLog := TStringList.Create;
    EventLog.Delimiter := ',';

    V1 := TView1.Create(nil);
    V1.EventLog := EventLog;
    V2 := TView2.Create(nil);
    V2.EventLog := EventLog;
    V3 := TView3.Create(nil);
    V3.EventLog := EventLog;

    AssertEquals('', EventLog.DelimitedText);

    Container.View := V1;
    AssertEquals('TView1.Start,TView1.Resume', EventLog.DelimitedText);
    Container.View := V2;
    AssertEquals('TView1.Start,TView1.Resume,TView1.Pause,TView1.Stop,TView2.Start,TView2.Resume', EventLog.DelimitedText);
    Container.PushView(V3);
    AssertEquals('TView1.Start,TView1.Resume,TView1.Pause,TView1.Stop,TView2.Start,TView2.Resume,TView2.Pause,TView3.Start,TView3.Resume', EventLog.DelimitedText);
    Container.PopView(V3);
    AssertEquals('TView1.Start,TView1.Resume,TView1.Pause,TView1.Stop,TView2.Start,TView2.Resume,TView2.Pause,TView3.Start,TView3.Resume,TView3.Pause,TView3.Stop,TView2.Resume', EventLog.DelimitedText);
    Container.View := nil;
    AssertEquals('TView1.Start,TView1.Resume,TView1.Pause,TView1.Stop,TView2.Start,TView2.Resume,TView2.Pause,TView3.Start,TView3.Resume,TView3.Pause,TView3.Stop,TView2.Resume,TView2.Pause,TView2.Stop', EventLog.DelimitedText);
    EventLog.Clear;

    Container.View := V1;
    AssertEquals('TView1.Start,TView1.Resume', EventLog.DelimitedText);
    Container.PushView(V2);
    AssertEquals('TView1.Start,TView1.Resume,TView1.Pause,TView2.Start,TView2.Resume', EventLog.DelimitedText);
    Container.View := V3;
    { Note that this should stop V1, without doing Resume+Pause on it.
      See https://forum.castle-engine.io/t/bug-i-dont-expect-resume-of-views-in-stack-when-i-assign-container-view/2061/4 }
    AssertEquals('TView1.Start,TView1.Resume,TView1.Pause,TView2.Start,TView2.Resume,TView2.Pause,TView2.Stop,TView1.Stop,TView3.Start,TView3.Resume', EventLog.DelimitedText);
    Container.View := nil;
    AssertEquals('TView1.Start,TView1.Resume,TView1.Pause,TView2.Start,TView2.Resume,TView2.Pause,TView2.Stop,TView1.Stop,TView3.Start,TView3.Resume,TView3.Pause,TView3.Stop', EventLog.DelimitedText);
    EventLog.Clear;

    Container.View := V1;
    AssertEquals('TView1.Start,TView1.Resume', EventLog.DelimitedText);
    Container.PushView(V2);
    AssertEquals('TView1.Start,TView1.Resume,TView1.Pause,TView2.Start,TView2.Resume', EventLog.DelimitedText);
    Container.View := nil;
    { Note that this should stop V1, without doing Resume+Pause on it.
      See https://forum.castle-engine.io/t/bug-i-dont-expect-resume-of-views-in-stack-when-i-assign-container-view/2061/4 }
    AssertEquals('TView1.Start,TView1.Resume,TView1.Pause,TView2.Start,TView2.Resume,TView2.Pause,TView2.Stop,TView1.Stop', EventLog.DelimitedText);
    EventLog.Clear;
  finally
    FreeAndNil(V1);
    FreeAndNil(V2);
    FreeAndNil(V3);
    FreeAndNil(EventLog);
    FreeAndNil(Container);
  end;
end;

initialization
  RegisterTest(TTestCastleUIControls);
end.
